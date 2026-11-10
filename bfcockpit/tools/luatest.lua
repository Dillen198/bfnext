-- Runs Scripts/Hooks/bfcockpit.lua outside DCS and checks the WIKI and DASH tabs.
--
--   "D:\DCS World\bin-mt\luae.exe" bfcockpit\tools\luatest.lua
--
-- luae.exe is a plain Lua 5.1 on DCS's own lua.dll, so this is the same
-- language the panel runs in. dxgui, the DLL, `net`, `DCS` and `lfs` are
-- stand-ins: widgets remember their text, skin and bounds; requests are
-- answered from bfdb/seed_wiki, the real page content. What it can't tell
-- is how anything looks -- that still needs DCS (TESTING.md).
--
-- Exits non-zero when a check fails.

local here = (arg and arg[0] or ""):gsub("[^/\\]*$", "")
local ROOT = here .. "..\\..\\"
local SCRIPT = ROOT .. "bfcockpit\\Scripts\\Hooks\\bfcockpit.lua"
local SEED = ROOT .. "bfdb\\seed_wiki\\"

local TMP = (os.getenv("TEMP") or ".") .. "\\bfcockpit-luatest-" .. os.time() .. "\\"
os.execute('mkdir "' .. TMP .. 'Config" 2>nul')
os.execute('mkdir "' .. TMP .. 'Mods\\Services\\BFCockpit" 2>nul')

local failures, checks = 0, 0
local function check(cond, what)
    checks = checks + 1
    if not cond then
        failures = failures + 1
        print("FAIL: " .. what)
    end
end
local function eq(a, b, what)
    check(a == b, what .. ": expected " .. tostring(b) .. ", got " .. tostring(a))
end

-- ── a small JSON decoder / encoder (DCS has net.json2lua / lua2json) ──

local function json_decode(s)
    local i = 1
    local function ws() i = s:find("[^ \t\r\n]", i) or #s + 1 end
    local value
    local function str()
        local out = {}
        i = i + 1
        while true do
            local c = s:sub(i, i)
            if c == '"' then i = i + 1 break end
            if c == "\\" then
                local e = s:sub(i + 1, i + 1)
                local map = { n = "\n", t = "\t", r = "\r", b = "\b", f = "\f", ['"'] = '"', ["\\"] = "\\", ["/"] = "/" }
                if e == "u" then
                    local cp = tonumber(s:sub(i + 2, i + 5), 16)
                    if cp < 0x80 then out[#out + 1] = string.char(cp)
                    elseif cp < 0x800 then out[#out + 1] = string.char(0xC0 + math.floor(cp / 64), 0x80 + cp % 64)
                    else out[#out + 1] = string.char(0xE0 + math.floor(cp / 4096), 0x80 + math.floor(cp / 64) % 64, 0x80 + cp % 64) end
                    i = i + 6
                else
                    out[#out + 1] = map[e] or e
                    i = i + 2
                end
            else
                out[#out + 1] = c
                i = i + 1
            end
        end
        return table.concat(out)
    end
    function value()
        ws()
        local c = s:sub(i, i)
        if c == "{" then
            local t = {}
            i = i + 1
            ws()
            if s:sub(i, i) == "}" then i = i + 1 return t end
            while true do
                ws()
                local k = str()
                ws(); i = i + 1  -- :
                t[k] = value()
                ws()
                local d = s:sub(i, i); i = i + 1
                if d == "}" then return t end
            end
        elseif c == "[" then
            local t = {}
            i = i + 1
            ws()
            if s:sub(i, i) == "]" then i = i + 1 return t end
            while true do
                t[#t + 1] = value()
                ws()
                local d = s:sub(i, i); i = i + 1
                if d == "]" then return t end
            end
        elseif c == '"' then
            return str()
        elseif s:sub(i, i + 3) == "true" then i = i + 4 return true
        elseif s:sub(i, i + 4) == "false" then i = i + 5 return false
        elseif s:sub(i, i + 3) == "null" then i = i + 4 return nil
        else
            local n = s:match("^-?%d+%.?%d*[eE]?[-+]?%d*", i)
            if not n or n == "" then error("bad JSON at " .. i .. ": " .. s:sub(i, i + 20)) end
            i = i + #n
            return tonumber(n)
        end
    end
    local v = value()
    return v
end

local function json_encode(v)
    local t = type(v)
    if t == "table" then
        if #v > 0 or next(v) == nil then
            local out = {}
            for k, x in ipairs(v) do out[k] = json_encode(x) end
            return "[" .. table.concat(out, ",") .. "]"
        end
        local out = {}
        for k, x in pairs(v) do out[#out + 1] = json_encode(tostring(k)) .. ":" .. json_encode(x) end
        return "{" .. table.concat(out, ",") .. "}"
    elseif t == "string" then
        return '"' .. v:gsub('[%c"\\]', function(c)
            local m = { ["\n"] = "\\n", ["\r"] = "\\r", ["\t"] = "\\t", ['"'] = '\\"', ["\\"] = "\\\\" }
            return m[c] or string.format("\\u%04x", c:byte())
        end) .. '"'
    elseif t == "number" or t == "boolean" then
        return tostring(v)
    end
    return "null"
end

-- ── stand-ins for DCS ──

local clock = 0
os.clock = function() return clock end

local log = {}
net = {
    log = function(m) log[#log + 1] = m end,
    json2lua = json_decode,
    lua2json = json_encode,
    get_server_settings = function() return { name = "Test Server" } end,
    get_my_player_id = function() return 1 end,
    send_chat = function() end,
}
package.loaded.net = net

local callbacks
DCS = {
    isMultiplayer = function() return true end,
    setUserCallbacks = function(t) callbacks = t end,
}

lfs = {
    writedir = function() return TMP end,
    attributes = function(path, what)
        local f = io.open(path, "rb")
        if f then f:close() return what and "file" or { mode = "file" } end
        return nil
    end,
    mkdir = function(path) os.execute('mkdir "' .. path:gsub("\\$", "") .. '" 2>nul') return true end,
    dir = function(path)
        local p = io.popen('dir /b "' .. path .. '" 2>nul')
        local names = {}
        for l in p:lines() do names[#names + 1] = l end
        p:close()
        local k = 0
        return function() k = k + 1 return names[k] end
    end,
}

-- A widget remembers what was done to it; any method it doesn't know is a
-- no-op, as a stand-in only needs to not fail.
local Widget = {}
Widget.__index = function(_, k)
    return rawget(Widget, k) or function() end
end
local function new_widget(kind, text)
    return setmetatable({ kind = kind, text = text or "", visible = true, enabled = true,
                          x = 0, y = 0, w = 0, h = 0, cbs = {}, children = {}, scroll = 0 }, Widget)
end
function Widget:setText(t) self.text = tostring(t) end
function Widget:getText() return self.text end
function Widget:setSkin(s) self.skin = s end
function Widget:setVisible(v) self.visible = v end
function Widget:getVisible() return self.visible end
function Widget:setEnabled(v) self.enabled = v end
function Widget:setBounds(x, y, w, h) self.x, self.y, self.w, self.h = x, y, w, h end
function Widget:getBounds() return self.x, self.y, self.w, self.h end
function Widget:getPosition() return self.x, self.y end
function Widget:getSize() return self.w, self.h end
function Widget:getClientRectSize() return self.w, self.h - 24 end
function Widget:insertWidget(c) self.children[#self.children + 1] = c; c.inserted = true end
function Widget:addMouseUpCallback(fn) self.cbs.up = fn end
function Widget:addChangeCallback(fn) self.cbs.change = fn end
function Widget:addHotKeyCallback(k, fn) self.cbs[k] = fn end
function Widget:setVertScrollValue(v) self.scroll = v end
function Widget:getVertScrollValue() return self.scroll end
function Widget:setFocused(v) self.focused = v end
function Widget:setHintText(t) self.hint = t end
function Widget:click() if self.cbs.up then self.cbs.up(self) end end
function Widget:type(t) self.text = t; if self.cbs.change then self.cbs.change(self) end end

local function module(kind)
    return { new = function(a, b, c, d, e)
        local w = new_widget(kind, kind == "Window" and e or a)
        if kind == "Window" then w.x, w.y, w.w, w.h = a, b, c, d end
        return w
    end }
end
local function skin()
    local st = function() return { [1] = {}, [2] = {} } end
    return { skinData = { params = {}, states = { released = st(), hover = st(), pressed = st(), disabled = st() },
                          skins = { header = { skinData = { params = {}, states = { released = st() } } },
                                    caret = { skinData = { states = { released = { [1] = {} } } } } } } }
end
package.loaded.dxgui = { GetScreenSize = function() return 1920, 1080 end, GetTextureSize = function() return 64, 64 end }
package.loaded.Window = module("Window")
package.loaded.Static = module("Static")
package.loaded.Button = module("Button")
package.loaded.ScrollPane = module("ScrollPane")
package.loaded.EditBox = module("EditBox")
package.loaded.Skin = setmetatable({}, { __index = function() return skin end })
-- DCS's embedded browser: remembers its callbacks and the address it was given
package.loaded.WebViewWidget = { new = function()
    local w = new_widget("WebView")
    w.browserCreated = function(self, cb) self.cbs.created = cb end
    w.onPageLoaded = function(self, cb) self.cbs.loaded = cb end
    w.onMountFailed = function(self, cb) self.cbs.failed = cb end
    w.cefLoadUrl = function(self, url) self.url = url end
    return w
end }

-- ── the DLL, answering from bfdb/seed_wiki ──

local script_text = assert(io.open(SCRIPT, "rb")):read("*a")
local VERSION = script_text:match('local BFCOCKPIT_VERSION = "([%d%.]+)"')

local SECTIONS = { introduction = "Introduction", ["getting-started"] = "Getting Started", playbooks = "Playbooks",
                   gameplay = "Core Gameplay", ["f10-menu"] = "F10 Menu Systems", advanced = "Advanced Topics",
                   reference = "Reference" }
local pages, page_list = {}, {}
do
    local p = io.popen('dir /s /b "' .. SEED .. '*.md"')
    for path in p:lines() do
        local rel = path:match("seed_wiki\\(.*)$"):gsub("\\", "/"):gsub("%.md$", "")
        -- SHOTLIST is the screenshot to-do list, not a page bfdb seeds.
        if rel ~= "SHOTLIST" then
            local f = io.open(path, "rb")
            local content = f:read("*a"):gsub("\r", "")
            f:close()
            local folder = rel:match("^([^/]+)/") or rel
            local e = { slug = rel, title = content:match("^#%s+([^\n]+)") or rel, section = SECTIONS[folder] or "Other",
                        order = #page_list, updated_at = "2026-10-01T00:00:00Z", content = content }
            pages[rel] = e
            page_list[#page_list + 1] = e
        end
    end
    p:close()
end

local requests, queue, next_id = {}, {}, 1
-- what /api/cockpit/dash and /api/cockpit/context answer (the DASH tests)
local dash_reply, dash_status, ctx_reply = nil, 200, nil
local function answer(id, status, body) queue[#queue + 1] = { id = id, status = status, body = body } end

local function route(method, url)
    local path, query = url:match("^https?://[^/]+(/[^?]*)%??(.*)$")
    local q = {}
    for k, v in query:gmatch("([^&=]+)=([^&]*)") do
        q[k] = v:gsub("%%(%x%x)", function(h) return string.char(tonumber(h, 16)) end)
    end
    if path == "/api/cockpit/plugin/version" then return 200, { version = VERSION } end
    if path == "/api/cockpit/dash" then
        if dash_status ~= 200 then return dash_status, nil end
        return 200, dash_reply
    end
    if path == "/api/cockpit/context" and ctx_reply then return 200, ctx_reply end
    if path == "/api/cockpit/wiki/index" then
        local rows, secs, seen = {}, {}, {}
        for _, e in ipairs(page_list) do
            rows[#rows + 1] = { slug = e.slug, title = e.title, section = e.section, order = e.order, updated_at = e.updated_at }
            if not seen[e.section] then seen[e.section] = true; secs[#secs + 1] = e.section end
        end
        return 200, { pages = rows, sections = secs }
    end
    local slug = path:match("^/api/wiki/pages/(.+)$")
    if slug then
        local e = pages[slug]
        if not e then return 404, { error = "page not found" } end
        return 200, e
    end
    if path == "/api/wiki/facts" then
        return 200, { instance = { id = "vs1", label = "Vector Strike 1" },
                      facts = { points = { air_kill = 400 }, default_lives = { Standard = { 3, 4 } } } }
    end
    if path == "/api/cockpit/wiki/search" then
        local hits = {}
        for _, e in ipairs(page_list) do
            if e.content:lower():find(q.q:lower(), 1, true) then
                hits[#hits + 1] = { slug = e.slug, title = e.title, section = e.section, snippet = "..." .. q.q .. "..." }
            end
        end
        return 200, { query = q.q, hits = hits }
    end
    local img = path:match("^/api/cockpit/wiki/image/([%x%-]+)/info$")
    if img then
        return 200, { id = img, format = "png", width = 800, height = 400, svg = false,
                      url = "/api/cockpit/wiki/image/" .. img .. "?w=" .. (q.w or "640") }
    end
    return 404, nil
end

local downloads = {}
package.loaded.bfcockpit_net = {
    headers = true, version = "1.1.0", script_version = VERSION, git = "test",
    random_key = function() return string.rep("k", 40) end,
    request = function(method, url, body, timeout, headers)
        local id = next_id; next_id = next_id + 1
        requests[#requests + 1] = { method = method, url = url }
        local status, data = route(method, url)
        answer(id, status, data and json_encode(data) or "")
        return id
    end,
    download = function(url, path, timeout)
        local id = next_id; next_id = next_id + 1
        downloads[#downloads + 1] = { url = url, path = path }
        local f = assert(io.open(path, "wb"))
        f:write("\137PNG\r\n\26\n")
        f:close()
        answer(id, 200, "")
        return id
    end,
    poll = function()
        local out = queue
        queue = {}
        return out
    end,
}

-- ── load it ──

BFCOCKPIT_TEST = {}
local chunk = assert(loadfile(SCRIPT))
chunk()
assert(callbacks, "the script registered no callbacks")
local P, W, wk = BFCOCKPIT_TEST.panel, BFCOCKPIT_TEST.wiki, BFCOCKPIT_TEST.wk
assert(P and W and wk, "test hooks missing")

local function frame(secs)
    clock = clock + (secs or 0.1)
    callbacks.onSimulationFrame()
end
local function settle()
    for _ = 1, 20 do frame(0.05) end
end

-- ── pure: tokens ──

local facts = { points = { air_kill = 400, ratio = 0.35, flt = 2.5 }, default_lives = { Standard = { 3, 4 } },
                lock_sides = true, names = { "a", "b" },
                deployables = { Blue = { { path = { "Air Defense", "Patriot" }, crates = { { name = "Patriot crate", required = 3, weight = 1200 } }, limit = 2, ewr = true } } } }
eq(W.apply_tokens("worth {{cfg:points.air_kill|350}} points", facts), "worth 400 points", "cfg token")
eq(W.apply_tokens("worth {{cfg:points.missing|350}} points", facts), "worth 350 points", "cfg fallback")
eq(W.apply_tokens("worth {{ cfg:points.missing }} points", facts), "worth points.missing points", "cfg without fallback reads its path")
eq(W.apply_tokens("{{cfg:default_lives.Standard[0]|1}}/{{cfg:default_lives.Standard[1]|1}}", facts), "3/4", "array index")
eq(W.apply_tokens("{{cfg:points.ratio|0}} {{cfg:points.flt|0}} {{cfg:lock_sides|no}} {{cfg:names|-}}", facts), "0.35 2.5 yes a, b", "formatting")
eq(W.apply_tokens("{{cfg:points.air_kill|350}}", nil), "350", "no facts at all")
local tbl = W.apply_tokens("{{list:deployables.Blue}}", facts)
check(tbl:find("| Deployable |", 1, true) and tbl:find("**Patriot**", 1, true) and tbl:find("3x Patriot crate (1200 kg)", 1, true), "list token table: " .. tbl)
check(W.apply_tokens("{{list:troops.Red}}", facts):find("No troops Red are configured", 1, true), "list token default")

-- ── pure: links ──

eq(W.resolve("../f10-menu/jtac.md", "getting-started/welcome"), "f10-menu/jtac", "relative link")
eq(W.resolve("./actions.md", "f10-menu/overview"), "f10-menu/actions", "./ link")
eq(W.resolve("cargo.md#crates", "f10-menu/overview"), "f10-menu/cargo", "anchor dropped")
eq(W.resolve("/gameplay/gci", "f10-menu/overview"), "gameplay/gci", "absolute link")
eq(W.resolve("gameplay/objectives.md", "introduction"), "gameplay/objectives", "from a top-level page")
eq(W.resolve("https://discord.gg/x", "a/b"), nil, "outside link")
eq(W.resolve("#top", "a/b"), nil, "anchor only")
eq(W.resolve("mailto:a@b", "a/b"), nil, "mailto")
eq(W.resolve("/api/cockpit/plugin/installer", "a/b"), nil, "bfdb endpoint")

local text, links, pics = W.inline("**Bold** and `x **not** y` and [JTAC](../f10-menu/jtac.md), [site](https://a.b/c) and ![pic](/api/wiki/images/abc) <br> _it_ snake_case &amp; \\*", "getting-started/welcome", "https://api")
eq(text, "Bold and x **not** y and JTAC, site (https://a.b/c) and  \n it snake_case & *", "inline text")
eq(#links, 1, "one internal link")
eq(links[1] and links[1].slug, "f10-menu/jtac", "link slug")
eq(#pics, 1, "inline picture")
local t2 = W.inline("[installer](/api/cockpit/plugin/installer)", "a", "https://api.x")
eq(t2, "installer (https://api.x/api/cockpit/plugin/installer)", "bfdb link written out")

-- ── pure: blocks ──

local md = [[
# Title

Para one
continues here.

## Section

- item one
  - nested [Cargo](cargo.md)
- [Only a link](jtac.md)
1. first
2. second

| Col A | Col B |
|---|:--:|
| a \| b | c |

> **Tip:** quoted

```lua
local x = 1
```

```f10menu
{"root":"F10 Other","items":[{"label":"Logistics","items":[{"label":"Unpack","says":"Unpacked","note":"near crates"}]}]}
```

---
![Alt text](/api/wiki/images/11111111-2222-3333-4444-555555555555)
]]
local blocks = W.parse(md)
local kinds = {}
for _, b in ipairs(blocks) do kinds[#kinds + 1] = b.kind end
eq(table.concat(kinds, ","), "heading,para,heading,list,list,table,quote,code,f10menu,rule,image", "block kinds")
eq(blocks[2].text, "Para one continues here.", "paragraph joined")
eq(#blocks[4].items, 3, "bullet items")
eq(blocks[4].items[2].level, 1, "nested level")
eq(blocks[5].items[2].n, 2, "numbered")
eq(blocks[6].rows[1][1], "a | b", "escaped pipe in a cell")

-- ── the panel ──

frame()  -- creates the window
local wid = P.widgets()
check(wid.window ~= nil, "window created")
P.show()
P.select_tab("wiki")
settle()

local function visible_rows()
    local pool, actions = P.rows()
    local out = {}
    for i, b in ipairs(pool.buttons) do
        if b.visible then out[#out + 1] = { y = b.y, x = b.x, w = b.w, h = b.h, text = b.text, button = true, action = actions[i], skin = b.skin } end
    end
    for _, t in ipairs(pool.texts) do
        if t.visible then out[#out + 1] = { y = t.y, x = t.x, w = t.w, h = t.h, text = t.text, skin = t.skin } end
    end
    for _, p in ipairs(pool.pics) do
        if p.visible then out[#out + 1] = { y = p.y, x = p.x, w = p.w, h = p.h, pic = true, skin = p.skin } end
    end
    table.sort(out, function(a, b) if a.y ~= b.y then return a.y < b.y end return a.x < b.x end)
    return out
end
local function find_button(pat)
    for _, r in ipairs(visible_rows()) do
        if r.button and r.text:find(pat) then return r end
    end
end
local function all_text()
    local t = {}
    for _, r in ipairs(visible_rows()) do if r.text then t[#t + 1] = r.text end end
    return table.concat(t, "\n")
end

-- home: the sections
if not find_button("Introduction") then print(all_text()) print(table.concat(log, "\n")) end
eq(wid.crumb.text, "WIKI", "crumb at home")
check(find_button("Introduction") and find_button("F10 Menu Systems"), "home lists the sections")
check(not wid.back.enabled, "back is off at home")

-- tab bar: eight tabs, at the smallest panel
wid.window:setBounds(0, 0, P.px(520), P.px(300))
P.render()
do
    local tabs = wid.tabs
    local order = { "menu", "cargo", "status", "dash", "ewr", "log", "wiki", "wheel" }
    local right = 0
    local widths = {}
    for _, id in ipairs(order) do
        local b = tabs[id]
        check(b ~= nil, "tab " .. id)
        local need = P.label_em(b.text) * P.px(13)
        check(b.w >= need, string.format("tab %s is %dpx, its label needs ~%dpx", id, b.w, need))
        check(b.x >= right, "tab " .. id .. " overlaps the one before")
        right = b.x + b.w
        widths[#widths + 1] = id .. "=" .. b.w
    end
    -- the opacity/close buttons start at cw - pad - 3*26 - 2*4
    check(right <= P.px(520) - P.px(8) - 3 * P.px(26) - 2 * P.px(4), "tabs run into the tool buttons: " .. right)
    print("tab widths at the smallest panel: " .. table.concat(widths, " "))
end
wid.window:setBounds(0, 0, 560, 640)
P.render()

-- a section, then a page
find_button("F10 Menu Systems").action()
settle()
eq(wid.crumb.text, "WIKI > F10 Menu Systems", "crumb in a section")
check(wid.back.enabled, "back is on in a section")
local before = #requests
local cargo = find_button("Cargo")
check(cargo, "section lists its pages")
cargo.action()
settle()
check(wid.crumb.text:find("^WIKI > F10 Menu Systems > "), "crumb on a page: " .. wid.crumb.text)
check(#requests > before, "page fetched")
local body = all_text()
check(#body > 200, "page has text")

-- back to the section, and the page again: no new fetch
wid.back:click()
settle()
eq(wid.crumb.text, "WIKI > F10 Menu Systems", "back to the section")
local n = #requests
find_button("Cargo").action()
settle()
local refetched = false
for k = n + 1, #requests do if requests[k].url:find("/api/wiki/pages/", 1, true) then refetched = true end end
check(not refetched, "a page seen before is not fetched again")

-- after a restart (memory gone) it comes from the disk cache, while its
-- date matches the index; a newer date fetches it again
wk.pages, wk.parsed = {}, {}
n = #requests
wid.back:click()
settle()
find_button("Cargo").action()
settle()
refetched = false
for k = n + 1, #requests do if requests[k].url:find("/api/wiki/pages/", 1, true) then refetched = true end end
check(not refetched, "a cached page is read from disk")
check(#all_text() > 200, "the cached page shows")
pages["f10-menu/cargo"].updated_at = "2026-11-01T00:00:00Z"
wk.index_at = -1e9  -- the next tick re-reads the index
settle()
refetched = false
for k = n + 1, #requests do if requests[k].url:find("/api/wiki/pages/f10-menu/cargo", 1, true) then refetched = true end end
check(refetched, "a page changed on the server is fetched again")

-- every seed page renders, with no Markdown left in its text
local bad = 0
for _, e in ipairs(page_list) do
    BFCOCKPIT_TEST.panel.wiki_api.open(e.slug)
    settle()
    local txt = {}
    for _, r in ipairs(visible_rows()) do
        if r.text and r.skin ~= P.skins.mono then
            if r.text:find("**", 1, true) or r.text:find("](", 1, true) or r.text:find("{{", 1, true) or r.text:find("```", 1, true) then
                bad = bad + 1
                print("  markdown left in " .. e.slug .. ": " .. r.text)
            end
        end
        txt[#txt + 1] = r.text or ""
    end
    check(#visible_rows() > 3, "page " .. e.slug .. " renders")
end
eq(bad, 0, "rows with markdown left in them")
check(not table.concat(log, "\n"):find("errored", 1, true), "nothing errored:\n" .. table.concat(log, "\n"):sub(1, 2000))

-- cfg tokens resolve from this server's facts
pages["test/tokens"] = { slug = "test/tokens", title = "Tokens", section = "Test", order = 1, updated_at = "x",
                         content = "An air kill is worth {{cfg:points.air_kill|350}} points, a ground kill {{cfg:points.ground_kill|50}}.\n\nSee [the cargo page](../f10-menu/cargo.md) for crates.\n\n![A diagram](/api/wiki/images/11111111-2222-3333-4444-555555555555)\n\n```steps\n[{\"title\":\"Land\",\"text\":\"Set down **near** the crates.\"},{\"title\":\"Unpack\",\"text\":\"Use F10.\"}]\n```\n\n```cockpitpanel\n{\"tab\":\"CARGO\",\"rows\":[{\"heading\":\"ON BOARD\"},{\"text\":\"Crates 1 / 2\"},{\"button\":\"UNPACK\",\"says\":\"Unpacked a FARP\"}],\"status\":\"Vector Strike 3.5.0\"}\n```\n\n```wheel\n{\"theme\":\"green\",\"items\":[{\"label\":\"Cargo\",\"items\":[{\"label\":\"Unpack\"}]}]}\n```\n\n```dcsmessage\nCrate spawned\nLoad it within 10 minutes\n```\n" }
BFCOCKPIT_TEST.panel.wiki_api.open("test/tokens")
settle()
body = all_text()
check(body:find("worth 400 points, a ground kill 50", 1, true), "tokens substituted: " .. body)
check(body:find("Numbers on this page are Vector Strike 1's", 1, true), "says whose numbers")
check(find_button("the cargo page"), "internal link is a button")
if os.getenv("LUATEST_DEBUG") then print(body) print(table.concat(log, "\n")) end
check(body:find("1. Land", 1, true) and body:find("Set down near the crates.", 1, true), "steps as a numbered list")
check(body:find("[ UNPACK ]", 1, true) and body:find("Unpacked a FARP", 1, true), "cockpit panel rows")
check(body:find("◯ Cargo >", 1, true) and body:find("– Unpack", 1, true), "wheel labels")
check(body:find("Load it within 10 minutes", 1, true), "dcs message")

-- the picture: info, one download into the cache, then a picture row
local ours = {}
for _, d in ipairs(downloads) do
    if d.path:find("11111111-2222-3333-4444-555555555555", 1, true) then ours[#ours + 1] = d end
end
eq(#ours, 1, "one picture downloaded")
local dl = ours[1]
check(dl and dl.path:find("Mods\\Services\\BFCockpit\\cache\\wiki\\img\\11111111-2222-3333-4444-555555555555-", 1, true), "cached under BFCockpit: " .. tostring(dl and dl.path))
check(dl and dl.url:find("/api/cockpit/wiki/image/11111111-2222-3333-4444-555555555555?w=", 1, true), "downloaded from the cockpit route")
local pic
for _, r in ipairs(visible_rows()) do if r.pic then pic = r end end
check(pic, "a picture row")
if pic then
    check(math.abs(pic.w / pic.h - 2) < 0.02, "picture keeps its shape: " .. pic.w .. "x" .. pic.h)
    local sk = pic.skin.skinData.states.released[1].picture
    check(sk.file == dl.path and sk.size.horz == pic.w and sk.size.vert == pic.h, "picture skin points at the file at the row's size")
end
-- opening it again uses the file on disk
wk.images = {}
wk.img_disk = nil
BFCOCKPIT_TEST.panel.wiki_api.open("test/tokens")
settle()
local again = 0
for _, d in ipairs(downloads) do
    if d.path:find("11111111-2222-3333-4444-555555555555", 1, true) then again = again + 1 end
end
eq(again, 1, "the cached picture is not downloaded again")

-- search: titles at once, the text after a pause
local box
for _, c in ipairs(wid.window.children) do if c.kind == "EditBox" then box = c end end
check(box and box.visible, "search box on the wiki tab")
box:type("cargo")
P.render()
eq(wid.crumb.text, "WIKI > Search", "crumb while searching")
check(all_text():find("PAGE TITLES", 1, true), "title matches")
local searched = false
for _ = 1, 20 do frame(0.1) end
for _, r in ipairs(requests) do if r.url:find("/api/cockpit/wiki/search?", 1, true) and r.url:find("q=cargo", 1, true) then searched = true end end
check(searched, "full-text search asked")
check(all_text():find("IN THE TEXT", 1, true) or all_text():find("Nothing else in the text", 1, true), "text matches shown")
wid.back:click()
eq(box.text, "", "back clears the search")

-- leaving the tab gives the keyboard back
box.focused = true
P.select_tab("menu")
eq(box.focused, false, "search box unfocused when the tab changes")
check(not box.visible, "search box hidden on other tabs")

-- a missing page says so
P.select_tab("wiki")
BFCOCKPIT_TEST.panel.wiki_api.open("nope/missing")
settle()
check(all_text():find('There is no page "nope/missing"', 1, true), "missing page")

-- LUATEST_PAGE=<slug> prints that page as the panel lays it out: x offset,
-- kind, text.
local show_page = os.getenv("LUATEST_PAGE")
if show_page then
    BFCOCKPIT_TEST.panel.wiki_api.open(show_page)
    settle()
    for _, r in ipairs(visible_rows()) do
        local kind = r.pic and "PIC " or (r.button and "BTN " or (r.skin == P.skins.mono and "MONO" or "    "))
        print(string.format("%4d %s %s", r.x, kind, r.pic and (r.w .. "x" .. r.h) or r.text))
    end
end

-- ── the DASH tab ──
local DASH = BFCOCKPIT_TEST.dash
check(DASH, "dashboard test hooks")
do
    local d, b = DASH.range_brg(42, 41, 43, 41)
    check(math.abs(d / 1852 - 60) < 0.3, "a degree of latitude is about 60 nm: " .. d / 1852)
    check(math.abs(b) < 1e-6, "due north: " .. b)
    local _, b2 = DASH.range_brg(42, 41, 42, 42)
    check(math.abs(b2 - 89.67) < 0.5, "about east: " .. b2)
    local _, b3 = DASH.range_brg(42, 42, 41.5, 41.5)
    check(b3 > 180 and b3 < 270, "south-west: " .. b3)
    eq(DASH.snap(1), 320, "map width snaps up from the smallest")
    eq(DASH.snap(481), 560, "map width snaps up")
    eq(DASH.snap(5000), 1280, "map width stops at the largest")
end

local function dash_data(rev)
    return {
        round = { number = 12, scenario = "Southern Flank", age = "3h" },
        live = true, restart_in = 5400, updated = "12:34:56Z",
        kpi = { online = 5, flying = 4, blue_obj = 4, red_obj = 3, neutral_obj = 0, at_risk = 2, kills_1h = 1 },
        front = {
            { name = "Sukhumi", kind = "Airbase", owner = "Red", lat = 42.86, lon = 41.13, health = 80,
              rank = 1, tone = "crit", reasons = { "Under attack" } },
            { name = "Stennis", kind = "Carrier Group", owner = "Blue", health = 100,
              rank = 0, tone = "crit", reasons = { "Ready to capture" } },
        },
        kills = { { ago = "2m", killer = "Viper", killer_side = "Blue", airframe = "F-16C_50", weapon = "AIM-120C",
                    victim = "Su-27", victim_side = "Red", air = true } },
        captures = { { ago = "30m", objective = "Senaki", side = "Blue", by = "Viper, Goose" } },
        air = { blue = { online = 4, types = { { type = "F-16C_50", n = 2 }, { type = "A-10C_2", n = 1 } } },
                red = { online = 1, types = { { type = "Su-27", n = 1 } } } },
        weather = { temp_c = 14, wind_speed_kts = 8, wind_from_deg = 270, qnh_hpa = 1013 },
        map = { rev = rev, north = 44, south = 40, west = 38, east = 46, aspect = 0.6667,
                widths = { 320, 400, 480, 560, 640, 800, 960, 1280 } },
    }
end
dash_reply = dash_data("abc123def456")
ctx_reply = { name = "Viper", side = "blue", points = 10, slot = { airframe = "F-16C_50", lat = 42, lon = 42, in_air = true } }
P.select_tab("dash")
local n_dl = #downloads
for _ = 1, 40 do frame(0.1) end
local dtext = all_text()
check(dtext:find("ROUND 12", 1, true) and dtext:find("Southern Flank", 1, true), "round heading:\n" .. dtext:sub(1, 400))
check(dtext:find("restart in 1:%d%d"), "restart countdown")
check(dtext:find("AT RISK", 1, true) and dtext:find("KILLS 1H", 1, true), "headline numbers")
check(dtext:find("BLUE 4", 1, true) and dtext:find("RED 3", 1, true), "territory bar")
check(dtext:find("Sukhumi  Airbase", 1, true) and dtext:find("Under attack", 1, true), "front line")
check(dtext:find("%d+ nm  %d%d%d°"), "range and bearing to a base")
check(dtext:find("Viper (F-16C_50) killed Su-27", 1, true), "kill feed")
check(dtext:find("Senaki taken by BLUE", 1, true), "captures")
check(dtext:find("F-16C_50 ×2", 1, true) and dtext:find("BLUE  4 online, 3 flying", 1, true), "air picture")
check(dtext:find("Wind 270° 8 kt", 1, true) and dtext:find("QNH 1013", 1, true), "weather")
eq(#downloads, n_dl + 1, "one map download")
local mdl = downloads[#downloads]
check(mdl and mdl.path:find("Mods\\Services\\BFCockpit\\cache\\dash\\map-abc123def456-560x373.png", 1, true),
    "map cached under BFCockpit: " .. tostring(mdl and mdl.path))
check(mdl and mdl.url:find("/api/cockpit/dash/map?", 1, true) and mdl.url:find("rev=abc123def456", 1, true)
    and mdl.url:find("w=560", 1, true), "map from the dash route: " .. tostring(mdl and mdl.url))
check(mdl and not mdl.url:find("ckey", 1, true), "the map URL carries no key")
local map_row, marker
for _, r in ipairs(visible_rows()) do
    if r.pic and r.skin and r.skin.skinData then
        local pic = r.skin.skinData.states.released[1].picture
        if pic and mdl and pic.file == mdl.path then map_row = r end
        if pic and pic.file == DASH.MARKER then marker = r end
    end
end
check(map_row, "a map row")
check(marker, "a marker for the player's own position")
if map_row and marker then
    check(math.abs(map_row.h - map_row.w * 0.6667) <= 1, "map keeps its shape: " .. map_row.w .. "x" .. map_row.h)
    -- 42N 42E on a 40-44N, 38-46E map: half way across, half way down
    local cx, cy = marker.x + marker.w / 2, marker.y + marker.h / 2
    check(math.abs(cx - (map_row.x + map_row.w / 2)) <= 1, "marker across: " .. cx)
    check(math.abs(cy - (map_row.y + map_row.h / 2)) <= 1, "marker down: " .. cy)
end
-- the same rev again: no new download; a new rev replaces the file
for _ = 1, 160 do frame(0.1) end
eq(#downloads, n_dl + 1, "same map, not downloaded again")
dash_reply = dash_data("fedcba987654")
for _ = 1, 160 do frame(0.1) end
eq(#downloads, n_dl + 2, "a new map when it changes")
check(io.open(DASH.DASH_DIR .. "map-abc123def456-560x373.png", "rb") == nil, "the old map is deleted")
local newest = io.open(DASH.DASH_DIR .. "map-fedcba987654-560x373.png", "rb")
check(newest ~= nil, "the new map is kept")
if newest then newest:close() end
-- a bfdb without the route
dash_status = 404
BFCOCKPIT_TEST.panel.dash_api.reset()
for _ = 1, 5 do frame(0.1) end
check(all_text():find("no dashboard for the panel yet", 1, true), "an older bfdb says so")
dash_status = 200
-- WEB VIEW: the page in DCS's browser, the native view as the fallback
dash_status = 200
for _ = 1, 160 do frame(0.1) end
do
    local native_b, web_b = DASH.buttons()
    check(native_b and web_b and native_b.visible and web_b.visible, "NATIVE / WEB VIEW buttons on the DASH tab")
    eq(native_b and native_b.skin, P.skins.tab_on, "NATIVE is the default")
    web_b:click()
    frame(0.1)
    local wv = DASH.WEB.view
    check(wv, "WEB VIEW makes the browser")
    eq(P.cfg.dash_web, true, "the choice is saved")
    eq(web_b.skin, P.skins.tab_on, "WEB VIEW lit")
    if wv then
        check(wv.visible and not wid.pane.visible, "the browser takes the pane's place")
        check(wv.w == wid.pane.w and wv.h == wid.pane.h and wv.y == wid.pane.y, "at the pane's bounds")
        check(wv.inserted, "inserted into the window")
        eq(rawget(wv, "url"), nil, "nothing loaded before the browser exists")
        wv.cbs.created()
        check(wv.url and wv.url:find("https://dashboard.vectorstrike.org/?", 1, true) and wv.url:find("server=Test%20Server", 1, true)
            and wv.url:find("theme=dark", 1, true), "loads the hosted dashboard: " .. tostring(wv.url))
        check(wv.url and not wv.url:find("ckey", 1, true), "the page address carries no key")
        wv.cbs.loaded()
        local n = #requests
        for _ = 1, 160 do frame(0.1) end
        eq(DASH.WEB.failed, nil, "a page that loaded is kept")
        local native_fetches = 0
        for i = n + 1, #requests do
            if requests[i].url:find("/api/cockpit/dash?", 1, true) then native_fetches = native_fetches + 1 end
        end
        eq(native_fetches, 0, "the native view does not fetch while the page shows")
        P.hide()
        check(not wv.visible, "the browser hides with the panel")
        P.show()
        frame(0.1)
        check(wv.visible and not wid.pane.visible, "and comes back with it")
        -- a new server: the page loads again, and this time never answers
        local first = rawget(wv, "url")
        wv.url = nil
        BFCOCKPIT_TEST.panel.dash_api.reset()
        frame(0.1)
        check(rawget(wv, "url") ~= nil, "loaded again for a new server")
        for _ = 1, 110 do frame(0.1) end
        check(DASH.WEB.failed and DASH.WEB.failed:find("no answer", 1, true), "given up when the page never answers: " .. tostring(DASH.WEB.failed))
        check(not wv.visible and wid.pane.visible, "back to the native view")
        for _ = 1, 30 do frame(0.1) end
        check(all_text():find("ROUND 12", 1, true), "the native view shows again")
        check(table.concat(log, "\n"):find("dashboard web view given up", 1, true), "logged why")
        -- picking WEB VIEW again tries again; NATIVE turns it off
        web_b:click()
        frame(0.1)
        check(wv.visible and DASH.WEB.failed == nil, "WEB VIEW again retries")
        wv.cbs.failed()
        check(DASH.WEB.failed and not wv.visible, "onMountFailed falls back at once")
        native_b:click()
        eq(P.cfg.dash_web, false, "NATIVE turns it off")
        check(first ~= nil, "first load had an address")
    end
end
check(not table.concat(log, "\n"):find("errored", 1, true), "nothing errored on the DASH tab:\n" .. table.concat(log, "\n"):sub(1, 2000))
P.select_tab("menu")


print(string.format("%d checks, %d failed", checks, failures))
os.execute('rmdir /s /q "' .. TMP .. '" 2>nul')
if failures > 0 then os.exit(1) end
