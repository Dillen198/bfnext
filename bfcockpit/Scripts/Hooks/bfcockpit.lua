-- BFNext cockpit plugin -- an in-game panel that replaces walking the F10
-- radio menu, drawn with DCS's own GUI widgets and fed by the campaign API.
--
-- INSTALL: run bfcockpit-install.exe. It puts these in each DCS profile:
--   Saved Games\DCS\Scripts\Hooks\bfcockpit.lua                (this file)
--   Saved Games\DCS\Mods\Services\BFCockpit\                   (settings page)
--   Saved Games\DCS\Mods\Services\BFCockpit\bin\bfcockpit_net.dll  (HTTPS)
--
-- SETTINGS: Options -> Special -> Vector Strike sets the show/hide key, the
-- comms-key behaviour and the panel size. Everything else lives in
--   Saved Games\DCS\Config\BFCockpit.lua
-- which this script writes on first run; the window's position, size and
-- opacity are saved back into it automatically.
--
-- WHAT IS ON IT:
--   MENU    your whole F10 menu; clicking an item shows exactly what F10 says
--   CARGO   what your aircraft can spawn, how many crates each thing needs,
--           what you are carrying and what is lying nearby
--   STATUS  side, points, lives, crates, aircraft, position, nearest objective
--   EWR     threat / friendly picture, ground intel, EWR on/off and units
--   LOG     every message the server sent you, newest first
--   WIKI    the player wiki: its sections and pages, a search box, the
--           pictures, and this server's own numbers; a link to another
--           page is a button. Works outside multiplayer too
--
-- UPDATES: when the server has a newer version this downloads its installer
-- (from the project's GitHub release for that version) and starts it hidden;
-- it installs when you close DCS. Turn it off on the Special page.
--
-- THE WHEEL: a Flow-style radial menu for doing things without reading a
-- panel. Tap its key (Ctrl+Alt+W unless changed), point at an item with your
-- head (TrackIR / VR) or the mouse, then click it or tap the key again. F10
-- submenus open as their own rings; your pinned F10 commands come first (the
-- star in the MENU tab), then presets for the kind of aircraft you are in.
-- Everything about it is on the Special options page.
--
-- HOW IT IS BUILT, AND WHY: version 1 hosted a web page in DCS's embedded
-- Chromium (WebViewWidget). When that browser comes up changed between DCS
-- builds, a page it cannot load is an empty grey box with no console, and
-- after a month of diagnostics it still showed blank for players. So version 2
-- has no browser at all. The panel is plain dxgui widgets -- Window, Button,
-- Static, ScrollPane, the same ones the briefing and ESC menus use --
-- created in code with skins copied from DCS's own, which render identically
-- in every build and in VR. The approach is the one rkusa/dcs-scratchpad has
-- shipped for years.
--
-- DATA: everything comes from the campaign's HTTPS API (/api/cockpit/*), the
-- same endpoints the web cockpit page uses. DCS's Lua has no TLS, and a
-- blocking request on this thread would freeze the sim, so the requests go
-- through bfcockpit_net.dll (bfcockpit/net in the repo): `request` queues one
-- and returns at once, `poll` hands back whatever has finished. It is polled
-- once per frame and nothing here ever waits on the network.
--
-- SHOWING AND HIDING: a dxgui window that is setVisible(false) stops
-- receiving its hotkeys, which is why version 1 could only be summoned through
-- the comms menu. So the window is created once and never hidden for real:
-- "hidden" is DCS's own near-invisible chat-minimised skin, no cursor, no
-- children, and a 1x1 size so it cannot catch a click meant for a cockpit
-- switch. Its hotkey keeps working from anywhere. The comms key still opens
-- it too: `onShowRadioMenu` is handed to every Hooks script whatever has focus.
--
-- TROUBLESHOOTING: everything logs to Saved Games\DCS\Logs\dcs.log; search it
-- for "BFCOCKPIT".

-- Bump on every change to this file. bfdb reads this exact line out of the
-- copy it was built with to answer /api/cockpit/plugin/version, and the panel
-- compares that with its own to tell the player when to update. Keep the
-- format exactly as it is.
local BFCOCKPIT_VERSION = "3.5.0"

local net = require('net')

local function logmsg(msg)
    net.log("BFCOCKPIT: " .. tostring(msg))
end

logmsg("script loading, version " .. BFCOCKPIT_VERSION)

-- ── Modules ──────────────────────────────────────────────────────────

local function try_require(name)
    local ok, mod = pcall(require, name)
    if not ok then
        logmsg("FATAL: require('" .. name .. "') failed: " .. tostring(mod))
        return nil
    end
    return mod
end

local dxgui      = try_require('dxgui')
local Window     = try_require('Window')
local Static     = try_require('Static')
local Button     = try_require('Button')
local ScrollPane = try_require('ScrollPane')
local Skin       = try_require('Skin')

if not (dxgui and Window and Static and Button and ScrollPane and Skin) then
    logmsg("FATAL: required GUI modules missing, panel disabled")
    return
end

-- The WIKI tab's search box, the module rkusa/dcs-scratchpad types into.
-- Optional: without it the tab has no box and everything else still works.
local EditBox = nil
do
    local ok, mod = pcall(require, 'EditBox')
    if ok and type(mod) == 'table' then EditBox = mod else logmsg("EditBox not available: " .. tostring(mod)) end
end

-- The HTTPS helper lives in the plugin's own folder, next to its settings
-- page. Version 2.0 put it in Scripts\BFCockpit, which is still searched so an
-- old install keeps working until the installer is re-run.
local NET_DIRS = {
    lfs.writedir() .. "Mods\\Services\\BFCockpit\\bin\\",
    lfs.writedir() .. "Scripts\\BFCockpit\\",
}
for _, d in ipairs(NET_DIRS) do package.cpath = package.cpath .. ";" .. d .. "?.dll" end
local http
-- Set when the DLL was built for another version of this script (its build
-- stamp, bfcockpit/net/build.rs). The installer always ships a matching
-- pair; a mismatch is a hand-copied or half-updated install, which may lack
-- what this script uses. Shown in the panel's status line.
local net_mismatch = nil
do
    local ok, mod = pcall(require, 'bfcockpit_net')
    if ok and type(mod) == 'table' then
        http = mod
        local built_for = mod.script_version
        logmsg("bfcockpit_net " .. tostring(mod.version) .. " loaded (built for script "
            .. tostring(built_for) .. ", commit " .. tostring(mod.git) .. ")")
        if type(built_for) ~= 'string' then
            net_mismatch = "bfcockpit_net.dll is older than this script"
        elseif built_for ~= BFCOCKPIT_VERSION then
            net_mismatch = "bfcockpit_net.dll is from version " .. built_for .. ", this script is " .. BFCOCKPIT_VERSION
        end
        if net_mismatch then logmsg("WARNING: " .. net_mismatch .. " -- re-run the Vector Strike installer") end
    else
        logmsg("bfcockpit_net.dll not loaded (" .. tostring(mod) .. ") -- expected in " .. NET_DIRS[1])
    end
end

-- ── Settings ─────────────────────────────────────────────────────────

local CONFIG_PATH = lfs.writedir() .. "Config\\BFCockpit.lua"

local DEFAULTS = {
    -- The campaign API. Point it at http://127.0.0.1:8880 if you run bfdb
    -- yourself.
    api = "https://api.vectorstrike.org",

    -- Pin to one bfdb instance id (e.g. "vs1") instead of letting the server
    -- name decide. Leave nil unless the panel shows the wrong server. A pin
    -- also tells the panel to trust whatever server you are on with its key
    -- (see "Panel key" in the script), so take it out before playing elsewhere.
    instance = nil,

    -- 0.3 (ghost) .. 1.0 (solid).
    opacity = 0.95,

    -- Used only until the Special options page has been saved once; after
    -- that the page wins.
    open_on_radio_menu = true,
    toggle_key = "ctrl+alt+c",

    -- "auto" reads VR.enable from your Config/options.lua; "on"/"off" force it.
    vr = "auto",

    -- Size of everything in the panel, until the Special page sets one.
    scale = nil,

    -- The wheel, until the Special page sets these.
    wheel_key = "ctrl+alt+w",
    wheel_head = true,
    wheel_reach = 10,   -- degrees of head turn to reach the ring
    wheel_dwell = 0,    -- seconds of looking that fires an item; 0 = off
    wheel_size = 1.0,
    wheel_theme = "green",  -- green | blue | amber | red | white
    wheel_opacity = 0.9,
    wheel_open_at = "center", -- center | cursor (always centre in VR)
    wheel_nav = "arrows",     -- keys while the wheel is open: arrows | numpad | ctrlalt | off

    -- Wheels built in the panel's WHEEL tab, per airframe type.
    wheels = {},

    -- F10 commands pinned to the wheel, per airframe type:
    -- { ["UH-1H"] = { { label = "...", path = { "Cargo", "..." } }, ... } }
    wheel_pins = {},

    -- Remembered geometry. nil = pick a sensible size and centre it.
    window = { x = nil, y = nil, w = nil, h = nil },

    debug = false,
}

local cfg = {}

local function deep_copy(t)
    if type(t) ~= 'table' then return t end
    local out = {}
    for k, v in pairs(t) do out[k] = deep_copy(v) end
    return out
end

-- Fill in anything the user's config file omits, so adding a setting to this
-- script never breaks an existing install.
local function apply_defaults(into, defaults)
    for k, v in pairs(defaults) do
        if type(v) == 'table' then
            if type(into[k]) ~= 'table' then into[k] = {} end
            apply_defaults(into[k], v)
        elseif into[k] == nil then
            into[k] = v
        end
    end
end

local function serialize(value, indent)
    indent = indent or ""
    local t = type(value)
    if t == 'string' then
        return string.format("%q", value)
    elseif t == 'number' or t == 'boolean' then
        return tostring(value)
    elseif t == 'table' then
        local inner = indent .. "    "
        local keys = {}
        for k in pairs(value) do keys[#keys + 1] = k end
        table.sort(keys, function(a, b) return tostring(a) < tostring(b) end)
        local parts = { "{\n" }
        for _, k in ipairs(keys) do
            -- Numbers stay numbers, or a saved list comes back with "1", "2"
            -- keys and ipairs sees nothing in it.
            local key = type(k) == 'number' and tostring(k) or string.format("%q", tostring(k))
            parts[#parts + 1] = inner .. "[" .. key .. "] = "
                .. serialize(value[k], inner) .. ",\n"
        end
        parts[#parts + 1] = indent .. "}"
        return table.concat(parts)
    end
    return "nil"
end

local function load_config()
    cfg = deep_copy(DEFAULTS)

    local chunk, err = loadfile(CONFIG_PATH)
    if not chunk then
        local msg = tostring(err)
        -- Lua 5.1's loadfile says "no file" / "cannot open" on the normal
        -- first run, when there is simply nothing there yet.
        if not (string.find(msg, "no file", 1, true)
                or string.find(msg, "No such file", 1, true)
                or string.find(msg, "cannot open", 1, true)) then
            logmsg("config load failed, using defaults: " .. msg)
        end
        return
    end

    -- Sandboxed: the file should only assign a table, and an error in it
    -- must not take the panel down with it.
    local env = {}
    setfenv(chunk, env)
    local ok, run_err = pcall(chunk)
    if not ok then
        logmsg("config file errored, using defaults: " .. tostring(run_err))
        return
    end

    local user = env.cockpit or env.BFCockpit or env.cfg
    if type(user) ~= 'table' then
        logmsg("config file defined no `cockpit` table, using defaults")
        return
    end

    for k, v in pairs(user) do cfg[k] = deep_copy(v) end

    -- Version 1 stored the web page's address (".../cockpit"); the API lives
    -- on the same host. Carry a custom one over rather than silently pointing
    -- a self-hosted install back at the public server.
    if user.api == nil and type(user.url) == 'string' then
        local origin = user.url:match("^(https?://[^/]+)")
        if origin then cfg.api = origin end
    end
    -- Version 1 settings that no longer mean anything.
    cfg.url, cfg.hotkeys, cfg.open_on_start = nil, nil, nil

    apply_defaults(cfg, DEFAULTS)
    logmsg("config loaded from " .. CONFIG_PATH)
end

local function save_config()
    local ok, err = pcall(function()
        local f, open_err = io.open(CONFIG_PATH, "w")
        if not f then error(tostring(open_err)) end
        f:write("-- BFNext cockpit plugin settings.\n")
        f:write("-- Written by Scripts/Hooks/bfcockpit.lua; safe to hand-edit.\n")
        f:write("-- The key, comms behaviour and size are set in Options -> Special -> Vector Strike.\n")
        f:write("-- Window position, size and opacity are updated automatically.\n\n")
        f:write("cockpit = " .. serialize(cfg) .. "\n")
        f:close()
    end)
    if not ok then
        logmsg("could not write config: " .. tostring(err))
    elseif cfg.debug then
        logmsg("config saved")
    end
end

load_config()

-- ── DCS's own options ────────────────────────────────────────────────
-- Config\options.lua holds both the VR switch and what the player chose on
-- Options -> Special -> Vector Strike (options.plugins["BFCockpit"], see
-- Mods\Services\BFCockpit\Options\optionsDb.lua). Read as a file rather than
-- through DCS's options module, which this Lua state does not always have.
local function read_dcs_options()
    local chunk = loadfile(lfs.writedir() .. "Config\\options.lua")
    if not chunk then return nil end
    local env = {}
    setfenv(chunk, env)
    if not pcall(chunk) then return nil end
    return type(env.options) == 'table' and env.options or nil
end

local function detect_vr(opts)
    if cfg.vr == "on" then return true end
    if cfg.vr == "off" then return false end
    local vr = opts and opts.VR
    if type(vr) ~= 'table' then return false end
    -- ED has spelled this both ways across versions.
    return vr.enable == true or vr.enabled == true
end

-- What is in force right now, from the Special page when it has been saved,
-- otherwise from BFCockpit.lua.
local settings = {}

local function compute_settings()
    local opts = read_dcs_options()
    local p = opts and type(opts.plugins) == 'table' and opts.plugins["BFCockpit"] or nil
    if type(p) ~= 'table' then p = nil end
    local s = { vr = detect_vr(opts), from_special = p ~= nil }
    -- Options -> System -> Scale GUI.
    s.gui_scale = opts and type(opts.graphics) == 'table' and tonumber(opts.graphics.scaleGui) or 1

    -- "[ctrl+][alt+][shift+]key" from a key dropdown and its three boxes.
    local function key_from(key, ctrl, alt, shift, fallback)
        if p and type(p[key]) == 'string' and p[key] ~= "" then
            local parts = {}
            if p[ctrl] ~= false then parts[#parts + 1] = "ctrl" end
            if p[alt] ~= false then parts[#parts + 1] = "alt" end
            if p[shift] == true then parts[#parts + 1] = "shift" end
            parts[#parts + 1] = p[key]
            return table.concat(parts, "+")
        end
        if type(fallback) == 'string' and fallback ~= "" then return fallback end
        return nil
    end
    local function opt(name, fallback)
        if p and p[name] ~= nil then return p[name] end
        return fallback
    end

    s.toggle_key = key_from("toggleKey", "toggleCtrl", "toggleAlt", "toggleShift", cfg.toggle_key)
    s.wheel_key = key_from("wheelKey", "wheelCtrl", "wheelAlt", "wheelShift", cfg.wheel_key)
    if s.wheel_key and s.wheel_key == s.toggle_key then
        logmsg("the wheel and the panel have the same key (" .. s.wheel_key .. "); the panel keeps it")
        s.wheel_key = nil
    end
    s.open_on_comms = opt("openOnComms", cfg.open_on_radio_menu) ~= false
    s.auto_update = opt("autoUpdate", cfg.auto_update) ~= false
    s.wheel_head = opt("wheelHead", cfg.wheel_head) ~= false
    s.wheel_reach = math.max(3, tonumber(opt("wheelReach", cfg.wheel_reach)) or 10)
    s.wheel_dwell = math.max(0, tonumber(opt("wheelDwell", cfg.wheel_dwell)) or 0)
    s.wheel_size = math.max(0.5, math.min(2.0, tonumber(opt("wheelSize", cfg.wheel_size)) or 1.0))
    s.wheel_theme = tostring(opt("wheelTheme", cfg.wheel_theme) or "green")
    s.wheel_opacity = math.max(0.4, math.min(1.0, tonumber(opt("wheelOpacity", cfg.wheel_opacity)) or 0.9))
    s.wheel_open_at = tostring(opt("wheelOpenAt", cfg.wheel_open_at) or "center")
    s.wheel_nav = tostring(opt("wheelNav", cfg.wheel_nav) or "arrows")

    local scale = p and tonumber(p.uiScale) or nil
    if not scale or scale <= 0 then scale = tonumber(cfg.scale) end
    if not scale or scale <= 0 then
        -- Auto: larger in VR; smaller on a small screen, where 100% would
        -- cover too much of it (a 1366x768 laptop, a 1280x800 Deck).
        local ok, _, sh = pcall(function() return dxgui.GetScreenSize() end)
        sh = ok and tonumber(sh) or 1080
        scale = s.vr and 1.35 or (sh < 820 and 0.8) or (sh < 1000 and 0.9) or 1.0
    end
    s.scale = math.max(0.5, math.min(2.5, scale))
    -- Text on its own: smaller or larger writing at the same layout.
    s.text_scale = math.max(0.6, math.min(1.5, tonumber(opt("textScale", cfg.text_scale)) or 1.0))
    return s
end

settings = compute_settings()
logmsg(string.format("VR %s, scale %.2f, key %s, wheel key %s, comms key %s (from %s)",
    settings.vr and "on" or "off", settings.scale, tostring(settings.toggle_key), tostring(settings.wheel_key),
    settings.open_on_comms and "opens it" or "does not", settings.from_special and "Special options" or "BFCockpit.lua"))

local function px(n) return math.floor(n * settings.scale + 0.5) end

-- ── Skins ────────────────────────────────────────────────────────────
-- Built from DCS's own skins, deep-copied (Skin.x() returns the cached table
-- every other DCS window shares) and recoloured. Starting from a real skin
-- rather than writing one from nothing means any field this file does not
-- set still has a value DCS knows how to draw.

local C = {
    bg        = "0x0d1009f2",
    header    = "0x1a2212ff",
    text      = "0xcdd4beff",
    dim       = "0x7d8a68ff",
    accent    = "0x8ec83fff",
    bad       = "0xe06c6cff",
    warn      = "0xe0a050ff",
    blue      = "0x6fa8dcff",
    btn       = "0x27341eff",
    btn_hover = "0x364a28ff",
    btn_off   = "0x14190fff",
    ink       = "0x0d1009ff",
    border    = "0x3a4a2eff",
    answer    = "0x1d2a14ff",
}

local FONT      = "DejaVuLGCSansCondensed.ttf"
local FONT_BOLD = "DejaVuLGCSansCondensed-Bold.ttf"
local FONT_MONO = "DejaVuLGCSansMono.ttf"

local function text_style(color, font, size, halign)
    return {
        blur = 0,
        color = color,
        font = font,
        -- Every font in the panel and the wheel comes through here, so the
        -- Text size setting is applied once, at this point.
        fontSize = math.max(7, math.floor(size * (settings.text_scale or 1) + 0.5)),
        horzAlign = { type = halign or "min" },
        vertAlign = { type = "middle", offset = 0 },
        shadowOffset = { horz = 0, vert = 0 },
    }
end

local skins = {}
local FONT_PX = 13

local function build_skins()
    local hdr = px(24)

    -- Window: dark body, our header colours, no close button (the panel has
    -- its own) and a header tall enough to grab in VR.
    local w = deep_copy(Skin.windowSkin())
    local sd = w.skinData
    sd.params.headerHeight = hdr
    sd.states.released[1].bkg = { center_center = C.bg }
    local h = sd.skins.header.skinData
    h.params.hasCloseButton = false
    for _, st in pairs(h.states) do
        for i in pairs(st) do
            st[i].bkg = { center_center = C.header }
            st[i].text = text_style(C.accent, FONT_BOLD, px(13))
        end
    end
    skins.window = w

    -- What the window wears while "hidden". DCS's own, so it is known to
    -- draw nothing visible (scratchpad has relied on it for years).
    skins.hidden = Skin.windowSkinChatMin()

    local function button(bg, fg, hover_bg, pressed_bg, pressed_fg, font, halign)
        local b = deep_copy(Skin.buttonSkin())
        local st = b.skinData.states
        local function state(bkg, color)
            return { [1] = { bkg = { center_center = bkg },
                             text = text_style(color, font, px(FONT_PX), halign) } }
        end
        st.released = state(bg, fg)
        st.hover    = state(hover_bg, fg)
        st.pressed  = state(pressed_bg, pressed_fg)
        st.disabled = state(C.btn_off, C.dim)
        return b
    end
    -- Menu rows read left to right like F10 does; toolbar buttons centre.
    skins.row      = button(C.btn, C.text, C.btn_hover, C.accent, C.ink, FONT, "min")
    skins.row_sub  = button(C.btn, C.accent, C.btn_hover, C.accent, C.ink, FONT_BOLD, "min")
    skins.tool     = button(C.btn, C.text, C.btn_hover, C.accent, C.ink, FONT_BOLD, "middle")
    skins.tab_on   = button(C.accent, C.ink, C.accent, C.accent, C.ink, FONT_BOLD, "middle")
    skins.go       = button(C.accent, C.ink, C.btn_hover, C.text, C.ink, FONT_BOLD, "middle")

    local function static(color, font, size, bkg)
        local s = deep_copy(Skin.staticSkin())
        s.skinData.params.textWrapping = false
        for _, st in pairs(s.skinData.states) do
            for i in pairs(st) do
                st[i].text = text_style(color, font, size)
                if bkg then st[i].bkg = { center_center = bkg } end
            end
        end
        return s
    end
    skins.text   = static(C.text, FONT, px(FONT_PX))
    skins.label  = static(C.dim, FONT, px(FONT_PX))
    skins.head   = static(C.accent, FONT_BOLD, px(14))
    skins.bad    = static(C.bad, FONT, px(FONT_PX))
    skins.warn   = static(C.warn, FONT, px(FONT_PX))
    skins.good   = static(C.accent, FONT, px(FONT_PX))
    skins.blue   = static(C.blue, FONT_BOLD, px(FONT_PX))
    skins.red    = static(C.bad, FONT_BOLD, px(FONT_PX))
    skins.mono   = static(C.text, FONT_MONO, px(12))
    skins.status = static(C.dim, FONT, px(12))
    skins.answer = static(C.text, FONT, px(FONT_PX), C.answer)
    skins.h1     = static(C.accent, FONT_BOLD, px(16))
    skins.bold   = static(C.text, FONT_BOLD, px(FONT_PX))

    -- The WIKI tab's search box: DCS's own edit box, in the panel's colours.
    skins.edit = nil
    pcall(function()
        local e = deep_copy(Skin.editBoxSkin())
        for _, st in pairs(e.skinData.states) do
            for i in pairs(st) do
                st[i].bkg = { center_center = C.btn }
                -- [2] is the hint ("Find a page...")
                st[i].text = text_style(i == 1 and C.text or C.dim, FONT, px(FONT_PX))
            end
        end
        local caret = type(e.skinData.skins) == 'table' and e.skinData.skins.caret or nil
        local cs = caret and caret.skinData and caret.skinData.states
        if cs and cs.released and cs.released[1] then cs.released[1].bkg = { center_center = C.accent } end
        e.skinData.params.selectionColor = C.btn_hover
        skins.edit = e
    end)

    local p = deep_copy(Skin.scrollPaneSkin())
    for _, st in pairs(p.skinData.states) do
        for i in pairs(st) do
            local bkg = st[i].bkg
            if type(bkg) == 'table' then
                for k, v in pairs(bkg) do
                    if type(v) == 'string' then bkg[k] = C.border end
                end
            end
        end
    end
    skins.pane = p

    -- The Vector Strike emblem, from the plugin's Theme folder. Only when the
    -- file is there: an install made by hand may not have it, and a missing
    -- picture is better than a broken one.
    local emblem = lfs.writedir() .. [[Mods\Services\BFCockpit\Theme\emblem-64.png]]
    local has_emblem = false
    pcall(function() has_emblem = lfs.attributes(emblem, "mode") == "file" end)
    skins.emblem = nil
    if has_emblem then
        local e = deep_copy(Skin.staticSkin())
        for _, st in pairs(e.skinData.states) do
            for i in pairs(st) do
                st[i].text = text_style(C.text, FONT, px(FONT_PX))
                st[i].picture = {
                    file = emblem,
                    color = "0xffffffff",
                    horzAlign = { type = "middle", offset = 0 },
                    vertAlign = { type = "middle", offset = 0 },
                    size = { horz = px(18), vert = px(18) },
                    userTexSampler = 0,
                }
            end
        end
        skins.emblem = e
        local big = deep_copy(e)
        for _, st in pairs(big.skinData.states) do
            for i in pairs(st) do
                st[i].picture.size = { horz = px(52 * settings.wheel_size), vert = px(52 * settings.wheel_size) }
            end
        end
        skins.emblem_big = big
    end
end

-- ── Who and where we are ─────────────────────────────────────────────

-- The name the server advertises in the server list. bfdb matches it against
-- each instance's `dcs_server_name`, which is how one bfdb fronting several
-- DCS servers hands back the right campaign. Read the way DCS's own role
-- screen reads it (mul_select_role.lua). NOT player 1's name: that is the
-- account hosting the server ("Vector Admin" on Vector Strike, seen live
-- 2026-10-20), which version 1 sent and bfdb rightly did not recognise.
local function server_name()
    local ok, name = pcall(function()
        local s = net.get_server_settings()
        return type(s) == 'table' and s.name or nil
    end)
    if ok and type(name) == 'string' and name ~= "" then return name end
    return nil
end

local function in_multiplayer()
    local ok, mp = pcall(DCS.isMultiplayer)
    return ok and mp == true
end

-- ── Panel key ────────────────────────────────────────────────────────
-- The player id DCS gives this connection is a small counter anybody can
-- guess, so on its own it proves nothing. The panel makes up a random key,
-- tells the campaign engine about it in a chat line sent as you (the server
-- swallows "-cockpitkey" lines before anyone sees them, and never logs them),
-- and quotes it on every request. Only a client that can send chat as you can
-- register a key for you. See bflib/src/cockpit.rs, "Overlay keys".
--
-- The chat line goes only to a server the campaign API knows (see "Is this a
-- Vector Strike server?" below): on anybody else's server it would be an
-- ordinary chat line, in front of everyone and in their logs.
--
-- The key comes from the OS's CSPRNG through bfcockpit_net.dll. A DLL older
-- than 3.4 has no random_key, and then Lua 5.1's math.random has to do: it is
-- not a strong generator, so the seed mixes several things only this machine
-- knows at this moment. bfdb rate-limits wrong keys either way.
local KEY_CHARS = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789"
local KEY_LEN = 40
local cockpit_key = nil
-- os.clock() after which the engine will have seen the key: the chat line is
-- handled on the server's next frame, and a request that beat it would be
-- turned away.
local key_ready_at = nil
local key_sent_at = -1e9

local server_confirmed  -- forward, see "Is this a Vector Strike server?"

local function weak_key()
    local addr = tonumber(tostring({}):match("(%x+)$") or "0", 16) or 0
    local seed = (os.time() * 1000003 + math.floor(os.clock() * 1000000) + addr) % 2147483647
    math.randomseed(seed)
    -- Throw away the first few outputs: MSVC's rand() barely moves on them.
    for _ = 1, 10 do math.random() end
    local out = {}
    for i = 1, KEY_LEN do
        if i % 10 == 0 then
            math.randomseed((math.random(1, 2147483646) + math.floor(os.clock() * 1000000)) % 2147483647)
        end
        local n = math.random(1, #KEY_CHARS)
        out[i] = KEY_CHARS:sub(n, n)
    end
    return table.concat(out)
end

local function new_key()
    if http and http.random_key then
        local ok, key, err = pcall(http.random_key, KEY_LEN)
        if ok and type(key) == 'string' then return key end
        logmsg("random_key failed (" .. tostring(ok and err or key) .. "), using math.random")
    end
    return weak_key()
end

-- (Re)send our key. At most every 15 s however often it is asked for, so a
-- server that keeps refusing cannot turn this into chat spam.
local function register_key()
    if not server_confirmed() then return end
    local now = os.clock()
    if now - key_sent_at < 15 then return end
    if not cockpit_key then cockpit_key = new_key() end
    key_sent_at = now
    local ok, err = pcall(function()
        net.send_chat("-cockpitkey " .. cockpit_key, false)
    end)
    if ok then
        key_ready_at = now + 1.5
    else
        logmsg("could not register panel key: " .. tostring(err))
    end
end

-- What "Is this a Vector Strike server?" found out, for the server `name`.
local function fresh_server_check()
    return { name = nil, answer = nil, asking = false, retry_at = -1e9 }
end
local server_check = fresh_server_check()

local function forget_key()
    cockpit_key, key_ready_at, key_sent_at = nil, nil, -1e9
    server_check = fresh_server_check()
end

-- ── API client ───────────────────────────────────────────────────────

local function urlencode(s)
    return (string.gsub(tostring(s or ""), "([^%w%-%.%_%~])", function(c)
        return string.format("%%%02X", string.byte(c))
    end))
end

local pending = {}  -- request id -> callback(data, err, status)
local pending_raw = {}  -- id -> callback(result), for http.update

-- The key goes in an X-Cockpit-Key header, which stays out of proxy and access
-- logs. In the URL (`?ckey=`) only when the DLL is too old to send headers,
-- or this server's bfdb is too old to read them (see api_call).
local key_in_query = not (http and http.headers)

-- `params.instance` / `params.server` replace the one this adds itself.
local function api_url(path, params, authed)
    local parts = { (tostring(cfg.api):gsub("/+$", "")), path }
    local sep = "?"
    local function add(k, v)
        if v == nil then return end
        parts[#parts + 1] = sep .. k .. "=" .. urlencode(v)
        sep = "&"
    end
    if authed then
        local ok, id = pcall(net.get_my_player_id)
        add("playerid", ok and id or nil)
        if key_in_query then add("ckey", cockpit_key) end
    end
    if params and (params.instance or params.server) then
        -- named by the caller: added with the other params below
    elseif cfg.instance then
        add("instance", cfg.instance)
    else
        add("server", server_name())
    end
    add("plugin", BFCOCKPIT_VERSION)
    for k, v in pairs(params or {}) do add(k, v) end
    return table.concat(parts)
end

-- cb(data, err, status): data is the decoded JSON on a 2xx, otherwise err is
-- a sentence fit to show the player.
local function api_call(method, path, params, body, authed, cb)
    if not http then
        cb(nil, "bfcockpit_net.dll is missing -- re-run the Vector Strike installer", 0)
        return
    end
    local json = nil
    if body ~= nil then
        local ok, s = pcall(net.lua2json, body)
        if not ok then cb(nil, "could not encode request: " .. tostring(s), 0) return end
        json = s
    end
    local url = api_url(path, params, authed)
    if cfg.debug then logmsg(method .. " " .. url:gsub("ckey=[^&]*", "ckey=<hidden>")) end
    local headers = nil
    if authed and cockpit_key and http.headers then
        headers = { ["X-Cockpit-Key"] = cockpit_key }
        if not key_in_query then
            -- A bfdb from before the header answers a call whose key it can't
            -- see "this cockpit overlay is out of date (no key)": quote the
            -- key in the URL from then on.
            local inner = cb
            cb = function(data, e, status)
                if status == 401 and not key_in_query and tostring(e):find("(no key)", 1, true) then
                    key_in_query = true
                    logmsg("this server's bfdb does not read X-Cockpit-Key yet -- sending the key in the URL")
                end
                inner(data, e, status)
            end
        end
    end
    local id, err = http.request(method, url, json, 15, headers)
    if not id then
        cb(nil, "request failed: " .. tostring(err), 0)
        return
    end
    pending[id] = cb
end

local function deliver(r)
    local raw = pending_raw[r.id]
    if raw then
        pending_raw[r.id] = nil
        raw(r)
        return
    end
    local cb = pending[r.id]
    pending[r.id] = nil
    if not cb then return end
    if r.status == 0 then
        cb(nil, "can't reach the campaign server (" .. tostring(r.error) .. ")", 0)
        return
    end
    local data = nil
    if r.body and r.body ~= "" then
        local ok, decoded = pcall(net.json2lua, r.body)
        if ok then data = decoded end
    end
    if r.status >= 200 and r.status < 300 then
        cb(data, nil, r.status)
    elseif r.status == 404 and type(data) ~= 'table' then
        -- A route this server's bfdb does not have yet (an older deploy).
        cb(nil, "not supported by this server yet", 404)
    else
        local msg = type(data) == 'table' and data.error or nil
        cb(nil, msg and tostring(msg) or ("server answered HTTP " .. r.status), r.status)
    end
end

local function pump_network()
    if not http then return end
    local ok, done = pcall(http.poll)
    if not ok or type(done) ~= 'table' then return end
    for _, r in ipairs(done) do
        local ok2, err = pcall(deliver, r)
        if not ok2 then logmsg("response handler errored: " .. tostring(err)) end
    end
end

-- ── Is this a Vector Strike server? ──────────────────────────────────
-- Asked of the campaign API before the key goes out over chat, by the name
-- this DCS server advertises -- the name bfdb routes on (`dcs_server_name`):
--  1. /api/cockpit/plugin/version?instance=<name> answers 200 only when that
--     name is one of bfdb's instances (non-public ones too: the cockpit
--     routes are open to them), and 400 "unknown instance" otherwise.
--  2. Failing that, /api/instances: a bfdb with a single campaign that never
--     set its `dcs_server_name` routes every name to that campaign, and can't
--     tell its own server from any other. That one is taken at its word.
-- A pinned `instance` in the config is the player saying which server this
-- is, and skips the question. The answer holds until the next server.

local function server_answer(name, yes, why)
    if server_check.name ~= name then return end
    server_check.asking = false
    if yes == nil then
        -- no answer (network, server error): ask again shortly
        server_check.retry_at = os.clock() + 15
        logmsg("could not ask whether this is a Vector Strike server: " .. tostring(why))
        return
    end
    server_check.answer = yes
    server_check.retry_at = os.clock() + 60
    logmsg((yes and "server confirmed: " or "not sending the panel key: ") .. why)
    if yes then register_key() end
end

local function ask_about_server(name)
    server_check.asking = true
    api_call("GET", "/api/cockpit/plugin/version", { instance = name }, nil, false, function(_, err, status)
        if status == 200 then
            return server_answer(name, true, "the campaign API knows " .. name)
        elseif status ~= 400 then
            return server_answer(name, nil, err)
        end
        -- `?server=<name>` rides along (api_url): a bfdb with several
        -- instances turns an unknown one away with 400 here as well.
        api_call("GET", "/api/instances", nil, nil, false, function(data, err2, status2)
            if status2 == 400 then
                return server_answer(name, false, name .. " is not a server the campaign API knows")
            elseif type(data) ~= 'table' then
                return server_answer(name, nil, err2)
            end
            local rows = type(data.instances) == 'table' and data.instances or {}
            local unnamed = #rows == 1 and type(rows[1]) == 'table'
                and not (type(rows[1].dcs_server_name) == 'string' and rows[1].dcs_server_name ~= "")
            if unnamed then
                server_answer(name, true, "the campaign API has one campaign and no server names")
            else
                server_answer(name, false, name .. " is not a server the campaign API knows")
            end
        end)
    end)
end

-- true once this server is confirmed; otherwise starts (or waits on) the
-- question and returns false.
server_confirmed = function()
    if cfg.instance then return true end
    local name = server_name()
    if not name then return false end
    if server_check.name ~= name then
        server_check = fresh_server_check()
        server_check.name = name
    end
    if server_check.answer == true then return true end
    if not server_check.asking and os.clock() >= server_check.retry_at then
        ask_about_server(name)
    end
    return false
end

-- What the panel says on a server that is not confirmed, or nil.
local function server_refused()
    if cfg.instance or server_check.answer ~= false then return nil end
    return "The campaign API does not know this server (" .. tostring(server_check.name) .. "), so the panel "
        .. "stays off here and does not send its key. If this is a Vector Strike server, set "
        .. "instance = \"<id>\" in Saved Games\\DCS\\Config\\BFCockpit.lua."
end

-- ── Formatting ───────────────────────────────────────────────────────

local function ddm(v, pos, neg, width)
    local hemi = v >= 0 and pos or neg
    v = math.abs(v)
    local d = math.floor(v)
    local m = (v - d) * 60
    return string.format("%s %0" .. width .. "d°%06.3f'", hemi, d, m)
end

-- A JSON field that is really there. Absent Option<> fields arrive as JSON
-- null, and how net.json2lua spells null is not documented -- so test for
-- what a value is rather than for nil.
local function present(v)
    return type(v) == 'string' or type(v) == 'number'
end

local function list(v)
    return type(v) == 'table' and v or {}
end

local function vparts(v)
    local t = {}
    for n in tostring(v):gmatch("%d+") do t[#t + 1] = tonumber(n) end
    return t
end

local function is_newer(a, b)
    local x, y = vparts(a), vparts(b)
    for i = 1, math.max(#x, #y) do
        local p, q = x[i] or 0, y[i] or 0
        if p ~= q then return p > q end
    end
    return false
end

-- Break text into lines that fit `width_px`. dxgui statics clip rather than
-- wrap reliably, and how wide a line may be is a guess either way, so this
-- errs short: DejaVu Sans Condensed averages a little over half its size.
local function wrap(text, width_px, font_px)
    local per_line = math.max(12, math.floor(width_px / ((font_px or px(FONT_PX)) * (settings.text_scale or 1) * 0.55)))
    local out = {}
    for para in (tostring(text or "") .. "\n"):gmatch("(.-)\r?\n") do
        if para == "" then
            out[#out + 1] = ""
        else
            local line = ""
            for word in para:gmatch("%S+") do
                if line == "" then
                    line = word
                elseif #line + 1 + #word <= per_line then
                    line = line .. " " .. word
                else
                    out[#out + 1] = line
                    line = word
                end
                while #line > per_line do
                    out[#out + 1] = line:sub(1, per_line)
                    line = line:sub(per_line + 1)
                end
            end
            out[#out + 1] = line
        end
    end
    while #out > 0 and out[#out] == "" do table.remove(out) end
    return out
end

local function clock_hms(rfc3339)
    return tostring(rfc3339 or ""):match("T(%d%d:%d%d:%d%d)") or ""
end

-- ── Panel state ──────────────────────────────────────────────────────

-- `fetch` is what each tab keeps fresh while it is showing, and how often.
local TABS = {
    { id = "menu",   label = "MENU",   fetch = { "menu", "status" }, every = 5 },
    { id = "cargo",  label = "CARGO",  fetch = { "cargo", "menu" }, every = 5 },
    { id = "status", label = "STATUS", fetch = { "status" },        every = 3 },
    { id = "ewr",    label = "EWR",    fetch = { "ewr" },           every = 5 },
    { id = "log",    label = "LOG",    fetch = {},                  every = 30 },
    -- the wiki fetches for itself (wiki_api.tick): it needs no key
    { id = "wiki",   label = "WIKI",   fetch = {},                  every = 60 },
    { id = "wheel",  label = "WHEEL",  fetch = { "menu", "status" }, every = 5 },
}

-- How often the message feed is read while the panel is open.
local FEED_EVERY = 3
local FEED_KEEP = 150

local state = {
    tab = "menu",
    menu = nil,          -- last menu, an array of {path, name, command, order}
    here = {},           -- path of the submenu being shown
    cargo = nil,         -- last /cargo
    cargo_cat = nil,     -- deployable category being shown, nil = overview
    status = nil,        -- last /context
    ewr = nil,           -- last report text
    ewr_mode = "enemy",  -- "enemy" | "friendly" | "intel"
    feed = {},           -- messages, oldest first
    feed_next = 0,
    feed_off = false,    -- this server has no /messages yet
    unread = 0,          -- "you" messages not yet seen on the LOG tab
    answer = nil,        -- { text, at } -- what the last action was told
    errors = {},         -- fetch name -> message from its last attempt, or nil
    inflight = {},       -- fetch name -> true while out
    next_fetch = 0,
    next_feed = 0,
    note = nil,          -- { text, bad } shown in the status line
    update = nil,        -- newer plugin version, if the server has one
    checked_version = false,
}

local window
local visible = false
local geometry_dirty = false
local w_tabs, w_tools = {}, {}
local w_back, w_crumb, w_pane, w_status
local w_emblem               -- the Vector Strike emblem, bottom left
local w_ewr = {}             -- mode buttons on the EWR tab
local pool = { buttons = {}, texts = {}, pics = {} }
local pool_actions = {}      -- button index -> function for its current row
local render                 -- forward
local render_wheel           -- forward
-- What the WIKI tab (see "Wiki") gives the rest of the panel: crumb, back,
-- create, layout, tick, ... -- one table, for Lua 5.1's 200 locals.
local wiki_api = {}
local create_wheel, destroy_wheel, wheel_key_pressed  -- forward, see "Wheel"
local wheel_update, close_wheel  -- forward, see "Wheel"
local pin_index, toggle_pin   -- forward, see "Wheel"
-- The wheel's state; see "Wheel" below.
local wheel = { open = false, stack = {}, shown = {}, hl = nil }
-- Hotkeys registered on the window: name -> key string, and the function
-- each is bound to (kept, since removing a hotkey needs the same function).
local bound = {}
local hotkey_fns = {}

-- Refresh whichever of the panel and the wheel is up.
local function redraw()
    if visible and render then render() end
    if wheel.open and render_wheel then render_wheel() end
end

-- The status line: the outcome of the last thing the player did, for long
-- enough to read it, then back to the update notice or the version.
local NOTE_SECS = 8

local function paint_note()
    if w_status then
        local t = state.note and state.note.text
            or (state.update and state.updating == "downloading" and ("Downloading version " .. state.update .. "..."))
            or (state.update and state.updating == "waiting" and ("Version " .. state.update .. " is downloaded -- it installs when you close DCS"))
            or (state.update and not settings.auto_update and ("Version " .. state.update .. " is available -- turn on auto-update in Options > Special, or re-run the installer"))
            or (state.update and ("Version " .. state.update .. " is available -- re-run the Vector Strike installer"))
            or (net_mismatch and (net_mismatch .. " -- re-run the Vector Strike installer"))
            or ("Vector Strike " .. BFCOCKPIT_VERSION .. (bound.toggle and ("  ·  " .. bound.toggle:upper() .. " panel") or "")
                .. (bound.wheel and ("  ·  " .. bound.wheel:upper() .. " wheel") or ""))
        pcall(function()
            w_status:setText(t)
            w_status:setSkin((state.note and state.note.bad) and skins.bad
                or (state.update and skins.good) or (net_mismatch and skins.bad) or skins.status)
        end)
    end
end

local function set_note(text, bad)
    state.note = text and { text = text, bad = bad, expires = os.clock() + NOTE_SECS } or nil
    paint_note()
end

local function set_answer(text)
    state.answer = text and { text = text, at = os.date("%H:%M:%S"), clock = os.clock() } or nil
end

-- ── Fetching ─────────────────────────────────────────────────────────

local function tab_def(id)
    for _, t in ipairs(TABS) do if t.id == id then return t end end
end

-- A 401 means the engine does not know our key: a mission restart or an
-- engine reload forgets every key. Re-send it (rate-limited) and let the next
-- poll try again.
local function on_auth_error(status)
    if status == 401 then register_key() end
end

local fetchers = {}

fetchers.menu = function(done)
    api_call("GET", "/api/cockpit/menu", nil, nil, true, function(data, err, status)
        on_auth_error(status)
        if data then state.menu = data end
        done(err)
    end)
end

fetchers.cargo = function(done)
    api_call("GET", "/api/cockpit/cargo", nil, nil, true, function(data, err, status)
        on_auth_error(status)
        if data then state.cargo = data end
        done(err)
    end)
end

fetchers.status = function(done)
    api_call("GET", "/api/cockpit/context", nil, nil, true, function(data, err, status)
        on_auth_error(status)
        if data then state.status = data end
        done(err)
    end)
end

fetchers.ewr = function(done)
    local path, params = "/api/cockpit/ewr/report", { friendly = state.ewr_mode == "friendly" and "true" or "false" }
    if state.ewr_mode == "intel" then path, params = "/api/cockpit/ewr/intel", nil end
    local mode = state.ewr_mode
    api_call("GET", path, params, nil, true, function(data, err, status)
        on_auth_error(status)
        -- A reply for the mode we have since left is not this mode's picture.
        if data and mode == state.ewr_mode then state.ewr = tostring(data.report or "") end
        done(err)
    end)
end

local function fetch(name)
    if state.inflight[name] or not fetchers[name] then return end
    state.inflight[name] = true
    local ok, err = pcall(fetchers[name], function(e)
        state.inflight[name] = nil
        state.errors[name] = e
        redraw()
    end)
    if not ok then
        state.inflight[name] = nil
        logmsg("fetch " .. name .. " errored: " .. tostring(err))
    end
end

-- The message feed: every text message the server sent this player, so the
-- panel can show what F10 answered (crate counts, "you must land first").
local function fetch_feed()
    if state.feed_off or state.inflight.feed then return end
    state.inflight.feed = true
    api_call("GET", "/api/cockpit/messages", { since = tostring(state.feed_next) }, nil, true,
        function(data, err, status)
            state.inflight.feed = nil
            on_auth_error(status)
            if status == 404 then
                state.feed_off = true
                logmsg("this server has no message feed yet")
                return
            end
            if type(data) ~= 'table' then return end
            local first = state.feed_next == 0
            state.feed_next = tonumber(data.next) or state.feed_next
            local newest_you = nil
            for _, m in ipairs(list(data.messages)) do
                if type(m) == 'table' and present(m.text) then
                    state.feed[#state.feed + 1] = m
                    if m.scope == "you" then
                        newest_you = m
                        if not first and state.tab ~= "log" then state.unread = state.unread + 1 end
                    end
                end
            end
            while #state.feed > FEED_KEEP do table.remove(state.feed, 1) end
            -- Something the server said to us since we last looked, and not
            -- just the backlog from before the panel opened: show it where the
            -- player is looking.
            if newest_you and not first then
                set_answer(tostring(newest_you.text))
            end
            if visible and (newest_you or state.tab == "log") then render() end
        end)
end

local function refresh_now()
    state.next_fetch = 0
end

-- Auto-update (on unless turned off in Options -> Special): fetch the
-- installer for the version the server has and start it hidden. It waits
-- for DCS to close, installs into this profile and exits, so the next start
-- is the new version. Once per session; a failure leaves the manual notice.
local function maybe_auto_update()
    if not state.update or state.updating or not settings.auto_update then return end
    if not (http and http.update) then return end
    local id, err = http.update(state.update, lfs.writedir())
    if not id then
        logmsg("auto-update not started: " .. tostring(err))
        state.updating = "failed"
        return
    end
    state.updating = "downloading"
    logmsg("auto-update: fetching " .. state.update)
    pending_raw[id] = function(r)
        if r.status == 200 then
            state.updating = "waiting"
            logmsg("auto-update: " .. state.update .. " " .. tostring(r.body))
        else
            state.updating = "failed"
            logmsg("auto-update failed: " .. tostring(r.error))
        end
        paint_note()
    end
    paint_note()
end

local function check_version()
    if state.checked_version then return end
    state.checked_version = true
    api_call("GET", "/api/cockpit/plugin/version", nil, nil, false, function(data)
        if type(data) == 'table' and data.version and is_newer(data.version, BFCOCKPIT_VERSION) then
            state.update = tostring(data.version)
            logmsg("server has plugin " .. state.update .. ", this is " .. BFCOCKPIT_VERSION)
            maybe_auto_update()
            paint_note()
        end
    end)
end

-- Click an F10 item. The reply is what the handler told the player -- the
-- same text F10 shows in the top right -- and it goes in the answer box.
local function invoke(path)
    set_note("Working...", false)
    api_call("POST", "/api/cockpit/menu/invoke", nil, { path = path }, true, function(data, err, status)
        on_auth_error(status)
        if err then
            set_note(err, true)
            set_answer(err)
        else
            local msg = type(data) == 'table' and present(data.message) and tostring(data.message) or ""
            -- An older engine answers a bare "ok"; its real answer then
            -- arrives through the feed a moment later.
            if msg ~= "" and msg ~= "ok" then set_answer(msg) end
            set_note(nil)
        end
        -- Acting usually changes things: pick the new state up at once, and
        -- read the feed straight away for anything the click set off.
        refresh_now()
        state.next_feed = 0
        redraw()
    end)
end

-- A command in the F10 menu by its label (first match), for the CARGO tab's
-- quick actions -- they are the same F10 commands, not reimplementations.
local function find_command(name)
    for _, it in ipairs(list(state.menu)) do
        if type(it) == 'table' and it.command and it.name == name and type(it.path) == 'table' then
            return it.path
        end
    end
end

local function ewr_post(path, body, what)
    set_note("Working...", false)
    api_call("POST", path, nil, body, true, function(data, err, status)
        on_auth_error(status)
        if err then set_note(err, true) return end
        local said = type(data) == 'table' and (data.state or data.units) or nil
        set_note(what .. (present(said) and (": " .. tostring(said)) or ""), false)
        refresh_now()
    end)
end

-- ── Layout ───────────────────────────────────────────────────────────

local function screen_size()
    local ok, w, h = pcall(dxgui.GetScreenSize)
    if ok and type(w) == 'number' and type(h) == 'number' and w > 0 and h > 0 then
        return w, h
    end
    return 1920, 1080
end

-- In VR a fixed corner is often outside the comfortable field of view, so
-- default to a large centred panel; in 2D keep it modest.
local function default_geometry()
    local sw, sh = screen_size()
    local w, h
    if settings.vr then
        w = math.floor(math.min(sw * 0.5, px(600)))
        h = math.floor(math.min(sh * 0.65, px(660)))
    else
        w = math.floor(math.min(sw * 0.35, px(560)))
        h = math.floor(math.min(sh * 0.6, px(640)))
    end
    return math.floor((sw - w) / 2), math.floor((sh - h) / 2), w, h
end

-- Clamped back on-screen: a remembered position from a different monitor
-- setup must not strand the window somewhere it cannot be reached.
local function geometry()
    local dx, dy, dw, dh = default_geometry()
    local g = cfg.window or {}
    local sw, sh = screen_size()
    -- A size remembered at another Panel size grows or shrinks with it, so
    -- picking 70% makes the window smaller too, not just what is in it.
    local gw, gh, gs = tonumber(g.w), tonumber(g.h), tonumber(g.scale)
    if gw and gh and gs and gs > 0 and math.abs(gs - settings.scale) > 0.001 then
        gw, gh = math.floor(gw * settings.scale / gs + 0.5), math.floor(gh * settings.scale / gs + 0.5)
    end
    -- 500 (it was 460 with six tabs): the narrowest the seven tabs fit at
    -- (bfcockpit/tools/luatest.lua checks it)
    local w = math.max(px(500), math.min(gw or dw, sw))
    local h = math.max(px(300), math.min(gh or dh, sh))
    local x = math.max(0, math.min(tonumber(g.x) or dx, sw - w))
    local y = math.max(0, math.min(tonumber(g.y) or dy, sh - h))
    return x, y, w, h
end

-- The client area, below the header. Children are positioned inside it.
local function client_size()
    local ok, w, h = pcall(function() return window:getClientRectSize() end)
    if ok and type(w) == 'number' and type(h) == 'number' and w > 0 then return w, h end
    local _, _, ww, wh = window:getBounds()
    return ww, wh - px(24)
end

local PAD = 8

-- Roughly how many ems wide an upper-case label is in the bold tab font.
local function label_em(s)
    local em = 0
    for c in tostring(s):gmatch(".") do
        em = em + (((c == "W" or c == "M") and 0.95) or ((c == "I" or c == " ") and 0.42)
            or (c:match("%d") and 0.63) or 0.7)
    end
    return em
end

local function show_only(widgets, on)
    for _, w in pairs(widgets) do w:setVisible(on) end
end

local function layout()
    if not window or not visible then return end
    local cw, ch = client_size()
    local pad, row, gap = px(PAD), px(26), px(4)

    -- Toolbar: opacity and close on the right, tabs share what is left.
    local small = px(26)
    local rx = cw - pad - small
    for i = #w_tools, 1, -1 do
        w_tools[i]:setBounds(rx, pad, small, row)
        rx = rx - small - gap
    end
    -- Each tab as wide as its label needs, out of what is left: seven fit
    -- the narrowest panel only because MENU, EWR, LOG and WIKI give way to
    -- STATUS and CARGO. The widths are a guess at the bold font's glyphs.
    local labels, units = {}, 0
    for i, t in ipairs(TABS) do
        labels[i] = t.id == "log" and state.unread > 0 and ("LOG " .. state.unread) or t.label
        units = units + label_em(labels[i]) + 1.2
    end
    local per = math.max(1, (rx - pad) - gap * #TABS) / units
    local x = pad
    for i, t in ipairs(TABS) do
        local b = w_tabs[t.id]
        local tab_w = math.min(px(80), math.floor(per * (label_em(labels[i]) + 1.2)))
        b:setBounds(x, pad, tab_w, row)
        b:setSkin(state.tab == t.id and skins.tab_on or skins.tool)
        b:setText(labels[i])
        x = x + tab_w + gap
    end

    -- Second row, per tab.
    local y2 = pad + row + px(6)
    local top = y2
    local crumb = (state.tab == "menu") or (state.tab == "cargo" and state.cargo_cat ~= nil) or state.tab == "wiki"
    w_back:setVisible(crumb)
    w_crumb:setVisible(crumb)
    show_only(w_ewr, state.tab == "ewr")

    if crumb then
        w_back:setBounds(pad, y2, px(64), row)
        w_crumb:setBounds(pad + px(72), y2, cw - pad * 2 - px(72), row)
        if state.tab == "menu" then
            w_back:setEnabled(#state.here > 0)
            w_crumb:setText("F10 > " .. (#state.here > 0 and table.concat(state.here, " > ") or "Other"))
        elseif state.tab == "wiki" then
            w_back:setEnabled(wiki_api.can_back and wiki_api.can_back() or false)
            w_crumb:setText(wiki_api.crumb and wiki_api.crumb() or "WIKI")
        else
            w_back:setEnabled(true)
            w_crumb:setText("CARGO > " .. tostring(state.cargo_cat))
        end
        top = y2 + row + px(6)
    elseif state.tab == "ewr" then
        local bw = math.floor((cw - pad * 2 - gap * 2) / 3)
        local modes = { "enemy", "friendly", "intel" }
        for i, m in ipairs(modes) do
            w_ewr[m]:setBounds(pad + (i - 1) * (bw + gap), y2, bw, row)
            w_ewr[m]:setSkin(state.ewr_mode == m and skins.tab_on or skins.tool)
        end
        top = y2 + row + px(6)
    end
    -- The wiki's search box, on a row of its own under the crumb.
    if wiki_api.widgets then show_only(wiki_api.widgets(), state.tab == "wiki") end
    if state.tab == "wiki" and wiki_api.layout then top = top + wiki_api.layout(pad, top, cw, row, gap) end

    local status_h = px(20)
    local em = w_emblem and px(22) or 0
    if w_emblem then w_emblem:setBounds(pad, ch - pad - status_h, em, status_h) end
    w_status:setBounds(pad + em, ch - pad - status_h, cw - pad * 2 - em, status_h)
    w_pane:setBounds(pad, top, cw - pad * 2, math.max(px(40), ch - pad - status_h - px(4) - top))
end

-- ── Content rows ─────────────────────────────────────────────────────
-- The scrolling content is a list of rows drawn from two pools of widgets
-- that grow as needed and are never destroyed: dxgui keeps every callback in
-- a module-level table, so rebuilding rows on every poll would leak, and
-- reusing them also avoids any flicker.

local function pooled_button(i)
    local b = pool.buttons[i]
    if b then return b end
    b = Button.new("")
    b:setSkin(skins.row)
    b:addMouseUpCallback(function()
        local fn = pool_actions[i]
        if fn then
            local ok, err = pcall(fn)
            if not ok then logmsg("row action errored: " .. tostring(err)) end
        end
    end)
    w_pane:insertWidget(b)
    pool.buttons[i] = b
    return b
end

local pic_row_cache = {}

local function pooled_pic(i)
    local t = pool.pics[i]
    if t then return t end
    t = Static.new("")
    w_pane:insertWidget(t)
    pool.pics[i] = t
    return t
end

-- A picture skin for a panel row: an icon tinted the panel's text colour.
local function row_pic_skin(file, w, h)
    local key = file .. w .. h
    local s = pic_row_cache[key]
    if not s then
        s = deep_copy(Skin.staticSkin())
        for _, st in pairs(s.skinData.states) do
            for i in pairs(st) do
                st[i].picture = { file = file, color = C.text, horzAlign = { type = "middle", offset = 0 },
                                  vertAlign = { type = "middle", offset = 0 }, size = { horz = w, vert = w }, userTexSampler = 0 }
            end
        end
        pic_row_cache[key] = s
    end
    return s
end

local function pooled_text(i)
    local t = pool.texts[i]
    if t then return t end
    t = Static.new("")
    t:setSkin(skins.text)
    w_pane:insertWidget(t)
    pool.texts[i] = t
    return t
end

-- The usable width of a content row, for the renderers' wrapping.
local function content_width()
    local pw = select(1, w_pane:getSize())
    -- Leave room for the scroll bar the skin draws on the right.
    return pw - px(38)
end

-- rows: each { text=, skin=, button=bool, action=fn, enabled=bool,
--              x=, w=, h=, same_line=bool, mono=bool }
-- `x`/`w` place a row inside the line (for side-by-side buttons);
-- `same_line` puts it on the previous row's line.
local function draw_rows(rows)
    local width = content_width()
    local button_h, text_h, mono_h, gap = px(26), px(19), px(16), px(3)
    local y, nb, nt, np = px(4), 0, 0, 0
    local line_h = 0
    for _, r in ipairs(rows) do
        local h = r.h or (r.button and button_h) or (r.mono and mono_h) or text_h
        if r.same_line then
            line_h = math.max(line_h, h)
        else
            y = y + line_h
            line_h = h + gap
        end
        local x = px(4) + (r.x or 0)
        local w = r.w or (width - (r.x or 0))
        if r.pic then
            np = np + 1
            local pc = pooled_pic(np)
            pc:setSkin(r.pic_skin or row_pic_skin(r.pic, math.min(w, h) - px(2), h))
            pc:setBounds(x, y, w, h)
            pc:setVisible(true)
        elseif r.button then
            nb = nb + 1
            local b = pooled_button(nb)
            b:setText(r.text or "")
            b:setSkin(r.skin or skins.row)
            b:setBounds(x, y, w, h)
            b:setEnabled(r.enabled ~= false)
            b:setVisible(true)
            pool_actions[nb] = r.action
        else
            nt = nt + 1
            local t = pooled_text(nt)
            t:setText(r.text or "")
            t:setSkin(r.skin or skins.text)
            t:setBounds(x, y, w, h)
            t:setVisible(true)
        end
    end
    for i = nb + 1, #pool.buttons do
        pool.buttons[i]:setVisible(false)
        pool.buttons[i]:setBounds(0, 0, 0, 0)
        pool_actions[i] = nil
    end
    for i = nt + 1, #pool.texts do
        pool.texts[i]:setVisible(false)
        pool.texts[i]:setBounds(0, 0, 0, 0)
    end
    for i = np + 1, #pool.pics do
        pool.pics[i]:setVisible(false)
        pool.pics[i]:setBounds(0, 0, 0, 0)
    end
    pcall(function() w_pane:updateWidgetsBounds() end)
end

local function same_prefix(path, prefix)
    if #path ~= #prefix + 1 then return false end
    for i = 1, #prefix do
        if path[i] ~= prefix[i] then return false end
    end
    return true
end

local function submenu_exists(items, path)
    if #path == 0 then return true end
    for _, it in ipairs(items) do
        if not it.command and #it.path == #path then
            local all = true
            for i = 1, #path do
                if it.path[i] ~= path[i] then all = false break end
            end
            if all then return true end
        end
    end
    return false
end

local function copy_path(p)
    local out = {}
    for i, v in ipairs(p) do out[i] = v end
    return out
end

local function message_rows(text, skin)
    local rows = {}
    for _, line in ipairs(wrap(text, content_width())) do
        rows[#rows + 1] = { text = line, skin = skin or skins.label }
    end
    return rows
end

local function add_text(rows, text, skin)
    for _, line in ipairs(wrap(text, content_width())) do
        rows[#rows + 1] = { text = line, skin = skin or skins.text }
    end
end

-- The answer box: what the last click (or the server since) told the player.
local function add_answer(rows)
    local a = state.answer
    if not a then return end
    rows[#rows + 1] = { text = "  " .. a.at .. "  -- the server says:", skin = skins.label }
    for _, line in ipairs(wrap(a.text, content_width() - px(12))) do
        rows[#rows + 1] = { text = "  " .. line, skin = skins.answer }
    end
    rows[#rows + 1] = { text = "", h = px(6) }
end

local renderers = {}

renderers.menu = function()
    local items = state.menu
    if state.errors.menu and not items then return message_rows(state.errors.menu, skins.bad) end
    if not items then return message_rows("Reading your F10 menu...") end

    -- The submenu we were in can vanish under us (an objective flips, a
    -- crate is picked up). Climb until we are somewhere that exists.
    while #state.here > 0 and not submenu_exists(items, state.here) do
        table.remove(state.here)
    end
    w_crumb:setText("F10 > " .. (#state.here > 0 and table.concat(state.here, " > ") or "Other"))
    w_back:setEnabled(#state.here > 0)

    local rows = {}
    if state.errors.menu then add_text(rows, state.errors.menu, skins.bad) end
    add_answer(rows)
    local any = false
    for _, it in ipairs(items) do
        if type(it.path) == 'table' and same_prefix(it.path, state.here) then
            any = true
            local path = copy_path(it.path)
            if it.command then
                -- The star pins this command to the wheel for this airframe.
                local star_w = px(30)
                local label = tostring(it.name)
                rows[#rows + 1] = { button = true, text = "  " .. label, skin = skins.row,
                                    w = content_width() - star_w - px(4),
                                    action = function() invoke(path) end }
                rows[#rows + 1] = { button = true, same_line = true, x = content_width() - star_w, w = star_w,
                                    text = pin_index(path) and "★" or "☆", skin = skins.tool,
                                    action = function()
                                        toggle_pin(path, label)
                                        render()
                                    end }
            else
                rows[#rows + 1] = { button = true, text = "  " .. tostring(it.name) .. "  >", skin = skins.row_sub,
                                    action = function()
                                        state.here = path
                                        render()
                                    end }
            end
        end
    end
    if not any then
        add_text(rows, #state.here > 0 and "This menu is empty." or "No F10 menu for this slot.", skins.label)
    end
    return rows
end

-- Deployables grouped by the first part of their menu path ("Air Defense",
-- "Logistics", ...), the way F10 groups them.
local function cargo_categories(c)
    local order, by = {}, {}
    for _, d in ipairs(list(c.deployables)) do
        if type(d) == 'table' then
            local cat = (type(d.path) == 'table' and #d.path > 1 and d.path[1]) or "Other"
            if not by[cat] then by[cat] = {}; order[#order + 1] = cat end
            table.insert(by[cat], d)
        end
    end
    return order, by
end

local function crate_rows(rows, cr, indent)
    if type(cr) ~= 'table' then return end
    local width = content_width()
    local req = tonumber(cr.required) or 1
    local spawn = type(cr.spawn) == 'table' and cr.spawn or nil
    local spawn_n = type(cr.spawn_n) == 'table' and cr.spawn_n or nil
    local btn_w = px(84)
    local all_w = px(96)
    local buttons_w = btn_w + ((spawn_n and req > 1 and req <= 9) and (all_w + px(4)) or 0)
    local label = string.format("%s%s  --  need %d%s", indent or "", tostring(cr.name), req,
        present(cr.weight_kg) and string.format(", %d kg each", tonumber(cr.weight_kg) or 0) or "")
    rows[#rows + 1] = { text = label, skin = skins.text, w = width - buttons_w - px(6) }
    local x = width - buttons_w
    rows[#rows + 1] = { button = true, same_line = true, x = x, w = btn_w, skin = skins.tool,
                        text = "SPAWN 1", enabled = spawn ~= nil,
                        action = spawn and function() invoke(copy_path(spawn)) end or nil }
    if spawn_n and req > 1 and req <= 9 then
        local p = copy_path(spawn_n)
        p[#p + 1] = tostring(req)
        rows[#rows + 1] = { button = true, same_line = true, x = x + btn_w + px(4), w = all_w, skin = skins.go,
                            text = "SPAWN ALL " .. req, action = function() invoke(p) end }
    end
end

renderers.cargo = function()
    local c = state.cargo
    if state.errors.cargo and not c then
        local rows = message_rows(state.errors.cargo, skins.bad)
        add_text(rows, "Use MENU > Cargo in the meantime -- every cargo command is there.", skins.label)
        return rows
    end
    if type(c) ~= 'table' then return message_rows("Reading your cargo...") end
    if not c.in_slot then return message_rows("Get in an aircraft to see what it can carry.") end

    local rows = {}
    if state.errors.cargo then add_text(rows, state.errors.cargo, skins.bad) end
    add_answer(rows)
    local width = content_width()

    -- One category of deployables, every crate with how many it needs.
    if state.cargo_cat then
        local _, by = cargo_categories(c)
        local deps = by[state.cargo_cat]
        if not deps then
            state.cargo_cat = nil
            return renderers.cargo()
        end
        for _, d in ipairs(deps) do
            local name = type(d.path) == 'table' and table.concat(d.path, " / ", math.min(2, #d.path)) or tostring(d.name)
            local cost = tonumber(d.cost) or 0
            rows[#rows + 1] = { text = name .. (cost > 0 and string.format("   (%d pts)", cost) or ""), skin = skins.head }
            local total = 0
            for _, cr in ipairs(list(d.crates)) do total = total + (tonumber(cr.required) or 0) end
            if total > 0 then
                rows[#rows + 1] = { text = string.format("Needs %d crate%s in all, unpacked together:", total, total == 1 and "" or "s"), skin = skins.label }
            end
            for _, cr in ipairs(list(d.crates)) do crate_rows(rows, cr, "  ") end
            if type(d.repair) == 'table' then
                rows[#rows + 1] = { text = "  Repair kit (fixes a damaged one):", skin = skins.label }
                crate_rows(rows, d.repair, "  ")
            end
            rows[#rows + 1] = { text = "", h = px(6) }
        end
        return rows
    end

    -- Overview: on board, nearby, quick actions, supply, categories.
    local carrying = list(c.carrying)
    local troops = list(c.troops)
    rows[#rows + 1] = { text = "ON BOARD", skin = skins.head }
    local cap = present(c.crate_slots) and (" / " .. tostring(c.crate_slots)) or ""
    local tcap = present(c.troop_slots) and (" / " .. tostring(c.troop_slots)) or ""
    rows[#rows + 1] = { text = string.format("Crates %d%s    Troops %d%s", #carrying, cap, #troops, tcap), skin = skins.text }
    for _, n in ipairs(carrying) do rows[#rows + 1] = { text = "  • " .. tostring(n), skin = skins.text } end
    for _, n in ipairs(troops) do rows[#rows + 1] = { text = "  • " .. tostring(n) .. " (troops)", skin = skins.text } end

    rows[#rows + 1] = { text = "", h = px(4) }
    rows[#rows + 1] = { text = "NEARBY CRATES", skin = skins.head }
    local nearby = list(c.nearby)
    if #nearby == 0 then
        rows[#rows + 1] = { text = "None close enough to load or unpack.", skin = skins.label }
    end
    for _, n in ipairs(nearby) do
        rows[#rows + 1] = { text = string.format("  %s  --  %d m, bearing %03d°", tostring(n.name),
            tonumber(n.distance_m) or 0, tonumber(n.bearing_deg) or 0), skin = skins.text }
    end

    -- The F10 cargo commands this slot has, two to a line.
    local actions = {
        "Unpack Nearby Crate(s)", "Load Nearby Crate", "Unload Crate",
        "List Cargo", "List Nearby Crates", "Destroy Nearby Crate",
    }
    local half = math.floor((width - px(4)) / 2)
    local col = 0
    for _, name in ipairs(actions) do
        local path = find_command(name)
        if path then
            rows[#rows + 1] = { button = true, same_line = col == 1, x = col * (half + px(4)), w = half,
                                skin = skins.tool, text = name:upper(),
                                action = function() invoke(copy_path(path)) end }
            col = 1 - col
        end
    end

    local supply = list(c.supply)
    if #supply > 0 then
        rows[#rows + 1] = { text = "", h = px(4) }
        rows[#rows + 1] = { text = "BASE SUPPLY", skin = skins.head }
        for _, cr in ipairs(supply) do crate_rows(rows, cr, "") end
    end

    local order, by = cargo_categories(c)
    if #order > 0 then
        rows[#rows + 1] = { text = "", h = px(4) }
        rows[#rows + 1] = { text = "BUILD SOMETHING -- pick a category", skin = skins.head }
        for _, cat in ipairs(order) do
            local n = #by[cat]
            rows[#rows + 1] = { button = true, skin = skins.row_sub,
                                text = string.format("  %s   (%d)  >", cat, n),
                                action = function()
                                    state.cargo_cat = cat
                                    render()
                                end }
        end
    end
    return rows
end

renderers.status = function()
    local s = state.status
    if state.errors.status and not s then return message_rows(state.errors.status, skins.bad) end
    if type(s) ~= 'table' then return message_rows("Reading your status...") end

    local rows = {}
    local lw = px(110)
    local function kv(k, v, vskin)
        rows[#rows + 1] = { text = k, skin = skins.label, w = lw }
        rows[#rows + 1] = { text = tostring(v), skin = vskin or skins.text, x = lw, same_line = true }
    end
    local side = tostring(s.side or "")
    rows[#rows + 1] = { text = tostring(s.name or "?"), skin = skins.head }
    kv("Coalition", side:upper(), side == "blue" and skins.blue or (side == "red" and skins.red or skins.text))
    kv("Points", s.points or 0)
    if type(s.lives) == 'table' and #s.lives > 0 then
        local parts = {}
        for _, l in ipairs(s.lives) do
            if type(l) == 'table' then parts[#parts + 1] = tostring(l[1]) .. " " .. tostring(l[2]) end
        end
        kv("Lives", table.concat(parts, ", "))
    end
    kv("Crates", s.crates or 0)

    local slot = s.slot
    if type(slot) ~= 'table' then
        rows[#rows + 1] = { text = "" }
        rows[#rows + 1] = { text = "Not in an aircraft -- pick a slot to see more.", skin = skins.label }
        return rows
    end
    rows[#rows + 1] = { text = "" }
    rows[#rows + 1] = { text = tostring(slot.airframe or "?") .. "  " .. tostring(slot.unit_name or ""), skin = skins.head }
    kv("Position", ddm(tonumber(slot.lat) or 0, "N", "S", 2) .. "  " .. ddm(tonumber(slot.lon) or 0, "E", "W", 3))
    kv("Altitude", string.format("%d ft", tonumber(slot.alt_ft) or 0))
    kv("Heading", string.format("%03d°  %d kts", tonumber(slot.heading_deg) or 0, tonumber(slot.speed_kts) or 0))
    kv("State", slot.in_air and "Airborne" or "On the ground")
    if present(slot.home_objective) then kv("Home", slot.home_objective) end
    if present(slot.at_objective) then kv("At", slot.at_objective) end
    local n = slot.nearest
    if type(n) == 'table' then
        local owner = tostring(n.owner or "")
        kv("Nearest", string.format("%s (%s) %.1f nm @ %03d°", tostring(n.name), owner,
            (tonumber(n.distance_m) or 0) / 1852, tonumber(n.bearing_deg) or 0),
            owner == "blue" and skins.blue or (owner == "red" and skins.red or skins.text))
    end
    local hold = tonumber(slot.takeoff_ok_in_secs)
    if hold and hold > 0 then
        kv("Takeoff in", string.format("%d:%02d", math.floor(hold / 60), hold % 60), skins.bad)
    end
    if state.errors.status then
        rows[#rows + 1] = { text = "" }
        add_text(rows, state.errors.status, skins.bad)
    end
    return rows
end

renderers.ewr = function()
    local rows = {}
    -- EWR settings: the same switches as F10's EWR menu.
    local width = content_width()
    local third = math.floor((width - px(8)) / 3)
    rows[#rows + 1] = { button = true, x = 0, w = third, skin = skins.tool, text = "EWR CALLS ON/OFF",
                        action = function() ewr_post("/api/cockpit/ewr/toggle", {}, "EWR calls") end }
    rows[#rows + 1] = { button = true, same_line = true, x = third + px(4), w = third, skin = skins.tool, text = "IMPERIAL",
                        action = function() ewr_post("/api/cockpit/ewr/units", { imperial = true }, "EWR units") end }
    rows[#rows + 1] = { button = true, same_line = true, x = (third + px(4)) * 2, w = third, skin = skins.tool, text = "METRIC",
                        action = function() ewr_post("/api/cockpit/ewr/units", { imperial = false }, "EWR units") end }
    rows[#rows + 1] = { text = "", h = px(4) }

    if state.errors.ewr and not state.ewr then
        add_text(rows, state.errors.ewr, skins.bad)
        return rows
    end
    if not state.ewr then
        add_text(rows, state.ewr_mode == "intel" and "Asking for ground intel..." or "Asking the EWR network...", skins.label)
        return rows
    end
    if state.errors.ewr then add_text(rows, state.errors.ewr, skins.bad) end
    local text = state.ewr ~= "" and state.ewr
        or (state.ewr_mode == "intel" and "No ground intel." or "Nothing on the scope.")
    for line in (text .. "\n"):gmatch("(.-)\r?\n") do
        rows[#rows + 1] = { text = line, skin = skins.mono, mono = true }
    end
    return rows
end

local SCOPE_SKIN = { you = "good", side = "blue", all = "text", chat = "label" }
local SCOPE_TAG = { you = "YOU", side = "SIDE", all = "ALL", chat = "CHAT" }

renderers.log = function()
    state.unread = 0
    if state.feed_off then
        return message_rows("This server does not send its messages to the panel yet. They still appear in the top right of your screen as usual.")
    end
    if #state.feed == 0 then
        return message_rows(key_ready_at and "Nothing yet. Everything the server tells you will be listed here." or "Waiting for the server...")
    end
    local rows = {}
    for i = #state.feed, 1, -1 do
        local m = state.feed[i]
        local skin = skins[SCOPE_SKIN[m.scope] or "text"]
        rows[#rows + 1] = { text = clock_hms(m.at) .. "  " .. (SCOPE_TAG[m.scope] or ""), skin = skins.label }
        for _, line in ipairs(wrap(m.text, content_width() - px(12))) do
            rows[#rows + 1] = { text = "  " .. line, skin = skin }
        end
        rows[#rows + 1] = { text = "", h = px(4) }
    end
    return rows
end

render = function()
    if not window or not visible then return end
    local ok, err = pcall(function()
        layout()
        if state.tab == "wiki" then
            -- Public pages: no key and no server check, so the wiki reads
            -- anywhere, single player included.
            draw_rows(renderers.wiki())
            if wiki_api.after_draw then wiki_api.after_draw() end
        -- No server, no engine, no key to register: nothing would ever load.
        elseif not in_multiplayer() then
            draw_rows(message_rows("The panel works on the Vector Strike multiplayer servers. Join one and open it again."))
        elseif server_refused() then
            draw_rows(message_rows(server_refused()))
        else
            draw_rows(renderers[state.tab]())
        end
    end)
    if not ok then logmsg("render errored: " .. tostring(err)) end
end

-- ── Window ───────────────────────────────────────────────────────────

local show, hide, toggle

-- The Panel size the current window was built at: its pixel size belongs to
-- that, not to whatever the setting has just been changed to.
local built_scale = nil

local function remember_geometry()
    if not window or not visible then return end
    local ok, x, y, w, h = pcall(function() return window:getBounds() end)
    if not ok or type(h) ~= 'number' then return end
    cfg.window = { x = x, y = y, w = w, h = h, scale = built_scale or settings.scale }
    geometry_dirty = true
end

local function flush_geometry()
    if not geometry_dirty then return end
    save_config()
    geometry_dirty = false
end

local function tool_button(label, tip, fn)
    local b = Button.new(label)
    b:setSkin(skins.tool)
    pcall(function() b:setTooltipText(tip) end)
    b:addMouseUpCallback(function()
        local ok, err = pcall(fn)
        if not ok then logmsg(tip .. " errored: " .. tostring(err)) end
    end)
    window:insertWidget(b)
    return b
end

local function label_widget(text)
    local s = Static.new(text)
    s:setSkin(skins.label)
    window:insertWidget(s)
    return s
end

local function set_opacity(v)
    cfg.opacity = math.max(0.3, math.min(1.0, v))
    if window and visible then pcall(function() window:setOpacity(cfg.opacity) end) end
    geometry_dirty = true
end

local function select_tab(id)
    if state.tab == id then return end
    -- the search box lets go of the keyboard when the wiki is left
    if id ~= "wiki" and wiki_api.unfocus then wiki_api.unfocus() end
    state.tab = id
    if id == "log" then state.unread = 0 end
    refresh_now()
    render()
end

-- Bind (or re-bind) the panel and wheel keys. Called at creation and whenever
-- the Special options page changes them, so a new key works without a
-- restart. Both live on the panel window, which is never really hidden.
local function bind_keys()
    local want = { toggle = settings.toggle_key, wheel = settings.wheel_key }
    for _, name in ipairs({ "toggle", "wheel" }) do
        local key = want[name]
        if key ~= bound[name] then
            if bound[name] then
                pcall(function() window:removeHotKeyCallback(bound[name], hotkey_fns[name]) end)
                bound[name] = nil
            end
            if type(key) == 'string' and key ~= "" then
                local ok, err = pcall(function() window:addHotKeyCallback(key, hotkey_fns[name]) end)
                if ok then
                    bound[name] = key
                    logmsg(name .. " key " .. key)
                else
                    logmsg("could not bind " .. name .. " key " .. key .. ": " .. tostring(err))
                end
            end
        end
    end
    paint_note()
end

local function unbind_keys()
    for name, key in pairs(bound) do
        pcall(function() window:removeHotKeyCallback(key, hotkey_fns[name]) end)
    end
    bound = {}
end

local function children()
    local list_ = { w_back, w_crumb, w_pane, w_status, w_emblem }
    for _, t in ipairs(TABS) do list_[#list_ + 1] = w_tabs[t.id] end
    for _, b in ipairs(w_tools) do list_[#list_ + 1] = b end
    return list_
end

-- Widgets that only some tabs show; layout() decides which.
local function tab_widgets()
    local list_ = {}
    for _, w in pairs(w_ewr) do list_[#list_ + 1] = w end
    if wiki_api.widgets then
        for _, w in ipairs(wiki_api.widgets()) do list_[#list_ + 1] = w end
    end
    return list_
end

local function create()
    build_skins()
    -- A fresh window gets fresh widgets: create() also runs again when DCS
    -- has thrown the last window away (see `revive`) or the size changed.
    w_tabs, w_tools, w_ewr = {}, {}, {}
    pool, pool_actions = { buttons = {}, texts = {}, pics = {} }, {}
    bound = {}
    local x, y, w, h = geometry()
    built_scale = settings.scale
    window = Window.new(x, y, w, h, "Vector Strike")
    window:setSkin(skins.window)
    do  -- what DCS reports, for diagnosing layout reports from dcs.log
        local sw, sh = screen_size()
        local _, bx, by, bw, bh = pcall(function() return window:getBounds() end)
        local _, cw, ch = pcall(function() return window:getClientRectSize() end)
        logmsg(string.format("window: screen %sx%s, asked %d,%d %dx%d, bounds %s,%s %sx%s, client %sx%s, Scale GUI %s, panel %.2f, text %.2f",
            tostring(sw), tostring(sh), x, y, w, h, tostring(bx), tostring(by), tostring(bw), tostring(bh),
            tostring(cw), tostring(ch), tostring(settings.gui_scale), settings.scale, settings.text_scale or 1))
    end
    window:setDraggable(true)
    window:setResizable(true)
    -- Above the cockpit, below DCS's own modal dialogs.
    window:setZOrder(113)

    for _, t in ipairs(TABS) do
        local id = t.id
        w_tabs[id] = tool_button(t.label, t.label, function() select_tab(id) end)
    end
    w_tools = {
        tool_button("-", "More see-through", function() set_opacity((tonumber(cfg.opacity) or 0.95) - 0.1) end),
        tool_button("+", "More solid", function() set_opacity((tonumber(cfg.opacity) or 0.95) + 0.1) end),
        tool_button("X", "Close", function() hide() end),
    }

    w_back = tool_button("< Back", "Up one level", function()
        if state.tab == "cargo" then
            state.cargo_cat = nil
        elseif state.tab == "wiki" then
            return wiki_api.back()
        elseif #state.here > 0 then
            table.remove(state.here)
        end
        render()
    end)
    w_crumb = label_widget("F10")

    local function ewr_mode(mode)
        return function()
            if state.ewr_mode == mode then return end
            state.ewr_mode = mode
            state.ewr = nil
            refresh_now()
            render()
        end
    end
    w_ewr.enemy = tool_button("THREATS", "Enemy air picture", ewr_mode("enemy"))
    w_ewr.friendly = tool_button("FRIENDLIES", "Friendly air picture", ewr_mode("friendly"))
    w_ewr.intel = tool_button("GROUND INTEL", "Known enemy ground units", ewr_mode("intel"))
    if wiki_api.create then wiki_api.create() end


    w_pane = ScrollPane.new()
    w_pane:setSkin(skins.pane)
    pcall(function() w_pane:setVertScrollBarStep(px(28)) end)
    window:insertWidget(w_pane)

    w_status = Static.new("")
    w_status:setSkin(skins.status)
    window:insertWidget(w_status)

    w_emblem = nil
    if skins.emblem then
        w_emblem = Static.new("")
        w_emblem:setSkin(skins.emblem)
        window:insertWidget(w_emblem)
    end

    window:addSizeCallback(function()
        if not visible then return end
        remember_geometry()
        render()
    end)
    window:addPositionCallback(function() remember_geometry() end)

    -- Must be visible for its hotkey to be live; hide() below only makes it
    -- look and behave hidden.
    window:setVisible(true)
    hotkey_fns.toggle = hotkey_fns.toggle or function() toggle() end
    hotkey_fns.wheel = hotkey_fns.wheel or function() if wheel_key_pressed then wheel_key_pressed() end end
    bind_keys()
    if create_wheel then create_wheel() end

    visible = true
    hide()
    logmsg(string.format("window created (%d,%d %dx%d), hidden until opened", x, y, w, h))
end

hide = function()
    if not window or not visible then return end
    remember_geometry()
    visible = false
    local ok, err = pcall(function()
        if wiki_api.unfocus then wiki_api.unfocus() end
        for _, c in ipairs(children()) do c:setVisible(false) end
        for _, c in ipairs(tab_widgets()) do c:setVisible(false) end
        window:setSkin(skins.hidden)
        window:setHasCursor(false)
        local x, y = window:getPosition()
        window:setBounds(x, y, 1, 1)
    end)
    if not ok then logmsg("hide failed: " .. tostring(err)) end
    if cfg.debug then logmsg("hidden") end
    flush_geometry()
end

show = function()
    if not window then return end
    local mp = in_multiplayer()
    if not visible then
        visible = true
        local ok, err = pcall(function()
            local x, y, w, h = geometry()
            -- Every time, not just at creation: DCS can hide our window
            -- behind our back (see `revive`), and reskinning an invisible
            -- window shows nothing.
            window:setVisible(true)
            window:setSkin(skins.window)
            window:setBounds(x, y, w, h)
            window:setOpacity(tonumber(cfg.opacity) or 0.95)
            window:setHasCursor(true)
            for _, c in ipairs(children()) do c:setVisible(true) end
        end)
        if not ok then logmsg("show failed: " .. tostring(err)) end
        if cfg.debug then logmsg("shown") end
    end
    check_version()
    if mp and not key_ready_at then register_key() end
    paint_note()
    refresh_now()
    state.next_feed = 0
    render()
end

toggle = function()
    if visible then hide() else show() end
end

-- ── Wheel ────────────────────────────────────────────────────────────
-- A Flow-style radial menu. A ring of 2..10 segments around a hub; pointing
-- at a group (an F10 submenu, or a group you made in the WHEEL editor) fans
-- its contents out on an arc just outside that segment, and pushing further
-- out picks one of them -- no clicking into submenus. Picking a group itself
-- opens it as the whole ring, for when it holds more than the arc shows.
--
-- The art is pre-rendered (bfcockpit/tools/wheelgen.py) as white textures in
-- Mods\Services\BFCockpit\Theme\wheel, placed by its layout.lua, and tinted
-- per theme through each picture's colour: one set of images, any colour.
--
-- One pointer, fed by whichever moved last:
--   - the mouse, over a transparent pad that covers the wheel;
--   - the head: where you looked when the wheel opened is the hub; turning
--     your head (TrackIR / VR moves the DCS camera) moves the pointer, the
--     "reach" setting reaching the middle of the ring and twice that the arc.
--     The aircraft's own heading and pitch are taken out.
-- Click, tap the wheel key again, or (optionally) keep looking, to fire what
-- is under it. The hub goes back a level, or closes the wheel at the top.
--
-- What is on it, per airframe type: what you built in the panel's WHEEL tab,
-- or until then a preset for the kind of aircraft plus anything pinned with
-- the star in the MENU tab. Live-info slots show a value instead of firing a
-- command (nearest objective, crates aboard, takeoff hold, points, last
-- message); picking one opens the panel where that comes from.


-- The wheel is its own function: Lua 5.1 allows 200 locals per function,
-- and the file's top level is near that. What the rest of the file needs
-- from it is exported through the forward declarations near the top.
local function wheel_module()
local bind_nav, unbind_nav   -- keys-only control, see "Wheel: keys only"

local WHEEL_DIR = lfs.writedir() .. [[Mods\Services\BFCockpit\Theme\wheel\]]
local WL = nil                -- the generated layout.lua
do
    local f = loadfile(WHEEL_DIR .. "layout.lua")
    if f then
        local ok, l = pcall(f)
        if ok and type(l) == 'table' and type(l.geom) == 'table' then WL = l end
    end
    if not WL then logmsg("wheel art not found in " .. WHEEL_DIR .. " -- re-run the installer") end
end

local THEMES = {
    green = { 0x8e, 0xc8, 0x3f },
    blue  = { 0x4f, 0xa3, 0xe0 },
    amber = { 0xe0, 0xa0, 0x50 },
    red   = { 0xe0, 0x6c, 0x6c },
    white = { 0xd8, 0xdd, 0xe0 },
}
local INK_RGB  = { 0x0d, 0x10, 0x09 }
local TEXT_RGB = { 0xcd, 0xd4, 0xbe }
local DIM_RGB  = { 0x8a, 0x96, 0x78 }
local BAD_RGB  = { 0xe0, 0x6c, 0x6c }

local function hexc(rgb, a)
    a = math.max(0, math.min(255, math.floor((a or 255) + 0.5)))
    return string.format("0x%02x%02x%02x%02x", rgb[1], rgb[2], rgb[3], a)
end

local function accent() return THEMES[settings.wheel_theme] or THEMES.green end

-- Canvas units to pixels. 100% wheel size is 0.75 of the canvas.
local function ws()
    -- Larger in VR: the GUI panel sits a fixed distance from your eyes and
    -- the headset's pixels are bigger, so the default size is hard to read.
    return 0.75 * settings.wheel_size * (settings.vr and 1.3 or 1.0)
end

local ICON_RULES = {
    { "unpack", "crate_open" }, { "unload", "unload" }, { "load", "load" },
    { "destroy", "destroy" }, { "delete", "destroy" }, { "nearby", "radar_sweep" },
    { "list", "list" }, { "spawn", "spawn" }, { "c-130", "transport" },
    { "cargo", "crate" }, { "crate", "crate" }, { "base supply", "hangar" },
    { "troop", "troops" }, { "csar", "csar" }, { "pilot", "csar" },
    { "jtac", "jtac" }, { "target", "target" }, { "smoke", "smoke" },
    { "objective", "flag" }, { "info", "info" }, { "action", "bolt" },
    { "deploy", "launcher" }, { "flight", "jet" }, { "wingman", "jet" },
    { "ewr", "ewr" }, { "gci", "ewr" }, { "recon", "recon" },
    { "repair", "wrench" }, { "fuel", "fuel" }, { "weapon", "bomb" },
    { "logistic", "hangar" }, { "ground", "tank" }, { "helo", "helo" },
}

local function icon_for(label)
    local l = tostring(label or ""):lower()
    for _, r in ipairs(ICON_RULES) do
        if l:find(r[1], 1, true) then return r[2] end
    end
    return "generic"
end

-- The icons the editor cycles through, in the order wheelgen.py made them.
local function icon_list()
    return (WL and type(WL.icons) == 'table') and WL.icons or { "generic" }
end

local PANEL_TABS = {
    menu = { label = "F10 MENU", icon = "panel" },
    cargo = { label = "CARGO", icon = "crate" },
    status = { label = "STATUS", icon = "info" },
    ewr = { label = "EWR", icon = "ewr" },
    log = { label = "LOG", icon = "chat" },
    wheel = { label = "WHEEL EDITOR", icon = "gear" },
    wiki = { label = "WIKI", icon = "list" },
}

-- Live-info slots. value() returns a short label for the ring and a longer
-- line for the hub.
local INFO = {
    nearest = {
        name = "NEAREST OBJECTIVE", icon = "compass", tab = "status",
        value = function()
            local s = state.status
            local n = type(s) == 'table' and type(s.slot) == 'table' and s.slot.nearest or nil
            if type(n) ~= 'table' then return "NO OBJ", "No objective nearby" end
            local nm = (tonumber(n.distance_m) or 0) / 1852
            local brg = tonumber(n.bearing_deg) or 0
            return string.format("%.0fNM %03d°", nm, brg),
                string.format("%s (%s) %.1f nm @ %03d°", tostring(n.name), tostring(n.owner), nm, brg)
        end,
    },
    crates = {
        name = "CRATES ABOARD", icon = "crate", tab = "cargo",
        value = function()
            local c = state.cargo
            if type(c) == 'table' and c.in_slot then
                local on = list(c.carrying)
                local cap = present(c.crate_slots) and ("/" .. tostring(c.crate_slots)) or ""
                return #on .. cap .. " CRATES", #on > 0 and table.concat(on, ", ") or "Nothing aboard"
            end
            local s = state.status
            return tostring(type(s) == 'table' and s.crates or 0) .. " CRATES", ""
        end,
    },
    takeoff = {
        name = "TAKEOFF", icon = "clock", tab = "status",
        value = function()
            local s = state.status
            local h = type(s) == 'table' and type(s.slot) == 'table' and tonumber(s.slot.takeoff_ok_in_secs) or nil
            if h and h > 0 then
                return string.format("WAIT %d:%02d", math.floor(h / 60), h % 60), "Takeoff hold"
            end
            return "CLEARED", "No takeoff hold"
        end,
    },
    points = {
        name = "POINTS", icon = "medal", tab = "status",
        value = function()
            local s = state.status
            if type(s) ~= 'table' then return "-- PTS", "" end
            return tostring(s.points or 0) .. " PTS", tostring(s.side or ""):upper() .. "  " .. tostring(s.name or "")
        end,
    },
    message = {
        name = "LAST MESSAGE", icon = "chat", tab = "log",
        value = function()
            local m = state.feed[#state.feed]
            if not m then return "NO MSG", "Nothing from the server yet" end
            return "MESSAGE", tostring(m.text)
        end,
    },
}
local INFO_ORDER = { "nearest", "crates", "takeoff", "points", "message" }

local function airframe()
    local s = state.status
    local slot = type(s) == 'table' and s.slot or nil
    if type(slot) == 'table' and present(slot.airframe) then return tostring(slot.airframe) end
    return "any"
end

local function path_key(p) return table.concat(p, "\31") end

local function f10_children(path)
    local out = {}
    for _, it in ipairs(list(state.menu)) do
        if type(it) == 'table' and type(it.path) == 'table' and same_prefix(it.path, path) then
            out[#out + 1] = { kind = it.command and "f10" or "f10menu", label = tostring(it.name), path = copy_path(it.path) }
        end
    end
    return out
end

local function f10_menu_exists(path)
    if #path == 0 then return true end
    for _, it in ipairs(list(state.menu)) do
        if type(it) == 'table' and not it.command and type(it.path) == 'table'
            and path_key(it.path) == path_key(path) then
            return true
        end
    end
    return false
end

local function f10_roots()
    local out = {}
    for _, it in ipairs(list(state.menu)) do
        if type(it) == 'table' and not it.command and type(it.path) == 'table' and #it.path == 1 then
            out[#out + 1] = it.path[1]
        end
    end
    return out
end

-- What a ring starts with for the kind of aircraft: one whose F10 menu has
-- cargo gets the logistics set, anything else the combat set. Entries whose
-- F10 menu this slot lacks are left out.
local PRESETS = {
    transport = {
        { menu = "Cargo" }, { menu = "C-130 Cargo" }, { menu = "Troops" }, { info = "crates" },
        { menu = "Objectives" }, { menu = "Actions" }, { info = "nearest" }, { panel = "menu" },
    },
    combat = {
        { menu = "JTAC" }, { menu = "GCI/EWR" }, { panel = "ewr" }, { info = "nearest" },
        { menu = "Objectives" }, { menu = "Actions" }, { menu = "Flight" }, { panel = "menu" },
    },
}

local function default_wheel()
    local items = {}
    -- 2.2 kept pins in wheel_pins; they lead the default wheel.
    local pins = type(cfg.wheel_pins) == 'table' and cfg.wheel_pins[airframe()] or nil
    for _, pin in ipairs(list(pins)) do
        if type(pin) == 'table' and type(pin.path) == 'table' then
            items[#items + 1] = { kind = "f10", label = tostring(pin.label or pin.path[#pin.path]), path = copy_path(pin.path) }
        end
    end
    local transport = f10_menu_exists({ "Cargo" }) or f10_menu_exists({ "C-130 Cargo" })
    for _, e in ipairs(transport and PRESETS.transport or PRESETS.combat) do
        if e.menu then
            if f10_menu_exists({ e.menu }) then
                items[#items + 1] = { kind = "f10menu", label = e.menu, path = { e.menu } }
            end
        elseif e.info then
            items[#items + 1] = { kind = "info", what = e.info }
        elseif e.panel then
            items[#items + 1] = { kind = "panel", tab = e.panel }
        end
    end
    return { items = items }
end

-- This airframe's wheel: the saved one, or the default (not saved).
local function wheel_def()
    local w = type(cfg.wheels) == 'table' and cfg.wheels[airframe()] or nil
    if type(w) == 'table' and type(w.items) == 'table' then return w, true end
    return default_wheel(), false
end

-- The wheel to edit: saves a copy of the default the first time it changes.
local function editable_wheel()
    if type(cfg.wheels) ~= 'table' then cfg.wheels = {} end
    local w, custom = wheel_def()
    if not custom then
        w = deep_copy(w)
        cfg.wheels[airframe()] = w
    end
    return w
end

local function save_wheels()
    save_config()
end

-- ── Wheel: resolving items into what is drawn ──

local DESTRUCTIVE = { "destroy", "delete", "disband", "abort", "scuttle", "remove", "clear" }

local function is_destructive(label)
    local l = tostring(label):lower()
    for _, w in ipairs(DESTRUCTIVE) do
        if l:find(w, 1, true) then return true end
    end
    return false
end

-- item -> entry { item, kind, label, icon, group, children(), danger, hub }
local function entry_of(item)
    if type(item) ~= 'table' then return nil end
    local k = item.kind
    if k == "f10" and type(item.path) == 'table' then
        local label = tostring(item.label or item.path[#item.path])
        return { item = item, kind = k, label = label, icon = item.icon or icon_for(label),
                 danger = is_destructive(label), hub = table.concat(item.path, " > ") }
    elseif k == "f10menu" and type(item.path) == 'table' then
        if not f10_menu_exists(item.path) then return nil end
        local label = tostring(item.label or item.path[#item.path])
        local path = item.path
        return { item = item, kind = k, label = label, icon = item.icon or icon_for(label), group = true,
                 children = function() return f10_children(path) end, hub = "F10 > " .. table.concat(path, " > ") }
    elseif k == "group" then
        local label = tostring(item.label or "GROUP")
        return { item = item, kind = k, label = label, icon = item.icon or "group", group = true,
                 children = function() return list(item.children) end, hub = "Your group" }
    elseif k == "panel" and PANEL_TABS[item.tab] then
        local t = PANEL_TABS[item.tab]
        return { item = item, kind = k, label = t.label, icon = item.icon or t.icon, hub = "Opens the panel" }
    elseif k == "info" and INFO[item.what] then
        local inf = INFO[item.what]
        local short, long = inf.value()
        return { item = item, kind = k, label = short, icon = item.icon or inf.icon, info = inf, hub = long, name = inf.name }
    elseif k == "more" then
        return item
    end
    return nil
end

local function resolve(items, max_n)
    local out = {}
    for _, it in ipairs(items) do
        local e = entry_of(it)
        if e then out[#out + 1] = e end
    end
    if #out > max_n then
        local rest = {}
        for i = max_n, #out do rest[#rest + 1] = out[i].item end
        local cut = {}
        for i = 1, max_n - 1 do cut[i] = out[i] end
        cut[max_n] = { kind = "more", label = "MORE", icon = "more", group = true,
                       children = function() return rest end, hub = (#rest) .. " more" }
        out = cut
    end
    return out
end

-- ── Wheel: state, pointer, firing ──

local w_wheel
local WW = {}                 -- the wheel's widgets, by layer
local pic_cache = {}
local wheel_origin = { x = 0, y = 0 }
local MAX_ARC = 9

local function slot_mid(i, n) return -90 + (i - 1) * 360 / n end
local function angle_diff(a, b) return math.abs(((a - b) + 540) % 360 - 180) end

local function close_arc() wheel.arc = nil end

local function open_arc(i)
    local e = wheel.entries[i]
    if not (e and e.group and WL) then
        wheel.arc = nil
        return
    end
    local kids = resolve(e.children(), MAX_ARC)
    if #kids == 0 then wheel.arc = nil return end
    local G = WL.geom
    local mid = slot_mid(i, #wheel.entries)
    local start = math.floor((mid - #kids * G.child_span / 2) / G.arc_step + 0.5) * G.arc_step
    wheel.arc = { owner = i, entries = kids, start = start }
end

local function rebuild_ring()
    local frame = wheel.stack[#wheel.stack]
    wheel.entries = resolve(frame.items(), (WL and WL.geom.max_ring) or 10)
    if wheel.hl_main and wheel.hl_main > #wheel.entries then wheel.hl_main = nil end
    if wheel.arc then
        local owner = wheel.arc.owner
        if owner <= #wheel.entries then open_arc(owner) else wheel.arc = nil end
    end
end

-- Where the pointer is, in canvas units from the hub.
local function hit(x, y)
    if not WL then return "none" end
    local G = WL.geom
    local n = #wheel.entries
    local r = math.sqrt(x * x + y * y)
    local ang = math.deg(math.atan2(y, x))
    if r < G.r_in - 4 then return "hub" end
    if wheel.arc and r > G.r_out + 4 and r <= G.sub_out + 50 then
        for j = 1, #wheel.arc.entries do
            local mid = wheel.arc.start + (j - 1) * G.child_span + G.child_span / 2
            if angle_diff(ang, mid) <= G.child_span / 2 then return "arc", j end
        end
    end
    if n == 0 or r > G.sub_out + 50 then return "none" end
    local best, bi = 1e9, nil
    for i = 1, n do
        local d = angle_diff(ang, slot_mid(i, n))
        if d < best then best, bi = d, i end
    end
    return "main", bi
end

local function point_at(x, y, source, now)
    wheel.ptr = { x = x, y = y, source = source }
    local zone, i = hit(x, y)
    local before = (wheel.hl_main or 0) .. ":" .. (wheel.hl_arc or 0) .. ":" .. (wheel.arc and wheel.arc.owner or 0)
    if zone == "main" then
        wheel.hl_main, wheel.hl_arc = i, nil
        if not wheel.arc or wheel.arc.owner ~= i then open_arc(i) end
    elseif zone == "arc" then
        wheel.hl_arc = i
        wheel.hl_main = wheel.arc.owner
    else
        wheel.hl_main, wheel.hl_arc = nil, nil
        if zone == "hub" then close_arc() end
    end
    wheel.on_hub = zone == "hub"
    local after = (wheel.hl_main or 0) .. ":" .. (wheel.hl_arc or 0) .. ":" .. (wheel.arc and wheel.arc.owner or 0)
    if before ~= after then
        wheel.hl_since = now or os.clock()
        wheel.armed = nil
        render_wheel()
    end
end

close_wheel = function()
    wheel.open, wheel.hl_main, wheel.hl_arc, wheel.arc, wheel.armed = false, nil, nil, nil, nil
    wheel.key_hold = nil
    if unbind_nav then unbind_nav() end
    if w_wheel then
        pcall(function()
            w_wheel:setHasCursor(false)
            w_wheel:setVisible(false)
        end)
    end
end

-- Camera direction, degrees, relative to the aircraft when its attitude can
-- be read. nil when this server does not export the camera.
local function head_angles()
    if type(Export) ~= 'table' or type(Export.LoGetCameraPosition) ~= 'function' then return nil end
    local ok, cam = pcall(Export.LoGetCameraPosition)
    if not ok or type(cam) ~= 'table' or type(cam.x) ~= 'table' then return nil end
    local f = cam.x
    local yaw = math.atan2(tonumber(f.z) or 0, tonumber(f.x) or 0)
    local pitch = math.asin(math.max(-1, math.min(1, tonumber(f.y) or 0)))
    if type(Export.LoGetSelfData) == 'function' then
        local ok2, me = pcall(Export.LoGetSelfData)
        if ok2 and type(me) == 'table' and tonumber(me.Heading) then
            yaw = yaw - tonumber(me.Heading)
            pitch = pitch - (tonumber(me.Pitch) or 0)
        end
    end
    return math.deg(yaw), math.deg(pitch)
end

-- Make where the player is looking now the hub.
local function rebase()
    local yaw, pitch = head_angles()
    wheel.base_yaw, wheel.base_pitch = yaw, pitch
    wheel.last_yaw, wheel.last_pitch = yaw, pitch
end

local function push_frame(title, items_fn)
    wheel.stack[#wheel.stack + 1] = { title = title, items = items_fn }
    wheel.hl_main, wheel.hl_arc, wheel.arc, wheel.armed = nil, nil, nil, nil
    rebase()
    rebuild_ring()
end

local function wheel_back()
    if #wheel.stack > 1 then
        table.remove(wheel.stack)
        wheel.hl_main, wheel.hl_arc, wheel.arc, wheel.armed = nil, nil, nil, nil
        rebase()
        rebuild_ring()
        render_wheel()
    else
        close_wheel()
    end
end

local function fire_entry(e)
    if not e then return end
    if e.kind == "f10" then
        local key = path_key(e.item.path)
        if e.danger and wheel.armed ~= key then
            wheel.armed, wheel.armed_until = key, os.clock() + 3
        else
            wheel.armed = nil
            set_answer(nil)
            invoke(e.item.path)
        end
    elseif e.group then
        push_frame(e.label, e.children)
    elseif e.kind == "panel" or e.kind == "info" then
        close_wheel()
        state.tab = e.kind == "panel" and e.item.tab or e.info.tab
        show()
        return
    end
    render_wheel()
end

local function fire_current()
    if wheel.hl_arc and wheel.arc then
        fire_entry(wheel.arc.entries[wheel.hl_arc])
    elseif wheel.hl_main then
        fire_entry(wheel.entries[wheel.hl_main])
    else
        wheel_back()
    end
end

-- The last place the GUI saw the mouse, for "open at the cursor".
local last_mouse = nil

local function open_wheel()
    if not (w_wheel and WL) then
        set_note("The wheel's art is missing -- re-run the Vector Strike installer.", true)
        return
    end
    wheel.open = true
    wheel.stack = { { title = "VECTOR STRIKE", items = function() return wheel_def().items end } }
    wheel.hl_main, wheel.hl_arc, wheel.arc, wheel.armed, wheel.ptr = nil, nil, nil, nil, nil
    rebase()
    rebuild_ring()
    local ok, err = pcall(function()
        local s = ws()
        local w = math.floor(WL.canvas * s)
        local h = math.floor((WL.canvas + 44) * s)
        local sw, sh = screen_size()
        local cx, cy = sw / 2, sh / 2
        -- In VR, DCS draws the GUI on a panel locked to the head, so the
        -- wheel always opens in the middle of it, where you are looking.
        if settings.wheel_open_at == "cursor" and last_mouse and not settings.vr then
            cx, cy = last_mouse.x, last_mouse.y
        end
        local x = math.floor(math.max(0, math.min(sw - w, cx - w / 2)))
        local y = math.floor(math.max(0, math.min(sh - h, cy - WL.canvas * s / 2)))
        wheel_origin = { x = x, y = y }
        w_wheel:setBounds(x, y, w, h)
        w_wheel:setVisible(true)
        w_wheel:setHasCursor(true)
    end)
    if not ok then logmsg("could not open the wheel: " .. tostring(err)) end
    wheel.key_hold = nil
    bind_nav()
    if in_multiplayer() and not key_ready_at then register_key() end
    refresh_now()
    state.next_feed = 0
    render_wheel()
end


-- ── Wheel: keys only ──
-- Everything the pointer does, from keys -- for VR players who would rather
-- not wave a head or a mouse, and for HOTAS hats and buttons mapped to keys by
-- the stick's own software. The keys are taken only while the wheel is open,
-- so they never get in the way of flying:
--   prev / next   step round the ring (or along the arc)
--   out           move out onto the highlighted group's arc
--   into          back from the arc to the ring
--   fire          run what is highlighted (as does tapping the wheel key)
--   back          the hub: back a level, or close at the top
-- While keys drive, head pointing is held until the head clearly moves, so a
-- VR headset's small movements cannot steal the selection.

local NAV_SETS = {
    arrows  = { prev = "left", next = "right", out = "up", into = "down", fire = "return", back = "backspace" },
    numpad  = { prev = "[4]", next = "[6]", out = "[8]", into = "[2]", fire = "[5]", back = "[0]" },
    ctrlalt = { prev = "ctrl+alt+left", next = "ctrl+alt+right", out = "ctrl+alt+up", into = "ctrl+alt+down",
                fire = "ctrl+alt+return", back = "ctrl+alt+backspace" },
}
local NAV_HINTS = {
    arrows  = "← → move  ·  ↑ arc  ·  enter fire  ·  backspace back",
    numpad  = "4 6 move  ·  8 arc  ·  5 fire  ·  0 back",
    ctrlalt = "ctrl+alt + arrows · enter fire · backspace back",
}
local nav_bound = {}         -- key -> the function bound to it

local function key_hold()
    local yaw, pitch = head_angles()
    wheel.key_hold = { yaw = yaw, pitch = pitch }
    wheel.ptr = { x = 0, y = 0, source = "keys" }
    wheel.hl_since = os.clock()
end

local function select_ring(i)
    local n = #wheel.entries
    if n == 0 then return end
    i = ((i - 1) % n) + 1
    if wheel.hl_main ~= i or wheel.hl_arc then wheel.armed = nil end
    wheel.hl_main, wheel.hl_arc, wheel.on_hub = i, nil, false
    if not wheel.arc or wheel.arc.owner ~= i then open_arc(i) end
    key_hold()
    render_wheel()
end

local function select_arc(j)
    local arc = wheel.arc
    if not arc or #arc.entries == 0 then return end
    j = ((j - 1) % #arc.entries) + 1
    if wheel.hl_arc ~= j then wheel.armed = nil end
    wheel.hl_arc, wheel.hl_main, wheel.on_hub = j, arc.owner, false
    key_hold()
    render_wheel()
end

local NAV = {
    next = function()
        if wheel.hl_arc then select_arc(wheel.hl_arc + 1) else select_ring((wheel.hl_main or 0) + 1) end
    end,
    prev = function()
        if wheel.hl_arc then select_arc(wheel.hl_arc - 1) else select_ring((wheel.hl_main or 1) - 1) end
    end,
    out = function()
        if not wheel.hl_main then return select_ring(1) end
        if not wheel.hl_arc and wheel.arc and wheel.arc.owner == wheel.hl_main then
            select_arc(math.ceil(#wheel.arc.entries / 2))
        end
    end,
    into = function()
        if wheel.hl_arc then select_ring(wheel.hl_main) end
    end,
    fire = function()
        if wheel.hl_main or wheel.hl_arc then
            fire_current()
        else
            select_ring(1)
        end
    end,
    back = function() wheel_back() end,
}

bind_nav = function()
    unbind_nav()
    local set = NAV_SETS[settings.wheel_nav]
    if not set or not window then return end
    for name, key in pairs(set) do
        -- never shadow the panel or wheel key itself
        if key ~= bound.toggle and key ~= bound.wheel then
            local fn = function()
                local ok, err = pcall(NAV[name])
                if not ok then logmsg("wheel key " .. name .. " errored: " .. tostring(err)) end
            end
            local ok = pcall(function() window:addHotKeyCallback(key, fn) end)
            if ok then nav_bound[key] = fn end
        end
    end
end

unbind_nav = function()
    for key, fn in pairs(nav_bound) do
        pcall(function() window:removeHotKeyCallback(key, fn) end)
    end
    nav_bound = {}
end

wheel_key_pressed = function()
    if not wheel.open then
        open_wheel()
    elseif wheel.hl_main or wheel.hl_arc then
        fire_current()
    else
        close_wheel()
    end
end

-- ── Wheel: drawing ──

local function pic_skin(file, color, w, h)
    local key = file .. "|" .. color .. "|" .. w .. "|" .. h
    local s = pic_cache[key]
    if not s then
        s = deep_copy(Skin.staticSkin())
        for _, st in pairs(s.skinData.states) do
            for i in pairs(st) do
                st[i].picture = {
                    file = file, color = color,
                    horzAlign = { type = "min", offset = 0 }, vertAlign = { type = "min", offset = 0 },
                    size = { horz = w, vert = h }, userTexSampler = 0,
                }
            end
        end
        pic_cache[key] = s
    end
    return s
end

local text_cache = {}
local function text_skin(rgb, size, bold)
    local key = hexc(rgb) .. size .. tostring(bold)
    local s = text_cache[key]
    if not s then
        s = deep_copy(Skin.staticSkin())
        s.skinData.params.textWrapping = false
        for _, st in pairs(s.skinData.states) do
            for i in pairs(st) do st[i].text = text_style(hexc(rgb), bold and FONT_BOLD or FONT, size, "middle") end
        end
        text_cache[key] = s
    end
    return s
end

-- Place a texture at a canvas rect.
local function put_pic(wdg, file, color, x, y, w, h)
    local s = ws()
    local pw, ph = math.max(1, math.floor(w * s + 0.5)), math.max(1, math.floor(h * s + 0.5))
    local key = file .. color .. pw .. ph
    if wdg.__k ~= key then
        wdg:setSkin(pic_skin(WHEEL_DIR .. file, color, pw, ph))
        wdg.__k = key
    end
    wdg:setBounds(math.floor(x * s + 0.5), math.floor(y * s + 0.5), pw, ph)
    wdg:setVisible(true)
end

local function put_text(wdg, text, rgb, size, bold, cx, cy, w, h)
    local s = ws()
    local px_size = math.max(8, math.floor(size * s * 1.33 + 0.5))
    local key = hexc(rgb) .. px_size .. tostring(bold)
    if wdg.__k ~= key then
        wdg:setSkin(text_skin(rgb, px_size, bold))
        wdg.__k = key
    end
    wdg:setText(text)
    wdg:setBounds(math.floor((cx - w / 2) * s + 0.5), math.floor((cy - h / 2) * s + 0.5), math.floor(w * s), math.floor(h * s))
    wdg:setVisible(true)
end

local function hide_w(wdg) if wdg then wdg:setVisible(false) end end

-- Shorten to n characters, keeping the case.
local function clip(text, n)
    text = tostring(text or "")
    if #text <= n then return text end
    return text:sub(1, n - 1) .. "…"
end

-- Labels on the ring are upper case.
local function short(text, n)
    return clip(tostring(text or ""):upper(), n)
end

render_wheel = function()
    if not (w_wheel and wheel.open and WL) then return end
    local ok, err = pcall(function()
        local G = WL.geom
        local c = WL.canvas / 2
        local acc = accent()
        local op = settings.wheel_opacity
        local n = #wheel.entries
        local rm = (G.r_in + G.r_out) / 2

        for i = 1, G.max_ring do
            local e = wheel.entries[i]
            if e and n >= 2 then
                local r = WL.ring[n][i]
                local base = "r" .. n .. "_" .. i
                local hot = wheel.hl_main == i and not wheel.hl_arc
                local owner = wheel.arc and wheel.arc.owner == i
                if hot then
                    put_pic(WW.ring_glow[i], base .. "_glow.png", hexc(acc, 150), r.x, r.y, r.w, r.h)
                else
                    hide_w(WW.ring_glow[i])
                end
                local fill = hot and hexc(acc, 240) or (owner and hexc(acc, 110 * op) or hexc(INK_RGB, 214 * op))
                local line = (hot or owner) and hexc(acc, 255) or hexc(acc, 80)
                if e.danger and not hot then line = hexc(BAD_RGB, 170) end
                put_pic(WW.ring_fill[i], base .. "_fill.png", fill, r.x, r.y, r.w, r.h)
                put_pic(WW.ring_line[i], base .. "_line.png", line, r.x, r.y, r.w, r.h)
                local a = math.rad(slot_mid(i, n))
                local ix, iy = c + (rm - 6) * math.cos(a), c + (rm - 6) * math.sin(a)
                local isz = n > 8 and 34 or 40
                local fg = hot and INK_RGB or (e.danger and BAD_RGB or TEXT_RGB)
                put_pic(WW.ring_icon[i], "icons/" .. e.icon .. ".png", hexc(fg), ix - isz / 2, iy - isz / 2 - 9, isz, isz)
                put_text(WW.ring_label[i], short(e.label, n > 8 and 10 or 13), fg, n > 8 and 9 or 10, true,
                    ix, iy + isz / 2 + 2, 120, 18)
                if e.group then
                    local gx, gy = c + (G.r_out - 10) * math.cos(a), c + (G.r_out - 10) * math.sin(a)
                    put_text(WW.ring_mark[i], "•", (hot or owner) and (hot and INK_RGB or acc) or acc, 12, true, gx, gy, 16, 16)
                else
                    hide_w(WW.ring_mark[i])
                end
            else
                hide_w(WW.ring_glow[i]); hide_w(WW.ring_fill[i]); hide_w(WW.ring_line[i])
                hide_w(WW.ring_icon[i]); hide_w(WW.ring_label[i]); hide_w(WW.ring_mark[i])
            end
        end

        local arc = wheel.arc
        for j = 1, MAX_ARC do
            local e = arc and arc.entries[j]
            if e then
                local a0 = arc.start + (j - 1) * G.child_span
                local k = math.floor((a0 % 360) / G.arc_step + 0.5) % (360 / G.arc_step)
                local r = WL.arc[k]
                local base = "a" .. k
                local hot = wheel.hl_arc == j
                if hot then
                    put_pic(WW.arc_glow[j], base .. "_glow.png", hexc(acc, 140), r.x, r.y, r.w, r.h)
                else
                    hide_w(WW.arc_glow[j])
                end
                put_pic(WW.arc_fill[j], base .. "_fill.png", hot and hexc(acc, 245) or hexc(INK_RGB, 226 * op), r.x, r.y, r.w, r.h)
                put_pic(WW.arc_line[j], base .. "_line.png", e.danger and hexc(BAD_RGB, 180) or hexc(acc, hot and 255 or 100), r.x, r.y, r.w, r.h)
                local mid = math.rad(a0 + G.child_span / 2)
                local am = (G.sub_in + G.sub_out) / 2
                local ix, iy = c + (am - 4) * math.cos(mid), c + (am - 4) * math.sin(mid)
                local fg = hot and INK_RGB or (e.danger and BAD_RGB or TEXT_RGB)
                put_pic(WW.arc_icon[j], "icons/" .. e.icon .. ".png", hexc(fg), ix - 15, iy - 22, 30, 30)
                put_text(WW.arc_label[j], short(e.label, 11), fg, 8, true, ix, iy + 14, 96, 16)
            else
                hide_w(WW.arc_glow[j]); hide_w(WW.arc_fill[j]); hide_w(WW.arc_line[j])
                hide_w(WW.arc_icon[j]); hide_w(WW.arc_label[j])
            end
        end

        -- hub
        local hr = WL.hub
        put_pic(WW.hub_fill, "hub_fill.png", hexc(INK_RGB, 236 * math.max(op, 0.8)), hr.x, hr.y, hr.w, hr.h)
        put_pic(WW.hub_line, "hub_line.png", wheel.on_hub and hexc(acc, 255) or hexc(acc, 190), hr.x, hr.y, hr.w, hr.h)
        if WW.emblem then
            put_pic(WW.emblem, "../emblem-64.png", "0xffffffff", c - 28, c - 74, 56, 56)
        end
        local frame = wheel.stack[#wheel.stack]
        local title, sub
        local cur = (wheel.hl_arc and arc and arc.entries[wheel.hl_arc]) or (wheel.hl_main and wheel.entries[wheel.hl_main])
        if not in_multiplayer() then
            title, sub = "VECTOR STRIKE", "Join a Vector Strike server"
        elseif not state.menu then
            title, sub = "VECTOR STRIKE", "Reading F10..."
        elseif wheel.armed and cur then
            title, sub = "AGAIN TO CONFIRM", short(cur.label, 24)
        elseif cur then
            local owner_e = wheel.hl_arc and wheel.entries[wheel.hl_main] or nil
            title = owner_e and (short(owner_e.label, 10) .. " › " .. short(cur.label, 12)) or short(cur.name or cur.label, 22)
            sub = cur.hub or ""
        elseif wheel.on_hub then
            title, sub = #wheel.stack > 1 and "‹ BACK" or "CLOSE", ""
        else
            title = short(frame.title, 22)
            local parts = { airframe() }
            local s = state.status
            if type(s) == 'table' then parts[#parts + 1] = tostring(s.points or 0) .. " pts" end
            sub = table.concat(parts, "  ·  ")
        end
        put_text(WW.hub_title, title, wheel.armed and BAD_RGB or acc, 11, true, c, c + 0, 176, 22)
        put_text(WW.hub_sub, clip(sub, 34), DIM_RGB, 8, false, c, c + 22, 176, 18)
        local hint = ""
        if wheel.ptr and wheel.ptr.source == "keys" then
            hint = NAV_HINTS[settings.wheel_nav] or ""
        elseif wheel.hl_main or wheel.hl_arc then
            hint = "click · or tap the key"
        end
        put_text(WW.hub_hint, hint, DIM_RGB, 7, false, c, c + 42, 196, 16)

        -- the server's latest answer, under the wheel, for a few seconds
        local a = state.answer
        if a and a.clock and os.clock() - a.clock < 12 then
            local sw_, sh_ = WL.strip.w, WL.strip.h
            local sx, sy = c - sw_ / 2, WL.canvas - 8
            put_pic(WW.strip_fill, "strip_fill.png", hexc(INK_RGB, 232), sx, sy, sw_, sh_)
            put_pic(WW.strip_line, "strip_line.png", hexc(acc, 160), sx, sy, sw_, sh_)
            put_text(WW.strip_tag, "SERVER", acc, 9, true, sx + 46, sy + sh_ / 2, 80, 20)
            put_text(WW.strip_text, clip(a.text, 70), TEXT_RGB, 9, false, sx + 46 + (sw_ - 92) / 2 + 30, sy + sh_ / 2, sw_ - 110, 20)
        else
            hide_w(WW.strip_fill); hide_w(WW.strip_line); hide_w(WW.strip_tag); hide_w(WW.strip_text)
        end
    end)
    if not ok then logmsg("wheel render errored: " .. tostring(err)) end
end

local function draw_pointer()
    local p = wheel.ptr
    if not (p and p.source == "head" and WL) then
        hide_w(WW.dot); hide_w(WW.dot_glow)
        return
    end
    local G = WL.geom
    local c = WL.canvas / 2
    local r = math.sqrt(p.x * p.x + p.y * p.y)
    local lim = G.sub_out + 12
    local x, y = p.x, p.y
    if r > lim then x, y = x / r * lim, y / r * lim end
    local acc = accent()
    pcall(function()
        put_pic(WW.dot_glow, "dot_glow.png", hexc(acc, 120), c + x - WL.dot.glow_r, c + y - WL.dot.glow_r, WL.dot.glow_r * 2, WL.dot.glow_r * 2)
        put_pic(WW.dot, "dot.png", hexc(acc, 255), c + x - WL.dot.r, c + y - WL.dot.r, WL.dot.r * 2, WL.dot.r * 2)
    end)
end

-- Mouse coordinates from dxgui are screen coordinates (DCS's own
-- me_contextMenu.lua feeds them to FindWidgetAtScreenPoint); convert to
-- canvas units from the hub. Falls back to widget-local in case a build ever
-- hands those over instead.
local function mouse_to_canvas(x, y)
    local s = ws()
    local ww = WL.canvas * s
    local lx, ly = x, y
    if x >= wheel_origin.x and x <= wheel_origin.x + ww and y >= wheel_origin.y and y <= wheel_origin.y + ww then
        lx, ly = x - wheel_origin.x, y - wheel_origin.y
    end
    return lx / s - WL.canvas / 2, ly / s - WL.canvas / 2
end

-- Every frame while open: head pointer, dwell, confirm timeout.
wheel_update = function(now)
    if not wheel.open then return end
    if wheel.armed and now > (wheel.armed_until or 0) then
        wheel.armed = nil
        render_wheel()
    end
    if settings.wheel_head and WL then
        local yaw, pitch = head_angles()
        if yaw and wheel.base_yaw then
            local moved = math.abs(((yaw - (wheel.last_yaw or yaw)) + 540) % 360 - 180)
                + math.abs(pitch - (wheel.last_pitch or pitch))
            wheel.last_yaw, wheel.last_pitch = yaw, pitch
            -- Keys are driving: hold the head off until it clearly moves.
            local kh = wheel.key_hold
            if kh then
                if kh.yaw and (math.abs(((yaw - kh.yaw) + 540) % 360 - 180) > 4 or math.abs(pitch - kh.pitch) > 4) then
                    wheel.key_hold = nil
                else
                    moved = 0
                end
            end
            -- Only take over from the mouse when the head actually moves.
            if moved > 0.05 or (wheel.ptr and wheel.ptr.source == "head") or not wheel.ptr then
                local dx = ((yaw - wheel.base_yaw + 540) % 360) - 180
                local dy = -(pitch - wheel.base_pitch)
                local G = WL.geom
                local k = ((G.r_in + G.r_out) / 2) / settings.wheel_reach
                if moved > 0.05 or (wheel.ptr and wheel.ptr.source == "head") then
                    point_at(dx * k, dy * k, "head", now)
                end
            end
        end
    end
    draw_pointer()
    if settings.wheel_dwell > 0 and wheel.ptr and wheel.ptr.source == "head"
        and (wheel.hl_main or wheel.hl_arc) and now - (wheel.hl_since or now) >= settings.wheel_dwell then
        wheel.hl_since = math.huge   -- once per look
        fire_current()
    end
end

create_wheel = function()
    WW = { ring_glow = {}, ring_fill = {}, ring_line = {}, ring_icon = {}, ring_label = {}, ring_mark = {},
           arc_glow = {}, arc_fill = {}, arc_line = {}, arc_icon = {}, arc_label = {} }
    pic_cache, text_cache = {}, {}
    if not WL then return end
    local s = ws()
    w_wheel = Window.new(0, 0, math.floor(WL.canvas * s), math.floor((WL.canvas + 44) * s), "")
    w_wheel:setSkin(skins.hidden)
    pcall(function() w_wheel:setZOrder(114) end)
    local function st()
        local w = Static.new("")
        w_wheel:insertWidget(w)
        w:setVisible(false)
        return w
    end
    -- Insertion order is drawing order.
    for i = 1, WL.geom.max_ring do WW.ring_glow[i] = st() end
    for j = 1, MAX_ARC do WW.arc_glow[j] = st() end
    for i = 1, WL.geom.max_ring do WW.ring_fill[i] = st() end
    for j = 1, MAX_ARC do WW.arc_fill[j] = st() end
    for i = 1, WL.geom.max_ring do WW.ring_line[i] = st() end
    for j = 1, MAX_ARC do WW.arc_line[j] = st() end
    WW.hub_fill, WW.hub_line = st(), st()
    for i = 1, WL.geom.max_ring do WW.ring_icon[i] = st(); WW.ring_label[i] = st(); WW.ring_mark[i] = st() end
    for j = 1, MAX_ARC do WW.arc_icon[j] = st(); WW.arc_label[j] = st() end
    if skins.emblem then WW.emblem = st() end
    WW.hub_title, WW.hub_sub, WW.hub_hint = st(), st(), st()
    WW.strip_fill, WW.strip_line, WW.strip_tag, WW.strip_text = st(), st(), st(), st()
    WW.dot_glow, WW.dot = st(), st()

    -- A transparent pad over everything takes the mouse.
    local pad = Button.new("")
    local ps = deep_copy(Skin.buttonSkin())
    for _, stt in pairs(ps.skinData.states) do
        for i in pairs(stt) do stt[i].bkg = nil; stt[i].text = text_style("0x00000000", FONT, 8) end
    end
    pad:setSkin(ps)
    pad:setBounds(0, 0, math.floor(WL.canvas * s), math.floor((WL.canvas + 44) * s))
    pad:addMouseMoveCallback(function(self, x, y)
        local ok, err = pcall(function()
            local cx, cy = mouse_to_canvas(x, y)
            wheel.key_hold = nil
            point_at(cx, cy, "mouse")
        end)
        if not ok then logmsg("wheel mouse errored: " .. tostring(err)) end
    end)
    pad:addMouseUpCallback(function(self, x, y)
        local ok, err = pcall(function()
            local cx, cy = mouse_to_canvas(x, y)
            point_at(cx, cy, "mouse")
            if wheel.on_hub then wheel_back() else fire_current() end
        end)
        if not ok then logmsg("wheel click errored: " .. tostring(err)) end
    end)
    w_wheel:insertWidget(pad)
    WW.pad = pad
    w_wheel:setVisible(false)
    wheel.open = false

    -- Remember where the GUI last saw the mouse, for "open at the cursor".
    if not state.mouse_hooked then
        state.mouse_hooked = true
        for _, kind in ipairs({ "move", "down" }) do
            pcall(function()
                dxgui.AddMouseCallback(kind, function(x, y)
                    if type(x) == 'number' and type(y) == 'number' then last_mouse = { x = x, y = y } end
                end)
            end)
        end
    end
end

destroy_wheel = function()
    close_wheel()
    if w_wheel then pcall(function() w_wheel:kill() end) end
    w_wheel, WW = nil, {}
end

-- ── Wheel editor (the panel's WHEEL tab) ──

-- Pin or unpin an F10 command (the star in the MENU tab): it goes on this
-- airframe's wheel, at the top level.
local function find_pin(items, path)
    local k = path_key(path)
    for i, it in ipairs(items) do
        if type(it) == 'table' and it.kind == "f10" and type(it.path) == 'table' and path_key(it.path) == k then
            return items, i
        end
        if type(it) == 'table' and it.kind == "group" then
            local l, j = find_pin(list(it.children), path)
            if l then return l, j end
        end
    end
end

pin_index = function(path)
    local w = wheel_def()
    local l, i = find_pin(w.items, path)
    return i
end

toggle_pin = function(path, label)
    local w = editable_wheel()
    local l, i = find_pin(w.items, path)
    if l then
        table.remove(l, i)
        set_note("Taken off the wheel: " .. label, false)
    else
        table.insert(w.items, 1, { kind = "f10", label = label, path = copy_path(path) })
        set_note("On the wheel (" .. airframe() .. "): " .. label, false)
    end
    save_wheels()
end

local function swap(t, i, j)
    if t[i] and t[j] then t[i], t[j] = t[j], t[i] end
end

local function next_icon(cur)
    local icons = icon_list()
    for i, name in ipairs(icons) do
        if name == cur then return icons[i % #icons + 1] end
    end
    return icons[1]
end

local function item_title(it)
    local e = entry_of(it)
    if it.kind == "info" and INFO[it.what] then return INFO[it.what].name, "live info" end
    if it.kind == "panel" and PANEL_TABS[it.tab] then return PANEL_TABS[it.tab].label, "opens the panel" end
    if it.kind == "f10menu" then return tostring(it.label or it.path[#it.path]), e and "F10 menu, opens an arc" or "F10 menu not in this slot" end
    if it.kind == "group" then return tostring(it.label or "GROUP"), #list(it.children) .. " in this group" end
    return tostring(it.label or "?"), "F10 command"
end

local function custom_groups(items)
    local g = {}
    for i, it in ipairs(items) do
        if type(it) == 'table' and it.kind == "group" then g[#g + 1] = it end
    end
    return g
end

local GROUP_NAMES = { "FAVOURITES", "LOGISTICS", "COMBAT", "SUPPORT", "QUICK", "GROUP" }

renderers.wheel = function()
    local rows = {}
    local width = content_width()
    if not WL then
        add_text(rows, "The wheel's art is missing -- re-run the Vector Strike installer.", skins.bad)
        return rows
    end
    local w, custom = wheel_def()
    rows[#rows + 1] = { text = "WHEEL FOR " .. airframe():upper(), skin = skins.head }
    add_text(rows, custom and "Your own layout. Changes apply the next time the wheel opens."
        or "The default for this kind of aircraft. Change anything and it becomes yours.", skins.label)
    if custom then
        rows[#rows + 1] = { button = true, x = 0, w = px(150), skin = skins.tool, text = "RESET TO DEFAULT",
                            action = function()
                                cfg.wheels[airframe()] = nil
                                save_wheels()
                                render()
                            end }
    end
    rows[#rows + 1] = { text = "", h = px(4) }

    local bw = px(30)
    local function controls(list_, i, extra)
        -- ▲ ▼ ICON (extra) ✕ at the right of the row just added
        local btns = {
            { "▲", function() swap(list_, i, i - 1) end },
            { "▼", function() swap(list_, i, i + 1) end },
            { "ICON", function() list_[i].icon = next_icon(list_[i].icon or (entry_of(list_[i]) or {}).icon) end, px(46) },
        }
        for _, x in ipairs(extra or {}) do btns[#btns + 1] = x end
        btns[#btns + 1] = { "✕", function() table.remove(list_, i) end }
        local total = 0
        for _, b in ipairs(btns) do total = total + (b[3] or bw) + px(3) end
        local x = width - total
        for _, b in ipairs(btns) do
            local fn = b[2]
            rows[#rows + 1] = { button = true, same_line = true, x = x, w = b[3] or bw, skin = skins.tool, text = b[1],
                                action = function()
                                    -- editing the default makes it the player's own
                                    if not custom then
                                        local own = editable_wheel()
                                        list_ = (list_ == w.items) and own.items or list_
                                    end
                                    fn()
                                    save_wheels()
                                    render()
                                end }
            x = x + (b[3] or bw) + px(3)
        end
        return total
    end

    local items = w.items
    local groups = custom_groups(items)
    for i, it in ipairs(items) do
        if type(it) == 'table' then
            local title, kind = item_title(it)
            local e = entry_of(it)
            local icon = it.icon or (e and e.icon) or "generic"
            rows[#rows + 1] = { pic = WHEEL_DIR .. "icons/" .. icon .. ".png", w = px(20), h = px(22) }
            local extra = {}
            if it.kind == "f10" and #groups > 0 then
                extra[1] = { "→ GROUP", function()
                    local g = custom_groups(items)[1]
                    for gi, gg in ipairs(custom_groups(items)) do
                        if gg.next_target then g = gg end
                    end
                    g.children = list(g.children)
                    g.children[#g.children + 1] = table.remove(items, i)
                end, px(70) }
            end
            if it.kind == "group" then
                extra[1] = { "RENAME", function()
                    local cur = tostring(it.label or "")
                    local nxt = GROUP_NAMES[1]
                    for gi, nm in ipairs(GROUP_NAMES) do
                        if nm == cur then nxt = GROUP_NAMES[gi % #GROUP_NAMES + 1] end
                    end
                    it.label = nxt
                end, px(70) }
            end
            local used = controls(items, i, extra)
            -- the title goes between the icon and the buttons
            table.insert(rows, #rows - (#extra + 3), { text = "  " .. title .. "   -- " .. kind, skin = e and skins.text or skins.label,
                                                       x = px(24), w = width - px(24) - used - px(6), same_line = true })
            if it.kind == "group" then
                for ci, ch in ipairs(list(it.children)) do
                    local ct = item_title(ch)
                    local ce = entry_of(ch)
                    rows[#rows + 1] = { pic = WHEEL_DIR .. "icons/" .. (ch.icon or (ce and ce.icon) or "generic") .. ".png",
                                        x = px(24), w = px(20), h = px(22) }
                    local cused = controls(it.children, ci, { { "OUT", function()
                        items[#items + 1] = table.remove(it.children, ci)
                    end, px(46) } })
                    table.insert(rows, #rows - 4, { text = "  " .. ct, skin = skins.text, x = px(48), w = width - px(48) - cused - px(6), same_line = true })
                end
            end
        end
    end
    if #items == 0 then add_text(rows, "The wheel is empty. Add something below.", skins.label) end
    if #items > 10 then add_text(rows, "Only 10 fit on the ring; the rest sit behind MORE.", skins.warn) end

    rows[#rows + 1] = { text = "", h = px(6) }
    rows[#rows + 1] = { text = "ADD TO THE WHEEL", skin = skins.head }
    local quarter = math.floor((width - px(9)) / 4)
    local adders = { { "F10 MENU", "f10" }, { "LIVE INFO", "info" }, { "PANEL", "panel" }, { "NEW GROUP", "group" } }
    for ai, a in ipairs(adders) do
        rows[#rows + 1] = { button = true, same_line = ai > 1, x = (ai - 1) * (quarter + px(3)), w = quarter,
                            skin = state.wheel_add == a[2] and skins.tab_on or skins.tool, text = "+ " .. a[1],
                            action = function()
                                if a[2] == "group" then
                                    local own = editable_wheel()
                                    local n = #custom_groups(own.items) + 1
                                    own.items[#own.items + 1] = { kind = "group", label = GROUP_NAMES[math.min(n, #GROUP_NAMES)], children = {} }
                                    save_wheels()
                                    state.wheel_add = nil
                                else
                                    state.wheel_add = state.wheel_add ~= a[2] and a[2] or nil
                                end
                                render()
                            end }
    end
    local function add(item)
        local own = editable_wheel()
        own.items[#own.items + 1] = item
        save_wheels()
        render()
    end
    if state.wheel_add == "f10" then
        local roots = f10_roots()
        if #roots == 0 then add_text(rows, "Get in a slot to see its F10 menus.", skins.label) end
        for _, r in ipairs(roots) do
            rows[#rows + 1] = { button = true, skin = skins.row, text = "  + " .. r .. "  (opens as an arc)",
                                action = function() add({ kind = "f10menu", label = r, path = { r } }) end }
        end
        add_text(rows, "Single F10 commands: use the ☆ next to them in the MENU tab.", skins.label)
    elseif state.wheel_add == "info" then
        for _, what in ipairs(INFO_ORDER) do
            rows[#rows + 1] = { button = true, skin = skins.row, text = "  + " .. INFO[what].name,
                                action = function() add({ kind = "info", what = what }) end }
        end
    elseif state.wheel_add == "panel" then
        for _, tab in ipairs({ "menu", "cargo", "status", "ewr", "log", "wiki", "wheel" }) do
            rows[#rows + 1] = { button = true, skin = skins.row, text = "  + " .. PANEL_TABS[tab].label,
                                action = function() add({ kind = "panel", tab = tab }) end }
        end
    end
    return rows
end
end
wheel_module()

-- ── Wiki ─────────────────────────────────────────────────────────────
-- The player wiki (the pages bfwiki shows at wiki.vectorstrike.org), read
-- in the panel. The same data as the web wiki: the pages from
-- /api/wiki/pages/<slug>, this server's numbers from /api/wiki/facts. Two
-- routes are the panel's own (bfdb/src/cockpit_wiki.rs): the index, with
-- each page's `updated_at` so a page is fetched again only when it changed,
-- and the pictures, which dxgui can only show from a local PNG or JPEG.
--
-- Markdown becomes panel rows: headings in the bold skins, paragraphs
-- wrapped to the panel, lists indented, tables and code in the mono font,
-- emphasis dropped. A link to another page is a button under its paragraph;
-- an outside link is written out as its address. The custom blocks the web
-- wiki draws as pictures of the F10 menu, the panel or the wheel are drawn
-- here as the rows and labels they show.
--
-- Pictures: /api/cockpit/wiki/image/<id>/info says what the file for this
-- width will be (PNG or JPEG, and its size), bfcockpit_net.dll's `download`
-- writes it under Mods\Services\BFCockpit\cache\wiki\img, and it is shown at
-- the panel's width with its own shape. Once there, a picture is never
-- fetched again. An SVG diagram comes back rasterised to PNG by bfdb.
--
-- Nothing here waits: every fetch goes through api_call / download and
-- re-renders when it lands, like the other tabs.

local function wiki_module()

-- What can be checked outside DCS (bfcockpit/tools/luatest.lua): pure
-- functions only, no widgets, no network.
local W = {}

local WIKI_DIR = lfs.writedir() .. [[Mods\Services\BFCockpit\cache\wiki\]]
local IMG_DIR = WIKI_DIR .. [[img\]]
local PAGE_DIR = WIKI_DIR .. [[pages\]]
-- Picture widths asked of the server: a few steps, so resizing the panel a
-- little reuses the file already on disk.
local IMG_WIDTHS = { 320, 480, 640, 800, 960, 1280, 1600 }
local INDEX_EVERY = 300   -- seconds between re-reads of the index
local FACTS_EVERY = 300
local SEARCH_DELAY = 0.6  -- seconds after the last keystroke

local wk = {
    nav = { view = "home" },  -- { view = "home" | "section" | "page", section, slug, scroll }
    stack = {},               -- where Back goes
    index = nil,              -- { pages = {...}, sections = {...}, by_slug = {}, dated = bool }
    index_err = nil,
    index_at = -1e9,
    facts = nil,              -- the fact object of /api/wiki/facts
    facts_label = nil,        -- which server's numbers they are
    facts_for = nil,          -- the server they were fetched for
    facts_at = -1e9,
    facts_rev = 0,
    pages = {},               -- slug -> { title, section, updated_at, content }
    page_err = {},            -- slug -> message
    parsed = {},              -- slug -> { key, blocks }
    query = "",
    query_at = 0,
    searched = nil,           -- the query the last full-text answer is for
    hits = nil,
    search_err = nil,
    images = {},              -- "<id>@<width>" -> { state, file, w, h, why }
    img_disk = nil,           -- what is already in IMG_DIR, read once
    inflight = {},
    scroll_top = false,
}
local w_search, w_clear, w_hint

-- ── Pure: text ──

local function trim(s) return (tostring(s or ""):gsub("^%s+", ""):gsub("%s+$", "")) end
W.trim = trim

local ENTITIES = { amp = "&", lt = "<", gt = ">", quot = '"', apos = "'", nbsp = " ",
                   mdash = "—", ndash = "–", hellip = "...", rarr = "→", larr = "←", times = "×", deg = "°" }

-- `{{cfg:path|fallback}}`: path into the fact object, `.` for keys and `[n]`
-- for array indices, as bfwiki/src/lib/cfgTokens.ts reads it.
function W.lookup(facts, path)
    local keys = {}
    for part in (tostring(path) .. "."):gmatch("([^%.]*)%.") do
        local head = part:match("^([^%[]*)")
        if head ~= "" then keys[#keys + 1] = head end
        for idx in part:gmatch("%[([^%]]*)%]") do keys[#keys + 1] = idx end
    end
    local cur = facts
    for _, k in ipairs(keys) do
        if type(cur) ~= 'table' then return nil end
        local n = tonumber(k)
        -- JSON arrays arrive 1-based; [0] is the first element
        if n and #cur > 0 then cur = cur[n + 1] else cur = cur[k] end
        if cur == nil then return nil end
    end
    return cur
end

function W.format_fact(v)
    local t = type(v)
    if t == 'number' then
        if v == math.floor(v) and math.abs(v) < 1e15 then return string.format("%.0f", v) end
        local s = string.format("%.2f", v):gsub("0+$", ""):gsub("%.$", "")
        return s
    elseif t == 'boolean' then
        return v and "yes" or "no"
    elseif t == 'string' then
        return v
    elseif t == 'table' and #v > 0 then
        local out = {}
        for i, x in ipairs(v) do out[i] = W.format_fact(x) or "" end
        return table.concat(out, ", ")
    end
    return nil
end

-- `{{list:path|fallback}}`: a table of a server's deployables, troops or
-- actions, as Markdown (cfgTokens.ts, applyListTokens).
local function num(v)
    if type(v) == 'number' then return W.format_fact(v) end
    if v == nil or type(v) == 'table' or type(v) == 'userdata' then return "" end
    return tostring(v)
end
local function cell(v) return (tostring(v):gsub("|", "\\|"):gsub("\n+", " ")) end
local LIST_SHAPES = {
    deployables = { { "Deployable", "Menu group", "Crates required", "Limit", "Provides" }, function(d)
        local path = type(d.path) == 'table' and d.path or {}
        local name = #path > 0 and tostring(path[#path]) or tostring(d.name or "?")
        local group = {}
        for i = 1, #path - 1 do group[i] = tostring(path[i]) end
        local need = {}
        for _, c in ipairs(type(d.crates) == 'table' and d.crates or {}) do
            if type(c) == 'table' then
                need[#need + 1] = num(c.required) .. "x " .. tostring(c.name or "") .. " (" .. num(c.weight) .. " kg)"
            end
        end
        local tags = {}
        if d.jtac == true then tags[#tags + 1] = "JTAC" end
        if d.ewr == true then tags[#tags + 1] = "EWR" end
        if d.gci == true then tags[#tags + 1] = "GCI" end
        return { "**" .. name .. "**", table.concat(group, " / "), #need > 0 and table.concat(need, ", ") or "—",
                 num(d.limit) ~= "" and num(d.limit) or "—", #tags > 0 and table.concat(tags, ", ") or "—" }
    end },
    troops = { { "Squad", "Cost", "Weight", "Can capture", "Limit" }, function(t)
        return { "**" .. tostring(t.name or "?") .. "**", num(t.cost) ~= "" and num(t.cost) or "0",
                 num(t.weight) .. " kg", t.can_capture == true and "yes" or "no", num(t.limit) ~= "" and num(t.limit) or "—" }
    end },
    actions = { { "Action", "Cost", "Type", "Limit" }, function(a)
        return { "**" .. tostring(a.name or "?") .. "**", num(a.cost) ~= "" and num(a.cost) or "0",
                 type(a.kind) == 'string' and a.kind or "—", num(a.limit) ~= "" and num(a.limit) or "—" }
    end },
}

local function list_table(path, facts)
    local shape = LIST_SHAPES[path:match("^[^%.]+") or ""]
    local v = W.lookup(facts, path)
    if not (shape and type(v) == 'table' and #v > 0) then return nil end
    local lines = { "| " .. table.concat(shape[1], " | ") .. " |" }
    local rule = {}
    for i = 1, #shape[1] do rule[i] = "---" end
    lines[2] = "|" .. table.concat(rule, "|") .. "|"
    for _, row in ipairs(v) do
        if type(row) == 'table' then
            local cells = shape[2](row)
            for i, c in ipairs(cells) do cells[i] = trim(cell(c)) end
            lines[#lines + 1] = "| " .. table.concat(cells, " | ") .. " |"
        end
    end
    return table.concat(lines, "\n")
end

-- Both kinds of token, lists first (a row may hold a cfg token). A token
-- that can't be resolved reads as its fallback, then as its path: never as
-- nothing, which would turn "worth 350 points" into "worth points".
function W.apply_tokens(md, facts)
    md = tostring(md or "")
    if not md:find("{{", 1, true) then return md end
    local function split(inner)
        local path, fallback = inner:match("^([^|]*)|(.*)$")
        if not path then path = inner end
        return trim(path), fallback and trim(fallback) or nil
    end
    md = md:gsub("{{%s*list:([^}]-)}}", function(inner)
        local path, fallback = split(inner)
        local t = list_table(path, facts)
        if t then return t end
        if fallback and fallback ~= "" then return fallback end
        return "_No " .. path:gsub("%.", " ", 1) .. " are configured on this server._"
    end)
    md = md:gsub("{{%s*cfg:([^}]-)}}", function(inner)
        local path, fallback = split(inner)
        local v = W.format_fact(W.lookup(facts, path))
        if v and v ~= "" then return v end
        if fallback then return fallback end
        return path
    end)
    return md
end

-- A link in page `slug` to another page, resolved the way bfwiki's
-- resolveHref does (relative to the page, ".md" dropped), as a slug; nil
-- for an outside link, an anchor, or anything that isn't a page.
function W.resolve(href, slug)
    href = trim(href)
    if href == "" or href:sub(1, 1) == "#" or href:match("^%a[%w%+%.%-]*:") or href:sub(1, 2) == "//" then
        return nil
    end
    href = href:gsub("#.*$", ""):gsub("%?.*$", "")
    if href == "" or href:sub(1, 5) == "/api/" then return nil end
    local parts = {}
    if href:sub(1, 1) ~= "/" then
        for seg in (tostring(slug or "") .. "/"):gmatch("([^/]*)/") do parts[#parts + 1] = seg end
        parts[#parts] = nil  -- the page itself: links resolve against its folder
    end
    for seg in (href .. "/"):gmatch("([^/]*)/") do
        if seg == ".." then
            if #parts > 0 then parts[#parts] = nil end
        elseif seg ~= "." and seg ~= "" then
            parts[#parts + 1] = seg
        end
    end
    local out = table.concat(parts, "/"):gsub("%.[mM][dD]$", ""):gsub("%.markdown$", "")
    if out == "" or not out:match("^[%w%-%._/]+$") then return nil end
    return out
end

-- Inline Markdown reduced to plain text. Returns the text, the links to
-- other pages ({ text, slug }) and the pictures in it ({ alt, src }).
-- Outside links stay in the text as "text (address)".
function W.inline(s, slug, api)
    s = tostring(s or "")
    local keep, links, pics = {}, {}, {}
    local function stash(v)
        keep[#keep + 1] = v
        return "\1" .. #keep .. "\2"
    end
    -- code first: what is inside `...` is shown as it is
    s = s:gsub("(`+)(.-)%1", function(_, code) return stash(trim(code)) end)
    s = s:gsub("\\([%p])", function(c) return stash(c) end)
    s = s:gsub("!%[([^%]]*)%]%(%s*([^%)%s]*)[^%)]*%)", function(alt, src)
        pics[#pics + 1] = { alt = alt, src = src }
        return ""
    end)
    s = s:gsub("%[([^%]]*)%]%(%s*([^%)%s]*)[^%)]*%)", function(text, href)
        local target = W.resolve(href, slug)
        if target then
            links[#links + 1] = { text = trim(text), slug = target }
            return text
        end
        if href:sub(1, 1) == "#" or href == "" then return text end
        if href:sub(1, 5) == "/api/" and api then href = api .. href end
        if trim(text) == href or text == "" then return stash(href) end
        return text .. " (" .. stash(href) .. ")"
    end)
    s = s:gsub("<(https?://[^>%s]+)>", function(u) return stash(u) end)
    s = s:gsub("<[bB][rR]%s*/?>", "\n")
    s = s:gsub("</?%a[^>]*>", "")
    s = s:gsub("%*%*%*(.-)%*%*%*", "%1"):gsub("%*%*(.-)%*%*", "%1"):gsub("__(.-)__", "%1")
    s = s:gsub("~~(.-)~~", "%1")
    s = s:gsub("%*([^%*%s][^%*]-)%*", "%1")
    s = (" " .. s .. " "):gsub("([%s%(%[\"'])_([^_%s][^_]-)_([%s%.,;:!%?%)%]\"'])", "%1%2%3")
    s = s:sub(2, -2)
    s = s:gsub("&(#?%w+);", function(e)
        if ENTITIES[e] then return ENTITIES[e] end
        local n = tonumber(e:match("^#(%d+)$") or "")
        if n and n < 128 then return string.char(n) end
        return "&" .. e .. ";"
    end)
    for _ = 1, 3 do  -- a stash can hold a stash (code inside a link's text)
        s = s:gsub("\1(%d+)\2", function(i) return keep[tonumber(i)] end)
    end
    return s, links, pics
end

-- ── Pure: blocks ──

local CUSTOM = { f10menu = true, dcsmessage = true, cockpitpanel = true, wheel = true,
                 steps = true, statushexes = true, hexlegend = true, mapsymbols = true }

local function is_rule(l) return l:match("^%s*[-*_][-*_%s]*$") and select(2, l:gsub("[-*_]", "")) >= 3 end
local function list_item(l)
    local sp, mark, rest = l:match("^(%s*)([-*+])%s+(.*)$")
    if sp then return #sp:gsub("\t", "    "), nil, rest end
    local sp2, n, rest2 = l:match("^(%s*)(%d+)[.)]%s+(.*)$")
    if sp2 then return #sp2:gsub("\t", "    "), tonumber(n), rest2 end
end
-- |---|:--:| under a table's header row
local function is_table_rule(l)
    return l:match("^[%s|:%-]+$") ~= nil and l:find("-", 1, true) ~= nil and l:find("|", 1, true) ~= nil
end
local function split_row(l)
    l = trim(l):gsub("^|", ""):gsub("|$", "")
    l = l:gsub("\\|", "\1")
    local cells = {}
    for c in (l .. "|"):gmatch("([^|]*)|") do cells[#cells + 1] = trim(c:gsub("\1", "|")) end
    return cells
end
local function starts_block(l, nxt)
    return l:match("^%s*```") or l:match("^%s*~~~") or l:match("^#+%s") or l:match("^%s*>")
        or list_item(l) or is_rule(l) or l:match("^%s*!%[[^%]]*%]%([^%)]*%)%s*$")
        or (nxt and l:find("|", 1, true) and is_table_rule(nxt))
end

-- Markdown to a list of blocks: heading, para, list, code, table, quote,
-- image, rule, and the custom fenced blocks (kind = their language).
function W.parse(md)
    local lines = {}
    md = tostring(md or ""):gsub("\r", ""):gsub("<!%-%-.-%-%->", "")
    for l in (md .. "\n"):gmatch("(.-)\n") do lines[#lines + 1] = l end
    local blocks, i = {}, 1
    while i <= #lines do
        local l = lines[i]
        local fence = l:match("^%s*(```+)") or l:match("^%s*(~~~+)")
        if fence then
            local lang = trim(l:sub(l:find(fence, 1, true) + #fence)):match("^([%w%-_]*)") or ""
            local body = {}
            i = i + 1
            while i <= #lines and not lines[i]:match("^%s*" .. fence:gsub("%p", "%%%0")) do
                body[#body + 1] = lines[i]
                i = i + 1
            end
            i = i + 1
            lang = lang:lower()
            blocks[#blocks + 1] = { kind = CUSTOM[lang] and lang or "code", lang = lang, text = table.concat(body, "\n") }
        elseif trim(l) == "" then
            i = i + 1
        elseif l:match("^#+%s") then
            local h, text = l:match("^(#+)%s+(.-)%s*#*%s*$")
            blocks[#blocks + 1] = { kind = "heading", level = #h, text = text }
            i = i + 1
        elseif is_rule(l) then
            blocks[#blocks + 1] = { kind = "rule" }
            i = i + 1
        elseif l:find("|", 1, true) and lines[i + 1] and is_table_rule(lines[i + 1]) then
            local t = { kind = "table", head = split_row(l), rows = {} }
            i = i + 2
            while i <= #lines and trim(lines[i]) ~= "" and lines[i]:find("|", 1, true) do
                t.rows[#t.rows + 1] = split_row(lines[i])
                i = i + 1
            end
            blocks[#blocks + 1] = t
        elseif l:match("^%s*>") then
            local inner = {}
            while i <= #lines and lines[i]:match("^%s*>") do
                inner[#inner + 1] = lines[i]:gsub("^%s*>%s?", "")
                i = i + 1
            end
            blocks[#blocks + 1] = { kind = "quote", blocks = W.parse(table.concat(inner, "\n")) }
        elseif list_item(l) then
            local items, stack = {}, {}
            while i <= #lines do
                local cur = lines[i]
                local ind, n, rest = list_item(cur)
                -- bullets then numbers (or back) at the top: a new list
                if ind and #items > 0 and ind <= stack[1] and (n ~= nil) ~= (items[1].n ~= nil) then break end
                if ind then
                    while #stack > 0 and ind < stack[#stack] do table.remove(stack) end
                    if #stack == 0 or ind > stack[#stack] then stack[#stack + 1] = ind end
                    local check = rest:match("^%[([ xX])%]%s+")
                    if check then rest = (check == " " and "☐ " or "☑ ") .. rest:gsub("^%[[ xX]%]%s+", "") end
                    items[#items + 1] = { level = #stack - 1, n = n, text = rest }
                    i = i + 1
                elseif trim(cur) == "" then
                    -- a blank line ends the list unless more of it follows
                    local nxt = lines[i + 1]
                    if nxt and (list_item(nxt) or (nxt:match("^%s%s+%S") and #items > 0)) then
                        i = i + 1
                    else
                        break
                    end
                elseif #items > 0 and not starts_block(cur, lines[i + 1]) then
                    items[#items].text = items[#items].text .. " " .. trim(cur)  -- continuation
                    i = i + 1
                else
                    break
                end
            end
            blocks[#blocks + 1] = { kind = "list", items = items }
        elseif l:match("^%s*!%[[^%]]*%]%([^%)]*%)%s*$") then
            local alt, src = l:match("!%[([^%]]*)%]%(%s*([^%)%s]*)")
            blocks[#blocks + 1] = { kind = "image", alt = alt, src = src }
            i = i + 1
        else
            local para = {}
            while i <= #lines and trim(lines[i]) ~= "" and (#para == 0 or not starts_block(lines[i], lines[i + 1])) do
                local cur = lines[i]
                -- two trailing spaces or a backslash: a line break
                local hard = cur:match("  $") or cur:match("\\$")
                para[#para + 1] = trim(cur:gsub("\\$", "")) .. (hard and "\n" or " ")
                i = i + 1
            end
            blocks[#blocks + 1] = { kind = "para", text = trim(table.concat(para)) }
        end
    end
    return blocks
end

-- ── Rows ──
-- ctx: { slug, width, x (indent), open(slug), image(src, alt, x, width) -> rows,
--        skin (override for every text row, e.g. quotes), json(text) -> value }

local function push(rows, r) rows[#rows + 1] = r return r end

-- Wrapped text. With a `prefix` (a list marker) the first line starts with
-- it and the others hang `hang` pixels in, under the text.
local function text_rows(rows, text, x, width, skin, font_px, prefix, hang)
    hang = prefix and (hang or px(14)) or 0
    local lines = wrap(text, width - hang, font_px)
    for k, line in ipairs(lines) do
        if k == 1 and prefix then
            push(rows, { text = prefix .. line, skin = skin, x = x, w = width })
        else
            push(rows, { text = line, skin = skin, x = x + hang, w = width - hang })
        end
    end
    return #lines
end

local function link_buttons(rows, links, ctx, x, width)
    local seen = {}
    for _, lk in ipairs(links) do
        if not seen[lk.slug] then
            seen[lk.slug] = true
            local target = lk.slug
            push(rows, { button = true, skin = skins.row_sub, x = x, w = width,
                         text = "  " .. (lk.text ~= "" and lk.text or target) .. "  >",
                         action = function() ctx.open(target) end })
        end
    end
end

local function pic_rows(rows, pics, ctx, x, width)
    for _, p in ipairs(pics) do
        for _, r in ipairs(ctx.image(p.src, p.alt, x, width)) do push(rows, r) end
    end
end

-- One run of inline text: a lone link is a button, anything else wrapped
-- text with its links as buttons underneath.
local function inline_rows(rows, raw, ctx, x, width, skin, font_px, prefix, hang)
    local text, links, pics = W.inline(raw, ctx.slug, ctx.api)
    local h = prefix and (hang or px(14)) or 0
    if #links == 1 and trim(text) == links[1].text then
        -- nothing but a link: the button is the line
        link_buttons(rows, links, ctx, x + h, width - h)
    else
        if trim(text) ~= "" then text_rows(rows, text, x, width, skin, font_px, prefix, hang) end
        link_buttons(rows, links, ctx, x + h, width - h)
    end
    pic_rows(rows, pics, ctx, x + h, width - h)
end

local function mono_rows(rows, text, x, width)
    local per = math.max(16, math.floor(width / (px(12) * (settings.text_scale or 1) * 0.62)))
    for line in (tostring(text) .. "\n"):gmatch("(.-)\n") do
        line = line:gsub("\t", "    ")
        if line == "" then push(rows, { text = "", skin = skins.mono, mono = true, x = x, w = width }) end
        while #line > 0 do
            push(rows, { text = line:sub(1, per), skin = skins.mono, mono = true, x = x, w = width })
            line = line:sub(per + 1)
        end
    end
    return per
end

local function gap(rows, h) push(rows, { text = "", h = px(h or 6) }) end

local custom_rows  -- forward

function W.rows(blocks, ctx)
    local rows = {}
    local x0 = ctx.x or 0
    local width = ctx.width - x0
    for bi, b in ipairs(blocks) do
        local k = b.kind
        if k == "heading" then
            if bi > 1 then gap(rows, b.level <= 2 and 8 or 4) end
            local skin, size = skins.bold, px(FONT_PX)
            if b.level == 1 then skin, size = skins.h1 or skins.head, px(16)
            elseif b.level == 2 then skin, size = skins.head, px(14) end
            local text, links = W.inline(b.text, ctx.slug, ctx.api)
            text_rows(rows, text, x0, width, ctx.skin or skin, size)
            link_buttons(rows, links, ctx, x0, width)
        elseif k == "para" then
            inline_rows(rows, b.text, ctx, x0, width, ctx.skin or skins.text)
            gap(rows, 4)
        elseif k == "list" then
            for _, it in ipairs(b.items) do
                local ind = x0 + it.level * px(16)
                local mark = it.n and (tostring(it.n) .. ". ") or "•  "
                local hang = it.n and px(10 + 7 * #tostring(it.n)) or px(14)
                inline_rows(rows, it.text, ctx, ind, ctx.width - ind, ctx.skin or skins.text, nil, mark, hang)
            end
            gap(rows, 4)
        elseif k == "code" then
            mono_rows(rows, b.text, x0, width)
            gap(rows, 4)
        elseif k == "table" then
            local head, body = {}, {}
            for c, v in ipairs(b.head) do head[c] = (W.inline(v, ctx.slug, ctx.api)) end
            for r, row in ipairs(b.rows) do
                body[r] = {}
                for c = 1, #head do body[r][c] = (W.inline(row[c] or "", ctx.slug, ctx.api)) end
            end
            local per = math.max(16, math.floor(width / (px(12) * (settings.text_scale or 1) * 0.62)))
            local widths, total = {}, 0
            for c = 1, #head do
                widths[c] = #head[c]
                for _, row in ipairs(body) do widths[c] = math.max(widths[c], #row[c]) end
                total = total + widths[c] + (c > 1 and 2 or 0)
            end
            if total <= per then
                local function line(cells)
                    local out = {}
                    for c = 1, #head do out[c] = cells[c] .. string.rep(" ", widths[c] - #cells[c]) end
                    return (table.concat(out, "  "):gsub("%s+$", ""))
                end
                push(rows, { text = line(head), skin = skins.mono, mono = true, x = x0, w = width })
                push(rows, { text = string.rep("-", total), skin = skins.mono, mono = true, x = x0, w = width })
                for _, row in ipairs(body) do
                    push(rows, { text = line(row), skin = skins.mono, mono = true, x = x0, w = width })
                end
            else
                -- too wide for the panel: one card per row, "Header: value"
                for _, row in ipairs(body) do
                    text_rows(rows, row[1], x0, width, skins.bold)
                    for c = 2, #head do
                        if row[c] ~= "" then
                            text_rows(rows, (head[c] ~= "" and (head[c] .. ": ") or "") .. row[c],
                                x0 + px(12), width - px(12), ctx.skin or skins.text)
                        end
                    end
                    gap(rows, 3)
                end
            end
            gap(rows, 4)
        elseif k == "quote" then
            local sub = {}
            for kk, vv in pairs(ctx) do sub[kk] = vv end
            sub.x, sub.skin = x0 + px(8), skins.answer
            for _, r in ipairs(W.rows(b.blocks, sub)) do push(rows, r) end
        elseif k == "image" then
            pic_rows(rows, { { alt = b.alt, src = b.src } }, ctx, x0, width)
            gap(rows, 4)
        elseif k == "rule" then
            push(rows, { text = string.rep("─", math.max(4, math.floor(width / px(9)))), skin = skins.label, x = x0, w = width, h = px(12) })
        elseif custom_rows[k] then
            local ok, err = pcall(custom_rows[k], rows, b.text, ctx, x0, width)
            if not ok then
                logmsg("wiki: " .. k .. " block in " .. tostring(ctx.slug) .. ": " .. tostring(err))
                mono_rows(rows, b.text, x0, width)
            end
            gap(rows, 6)
        end
    end
    return rows
end

-- ── Pure: the custom blocks ──

local function json_of(ctx, text)
    local ok, v = pcall(ctx.json, text)
    if ok and type(v) == 'table' then return v end
    error("not JSON")
end
local function str(v) return (type(v) == 'string' or type(v) == 'number') and tostring(v) or nil end

custom_rows = {}

-- An F10 menu (or wheel) as an indented tree: a submenu with ">" after it,
-- a command with what F10 answers under it.
local function tree_rows(rows, items, ctx, x, width, depth, wheel)
    for _, it in ipairs(type(items) == 'table' and items or {}) do
        if type(it) == 'table' then
            local ind = x + depth * px(16)
            local label = str(it.label) or str(it.name) or "?"
            local kids = type(it.items) == 'table' and #it.items > 0
            local value = str(it.value) or str(it.info)
            local text
            if wheel then
                text = (depth == 0 and "◯ " or "– ") .. label .. (value and ("   " .. value) or "") .. (kids and "  >" or "")
            else
                text = kids and (label .. "  >") or label
            end
            text_rows(rows, text, ind, x + width - ind, kids and skins.good or skins.text)
            local says = str(it.says)
            if says then
                for _, line in ipairs(wrap(says, x + width - ind - px(28))) do
                    push(rows, { text = "  " .. line, skin = skins.answer, x = ind + px(16), w = x + width - ind - px(16) })
                end
            end
            local note = str(it.note)
            if note then text_rows(rows, note, ind + px(16), x + width - ind - px(16), skins.label) end
            if kids then tree_rows(rows, it.items, ctx, x, width, depth + 1, wheel) end
        end
    end
end

custom_rows.f10menu = function(rows, text, ctx, x, width)
    local m = json_of(ctx, text)
    local root = str(m.root) or "F10 Other"
    push(rows, { text = "F10 RADIO MENU  --  " .. root:gsub("^F10%s*", ""), skin = skins.label, x = x, w = width })
    tree_rows(rows, m.items, ctx, x + px(8), width - px(8), 0, false)
end

custom_rows.wheel = function(rows, text, ctx, x, width)
    local m = json_of(ctx, text)
    push(rows, { text = "THE WHEEL" .. (str(m.title) and ("  --  " .. str(m.title)) or ""), skin = skins.label, x = x, w = width })
    tree_rows(rows, m.items, ctx, x + px(8), width - px(8), 0, true)
end

custom_rows.cockpitpanel = function(rows, text, ctx, x, width)
    local m = json_of(ctx, text)
    push(rows, { text = "THE PANEL" .. (str(m.tab) and ("  --  " .. str(m.tab):upper() .. " TAB") or ""), skin = skins.label, x = x, w = width })
    local ix, iw = x + px(8), width - px(8)
    for _, r in ipairs(type(m.rows) == 'table' and m.rows or {}) do
        if type(r) == 'table' then
            if str(r.heading) then text_rows(rows, str(r.heading), ix, iw, skins.head, px(14)) end
            if str(r.text) then text_rows(rows, str(r.text), ix, iw, skins.text) end
            if str(r.label) and str(r.value) then
                text_rows(rows, str(r.label) .. ":  " .. str(r.value), ix, iw, skins.text)
            end
            if str(r.button) then text_rows(rows, "[ " .. str(r.button) .. " ]", ix, iw, skins.bold) end
            if str(r.says) then
                for _, line in ipairs(wrap(str(r.says), iw - px(28))) do
                    push(rows, { text = "  " .. line, skin = skins.answer, x = ix + px(16), w = iw - px(16) })
                end
            end
            if str(r.note) then text_rows(rows, str(r.note), ix + px(16), iw - px(16), skins.label) end
        end
    end
    if str(m.status) then text_rows(rows, str(m.status), ix, iw, skins.status, px(12)) end
end

custom_rows.dcsmessage = function(rows, text, ctx, x, width)
    push(rows, { text = "ON SCREEN IN DCS", skin = skins.label, x = x, w = width })
    for _, line in ipairs(wrap(text, width - px(20))) do
        push(rows, { text = "  " .. line, skin = skins.answer, x = x + px(8), w = width - px(8) })
    end
end

custom_rows.steps = function(rows, text, ctx, x, width)
    local steps = json_of(ctx, text)
    for n, s in ipairs(steps) do
        if type(s) == 'table' then
            text_rows(rows, n .. ".  " .. (str(s.title) or ""), x, width, skins.bold)
            if str(s.text) then
                local sub = {}
                for kk, vv in pairs(ctx) do sub[kk] = vv end
                sub.x, sub.width = x + px(22), x + width
                for _, r in ipairs(W.rows(W.parse(str(s.text)), sub)) do push(rows, r) end
            end
            if str(s.image) then
                pic_rows(rows, { { alt = str(s.alt) or str(s.title) or "", src = str(s.image) } }, ctx, x + px(22), width - px(22))
            end
            gap(rows, 4)
        end
    end
end

local function bucket(v)
    v = tonumber(v)
    if not v then return nil end
    return v > 66 and "green" or (v >= 33 and "amber" or "red")
end

custom_rows.statushexes = function(rows, text, ctx, x, width)
    local h = json_of(ctx, text)
    local unlimited, focus = {}, nil
    for _, u in ipairs(type(h.unlimited) == 'table' and h.unlimited or {}) do unlimited[tostring(u)] = true end
    if type(h.focus) == 'table' and #h.focus > 0 then
        focus = {}
        for _, f in ipairs(h.focus) do focus[tostring(f)] = true end
    end
    local NAMES = { { "health", "Health" }, { "logi", "Logistics" }, { "supply", "Supply" }, { "fuel", "Fuel" } }
    for _, nm in ipairs(NAMES) do
        local v = h[nm[1]]
        if (type(v) == 'number' or unlimited[nm[1]]) and (not focus or focus[nm[1]]) then
            local val = unlimited[nm[1]] and "unlimited" or (string.format("%d%%", v) .. "  (" .. bucket(v) .. ")")
            push(rows, { text = string.format("  %-10s %s", nm[2], val), skin = skins.mono, mono = true, x = x, w = width })
        end
    end
    if str(h.caption) then text_rows(rows, str(h.caption), x, width, skins.label) end
end

custom_rows.hexlegend = function(rows, text, ctx, x, width)
    for _, r in ipairs({ { "Green", "Above 66% — fine." }, { "Amber", "33–66% — getting thin." },
                         { "Red", "Below 33% — critical." },
                         { "Gold outline", "That resource is unlimited — it never runs dry." } }) do
        text_rows(rows, r[1] .. ":  " .. r[2], x, width, skins.text)
    end
end

custom_rows.mapsymbols = function(rows, text, ctx, x, width)
    for _, r in ipairs(json_of(ctx, text)) do
        if type(r) == 'table' then
            local side = str(r.side)
            text_rows(rows, (str(r.name) or str(r.icon) or "?") .. (side and ("  (" .. side .. ")") or ""), x, width,
                side == "blue" and skins.blue or (side == "red" and skins.red or skins.bold))
            if str(r.text) then text_rows(rows, str(r.text), x + px(14), width - px(14), skins.text) end
        end
    end
end

-- ── Index, pages, facts ──

local function api_root() return (tostring(cfg.api):gsub("/+$", "")) end

local function server_key() return tostring(cfg.instance or server_name() or "") end

local function page_file(slug) return PAGE_DIR .. slug:gsub("[^%w%-%._]", "~") .. ".lua" end

local function mkdirs()
    if wk.dirs_made then return end
    wk.dirs_made = true
    local base = lfs.writedir() .. [[Mods\Services\BFCockpit\]]
    for _, d in ipairs({ base, base .. [[cache\]], WIKI_DIR, IMG_DIR, PAGE_DIR }) do
        pcall(lfs.mkdir, (d:gsub("\\$", "")))
    end
end

local function load_page_file(slug)
    local chunk = loadfile(page_file(slug))
    if not chunk then return nil end
    setfenv(chunk, {})
    local ok, p = pcall(chunk)
    if ok and type(p) == 'table' and type(p.content) == 'string' then return p end
    return nil
end

local function save_page_file(slug, p)
    mkdirs()
    local ok, err = pcall(function()
        local f, e = io.open(page_file(slug), "w")
        if not f then error(tostring(e)) end
        f:write("return " .. serialize({ title = p.title, section = p.section, updated_at = p.updated_at, content = p.content }) .. "\n")
        f:close()
    end)
    if not ok then logmsg("wiki: could not cache " .. slug .. ": " .. tostring(err)) end
end

local function index_entry(slug)
    return wk.index and wk.index.by_slug[slug] or nil
end

local function set_index(pages, sections, dated)
    local idx = { pages = {}, sections = {}, by_slug = {}, dated = dated }
    local seen = {}
    for _, p in ipairs(list(pages)) do
        if type(p) == 'table' and present(p.slug) then
            local e = { slug = tostring(p.slug), title = present(p.title) and tostring(p.title) or tostring(p.slug),
                        section = present(p.section) and tostring(p.section) or "Other",
                        order = tonumber(p.order) or 0, updated_at = present(p.updated_at) and tostring(p.updated_at) or nil }
            idx.pages[#idx.pages + 1] = e
            idx.by_slug[e.slug] = e
            if not seen[e.section] then seen[e.section] = true; idx.sections[#idx.sections + 1] = e.section end
        end
    end
    -- the server's own section order when it gives one
    if type(sections) == 'table' and #sections > 0 then
        local ordered, have = {}, {}
        for _, s in ipairs(sections) do
            if present(s) and seen[tostring(s)] and not have[tostring(s)] then
                ordered[#ordered + 1] = tostring(s); have[tostring(s)] = true
            end
        end
        for _, s in ipairs(idx.sections) do if not have[s] then ordered[#ordered + 1] = s end end
        idx.sections = ordered
    end
    wk.index = idx
end

local function fetch_index()
    if wk.inflight.index then return end
    wk.inflight.index = true
    api_call("GET", "/api/cockpit/wiki/index", nil, nil, false, function(data, err, status)
        if type(data) == 'table' and type(data.pages) == 'table' then
            wk.inflight.index = nil
            wk.index_err = nil
            set_index(data.pages, data.sections, true)
            redraw()
            return
        end
        if status ~= 404 then
            wk.inflight.index = nil
            wk.index_err = err
            redraw()
            return
        end
        -- a bfdb from before the panel's own index: the web wiki's list,
        -- which has no dates, so pages are cached for this session only
        api_call("GET", "/api/wiki/pages", nil, nil, false, function(d2, e2)
            wk.inflight.index = nil
            if type(d2) == 'table' then
                wk.index_err = nil
                set_index(d2, nil, false)
            else
                wk.index_err = e2
            end
            redraw()
        end)
    end)
end

local function fetch_facts()
    if wk.inflight.facts then return end
    wk.inflight.facts = true
    local key = server_key()
    api_call("GET", "/api/wiki/facts", nil, nil, false, function(data)
        wk.inflight.facts = nil
        wk.facts_for = key
        -- no numbers is fine: every token has its fallback
        if type(data) == 'table' and type(data.facts) == 'table' then
            wk.facts = data.facts
            local inst = type(data.instance) == 'table' and data.instance or {}
            wk.facts_label = present(inst.label) and tostring(inst.label) or (present(inst.id) and tostring(inst.id) or nil)
        end
        wk.facts_rev = wk.facts_rev + 1
        redraw()
    end)
end

-- The page's text as it is now, from memory, the disk cache or the server.
-- nil while it is being fetched.
local function page_now(slug)
    local e = index_entry(slug)
    local want = e and e.updated_at or nil
    local p = wk.pages[slug]
    local dated = wk.index and wk.index.dated
    if p and (not dated or not want or p.updated_at == want) then return p end
    if dated and want and not p then
        local disk = load_page_file(slug)
        if disk and disk.updated_at == want then
            wk.pages[slug] = disk
            return disk
        end
    end
    if not wk.inflight["page:" .. slug] and not wk.page_err[slug] then
        wk.inflight["page:" .. slug] = true
        api_call("GET", "/api/wiki/pages/" .. slug, nil, nil, false, function(data, err, status)
            wk.inflight["page:" .. slug] = nil
            if type(data) == 'table' and present(data.content) then
                local got = { title = present(data.title) and tostring(data.title) or slug,
                              section = present(data.section) and tostring(data.section) or "",
                              updated_at = present(data.updated_at) and tostring(data.updated_at) or nil,
                              content = tostring(data.content) }
                wk.pages[slug] = got
                wk.parsed[slug] = nil
                if got.updated_at then save_page_file(slug, got) end
            else
                wk.page_err[slug] = status == 404 and ("There is no page \"" .. slug .. "\" in this wiki.") or err
            end
            redraw()
        end)
    end
    return p  -- a stale copy is better than nothing while the new one comes
end

-- ── Pictures ──

local function img_width(w)
    for _, s in ipairs(IMG_WIDTHS) do if s >= w then return s end end
    return IMG_WIDTHS[#IMG_WIDTHS]
end

local function scan_img_dir()
    wk.img_disk = {}
    local ok = pcall(function()
        for f in lfs.dir(IMG_DIR) do
            local id, bw, w, h = f:match("^([%x%-]+)%-(%d+)%-(%d+)x(%d+)%.%a+$")
            if id then wk.img_disk[id .. "@" .. bw] = { file = IMG_DIR .. f, w = tonumber(w), h = tonumber(h) } end
        end
    end)
    if not ok then wk.img_disk = {} end
end

local pic_skins, pic_skin_count = {}, 0

local function picture_skin(file, w, h)
    local key = file .. "|" .. w .. "|" .. h
    local s = pic_skins[key]
    if s then return s end
    if pic_skin_count > 200 then pic_skins, pic_skin_count = {}, 0 end
    s = deep_copy(Skin.staticSkin())
    for _, st in pairs(s.skinData.states) do
        for i in pairs(st) do
            st[i].picture = { file = file, color = "0xffffffff", horzAlign = { type = "min", offset = 0 },
                              vertAlign = { type = "min", offset = 0 }, size = { horz = w, vert = h }, userTexSampler = 0 }
        end
    end
    pic_skins[key] = s
    pic_skin_count = pic_skin_count + 1
    return s
end

local function start_image(key, id, bw)
    local img = { state = "info" }
    wk.images[key] = img
    api_call("GET", "/api/cockpit/wiki/image/" .. id .. "/info", { w = tostring(bw) }, nil, false, function(info, err)
        if type(info) ~= 'table' or not present(info.url) or not tostring(info.url):match("^/api/") then
            img.state, img.why = "failed", err or "no picture"
            if err then logmsg("wiki picture " .. id .. ": " .. tostring(err)) end
            redraw()
            return
        end
        local w, h = tonumber(info.width), tonumber(info.height)
        local ext = info.format == "jpg" and "jpg" or "png"
        if not (w and h and w > 0 and h > 0) then
            img.state, img.why = "failed", "no size"
            redraw()
            return
        end
        local file = IMG_DIR .. id .. "-" .. bw .. "-" .. w .. "x" .. h .. "." .. ext
        local did, derr = http.download(api_root() .. tostring(info.url), file, 60)
        if not did then
            img.state, img.why = "failed", derr
            logmsg("wiki picture " .. id .. ": " .. tostring(derr))
            redraw()
            return
        end
        img.state = "downloading"
        pending_raw[did] = function(r)
            if r.status >= 200 and r.status < 300 and not r.error then
                img.state, img.file, img.w, img.h = "ready", file, w, h
            else
                img.state, img.why = "failed", r.error
                logmsg("wiki picture " .. id .. ": " .. tostring(r.error))
            end
            redraw()
        end
    end)
end

-- Rows for one picture: the picture itself once it is on disk, otherwise
-- its alt text as a caption (while it comes, or for good when it can't).
local function image_rows(src, alt, x, width)
    local rows = {}
    alt = trim(W.inline(alt or ""))
    local id = tostring(src or ""):match("/api/wiki/images/(%x%x%x%x%x%x%x%x%-[%x%-]+)")
    local why
    if not id then
        why = "picture"
    elseif not (http and http.download) then
        why = "picture -- needs the newer bfcockpit_net.dll"
    else
        local bw = img_width(width)
        local key = id .. "@" .. bw
        if not wk.img_disk then scan_img_dir() end
        local img = wk.images[key]
        if not img and wk.img_disk[key] then
            local d = wk.img_disk[key]
            img = { state = "ready", file = d.file, w = d.w, h = d.h }
            wk.images[key] = img
        end
        if not img then
            mkdirs()
            start_image(key, id, bw)
            img = wk.images[key]
        end
        if img.state == "ready" then
            local dw = math.min(width, img.w)
            local dh = math.floor(dw * img.h / img.w + 0.5)
            local max_h = px(520)
            if dh > max_h then dw, dh = math.floor(dw * max_h / dh + 0.5), max_h end
            rows[1] = { pic = img.file, pic_skin = picture_skin(img.file, dw, dh), x = x, w = dw, h = dh }
            return rows
        end
        why = img.state == "failed" and "picture" or "picture, loading"
    end
    text_rows(rows, "[" .. why .. "]" .. (alt ~= "" and ("  " .. alt) or ""), x, width, skins.label)
    return rows
end
W.image_rows = image_rows

-- ── Views ──

local function scroll_top() wk.scroll_top = true end

local function go(nav)
    local ok, v = pcall(function() return w_pane:getVertScrollValue() end)
    wk.nav.scroll = ok and v or nil
    wk.stack[#wk.stack + 1] = wk.nav
    if #wk.stack > 30 then table.remove(wk.stack, 1) end
    wk.nav = nav
    scroll_top()
    render()
end

local function open_page(slug)
    wk.page_err[slug] = nil
    go({ view = "page", slug = slug })
end

local function clear_search()
    wk.query, wk.hits, wk.searched, wk.search_err = "", nil, nil, nil
    if w_search then pcall(function() w_search:setText("") end) end
end

local function back()
    if wk.query ~= "" then
        clear_search()
    elseif #wk.stack > 0 then
        wk.nav = table.remove(wk.stack)
        wk.restore = wk.nav.scroll
    else
        wk.nav = { view = "home" }
    end
    render()
end

local function crumb()
    if wk.query ~= "" then return "WIKI > Search" end
    local n = wk.nav
    if n.view == "section" then return "WIKI > " .. tostring(n.section) end
    if n.view == "page" then
        local e = index_entry(n.slug)
        local p = wk.pages[n.slug]
        local section = (e and e.section) or (p and p.section) or ""
        local title = (e and e.title) or (p and p.title) or n.slug
        return "WIKI > " .. (section ~= "" and (section .. " > ") or "") .. title
    end
    return "WIKI"
end

local function pages_in(section)
    local out = {}
    for _, p in ipairs(wk.index and wk.index.pages or {}) do
        if p.section == section then out[#out + 1] = p end
    end
    table.sort(out, function(a, b)
        if a.order ~= b.order then return a.order < b.order end
        return a.title < b.title
    end)
    return out
end

local function page_button(rows, p, text)
    local slug = p.slug
    push(rows, { button = true, skin = skins.row, text = "  " .. (text or p.title), action = function() open_page(slug) end })
end

local function view_home()
    local rows = {}
    if not wk.index then
        add_text(rows, wk.index_err or "Reading the wiki...", wk.index_err and skins.bad or skins.label)
        return rows
    end
    if wk.index_err then add_text(rows, wk.index_err, skins.bad) end
    if #wk.index.pages == 0 then
        add_text(rows, "This server's wiki has no pages yet.", skins.label)
        return rows
    end
    add_text(rows, string.format("%d pages. Pick a section, or type in the box above to find a page.", #wk.index.pages), skins.label)
    gap(rows, 2)
    for _, s in ipairs(wk.index.sections) do
        local section = s
        push(rows, { button = true, skin = skins.row_sub, text = string.format("  %s   (%d)  >", s, #pages_in(s)),
                     action = function() go({ view = "section", section = section }) end })
    end
    return rows
end

local function view_section(section)
    local rows = {}
    local list_ = pages_in(section)
    if #list_ == 0 then
        add_text(rows, wk.index and "Nothing in this section any more." or "Reading the wiki...", skins.label)
        return rows
    end
    push(rows, { text = section:upper(), skin = skins.head })
    for _, p in ipairs(list_) do page_button(rows, p) end
    return rows
end

local function page_ctx(slug, width)
    return { slug = slug, width = width, x = 0, open = open_page, image = image_rows, api = api_root(),
             json = function(t) return net.json2lua(t) end }
end

local function view_page(slug)
    local rows = {}
    local p = page_now(slug)
    if not p then
        if wk.page_err[slug] then
            add_text(rows, wk.page_err[slug], skins.bad)
            push(rows, { button = true, skin = skins.tool, x = 0, w = px(120), text = "TRY AGAIN",
                         action = function() wk.page_err[slug] = nil; render() end })
        else
            add_text(rows, "Reading the page...", skins.label)
        end
        return rows
    end
    -- a server change brings other numbers
    if wk.facts_for ~= server_key() and not wk.inflight.facts then fetch_facts() end
    local width = content_width()
    local key = tostring(p.updated_at) .. "|" .. wk.facts_rev
    local parsed = wk.parsed[slug]
    if not parsed or parsed.key ~= key then
        parsed = { key = key, blocks = W.parse(W.apply_tokens(p.content, wk.facts)) }
        wk.parsed[slug] = parsed
    end
    local blocks = parsed.blocks
    -- the page's own "# Title" stands for the title; otherwise add it
    if not (blocks[1] and blocks[1].kind == "heading" and blocks[1].level == 1) then
        text_rows(rows, p.title, 0, width, skins.h1 or skins.head, px(16))
    end
    for _, r in ipairs(W.rows(blocks, page_ctx(slug, width))) do push(rows, r) end
    if p.content:find("{{", 1, true) and wk.facts_label then
        add_text(rows, "Numbers on this page are " .. wk.facts_label .. "'s.", skins.label)
    end
    -- the next page in its section, like turning a page
    local e = index_entry(slug)
    if e then
        local list_ = pages_in(e.section)
        for i, q in ipairs(list_) do
            if q.slug == slug and list_[i + 1] then
                gap(rows, 6)
                local nxt = list_[i + 1]
                push(rows, { button = true, skin = skins.row_sub, text = "  NEXT:  " .. nxt.title .. "  >",
                             action = function() open_page(nxt.slug) end })
            end
        end
    end
    if present(p.updated_at) then
        add_text(rows, "Updated " .. tostring(p.updated_at):gsub("T.*$", ""), skins.label)
    end
    return rows
end

local function view_search()
    local rows = {}
    local q = wk.query:lower()
    local titles, shown = {}, {}
    for _, p in ipairs(wk.index and wk.index.pages or {}) do
        if p.title:lower():find(q, 1, true) then titles[#titles + 1] = p end
    end
    push(rows, { text = #titles > 0 and "PAGE TITLES" or "No page title has that in it.", skin = #titles > 0 and skins.head or skins.label })
    for _, p in ipairs(titles) do
        page_button(rows, p, p.title .. "   -- " .. p.section)
        shown[p.slug] = true
    end
    gap(rows, 6)
    if wk.search_err then
        add_text(rows, wk.search_err, skins.label)
    elseif wk.searched ~= wk.query or not wk.hits then
        add_text(rows, "Searching the text...", skins.label)
    else
        local more = {}
        for _, h in ipairs(wk.hits) do
            if type(h) == 'table' and present(h.slug) and not shown[tostring(h.slug)] then more[#more + 1] = h end
        end
        push(rows, { text = #more > 0 and "IN THE TEXT" or "Nothing else in the text.", skin = #more > 0 and skins.head or skins.label })
        for _, h in ipairs(more) do
            local slug = tostring(h.slug)
            push(rows, { button = true, skin = skins.row, text = "  " .. tostring(h.title or slug) .. "   -- " .. tostring(h.section or ""),
                         action = function() open_page(slug) end })
            if present(h.snippet) then
                for _, line in ipairs(wrap(tostring(h.snippet), content_width() - px(16), px(12))) do
                    push(rows, { text = line, skin = skins.label, x = px(12), w = content_width() - px(12) })
                end
            end
        end
    end
    return rows
end

renderers.wiki = function()
    if wk.query ~= "" then return view_search() end
    local n = wk.nav
    if n.view == "section" then return view_section(n.section) end
    if n.view == "page" then return view_page(n.slug) end
    return view_home()
end

-- ── What the rest of the panel calls ──

wiki_api.crumb = crumb
wiki_api.back = back
wiki_api.can_back = function() return wk.query ~= "" or #wk.stack > 0 or wk.nav.view ~= "home" end
wiki_api.open = open_page

-- Every frame while the tab is showing: keep the index and the numbers
-- fresh, and run the full-text search once typing stops.
wiki_api.tick = function(now)
    if now - wk.index_at > INDEX_EVERY then
        wk.index_at = now
        fetch_index()
    end
    if wk.facts_for ~= server_key() or now - wk.facts_at > FACTS_EVERY then
        if not wk.inflight.facts then
            wk.facts_at = now
            fetch_facts()
        end
    end
    if wk.query ~= "" and wk.searched ~= wk.query and not wk.inflight.search and now - wk.query_at >= SEARCH_DELAY then
        local q = wk.query
        wk.inflight.search = true
        api_call("GET", "/api/cockpit/wiki/search", { q = q }, nil, false, function(data, err, status)
            wk.inflight.search = nil
            wk.searched = q
            if type(data) == 'table' then
                wk.hits, wk.search_err = list(data.hits), nil
            else
                wk.hits = {}
                wk.search_err = status == 404 and "This server can only search page titles." or err
            end
            if q == wk.query then render() end
        end)
    end
end

wiki_api.after_draw = function()
    if wk.scroll_top then
        wk.scroll_top = false
        pcall(function() w_pane:setVertScrollValue(0) end)
    elseif wk.restore then
        local v = wk.restore
        wk.restore = nil
        pcall(function() w_pane:setVertScrollValue(v) end)
    end
end

local function on_query(text)
    text = trim(text)
    if text == wk.query then return end
    wk.query = text
    wk.query_at = os.clock()
    wk.search_err = nil
    scroll_top()
    render()
end

wiki_api.create = function()
    w_search, w_clear, w_hint = nil, nil, nil
    if not EditBox then
        w_hint = label_widget("Search needs DCS's EditBox -- not available")
        return
    end
    local ok, err = pcall(function()
        w_search = EditBox.new("")
        if skins.edit then w_search:setSkin(skins.edit) end
        pcall(function() w_search:setHintText("Find a page...") end)
        w_search:addChangeCallback(function()
            local ok2, e2 = pcall(function() on_query(w_search:getText()) end)
            if not ok2 then logmsg("wiki search errored: " .. tostring(e2)) end
        end)
        window:insertWidget(w_search)
        w_clear = tool_button("X", "Clear the search", function()
            clear_search()
            render()
        end)
    end)
    if not ok then
        logmsg("wiki search box not available: " .. tostring(err))
        w_search = nil
    end
end

wiki_api.widgets = function()
    local out = {}
    for _, w in ipairs({ w_search, w_clear, w_hint }) do if w then out[#out + 1] = w end end
    return out
end

-- Lay out the search row at y; returns the height it took.
wiki_api.layout = function(pad, y, cw, row, gap_)
    if w_hint then w_hint:setBounds(pad, y, cw - pad * 2, row) end
    if w_search then
        w_search:setBounds(pad, y, cw - pad * 2 - row - gap_, row)
        w_clear:setBounds(cw - pad - row, y, row, row)
    end
    return (w_search or w_hint) and (row + px(6)) or 0
end

-- Give the keyboard back to the sim when the tab or the panel goes away.
wiki_api.unfocus = function()
    if w_search then pcall(function() w_search:setFocused(false) end) end
end

-- A new server: its own numbers; the pages and pictures stay cached.
wiki_api.reset = function()
    wk.facts, wk.facts_label, wk.facts_for, wk.facts_at = nil, nil, nil, -1e9
    wk.facts_rev = wk.facts_rev + 1
    wk.index_at = -1e9
    wk.page_err = {}
    for k, img in pairs(wk.images) do
        if img.state == "failed" then wk.images[k] = nil end
    end
end

if rawget(_G, "BFCOCKPIT_TEST") then
    BFCOCKPIT_TEST.wiki = W
    BFCOCKPIT_TEST.wk = wk
end
end
wiki_module()

-- ── DCS callbacks ────────────────────────────────────────────────────

local create_failed = false

local function reset_session()
    state.menu, state.status, state.ewr, state.cargo = nil, nil, nil, nil
    state.here, state.errors, state.note, state.cargo_cat = {}, {}, nil, nil
    state.feed, state.feed_next, state.feed_off, state.unread, state.answer = {}, 0, false, 0, nil
    state.checked_version = false
    if wiki_api.reset then pcall(wiki_api.reset) end
end

-- Throw the window away so the next frame builds a new one -- for a changed
-- panel size, whose skins and fonts are fixed when the window is made.
local function rebuild(reason)
    if not window then return end
    logmsg("rebuilding the panel: " .. reason)
    local was_visible = visible
    pcall(hide)
    unbind_keys()
    if destroy_wheel then destroy_wheel() end
    pcall(function() window:kill() end)
    window, visible = nil, false
    state.reopen = was_visible
end

-- The hotkey only lives while our window is setVisible(true), and DCS does
-- not leave a Hooks window alone across the move from the main menu into a
-- mission: seen live 2026-10-20 -- made and toggled fine at the main menu,
-- then in a mission neither the hotkey nor the comms key brought it back.
-- So the window is checked every couple of seconds and at each mission start:
-- hidden by DCS -> made visible again (still wearing whatever skin we chose,
-- so a "hidden" panel stays invisible); gone altogether -> rebuilt from
-- scratch on the next frame. Each case is logged once per occurrence, so
-- dcs.log says which one DCS did.
local next_revive = 0

local function revive(reason)
    if not window then return end
    local ok, shown = pcall(function() return window:getVisible() end)
    if not ok then
        logmsg("window was destroyed (" .. reason .. "): " .. tostring(shown) .. " -- rebuilding it")
        window, visible, bound = nil, false, {}
        return
    end
    if shown == false then
        logmsg("window was hidden by DCS (" .. reason .. ") -- making it visible again")
        local ok2, err = pcall(function() window:setVisible(true) end)
        if not ok2 then
            logmsg("could not re-show it: " .. tostring(err) .. " -- rebuilding it")
            window, visible, bound = nil, false, {}
        end
    end
end

-- The Special options page can be changed at any time, including in a
-- mission; pick changes up without a restart. options.lua holds every
-- module's options and can be large, so it is only re-read when DCS has
-- actually written it.
local SETTINGS_EVERY = 5
local next_settings = 0
local options_mtime = nil

local function recheck_settings()
    local ok, mtime = pcall(lfs.attributes, lfs.writedir() .. "Config\\options.lua", "modification")
    mtime = ok and mtime or nil
    if mtime ~= nil and mtime == options_mtime then return end
    options_mtime = mtime
    local s = compute_settings()
    local scale_changed = math.abs(s.scale - settings.scale) > 0.001
        or math.abs(s.text_scale - settings.text_scale) > 0.001
    local wheel_size_changed = math.abs(s.wheel_size - settings.wheel_size) > 0.001
        or math.abs(s.text_scale - settings.text_scale) > 0.001
    -- A size saved before 3.3 doesn't say which Panel size it was saved at.
    if cfg.window and not cfg.window.scale then cfg.window.scale = settings.scale end
    settings = s
    maybe_auto_update()  -- in case it was just turned on
    if wheel_size_changed and not scale_changed and window and destroy_wheel and create_wheel then
        destroy_wheel()
        create_wheel()
    end
    if scale_changed then
        rebuild(string.format("size now %.2f", s.scale))
    elseif window then
        bind_keys()
    end
end

DCS.setUserCallbacks({
    onSimulationFrame = function()
        -- The GUI is only guaranteed to be up once a mission is running, so
        -- the window is made on the first frame rather than at load.
        if not window and not create_failed then
            local ok, err = pcall(create)
            if not ok then
                create_failed = true
                logmsg("FATAL: could not create the panel: " .. tostring(err))
                window = nil
            elseif state.reopen then
                state.reopen = nil
                pcall(show)
            end
        end

        local now = os.clock()
        if window and now >= next_revive then
            next_revive = now + 2
            revive("periodic check")
        end
        if now >= next_settings then
            next_settings = now + SETTINGS_EVERY
            local ok, err = pcall(recheck_settings)
            if not ok then logmsg("reading settings failed: " .. tostring(err)) end
        end

        pump_network()

        if state.note and now >= state.note.expires then set_note(nil) end

        local ok_w, err_w = pcall(wheel_update, now)
        if not ok_w then logmsg("wheel update errored: " .. tostring(err_w)) end

        -- The wiki fetches for itself: it needs neither the key nor a
        -- confirmed server.
        if visible and state.tab == "wiki" and wiki_api.tick then
            local ok_k, err_k = pcall(wiki_api.tick, now)
            if not ok_k then logmsg("wiki update errored: " .. tostring(err_k)) end
        end

        if (visible or wheel.open) and key_ready_at and now >= key_ready_at then
            if now >= state.next_fetch then
                local t = visible and tab_def(state.tab) or nil
                state.next_fetch = now + (t and t.every or 5)
                local names = {}
                for _, name in ipairs(t and t.fetch or {}) do names[name] = true end
                -- The wheel is built from the F10 menu, and its pins and
                -- presets depend on the aircraft.
                if wheel.open then names.menu, names.status, names.cargo = true, true, true end
                for name in pairs(names) do fetch(name) end
            end
            if now >= state.next_feed then
                state.next_feed = now + FEED_EVERY
                fetch_feed()
            end
        end
    end,

    onSimulationStart = function()
        pcall(revive, "mission start")
    end,

    onMissionLoadEnd = function()
        pcall(revive, "mission loaded")
    end,

    onSimulationStop = function()
        pcall(close_wheel)
        pcall(hide)
        pcall(reset_session)
    end,

    onShowRadioMenu = function()
        if not settings.open_on_comms then return end
        local ok, err = pcall(show)
        if not ok then logmsg("onShowRadioMenu errored: " .. tostring(err)) end
    end,

    -- A different server is a different campaign, player id and key.
    onNetMissionChanged = function()
        pcall(reset_session)
        if visible then refresh_now() end
    end,

    onNetDisconnect = function()
        forget_key()
        pcall(close_wheel)
        pcall(hide)
        pcall(reset_session)
    end,
})

-- Materialise the settings file on first run so there is something to edit.
save_config()

-- For bfcockpit/tools/luatest.lua, which runs this file outside DCS with
-- stand-ins for dxgui and the DLL. Never set inside DCS.
if rawget(_G, "BFCOCKPIT_TEST") then
    BFCOCKPIT_TEST.panel = {
        state = state, cfg = cfg, settings = settings, skins = skins, wiki_api = wiki_api,
        show = function() show() end, hide = function() hide() end,
        select_tab = function(id) select_tab(id) end, render = function() render() end,
        widgets = function() return { tabs = w_tabs, back = w_back, crumb = w_crumb, pane = w_pane, window = window } end,
        rows = function() return pool, pool_actions end,
        label_em = label_em, wrap = wrap, px = px,
    }
end

logmsg("hooks registered (api " .. tostring(cfg.api) .. ")")
