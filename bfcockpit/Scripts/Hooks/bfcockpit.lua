-- BFNext cockpit overlay -- an in-game window that replaces walking the F10
-- radio menu with a real UI, drawn on top of DCS by the sim's own GUI layer.
--
-- INSTALL: copy this file to
--   %USERPROFILE%\Saved Games\DCS\Scripts\Hooks\bfcockpit.lua
-- (use the "DCS.openbeta" folder instead if you run Open Beta). Nothing else
-- to do -- no account linking, no pairing code, no separate program.
--
-- CONFIGURE: on first run this writes a settings file next to your other DCS
-- settings:
--   %USERPROFILE%\Saved Games\DCS\Config\BFCockpit.lua
-- Edit that, not this script, so a script update never clobbers your setup.
-- Window position, size and opacity are also saved back into it whenever you
-- move, resize or dim the window, so it comes back where you left it.
--
-- DEFAULT KEYS (all rebindable in BFCockpit.lua):
--   your normal Comms/radio-menu key .... opens the overlay
--   Ctrl+Alt+C ......................... show / hide
--   Ctrl+Alt+O / Ctrl+Alt+L ............ opacity up / down
--   Ctrl+Alt+T ......................... click-through on/off
--   Ctrl+Alt+R ......................... reload the page
--   Escape ............................. hide
--
-- WHY THE COMMS KEY OPENS IT: DCS exposes no global keyboard hook to a Hooks
-- script. dxgui hotkeys (the Ctrl+Alt+... ones above) are registered on OUR
-- window, so DCS only routes a keypress to them while that window has GUI
-- focus -- fine for acting on a window you can see, useless for summoning a
-- hidden one. The one key DCS *does* hand us unconditionally is the comms
-- menu: `onShowRadioMenu` fires in this Lua state every time the player opens
-- it, whatever key they have it bound to, focus or no focus. So the overlay
-- rides that key. It is already rebindable in Options -> Controls -> UI
-- Layer, so "which key opens the cockpit UI" stays the player's choice, and
-- the key everyone already presses to reach the radio menu now reaches the
-- thing that replaces it. Set open_on_radio_menu = false to decouple them.
--
-- VR: the overlay is a normal DCS GUI window, so it renders in the headset
-- the same way the briefing and ESC menus do. Two things have to change for
-- VR to be usable, and both happen automatically when VR is detected (read
-- from your own Config/options.lua): the window is centred and sized off the
-- real screen size instead of being parked at a fixed 20,20 that can sit
-- outside the headset's comfortable view, and the page is loaded with
-- ?vr=1&scale=<n> so it renders with larger type and bigger hit targets for
-- an unsteady VR cursor. Force it either way with vr = "on" / "off".
--
-- MULTIPLE SERVERS: one bfdb can front several DCS servers, so the overlay
-- has to say which one it is looking at or a player on server #2 gets server
-- #1's picture. It sends the connected server's own name (?server=...), which
-- bfdb matches against each instance's `dcs_server_name`; set `instance` in
-- the config to pin it by id instead. Nothing to configure per server.
--
-- TROUBLESHOOTING: everything here logs to
--   %USERPROFILE%\Saved Games\DCS\Logs\dcs.log
-- Search that file for "BFCOCKPIT" after joining a mission.

-- Bump on every change to this file. It is sent to the server on the panel URL
-- (`&plugin=`), which compares it against the version it ships and puts an
-- update notice on the panel itself if this copy is behind -- the player finds
-- out where they are already looking, and nothing here has to make a network
-- call of its own. `bfdb` reads this same line out of the file it serves, so
-- the two can never disagree; keep the format exactly as it is.
local BFCOCKPIT_VERSION = "1.0.1"

local net = require('net')

local function logmsg(msg)
    net.log("BFCOCKPIT: " .. tostring(msg))
end

logmsg("script loading, version " .. BFCOCKPIT_VERSION)

-- ── DCS GUI modules ──────────────────────────────────────────────────
-- Loaded up front so a missing one fails loudly here rather than midway
-- through building a window. Only ever call methods that actually exist on
-- these bindings -- CEFTest.lua's shipped example calls a
-- webview:cefCallback() that this version of WebViewWidget.lua does not
-- have, and a missing method throws into a pcall and silently leaves you
-- with an empty window. Read dxgui/bind/*.lua, not the examples.
local function try_require(name)
    local ok, mod = pcall(require, name)
    if not ok then
        logmsg("FATAL: require('" .. name .. "') failed: " .. tostring(mod))
        return nil
    end
    return mod
end

local dxgui         = try_require('dxgui')
local Window        = try_require('Window')
local WebViewWidget = try_require('WebViewWidget')

if not (dxgui and Window and WebViewWidget) then
    logmsg("FATAL: required GUI modules missing, overlay disabled")
    return
end

logmsg("dxgui/Window/WebViewWidget all loaded OK")

-- ── Settings ─────────────────────────────────────────────────────────

local CONFIG_PATH = lfs.writedir() .. "Config\\BFCockpit.lua"

local DEFAULTS = {
    -- Where the cockpit UI is served from. This is the campaign's public API
    -- host; point it at a LAN address or http://127.0.0.1:8880/cockpit if you
    -- run bfdb yourself.
    url = "https://api.vectorstrike.org/cockpit",

    -- Pin to one bfdb instance id (e.g. "vs1") instead of letting the server
    -- name decide. Leave nil unless bfdb reports the wrong server.
    instance = nil,

    -- 0.15 (ghost) .. 1.0 (solid).
    opacity = 0.92,

    -- Open the overlay whenever the player opens the comms/radio menu. This
    -- is the only key DCS reliably reports to a Hooks script; see the header.
    open_on_radio_menu = true,

    -- Open it once automatically when the mission starts.
    open_on_start = false,

    -- Window-scoped hotkeys. These fire while the overlay has GUI focus.
    -- Format: "[ctrl+][alt+][shift+]key", key names from
    -- DCS World\dxgui\bind\KeyNames.txt. Avoid names containing a space
    -- ("page up") -- the parser splits on whitespace as well as '+'.
    -- Set any of them to false to leave that key alone.
    hotkeys = {
        toggle        = "ctrl+alt+c",
        hide          = "escape",
        opacity_up    = "ctrl+alt+o",
        opacity_down  = "ctrl+alt+l",
        click_through = "ctrl+alt+t",
        reload        = "ctrl+alt+r",
    },

    -- "auto" reads VR.enable from your Config/options.lua; "on"/"off" force it.
    vr = "auto",

    -- Extra UI magnification passed to the page. 1.0 in 2D, 1.35 in VR unless
    -- you set this explicitly.
    scale = nil,

    -- Remembered geometry. nil = pick a sensible size and centre it.
    window = { x = nil, y = nil, w = nil, h = nil },

    -- Log every state change (opacity nudges, geometry saves, URL loads).
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
            parts[#parts + 1] = inner .. "[" .. string.format("%q", tostring(k)) .. "] = "
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
        -- A missing file on first run is expected, not worth a scary log line.
        local msg = tostring(err)
        -- Lua 5.1's loadfile says "no file '<path>'" when it simply is not
        -- there yet, which is the normal first run, not a problem.
        if not (string.find(msg, "no file", 1, true)
                or string.find(msg, "No such file", 1, true)
                or string.find(msg, "cannot open", 1, true)) then
            logmsg("config load failed, using defaults: " .. msg)
        end
        return false
    end

    -- Sandbox the config file: it should only ever assign a table, and an
    -- error in it must not take the overlay down with it.
    local env = {}
    setfenv(chunk, env)
    local ok, run_err = pcall(chunk)
    if not ok then
        logmsg("config file errored, using defaults: " .. tostring(run_err))
        return false
    end

    local user = env.cockpit or env.BFCockpit or env.cfg
    if type(user) ~= 'table' then
        logmsg("config file defined no `cockpit` table, using defaults")
        return false
    end

    for k, v in pairs(user) do cfg[k] = deep_copy(v) end
    apply_defaults(cfg, DEFAULTS)
    logmsg("config loaded from " .. CONFIG_PATH)
    return true
end

local function save_config()
    local ok, err = pcall(function()
        local f, open_err = io.open(CONFIG_PATH, "w")
        if not f then error(tostring(open_err)) end
        f:write("-- BFNext cockpit overlay settings.\n")
        f:write("-- Written by Scripts/Hooks/bfcockpit.lua; safe to hand-edit.\n")
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

-- ── VR detection ─────────────────────────────────────────────────────
-- Read from the player's own options.lua rather than guessed at, so "is this
-- a headset" is answered by the same setting the sim uses.
local function detect_vr()
    if cfg.vr == "on" then return true end
    if cfg.vr == "off" then return false end

    local chunk = loadfile(lfs.writedir() .. "Config\\options.lua")
    if not chunk then return false end
    local env = {}
    setfenv(chunk, env)
    if not pcall(chunk) then return false end

    local vr = env.options and env.options.VR
    if type(vr) ~= 'table' then return false end
    -- ED has spelled this both ways across versions.
    return vr.enable == true or vr.enabled == true
end

local IS_VR = detect_vr()
local UI_SCALE = tonumber(cfg.scale) or (IS_VR and 1.35 or 1.0)
logmsg("VR " .. (IS_VR and "detected" or "not detected")
    .. " (setting: " .. tostring(cfg.vr) .. "), ui scale " .. tostring(UI_SCALE))

-- ── URL building ─────────────────────────────────────────────────────

local function urlencode(s)
    return (string.gsub(tostring(s or ""), "([^%w%-%.%_%~])", function(c)
        return string.format("%%%02X", string.byte(c))
    end))
end

-- The name the server advertises. bfdb matches this against each instance's
-- `dcs_server_name`, which is how one bfdb fronting several DCS servers hands
-- back the right campaign. Player id 1 is the server's own entry in the
-- player list (net.get_server_id() is documented as always 1).
local function server_name()
    local ok, name = pcall(function()
        return net.get_player_info(net.get_server_id(), 'name')
    end)
    if ok and type(name) == 'string' and name ~= "" then return name end
    return nil
end

local function mission_name()
    local ok, name = pcall(DCS.getMissionName)
    if ok and type(name) == 'string' and name ~= "" then return name end
    return nil
end

local function build_url(playerId)
    local base = cfg.url
    local sep = string.find(base, "?", 1, true) and "&" or "?"
    local parts = { base, sep, "playerid=", urlencode(playerId) }

    local function add(key, value)
        if value == nil then return end
        parts[#parts + 1] = "&" .. key .. "=" .. urlencode(value)
    end

    -- An explicit instance id wins; otherwise let bfdb resolve the server name.
    if cfg.instance then
        add("instance", cfg.instance)
    else
        add("server", server_name())
    end
    add("mission", mission_name())
    add("vr", IS_VR and "1" or "0")
    add("scale", string.format("%.2f", UI_SCALE))
    add("plugin", BFCOCKPIT_VERSION)

    return table.concat(parts)
end

-- ── Window ───────────────────────────────────────────────────────────

local TITLE_H = 26

local window, webview
-- Set when the page still has not loaded and we are waiting on
-- browserCreated; checked each simulation frame so a callback that never
-- fires still ends in a loaded page rather than a grey rectangle.
local load_deadline = nil
local visible = false
local click_through = false
local geometry_dirty = false

-- Forward declarations: the hotkey bindings inside create() close over these,
-- and the DCS callbacks at the bottom call them. Kept local so nothing leaks
-- into the shared GUI Lua state, where every other hook script also lives.
local show, hide, toggle

local function screen_size()
    local ok, w, h = pcall(dxgui.GetScreenSize)
    if ok and type(w) == 'number' and type(h) == 'number' and w > 0 and h > 0 then
        return w, h
    end
    return 1920, 1080
end

-- Where the window sits when there is nothing remembered. In VR a fixed
-- top-left corner is often outside the comfortable field of view, so default
-- to a large centred panel; in 2D keep it modest and out of the way.
local function default_geometry()
    local sw, sh = screen_size()
    local w, h
    if IS_VR then
        w = math.floor(math.min(sw * 0.60, 1280))
        h = math.floor(math.min(sh * 0.66, 900))
    else
        w = math.floor(math.min(sw * 0.42, 900))
        h = math.floor(math.min(sh * 0.52, 680))
    end
    return math.floor((sw - w) / 2), math.floor((sh - h) / 2), w, h
end

local function geometry()
    local dx, dy, dw, dh = default_geometry()
    local g = cfg.window or {}
    local w = tonumber(g.w) or dw
    local h = tonumber(g.h) or dh
    local x = tonumber(g.x) or dx
    local y = tonumber(g.y) or dy

    -- Clamp back on-screen: a remembered position from a different monitor
    -- setup (or a headset with a different render size) must not strand the
    -- window somewhere it cannot be reached.
    local sw, sh = screen_size()
    w = math.max(320, math.min(w, sw))
    h = math.max(240, math.min(h, sh))
    x = math.max(0, math.min(x, sw - w))
    y = math.max(0, math.min(y, sh - h))
    return x, y, w, h
end

local function window_bounds()
    if not window then return nil end
    local ok, x, y, w, h = pcall(function() return window:getBounds() end)
    if not ok or type(x) ~= 'number' or type(h) ~= 'number' then return nil end
    return x, y, w, h
end

-- Persist whatever the player does with the window so it comes back the same
-- next mission. Writing the file on every pixel of a drag would be absurd, so
-- this only marks it dirty; it is written when the overlay hides or the
-- mission ends.
local function remember_geometry()
    local x, y, w, h = window_bounds()
    if not x then return end
    cfg.window = { x = x, y = y, w = w, h = h }
    geometry_dirty = true
    if cfg.debug then
        logmsg(string.format("geometry now %d,%d %dx%d", x, y, w, h))
    end
end

local function flush_geometry()
    if not geometry_dirty then return end
    save_config()
    geometry_dirty = false
end

-- Keep the webview filling the window under the title strip whenever the user
-- drags the resize corner.
local function layout_children()
    local _, _, w, h = window_bounds()
    if not w then return end
    pcall(function()
        if webview then webview:setBounds(0, TITLE_H, w, math.max(0, h - TITLE_H)) end
    end)
end

local function set_opacity(value)
    value = math.max(0.15, math.min(1.0, value))
    cfg.opacity = value
    if window then
        pcall(function() window:setOpacity(value) end)
    end
    if cfg.debug then logmsg("opacity " .. string.format("%.2f", value)) end
    save_config()
end

local function nudge_opacity(delta)
    set_opacity((tonumber(cfg.opacity) or 0.92) + delta)
end

-- Click-through lets the overlay stay on screen as a read-only HUD while the
-- mouse keeps flying the aircraft instead of being eaten by the window.
local function set_click_through(on)
    click_through = on and true or false
    if window then
        pcall(function() window:setTransparentForUserInput(click_through) end)
        pcall(function() window:setHasCursor(not click_through) end)
    end
    -- Shown in the window's own title bar. An overlay widget of our own up
    -- there just collides with the title DCS already draws (seen live
    -- 2026-09-12: two strings printed on top of each other).
    if window then
        pcall(function()
            window:setText(click_through and "BFNext Cockpit  --  click-through"
                                          or "BFNext Cockpit")
        end)
    end
    logmsg("click-through " .. (click_through and "on" or "off"))
end

local function close()
    load_deadline = nil
    if not window then return end
    flush_geometry()
    pcall(function() window:close() end)
    window, webview = nil, nil
    visible = false
end

local function load_page(reason)
    if not webview then return end
    local ok_id, playerId = pcall(net.get_my_player_id)
    if not ok_id or playerId == nil then
        logmsg("no local player id yet, not loading page (" .. tostring(playerId) .. ")")
        return
    end
    local url = build_url(playerId)
    logmsg("loading " .. url .. " (" .. reason .. ")")
    local ok, err = pcall(function() webview:cefLoadUrl(url) end)
    if not ok then
        logmsg("FATAL: cefLoadUrl failed: " .. tostring(err))
    end
end

local function bind_hotkeys()
    local keys = cfg.hotkeys or {}
    local bindings = {
        { "toggle",        keys.toggle,        function() toggle() end },
        { "hide",          keys.hide,          function() hide() end },
        { "opacity_up",    keys.opacity_up,    function() nudge_opacity(0.05) end },
        { "opacity_down",  keys.opacity_down,  function() nudge_opacity(-0.05) end },
        { "click_through", keys.click_through, function() set_click_through(not click_through) end },
        { "reload",        keys.reload,        function() load_page("reload hotkey") end },
    }
    for _, b in ipairs(bindings) do
        local name, combo, fn = b[1], b[2], b[3]
        if type(combo) == 'string' and combo ~= "" then
            local ok, err = pcall(function() window:addHotKeyCallback(combo, fn) end)
            if ok then
                if cfg.debug then logmsg("bound " .. name .. " to " .. combo) end
            else
                logmsg("could not bind " .. name .. " (" .. combo .. "): " .. tostring(err))
            end
        end
    end
end

local function create()
    close()

    local x, y, w, h = geometry()

    local ok, err = pcall(function()
        window = Window.new(x, y, w, h, 'BFNext Cockpit')
        window:setDraggable(true)
        window:setResizable(true)
        window:setHasCursor(true)
        window:setZOrder(1000000)
        window:setOpacity(tonumber(cfg.opacity) or 0.92)
        window:addCloseCallback(function() close() end)

        pcall(function()
            window:addSizeCallback(function()
                layout_children()
                remember_geometry()
            end)
        end)
        pcall(function()
            window:addPositionCallback(function() remember_geometry() end)
        end)

        webview = WebViewWidget.new()
        webview:setBounds(0, TITLE_H, w, math.max(0, h - TITLE_H))

        -- WHEN the CEF browser actually exists has flipped between DCS
        -- builds, and both orderings have now bitten us:
        --
        --   2026-08-27: insertWidget created the browser synchronously, so
        --   browserCreated never fired unless it was registered FIRST.
        --   2026-09-12: the reverse -- browserCreated fired ~240ms AFTER
        --   insertWidget returned. A cefLoadUrl issued in between went to a
        --   browser that did not exist yet, and a one-shot guard then
        --   suppressed the real load. Window opened, stayed blank grey.
        --
        -- So: register before insertWidget (harmless either way) and treat
        -- browserCreated as THE moment to load. The call after insertWidget
        -- is only for the old ordering, and is skipped when the callback has
        -- already run. Never gate browserCreated behind a once-only flag --
        -- that is exactly what broke it.
        local browser_ready = false

        pcall(function()
            webview:browserCreated(function()
                browser_ready = true
                load_deadline = nil
                logmsg("browserCreated callback fired")
                load_page("browserCreated")
            end)
        end)

        window:insertWidget(webview)

        if browser_ready then
            logmsg("browser existed before insertWidget returned, already loaded")
        else
            -- Don't load here: on the 2026-09-12 ordering the browser does not
            -- exist yet and the call is thrown away. Give browserCreated a
            -- couple of seconds, and only step in if it never arrives.
            load_deadline = os.clock() + 2.0
        end

        window:setVisible(true)
        bind_hotkeys()
        if click_through then set_click_through(true) end
    end)

    if not ok then
        logmsg("FATAL: failed to create window: " .. tostring(err))
        if window then pcall(function() window:close() end) end
        window, webview = nil, nil
        visible = false
        return false
    end

    visible = true
    logmsg(string.format("window created at %d,%d %dx%d", x, y, w, h))
    return true
end

show = function()
    if window then
        pcall(function() window:setVisible(true) end)
        pcall(function() window:setActive(true) end)
        visible = true
        return
    end
    create()
end

hide = function()
    if not window then return end
    pcall(function() window:setVisible(false) end)
    visible = false
    flush_geometry()
end

toggle = function()
    if visible then hide() else show() end
end

-- ── DCS callbacks ────────────────────────────────────────────────────

DCS.setUserCallbacks({
    onSimulationStart = function()
        local ok, err = pcall(function()
            if cfg.open_on_start then
                show()
            else
                logmsg("ready -- press your comms/radio menu key to open, then "
                    .. tostring((cfg.hotkeys or {}).toggle) .. " to toggle")
            end
        end)
        if not ok then logmsg("onSimulationStart errored: " .. tostring(err)) end
    end,

    -- Nothing but a deadline check; this runs every frame, so it stays a
    -- single comparison in the common case.
    onSimulationFrame = function()
        if not load_deadline then return end
        if os.clock() < load_deadline then return end
        load_deadline = nil
        logmsg("browserCreated never fired, loading anyway")
        pcall(function() load_page("browserCreated timeout") end)
    end,

    onSimulationStop = function()
        local ok, err = pcall(close)
        if not ok then logmsg("onSimulationStop errored: " .. tostring(err)) end
    end,

    -- The only keypress DCS hands a Hooks script unconditionally. Riding it
    -- means the overlay opens on whatever key the player already uses for the
    -- comms menu, with no extra binding and no focus requirement.
    onShowRadioMenu = function()
        if not cfg.open_on_radio_menu then return end
        local ok, err = pcall(show)
        if not ok then logmsg("onShowRadioMenu errored: " .. tostring(err)) end
    end,

    -- A server change means a different campaign and a different player id,
    -- so the page has to be re-fetched with a freshly built URL.
    onNetMissionChanged = function()
        if not window then return end
        local ok, err = pcall(function() load_page("mission changed") end)
        if not ok then logmsg("onNetMissionChanged errored: " .. tostring(err)) end
    end,

    onNetDisconnect = function()
        local ok, err = pcall(close)
        if not ok then logmsg("onNetDisconnect errored: " .. tostring(err)) end
    end,
})

-- Materialise the settings file on first run so there is something to edit.
save_config()

logmsg("hooks registered (url " .. tostring(cfg.url) .. ")")
