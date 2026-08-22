-- BFNext cockpit overlay: opens automatically whenever this player is in a
-- running mission and shows the cockpit UI (currently: EWR) inside a real
-- in-game window, no separate application needed.
--
-- Install: copy this file to
--   %USERPROFILE%\Saved Games\DCS\Scripts\Hooks\bfcockpit.lua
-- (use "Saved Games\DCS.openbeta" instead if you run DCS Open Beta)
--
-- How it identifies you: net.get_my_player_id() returns this connection's
-- local player id (this call only works client-side; it's meaningless on
-- a dedicated server, which is why this is a Hooks script installed by
-- each player, not something bflib ships or triggers remotely -- bflib's
-- own mission-scripting state runs on the server and has no path to a
-- connecting player's screen at all). bfdb resolves that id to a ucid via
-- bflib's "resolve-player-id" RPC using the live connected-player table,
-- so there is no manual pairing step -- it just works the moment you join.
--
-- Configure BFCOCKPIT_URL below to your server's public bfdb URL.
--
-- Troubleshooting: everything below logs to
--   %USERPROFILE%\Saved Games\DCS\Logs\dcs.log
-- search that file for "BFCOCKPIT" after joining a mission. If you see
-- "loaded OK" but no window appears, the problem is in the window/CEF
-- setup below, not in loading this script -- report that line back.
-- If you see nothing at all with "BFCOCKPIT" in it, this file either isn't
-- in the right folder or DCS hasn't been restarted since it was added.

-- TEMPORARY DIAGNOSTIC: pointed at a guaranteed-reachable page to isolate
-- whether the CEF/webview mechanism itself works, independent of bfdb.
-- Revert to your real bfdb URL once this is confirmed working.
local BFCOCKPIT_URL = "https://www.google.com"

-- net is always available in the Hooks environment, so this alone should
-- never fail -- if it does, nothing below can even log, so there's no
-- point wrapping it defensively.
local net = require('net')

local function logmsg(msg)
    net.log("BFCOCKPIT: " .. msg)
end

logmsg("script loading")

local ok_dxgui, dxgui_err = pcall(require, 'dxgui')
if not ok_dxgui then
    logmsg("FATAL: require('dxgui') failed: " .. tostring(dxgui_err))
    return
end

local ok_window, Window = pcall(require, 'Window')
if not ok_window then
    logmsg("FATAL: require('Window') failed: " .. tostring(Window))
    return
end

local ok_webview, WebViewWidget = pcall(require, 'WebViewWidget')
if not ok_webview then
    logmsg("FATAL: require('WebViewWidget') failed: " .. tostring(WebViewWidget))
    return
end

local ok_static, Static = pcall(require, 'Static')
if not ok_static then
    logmsg("FATAL: require('Static') failed: " .. tostring(Static))
    return
end

logmsg("dxgui/Window/WebViewWidget all loaded OK")

local window, webview

local function close()
    if window then
        local ok, err = pcall(function() window:close() end)
        if not ok then
            logmsg("error closing window: " .. tostring(err))
        end
        window = nil
        webview = nil
    end
end

local function open()
    if window then
        return
    end

    local ok_id, playerId = pcall(net.get_my_player_id)
    if not ok_id then
        logmsg("could not get local player id, not opening overlay: " .. tostring(playerId))
        return
    end
    logmsg("opening for local player id " .. tostring(playerId))

    local ok, err = pcall(function()
        window = Window.new(20, 20, 420, 340, 'BFNext Cockpit')
        window:setDraggable(true)
        window:setResizable(true)
        window:setZOrder(1000000)
        window:setVisible(true)
        window:addCloseCallback(function()
            close()
        end)

        webview = WebViewWidget.new()
        webview:setBounds(0, 0, 420, 340)
        window:insertWidget(webview)
        -- NOTE: the real dxgui/bind/WebViewWidget.lua only exposes
        -- browserCreated(self, callback) + cefLoadUrl(self, url) as two
        -- separate methods -- CEFTest.lua's own example calls a
        -- webview:cefCallback(...) that doesn't actually exist in that
        -- binding, which silently no-ops the load (window shows, stays
        -- black). browserCreated is the real hook: it fires once the
        -- underlying CEF browser instance exists, which is the right time
        -- to call cefLoadUrl.
        webview:browserCreated(function()
            local url = string.format("%s?playerid=%d", BFCOCKPIT_URL, playerId)
            logmsg("loading " .. url)
            webview:cefLoadUrl(url)
        end)

        -- DIAGNOSTIC: a plain non-CEF widget drawn as an overlay on top of
        -- the webview, to isolate whether dxgui widget rendering itself
        -- works in a Hooks-created window (separate question from whether
        -- CEF's texture specifically composites into it). If you see this
        -- text, dxgui rendering is fine and the problem is CEF-specific.
        -- If you don't see it either, the problem is broader than CEF.
        local label = Static.new("BFCOCKPIT TEST -- if you can read this, dxgui rendering works")
        label:setBounds(4, 4, 410, 40)
        window:insertOverlayWidget(label)
    end)

    if not ok then
        logmsg("FATAL: failed to create window/webview: " .. tostring(err))
        if window then
            pcall(function() window:close() end)
        end
        window = nil
        webview = nil
        return
    end

    logmsg("window created")
end

DCS.setUserCallbacks({
    onSimulationStart = function()
        local ok, err = pcall(open)
        if not ok then
            logmsg("FATAL: onSimulationStart handler errored: " .. tostring(err))
        end
    end,
    onSimulationStop = function()
        local ok, err = pcall(close)
        if not ok then
            logmsg("error in onSimulationStop handler: " .. tostring(err))
        end
    end,
})

logmsg("hooks registered, waiting for simulation start")
