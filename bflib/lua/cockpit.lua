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
-- search that file for "BFCOCKPIT" after joining a mission.
--
-- STATUS as of the last live test (2026-08-27): FIXED and CONFIRMED --
-- registering webview:browserCreated(fn) *before* window:insertWidget(webview)
-- (instead of after) fixed a subscribe-after-fire race: insertWidget was
-- creating the underlying CEF browser synchronously, so registering the
-- callback afterward was already too late to catch that event. dcs.log
-- confirmed "browserCreated callback fired" now logs, and the local
-- file:// test page rendered correctly in-game (readable green text on a
-- dark background) -- CEF itself and the whole widget/window/callback
-- chain all work. The eager cefLoadUrl-immediately-after-insertWidget
-- hedge turned out to be unnecessary (its try_load call was skipped,
-- browserCreated's own path did the loading) but is left in as a harmless
-- safety net.
--
-- This version moves on to the next open question: does DCS's embedded
-- CEF have real network access, or only local file:// access? Tests
-- https://www.google.com instead of the local test page. If this renders,
-- point BFCOCKPIT_URL (search for it below) at your real bfdb server and
-- you're done. If it stays blank while the local test still works, network
-- access is restricted and the cockpit page needs to be served/mirrored
-- locally instead.

-- Set this to your bfdb server's cockpit URL once network-access is
-- confirmed working (see the STATUS note above), e.g.
-- "http://127.0.0.1:8880/cockpit" or "http://192.168.1.50:8880/cockpit" or
-- "https://your-domain.example/cockpit". Left nil for now -- this
-- diagnostic build tests https://www.google.com instead, to isolate
-- whether DCS's embedded CEF has network access at all before wiring up
-- the real target.
local BFCOCKPIT_URL = nil

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

-- Write a trivial local test page and return a file:// URL for it, or nil
-- (logging why) if that isn't possible.
local function write_local_test_page()
    local ok, result = pcall(function()
        local dir = lfs.writedir() .. "Temp\\"
        pcall(lfs.mkdir, dir)
        local path = dir .. "bfcockpit_test.html"
        local f, err = io.open(path, "w")
        if not f then
            error("could not open " .. path .. " for write: " .. tostring(err))
        end
        f:write([[<html><body style="background:#123;color:#7f7;font-family:sans-serif;font-size:22px;padding:2rem;">
LOCAL FILE TEST OK<br/>if you can read this, CEF works and the issue is network access
</body></html>]])
        f:close()
        return "file:///" .. path:gsub("\\", "/")
    end)
    if not ok then
        logmsg("could not write local test page: " .. tostring(result))
        return nil
    end
    return result
end

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
    -- Force-close any leftover window from a previous script load before
    -- creating a new one, instead of leaking a duplicate.
    close()

    local ok_id, playerId = pcall(net.get_my_player_id)
    if not ok_id then
        logmsg("could not get local player id, not opening overlay: " .. tostring(playerId))
        return
    end
    logmsg("opening for local player id " .. tostring(playerId))

    local test_url = write_local_test_page()
    if test_url then
        logmsg("local test page written: " .. test_url)
    end

    local ok, err = pcall(function()
        window = Window.new(20, 20, 420, 340, 'BFNext Cockpit')
        window:setDraggable(true)
        window:setResizable(true)
        window:setZOrder(1000000)
        window:setVisible(true)
        window:addCloseCallback(function()
            close()
        end)

        -- Title bar label, drawn first, above where the webview starts, so
        -- it can't overlap the window chrome the way the last test's
        -- diagnostic label (positioned at y=4) did.
        local label = Static.new("BFCOCKPIT")
        label:setBounds(4, 26, 410, 18)
        window:insertOverlayWidget(label)

        webview = WebViewWidget.new()
        webview:setBounds(0, 48, 420, 292)

        -- Local file:// rendering is now confirmed working (see header) --
        -- this round tests real network access instead, so ignore test_url
        -- even though it was still written above (harmless). Once
        -- BFCOCKPIT_URL is set, this always prefers the real target over
        -- the network-access test.
        local url = BFCOCKPIT_URL or "https://www.google.com"
        local load_attempted = false
        local function try_load(from)
            if load_attempted then
                logmsg("try_load(" .. from .. ") skipped, already loaded")
                return
            end
            load_attempted = true
            logmsg("loading " .. url .. " (triggered by " .. from .. ")")
            local ok_load, load_err = pcall(function() webview:cefLoadUrl(url) end)
            if not ok_load then
                logmsg("FATAL: cefLoadUrl failed: " .. tostring(load_err))
                load_attempted = false -- allow the other trigger to retry
            end
        end

        -- NOTE: the real dxgui/bind/WebViewWidget.lua only exposes
        -- browserCreated(self, callback) + cefLoadUrl(self, url) as two
        -- separate methods -- CEFTest.lua's own example calls a
        -- webview:cefCallback(...) that doesn't actually exist in that
        -- binding, which silently no-ops the load. browserCreated fires
        -- once the underlying CEF browser instance exists -- but only if
        -- registered *before* window:insertWidget(webview) below, since
        -- insertWidget creates the browser synchronously (confirmed live
        -- 2026-08-27: registering after insertWidget meant the callback
        -- never fired at all). The eager cefLoadUrl call right after
        -- insertWidget is a harmless belt-and-suspenders fallback, kept in
        -- case a future DCS build changes this timing again.
        local ok_bc, bc_err = pcall(function()
            webview:browserCreated(function()
                logmsg("browserCreated callback fired")
                try_load("browserCreated callback")
            end)
        end)
        if not ok_bc then
            logmsg("browserCreated registration failed: " .. tostring(bc_err))
        end

        window:insertWidget(webview)
        try_load("immediate fallback after insertWidget")
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
