-- JTAC camera spike: can DCS's own DemoSceneWidget draw a 3D "camera" inside
-- a Hooks window while a mission is running?
--
-- Not part of the plugin. Copy this one file to
--   Saved Games\DCS\Scripts\Hooks\jtaccam_spike.lua
-- start any mission (single player first, then a multiplayer server), and
-- press Ctrl+Alt+J. Delete the file again when done.
--
-- DemoSceneWidget (dxgui/bind/DemoSceneWidget.lua, its API in
-- Scripts/DemoScenes/demosceneEnvironment.lua) is what the Mission Editor's
-- loadout, static and encyclopedia previews are drawn with. Its camera has a
-- FOV, an INFRARED / NIGHTVISION video mode, and the scene a terrain mode --
-- everything a synthetic JTAC camera needs -- but ED only ever uses it in the
-- editor, and its one terrain scene (mapScene.lua) is unused. This answers:
--
--   1. Do the modules load in the GUI state during a mission?
--   2. Does the widget draw at all there, and does it cost frames?
--   3. Does a terrain mode draw the real theatre at world coordinates?
--      (FOLLOW puts the scene camera where your own view is: if the widget
--      shows the same hills as the screen behind it, world space matches.)
--   4. Do unit models (by shape name, from the editor's DB) appear?
--   5. Does the camera's IR mode work, and do zoomed FOVs hold up?
--   6. Can lines and text be drawn over it?
--
-- Every step is logged, with what it returned, to dcs.log (search
-- "JTACCAM SPIKE") and to Saved Games\DCS\Logs\jtaccam-spike.log.
--
-- The buttons:
--   BUILD     the scene: a camera on a 4 km orbit, 2.5 km up, around a point
--             3 km ahead of where you are looking, with four vehicles there
--   TERRAIN   cycles the scene's terrain mode
--   VIDEO     none / IR / NV
--   FOV       40 / 15 / 5 / 1.5 degrees
--   VIEW      ORBIT around the vehicles, or FOLLOW your own view
--   BENCH     8 s with the widget hidden, 8 s shown: frame times for each
--   ENV       sets the preview scenes' environment map (lighting)
--   CLOSE     tears the scene down and minimises (Ctrl+Alt+J reopens)

local net = require('net')
local lfs = require('lfs')

local TAG = "JTACCAM SPIKE: "
local LOG_FILE = lfs.writedir() .. [[Logs\jtaccam-spike.log]]
local SCENE_DIR = lfs.writedir() .. [[Scripts\JTACCamSpike\]]
local SCENE_FILE = SCENE_DIR .. "scene.lua"

local function logmsg(msg)
    net.log(TAG .. tostring(msg))
    local f = io.open(LOG_FILE, "a")
    if f then
        f:write(os.date("%Y-%m-%d %H:%M:%S  "), tostring(msg), "\n")
        f:close()
    end
end

logmsg("---- loaded")

local function probe(name)
    local ok, mod = pcall(require, name)
    if ok then
        logmsg("require('" .. name .. "'): ok (" .. type(mod) .. ")")
        return mod
    end
    logmsg("require('" .. name .. "'): FAILED " .. tostring(mod))
    return nil
end

-- DemoSceneWidget requires 'demosceneEnvironment', which lives in
-- Scripts/DemoScenes/: on the Mission Editor's search path, not the Hooks
-- GUI state's. Without this every run stopped at that require (93 runs,
-- 3-12 Nov 2026, jtaccam-spike.log).
if not package.path:find("DemoScenes", 1, true) then
    package.path = package.path .. ";./Scripts/DemoScenes/?.lua"
    logmsg("package.path += ./Scripts/DemoScenes/?.lua")
end

local Window = probe('Window')
local Static = probe('Static')
local Button = probe('Button')
local Skin = probe('Skin')
local DemoSceneWidget = probe('DemoSceneWidget')
local ED_demosceneAPI = probe('ED_demosceneAPI')
local OverlayWidget = probe('OverlayWidget')
local Terrain = probe('terrain')

if type(ED_demosceneAPI) == 'table' then
    local names = {}
    for k, v in pairs(ED_demosceneAPI) do names[#names + 1] = k .. (type(v) == 'function' and "()" or "") end
    table.sort(names)
    logmsg("ED_demosceneAPI has " .. #names .. ": " .. table.concat(names, " "))
end

if not (Window and Static and Button and Skin and DemoSceneWidget) then
    logmsg("RESULT: GUI or DemoSceneWidget modules missing -- nothing to test")
    return
end

-- ── Settings ─────────────────────────────────────────────────────────

local HOTKEY = "Ctrl+Alt+J"
local TARGET_AHEAD = 3000   -- m ahead of your view, along the ground
local ORBIT_RADIUS = 4000   -- m
local ORBIT_HEIGHT = 2500   -- m above the target
local ORBIT_RATE = 0.03     -- rad/s, about a lap every 3.5 minutes
local SCENE_W, SCENE_H = 640, 360
local WIN_W, WIN_H = SCENE_W + 20, SCENE_H + 100

local TERRAIN_MODES = { "finalShading", "mapSatellite", "map", "mapAlt", "mapTAD", "off" }
local VIDEO_MODES = { [0] = "none", [1] = "IR", [2] = "NV" }
local FOVS = { 40, 15, 5, 1.5 }

-- Unit type (the editor's DB key) and a shape to fall back on when the DB
-- has none. btr-80 is the shape ED's own testScene.lua uses.
local VEHICLES = {
    { type = "T-72B", shape = "t-72b", dx = 0, dz = 0 },
    { type = "BTR-80", shape = "btr-80", dx = 25, dz = 15 },
    { type = "ZSU-23-4 Shilka", shape = "zsu-23-4", dx = -30, dz = 20 },
    { type = "Ural-375", shape = "ural-375", dx = 15, dz = -35 },
}

-- ── State ────────────────────────────────────────────────────────────

local window, w_status, w_scene, w_overlay, w_hud
local buttons = {}
local open = false
local S = {
    scene = nil, cam = nil, models = {},
    target = nil, angle = 0, view = "orbit",
    terrain = 1, video = 0, fov = 1,
    scene_frames = 0,
}
local bench = nil
local last_frame_at = nil

local function now() return DCS.getRealTime() end

local function status(text)
    logmsg(text)
    if w_status then pcall(function() w_status:setText(text) end) end
end

-- Runs one step, logs it, and returns ok plus whatever it returned.
local function step(name, fn, ...)
    local res = { pcall(fn, ...) }
    if res[1] then
        local out = {}
        for i = 2, #res do out[#out + 1] = tostring(res[i]) end
        logmsg("step " .. name .. ": ok" .. (#out > 0 and (" -> " .. table.concat(out, ", ")) or ""))
    else
        logmsg("step " .. name .. ": FAILED " .. tostring(res[2]))
    end
    return unpack(res)
end

-- ── World ────────────────────────────────────────────────────────────

local function player_view()
    if type(Export) ~= 'table' or type(Export.LoGetCameraPosition) ~= 'function' then
        return nil, "Export.LoGetCameraPosition missing"
    end
    local ok, cam = pcall(Export.LoGetCameraPosition)
    if not ok or type(cam) ~= 'table' or type(cam.p) ~= 'table' or type(cam.x) ~= 'table' then
        return nil, "camera not exported here (" .. tostring(cam) .. ")"
    end
    return cam
end

local function ground(x, z)
    if Terrain and type(Terrain.GetHeight) == 'function' then
        local ok, h = pcall(Terrain.GetHeight, x, z)
        if ok and tonumber(h) then return tonumber(h) end
    end
    return nil
end

-- The editor's own unit-type -> shape lookup (me_utilities.getShape).
local function shape_of(v)
    local db = package.loaded['me_db_api']
    if not db then
        local ok, mod = pcall(require, 'me_db_api')
        db = ok and mod or nil
    end
    local def = type(db) == 'table' and type(db.unit_by_type) == 'table' and db.unit_by_type[v.type] or nil
    if type(def) ~= 'table' then return v.shape, "no DB entry, fallback" end
    local shape = def.ShapeName or def.Shape
    if type(def.visual) == 'table' and def.visual.shape and def.visual.shape ~= "" then shape = def.visual.shape end
    if type(def.shape_table_data) == 'table' and type(def.shape_table_data[1]) == 'table'
        and def.shape_table_data[1].file then
        shape = def.shape_table_data[1].file
    end
    if not shape then return v.shape, "DB entry has no shape, fallback" end
    return shape, "from DB"
end

-- ── Scene ────────────────────────────────────────────────────────────

-- Called by name from the scene each frame it draws.
function JTACCAM_SPIKE_UPDATE(t, dt)
    S.scene_frames = S.scene_frames + 1
end

local function write_scene_file()
    lfs.mkdir(SCENE_DIR)
    local f = io.open(SCENE_FILE, "w")
    if not f then return false end
    -- The editor's scenes build in loadScene; this one only says it ran, so
    -- the log shows whether loadScript reached the same Lua state.
    f:write("function loadScene(scenePtr)\n",
        "    JTACCAM_SPIKE_LOADED = tostring(scenePtr)\n",
        "end\n")
    f:close()
    return true
end

local function aim_camera()
    if not (S.cam and S.target) then return end
    if S.view == "follow" then
        local cam = player_view()
        if not cam then return end
        local p, f, u = cam.p, cam.x, cam.y
        S.cam.transform:setPosition(p.x, p.y, p.z)
        S.cam.transform:lookAtPointUp(p.x + f.x * 1000, p.y + f.y * 1000, p.z + f.z * 1000, u.x, u.y, u.z)
    else
        local t = S.target
        local x = t.x + math.cos(S.angle) * ORBIT_RADIUS
        local z = t.z + math.sin(S.angle) * ORBIT_RADIUS
        S.cam.transform:setPosition(x, t.y + ORBIT_HEIGHT, z)
        S.cam.transform:lookAtPoint(t.x, t.y, t.z)
    end
end

local function hud_text()
    return string.format("%s  FOV %s  %s  %s", VIDEO_MODES[S.video], tostring(FOVS[S.fov]),
        TERRAIN_MODES[S.terrain], S.view:upper())
end

local function teardown()
    if w_overlay then
        pcall(function() window:removeWidget(w_overlay) end)
        pcall(function() w_overlay:destroy() end)
        w_overlay = nil
    end
    if w_hud then
        pcall(function() window:removeWidget(w_hud) end)
        pcall(function() w_hud:destroy() end)
        w_hud = nil
    end
    if w_scene then
        if S.scene then
            for _, m in ipairs(S.models) do pcall(function() S.scene.remove(m) end) end
        end
        step("removeWidget", function() window:removeWidget(w_scene) end)
        step("destroy", function() w_scene:destroy() end)
        w_scene = nil
    end
    S.scene, S.cam, S.models, S.target = nil, nil, {}, nil
    bench = nil
end

local function build()
    teardown()
    local cam0, why = player_view()
    if not cam0 then return status("BUILD: no view to aim from -- " .. why) end

    -- A point on the ground TARGET_AHEAD in front of your view.
    local f = cam0.x
    local hl = math.sqrt(f.x * f.x + f.z * f.z)
    local dx, dz = 1, 0
    if hl > 1e-3 then dx, dz = f.x / hl, f.z / hl end
    local tx, tz = cam0.p.x + dx * TARGET_AHEAD, cam0.p.z + dz * TARGET_AHEAD
    local ty = ground(tx, tz)
    logmsg(string.format("your view at %.0f, %.0f, %.0f; target %.0f, %s, %.0f (terrain %s)",
        cam0.p.x, cam0.p.y, cam0.p.z, tx, tostring(ty), tz, ty and "read" or "NOT readable, using 0"))
    S.target = { x = tx, y = ty or 0, z = tz }
    -- Start the orbit behind you, so the camera looks the way you do.
    S.angle = math.atan2(-dz, -dx)

    local t0 = now()
    local ok, w = step("DemoSceneWidget.new", function() return DemoSceneWidget.new() end)
    if not ok then return status("BUILD: DemoSceneWidget.new failed -- see log") end
    w_scene = w
    if write_scene_file() then
        JTACCAM_SPIKE_LOADED = nil
        step("loadScript", function() w_scene:loadScript(SCENE_FILE) end)
        logmsg("loadScene ran in this Lua state: " .. tostring(JTACCAM_SPIKE_LOADED ~= nil)
            .. " (" .. tostring(JTACCAM_SPIKE_LOADED) .. ")")
    else
        logmsg("could not write " .. SCENE_FILE .. " -- going on without loadScript")
    end
    step("insertWidget", function() window:insertWidget(w_scene) end)
    step("setBounds", function() w_scene:setBounds(10, 60, SCENE_W, SCENE_H) end)

    local ok2, scene = step("getScene", function() return w_scene:getScene() end)
    if not ok2 or type(scene) ~= 'table' then return status("BUILD: no scene interface -- see log") end
    S.scene = scene

    step("setSky", function() scene:setSky(true) end)
    step("setTerrainMode(" .. TERRAIN_MODES[S.terrain] .. ")", function() scene:setTerrainMode(TERRAIN_MODES[S.terrain]) end)
    step("setUpdateFunc", function() scene:setUpdateFunc('JTACCAM_SPIKE_UPDATE') end)

    local ok3, cam = step("addCamera", function() return scene:addCamera(S.target.x, S.target.y + ORBIT_HEIGHT, S.target.z) end)
    if not ok3 or type(cam) ~= 'table' then return status("BUILD: addCamera failed -- see log") end
    S.cam = cam
    step("setNearClip", function() cam:setNearClip(1) end)
    step("setFarClip", function() cam:setFarClip(80000) end)
    step("setFov", function() cam:setFov(FOVS[S.fov]) end)
    step("setVideoMode", function() return cam:setVideoMode(S.video) end)
    step("aim", aim_camera)
    step("setActive", function() cam:setActive() end)
    step("getPixelDistancePer10Mils", function() return cam:getPixelDistancePer10Mils(FOVS[S.fov]) end)

    for _, v in ipairs(VEHICLES) do
        local shape, src = shape_of(v)
        local x, z = S.target.x + v.dx, S.target.z + v.dz
        local y = ground(x, z) or S.target.y
        local okm, m = step("addModel " .. v.type .. " as '" .. tostring(shape) .. "' (" .. src .. ")",
            function() return scene:addModel(shape, x, y, z) end)
        if okm and type(m) == 'table' then
            logmsg("  model " .. v.type .. " valid = " .. tostring(m.valid))
            S.models[#S.models + 1] = m
        end
    end

    -- Drawn over the scene: a crosshair, and a line of text.
    if OverlayWidget then
        local oko, o = step("OverlayWidget.new", function() return OverlayWidget.new() end)
        if oko and o then
            w_overlay = o
            step("overlay insert", function() window:insertWidget(o); o:setBounds(10, 60, SCENE_W, SCENE_H) end)
            local c = { r = 1, g = 1, b = 1, a = 0.9 }
            local cx, cy = SCENE_W / 2, SCENE_H / 2
            step("overlay lines", function()
                o:addLine(cx - 40, cy, cx - 8, cy, c)
                o:addLine(cx + 8, cy, cx + 40, cy, c)
                o:addLine(cx, cy - 40, cx, cy - 8, c)
                o:addLine(cx, cy + 8, cx, cy + 40, c)
                o:addLine(10, 10, 40, 10, c)
                o:addLine(10, 10, 10, 40, c)
                o:addLine(SCENE_W - 10, SCENE_H - 10, SCENE_W - 40, SCENE_H - 10, c)
                o:addLine(SCENE_W - 10, SCENE_H - 10, SCENE_W - 10, SCENE_H - 40, c)
            end)
        end
    end
    local okh, h = step("hud Static", function()
        local s = Static.new(hud_text())
        s:setSkin(Skin.staticSkin())
        window:insertWidget(s)
        s:setBounds(20, 66, SCENE_W - 20, 20)
        return s
    end)
    if okh then w_hud = h end

    S.scene_frames = 0
    S.built_at = now()
    status(string.format("BUILD done in %.0f ms -- %d model(s). Scene frames are counted; BENCH measures cost.",
        (now() - t0) * 1000, #S.models))
end

local function refresh_hud()
    if w_hud then pcall(function() w_hud:setText(hud_text()) end) end
end

local function cycle_terrain()
    S.terrain = S.terrain % #TERRAIN_MODES + 1
    if S.scene then step("setTerrainMode(" .. TERRAIN_MODES[S.terrain] .. ")", function() S.scene:setTerrainMode(TERRAIN_MODES[S.terrain]) end) end
    refresh_hud()
    status("terrain " .. TERRAIN_MODES[S.terrain])
end

local function cycle_video()
    S.video = (S.video + 1) % 3
    if S.cam then step("setVideoMode(" .. S.video .. ")", function() return S.cam:setVideoMode(S.video) end) end
    refresh_hud()
    status("video " .. VIDEO_MODES[S.video])
end

local function cycle_fov()
    S.fov = S.fov % #FOVS + 1
    if S.cam then
        step("setFov(" .. FOVS[S.fov] .. ")", function() S.cam:setFov(FOVS[S.fov]) end)
        step("getPixelDistancePer10Mils", function() return S.cam:getPixelDistancePer10Mils(FOVS[S.fov]) end)
    end
    refresh_hud()
    status("FOV " .. FOVS[S.fov])
end

local function toggle_view()
    S.view = (S.view == "orbit") and "follow" or "orbit"
    refresh_hud()
    status("view " .. S.view .. (S.view == "follow" and " -- the widget should match the screen behind it" or ""))
end

local function set_env()
    if S.scene then step("setEnvironmentMap(normandy03)", function() S.scene:setEnvironmentMap("normandy03") end) end
end

local function start_bench()
    if not w_scene then return status("BENCH: BUILD first") end
    pcall(function() w_scene:setVisible(false) end)
    if w_overlay then pcall(function() w_overlay:setVisible(false) end) end
    bench = { phase = "hidden", started = now(), frames = 0, scene0 = S.scene_frames }
    status("BENCH: 8 s hidden, then 8 s shown -- hold still")
end

local function bench_frame(t)
    if not bench then return end
    bench.frames = bench.frames + 1
    local dt = t - bench.started
    if dt < 8 then return end
    local ms = dt * 1000 / bench.frames
    if bench.phase == "hidden" then
        bench.hidden_ms = ms
        pcall(function() w_scene:setVisible(true) end)
        if w_overlay then pcall(function() w_overlay:setVisible(true) end) end
        bench.phase, bench.started, bench.frames, bench.scene0 = "shown", t, 0, S.scene_frames
        status(string.format("BENCH: hidden %.2f ms/frame, now measuring shown...", ms))
    else
        local scene_fps = (S.scene_frames - bench.scene0) / dt
        status(string.format("RESULT bench: hidden %.2f ms/frame (%.0f fps), shown %.2f ms/frame (%.0f fps), +%.2f ms; scene drew %.0f/s",
            bench.hidden_ms, 1000 / bench.hidden_ms, ms, 1000 / ms, ms - bench.hidden_ms, scene_fps))
        bench = nil
    end
end

-- ── Window ───────────────────────────────────────────────────────────

local function show()
    open = true
    pcall(function()
        window:setVisible(true)
        window:setSkin(Skin.windowSkin())
        window:setBounds(40, 120, WIN_W, WIN_H)
        window:setHasCursor(true)
        w_status:setVisible(true)
        for _, b in ipairs(buttons) do b:setVisible(true) end
    end)
end

-- Hidden the way bfcockpit hides: a window that is setVisible(false) loses
-- its hotkey, so it stays visible, 1x1, in the chat-minimised skin.
local function hide()
    teardown()
    open = false
    pcall(function()
        w_status:setVisible(false)
        for _, b in ipairs(buttons) do b:setVisible(false) end
        window:setSkin(Skin.windowSkinChatMin())
        window:setHasCursor(false)
        window:setBounds(40, 120, 1, 1)
    end)
end

local function create()
    window = Window.new(40, 120, WIN_W, WIN_H, "JTAC camera spike -- " .. HOTKEY)
    window:setSkin(Skin.windowSkin())
    window:setZOrder(113)
    local defs = {
        { "BUILD", build }, { "TERRAIN", cycle_terrain }, { "VIDEO", cycle_video }, { "FOV", cycle_fov },
        { "VIEW", toggle_view }, { "BENCH", start_bench }, { "ENV", set_env }, { "CLOSE", hide },
    }
    local bw = math.floor(SCENE_W / #defs)
    for i, d in ipairs(defs) do
        local b = Button.new(d[1])
        b:setSkin(Skin.buttonSkin())
        b:setBounds(10 + (i - 1) * bw, 4, bw - 4, 26)
        local fn, label = d[2], d[1]
        b:addMouseUpCallback(function()
            local ok, err = pcall(fn)
            if not ok then status(label .. " errored: " .. tostring(err)) end
        end)
        window:insertWidget(b)
        buttons[#buttons + 1] = b
    end
    w_status = Static.new("BUILD makes the scene. Log: Saved Games\\DCS\\Logs\\jtaccam-spike.log")
    w_status:setSkin(Skin.staticSkin())
    w_status:setBounds(10, 34, SCENE_W, 22)
    window:insertWidget(w_status)
    window:setVisible(true)
    local ok, err = pcall(function() window:addHotKeyCallback(HOTKEY, function()
        if open then hide() else show() end
    end) end)
    logmsg("window made; hotkey " .. HOTKEY .. (ok and " bound" or (" NOT bound: " .. tostring(err))))
    if type(Export) == 'table' and type(Export.LoIsOwnshipExportAllowed) == 'function' then
        logmsg("ownship export allowed (camera position readable): " .. tostring(Export.LoIsOwnshipExportAllowed()))
    end
    hide()
end

local create_failed = false

DCS.setUserCallbacks({
    onSimulationFrame = function()
        if not window and not create_failed then
            local ok, err = pcall(create)
            if not ok then
                create_failed = true
                logmsg("could not create the window: " .. tostring(err))
            end
        end
        local t = now()
        if S.cam then
            if S.view == "orbit" and last_frame_at then S.angle = S.angle + ORBIT_RATE * (t - last_frame_at) end
            local ok, err = pcall(aim_camera)
            if not ok then
                logmsg("aim failed, camera stopped: " .. tostring(err))
                S.cam = nil
            end
        end
        last_frame_at = t
        bench_frame(t)
    end,

    onSimulationStart = function()
        logmsg("mission started (multiplayer: " .. tostring(DCS.isMultiplayer and DCS.isMultiplayer()) .. ")")
        if window then pcall(function() window:setVisible(true) end) end
    end,

    onSimulationStop = function()
        pcall(hide)
    end,
})

logmsg("hooks registered -- start a mission and press " .. HOTKEY)
