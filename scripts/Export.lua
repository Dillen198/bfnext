--[[
  Fowl Engine – DCS Unit Position Export

  Install: place this file (or merge its contents) into:
    %USERPROFILE%\Saved Games\DCS\Scripts\Export.lua

  This script sends live unit positions to bfdb via UDP on localhost:42001.
  bfdb receives them and broadcasts to WebSocket clients for the live map.

  Data is sent as newline-terminated JSON, split into 50-unit batches to
  stay within UDP's practical payload limits.

  It also runs a client-side performance monitor (see BF_PERFMON_* below)
  that logs FPS, this Lua VM's memory, and world object count every few
  seconds to:
    %USERPROFILE%\Saved Games\DCS\Logs\bf_perfmon.csv
  Open that CSV (Excel/Sheets) after a flight to spot FPS dips or memory
  growth and correlate them to mission time. Set BF_PERFMON_ENABLE = false
  to turn it off.

  Coalition values from DCS:
    0 = Neutral / No coalition
    1 = Red
    2 = Blue

  Type.level1 (category):
    1 = Airplane
    2 = Helicopter
    3 = Ground
    4 = Ship
    5 = Structure / Static
--]]

local BF_HOST = "127.0.0.1"
local BF_PORT = 42001
local BF_INTERVAL = 0.25  -- seconds between exports

local bf_socket = nil

-- ═══════════════════════════════════════════════════════════════════════
-- Client-side performance monitor
--
-- Runs entirely inside the DCS client process (this Export.lua runs in
-- its own Lua VM, separate from the mission/server scripting engine).
-- Samples real frame rate, this VM's Lua GC memory, and world object
-- count, and appends a CSV row to Logs/bf_perfmon.csv every
-- BF_PERFMON_INTERVAL seconds. Use this to correlate FPS drops/stutters
-- and memory growth with what's happening in the mission while you fly.
--
-- Set BF_PERFMON_ENABLE = false to disable without removing the code.
-- ═══════════════════════════════════════════════════════════════════════
local BF_PERFMON_ENABLE = true
local BF_PERFMON_INTERVAL = 5       -- seconds between CSV samples
local BF_PERFMON_LEAK_WINDOW = 12   -- samples (~1 min at default interval) to judge a leak trend
local BF_PERFMON_LEAK_KB = 51200    -- warn if export-VM memory grew this much (50MB) over the window

local bf_pm_logpath = nil
local bf_pm_frame_count = 0         -- frames seen since last 1s fps tick
local bf_pm_fps_last_tick = nil     -- os.clock() of last 1s fps tick
local bf_pm_fps_samples = {}        -- fps values collected during current BF_PERFMON_INTERVAL window
local bf_pm_min_fps_window = nil
local bf_pm_mem_history = {}        -- ring buffer of recent export-VM mem_kb samples (for leak trend)

local function bf_pm_logdir()
  return (lfs and lfs.writedir and lfs.writedir() or "") .. "Logs\\"
end

local function bf_pm_init()
  local ok = pcall(function()
    bf_pm_logpath = bf_pm_logdir() .. "bf_perfmon.csv"
    local exists = false
    local f = io.open(bf_pm_logpath, "r")
    if f then exists = true; f:close() end
    if not exists then
      local w = io.open(bf_pm_logpath, "w")
      if w then
        w:write("timestamp,model_time,fps_avg,fps_min,export_mem_kb,export_mem_delta_kb,world_objects\n")
        w:close()
      end
    end
  end)
  if not ok then bf_pm_logpath = nil end
end

local function bf_pm_write_row(row)
  if not bf_pm_logpath then return end
  pcall(function()
    local f = io.open(bf_pm_logpath, "a")
    if f then
      f:write(row .. "\n")
      f:close()
    end
  end)
end

-- Called every real frame (DCS export callback) to accumulate an FPS estimate.
local function bf_pm_on_frame()
  if not BF_PERFMON_ENABLE then return end
  bf_pm_frame_count = bf_pm_frame_count + 1
  local now = os.clock()
  if not bf_pm_fps_last_tick then
    bf_pm_fps_last_tick = now
    return
  end
  local elapsed = now - bf_pm_fps_last_tick
  if elapsed >= 1.0 then
    local fps = bf_pm_frame_count / elapsed
    bf_pm_fps_samples[#bf_pm_fps_samples + 1] = fps
    if not bf_pm_min_fps_window or fps < bf_pm_min_fps_window then
      bf_pm_min_fps_window = fps
    end
    bf_pm_frame_count = 0
    bf_pm_fps_last_tick = now
  end
end

-- Called on the BF_PERFMON_INTERVAL schedule to sample memory/objects and log a row.
local function bf_pm_sample(mtime)
  if not BF_PERFMON_ENABLE then return end
  if not bf_pm_logpath then bf_pm_init() end

  local fps_avg = 0
  if #bf_pm_fps_samples > 0 then
    local sum = 0
    for _, v in ipairs(bf_pm_fps_samples) do sum = sum + v end
    fps_avg = sum / #bf_pm_fps_samples
  end
  local fps_min = bf_pm_min_fps_window or fps_avg
  bf_pm_fps_samples = {}
  bf_pm_min_fps_window = nil

  collectgarbage("collect")
  local mem_kb = collectgarbage("count")

  bf_pm_mem_history[#bf_pm_mem_history + 1] = mem_kb
  while #bf_pm_mem_history > BF_PERFMON_LEAK_WINDOW do
    table.remove(bf_pm_mem_history, 1)
  end
  local mem_delta = 0
  if #bf_pm_mem_history > 1 then
    mem_delta = mem_kb - bf_pm_mem_history[1]
  end

  local world_objects = 0
  if LoGetWorldObjects then
    local ok, objs = pcall(LoGetWorldObjects)
    if ok and objs then
      for _ in pairs(objs) do world_objects = world_objects + 1 end
    end
  end

  local ts = os.date("%Y-%m-%d %H:%M:%S")
  bf_pm_write_row(string.format(
    "%s,%.1f,%.1f,%.1f,%.0f,%.0f,%d",
    ts, mtime or 0, fps_avg, fps_min, mem_kb, mem_delta, world_objects
  ))

  if #bf_pm_mem_history >= BF_PERFMON_LEAK_WINDOW and mem_delta >= BF_PERFMON_LEAK_KB then
    local msg = string.format(
      "BF_PERFMON: possible memory growth: export VM memory grew %.0f KB over last %d samples (now %.0f KB)",
      mem_delta, BF_PERFMON_LEAK_WINDOW, mem_kb
    )
    bf_pm_write_row(string.format("%s,,,,,,%s", ts, "\"" .. msg .. "\""))
    if log then log.write("BF_PERFMON", log.WARNING, msg) end
  end
end

-- ── JSON encoder (minimal, no external dependency) ────────────────────
local function jsonVal(v)
  local t = type(v)
  if t == "nil" then return "null"
  elseif t == "boolean" then return tostring(v)
  elseif t == "number" then
    if v ~= v then return "0" end  -- NaN guard
    return string.format("%.6g", v)
  elseif t == "string" then
    return '"' .. v:gsub('\\', '\\\\'):gsub('"', '\\"'):gsub('\n', '\\n'):gsub('\r', '\\r') .. '"'
  elseif t == "table" then
    -- array check: all keys are sequential integers
    local isArr = true
    local n = 0
    for k, _ in pairs(v) do
      n = n + 1
      if type(k) ~= "number" then isArr = false; break end
    end
    if isArr and n == #v then
      local parts = {}
      for _, val in ipairs(v) do parts[#parts+1] = jsonVal(val) end
      return "[" .. table.concat(parts, ",") .. "]"
    else
      local parts = {}
      for k, val in pairs(v) do
        if type(k) == "string" or type(k) == "number" then
          parts[#parts+1] = '"' .. tostring(k) .. '":' .. jsonVal(val)
        end
      end
      return "{" .. table.concat(parts, ",") .. "}"
    end
  end
  return "null"
end

-- ── Socket helpers ─────────────────────────────────────────────────────
local function bf_connect()
  local ok, sock = pcall(function()
    local s = require("socket").udp()
    s:settimeout(0)
    s:setpeername(BF_HOST, BF_PORT)
    return s
  end)
  if ok then
    bf_socket = sock
  else
    bf_socket = nil
  end
end

local function bf_send(data)
  if not bf_socket then return end
  local ok, err = pcall(function() bf_socket:send(data .. "\n") end)
  if not ok then
    pcall(function() bf_socket:close() end)
    bf_socket = nil
  end
end

-- ── DCS export callbacks ───────────────────────────────────────────────
local bf_pm_next_sample = 0

function LuaExportStart()
  bf_connect()
  if BF_PERFMON_ENABLE then
    bf_pm_init()
    bf_pm_next_sample = 0
  end
end

function LuaExportStop()
  if bf_socket then
    pcall(function() bf_socket:close() end)
    bf_socket = nil
  end
end

-- Called by DCS every real frame; used only to accumulate the FPS estimate.
function LuaExportBeforeNextFrame()
  local ok, err = pcall(bf_pm_on_frame)
  if not ok and log then
    log.write("BF_PERFMON", log.ERROR, tostring(err))
  end
end

function LuaExportActivityNextEvent(t)
  local ok, err = pcall(doExport)
  if not ok and log then
    log.write("BF_EXPORT", log.ERROR, tostring(err))
  end

  if BF_PERFMON_ENABLE and t >= bf_pm_next_sample then
    local mtime = LoGetModelTime and LoGetModelTime() or 0
    local okPm, errPm = pcall(bf_pm_sample, mtime)
    if not okPm and log then
      log.write("BF_PERFMON", log.ERROR, tostring(errPm))
    end
    bf_pm_next_sample = t + BF_PERFMON_INTERVAL
  end

  return t + BF_INTERVAL
end

-- ── Player/pilot lookup ─────────────────────────────────────────────────
-- Export.get_player_list()/get_player_info() are dedicated-server-only
-- export APIs (distinct from the mission-scripting net.* table) that let
-- us map a unit id to the pilot occupying it.
local function buildPilotMap()
  local pilots = {}
  if not (Export and Export.get_player_list and Export.get_player_info) then
    return pilots
  end
  local ok, ids = pcall(Export.get_player_list)
  if not ok or not ids then return pilots end
  for _, pid in ipairs(ids) do
    local okSlot, slot = pcall(Export.get_player_info, pid, 'slot')
    if okSlot and slot and slot ~= '' and slot ~= '0' then
      local okName, name = pcall(Export.get_player_info, pid, 'name')
      if okName and name and name ~= '' then
        pilots[tostring(slot)] = name
      end
    end
  end
  return pilots
end

-- ── Core export logic ──────────────────────────────────────────────────
function doExport()
  if not bf_socket then bf_connect() end
  if not bf_socket then return end

  local objects = LoGetWorldObjects and LoGetWorldObjects() or {}
  local mtime = LoGetModelTime and LoGetModelTime() or 0
  local pilotMap = buildPilotMap()

  -- Collect relevant units
  local units = {}
  for id, obj in pairs(objects) do
    if obj and obj.Type and obj.LatLongAlt and obj.Coalition and obj.Coalition > 0 then
      local cat = obj.Type.level1 or 0
      -- 1=Plane, 2=Helo, 3=Ground, 4=Ship (skip 5=Static)
      if cat >= 1 and cat <= 4 then
        local vel = obj.Velocity or { x = 0, y = 0, z = 0 }
        -- Heading from velocity vector
        local hdg = 0
        if vel.x ~= 0 or vel.z ~= 0 then
          -- DCS world axes: x=north, z=east. Bearing is atan2(east, north).
          hdg = math.deg(math.atan2(vel.z, vel.x))
          if hdg < 0 then hdg = hdg + 360 end
        end
        local spd_mps = math.sqrt(vel.x*vel.x + vel.y*vel.y + vel.z*vel.z)
        local idStr = tostring(id)
        units[#units+1] = {
          id   = idStr,
          nm   = obj.UnitName or "",
          typ  = obj.Type.level3 or "",
          cat  = cat,
          coa  = obj.Coalition,
          lat  = obj.LatLongAlt.Lat,
          lon  = obj.LatLongAlt.Long,
          alt  = obj.LatLongAlt.Alt,
          hdg  = hdg,
          spd  = spd_mps * 1.94384,  -- m/s → knots
          vspd = vel.y,               -- vertical speed m/s (positive = climbing)
          pilot = pilotMap[idStr],    -- nil for AI-flown units
        }
      end
    end
  end

  -- Bullseye points from DCS coalition reference points
  local bull = {}
  if coalition and coalition.getMainRefPoint then
    -- coalition.side: RED=1, BLUE=2
    for _, side in ipairs({ 1, 2 }) do
      local ok, pt = pcall(function() return coalition.getMainRefPoint(side) end)
      if ok and pt then
        local okll, ll = pcall(function() return coord.LOtoLL(pt) end)
        if okll and ll then
          bull[#bull+1] = { side = side, lat = ll.latitude, lon = ll.longitude }
        end
      end
    end
  end

  -- Send in batches of 50 to stay within UDP limits
  local BATCH = 50
  local total = #units
  local seq = 0
  local i = 1
  while i <= total do
    local batch = {}
    for j = i, math.min(i + BATCH - 1, total) do
      batch[#batch+1] = units[j]
    end
    local isLast = (i + BATCH - 1) >= total
    local msg = jsonVal({
      t    = mtime,
      seq  = seq,
      last = isLast,
      bull = isLast and bull or nil,  -- only send bullseye in final batch
      n    = total,
      u    = batch,
    })
    bf_send(msg)
    seq = seq + 1
    i = i + BATCH
  end

  -- If no units at all, still send a heartbeat (with bullseye) so bfdb stays live
  if total == 0 then
    bf_send(jsonVal({ t = mtime, seq = 0, last = true, n = 0, u = {}, bull = bull }))
  end
end

-- ── Compatibility shim: chain existing Export.lua if present ──────────
-- (Uncomment and adjust if you already have an Export.lua)
--
-- local _existing = loadfile(lfs.writedir() .. "Scripts/ExistingExport.lua")
-- if _existing then _existing() end
