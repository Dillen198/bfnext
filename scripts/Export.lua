--[[
  Fowl Engine – DCS Unit Position Export

  Install: place this file (or merge its contents) into:
    %USERPROFILE%\Saved Games\DCS\Scripts\Export.lua

  This script sends live unit positions to bfdb via UDP on localhost:42001.
  bfdb receives them and broadcasts to WebSocket clients for the live map.

  Data is sent as newline-terminated JSON, split into 50-unit batches to
  stay within UDP's practical payload limits.

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
local BF_INTERVAL = 1.0  -- seconds between exports

local bf_socket = nil

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
function LuaExportStart()
  bf_connect()
end

function LuaExportStop()
  if bf_socket then
    pcall(function() bf_socket:close() end)
    bf_socket = nil
  end
end

function LuaExportActivityNextEvent(t)
  local ok, err = pcall(doExport)
  if not ok and log then
    log.write("BF_EXPORT", log.ERROR, tostring(err))
  end
  return t + BF_INTERVAL
end

-- ── Core export logic ──────────────────────────────────────────────────
function doExport()
  if not bf_socket then bf_connect() end
  if not bf_socket then return end

  local objects = LoGetWorldObjects and LoGetWorldObjects() or {}
  local mtime = LoGetModelTime and LoGetModelTime() or 0

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
          hdg = math.deg(math.atan2(vel.x, vel.z))
          if hdg < 0 then hdg = hdg + 360 end
        end
        local spd_mps = math.sqrt(vel.x*vel.x + vel.y*vel.y + vel.z*vel.z)
        units[#units+1] = {
          id   = tostring(id),
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
