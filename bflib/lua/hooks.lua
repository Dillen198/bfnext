-- Copyright 2024 Eric Stokes.

-- This file is part of bflib.

-- bflib is free software: you can redistribute it and/or modify it under
-- the terms of the GNU Affero Public License as published by the Free
-- Software Foundation, either version 3 of the License, or (at your
-- option) any later version.

-- bflib is distributed in the hope that it will be useful, but WITHOUT
-- ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
-- FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero Public License
-- for more details.

net.log("loading bflib.dll")

package.cpath = package.cpath .. ";" .. lfs.writedir() .. "\\Scripts\\?.dll"

local function file_exists(name)
    local f=io.open(name, "r")
    if f ~= nil then
        io.close(f)
        return true
    else
        return false
    end
end

-- Install the staged dll `from` as `to`. Returns nil on success, an error
-- string otherwise. `to` is only replaced once a complete copy is on disk:
-- the bytes go to a temp file, its size is checked, then it is renamed into
-- place (the old dll is kept as .bak until the rename has worked). On any
-- failure the old dll stays where it was and the caller keeps `from`, so the
-- next mission load tries again.
local function install_file(from, to)
    local fromf, fe = io.open(from, "rb")
    if fromf == nil then
        return "opening " .. from .. ": " .. tostring(fe)
    end
    local data = fromf:read("*a")
    fromf:close()
    if data == nil or #data == 0 then
        return from .. " is empty or unreadable"
    end
    local tmp = to .. ".new"
    local bak = to .. ".bak"
    os.remove(tmp)
    local tof, te = io.open(tmp, "wb")
    if tof == nil then
        return "opening " .. tmp .. ": " .. tostring(te)
    end
    local ok, we = tof:write(data)
    local closed, ce = tof:close()
    if not ok or not closed then
        os.remove(tmp)
        return "writing " .. tmp .. ": " .. tostring(we or ce)
    end
    local check = io.open(tmp, "rb")
    local size = check and check:seek("end")
    if check then check:close() end
    if size ~= #data then
        os.remove(tmp)
        return "short write to " .. tmp .. ": " .. tostring(size) .. " of " .. #data .. " bytes"
    end
    -- Windows can't rename over an existing file: move the old dll aside
    -- first, and put it back if the new one won't go in.
    os.remove(bak)
    local had_old = file_exists(to)
    if had_old then
        local moved, me = os.rename(to, bak)
        if not moved then
            os.remove(tmp)
            return "moving " .. to .. " aside: " .. tostring(me)
        end
    end
    local renamed, re = os.rename(tmp, to)
    if not renamed then
        if had_old then
            os.rename(bak, to)
        end
        os.remove(tmp)
        return "renaming " .. tmp .. " to " .. to .. ": " .. tostring(re)
    end
    os.remove(bak)
    return nil
end

local bflib_update_file = lfs.writedir() .. "\\Scripts\\_bflib.dll"
local bflib_dll = lfs.writedir() .. "\\Scripts\\bflib.dll"

if file_exists(bflib_update_file) then
    local e = install_file(bflib_update_file, bflib_dll)
    if e ~= nil then
        -- the staged file is kept; the next load retries
        net.log("could not install updated dll, keeping the staged copy: " .. e)
    else
        os.remove(bflib_update_file)
        net.log("installed updated bflib.dll")
    end
end

local bflib = require("bflib")
bflib.initHooks()
