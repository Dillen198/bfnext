-- Copyright (c) 2026 Dillen Weerasinghe. All rights reserved.
-- Proprietary and confidential. No license is granted to use, copy, modify, or
-- distribute this file. See bfrange/LICENSE and the repository NOTICE file.
-- Vector Strike training range: server hook loader.
--
-- INSTALL: copy to <Saved Games>\<server>\Scripts\Hooks\bfrange_hooks.lua on
-- the RANGE server only, next to bfrange.dll in <Saved Games>\<server>\Scripts\.
-- Do NOT also install bflib's hooks.lua on that server: it gates every slot
-- change through the campaign database and would refuse every slot on a
-- mission it doesn't know.
--
-- An updated DLL dropped in as Scripts\_bfrange.dll (the bot's staging does
-- this) is swapped in here, before anything has the old one open.

net.log("loading bfrange.dll")

package.cpath = package.cpath .. ";" .. lfs.writedir() .. "\\Scripts\\?.dll"

local function file_exists(name)
    local f = io.open(name, "r")
    if f ~= nil then
        io.close(f)
        return true
    end
    return false
end

-- Read a whole file; nil, err on failure.
local function read_all(name)
    local f, e = io.open(name, "rb")
    if f == nil then
        return nil, e
    end
    local data = f:read("*a")
    f:close()
    if data == nil then
        return nil, "could not read " .. name
    end
    return data
end

-- Install `from` as `to`, never leaving `to` missing or half-written: the
-- bytes go to `to`.new first and are read back and compared; the old file
-- is moved aside to `to`.old (Windows' rename won't replace a file) and put
-- back if the swap fails. Returns nil on success, an error string otherwise.
local function install_file(from, to)
    local data, e = read_all(from)
    if data == nil then
        return tostring(e)
    end
    if #data == 0 then
        return from .. " is empty"
    end
    local tmp = to .. ".new"
    local old = to .. ".old"
    local tof, te = io.open(tmp, "wb")
    if tof == nil then
        return tostring(te)
    end
    local ok, we = tof:write(data)
    local closed, ce = tof:close()
    if not ok or not closed then
        os.remove(tmp)
        return "writing " .. tmp .. ": " .. tostring(we or ce)
    end
    local check = read_all(tmp)
    if check == nil or #check ~= #data or check ~= data then
        os.remove(tmp)
        return tmp .. " did not read back what was written"
    end
    os.remove(old)
    local had_old = file_exists(to)
    if had_old then
        local moved, me = os.rename(to, old)
        if not moved then
            os.remove(tmp)
            return "moving the old " .. to .. " aside: " .. tostring(me)
        end
    end
    local swapped, se = os.rename(tmp, to)
    if not swapped then
        if had_old then
            os.rename(old, to)
        end
        os.remove(tmp)
        return "moving " .. tmp .. " into place: " .. tostring(se)
    end
    os.remove(old)
    return nil
end

local update = lfs.writedir() .. "\\Scripts\\_bfrange.dll"
local dll = lfs.writedir() .. "\\Scripts\\bfrange.dll"

if file_exists(update) then
    local e = install_file(update, dll)
    if e ~= nil then
        -- the staged file stays, so the next start tries again
        net.log("could not install updated bfrange.dll, keeping " .. update .. ": " .. tostring(e))
    else
        os.remove(update)
        net.log("installed updated bfrange.dll")
    end
end

local bfrange = require("bfrange")
bfrange.initHooks()
