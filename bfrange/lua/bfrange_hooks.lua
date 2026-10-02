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

local function copy_file(from, to)
    local fromf, fe = io.open(from, "rb")
    if fromf == nil then
        return fe
    end
    local tof, te = io.open(to, "wb")
    if tof == nil then
        fromf:close()
        return te
    end
    tof:write(fromf:read("*a"))
    fromf:close()
    tof:close()
end

local update = lfs.writedir() .. "\\Scripts\\_bfrange.dll"
local dll = lfs.writedir() .. "\\Scripts\\bfrange.dll"

if file_exists(update) then
    local e = copy_file(update, dll)
    if e ~= nil then
        net.log("could not install updated bfrange.dll " .. tostring(e))
    else
        os.remove(update)
        net.log("installed updated bfrange.dll")
    end
end

local bfrange = require("bfrange")
bfrange.initHooks()
