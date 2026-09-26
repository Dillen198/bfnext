-- Copyright (c) 2026 Dillen Weerasinghe. All rights reserved.
-- Proprietary and confidential. No license is granted to use, copy, modify, or
-- distribute this file. See bfrange/LICENSE and the repository NOTICE file.
-- Vector Strike training range: mission loader.
--
-- Put this in the range mission as a MISSION START trigger, action DO SCRIPT
-- (paste the three lines below). The server's MissionScripting.lua must leave
-- `require`, `package` and `lfs` available (the same desanitisation bflib
-- needs).
package.cpath = package.cpath .. ";" .. lfs.writedir() .. "\\Scripts\\?.dll"
local bfrange = require("bfrange")
bfrange.initMiz()
