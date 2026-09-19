local base = _G
local utils = base.require("DCSServerBotUtils")
local config = base.require("DCSServerBotConfig")

local vectorstrike = {}

-- We can add hooks here later to capture DCS events (like onEvent)
-- and send them back to the python bot via utils.sendBotTable if needed.

Sim.setUserCallbacks(vectorstrike)
