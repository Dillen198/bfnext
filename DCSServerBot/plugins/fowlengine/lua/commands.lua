local base = _G
local dcsbot = base.dcsbot
local utils = base.require("DCSServerBotUtils")

function dcsbot.vs_get_campaign_state(json)
    log.write('DCSServerBot', log.DEBUG, 'VectorStrike: vs_get_campaign_state()')
    local msg = {}
    msg.command = 'vs_get_campaign_state'
    
    if base.vector_strike and base.vector_strike.get_campaign_state then
        msg.state = base.vector_strike.get_campaign_state()
    else
        msg.state = "{}"
    end
    
    utils.sendBotTable(msg, json.channel)
end

function dcsbot.vs_get_objectives(json)
    log.write('DCSServerBot', log.DEBUG, 'VectorStrike: vs_get_objectives()')
    local msg = {}
    msg.command = 'vs_get_objectives'
    
    if base.vector_strike and base.vector_strike.get_objectives then
        msg.objectives = base.vector_strike.get_objectives()
    else
        msg.objectives = "[]"
    end
    
    utils.sendBotTable(msg, json.channel)
end

function dcsbot.vs_join_faction(json)
    log.write('DCSServerBot', log.DEBUG, 'VectorStrike: vs_join_faction()')
    -- Call the bflib admin command for faction join
    -- In a real setup, we would verify UCID or discord mapping here
    if base.vector_strike and base.vector_strike.dispatch_admin_command then
        base.vector_strike.dispatch_admin_command(json.ucid, json.side)
    end
end

function dcsbot.vs_spawn_deployable(json)
    log.write('DCSServerBot', log.DEBUG, 'VectorStrike: vs_spawn_deployable()')
    -- Pass this logic into Vector Strike
    -- Need lua implementation inside vector_strike.spawn_deployable
end

function dcsbot.vs_set_priority(json)
    log.write('DCSServerBot', log.DEBUG, 'VectorStrike: vs_set_priority()')
    -- Pass this logic into Vector Strike
end
