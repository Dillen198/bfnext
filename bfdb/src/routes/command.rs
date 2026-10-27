//! Ground war, HQ and commander routes.

use crate::*;

/// GET /api/groundwar — the ground war as the caller's coalition sees it:
/// its own formations in full, enemy formations only where its forces are in
/// contact, the battles being fought, and the objectives to order them at.
///
/// Coalition-locked like `/api/situation`: the session resolves to a side and
/// the engine builds the picture for that side alone. `can_command` says
/// whether this viewer may give orders -- a pilot registered on a side; an
/// admin looking in with `?side=` sees but can't command.
pub(crate) async fn api_groundwar(
    session_id: Option<Uuid>,
    db: StatsDb,
    bot_cfg: Arc<Option<BotLinkConfig>>,
    query: std::collections::HashMap<std::string::String, std::string::String>,
    inst: Inst,
) -> std::result::Result<impl warp::Reply, Error> {
    use netidx::publisher::Value;
    let c = require_coalition(&query, session_id, &db, &bot_cfg, &inst).await?;
    let side_str = match c.side {
        dcso3::coalition::Side::Red => "red",
        _ => "blue",
    };
    let raw = call_engine_rpc_str_optional(
        &db,
        &inst,
        "query-ground-war",
        vec![("side", Value::from(side_str.to_string()))],
    )
    .await?;
    let mut v: serde_json::Value =
        serde_json::from_str(&raw).map_err(anyhow::Error::from)?;
    let can = c.commands(&db, &inst);
    let status = c.command_status(&db, &inst);
    if let Some(o) = v.as_object_mut() {
        o.insert("can_command".into(), serde_json::json!(can));
        o.insert("god_mode".into(), serde_json::json!(c.god_mode));
        o.insert("commander".into(), status);
    }
    Ok(json_response(v.to_string()))
}

/// POST /api/groundwar/command — order one of the caller's side's ground
/// formations, or raise a new one. The body is a `GroundCommand`. The
/// engine resolves the caller's side itself, from the ucid their Discord
/// login is linked to, and refuses an order for a formation that isn't
/// theirs -- nothing in the body can reach the other side's forces.
pub(crate) async fn api_groundwar_command(
    session_id: Option<Uuid>,
    query: std::collections::HashMap<std::string::String, std::string::String>,
    body: bfprotocols::groundwar::GroundCommand,
    db: StatsDb,
    bot_cfg: Arc<Option<BotLinkConfig>>,
    inst: Inst,
) -> std::result::Result<impl warp::Reply, Error> {
    use netidx::publisher::Value;
    let c = require_coalition(&query, session_id, &db, &bot_cfg, &inst).await?;
    if c.god_mode {
        return Err(websec::forbidden(
            "commanding ground forces needs a pilot registered on a side this campaign",
        )
        .into());
    }
    let Some(ucid) = c.ucid() else {
        return Err(websec::forbidden(
            "account not linked -- type -linkme <token> in DCS chat (get the token with /linkme in Discord)",
        )
        .into());
    };
    if !c.commands(&db, &inst) {
        let status =
            task::block_in_place(|| command::status_of(&db, &inst, ucid)).ok().flatten();
        return Err(websec::forbidden(command::refusal(&inst, status.as_ref())).into());
    }
    let cmd = serde_json::to_string(&body).map_err(anyhow::Error::from)?;
    let raw = call_engine_rpc_str_patient(
        &db,
        &inst,
        "ground-command",
        vec![("ucid", Value::from(ucid.to_string())), ("cmd", Value::from(cmd))],
    )
    .await?;
    Ok(json_response(raw))
}

/// GET /api/hq — the caller's coalition's theatre HQ: its posture, main
/// effort and intent, the operations it is running, the support requests
/// waiting, its record, and the fog-of-war picture it plans from.
///
/// Coalition-locked like `/api/groundwar`. `can_command` comes from the
/// engine (the HQ's `override_rule`, or a server admin); a dashboard admin
/// may always command. `can_request` says whether this viewer can ask for
/// support (a pilot registered on the side).
pub(crate) async fn api_hq(
    session_id: Option<Uuid>,
    db: StatsDb,
    bot_cfg: Arc<Option<BotLinkConfig>>,
    query: std::collections::HashMap<std::string::String, std::string::String>,
    inst: Inst,
) -> std::result::Result<impl warp::Reply, Error> {
    use netidx::publisher::Value;
    let c = require_coalition(&query, session_id, &db, &bot_cfg, &inst).await?;
    let side_str = match c.side {
        dcso3::coalition::Side::Red => "red",
        _ => "blue",
    };
    let ucid = c.ucid().map(|u| u.to_string()).unwrap_or_default();
    let raw = call_engine_rpc_str_optional(
        &db,
        &inst,
        "query-hq",
        vec![("side", Value::from(side_str.to_string())), ("ucid", Value::from(ucid))],
    )
    .await?;
    let mut v: serde_json::Value =
        serde_json::from_str(&raw).map_err(anyhow::Error::from)?;
    if let Some(o) = v.as_object_mut() {
        let engine_says = o.get("can_command").and_then(|b| b.as_bool()).unwrap_or(false);
        let commander = !c.god_mode
            && c.ucid().map_or(false, |u| {
                task::block_in_place(|| command::is_commander(&db, &inst, u, c.side))
                    .unwrap_or(false)
            });
        if c.session.is_admin {
            if let Some(u) = c.ucid() {
                command::note_admin(*u);
            }
        }
        o.insert(
            "can_command".into(),
            serde_json::json!(c.session.is_admin || engine_says || commander),
        );
        o.insert(
            "can_request".into(),
            serde_json::json!(!c.god_mode && c.ucid().is_some()),
        );
        o.insert("god_mode".into(), serde_json::json!(c.god_mode));
    }
    Ok(json_response(v.to_string()))
}

/// POST /api/hq/command — a support request, or (for a commander) orders to
/// the caller's coalition's HQ. The body is an `HqCommand`. Requests go as
/// the caller's linked pilot; orders from a dashboard admin go with the
/// admin's authority (an empty ucid), everyone else's as their pilot, and the
/// engine decides whether that pilot may give them.
pub(crate) async fn api_hq_command(
    session_id: Option<Uuid>,
    query: std::collections::HashMap<std::string::String, std::string::String>,
    body: bfprotocols::hq::HqCommand,
    db: StatsDb,
    bot_cfg: Arc<Option<BotLinkConfig>>,
    inst: Inst,
) -> std::result::Result<impl warp::Reply, Error> {
    use bfprotocols::hq::HqCommand;
    use netidx::publisher::Value;
    let c = require_coalition(&query, session_id, &db, &bot_cfg, &inst).await?;
    let is_request =
        matches!(body, HqCommand::Request { .. } | HqCommand::CancelRequest { .. });
    let ucid = if is_request {
        if c.god_mode {
            return Err(websec::forbidden(
                "asking for support needs a pilot registered on a side this campaign",
            )
            .into());
        }
        match c.ucid() {
            Some(u) => u.to_string(),
            None => {
                return Err(websec::forbidden(
                    "account not linked -- type -linkme <token> in DCS chat (get the token with /linkme in Discord)",
                )
                .into())
            }
        }
    } else if c.session.is_admin {
        String::new()
    } else {
        match c.ucid() {
            Some(u) => u.to_string(),
            None => return Err(websec::forbidden("account not linked").into()),
        }
    };
    let side_str = match c.side {
        dcso3::coalition::Side::Red => "red",
        _ => "blue",
    };
    let cmd = serde_json::to_string(&body).map_err(anyhow::Error::from)?;
    let raw = call_engine_rpc_str_patient(
        &db,
        &inst,
        "hq-command",
        vec![
            ("side", Value::from(side_str.to_string())),
            ("ucid", Value::from(ucid)),
            ("cmd", Value::from(cmd)),
        ],
    )
    .await?;
    Ok(json_response(raw))
}

/// One engine call per side per this long, however many commanders are
/// watching: the picture reads every own unit's position out of DCS.
pub(crate) const COMMAND_PICTURE_TTL: std::time::Duration =
    std::time::Duration::from_millis(3000);

pub(crate) static COMMAND_CACHE: std::sync::LazyLock<
    websec::CacheMap<std::string::String, Arc<std::string::String>>,
> = std::sync::LazyLock::new(websec::CacheMap::new);

/// The last picture the engine did send, per instance and side, served when
/// a refresh fails -- for this long, after which the error shows instead.
pub(crate) static COMMAND_LAST_GOOD: std::sync::LazyLock<
    std::sync::Mutex<
        std::collections::HashMap<
            std::string::String,
            (std::time::Instant, Arc<std::string::String>),
        >,
    >,
> = std::sync::LazyLock::new(Default::default);
pub(crate) const COMMAND_STALE_MAX: std::time::Duration =
    std::time::Duration::from_secs(120);

/// GET /api/command — the command map's picture of the caller's side: its
/// own AI flights, convoys, deployed units, troops, batteries and carrier
/// groups, where DCS has them now, the treasury, and what can be launched.
/// Only ever the caller's own side's assets; the enemy comes from the
/// fog-of-war feeds (`/ws/tacmap`, `/ws/groundwar`). Coalition-locked like
/// those; an admin with no side looks at `?side=`.
/// GET /api/command/log -- the caller's side's combat log for this round,
/// newest first, `limit` (default 200, at most 500) lines from before
/// `before` (unix nanos, from the last page). Kept for the whole round, so
/// it survives restarts and is only cleared by a campaign reset.
/// Coalition-locked: a side reads its own log only.
pub(crate) async fn api_command_log(
    session_id: Option<Uuid>,
    query: std::collections::HashMap<std::string::String, std::string::String>,
    db: StatsDb,
    bot_cfg: Arc<Option<BotLinkConfig>>,
    inst: Inst,
) -> std::result::Result<impl warp::Reply, Error> {
    let c = require_coalition(&query, session_id, &db, &bot_cfg, &inst).await?;
    let side = match c.side {
        dcso3::coalition::Side::Red => "Red",
        _ => "Blue",
    };
    let before = query.get("before").and_then(|b| b.parse::<i64>().ok());
    let limit = query
        .get("limit")
        .and_then(|l| l.parse::<usize>().ok())
        .unwrap_or(200)
        .clamp(1, 500);
    let lines = task::block_in_place(|| -> anyhow::Result<_> {
        let Some(round) = db.active_round_id(&inst.id)? else { return Ok(vec![]) };
        db.combat_log(round, side, before, limit)
    })?;
    let out: Vec<serde_json::Value> = lines
        .into_iter()
        .map(|(ns, l)| {
            serde_json::json!({
                "ns": ns, "at": l.at, "kind": l.kind, "text": l.text,
                "pos": l.pos, "formation": l.formation,
            })
        })
        .collect();
    Ok(json_response(serde_json::json!({ "lines": out }).to_string()))
}

pub(crate) async fn api_command(
    session_id: Option<Uuid>,
    query: std::collections::HashMap<std::string::String, std::string::String>,
    db: StatsDb,
    bot_cfg: Arc<Option<BotLinkConfig>>,
    inst: Inst,
) -> std::result::Result<impl warp::Reply, Error> {
    use netidx::publisher::Value;
    let c = require_coalition(&query, session_id, &db, &bot_cfg, &inst).await?;
    let side_str = match c.side {
        dcso3::coalition::Side::Red => "red",
        _ => "blue",
    };
    let key = format!("{}:{side_str}", inst.id);
    let fresh = COMMAND_CACHE
        .entry(&key)
        .get_or_refresh(COMMAND_PICTURE_TTL, || async {
            call_engine_rpc_str_patient(
                &db,
                &inst,
                "query-command",
                vec![("side", Value::from(side_str.to_string()))],
            )
            .await
            .map(Arc::new)
        })
        .await;
    // A busy engine misses a refresh now and then; the map keeps the last
    // picture it had, says how old it is, and goes on asking, rather than
    // blanking out for a commander mid-order.
    let (raw, stale) = {
        let mut last = COMMAND_LAST_GOOD.lock().unwrap_or_else(|e| e.into_inner());
        match fresh {
            Ok(raw) => {
                last.insert(key, (std::time::Instant::now(), raw.clone()));
                (raw, None)
            }
            Err(e) => match last.get(&key) {
                Some((at, raw)) if at.elapsed() < COMMAND_STALE_MAX => {
                    (raw.clone(), Some(at.elapsed().as_secs()))
                }
                _ => return Err(e),
            },
        }
    };
    let mut v: serde_json::Value =
        serde_json::from_str(&raw).map_err(anyhow::Error::from)?;
    let can = (c.god_mode && c.session.is_admin) || c.commands(&db, &inst);
    let status = c.command_status(&db, &inst);
    if let Some(o) = v.as_object_mut() {
        o.insert("can_command".into(), serde_json::json!(can));
        o.insert("god_mode".into(), serde_json::json!(c.god_mode));
        o.insert("commander".into(), status);
        if let Some(secs) = stale {
            o.insert("stale_secs".into(), serde_json::json!(secs));
        }
    }
    Ok(json_response(v.to_string()))
}

/// POST /api/command/order — a commander's order from the command map. The
/// body is a `CommandOrder`. The side is the caller's own (an admin with no
/// side gives `?side=` and orders with admin authority); bfdb checks they
/// command, and the engine checks the asset is that side's, that the
/// treasury can pay, and the rest.
pub(crate) async fn api_command_order(
    session_id: Option<Uuid>,
    query: std::collections::HashMap<std::string::String, std::string::String>,
    body: bfprotocols::command::CommandOrder,
    db: StatsDb,
    bot_cfg: Arc<Option<BotLinkConfig>>,
    inst: Inst,
) -> std::result::Result<impl warp::Reply, Error> {
    use netidx::publisher::Value;
    let c = require_coalition(&query, session_id, &db, &bot_cfg, &inst).await?;
    let ucid = if c.god_mode {
        if !c.session.is_admin {
            return Err(websec::forbidden(
                "commanding needs a pilot registered on a side",
            )
            .into());
        }
        std::string::String::new()
    } else {
        let Some(u) = c.ucid() else {
            return Err(websec::forbidden(
                "account not linked -- type -linkme <token> in DCS chat (get the token with /linkme in Discord)",
            )
            .into());
        };
        if !c.commands(&db, &inst) {
            let status =
                task::block_in_place(|| command::status_of(&db, &inst, u)).ok().flatten();
            return Err(
                websec::forbidden(command::refusal(&inst, status.as_ref())).into()
            );
        }
        u.to_string()
    };
    let side_str = match c.side {
        dcso3::coalition::Side::Red => "red",
        _ => "blue",
    };
    let order = serde_json::to_string(&body).map_err(anyhow::Error::from)?;
    log::info!(
        "[{}] command order from {} ({side_str}): {order}",
        inst.id,
        c.session.username
    );
    let raw = call_engine_rpc_str_patient(
        &db,
        &inst,
        "command-order",
        vec![
            ("side", Value::from(side_str.to_string())),
            ("ucid", Value::from(ucid)),
            ("order", Value::from(order)),
        ],
    )
    .await?;
    Ok(json_response(raw))
}

/// GET /api/command/me — the caller's commander standing on this server:
/// rank, score, what unlocks command and whether they have it. `can_command`
/// is what the order endpoints will check.
pub(crate) async fn api_command_me(
    session_id: Option<Uuid>,
    query: std::collections::HashMap<std::string::String, std::string::String>,
    db: StatsDb,
    bot_cfg: Arc<Option<BotLinkConfig>>,
    inst: Inst,
) -> std::result::Result<impl warp::Reply, Error> {
    let c = require_coalition(&query, session_id, &db, &bot_cfg, &inst).await?;
    let cfg = command::command_cfg(&inst);
    Ok(json_response(
        serde_json::json!({
            "can_command": c.commands(&db, &inst),
            "is_admin": c.session.is_admin,
            "god_mode": c.god_mode,
            "side": format!("{:?}", c.side),
            "require_commander": cfg.require_commander,
            "commander_rank": cfg.commander_rank,
            "commander_score": bfprotocols::cfg::rank_min_score(cfg.commander_rank),
            "status": c.command_status(&db, &inst),
        })
        .to_string(),
    ))
}

/// GET /api/admin/commanders — every pilot's commander standing on this
/// server, highest score first, as `{instance, commander_rank,
/// commander_score, require_commander, pilots: [CommanderStatus]}`. The
/// Discord bot reads this to keep the Commander role in step.
pub(crate) async fn api_admin_commanders(
    session_id: Option<Uuid>,
    db: StatsDb,
    inst: Inst,
) -> std::result::Result<impl warp::Reply, Error> {
    require_role(session_id, db.clone(), admin::Role::MODERATION).await?;
    let cfg = command::command_cfg(&inst);
    let roster = task::block_in_place(|| command::roster(&db, &inst))?;
    Ok(json_response(
        serde_json::json!({
            "instance": inst.id.to_string(),
            "require_commander": cfg.require_commander,
            "commander_rank": cfg.commander_rank,
            "commander_score": bfprotocols::cfg::rank_min_score(cfg.commander_rank),
            "pilots": *roster,
        })
        .to_string(),
    ))
}

#[derive(Debug, serde::Deserialize)]
pub(crate) struct CommanderGrantBody {
    pub(crate) ucid: std::string::String,
    /// "granted", "revoked", or null to go back to what their rank says.
    pub(crate) grant: Option<bfprotocols::command::CommanderGrant>,
}

/// POST /api/admin/commanders — grant or withdraw one pilot's commander
/// access whatever their rank, or clear the override. Applies on every
/// server; each engine hears about it straight away.
pub(crate) async fn api_admin_commander_grant(
    session_id: Option<Uuid>,
    body: CommanderGrantBody,
    db: StatsDb,
    inst: Inst,
) -> std::result::Result<impl warp::Reply, Error> {
    let session = require_role(session_id, db.clone(), admin::Role::MODERATION).await?;
    let ucid: dcso3::net::Ucid = match body.ucid.trim().parse() {
        Ok(u) => u,
        Err(_) => return Err(websec::bad_request("not a ucid").into()),
    };
    let rec = body.grant.map(|grant| command::GrantRecord {
        grant,
        by: session.username.clone(),
        at: chrono::Utc::now(),
    });
    task::block_in_place(|| db.set_commander_grant(&ucid, rec.as_ref()))?;
    command::invalidate();
    audit(
        &db,
        &session,
        "commander access",
        Some(&format!("{ucid} -> {:?}", body.grant)),
        Some(&inst.id),
    );
    let ids: Vec<InstanceId> =
        db.instances().all().iter().map(|c| Arc::from(c.id.as_str())).collect();
    for id in ids {
        let st = db.state(&id);
        command::push(&db, &st).await;
    }
    let status = task::block_in_place(|| command::status_of(&db, &inst, &ucid))?;
    Ok(json_response(serde_json::json!({ "ok": true, "status": status }).to_string()))
}

#[derive(serde::Deserialize)]
pub(crate) struct SpawnBody {
    pub(crate) airbase: std::string::String,
    #[serde(rename = "type")]
    pub(crate) item_type: std::string::String,
}

/// POST /api/commander/spawn  — spawn logistics from dashboard.
/// Resolves the airbase name to its live DCS position + owning side via
/// bflib's query-objective RPC, then calls its spawn-deployable RPC.
pub(crate) async fn api_commander_spawn(
    session_id: Option<Uuid>,
    body: SpawnBody,
    db: StatsDb,
    inst: Inst,
) -> std::result::Result<impl warp::Reply, Error> {
    let who = require_admin(session_id, db.clone()).await?;
    audit(
        &db,
        &who,
        "spawn",
        Some(&format!("{} at {}", body.item_type, body.airbase)),
        Some(&inst.id),
    );

    use netidx::publisher::Value;

    let details_json = call_engine_rpc_str(
        &db,
        &inst,
        "query-objective",
        vec![("name", Value::from(body.airbase.clone()))],
    )
    .await?;
    let details: bfprotocols::api::ObjectiveDetails = serde_json::from_str(&details_json)
        .map_err(|e| Error(anyhow::anyhow!("bad objective details from engine: {e}")))?;
    let (x, z) = details.info.pos;
    let side = details.info.owner;

    let spawn_json = call_engine_rpc_str(
        &db,
        &inst,
        "spawn-deployable",
        vec![
            ("side", Value::from(side.to_str())),
            ("name", Value::from(body.item_type.clone())),
            ("x", Value::from(x)),
            ("z", Value::from(z)),
            ("heading", Value::from(0.0)),
        ],
    )
    .await?;
    let result: serde_json::Value = serde_json::from_str(&spawn_json)
        .unwrap_or_else(|_| serde_json::json!({"success": true}));
    Ok(warp::reply::json(&result))
}

#[derive(serde::Deserialize)]
pub(crate) struct PriorityBody {
    pub(crate) objective: std::string::String,
    pub(crate) priority: bool,
}

/// POST /api/admin/priority  — mark/unmark an objective as commander's-intent
/// priority (display/coordination only, see bflib's SetObjectivePriority).
pub(crate) async fn api_admin_priority(
    session_id: Option<Uuid>,
    body: PriorityBody,
    db: StatsDb,
    inst: Inst,
) -> std::result::Result<impl warp::Reply, Error> {
    let who = require_admin(session_id, db.clone()).await?;
    use netidx::publisher::Value;
    call_engine_rpc_str(
        &db,
        &inst,
        "set-objective-priority",
        vec![
            ("objective", Value::from(body.objective.clone())),
            ("priority", Value::from(body.priority)),
        ],
    )
    .await?;
    audit(
        &db,
        &who,
        "objective priority",
        Some(&format!("{} = {}", body.objective, body.priority)),
        Some(&inst.id),
    );
    Ok(warp::reply::json(&serde_json::json!({"ok": true, "priority": body.priority})))
}
