//! Public campaign routes: rounds, leaderboard, objectives, briefing,
//! situation, stats, warehouse, instances, unit database.

use crate::*;

/// The campaign.json keys the public `/api/config` may hand out: the
/// dashboard's branding (bfweb/src/config/campaign.ts) and the site's copy.
/// An allow-list, because campaign.json is also where a `gci` block can live
/// -- SRS/EAM passwords included -- and the file used to be served raw to
/// anyone who asked.
pub(crate) const PUBLIC_CONFIG_KEYS: &[&str] = &[
    "name", "shortName", "tagline", "version", "logoUrl", "description",
    "discord", "server", "servers", "serverIp", "websiteUrl", "wikiUrl", "dashboardUrl", "donationUrl",
    "blueLabel", "redLabel",
    "accentColor", "accentHoverColor", "blueColor", "redColor",
    "bgColor", "bgCardColor", "bgElevatedColor", "borderColor",
    "mapCenter", "mapZoom",
    "dashboardRightPanelWidth", "dashboardObjectivesHeight", "dashboardKillFeedCount",
    "backgroundImage", "backgroundImageOpacity", "backgroundImageSize", "backgroundImageBlend",
    "camoPattern", "camoOpacity",
    "engineCredits", "serverCredits", "features", "joinSteps",
];

/// campaign.json reduced to [`PUBLIC_CONFIG_KEYS`].
pub(crate) fn public_campaign_config(raw: &str) -> String {
    let Ok(serde_json::Value::Object(map)) = serde_json::from_str::<serde_json::Value>(raw) else {
        return "{}".to_string();
    };
    let kept: serde_json::Map<std::string::String, serde_json::Value> = map
        .into_iter()
        .filter(|(k, _)| PUBLIC_CONFIG_KEYS.contains(&k.as_str()))
        .collect();
    serde_json::Value::Object(kept).to_string()
}

pub(crate) async fn api_config(cfg_json: Arc<String>) -> impl warp::Reply {
    reply::with_header(
        reply::with_header((*cfg_json).clone(), "content-type", "application/json"),
        "cache-control",
        "no-store",
    )
}

// ── API handlers ────────────────────────────────────────────────────

/// GET /api/rounds — the round history for one DCS server instance.
///
/// `?instance=all` returns every instance's rounds instead; each row carries
/// its own `instance` id either way, so the dashboard's round picker can label
/// (or group) them without a second request.
pub(crate) async fn api_rounds(
    db: StatsDb,
    q: std::collections::HashMap<std::string::String, std::string::String>,
    inst: Inst,
) -> std::result::Result<impl warp::Reply, Error> {
    let all = q.get("instance").map(|s| s == "all").unwrap_or(false);
    let data = task::block_in_place(|| -> Result<String> {
        let rounds = db.all_rounds()?;
        let numbers = db.round_numbers()?;
        let entries: Vec<_> = rounds
            .iter()
            .filter_map(|(scenario, rid, round)| {
                let owner = db.round_instance_of(*rid);
                if !all && owner != inst.id {
                    return None;
                }
                Some(serde_json::json!({
                    "id": rid.0,
                    // What to call it: the id is a database key.
                    "number": numbers.get(rid),
                    "instance": owner.to_string(),
                    "scenario": scenario.to_string(),
                    "start": round.start.to_rfc3339(),
                    "end": round.end.map(|d| d.to_rfc3339()),
                    "active": round.end.is_none(),
                    "winner": round.winner.map(|s| format!("{s:?}")),
                }))
            })
            .collect();
        Ok(serde_json::to_string(&entries)?)
    })?;
    Ok(json_response(data))
}

/// GET /api/rounds/{id}/summary -- one round's top pilots and its totals,
/// for the round history page. A per-round leaderboard walks the whole
/// aggregates tree, so it is its own (cached) request, made only for the
/// round someone opens.
pub(crate) async fn api_round_summary(
    id: u64,
    db: StatsDb,
    inst: Inst,
) -> std::result::Result<impl warp::Reply, Error> {
    let data = cached_json(format!("round_summary:{}:{id}", inst.id), move || -> Result<String> {
        let rid = db::RoundId(id);
        if db.round_instance_of(rid) != inst.id {
            return Ok("null".into());
        }
        let mut pilots = db.pilot_leaderboard(Some(rid))?;
        // The best pilot in each line of work, for the round-over rundown.
        // None where nobody did any of it.
        let leader = |f: &dyn Fn(&db::Aggregates) -> u32| {
            pilots
                .iter()
                .map(|(_, name, a)| (name, f(a)))
                .filter(|(_, v)| *v > 0)
                .max_by_key(|(_, v)| *v)
                .map(|(name, v)| serde_json::json!({ "name": name, "value": v }))
        };
        let leaders = serde_json::json!({
            "air": leader(&|a| a.air_kills),
            "ground": leader(&|a| a.ground_kills),
            "captures": leader(&|a| a.captures),
            "logistics": leader(&|a| a.supply_transfers + a.repairs + a.troops + a.farps + a.deploys),
        });
        let round = db.all_rounds()?.into_iter().find(|(_, r, _)| *r == rid).map(|(_, _, r)| r);
        let (mut air, mut ground, mut captures, mut supply, mut hours) = (0u32, 0u32, 0u32, 0u32, 0f32);
        for (_, _, a) in &pilots {
            air += a.air_kills;
            ground += a.ground_kills;
            captures += a.captures;
            supply += a.supply_transfers;
            hours += a.hours;
        }
        // Best all-round effort first: kills, captures and logistics all count.
        let score = |a: &db::Aggregates| {
            (a.air_kills + a.ground_kills) as u64 * 10
                + a.captures as u64 * 30
                + (a.supply_transfers + a.repairs + a.troops + a.farps + a.deploys) as u64 * 5
        };
        pilots.sort_by(|x, y| score(&y.2).cmp(&score(&x.2)));
        let top: Vec<_> = pilots
            .iter()
            .take(5)
            .map(|(ucid, name, a)| {
                serde_json::json!({
                    "ucid": ucid,
                    "name": name,
                    "air_kills": a.air_kills,
                    "ground_kills": a.ground_kills,
                    "captures": a.captures,
                    "logistics": a.supply_transfers + a.repairs + a.troops + a.farps + a.deploys,
                    "deaths": a.deaths,
                    "hours": a.hours,
                })
            })
            .collect();
        Ok(serde_json::to_string(&serde_json::json!({
            "id": id,
            "number": db.round_number(rid),
            "start": round.as_ref().map(|r| r.start),
            "end": round.as_ref().and_then(|r| r.end),
            "winner": round.as_ref().and_then(|r| r.winner).map(|s| format!("{s:?}")),
            "leaders": leaders,
            "pilots": pilots.len(),
            "air_kills": air,
            "ground_kills": ground,
            "captures": captures,
            "supply_runs": supply,
            "hours": hours,
            "top": top,
        }))?)
    })
    .await?;
    Ok(json_response(data))
}

/// GET /api/rounds/:id/report — the round over time: kills and captures by
/// side per bucket, territory held, most contested objectives.
pub(crate) async fn api_round_report(
    id: u64,
    db: StatsDb,
    inst: Inst,
) -> std::result::Result<impl warp::Reply, Error> {
    let data = cached_json(format!("round_report:{}:{id}", inst.id), move || -> Result<String> {
        let rid = db::RoundId(id);
        if db.round_instance_of(rid) != inst.id {
            return Ok("null".into());
        }
        Ok(serde_json::to_string(&db.round_report(rid)?)?)
    })
    .await?;
    Ok(json_response(data))
}

pub(crate) async fn api_leaderboard(
    db: StatsDb,
    inst: Inst,
    q: std::collections::HashMap<std::string::String, std::string::String>,
) -> std::result::Result<impl warp::Reply, Error> {
    // ?round=<id> ranks one round; without it, all-time totals (so pilot
    // stats are never empty between rounds).
    let round = q.get("round").and_then(|r| r.parse::<u64>().ok());
    let data = cached_json(format!("leaderboard:{}:{round:?}", inst.id), move || -> Result<String> {
        let pilots = match round {
            // Another instance's round is not this instance's to rank.
            Some(id) if db.round_instance_of(db::RoundId(id)) != inst.id => vec![],
            Some(id) => db.pilot_leaderboard(Some(db::RoundId(id)))?,
            None => db.pilot_leaderboard(None)?,
        };
        // Coalition drives which service's rank insignia the dashboard draws
        // for this pilot. Same resolution the recon-intel gate uses: the
        // active round's registration, else the most recent side on record
        // (this instance's rounds only). None for a pilot who never
        // registered a side. One pass for the whole roster -- this used to be
        // a prefix scan per pilot on every poll.
        let sides: std::collections::HashMap<dcso3::net::Ucid, dcso3::coalition::Side> =
            db.all_pilot_sides(&inst.id)?.into_iter().collect();
        let entries: Vec<_> = pilots
            .iter()
            .map(|(ucid, name, agg)| {
                let side = sides
                    .get(ucid)
                    // Debug, not Display: the rest of the API emits "Blue"/"Red",
                    // while Side's Display is the DCS-side lowercase spelling.
                    .map(|s| format!("{s:?}"));
                serde_json::json!({
                    "ucid": ucid.to_string(),
                    "name": name.to_string(),
                    "side": side,
                    "air_kills": agg.air_kills,
                    "ground_kills": agg.ground_kills,
                    "captures": agg.captures,
                    "repairs": agg.repairs,
                    "supply_transfers": agg.supply_transfers,
                    "troops": agg.troops,
                    "farps": agg.farps,
                    "deploys": agg.deploys,
                    "actions": agg.actions,
                    "deaths": agg.deaths,
                    "hours": agg.hours,
                    "donated_points": agg.donated_points,
                })
            })
            .collect();
        Ok(serde_json::to_string(&entries)?)
    })
    .await?;
    Ok(json_response(data))
}

pub(crate) async fn api_objectives(
    db: StatsDb,
    round_id: Option<u64>,
    inst: Inst,
) -> std::result::Result<impl warp::Reply, Error> {
    let (mut entries, is_active) = task::block_in_place(|| -> Result<(Vec<serde_json::Value>, bool)> {
        let rounds = db.latest_rounds_for(&inst.id)?;
        let active_rid = rounds.iter().find(|(_, _, r)| r.end.is_none()).map(|(_, rid, _)| *rid);
        let rid = match round_id {
            // Another instance's round is not this instance's to show (and
            // may be a private one).
            Some(id) if db.round_instance_of(db::RoundId(id)) != inst.id => return Ok((vec![], false)),
            Some(id) => db::RoundId(id),
            None => match active_rid {
                Some(rid) => rid,
                None => match rounds.first() {
                    Some((_, rid, _)) => *rid,
                    None => return Ok((vec![], false)),
                },
            },
        };
        let is_active = active_rid == Some(rid);
        let objs = db.objectives_for_round(rid)?;
        let entries: Vec<_> = objs
            .iter()
            .filter_map(|(oid, obj)| {
                // Special SAM sites are secret objectives -- hidden entirely.
                if obj.kind.is_special_sam_site() {
                    return None;
                }
                // Carrier groups are shown for status (health/supply/owner/etc)
                // but their position is withheld -- it's mobile and sensitive.
                let hide_pos = obj.kind.is_carrier_group();
                Some(serde_json::json!({
                    "id": format!("{:?}", oid),
                    "name": obj.name.to_string(),
                    "kind": obj.kind.name(),
                    "owner": format!("{:?}", obj.owner),
                    "lat": if hide_pos { 0.0 } else { obj.pos.latitude },
                    "lon": if hide_pos { 0.0 } else { obj.pos.longitude },
                    "health": obj.health,
                    "logi": obj.logi,
                    "supply": obj.supply,
                    "fuel": obj.fuel,
                    "last_change": obj.last_change.to_rfc3339(),
                    // Overwritten below with live values for the active round,
                    // when bflib is reachable. Historical rounds keep the defaults.
                    "priority": false,
                    "threatened": false,
                    "captureable": false,
                    // Whether owner/health/... below came from the running
                    // engine or from the (possibly stale) persisted snapshot.
                    // A consumer that diffs successive polls to detect state
                    // CHANGES -- the Discord alert poller -- must ignore a
                    // response where this is false: mixing live and stale
                    // values across polls looks exactly like an objective
                    // flipping owner or crossing a health threshold, and
                    // produces an endless stream of phantom alerts.
                    "live": false,
                }))
            })
            .collect();
        Ok((entries, is_active))
    })?;

    // The priority flag is live engine state, not something bfdb persists on
    // its own -- refresh it from bflib for the currently active round only.
    // bflib's netidx RPC has no timeout of its own, so if the mission is
    // restarting/unreachable this call would otherwise hang the whole
    // request indefinitely -- bound it so /api/objectives always answers
    // within a few seconds, falling back to the persisted (possibly stale)
    // priority flags on timeout rather than blocking every caller (including
    // the Discord bot's poller, which has its own 10s client timeout).
    let mut live_ok = false;
    if is_active {
        let live = engine_objectives(&db, &inst).await;
        match &*live {
            Ok(json) => {
                if let Ok(live) = serde_json::from_str::<Vec<bfprotocols::api::ObjectiveInfo>>(&json) {
                    // The persisted health/logi/supply/fuel can lag the engine
                    // (e.g. after a mission reload re-emits Stat::Objective at
                    // 100, or if the archive replay missed a batch), which made
                    // the tactical map disagree with the in-game markup. For the
                    // active round, take these straight from the engine.
                    let by_name: std::collections::HashMap<&str, &bfprotocols::api::ObjectiveInfo> =
                        live.iter().map(|o| (o.name.as_str(), o)).collect();
                    for entry in entries.iter_mut() {
                        if let Some(name) = entry.get("name").and_then(|n| n.as_str()) {
                            if let Some(&o) = by_name.get(name) {
                                entry["priority"] = serde_json::Value::Bool(o.priority);
                                entry["health"] = serde_json::json!(o.health);
                                entry["logi"] = serde_json::json!(o.logi);
                                entry["supply"] = serde_json::json!(o.supply);
                                entry["fuel"] = serde_json::json!(o.fuel);
                                entry["owner"] = serde_json::json!(format!("{:?}", o.owner));
                                entry["threatened"] = serde_json::Value::Bool(o.threatened);
                                entry["captureable"] = serde_json::Value::Bool(o.captureable);
                                entry["live"] = serde_json::Value::Bool(true);
                            }
                        }
                    }
                    live_ok = true;
                    // What the engine doesn't have is not in this campaign.
                    // A round store can hold another scenario's objectives --
                    // vs2's 2008 round kept 16 Syria ones from an ODF run --
                    // and every map drew them: the Discord snapshot of the
                    // Caucasus war zoomed out to take in Lebanon.
                    entries.retain(|e| e["live"] == serde_json::Value::Bool(true));
                }
            }
            // Already logged when the (cached) call was made.
            Err(()) => (),
        }
    }

    let data = serde_json::to_string(&entries).map_err(|e| Error(e.into()))?;
    // Also as a header, so a consumer can tell a degraded response from an
    // empty one without inspecting every element.
    Ok(warp::reply::with_header(
        json_response(data),
        "x-fowl-live",
        if live_ok { "1" } else { "0" },
    ))
}

/// GET /api/frontline?round=N — the dividing line between blue-held and
/// red-held ground, as `[{mid, blue, red}]` where each is a `[[lat, lon], …]`
/// polyline. Computed with the exact same code bflib uses for the F10-map
/// overlay (`bfprotocols::frontline`), so the two never disagree. Only the
/// line geometry is returned — never an objective's position.
pub(crate) async fn api_frontline(
    db: StatsDb,
    round_id: Option<u64>,
    inst: Inst,
) -> std::result::Result<impl warp::Reply, Error> {
    // Only the live map can be checked against the engine; see api_objectives
    // for why objectives it doesn't have are left out.
    let in_engine = match round_id {
        None => engine_objective_names(&db, &inst).await,
        Some(_) => None,
    };
    let data = task::block_in_place(|| -> Result<String> {
        let rounds = db.latest_rounds_for(&inst.id)?;
        let active = rounds.iter().find(|(_, _, r)| r.end.is_none()).map(|(_, rid, _)| *rid);
        let rid = match round_id {
            // Another instance's round is not this instance's to show.
            Some(id) if db.round_instance_of(db::RoundId(id)) != inst.id => return Ok("[]".to_string()),
            Some(id) => db::RoundId(id),
            None => match active {
                Some(rid) => rid,
                None => match rounds.first() {
                    Some((_, rid, _)) => *rid,
                    None => return Ok("[]".to_string()),
                },
            },
        };
        let in_engine = in_engine.as_ref().filter(|_| active == Some(rid));
        let objs = db.objectives_for_round(rid)?;

        // Owned, on-the-ground objectives (SAM sites included, carriers not) —
        // the same set bflib feeds the overlay.
        let ll: Vec<(f64, f64, dcso3::coalition::Side)> = objs
            .iter()
            .filter(|(_, o)| matches!(o.owner, dcso3::coalition::Side::Blue | dcso3::coalition::Side::Red))
            .filter(|(_, o)| !o.kind.is_carrier_group())
            .filter(|(_, o)| in_engine.map_or(true, |n| n.contains(o.name.as_str())))
            .map(|(_, o)| (o.pos.latitude, o.pos.longitude, o.owner))
            .collect();
        if ll.len() < 4 {
            return Ok("[]".to_string());
        }

        // Project lat/lon to a local equirectangular frame in metres so the
        // metre-denominated tuning in bfprotocols::frontline means the same
        // thing here as it does in the engine.
        const M_PER_DEG: f64 = 111_320.0;
        let lat0 = ll.iter().map(|(la, _, _)| *la).sum::<f64>() / ll.len() as f64;
        let lon0 = ll.iter().map(|(_, lo, _)| *lo).sum::<f64>() / ll.len() as f64;
        let coslat = lat0.to_radians().cos().max(1e-6);
        let to_m = |lat: f64, lon: f64| ((lat - lat0) * M_PER_DEG, (lon - lon0) * M_PER_DEG * coslat);
        let to_ll = |x: f64, y: f64| [lat0 + x / M_PER_DEG, lon0 + y / (M_PER_DEG * coslat)];

        let pts: Vec<(f64, f64, f64)> = ll
            .iter()
            .map(|&(lat, lon, side)| {
                let (x, y) = to_m(lat, lon);
                (x, y, if side == dcso3::coalition::Side::Blue { 1.0 } else { -1.0 })
            })
            .collect();

        let fl = bfprotocols::frontline::compute(&pts, &bfprotocols::frontline::Params::default());
        let cvt = |lines: &[Vec<[f64; 2]>]| -> Vec<Vec<[f64; 2]>> {
            lines
                .iter()
                .map(|l| l.iter().map(|q| to_ll(q[0], q[1])).collect())
                .collect()
        };
        let out = serde_json::json!({
            "mid": cvt(&fl.mid),
            "blue": cvt(&fl.blue),
            "red": cvt(&fl.red),
        });
        Ok(serde_json::to_string(&out)?)
    })?;
    Ok(json_response(data))
}

/// GET /api/briefing — the caller's own coalition kneeboard briefing (navaids,
/// radios, artillery, deployables, threats). Coalition-locked: you get your
/// side and can't request the enemy's; admins may pass `?side=blue|red`.
/// Proxied live from the engine; only meaningful for the active round.
pub(crate) async fn api_briefing(
    session_id: Option<Uuid>,
    db: StatsDb,
    bot_cfg: Arc<Option<BotLinkConfig>>,
    query: std::collections::HashMap<std::string::String, std::string::String>,
    inst: Inst,
) -> std::result::Result<impl warp::Reply, Error> {
    use netidx::publisher::Value;
    let c = require_coalition(&query, session_id, &db, &bot_cfg, &inst).await?;
    // dcso3's Side::from_str only accepts lowercase.
    let side = match c.side {
        dcso3::coalition::Side::Red => "red",
        _ => "blue",
    };
    match tokio::time::timeout(
        std::time::Duration::from_secs(5),
        call_engine_rpc_str(&db, &inst, "query-briefing", vec![("side", Value::from(side.to_string()))]),
    )
    .await
    {
        Ok(Ok(data)) => Ok(json_response(data)),
        Ok(Err(e)) => {
            log::warn!("api_briefing: query-briefing RPC failed ({side}): {}", e.0);
            Err(e)
        }
        Err(_) => {
            log::warn!("api_briefing: query-briefing RPC timed out after 5s ({side}) -- engine unreachable or old bflib.dll");
            Err(Error(websec::unavailable("engine did not answer query-briefing (unreachable, or bflib.dll predates this feature)")))
        }
    }
}

/// GET /api/situation — the caller's own coalition auto-generated situational
/// briefing: posture, ranked tasking, hotspots, the air-defence areas *that
/// side* has actually earned intel on, the air picture, logistics and the comms
/// card, plus a positioned objective layer for the briefing map.
///
/// Coalition-locked exactly like `/api/briefing`: the viewer's session cookie
/// resolves to a coalition and the engine builds the report for that side only,
/// so a browser can never pull the other side's intel. Admins with no in-game
/// side may pass `?side=blue|red`.
///
/// The engine has no campaign history of its own, so `recent` comes back as an
/// in-session derivation; we replace it with the real persisted capture log
/// before answering.
pub(crate) async fn api_situation(
    session_id: Option<Uuid>,
    db: StatsDb,
    bot_cfg: Arc<Option<BotLinkConfig>>,
    query: std::collections::HashMap<std::string::String, std::string::String>,
    inst: Inst,
) -> std::result::Result<impl warp::Reply, Error> {
    use netidx::publisher::Value;
    let c = require_coalition(&query, session_id, &db, &bot_cfg, &inst).await?;
    // dcso3's Side::from_str only accepts lowercase.
    let side_str = match c.side {
        dcso3::coalition::Side::Red => "red",
        _ => "blue",
    };
    // The engine walks every objective, the intel db and the radar net to build
    // this -- it is heavier than query-briefing, so allow the same 8s the
    // objectives poll uses rather than 5s.
    let raw = match tokio::time::timeout(
        std::time::Duration::from_secs(8),
        call_engine_rpc_str(
            &db,
            &inst,
            "query-situation",
            vec![("side", Value::from(side_str.to_string()))],
        ),
    )
    .await
    {
        Ok(Ok(data)) => data,
        Ok(Err(e)) => {
            log::warn!("api_situation: query-situation RPC failed ({side_str}): {}", e.0);
            return Err(e);
        }
        Err(_) => {
            log::warn!(
                "api_situation: query-situation RPC timed out after 8s ({side_str}) -- engine unreachable or old bflib.dll"
            );
            // 503 like every other "engine didn't answer", not a 500.
            return Err(Error(websec::unavailable(
                "engine did not answer query-situation (unreachable, or bflib.dll predates this feature)",
            )));
        }
    };

    // Swap the engine's in-session guess at "recent" for the persisted capture
    // log, which survives mission restarts and names the pilots involved.
    let mut rep: bfprotocols::situation::SituationReport = match serde_json::from_str(&raw) {
        Ok(r) => r,
        Err(e) => {
            // Don't fail the request over an enrichment step -- hand the
            // engine's own JSON through unchanged.
            log::warn!("api_situation: could not parse engine report for enrichment: {e}");
            return Ok(json_response(raw));
        }
    };
    let captures = task::block_in_place(|| -> Result<Vec<serde_json::Value>> {
        let rounds = db.latest_rounds_for(&inst.id)?;
        let Some((_, rid, _)) = rounds.iter().find(|(_, _, r)| r.end.is_none()) else {
            return Ok(vec![]);
        };
        Ok(db
            .recent_captures(*rid, 20)?
            .iter()
            .map(|cap| {
                serde_json::json!({
                    "at": cap.time.to_rfc3339(),
                    "objective": cap.objective_name.to_string(),
                    "side": format!("{:?}", cap.side),
                    "by": cap.by.iter()
                        .map(|u| db.pilot_name(u).unwrap_or_else(|| u.to_string()))
                        .collect::<Vec<_>>(),
                })
            })
            .collect())
    })
    .unwrap_or_default();
    if !captures.is_empty() {
        let mut recent = vec![];
        for cap in &captures {
            let obj = cap["objective"].as_str().unwrap_or("?");
            let by_side = cap["side"].as_str().unwrap_or("?");
            let pilots: Vec<&str> = cap["by"].as_array().map_or(vec![], |a| {
                a.iter().filter_map(|v| v.as_str()).collect()
            });
            let who = if pilots.is_empty() {
                std::string::String::new()
            } else {
                format!(" by {}", pilots.join(", "))
            };
            let at = cap["at"]
                .as_str()
                .and_then(|s| chrono::DateTime::parse_from_rfc3339(s).ok())
                .map(|d| d.with_timezone(&chrono::Utc))
                .unwrap_or_else(chrono::Utc::now);
            recent.push(bfprotocols::situation::SituationEvent {
                at,
                text: format!("{obj} captured by {by_side}{who}"),
                good: Some(by_side == format!("{:?}", c.side)),
                lat: None,
                lon: None,
            });
        }
        // Keep the engine's damage/neutral lines too -- they aren't captures and
        // nothing else records them. Newest first.
        recent.extend(rep.recent.into_iter());
        recent.sort_by(|a, b| b.at.cmp(&a.at));
        recent.truncate(20);
        rep.recent = recent;
    }
    let data = serde_json::to_string(&rep).map_err(|e| Error(e.into()))?;
    Ok(json_response(data))
}

pub(crate) async fn api_kills(
    db: StatsDb,
    round_id: Option<u64>,
    limit: Option<usize>,
    inst: Inst,
) -> std::result::Result<impl warp::Reply, Error> {
    // Bounded so the cache key space (and the answer) stays small.
    let limit = limit.unwrap_or(50).clamp(1, 500);
    let key = format!("kills:{}:{round_id:?}:{limit}", inst.id);
    // recent_kills walks the whole kills tree; see ROUTE_CACHE.
    let data = cached_json(key, move || -> Result<String> {
        let rounds = db.latest_rounds_for(&inst.id)?;
        let rid = match round_id {
            // Another instance's round is not this instance's to show.
            Some(id) if db.round_instance_of(db::RoundId(id)) != inst.id => return Ok("[]".to_string()),
            Some(id) => db::RoundId(id),
            None => match rounds.iter().find(|(_, _, r)| r.end.is_none()) {
                Some((_, rid, _)) => *rid,
                None => match rounds.first() {
                    Some((_, rid, _)) => *rid,
                    None => return Ok("[]".to_string()),
                },
            },
        };
        let kills = db.recent_kills(rid, limit)?;
        let entries: Vec<_> = kills
            .iter()
            .map(|dead| {
                let victim_name = dead.victim.ucid().map(|u| u.to_string());
                let victim_side = format!("{:?}", dead.victim.side());
                // Same classification as air_kills/ground_kills in the stats
                // aggregator (record_kill) -- lets API consumers (e.g. the
                // Discord kill-streak/achievement poller) filter on air kills
                // specifically instead of guessing from target_type's raw DCS
                // unit-type string.
                let is_air = db.victim_is_air(rid, &dead.victim).unwrap_or(false);
                // The "killer" is the shot that finished the target -- the last
                // hit, not the first (a target can be grazed by several shooters
                // over a long engagement before one drops it).
                let killer = dead
                    .shots
                    .iter()
                    .filter(|s| s.hit)
                    .max_by_key(|s| s.time)
                    .or_else(|| dead.shots.last())
                    .map(|s| {
                        serde_json::json!({
                            "ucid": s.shooter.ucid().map(|u| u.to_string()),
                            "side": format!("{:?}", s.shooter.side()),
                            "weapon": display_weapon(s.weapon_name.as_ref()),
                            "airframe": s.shooter_typ.as_deref(),
                        })
                    });
                serde_json::json!({
                    "time": dead.time.to_rfc3339(),
                    "victim": {
                        "ucid": victim_name,
                        "side": victim_side,
                    },
                    "killer": killer,
                    "target_type": dead.shots.first().map(|s| s.target_typ.to_string()),
                    "is_air": is_air,
                })
            })
            .collect();
        Ok(serde_json::to_string(&entries)?)
    })
    .await?;
    Ok(json_response(data))
}

/// Warehouse + supply topology for one objective.
///
/// Coalition-gated on purpose. The engine decides what the asking side may
/// see -- own objectives in full, enemy objectives reduced to whatever that
/// side's intel database already knows -- so this route's only job is to
/// resolve who is asking and pass it through. An admin with no resolvable
/// coalition gets the unscoped view.
pub(crate) async fn api_warehouse(
    session_id: Option<Uuid>,
    db: StatsDb,
    bot_cfg: Arc<Option<BotLinkConfig>>,
    query: std::collections::HashMap<std::string::String, std::string::String>,
    inst: Inst,
) -> std::result::Result<impl warp::Reply, Error> {
    use netidx::publisher::Value;
    let objective = query
        .get("objective")
        .cloned()
        .ok_or_else(|| Error(websec::bad_request("missing ?objective=<id or exact name>")))?;

    // This MUST fail closed. require_coalition returns Err for "not logged in"
    // and "session expired" as well as "no coalition", and an earlier version
    // mapped every one of those to an unscoped query -- which the engine
    // answers in full, for every objective. That handed any anonymous visitor
    // the enemy's complete stock and supply map and defeated the entire point
    // of gating this route.
    //
    // Admins are the only unscoped case, and require_coalition already marks
    // them: a session with no side but is_admin comes back with god_mode set.
    let caller = require_coalition(&query, session_id, &db, &bot_cfg, &inst).await?;
    let side = if caller.god_mode {
        ""
    } else {
        match caller.side {
            dcso3::coalition::Side::Red => "red",
            _ => "blue",
        }
    };

    // A person is waiting on this (the TacMap's base drawer, the objectives
    // page), so it goes the patient way, like /api/command: on a busy engine
    // (vs1) the background pollers open the breaker while the engine still
    // answers in a few seconds, and a counted call was refused outright. One
    // read per instance, side and base serves everyone looking at it for a
    // while; the key carries the side, so a side only ever gets its own read.
    let key = format!("{}:{side}:{objective}", inst.id);
    let fresh = WAREHOUSE_CACHE
        .entry(&key)
        .get_or_refresh(WAREHOUSE_TTL, || async {
            call_engine_rpc_str_patient(
                &db,
                &inst,
                "query-warehouse",
                vec![
                    ("objective", Value::from(objective.clone())),
                    ("side", Value::from(side.to_string())),
                ],
            )
            .await
            .map(Arc::new)
        })
        .await;
    let mut last = WAREHOUSE_LAST_GOOD.lock().unwrap_or_else(|e| e.into_inner());
    match fresh {
        Ok(raw) => {
            // Caller-chosen keys (`?objective=`): keep the map bounded.
            if last.len() > 512 {
                last.clear();
            }
            last.insert(key, (std::time::Instant::now(), raw.clone()));
            Ok(json_response(raw.to_string()))
        }
        // A missed refresh serves the last read the engine did send for a
        // while: stock moves slowly, and a drawer going blank helps nobody.
        Err(e) => match last.get(&key) {
            Some((at, raw)) if at.elapsed() < WAREHOUSE_STALE_MAX => {
                log::debug!("api_warehouse: {objective}: {}; serving the read from {}s ago", e.0, at.elapsed().as_secs());
                Ok(json_response(raw.to_string()))
            }
            _ => {
                log::warn!("api_warehouse: query-warehouse failed for {objective}: {}", e.0);
                Err(e)
            }
        },
    }
}

/// How long one engine read of a base's warehouse serves every viewer.
pub(crate) const WAREHOUSE_TTL: std::time::Duration = std::time::Duration::from_secs(20);
pub(crate) static WAREHOUSE_CACHE: std::sync::LazyLock<websec::CacheMap<std::string::String, Arc<std::string::String>>> =
    std::sync::LazyLock::new(websec::CacheMap::new);
/// The last read the engine did send, per instance, side and base, served
/// when a refresh fails -- for this long, after which the error shows.
pub(crate) static WAREHOUSE_LAST_GOOD: std::sync::LazyLock<
    std::sync::Mutex<std::collections::HashMap<std::string::String, (std::time::Instant, Arc<std::string::String>)>>,
> = std::sync::LazyLock::new(Default::default);
pub(crate) const WAREHOUSE_STALE_MAX: std::time::Duration = std::time::Duration::from_secs(600);

/// Whether the engine still needs to be told about this server info.
///
/// `/api/stats` runs on every dashboard poll from every open tab, and each run
/// used to fire a `set-server-info` RPC at the engine. Send it when something
/// actually changed, and otherwise no more than once a minute -- enough to
/// re-seed an engine that restarted, without hammering it with a value it
/// already has.
pub(crate) fn should_push_server_info(inst: &Inst, payload: &str) -> bool {
    use std::collections::hash_map::DefaultHasher;
    use std::hash::{Hash, Hasher};
    use std::sync::{LazyLock, Mutex};
    const QUIET_SECS: i64 = 60;
    static LAST: LazyLock<Mutex<std::collections::HashMap<std::string::String, (i64, u64)>>> =
        LazyLock::new(|| Mutex::new(std::collections::HashMap::new()));
    let mut h = DefaultHasher::new();
    payload.hash(&mut h);
    let hash = h.finish();
    let now = chrono::Utc::now().timestamp();
    let mut last = match LAST.lock() {
        Ok(l) => l,
        Err(e) => e.into_inner(),
    };
    match last.get(&inst.id.to_string()) {
        Some((ts, h)) if *h == hash && now - *ts < QUIET_SECS => false,
        _ => {
            last.insert(inst.id.to_string(), (now, hash));
            true
        }
    }
}

pub(crate) async fn api_stats(
    db: StatsDb,
    bot_cfg: Arc<Option<BotLinkConfig>>,
    inst: Inst,
) -> std::result::Result<impl warp::Reply, Error> {
    // The per-round leaderboard behind this walks the whole aggregates tree;
    // every dashboard polls it. See ROUTE_CACHE.
    static STATS: std::sync::LazyLock<websec::CacheMap<InstanceId, serde_json::Value>> =
        std::sync::LazyLock::new(websec::CacheMap::new);
    let db2 = db.clone();
    let inst2 = inst.clone();
    let mut value = STATS
        .entry(&inst.id)
        .get_or_refresh(PUBLIC_CACHE_TTL, || async move {
            task::block_in_place(|| stats_summary(&db2, &inst2)).map_err(Error)
        })
        .await?;
    let bot_info = fetch_bot_server_info(&bot_cfg, &inst).await;
    stats_apply_bot_info(&db, &inst, &mut value, bot_info).await;
    Ok(json_response(serde_json::to_string(&value).map_err(anyhow::Error::from)?))
}

/// The DB-derived half of `/api/stats`.
pub(crate) fn stats_summary(db: &StatsDb, inst: &Inst) -> Result<serde_json::Value> {
    {
        let rounds = db.latest_rounds_for(&inst.id)?;
        let active_round = rounds.iter().find(|(_, _, r)| r.end.is_none());
        let active_rid = active_round.map(|(_, rid, _)| *rid);
        let pilots = db.pilot_leaderboard(active_rid)?;
        let obj_count = if let Some((_, rid, _)) = active_round {
            // Match the filtering in api_objectives (only special SAM sites
            // are hidden now) so this count stays consistent with what
            // /api/objectives actually reports.
            db.objectives_for_round(*rid)?
                .iter()
                .filter(|(_, obj)| !obj.kind.is_special_sam_site())
                .count()
        } else {
            0
        };
        let total_kills: u32 = pilots.iter().map(|(_, _, a)| a.air_kills + a.ground_kills).sum();
        // Fallback only -- bflib's own session stop_time, used when
        // DCSServerBot isn't configured/reachable (see below, api_stats
        // prefers the bot's own scheduler restart time when available,
        // since that's what actually restarts the server on this setup).
        let local_restart_at = active_round
            .and_then(|(_, rid, _)| db.active_session_stop(*rid))
            .map(|t| t.to_rfc3339());
        let weather = db.latest_weather(&inst).map(|w| serde_json::json!({
            "temp_c": w.temp_c,
            "wind_speed_kts": w.wind_speed_kts,
            "wind_from_deg": w.wind_from_deg,
            "cloud_base_m": w.cloud_base_m,
            "qnh_hpa": w.qnh_hpa,
            "cloud_density": w.cloud_density,
            "visibility_m": w.visibility_m,
        }));
        let (blue_reg, red_reg, blue_online, red_online) = if let Some((_, rid, _)) = active_round {
            db.pilot_side_counts(*rid).unwrap_or_default()
        } else {
            (0, 0, 0, 0)
        };
        // Round history shown as a headline figure is the public one, so a
        // test instance's rounds don't inflate it. (`rounds` above is already
        // this instance's own, but when a private instance is selected by an
        // admin we still report the public total for consistency with the
        // leaderboard beside it.)
        let public_rounds = db.public_rounds()?;
        let total_rounds = match &public_rounds {
            None => rounds.len(),
            Some(allowed) => rounds.iter().filter(|(_, rid, _)| allowed.contains(rid)).count(),
        };
        Ok(serde_json::json!({
            "total_pilots": pilots.len(),
            "total_rounds": total_rounds,
            "active_round": active_round.map(|(s, rid, r)| serde_json::json!({
                "id": rid.0,
                "number": db.round_number(*rid),
                "scenario": s.to_string(),
                "start": r.start.to_rfc3339(),
            })),
            "objective_count": obj_count,
            "total_kills": total_kills,
            "restart_at": local_restart_at,
            "weather": weather,
            "blue_registered": blue_reg,
            "red_registered": red_reg,
            "blue_online": blue_online,
            "red_online": red_online,
        }))
    }
}

/// The DCSServerBot half of `/api/stats`: its restart time and weather win
/// over the engine's, and are pushed back into the engine.
pub(crate) async fn stats_apply_bot_info(
    db: &StatsDb,
    inst: &Inst,
    value: &mut serde_json::Value,
    bot_info: Option<BotServerInfo>,
) {
    // DCSServerBot's Scheduler plugin is what actually restarts this server
    // (bflib's own stop_time isn't in play here) -- prefer its restart_time
    // when reachable, keep the bflib-derived fallback above otherwise. The
    // bot appears to hold onto the last-computed restart_time rather than
    // always keeping a future one queued (observed: it can sit on an
    // already-elapsed moment for a while after a restart), so only trust
    // it if it's actually still ahead of us -- a countdown to the past
    // just clamps to zero and sits there, which reads as broken rather
    // than "no restart currently scheduled".
    if let Some(bot_restart_at) = bot_info.as_ref().and_then(|s| s.restart_time.as_deref()).and_then(parse_bot_datetime) {
        if bot_restart_at > chrono::Utc::now() {
            value["restart_at"] = serde_json::json!(bot_restart_at.to_rfc3339());
        } else {
            value["restart_at"] = serde_json::Value::Null;
        }
    }

    // Prefer DCSServerBot's live weather reading (same /servers response as
    // restart_time above -- no extra request). bflib's own pipeline
    // (bftools --live-weather -> mission file -> atmosphere.getWind ->
    // Stat::Weather) reports dead calm (0 kt) whenever the live-weather sync
    // isn't running, and the bot has the real METAR-derived values. Fall
    // back to whatever bflib published only when the bot has nothing.
    // See BotWeather for the unit conversions this needs (feet->meters,
    // mmHg->hPa).
    let mut bot_weather_json: Option<serde_json::Value> = None;
    if let Some(w) = bot_info.as_ref().and_then(|s| s.weather.as_ref()) {
        if w.wind_speed.is_some() || w.temperature.is_some() || w.pressure.is_some() {
            let wx = serde_json::json!({
                "temp_c": w.temperature,
                "wind_speed_kts": w.wind_speed,
                "wind_from_deg": w.wind_direction,
                "cloud_base_m": w.clouds_base.map(|ft| ft * 0.3048),
                "qnh_hpa": w.pressure.map(|mmhg| mmhg * 1.33322),
                "cloud_density": w.clouds_density,
                "visibility_m": w.visibility,
            });
            value["weather"] = wx.clone();
            bot_weather_json = Some(wx);
        }
    }

    // Push the bot-derived restart time + weather into the running engine so
    // the in-game F10 Info menu matches the dashboard. Fire-and-forget with a
    // short timeout -- a missing/slow engine must not hold up /api/stats. Only
    // the bot's own weather is pushed (never bflib's calm-mission reading
    // echoed back).
    if bot_info.is_some() {
        let payload = serde_json::json!({
            "restart_at": value["restart_at"].as_str(),
            "weather": bot_weather_json,
        })
        .to_string();
        if should_push_server_info(&inst, &payload) {
            let _ = tokio::time::timeout(
                std::time::Duration::from_secs(2),
                call_engine_rpc_str(
                    &db,
                    &inst,
                    "set-server-info",
                    vec![("info", netidx::publisher::Value::from(payload))],
                ),
            )
            .await;
        }
    }
}

pub(crate) async fn api_points(db: StatsDb, inst: Inst) -> std::result::Result<impl warp::Reply, Error> {
    // pilot_points walks every pilot's every round; see ROUTE_CACHE.
    let data = cached_json(format!("points:{}", inst.id), move || -> Result<String> {
        let rounds = db.latest_rounds_for(&inst.id)?;
        let rid = match rounds.iter().find(|(_, _, r)| r.end.is_none()) {
            Some((_, rid, _)) => *rid,
            None => return Ok("[]".to_string()),
        };
        let entries = db.pilot_points(rid)?;
        let json: Vec<_> = entries.iter().map(|(name, pts, side)| serde_json::json!({
            "name": name, "points": pts, "side": side,
        })).collect();
        Ok(serde_json::to_string(&json)?)
    })
    .await?;
    Ok(json_response(data))
}

pub(crate) async fn api_captures(db: StatsDb, inst: Inst) -> std::result::Result<impl warp::Reply, Error> {
    let data = task::block_in_place(|| -> Result<String> {
        let rounds = db.latest_rounds_for(&inst.id)?;
        let rid = match rounds.iter().find(|(_, _, r)| r.end.is_none()) {
            Some((_, rid, _)) => *rid,
            None => return Ok("[]".to_string()),
        };
        let entries = db.most_captured(rid)?;
        let json: Vec<_> = entries.iter().map(|(name, count)| serde_json::json!({
            "name": name, "count": count,
        })).collect();
        Ok(serde_json::to_string(&json)?)
    })?;
    Ok(json_response(data))
}

/// Recent capture events with pilot attribution -- distinct from
/// /api/captures, which is just a per-objective running count with no
/// timeline or "who did it".
pub(crate) async fn api_capture_events(
    db: StatsDb,
    round_id: Option<u64>,
    limit: Option<usize>,
    offset: usize,
    inst: Inst,
) -> std::result::Result<impl warp::Reply, Error> {
    let data = task::block_in_place(|| -> Result<String> {
        let rounds = db.latest_rounds_for(&inst.id)?;
        let rid = match round_id {
            // Another instance's round is not this instance's to show.
            Some(id) if db.round_instance_of(db::RoundId(id)) != inst.id => return Ok("[]".to_string()),
            Some(id) => db::RoundId(id),
            None => match rounds.iter().find(|(_, _, r)| r.end.is_none()) {
                Some((_, rid, _)) => *rid,
                None => match rounds.first() {
                    Some((_, rid, _)) => *rid,
                    None => return Ok("[]".to_string()),
                },
            },
        };
        // Same cap as /api/kills: `?limit=` is the caller's to choose, and
        // `?offset=` pages further back.
        let limit = limit.unwrap_or(50).clamp(1, 500);
        let offset = offset.min(10_000);
        let entries: Vec<_> = db.recent_captures(rid, limit + offset)?.into_iter().skip(offset).collect();
        let json: Vec<_> = entries
            .iter()
            .map(|c| {
                serde_json::json!({
                    "time": c.time.to_rfc3339(),
                    "objective": c.objective_name,
                    "side": format!("{:?}", c.side),
                    // pilot display names (fall back to ucid only if unknown)
                    "by": c.by.iter()
                        .map(|u| db.pilot_name(u).unwrap_or_else(|| u.to_string()))
                        .collect::<Vec<_>>(),
                    "by_ucid": c.by.iter().map(|u| u.to_string()).collect::<Vec<_>>(),
                })
            })
            .collect();
        Ok(serde_json::to_string(&json)?)
    })?;
    Ok(json_response(data))
}

pub(crate) async fn api_aircraft_usage(db: StatsDb, inst: Inst) -> std::result::Result<impl warp::Reply, Error> {
    let data = task::block_in_place(|| -> Result<String> {
        let rounds = db.latest_rounds_for(&inst.id)?;
        let rid = match rounds.iter().find(|(_, _, r)| r.end.is_none()) {
            Some((_, rid, _)) => *rid,
            None => return Ok("[]".to_string()),
        };
        let entries = db.aircraft_usage(rid)?;
        let json: Vec<_> = entries.iter().map(|(vehicle, count, hours)| serde_json::json!({
            "vehicle": vehicle, "sorties": count, "hours": (hours * 10.0).round() / 10.0,
        })).collect();
        Ok(serde_json::to_string(&json)?)
    })?;
    Ok(json_response(data))
}

pub(crate) async fn api_online(db: StatsDb, inst: Inst) -> std::result::Result<impl warp::Reply, Error> {
    let data = task::block_in_place(|| -> Result<String> {
        let rounds = db.latest_rounds_for(&inst.id)?;
        let rid = match rounds.iter().find(|(_, _, r)| r.end.is_none()) {
            Some((_, rid, _)) => *rid,
            None => return Ok("[]".to_string()),
        };
        let pilots = db.connected_pilots(rid)?;
        let entries: Vec<_> = pilots.iter().map(|(ucid, name, side, aircraft)| {
            serde_json::json!({
                "ucid": ucid,
                "name": name,
                "side": format!("{:?}", side),
                "aircraft": aircraft,
            })
        }).collect();
        Ok(serde_json::to_string(&entries)?)
    })?;
    Ok(json_response(data))
}

/// GET /api/units — raw detected-unit dump for the current round.
/// **Admin only**: this is not fogged per requesting coalition. Players get
/// their fogged picture from `/ws/tacmap`.
pub(crate) async fn api_units(
    session_id: Option<Uuid>,
    db: StatsDb,
    inst: Inst,
) -> std::result::Result<impl warp::Reply, Error> {
    require_admin(session_id, db.clone()).await?;
    let data = task::block_in_place(|| -> Result<String> {
        let rounds = db.latest_rounds_for(&inst.id)?;
        let rid = match rounds.iter().find(|(_, _, r)| r.end.is_none()) {
            Some((_, rid, _)) => *rid,
            None => match rounds.first() {
                Some((_, rid, _)) => *rid,
                None => return Ok("[]".to_string()),
            },
        };
        let units = db.detected_units_for_round(rid)?;
        let entries: Vec<_> = units
            .iter()
            .filter(|(_, unit, _)| !unit.tags.contains(UnitTag::Boat))
            .map(|(eid, unit, flags)| {
                let vel = &unit.pos.velocity;
                // DCS world axes: x=north, z=east. Bearing is atan2(east, north).
                let heading = (vel.z.atan2(vel.x).to_degrees() + 360.0) % 360.0;
                let speed_mps = (vel.x * vel.x + vel.y * vel.y + vel.z * vel.z).sqrt();
                let speed_kts = speed_mps * 1.94384;
                let tags: Vec<String> = unit.tags.iter().map(|t| format!("{:?}", t)).collect();
                let detected_by: Vec<String> = flags.iter().map(|d| format!("{:?}", d)).collect();
                serde_json::json!({
                    "id": format!("{}", eid),
                    "owner": format!("{:?}", unit.owner),
                    "typ": unit.typ.to_string(),
                    "tags": tags,
                    "lat": unit.pos.pos.latitude,
                    "lon": unit.pos.pos.longitude,
                    "alt": unit.pos.pos.altitude,
                    "heading": heading,
                    "speed": speed_kts,
                    "detected_by": detected_by,
                })
            })
            .collect();
        Ok(serde_json::to_string(&entries)?)
    })?;
    Ok(json_response(data))
}

// ── Auth handlers ────────────────────────────────────────────────────

/// GET /api/trails  — return recent trail points for the active round.
/// **Admin only** (derived from the raw god's-eye `Export.lua` feed).
pub(crate) async fn api_trails(
    session_id: Option<Uuid>,
    db: StatsDb,
    inst: Inst,
) -> std::result::Result<impl warp::Reply, Error> {
    require_admin(session_id, db.clone()).await?;
    let data = task::block_in_place(|| -> Result<String> {
        let rounds = db.latest_rounds_for(&inst.id)?;
        let rid = match rounds.iter().find(|(_, _, r)| r.end.is_none()) {
            Some((_, rid, _)) => *rid,
            None => match rounds.first() {
                Some((_, rid, _)) => *rid,
                None => return Ok("[]".to_string()),
            },
        };
        let points = db.get_trail_points(rid)?;
        let entries: Vec<_> = points.iter().map(|p| serde_json::json!({
            "id":  p.unit_id,
            "lat": p.lat,
            "lon": p.lon,
            "alt": p.alt,
            "hdg": p.hdg,
            "ts":  p.ts,
        })).collect();
        Ok(serde_json::to_string(&entries)?)
    })?;
    Ok(json_response(data))
}

// ── Session cookie extraction helper ────────────────────────────────

pub(crate) async fn api_unitdb(db: StatsDb, inst: Inst) -> std::result::Result<impl warp::Reply, Error> {
    let snap = task::block_in_place(|| db.unit_db_latest(&inst.id.to_string()))?;
    let body = match snap {
        Some((_, json)) => json,
        None => String::from("{\"dcs_version\":null,\"harvested_at\":null,\"by_type\":{}}"),
    };
    Ok(warp::reply::with_header(
        body,
        "content-type",
        "application/json",
    ))
}


pub(crate) async fn api_unitdb_stale(
    db: StatsDb,
    inst: Inst,
) -> std::result::Result<impl warp::Reply, Error> {
    let stale = task::block_in_place(|| db.unit_db_stale_overrides(&inst.id.to_string()))?;
    Ok(warp::reply::json(&stale))
}


/// SRS's own export (whether read from CLIENT_EXPORT_FILE_PATH or served by
/// its optional HTTP_SERVER feature) uses PascalCase keys at the top level
/// ("Clients", "ServerVersion") -- normalize to what the dashboard expects
/// ("clients"/"version", lowercase) regardless of source.
pub(crate) fn normalize_srs_json(raw: serde_json::Value) -> serde_json::Value {
    let clients = raw
        .get("clients")
        .or_else(|| raw.get("Clients"))
        .cloned()
        .unwrap_or_else(|| serde_json::json!([]));
    let version = raw
        .get("version")
        .or_else(|| raw.get("Version"))
        .or_else(|| raw.get("ServerVersion"))
        .cloned()
        .unwrap_or(serde_json::Value::Null);
    serde_json::json!({"version": version, "clients": clients})
}

pub(crate) async fn api_srs(srs_url: Arc<Option<String>>) -> Response {
    let empty = warp::reply::json(&serde_json::json!({"version": null, "clients": []}));
    let src = match srs_url.as_deref() {
        Some(u) => u,
        None => return empty.into_response(),
    };
    // Simplest and most reliable path: SRS writes its client list straight
    // to a JSON file when CLIENT_EXPORT_ENABLED=True (CLIENT_EXPORT_FILE_PATH
    // in SRS.cfg). No port, no auth, no dependency on SRS's own optional
    // HTTP_SERVER feature (which needs admin elevation to work reliably).
    if !src.starts_with("http://") && !src.starts_with("https://") {
        return match task::block_in_place(|| std::fs::read_to_string(src)) {
            Ok(raw) => match serde_json::from_str::<serde_json::Value>(&raw) {
                Ok(json) => warp::reply::json(&normalize_srs_json(json)).into_response(),
                Err(e) => {
                    log::warn!("api_srs: {src:?} is not valid JSON: {e}");
                    empty.into_response()
                }
            },
            Err(e) => {
                log::warn!("api_srs: could not read {src:?}: {e}");
                empty.into_response()
            }
        };
    }
    // reqwest::get() uses a default client with no timeout -- if the local
    // SRS server is down or hanging (not just refusing the connection
    // outright), this would otherwise block the request indefinitely,
    // right through to Cloudflare's own ~100s edge timeout (a 524) instead
    // of falling back quickly like every other failure mode here already does.
    let client = reqwest::Client::builder()
        .timeout(std::time::Duration::from_secs(3))
        .build();
    let Ok(client) = client else { return empty.into_response() };
    match client.get(src).send().await {
        Ok(resp) => match resp.json::<serde_json::Value>().await {
            Ok(json) => warp::reply::json(&normalize_srs_json(json)).into_response(),
            Err(_)   => empty.into_response(),
        },
        Err(_) => empty.into_response(),
    }
}

// ── Static file serving ─────────────────────────────────────────────

/// `GET /api/instances` -- the DCS servers this bfdb fronts, for the
/// dashboard's instance selector. Public: it is just names and ids, the same
/// information the server browser shows.
///
/// An instance with `public: false` (a test/staging server) is omitted unless
/// the caller is a dashboard admin, so it never shows up in a player's server
/// selector. See `InstanceCfg::public` -- this hides it, it does not lock it.
///
/// Training-range instances (`kind: "range"`) are left out too, since the
/// campaign dashboard has nothing to show for them: `?kind=range` lists only
/// those, `?all=1` lists every kind. Each row carries its `kind`.
pub(crate) async fn api_instances(
    session_id: Option<Uuid>,
    db: StatsDb,
    q: std::collections::HashMap<std::string::String, std::string::String>,
) -> std::result::Result<impl warp::Reply, Error> {
    let is_admin = match session_id {
        Some(id) => task::block_in_place(|| db.get_session(id))
            .ok()
            .flatten()
            .map(|s| s.is_admin)
            .unwrap_or(false),
        None => false,
    };
    let default = db.instances().default_id().to_string();
    let rows: Vec<serde_json::Value> = db
        .instances()
        .all()
        .iter()
        .filter(|cfg| cfg.public || is_admin)
        .filter(|cfg| {
            let all = q.get("all").map_or(false, |v| v == "1" || v == "true");
            match q.get("kind").map(|k| k.as_str()) {
                _ if all => true,
                Some("range") => cfg.is_range(),
                Some("all") => true,
                _ => !cfg.is_range(),
            }
        })
        .map(|cfg| {
            let id: InstanceId = Arc::from(cfg.id.as_str());
            let st = db.state(&id);
            let active = task::block_in_place(|| db.latest_rounds_for(&id))
                .ok()
                .and_then(|rounds| {
                    rounds
                        .into_iter()
                        .find(|(_, _, r)| r.end.is_none())
                        .map(|(scenario, rid, r)| {
                            serde_json::json!({
                                "id": rid.0,
                                "number": db.round_number(rid),
                                "scenario": scenario.to_string(),
                                "start": r.start,
                            })
                        })
                });
            let last_round = task::block_in_place(|| last_round(&db, cfg));
            serde_json::json!({
                "id": cfg.id,
                "label": cfg.label(),
                "default": cfg.id == default,
                // Whether this instance has a live engine we can query at all
                // (vs. a stats-only / offline instance).
                "live": cfg.base.is_some(),
                // The mission currently publishing, if any -- the dashboard
                // greys out a selector entry whose server is down.
                "sortie": st.live_sortie_public(),
                "active_round": active,
                "last_round": last_round,
                // The sides by this server's own names ("NATO", "Georgia"),
                // as configured -- null where they are not, so the dashboard
                // keeps its campaign-wide BLUFOR/REDFOR rather than a name
                // the war diary guessed from the map.
                "blue_label": cfg.blue_faction.as_deref().map(str::trim).filter(|s| !s.is_empty()),
                "red_label": cfg.red_faction.as_deref().map(str::trim).filter(|s| !s.is_empty()),
                "dcs_server_name": cfg.dcs_server_name,
                // false only ever reaches an admin (see the filter above) --
                // the dashboard uses it to mark the entry as internal.
                "public": cfg.public,
                "kind": cfg.kind,
            })
        })
        .collect();
    // "default" names a row of this list, or nothing: on a range-only box the
    // default is the range, which the campaign listing leaves out, and a
    // default the client can't find makes it query that range anyway.
    let default = if rows.iter().any(|r| r["id"] == default.as_str()) {
        serde_json::Value::from(default)
    } else {
        rows.first().map(|r| r["id"].clone()).unwrap_or(serde_json::Value::Null)
    };
    Ok(warp::reply::json(&serde_json::json!({
        "default": default,
        "instances": rows,
    })))
}

/// The instance's most recently finished round and who won it, for the
/// "round over" announcements: the dashboard banner, the site's server cards
/// and the Discord bot all read it from here, so they agree.
///
/// Every round is scanned, not `latest_rounds_for`, which keeps one round per
/// scenario and so drops a finished round as soon as a new round of the same
/// scenario opens. `winner` is "Blue", "Red" or null (a draw, or an admin
/// reset that named nobody); `winner_name` is that side as the war diary
/// calls it, from the round's own last dispatch where there is one.
pub(crate) fn last_round(db: &StatsDb, cfg: &InstanceCfg) -> Option<serde_json::Value> {
    let id: InstanceId = Arc::from(cfg.id.as_str());
    let (scenario, rid, round) = db
        .all_rounds()
        .ok()?
        .into_iter()
        .filter(|(_, rid, r)| r.end.is_some() && db.round_instance_of(*rid) == id)
        .max_by_key(|(_, _, r)| r.end)?;
    let winner = round.winner.map(|s| format!("{s:?}"));
    let factions = db
        .news_history(rid, 1)
        .ok()
        .and_then(|mut d| d.pop())
        .map(|d| d.factions)
        .unwrap_or_else(|| {
            news::Factions::new(
                cfg.blue_faction.as_deref(),
                cfg.red_faction.as_deref(),
                cfg.blue_adjective.as_deref(),
                cfg.red_adjective.as_deref(),
            )
        });
    Some(serde_json::json!({
        "id": rid.0,
        "number": db.round_number(rid),
        "scenario": scenario.to_string(),
        "start": round.start,
        "end": round.end,
        "winner": winner,
        "winner_name": winner.as_deref().map(|w| factions.name(w).to_string()),
        "loser_name": winner.as_deref().map(|w| {
            factions.name(if w == "Blue" { "Red" } else { "Blue" }).to_string()
        }),
    }))
}
