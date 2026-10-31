//! Pilot pages, progression, kills and squadrons.

use crate::*;

/// GET /api/pilots — all pilot UCIDs + names (all-time, for name resolution)
///
/// A walk of every pilot ever seen, polled by every dashboard: shared, and
/// refreshed at most once a minute (a new pilot's name shows up within one).
pub(crate) async fn api_all_pilots(
    db: StatsDb,
    q: Q,
) -> std::result::Result<impl warp::Reply, Error> {
    const PILOTS_TTL: std::time::Duration = std::time::Duration::from_secs(60);
    static PILOTS: std::sync::LazyLock<Arc<websec::Cached<Arc<Vec<serde_json::Value>>>>> =
        std::sync::LazyLock::new(|| Arc::new(websec::Cached::new()));
    let all = PILOTS
        .clone()
        .get_swr(
            PILOTS_TTL,
            PILOTS_TTL * 10,
            move || -> Result<Arc<Vec<serde_json::Value>>> {
                let pilots = db.all_pilot_names()?;
                Ok(Arc::new(pilots.iter().map(|(ucid, name)| {
                serde_json::json!({ "ucid": ucid.to_string(), "name": name.to_string() })
            }).collect()))
            },
        )
        .await
        .map_err(Error)?;
    let total = all.len();
    let num = |k: &str| q.get(k).and_then(|s| s.trim().parse::<usize>().ok());
    let offset = num("offset").unwrap_or(0).min(total);
    let limit = num("limit").unwrap_or(PILOT_NAMES_CAP).min(PILOT_NAMES_CAP);
    let rows = &all[offset..(offset + limit).min(total)];
    Ok(paged_response((serde_json::to_string(rows).map_err(anyhow::Error::from)?, total)))
}

pub(crate) async fn api_pilot(
    ucid: String,
    db: StatsDb,
) -> std::result::Result<impl warp::Reply, Error> {
    let data = cached_json(format!("pilot:{ucid}"), move || -> Result<String> {
        let ucid: dcso3::net::Ucid =
            ucid.parse().map_err(|_| websec::bad_request("not a valid ucid"))?;
        match db.pilot_detail(&ucid)? {
            None => Ok(serde_json::json!({ "error": "not found" }).to_string()),
            Some((name, agg)) => Ok(serde_json::to_string(&serde_json::json!({
                "ucid": ucid.to_string(),
                "name": name.to_string(),
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
            }))?),
        }
    })
    .await?;
    Ok(json_response(data))
}

/// Which rounds the instance-agnostic pilot routes may list: every round
/// but those of a non-public instance or a range -- the same rule the
/// lifetime totals, XP and leaderboards follow (`counts_toward_totals`).
/// These routes take no `?instance=`, so they could not be locked like the
/// per-instance ones.
pub(crate) fn shown_round(db: &StatsDb) -> Result<impl Fn(db::RoundId) -> bool> {
    let public = db.public_rounds()?;
    Ok(move |r| public.as_ref().is_none_or(|s| s.contains(&r)))
}

/// GET /api/pilot/:ucid/sorties — all sorties for a pilot
pub(crate) async fn api_pilot_sorties(
    ucid: String,
    db: StatsDb,
    q: Q,
) -> std::result::Result<impl warp::Reply, Error> {
    let rows = cached_rows(format!("pilot_sorties:{ucid}"), move || -> Result<Vec<serde_json::Value>> {
        let ucid: dcso3::net::Ucid = ucid.parse().map_err(|_| websec::bad_request("not a valid ucid"))?;
        // Summary rows, newest first. The per-sortie event lists are left to
        // /api/pilot/:ucid/sortie/:id so a long career stays a small payload.
        let shown = shown_round(&db)?;
        let sorties = db.pilot_sortie_stats(&ucid)?;
        let entries: Vec<_> = sorties.iter().rev().filter(|s| shown(s.round_id)).map(|s| {
            serde_json::json!({
                "round_id": s.round_id.0,
                "sortie_id": s.sortie_id.0,
                "aircraft": s.aircraft,
                "takeoff": s.takeoff.to_rfc3339(),
                "land": s.land.map(|l| l.to_rfc3339()),
                "duration_secs": s.duration_secs,
                "landed": s.landed,
                "outcome": s.outcome,
                "air_kills": s.air_kills,
                "ground_kills": s.ground_kills,
                "shots": s.shots,
                "hits": s.hits,
                "deploys": s.deploys.len(),
                // Who ended it, for the pilot page's nemesis.
                "killed_by_ucid": s.death.as_ref().and_then(|d| d.killer_ucid.clone()),
                "killed_by": s.death.as_ref().and_then(|d| d.killer_name.clone().or_else(|| d.killer_type.clone())),
            })
        }).collect();
        Ok(entries)
    })
    .await?;
    let data = page_slice(&rows, &q, PILOT_LIST_CAP)?;
    Ok(paged_response(data))
}

/// GET /api/pilot/:ucid/sortie/:sortie_id — one sortie with its kills,
/// weapons, death and deploys
pub(crate) async fn api_pilot_sortie(
    ucid: String,
    sortie_id: u64,
    db: StatsDb,
) -> std::result::Result<impl warp::Reply, Error> {
    let data = cached_json(
        format!("pilot_sortie:{ucid}:{sortie_id}"),
        move || -> Result<String> {
            let ucid: dcso3::net::Ucid =
                ucid.parse().map_err(|_| websec::bad_request("not a valid ucid"))?;
            let shown = shown_round(&db)?;
            let sortie = db
                .pilot_sortie_stats(&ucid)?
                .into_iter()
                .find(|s| s.sortie_id.0 == sortie_id && shown(s.round_id))
                .ok_or_else(|| websec::not_found("no such sortie"))?;
            Ok(serde_json::to_string(&sortie)?)
        },
    )
    .await?;
    Ok(json_response(data))
}

/// GET /api/pilot/:ucid/breakdown — per-round aggregates for a pilot
pub(crate) async fn api_pilot_breakdown(
    ucid: String,
    db: StatsDb,
) -> std::result::Result<impl warp::Reply, Error> {
    let data =
        cached_json(format!("pilot_breakdown:{ucid}"), move || -> Result<String> {
            let ucid: dcso3::net::Ucid =
                ucid.parse().map_err(|_| websec::bad_request("not a valid ucid"))?;
            let shown = shown_round(&db)?;
            let rounds = db.pilot_round_breakdown(&ucid)?;
            let entries: Vec<_> = rounds
                .iter()
                .filter(|(_, rid, _)| shown(*rid))
                .map(|(scenario, rid, agg)| {
                    serde_json::json!({
                        "round_id": rid.0,
                        "scenario": scenario,
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
                    })
                })
                .collect();
            Ok(serde_json::to_string(&entries)?)
        })
        .await?;
    Ok(json_response(data))
}

/// GET /api/pilot/:ucid/progression -- XP, level, today's and this week's
/// challenges, and achievement tiers (`progression.rs`).
pub(crate) async fn api_pilot_progression(
    ucid: String,
    db: StatsDb,
) -> std::result::Result<impl warp::Reply, Error> {
    let data =
        cached_json(format!("pilot_progression:{ucid}"), move || -> Result<String> {
            let ucid: dcso3::net::Ucid =
                ucid.parse().map_err(|_| websec::bad_request("not a valid ucid"))?;
            Ok(serde_json::to_string(&db.pilot_progression(&ucid)?)?)
        })
        .await?;
    Ok(json_response(data))
}

pub(crate) fn squadron_json(
    db: &StatsDb,
    s: &squadrons::Squadron,
    members: bool,
) -> serde_json::Value {
    let mut v = serde_json::json!({
        "tag": s.tag,
        "name": s.name,
        "owner": s.owner,
        "owner_name": db.pilot_name(&s.owner).unwrap_or_default(),
        "created": s.created.to_rfc3339(),
        "member_count": s.members.len(),
        "xp": s.xp,
    });
    if members {
        let mut m: Vec<_> = s
            .members
            .iter()
            .map(|m| {
                serde_json::json!({
                    "ucid": m.ucid,
                    "name": db.pilot_name(&m.ucid).unwrap_or_default(),
                    "joined": m.joined.to_rfc3339(),
                    "xp": m.xp,
                })
            })
            .collect();
        m.sort_by(|a, b| b["xp"].as_u64().cmp(&a["xp"].as_u64()));
        v["members"] = serde_json::Value::Array(m);
    }
    v
}

/// GET /api/squadrons -- every squadron, by XP served.
pub(crate) async fn api_squadrons(
    db: StatsDb,
    q: Q,
) -> std::result::Result<impl warp::Reply, Error> {
    let data = blocking(move || -> Result<(String, usize)> {
        let v: Vec<_> =
            db.squadrons().all()?.iter().map(|s| squadron_json(&db, s, false)).collect();
        page_json(v, &q, SQUADRON_LIST_CAP)
    })
    .await?;
    Ok(paged_response(data))
}

/// GET /api/squadrons/{tag} -- one squadron with its members.
pub(crate) async fn api_squadron(
    tag: String,
    db: StatsDb,
) -> std::result::Result<impl warp::Reply, Error> {
    let data = blocking(move || -> Result<String> {
        let s = db
            .squadrons()
            .get(&tag)
            .map_err(|e| websec::bad_request(e.to_string()))?
            .ok_or_else(|| websec::not_found(format!("no squadron {tag}")))?;
        Ok(serde_json::to_string(&squadron_json(&db, &s, true))?)
    })
    .await?;
    Ok(json_response(data))
}

#[derive(Deserialize)]
pub(crate) struct SquadronCreateBody {
    pub(crate) tag: String,
    pub(crate) name: String,
}

#[derive(Deserialize)]
pub(crate) struct SquadronKickBody {
    pub(crate) ucid: String,
}

/// POST /api/squadrons {tag, name} -- the signed-in pilot founds a squadron.
pub(crate) async fn api_squadron_create(
    session_id: Option<Uuid>,
    body: SquadronCreateBody,
    db: StatsDb,
    bot_cfg: Arc<Option<BotLinkConfig>>,
) -> std::result::Result<impl warp::Reply, Error> {
    let ucid = session_player(session_id, &db, &bot_cfg).await?;
    let data = task::block_in_place(|| -> Result<String> {
        let s = db
            .squadrons()
            .create(ucid, &body.tag, &body.name)
            .map_err(|e| websec::bad_request(e.to_string()))?;
        Ok(serde_json::to_string(&squadron_json(&db, &s, true))?)
    })?;
    Ok(json_response(data))
}

/// POST /api/squadrons/{tag}/join -- the signed-in pilot joins.
pub(crate) async fn api_squadron_join(
    tag: String,
    session_id: Option<Uuid>,
    db: StatsDb,
    bot_cfg: Arc<Option<BotLinkConfig>>,
) -> std::result::Result<impl warp::Reply, Error> {
    let ucid = session_player(session_id, &db, &bot_cfg).await?;
    task::block_in_place(|| db.squadrons().join(ucid, &tag))
        .map_err(|e| websec::bad_request(e.to_string()))?;
    Ok(json_response(r#"{"ok":true}"#.into()))
}

/// POST /api/squadrons/leave -- the signed-in pilot leaves their squadron.
pub(crate) async fn api_squadron_leave(
    session_id: Option<Uuid>,
    db: StatsDb,
    bot_cfg: Arc<Option<BotLinkConfig>>,
) -> std::result::Result<impl warp::Reply, Error> {
    let ucid = session_player(session_id, &db, &bot_cfg).await?;
    task::block_in_place(|| db.squadrons().leave(ucid, None))
        .map_err(|e| websec::bad_request(e.to_string()))?;
    Ok(json_response(r#"{"ok":true}"#.into()))
}

/// POST /api/squadrons/kick {ucid} -- the owner removes a member.
pub(crate) async fn api_squadron_kick(
    session_id: Option<Uuid>,
    body: SquadronKickBody,
    db: StatsDb,
    bot_cfg: Arc<Option<BotLinkConfig>>,
) -> std::result::Result<impl warp::Reply, Error> {
    let by = session_player(session_id, &db, &bot_cfg).await?;
    let target: dcso3::net::Ucid =
        body.ucid.parse().map_err(|_| websec::bad_request("not a valid ucid"))?;
    task::block_in_place(|| db.squadrons().leave(target, Some(by)))
        .map_err(|e| websec::bad_request(e.to_string()))?;
    Ok(json_response(r#"{"ok":true}"#.into()))
}

/// GET /api/pilot/:ucid/card.png -- the shareable pilot card.
pub(crate) async fn api_pilot_card(
    ucid: String,
    db: StatsDb,
) -> std::result::Result<Response, Error> {
    let ucid: dcso3::net::Ucid =
        ucid.parse().map_err(|_| websec::bad_request("not a valid ucid"))?;
    let png = task::block_in_place(|| db.pilot_card_png(&ucid))?
        .ok_or_else(|| websec::not_found("no such pilot"))?;
    let mut r = Response::new(png.into());
    let h = r.headers_mut();
    h.insert(
        warp::http::header::CONTENT_TYPE,
        warp::http::HeaderValue::from_static("image/png"),
    );
    h.insert(
        warp::http::header::CACHE_CONTROL,
        warp::http::HeaderValue::from_static("public, max-age=300"),
    );
    Ok(r)
}

/// GET /api/progression/top -- the 50 pilots with the most XP.
pub(crate) async fn api_xp_leaderboard(
    db: StatsDb,
) -> std::result::Result<impl warp::Reply, Error> {
    let data = cached_json("xp_leaderboard".into(), move || -> Result<String> {
        let rows: Vec<_> = db
            .xp_leaderboard(50)?
            .into_iter()
            .map(|(ucid, name, xp, level)| {
                serde_json::json!({
                    "ucid": ucid, "name": name, "xp": xp, "level": level,
                })
            })
            .collect();
        Ok(serde_json::to_string(&rows)?)
    })
    .await?;
    Ok(json_response(data))
}

/// GET /api/pilot/:ucid/kills — all kills made by a pilot
pub(crate) async fn api_pilot_kills(
    ucid: String,
    db: StatsDb,
    q: Q,
) -> std::result::Result<impl warp::Reply, Error> {
    let rows = cached_rows(
        format!("pilot_kills:{ucid}"),
        move || -> Result<Vec<serde_json::Value>> {
            let ucid: dcso3::net::Ucid =
                ucid.parse().map_err(|_| websec::bad_request("not a valid ucid"))?;
            let shown = shown_round(&db)?;
            let kills = db.pilot_kills_for(&ucid)?;
            let entries: Vec<_> = kills
                .iter()
                .filter(|(rid, _)| shown(*rid))
                .map(|(round_id, dead)| {
                    // `dead.shots` holds every shot that ever landed on this victim,
                    // possibly from several shooters across a long engagement. This is
                    // *this* pilot's kill list, so pick this pilot's own shot for the
                    // weapon/airframe columns -- `.find(|s| s.hit)` alone would show
                    // whoever's shot happens to sort first, i.e. the wrong aircraft.
                    let mine =
                        |s: &&bfprotocols::shots::Shot| s.shooter.ucid() == Some(&ucid);
                    let shot = dead
                        .shots
                        .iter()
                        .find(|s| s.hit && mine(s))
                        .or_else(|| dead.shots.iter().find(|s| mine(s)))
                        .or_else(|| dead.shots.iter().find(|s| s.hit))
                        .or_else(|| dead.shots.first());
                    let weapon =
                        shot.and_then(|s| display_weapon(s.weapon_name.as_ref()));
                    let airframe = shot
                        .and_then(|s| s.shooter_typ.as_deref().map(|t| t.to_string()));
                    let target_type = shot.map(|s| s.target_typ.to_string());
                    let victim_ucid = dead.victim.ucid().map(|u| u.to_string());
                    let victim_side = format!("{:?}", dead.victim.side());
                    serde_json::json!({
                        "round_id": round_id.0,
                        "time": dead.time.to_rfc3339(),
                        "victim_ucid": victim_ucid,
                        "victim_side": victim_side,
                        "target_type": target_type,
                        "weapon": weapon,
                        "killer_airframe": airframe,
                    })
                })
                .collect();
            Ok(entries)
        },
    )
    .await?;
    let data = page_slice(&rows, &q, PILOT_LIST_CAP)?;
    Ok(paged_response(data))
}

/// GET /api/pilot/:ucid/deploys — all deployable-crate deploys done by a pilot
pub(crate) async fn api_pilot_deploys(
    ucid: String,
    db: StatsDb,
    q: Q,
) -> std::result::Result<impl warp::Reply, Error> {
    let rows = cached_rows(
        format!("pilot_deploys:{ucid}"),
        move || -> Result<Vec<serde_json::Value>> {
            let ucid: dcso3::net::Ucid =
                ucid.parse().map_err(|_| websec::bad_request("not a valid ucid"))?;
            let shown = shown_round(&db)?;
            let deploys = db.pilot_deploys_for(&ucid)?;
            let entries: Vec<_> = deploys
                .iter()
                .filter(|(rid, _)| shown(*rid))
                .map(|(round_id, rec)| {
                    serde_json::json!({
                        "round_id": round_id.0,
                        "time": rec.time.to_rfc3339(),
                        "deployable": rec.deployable,
                        "aircraft": rec.aircraft,
                        "method": rec.method,
                    })
                })
                .collect();
            Ok(entries)
        },
    )
    .await?;
    let data = page_slice(&rows, &q, PILOT_LIST_CAP)?;
    Ok(paged_response(data))
}
