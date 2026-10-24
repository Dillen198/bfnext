//! `/ws/groundwar`: the ground war, live, for the viewer's own coalition.
//!
//! The engine builds the picture for one side at a time (`query-ground-war`,
//! fog of war included), so this polls it once per side every couple of
//! seconds -- only while someone is watching that side -- and fans the
//! result out to every viewer on it. Per viewer it then marks which of the
//! side's players is the viewer (`is_self`) and strips every player's ucid,
//! which the engine only sends so that this can be done.
//!
//! The side comes from the session exactly as for `/ws/tacmap`: a pilot's
//! own coalition, or for a dashboard admin with none, `?side=` (Blue by
//! default) as a view-only god mode.

use crate::{db::StatsDb, resolve_ucid_via_bot, websec, BotLinkConfig, Inst};
use dcso3::coalition::Side;
use futures::{SinkExt, StreamExt};
use netidx::publisher::Value;
use std::{
    sync::Arc,
    time::{Duration, Instant},
};
use tokio::task;
use uuid::Uuid;
use warp::{
    reply::{Reply, Response},
    ws::{Message, WebSocket},
};

/// How often each watched side is polled, and how often viewers get a frame.
const PERIOD: Duration = Duration::from_millis(2000);
/// A picture older than this is no picture: the engine has stopped answering.
const STALE: Duration = Duration::from_secs(20);

struct Fresh {
    at: Instant,
    /// The picture, or why there isn't one ("disabled", "unavailable").
    picture: Result<serde_json::Value, &'static str>,
}

#[derive(Default)]
pub(crate) struct GroundCache {
    blue: Option<Fresh>,
    red: Option<Fresh>,
    watching_blue: usize,
    watching_red: usize,
}

impl GroundCache {
    fn get(&self, side: Side) -> Option<&Fresh> {
        match side {
            Side::Red => self.red.as_ref(),
            _ => self.blue.as_ref(),
        }
    }

    fn watching(&mut self, side: Side) -> &mut usize {
        match side {
            Side::Red => &mut self.watching_red,
            _ => &mut self.watching_blue,
        }
    }
}

pub(crate) type GroundState = Arc<tokio::sync::RwLock<GroundCache>>;

async fn fetch(db: &StatsDb, inst: &Inst, side: &str) -> Result<serde_json::Value, &'static str> {
    let call = db.call_engine_rpc_optional(inst, "query-ground-war", vec![("side", Value::from(side.to_string()))]);
    match tokio::time::timeout(Duration::from_secs(8), call).await {
        Ok(Ok(Value::String(s))) => match serde_json::from_str::<serde_json::Value>(&s) {
            Ok(v) if v.get("enabled").and_then(|e| e.as_bool()) == Some(false) => Err("disabled"),
            Ok(v) => Ok(v),
            Err(e) => {
                log::warn!("[{}] ground feed: unparseable {side} picture: {e}", inst.id);
                Err("unavailable")
            }
        },
        Ok(Ok(_)) | Ok(Err(_)) | Err(_) => Err("unavailable"),
    }
}

/// Background task, one per campaign instance: keep the watched sides'
/// pictures fresh.
pub(crate) async fn poller(db: StatsDb, inst: Inst, state: GroundState) {
    let mut tick = tokio::time::interval(PERIOD);
    tick.set_missed_tick_behavior(tokio::time::MissedTickBehavior::Delay);
    loop {
        tick.tick().await;
        let (blue, red) = {
            let c = state.read().await;
            (c.watching_blue > 0, c.watching_red > 0)
        };
        for (side, watched) in [(Side::Blue, blue), (Side::Red, red)] {
            if !watched {
                continue;
            }
            let name = if side == Side::Red { "red" } else { "blue" };
            let picture = fetch(&db, &inst, name).await;
            let fresh = Fresh { at: Instant::now(), picture };
            let mut c = state.write().await;
            match side {
                Side::Red => c.red = Some(fresh),
                _ => c.blue = Some(fresh),
            }
        }
    }
}

/// Who is watching: their side, their ucid (to find them among the side's
/// players) and whether it is an admin's view-only god mode.
struct Viewer {
    side: Side,
    ucid: Option<String>,
    god: bool,
    /// May give orders (`crate::command`), as of when the viewer was resolved.
    commander: bool,
}

async fn viewer(
    session_id: Option<Uuid>,
    query: &std::collections::HashMap<String, String>,
    db: &StatsDb,
    bot_cfg: &Arc<Option<BotLinkConfig>>,
    inst: &Inst,
) -> Result<Viewer, &'static str> {
    let id = session_id.ok_or("login")?;
    let session = task::block_in_place(|| db.get_session(id)).ok().flatten().ok_or("login")?;
    let ucid = resolve_ucid_via_bot(bot_cfg, &session.discord_id).await;
    let own = match &ucid {
        Some(u) => task::block_in_place(|| db.pilot_current_side(&inst.id, u)).ok().flatten(),
        None => None,
    };
    match own {
        Some(side) => {
            let Some(u) = ucid else { return Err("nocoalition") };
            let commander = if session.is_admin {
                crate::command::note_admin(u);
                true
            } else {
                task::block_in_place(|| crate::command::may_command(db, inst, &u, side)).unwrap_or(false)
            };
            Ok(Viewer { side, ucid: Some(u.to_string()), god: false, commander })
        }
        None if session.is_admin => {
            let side = match query.get("side").map(|s| s.to_ascii_lowercase()) {
                Some(s) if s == "red" => Side::Red,
                _ => Side::Blue,
            };
            Ok(Viewer { side, ucid: None, god: true, commander: false })
        }
        None => Err("nocoalition"),
    }
}

/// This viewer's frame from the cached picture.
fn frame(cache: &GroundCache, v: &Viewer) -> (serde_json::Value, Option<Instant>) {
    let Some(f) = cache.get(v.side) else {
        return (serde_json::json!({ "reason": "unavailable" }), None);
    };
    if f.at.elapsed() > STALE {
        return (serde_json::json!({ "reason": "unavailable" }), Some(f.at));
    }
    match &f.picture {
        Err(r) => (serde_json::json!({ "reason": r }), Some(f.at)),
        Ok(p) => {
            let mut p = p.clone();
            if let Some(players) = p.get_mut("players").and_then(|x| x.as_array_mut()) {
                for pl in players.iter_mut() {
                    if let Some(o) = pl.as_object_mut() {
                        let me = match (o.get("ucid").and_then(|u| u.as_str()), v.ucid.as_deref()) {
                            (Some(a), Some(b)) => a == b,
                            _ => false,
                        };
                        o.insert("is_self".into(), serde_json::json!(me));
                        o.remove("ucid");
                    }
                }
            }
            if let Some(o) = p.as_object_mut() {
                o.insert("can_command".into(), serde_json::json!(!v.god && v.ucid.is_some() && v.commander));
                o.insert("god_mode".into(), serde_json::json!(v.god));
            }
            (serde_json::json!({ "picture": p }), Some(f.at))
        }
    }
}

#[allow(clippy::too_many_arguments)]
pub(crate) async fn handler(
    ws: warp::ws::Ws,
    ip: std::net::IpAddr,
    session_id: Option<Uuid>,
    query: std::collections::HashMap<String, String>,
    db: StatsDb,
    bot_cfg: Arc<Option<BotLinkConfig>>,
    inst: Inst,
    state: GroundState,
) -> Response {
    let Some(slot) = websec::ws_slot(ip) else {
        return websec::ws_refused(warp::http::StatusCode::TOO_MANY_REQUESTS, "too many open connections");
    };
    let v = viewer(session_id, &query, &db, &bot_cfg, &inst).await;
    ws.on_upgrade(move |socket| async move {
        let _slot = slot;
        run(socket, v, state, session_id, query, db, bot_cfg, inst).await
    })
    .into_response()
}

#[allow(clippy::too_many_arguments)]
async fn run(
    ws: WebSocket,
    v: Result<Viewer, &'static str>,
    state: GroundState,
    session_id: Option<Uuid>,
    query: std::collections::HashMap<String, String>,
    db: StatsDb,
    bot_cfg: Arc<Option<BotLinkConfig>>,
    inst: Inst,
) {
    const RECHECK: Duration = Duration::from_secs(60);
    const IDLE: Duration = Duration::from_secs(90);
    let (mut sink, mut stream) = ws.split();
    let mut v = match v {
        Ok(v) => v,
        Err(reason) => {
            let _ = sink.send(Message::text(serde_json::json!({ "reason": reason }).to_string())).await;
            let _ = sink.send(Message::close()).await;
            return;
        }
    };
    *state.write().await.watching(v.side) += 1;
    // Don't make the first viewer wait a whole period for the first poll.
    let mut first = true;
    let mut last_sent: Option<Instant> = None;
    let mut last_check = Instant::now();
    let mut last_heard = Instant::now();
    let mut round = task::block_in_place(|| db.active_round_id(&inst.id)).ok().flatten();
    let mut tick = tokio::time::interval(Duration::from_millis(500));
    tick.set_missed_tick_behavior(tokio::time::MissedTickBehavior::Delay);
    let mut ticks: u32 = 0;
    loop {
        tokio::select! {
            _ = tick.tick() => {
                ticks = ticks.wrapping_add(1);
                if last_heard.elapsed() > IDLE {
                    break;
                }
                if ticks % 60 == 0 && sink.send(Message::ping(Vec::new())).await.is_err() {
                    break;
                }
                if last_check.elapsed() >= RECHECK {
                    last_check = Instant::now();
                    let now_round = task::block_in_place(|| db.active_round_id(&inst.id)).ok().flatten();
                    if now_round != round {
                        round = now_round;
                        match viewer(session_id, &query, &db, &bot_cfg, &inst).await {
                            Ok(nv) => {
                                if nv.side != v.side {
                                    let mut c = state.write().await;
                                    let w = c.watching(v.side);
                                    *w = w.saturating_sub(1);
                                    *c.watching(nv.side) += 1;
                                }
                                v = nv;
                                last_sent = None;
                            }
                            Err(reason) => {
                                let _ = sink.send(Message::text(serde_json::json!({ "reason": reason }).to_string())).await;
                                break;
                            }
                        }
                    }
                }
                let (json, at) = {
                    let c = state.read().await;
                    frame(&c, &v)
                };
                // Only a new picture is worth sending; the first frame goes
                // out whatever it is, so the page knows where it stands.
                if !first && (at.is_none() || at == last_sent) {
                    continue;
                }
                if first && at.is_none() && ticks < 8 {
                    // Give the poller a moment to fetch the first picture.
                    continue;
                }
                first = false;
                last_sent = at;
                if sink.send(Message::text(json.to_string())).await.is_err() {
                    break;
                }
            }
            msg = stream.next() => match msg {
                Some(Ok(m)) if m.is_close() => break,
                Some(Ok(_)) => last_heard = Instant::now(),
                None | Some(Err(_)) => break,
            }
        }
    }
    let mut c = state.write().await;
    let w = c.watching(v.side);
    *w = w.saturating_sub(1);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn a_viewer_sees_themselves_and_no_ucids() {
        let mut c = GroundCache::default();
        c.blue = Some(Fresh {
            at: Instant::now(),
            picture: Ok(serde_json::json!({
                "side": "Blue", "enabled": true,
                "players": [{"name": "a", "ucid": "u1"}, {"name": "b", "ucid": "u2"}]
            })),
        });
        let v = Viewer { side: Side::Blue, ucid: Some("u2".into()), god: false, commander: true };
        let (f, at) = frame(&c, &v);
        assert!(at.is_some());
        let players = f["picture"]["players"].as_array().unwrap();
        assert_eq!(players[0]["is_self"], false);
        assert_eq!(players[1]["is_self"], true);
        assert!(players.iter().all(|p| p.get("ucid").is_none()));
        assert_eq!(f["picture"]["can_command"], true);
        // A pilot on the side who isn't a commander watches.
        let w = Viewer { side: Side::Blue, ucid: Some("u1".into()), god: false, commander: false };
        assert_eq!(frame(&c, &w).0["picture"]["can_command"], false);
        // The other side has nothing yet.
        let r = Viewer { side: Side::Red, ucid: None, god: true, commander: false };
        assert_eq!(frame(&c, &r).0["reason"], "unavailable");
    }
}
