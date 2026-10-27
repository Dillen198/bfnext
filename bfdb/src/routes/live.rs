//! Live feeds: the DCS export listener, /ws/units, the tacmap and GCI transmit.

use crate::*;

#[derive(Deserialize)]
pub(crate) struct GciTransmitBody {
    pub(crate) text: std::string::String,
}

/// POST /api/gci/transmit {text} -- the human GCI seat: an admin or a side's
/// commander types a call on the Map page and the GCI voice speaks it on
/// that side's frequency over SRS.
pub(crate) async fn api_gci_transmit(
    query: std::collections::HashMap<std::string::String, std::string::String>,
    session_id: Option<Uuid>,
    body: GciTransmitBody,
    db: StatsDb,
    bot_cfg: Arc<Option<BotLinkConfig>>,
    inst: Inst,
) -> std::result::Result<impl warp::Reply, Error> {
    let c = require_coalition(&query, session_id, &db, &bot_cfg, &inst).await?;
    let allowed = c.god_mode
        || c.session.is_admin
        || c.ucid
            .as_ref()
            .map(|u| task::block_in_place(|| command::is_commander(&db, &inst, u, c.side)).unwrap_or(false))
            .unwrap_or(false);
    if !allowed {
        return Err(websec::forbidden("only your side's commanders and admins can take the GCI seat").into());
    }
    let text = body.text.trim();
    if text.is_empty() || text.chars().count() > 200 {
        return Err(websec::bad_request("a call is 1 to 200 characters").into());
    }
    gci::human_call(&inst.id, c.side, text).map_err(|e| websec::bad_request(e))?;
    log::info!("[{}] human GCI ({}, {:?}): {text}", inst.id, c.session.username, c.side);
    Ok(json_response(r#"{"ok":true}"#.into()))
}

/// A single unit record from the DCS Export.lua UDP feed.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub(crate) struct LiveUnit {
    pub(crate) id:   String,
    pub(crate) nm:   String,
    pub(crate) typ:  String,
    /// 1=Plane 2=Helo 3=Ground 4=Ship
    pub(crate) cat:  u8,
    /// 1=Red 2=Blue
    pub(crate) coa:  u8,
    pub(crate) lat:  f64,
    pub(crate) lon:  f64,
    pub(crate) alt:  f64,
    pub(crate) hdg:  f64,
    pub(crate) spd:  f64,
    /// Vertical speed m/s (positive = climbing), optional
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub(crate) vspd: Option<f64>,
    /// Occupying player's name, if this unit is player-flown (absent for AI)
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub(crate) pilot: Option<String>,
}

/// Bullseye reference point for one coalition.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub(crate) struct Bullseye {
    /// 1=Red 2=Blue
    pub(crate) side: u8,
    pub(crate) lat:  f64,
    pub(crate) lon:  f64,
}

/// One UDP message from Export.lua.
#[allow(dead_code)]
#[derive(Debug, Deserialize)]
pub(crate) struct ExportMsg {
    /// DCS model time
    pub(crate) t:    f64,
    /// Batch sequence index within this tick
    pub(crate) seq:  u32,
    /// True if this is the last batch for this tick
    pub(crate) last: bool,
    /// Total unit count for this tick (across all batches)
    pub(crate) n:    u32,
    /// Units in this batch
    pub(crate) u:    Vec<LiveUnit>,
    /// Bullseye points (only present on the last batch)
    #[serde(default)]
    pub(crate) bull: Vec<Bullseye>,
    /// Shared secret (`BF_SECRET` in Export.lua), checked when bfdb runs with
    /// --export-secret.
    #[serde(default)]
    pub(crate) k: Option<String>,
}

/// Where and how the Export.lua listeners accept packets.
#[derive(Clone)]
pub(crate) struct ExportListenCfg {
    pub(crate) bind: std::net::IpAddr,
    pub(crate) secret: Option<Arc<String>>,
}

/// Message broadcast to all WebSocket clients.
#[derive(Debug, Clone, Serialize)]
pub(crate) struct WsUnitsMsg<'a> {
    pub(crate) t:     f64,
    pub(crate) units: &'a [LiveUnit],
    pub(crate) bull:  &'a [Bullseye],
}

pub(crate) type LiveState = Arc<tokio::sync::RwLock<(f64, Vec<LiveUnit>, Vec<Bullseye>)>>;

/// Background task: listens on UDP 42001, accumulates batches, and
/// broadcasts the full unit list to all WebSocket clients each tick.
/// Also samples unit positions every ~10s into the trail_points DB.
pub(crate) async fn udp_export_listener(
    state: LiveState,
    tx: broadcast::Sender<String>,
    db: StatsDb,
    inst: Inst,
    port: u16,
    listen: ExportListenCfg,
) {
    // Units per tick is a few hundred on a busy server; a sender that never
    // says `last` must not grow this without bound.
    const MAX_PENDING_UNITS: usize = 20_000;
    // One trail sample per unit this often.
    const TRAIL_EVERY: std::time::Duration = std::time::Duration::from_secs(10);
    let sock = match tokio::net::UdpSocket::bind((listen.bind, port)).await {
        Ok(s) => s,
        Err(e) => {
            log::error!(
                "[{}] failed to bind UDP {port} for DCS export: {e} -- is another instance \
                 (or another bfdb) already using this port? Each instance needs its own \
                 export_port, matching BF_PORT in its Export.lua",
                inst.id
            );
            return;
        }
    };
    log::info!(
        "[{}] DCS export listener on UDP {}:{port}{}",
        inst.id,
        listen.bind,
        if listen.secret.is_some() { " (secret required)" } else { "" }
    );
    if !listen.bind.is_loopback() && listen.secret.is_none() {
        log::warn!(
            "[{}] the export listener accepts unauthenticated packets from the network -- \
             set --export-secret (and BF_SECRET in Export.lua)",
            inst.id
        );
    }

    let mut buf = vec![0u8; 65536];
    // Accumulate batches for one tick before broadcasting
    let mut pending:      Vec<LiveUnit> = Vec::new();
    let mut pending_bull: Vec<Bullseye> = Vec::new();
    let mut pending_t:    f64;
    let mut last_trail: Option<std::time::Instant> = None;

    loop {
        let len = match sock.recv(&mut buf).await {
            Ok(n) => n,
            Err(_) => continue,
        };
        // Strip trailing newline/whitespace
        let slice = buf[..len].iter().rposition(|&b| b > b' ')
            .map(|i| &buf[..=i]).unwrap_or(&buf[..len]);
        let msg: ExportMsg = match serde_json::from_slice(slice) {
            Ok(m) => m,
            Err(_) => continue,
        };

        if let Some(want) = &listen.secret {
            if !msg.k.as_deref().map_or(false, |k| websec::ct_eq(k, want)) {
                continue;
            }
        }
        pending_t = msg.t;
        if pending.len().saturating_add(msg.u.len()) > MAX_PENDING_UNITS {
            // A tick that never ends: start over rather than grow forever.
            pending.clear();
        }
        pending.extend(msg.u);
        if !msg.bull.is_empty() {
            pending_bull = msg.bull;
        }

        if msg.last {
            // Full tick received — update state and broadcast
            let json = {
                let mut w = state.write().await;
                *w = (pending_t, std::mem::take(&mut pending), std::mem::take(&mut pending_bull));
                let broadcast_msg = WsUnitsMsg { t: w.0, units: &w.1, bull: &w.2 };
                serde_json::to_string(&broadcast_msg).unwrap_or_default()
            };
            let _ = tx.send(json);

            // Sample trail points every 10 seconds. By elapsed time: the old
            // `now % 10 == 0` test wrote every tick that landed in that second
            // (four at 4 Hz) and none at all when a stall skipped it.
            let now_secs = chrono::Utc::now().timestamp();
            if last_trail.map_or(true, |t| t.elapsed() >= TRAIL_EVERY) {
                last_trail = Some(std::time::Instant::now());
                let db2 = db.clone();
                let inst2 = inst.clone();
                let units_snapshot = {
                    let r = state.read().await;
                    r.1.clone()
                };
                task::spawn_blocking(move || {
                    let rounds = db2.latest_rounds_for(&inst2.id).ok();
                    let rid = rounds.as_deref().and_then(|rs| {
                        rs.iter().find(|(_, _, r)| r.end.is_none()).map(|(_, rid, _)| *rid)
                    });
                    if let Some(rid) = rid {
                        for u in &units_snapshot {
                            let _ = db2.append_trail_point(
                                rid, &u.id, now_secs, u.lat, u.lon, u.alt, u.hdg,
                            );
                        }
                    }
                });
            }
        }
    }
}

/// WebSocket handler for `/ws/units` — streams live unit positions.
///
/// This is the raw, unfogged god's-eye feed straight off `Export.lua`
/// (every unit on both coalitions, precise, with pilot names). It is
/// **admin only** — the fog-of-war player picture is `/ws/tacmap`.
pub(crate) async fn ws_units_handler(
    ws: warp::ws::Ws,
    ip: std::net::IpAddr,
    session_id: Option<Uuid>,
    db: StatsDb,
    state: LiveState,
    tx: broadcast::Sender<String>,
) -> impl Reply {
    let slot = match admin_ws_gate(&db, session_id, ip) {
        Ok(s) => s,
        Err(r) => return r,
    };
    ws.on_upgrade(move |socket| async move {
        let _slot = slot;
        ws_units(socket, state, tx.subscribe()).await
    })
    .into_response()
}

pub(crate) async fn ws_units(ws: WebSocket, state: LiveState, mut rx: broadcast::Receiver<String>) {
    let (mut sink, mut stream) = ws.split();

    // Send current snapshot immediately on connect
    {
        let r = state.read().await;
        let msg = WsUnitsMsg { t: r.0, units: &r.1, bull: &r.2 };
        if let Ok(json) = serde_json::to_string(&msg) {
            if sink.send(Message::text(json)).await.is_err() {
                return;
            }
        }
    }

    // Stream updates, quit if client disconnects
    loop {
        tokio::select! {
            update = rx.recv() => {
                match update {
                    Ok(json) => {
                        if sink.send(Message::text(json)).await.is_err() { break; }
                    }
                    Err(broadcast::error::RecvError::Lagged(_)) => continue,
                    Err(_) => break,
                }
            }
            msg = stream.next() => {
                match msg {
                    Some(Ok(m)) if m.is_close() => break,
                    None => break,
                    _ => {}
                }
            }
        }
    }
}

// ── Tactical picture (fog of war) — /ws/tacmap ──────────────────────
//
// The engine fuses each coalition's own sensor state (EWR/AWACS radar
// network, JTAC eyes-on, recon/ELINT database) into a `TacPicture`. bfdb
// polls both sides on a fixed cadence, then `/ws/tacmap` streams the caller
// the picture for *their* coalition only — resolved from the session cookie
// exactly like the coalition-locked REST endpoints. Anonymous or
// no-coalition viewers get an empty frame (territory map only). This is the
// anti-cheat boundary: a browser never receives a contact its side hasn't
// earned.

#[derive(Default)]
pub(crate) struct TacCache {
    pub(crate) blue: Option<bfprotocols::tacmap::TacPicture>,
    pub(crate) red: Option<bfprotocols::tacmap::TacPicture>,
}
pub(crate) type TacState = Arc<tokio::sync::RwLock<TacCache>>;

/// What a connected `/ws/tacmap` client is allowed to see.
pub(crate) enum TacView {
    /// A resolved coalition — stream only this side's picture.
    Side(dcso3::coalition::Side),
    /// A dashboard admin with no coalition of their own — merged both-sides view.
    God,
    /// Not logged in (`"login"`) or logged in with no side (`"nocoalition"`).
    Denied(&'static str),
}


/// A weapon name worth showing, or None.
///
/// Kills recorded before the engine learned that DCS leaves `weapon_name` nil
/// on some hits (cluster submunitions especially) are stored with the literal
/// text "nil" -- dcso3's `FromLua` for String stringified the Lua nil. The
/// engine no longer produces those, but the rows already in the database do,
/// and they render in the kill log as if "nil" were the weapon. Filter it here
/// so the API never hands one out, rather than rewriting stored history.
pub(crate) fn display_weapon<S: std::fmt::Display>(name: Option<&S>) -> Option<std::string::String> {
    name.map(|w| w.to_string())
        .filter(|w| !w.is_empty() && w != "nil")
}

/// Background task: poll `query-tacmap` for both coalitions and cache the
/// results. Cheap on the engine side (in-memory sensor maps). No-op when
/// bfdb has no live engine.
pub(crate) async fn tacmap_poller(db: StatsDb, inst: Inst, state: TacState) {
    use netidx::publisher::Value;
    // 1 Hz was hammering the engine's single admin-RPC queue (blue + red every
    // second, on top of query-gci and on-demand query-objectives) which made
    // *every* RPC on that queue miss its deadline. 2 Hz total (one query-tacmap
    // call per side per 2s = 1 query/sec average) stays well under that
    // threshold while roughly halving perceived TACMAP staleness vs. the old
    // 3s tick.
    let mut tick = tokio::time::interval(std::time::Duration::from_millis(2000));
    tick.set_missed_tick_behavior(tokio::time::MissedTickBehavior::Delay);
    // Diagnostics, logged at most once every ~30s per side so the log stays
    // useful for "why is the scope empty" without spamming.
    let mut last_report: [chrono::DateTime<chrono::Utc>; 2] =
        [chrono::DateTime::UNIX_EPOCH; 2];
    let mut last_err_kind: [u8; 2] = [255; 2]; // 0=ok 1=rpc-err 2=timeout 3=bad-json
    // Consecutive failures per side, so an instance that is simply switched off
    // backs off to a heartbeat instead of a steady warning stream.
    let mut fail_streak: [u32; 2] = [0; 2];
    loop {
        tick.tick().await;
        for (i, side) in ["blue", "red"].into_iter().enumerate() {
            let res = tokio::time::timeout(
                std::time::Duration::from_secs(6),
                call_engine_rpc_str(&db, &inst, "query-tacmap", vec![("side", Value::from(side))]),
            )
            .await;
            let (pic, kind, detail) = match res {
                Ok(Ok(json)) => match serde_json::from_str::<bfprotocols::tacmap::TacPicture>(&json) {
                    Ok(p) => {
                        let d = format!("{} air, {} ground, {} rings", p.air.len(), p.ground.len(), p.radar_rings.len());
                        (Some(p), 0u8, d)
                    }
                    Err(e) => (None, 3u8, format!("unparseable JSON ({e})")),
                },
                Ok(Err(e)) => (None, 1u8, format!("RPC error: {} -- is bflib.dll current?", e.0)),
                Err(_) => (None, 2u8, "RPC timed out after 6s (engine overloaded, restarting, or bflib.dll not current)".to_string()),
            };
            // Report on a state change, or on the reporting interval. Every
            // instance runs its own poller, so tag the line with the instance
            // id -- without it a configured-but-not-running instance (a test
            // server that is simply off) produced ~200 identical warnings an
            // hour that looked exactly like the live server failing.
            let now = chrono::Utc::now();
            if kind == 0 {
                fail_streak[i] = 0;
            } else {
                fail_streak[i] = fail_streak[i].saturating_add(1);
            }
            // An instance whose engine is down fails forever; after the first
            // minute of that, drop to one line every 10 minutes. A state change
            // still reports immediately, so recovery is never delayed.
            let interval = if fail_streak[i] > 30 { 600 } else { 30 };
            // While the engine is down the breaker (1) and the probe that gets
            // through every 10s and times out (2) alternate; that is one state,
            // not a change, or every probe re-reports it (~5,400 WARNs in one
            // 3.5h outage).
            let class = |k: u8| if k == 2 { 1 } else { k };
            if class(last_err_kind[i]) != class(kind)
                || (now - last_report[i]).num_seconds() >= interval
            {
                let id = &inst.id;
                if kind == 0 {
                    // Only the recovery is news; the steady "ok" was a quarter
                    // of the whole log.
                    if last_err_kind[i] != 0 && last_err_kind[i] != 255 {
                        log::info!("[{id}] tacmap_poller: query-tacmap({side}) ok again -- {detail}");
                    } else {
                        log::debug!("[{id}] tacmap_poller: query-tacmap({side}) ok -- {detail}");
                    }
                } else if fail_streak[i] > 30 {
                    log::warn!(
                        "[{id}] tacmap_poller: query-tacmap({side}) {detail} (failing since {} attempt(s) ago -- is this instance's DCS server running?)",
                        fail_streak[i]
                    );
                } else {
                    log::warn!("[{id}] tacmap_poller: query-tacmap({side}) {detail}");
                }
                last_report[i] = now;
                last_err_kind[i] = kind;
            }
            if let Some(pic) = pic {
                let mut w = state.write().await;
                if side == "blue" {
                    w.blue = Some(pic);
                } else {
                    w.red = Some(pic);
                }
            }
        }
    }
}

/// Build the frame to send this tick for one viewer.
pub(crate) async fn build_tac_frame(
    view: &TacView,
    tac: &TacState,
    live: &LiveState,
) -> bfprotocols::tacmap::TacFrame {
    use bfprotocols::tacmap::{TacBullseye, TacFrame, TacPicture};
    use dcso3::coalition::Side;

    let (side_opt, god) = match view {
        TacView::Denied(reason) => {
            return TacFrame { picture: None, reason: Some((*reason).to_string()) };
        }
        TacView::Side(s) => (Some(*s), false),
        TacView::God => (None, true),
    };

    // A cached picture older than this is treated as "engine went away".
    let fresh = |p: &TacPicture| (chrono::Utc::now() - p.time).num_seconds() < 20;
    let empty = || TacPicture {
        side: side_opt,
        time: chrono::Utc::now(),
        bullseye: vec![],
        air: vec![],
        ground: vec![],
        radar_rings: vec![],
    };

    let mut picture = {
        let cache = tac.read().await;
        if god {
            let mut merged = empty();
            let mut seen_air = std::collections::HashSet::new();
            let mut seen_gnd = std::collections::HashSet::new();
            for p in [cache.blue.as_ref(), cache.red.as_ref()]
                .into_iter()
                .flatten()
                .filter(|p| fresh(p))
            {
                // Same contact can appear in both sides' pictures (hostile in
                // one, friendly in the other) — the EnId hash id is stable, so
                // keep the first (friendly wins by blue-before-red order only
                // incidentally; god view colours by `side` anyway).
                for t in &p.air {
                    if seen_air.insert(t.id) {
                        merged.air.push(t.clone());
                    }
                }
                for g in &p.ground {
                    if seen_gnd.insert(g.id) {
                        merged.ground.push(g.clone());
                    }
                }
                merged.radar_rings.extend(p.radar_rings.iter().cloned());
            }
            merged
        } else {
            let p = match side_opt {
                Some(Side::Blue) => cache.blue.clone(),
                Some(Side::Red) => cache.red.clone(),
                _ => None,
            };
            p.filter(|p| fresh(p)).unwrap_or_else(empty)
        }
    };

    // Bullseye comes from the Export.lua feed (side 1 = Red, 2 = Blue).
    let bulls = { live.read().await.2.clone() };
    picture.bullseye = bulls
        .into_iter()
        .filter_map(|b| {
            let s = match b.side {
                1 => Side::Red,
                2 => Side::Blue,
                _ => return None,
            };
            if god || side_opt == Some(s) {
                Some(TacBullseye { side: s, lat: b.lat, lon: b.lon })
            } else {
                None
            }
        })
        .collect();

    TacFrame { picture: Some(picture), reason: None }
}

pub(crate) async fn ws_tacmap_handler(
    ws: warp::ws::Ws,
    ip: std::net::IpAddr,
    session_id: Option<Uuid>,
    query: std::collections::HashMap<std::string::String, std::string::String>,
    db: StatsDb,
    bot_cfg: Arc<Option<BotLinkConfig>>,
    inst: Inst,
    tac: TacState,
    live: LiveState,
) -> Response {
    let Some(slot) = websec::ws_slot(ip) else {
        return websec::ws_refused(warp::http::StatusCode::TOO_MANY_REQUESTS, "too many open connections");
    };
    let view = tacmap_view(session_id, &query, &db, &bot_cfg, &inst).await;
    ws.on_upgrade(move |socket| async move {
        let _slot = slot;
        ws_tacmap(socket, view, tac, live, TacIdentity { session_id, query, db, bot_cfg, inst }).await
    })
    .into_response()
}

/// Everything needed to work out a viewer's side again later.
pub(crate) struct TacIdentity {
    pub(crate) session_id: Option<Uuid>,
    pub(crate) query: std::collections::HashMap<std::string::String, std::string::String>,
    pub(crate) db: StatsDb,
    pub(crate) bot_cfg: Arc<Option<BotLinkConfig>>,
    pub(crate) inst: Inst,
}

/// Which picture this viewer may see, right now.
pub(crate) async fn tacmap_view(
    session_id: Option<Uuid>,
    query: &std::collections::HashMap<std::string::String, std::string::String>,
    db: &StatsDb,
    bot_cfg: &Arc<Option<BotLinkConfig>>,
    inst: &Inst,
) -> TacView {
    use dcso3::coalition::Side;
    match session_id {
        None => TacView::Denied("login"),
        Some(id) => match task::block_in_place(|| db.get_session(id)).ok().flatten() {
            None => TacView::Denied("login"),
            Some(session) => {
                let ucid = resolve_ucid_via_bot(&bot_cfg, &session.discord_id).await;
                let own = match &ucid {
                    Some(u) => task::block_in_place(|| db.pilot_current_side(&inst.id, u)).ok().flatten(),
                    None => None,
                };
                match own {
                    Some(s) => TacView::Side(s),
                    None if session.is_admin => match query.get("side").map(|s| s.as_str()) {
                        Some("blue") => TacView::Side(Side::Blue),
                        Some("red") => TacView::Side(Side::Red),
                        _ => TacView::God,
                    },
                    None => TacView::Denied("nocoalition"),
                }
            }
        },
    }
}

pub(crate) async fn ws_tacmap(
    ws: WebSocket,
    mut view: TacView,
    tac: TacState,
    live: LiveState,
    who: TacIdentity,
) {
    // How often the viewer's side is worked out again. A side is fixed per
    // round, but a new round (or a campaign reset) can move a pilot, and a
    // tab left open overnight used to keep streaming last round's side.
    const RECHECK: std::time::Duration = std::time::Duration::from_secs(60);
    // Browsers answer pings on their own; a peer that has said nothing for
    // this long is gone, whatever TCP thinks.
    const IDLE: std::time::Duration = std::time::Duration::from_secs(90);
    let (mut sink, mut stream) = ws.split();
    let mut tick = tokio::time::interval(std::time::Duration::from_millis(1000));
    tick.set_missed_tick_behavior(tokio::time::MissedTickBehavior::Delay);
    // Send one frame immediately so the client doesn't wait a full second.
    let frame = build_tac_frame(&view, &tac, &live).await;
    if sink
        .send(Message::text(serde_json::to_string(&frame).unwrap_or_default()))
        .await
        .is_err()
    {
        return;
    }
    // A denied viewer never gets a picture. It has its answer (the reason in
    // that frame); close rather than hold an idle socket per anonymous tab.
    if matches!(view, TacView::Denied(_)) {
        let _ = sink.send(Message::close()).await;
        return;
    }
    let mut round = task::block_in_place(|| who.db.active_round_id(&who.inst.id)).ok().flatten();
    let mut last_check = std::time::Instant::now();
    let mut last_heard = std::time::Instant::now();
    let mut ticks: u32 = 0;
    loop {
        tokio::select! {
            _ = tick.tick() => {
                ticks = ticks.wrapping_add(1);
                if last_heard.elapsed() > IDLE {
                    break;
                }
                if ticks % 30 == 0 && sink.send(Message::ping(Vec::new())).await.is_err() {
                    break;
                }
                if last_check.elapsed() >= RECHECK {
                    last_check = std::time::Instant::now();
                    let now_round = task::block_in_place(|| who.db.active_round_id(&who.inst.id)).ok().flatten();
                    if now_round != round {
                        round = now_round;
                        view = tacmap_view(who.session_id, &who.query, &who.db, &who.bot_cfg, &who.inst).await;
                    }
                    if matches!(view, TacView::Denied(_)) {
                        let frame = build_tac_frame(&view, &tac, &live).await;
                        let _ = sink.send(Message::text(serde_json::to_string(&frame).unwrap_or_default())).await;
                        let _ = sink.send(Message::close()).await;
                        break;
                    }
                }
                let frame = build_tac_frame(&view, &tac, &live).await;
                let json = serde_json::to_string(&frame).unwrap_or_default();
                if sink.send(Message::text(json)).await.is_err() { break; }
            }
            msg = stream.next() => match msg {
                Some(Ok(m)) if m.is_close() => break,
                Some(Ok(_)) => last_heard = std::time::Instant::now(),
                None | Some(Err(_)) => break,
            }
        }
    }
}

// ── SRS proxy ───────────────────────────────────────────────────────
