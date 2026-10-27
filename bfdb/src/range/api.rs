// Copyright (c) 2026 Dillen Weerasinghe. All rights reserved.
// Proprietary and confidential. No license is granted to use, copy, modify, or
// distribute this file. See NOTICE section 3 and the repository NOTICE file.
//! `/api/range/*` -- the range site's and the Discord result feed's API.
//!
//! Mounted as ONE boxed filter ahead of the static-file catch-all (see
//! main.rs); every route answers with a plain `Response` so the sub-chains
//! unify cheaply and the whole thing adds a single level to main's already
//! deep route type.
//!
//! Every route takes an optional `?instance=<id>` (or `?server=<dcs name>`):
//! absent means the first `kind: "range"` instance, `all` means every range
//! instance (read-only views), and a campaign instance is a 400. Public unless
//! noted; the spawn routes need a bot-linked Discord session and fill the ucid
//! from that session, never from the request.

use super::{
    ballistics, boards, cards, clip, discord, parse_ts, store::Stored, RangeCtx, Scope,
};
use bfprotocols::range::{SpawnRequest, WeaponDb};
use netidx::publisher::Value as NValue;
use serde::Deserialize;
use serde_json::{json, Value};
use std::{
    collections::{BTreeMap, HashMap},
    sync::Arc,
    time::{Duration, SystemTime},
};
use tokio::task::block_in_place;
use uuid::Uuid;
use warp::{
    filters::BoxedFilter,
    http::{header, HeaderValue, StatusCode},
    reply::Response,
    Filter, Reply,
};

type Q = HashMap<String, String>;

// ── replies ────────────────────────────────────────────────────────────

fn reply_json(code: StatusCode, v: &impl serde::Serialize) -> Response {
    let mut r = warp::reply::with_status(warp::reply::json(v), code).into_response();
    r.headers_mut().insert(header::CACHE_CONTROL, HeaderValue::from_static("no-store"));
    r
}

fn ok(v: &impl serde::Serialize) -> Response {
    reply_json(StatusCode::OK, v)
}

fn err(code: StatusCode, msg: impl std::fmt::Display) -> Response {
    reply_json(code, &json!({ "error": msg.to_string() }))
}

/// `{"ok":false,"message":...}` -- the shape the spawn routes always answer
/// with, so the site has one thing to show.
fn fail(code: StatusCode, msg: impl std::fmt::Display) -> Response {
    reply_json(code, &json!({ "ok": false, "message": msg.to_string() }))
}

/// A 500 that names a log reference, not the anyhow chain (sled paths,
/// file names, serde positions), which goes to the log.
fn internal(e: anyhow::Error) -> Response {
    crate::websec::error_response(&e.context("range api"))
}

fn bytes_reply(body: Vec<u8>, ctype: &'static str, max_age: u32) -> Response {
    let mut r = Response::new(body.into());
    let h = r.headers_mut();
    h.insert(header::CONTENT_TYPE, HeaderValue::from_static(ctype));
    if let Ok(v) = HeaderValue::from_str(&format!("public, max-age={max_age}")) {
        h.insert(header::CACHE_CONTROL, v);
    }
    r
}

macro_rules! scope_or_400 {
    ($ctx:expr, $q:expr) => {
        match $ctx.scope(&$q) {
            Ok(s) => s,
            Err(e) => return err(StatusCode::BAD_REQUEST, e),
        }
    };
}

fn num<T: std::str::FromStr>(q: &Q, k: &str) -> Option<T> {
    q.get(k).and_then(|v| v.trim().parse().ok())
}

fn nonempty<'a>(q: &'a Q, k: &str) -> Option<&'a str> {
    q.get(k).map(|s| s.trim()).filter(|s| !s.is_empty())
}

fn since_days(days: Option<i64>) -> u64 {
    match days {
        Some(d) if d > 0 => (chrono::Utc::now() - chrono::Duration::days(d))
            .timestamp_millis()
            .max(0) as u64,
        _ => 0,
    }
}

/// Path segments arrive percent-encoded; ids may contain spaces.
fn decode_seg(s: &str) -> String {
    urlencoding::decode(s).map(|c| c.into_owned()).unwrap_or_else(|_| s.to_string())
}

// ── identity ───────────────────────────────────────────────────────────

/// A logged-in, bot-linked player: (is dashboard admin, ucid).
async fn linked_player(
    ctx: &RangeCtx,
    sid: Option<Uuid>,
) -> Result<(bool, String), Response> {
    let Some(sid) = sid else {
        return Err(fail(StatusCode::UNAUTHORIZED, "not logged in"));
    };
    let s = match block_in_place(|| ctx.db.get_session(sid)) {
        Ok(Some(s)) => s,
        Ok(None) => {
            return Err(fail(StatusCode::UNAUTHORIZED, "session expired -- log in again"))
        }
        Err(e) => return Err(internal(e)),
    };
    match ctx.session_ucid(sid, &s.discord_id).await {
        Some(u) => Ok((s.is_admin, u)),
        None => Err(fail(
            StatusCode::FORBIDDEN,
            "account not linked -- type -linkme <token> in DCS chat (get the token with /linkme in Discord)",
        )),
    }
}

// ── handlers: listing ──────────────────────────────────────────────────

async fn h_live(ctx: Arc<RangeCtx>, q: Q) -> Response {
    let sc = scope_or_400!(ctx, q);
    let r = ctx
        .rpc_cached(
            sc.primary(),
            "query-range",
            Duration::from_secs(2),
            Duration::from_secs(8),
            Duration::from_secs(15),
        )
        .await;
    match r {
        Ok(v) => ok(&json!({ "live": v, "reason": null })),
        Err(e) => ok(&json!({ "live": null, "reason": e })),
    }
}

/// `?before_id=<id>` (the last item of the previous page) wins over
/// `?before=<rfc3339>`: it pages exactly, even through records that share a
/// timestamp.
async fn h_feed(ctx: Arc<RangeCtx>, q: Q) -> Response {
    let sc = scope_or_400!(ctx, q);
    let limit = num::<usize>(&q, "limit").unwrap_or(50).clamp(1, 200);
    let by_id = match nonempty(&q, "before_id") {
        Some(id) => match block_in_place(|| ctx.store.position(id)) {
            Ok(Some(p)) => Some(p),
            Ok(None) => {
                return err(StatusCode::BAD_REQUEST, format!("no range result {id:?}"))
            }
            Err(e) => return internal(e),
        },
        None => None,
    };
    let before: Option<(u64, String)> = match (by_id, nonempty(&q, "before")) {
        (Some(p), _) => Some(p),
        (None, Some(b)) => match parse_ts(b) {
            Ok(t) => Some((t.timestamp_millis().max(0) as u64, String::new())),
            Err(e) => return err(StatusCode::BAD_REQUEST, e),
        },
        (None, None) => None,
    };
    let kind = nonempty(&q, "kind").map(|s| s.to_string());
    let r = block_in_place(|| {
        let mut items = vec![];
        let before = before.as_ref().map(|(t, id)| (*t, id.as_str()));
        ctx.store.walk_newest(&sc.ids(), 0, before, |s| {
            if kind.as_deref().map_or(true, |k| s.kind() == k) {
                items.push(boards::summary(&ctx.store, &s));
            }
            items.len() < limit
        })?;
        anyhow::Ok(items)
    });
    match r {
        Ok(items) => ok(&json!({ "items": items })),
        Err(e) => internal(e),
    }
}

async fn h_results(ctx: Arc<RangeCtx>, q: Q) -> Response {
    let sc = scope_or_400!(ctx, q);
    let limit = num::<usize>(&q, "limit").unwrap_or(50).clamp(1, 500);
    let offset = num::<usize>(&q, "offset").unwrap_or(0);
    let pilot = nonempty(&q, "pilot").map(|s| s.to_string());
    let kind = nonempty(&q, "kind").map(|s| s.to_string());
    let unit = nonempty(&q, "unit_type").map(|s| s.to_string());
    let station = nonempty(&q, "station").map(|s| s.to_string());
    let since = since_days(num(&q, "days"));
    let keep = |s: &Stored| {
        s.ts_ms >= since
            && kind.as_deref().map_or(true, |k| s.kind() == k)
            && unit.as_deref().map_or(true, |u| s.unit_type().eq_ignore_ascii_case(u))
            && station.as_deref().map_or(true, |st| s.station() == Some(st))
    };
    let r = block_in_place(|| {
        let mut total = 0usize;
        let mut items = vec![];
        let mut visit = |s: Stored| {
            if s.ts_ms < since {
                return false; // both walks are newest first
            }
            if keep(&s) {
                if total >= offset && items.len() < limit {
                    items.push(boards::summary(&ctx.store, &s));
                }
                total += 1;
            }
            true
        };
        match &pilot {
            Some(p) => ctx.store.walk_pilot(p, &sc.ids(), &mut visit)?,
            None => ctx.store.walk_newest(&sc.ids(), since, None, &mut visit)?,
        }
        anyhow::Ok((items, total))
    });
    match r {
        Ok((items, total)) => ok(&json!({ "items": items, "total": total })),
        Err(e) => internal(e),
    }
}

async fn h_result(ctx: Arc<RangeCtx>, id: String) -> Response {
    let id = decode_seg(&id);
    match block_in_place(|| ctx.store.full(&id)) {
        Ok(Some(s)) => {
            let mut v = boards::summary(&ctx.store, &s);
            if let (Some(o), Some(t)) = (v.as_object_mut(), s.raw.get("track")) {
                o.insert("track".into(), t.clone());
            }
            ok(&v)
        }
        Ok(None) => err(StatusCode::NOT_FOUND, format!("no range result {id:?}")),
        Err(e) => internal(e),
    }
}

/// The card SVG for a stored record (with its track when it still has one).
fn card_svg(s: &Stored) -> String {
    match s.decode() {
        Some(r) => cards::render_svg(&r),
        None => cards::unknown_svg(
            s.pilot_name(),
            s.unit_type(),
            s.kind(),
            &s.ts().format("%Y-%m-%d %H:%M:%SZ").to_string(),
        ),
    }
}

async fn h_card(ctx: Arc<RangeCtx>, id: String, png: bool) -> Response {
    let id = decode_seg(&id);
    if png {
        if let Some(b) = ctx.png_get(&id) {
            return bytes_reply((*b).clone(), "image/png", 3600);
        }
    }
    let s = match block_in_place(|| ctx.store.full(&id)) {
        Ok(Some(s)) => s,
        Ok(None) => return err(StatusCode::NOT_FOUND, format!("no range result {id:?}")),
        Err(e) => return internal(e),
    };
    if !png {
        return bytes_reply(card_svg(&s).into_bytes(), "image/svg+xml", 3600);
    }
    let r = tokio::task::spawn_blocking(move || cards::png(&card_svg(&s))).await;
    match r {
        Ok(Ok(bytes)) => {
            let b = Arc::new(bytes);
            ctx.png_put(&id, b.clone());
            bytes_reply((*b).clone(), "image/png", 3600)
        }
        Ok(Err(e)) => internal(e),
        Err(e) => internal(anyhow::anyhow!("card render task failed: {e}")),
    }
}

async fn h_discord(ctx: Arc<RangeCtx>, id: String) -> Response {
    let id = decode_seg(&id);
    match block_in_place(|| ctx.store.get(&id)) {
        Ok(Some(s)) => match s.decode() {
            Some(r) => ok(&discord::embed(&ctx, &r)),
            None => err(
                StatusCode::UNPROCESSABLE_ENTITY,
                "this result was written by a newer range engine than this bfdb understands",
            ),
        },
        Ok(None) => err(StatusCode::NOT_FOUND, format!("no range result {id:?}")),
        Err(e) => internal(e),
    }
}

/// The pilot's Discord picture: their own, or the default one Discord gives
/// an account without one (picked from the id, as Discord picks it).
fn avatar_url(discord_id: &str, avatar: Option<&str>) -> String {
    match avatar.filter(|a| !a.is_empty()) {
        Some(a) => {
            let ext = if a.starts_with("a_") { "gif" } else { "png" };
            format!("https://cdn.discordapp.com/avatars/{discord_id}/{a}.{ext}?size=64")
        }
        None => {
            let n = discord_id.parse::<u64>().map(|id| (id >> 22) % 6).unwrap_or(0);
            format!("https://cdn.discordapp.com/embed/avatars/{n}.png")
        }
    }
}

async fn h_me(ctx: Arc<RangeCtx>, sid: Option<Uuid>, q: Q) -> Response {
    let anon = json!({ "logged_in": false, "ucid": null, "name": null, "admin": false });
    let Some(sid) = sid else { return ok(&anon) };
    let s = match block_in_place(|| ctx.db.get_session(sid)) {
        Ok(Some(s)) => s,
        _ => return ok(&anon),
    };
    let ucid = ctx.session_ucid(sid, &s.discord_id).await;
    let ids = ctx.scope(&q).map(|sc| sc.ids()).unwrap_or_default();
    let name = ucid
        .as_deref()
        .and_then(|u| ctx.store.pilot_name(u, &ids))
        .or_else(|| ucid.as_deref().and_then(|u| ctx.db.pilot_name(&u.parse().ok()?)))
        .unwrap_or_else(|| s.username.clone());
    ok(&json!({
        "logged_in": true,
        "ucid": ucid,
        "name": name,
        "discord_name": s.username,
        "avatar_url": avatar_url(&s.discord_id, s.avatar.as_deref()),
        "admin": s.is_admin,
    }))
}

async fn h_pilot(ctx: Arc<RangeCtx>, ucid: String, q: Q) -> Response {
    let sc = scope_or_400!(ctx, q);
    let ucid = decode_seg(&ucid);
    match block_in_place(|| {
        boards::pilot_profile(&ctx.store, &sc.ids(), &ucid, chrono::Utc::now())
    }) {
        Ok(Some(v)) => ok(&v),
        Ok(None) => err(StatusCode::NOT_FOUND, "no range results for that pilot"),
        Err(e) => internal(e),
    }
}

async fn h_pilots(ctx: Arc<RangeCtx>, q: Q) -> Response {
    let sc = scope_or_400!(ctx, q);
    let needle = nonempty(&q, "q").map(|s| s.to_lowercase());
    let r = block_in_place(|| {
        let ids = sc.ids();
        let mut all = ctx.store.pilots(&ids)?;
        let mut hits: Vec<(u8, u64, String, String)> = all
            .drain(..)
            .filter_map(|(ucid, name, last)| {
                let rank = match &needle {
                    None => 1,
                    Some(n) => {
                        let l = name.to_lowercase();
                        if l.starts_with(n.as_str()) {
                            0
                        } else if l.contains(n.as_str()) || ucid == *n {
                            1
                        } else {
                            return None;
                        }
                    }
                };
                Some((rank, last, ucid, name))
            })
            .collect();
        hits.sort_by(|a, b| a.0.cmp(&b.0).then(b.1.cmp(&a.1)));
        let mut out = vec![];
        for (_, _, ucid, name) in hits.into_iter().take(50) {
            let count = ctx.store.pilot_count(&ucid, &ids)?;
            out.push(json!({ "ucid": ucid, "name": name, "count": count }));
        }
        anyhow::Ok(out)
    });
    match r {
        Ok(v) => ok(&v),
        Err(e) => internal(e),
    }
}

/// Serve a whole-window aggregate from the short cache, computing it on a
/// miss.
fn cached_window(
    ctx: &RangeCtx,
    key: String,
    ttl: Duration,
    sc: &Scope,
    days: i64,
    f: impl FnOnce(&[Stored]) -> Value,
) -> anyhow::Result<Value> {
    if let Some(v) = ctx.agg_get(&key, ttl) {
        return Ok(v);
    }
    let rows = ctx.store.window(&sc.ids(), since_days(Some(days)))?;
    let v = f(&rows);
    ctx.agg_put(key, v.clone());
    Ok(v)
}

async fn h_greenie(ctx: Arc<RangeCtx>, q: Q) -> Response {
    let sc = scope_or_400!(ctx, q);
    let days = num::<i64>(&q, "days").unwrap_or(30).clamp(1, 365);
    let carrier = nonempty(&q, "carrier").map(|s| s.to_string());
    let unit = nonempty(&q, "unit_type").map(|s| s.to_string());
    let key = format!("greenie|{}|{days}|{carrier:?}|{unit:?}", sc.key());
    let r = block_in_place(|| {
        cached_window(&ctx, key, Duration::from_secs(20), &sc, days, |rows| {
            boards::greenie(rows, carrier.as_deref(), unit.as_deref())
        })
    });
    match r {
        Ok(v) => ok(&v),
        Err(e) => internal(e),
    }
}

async fn h_leaderboards(ctx: Arc<RangeCtx>, q: Q) -> Response {
    let sc = scope_or_400!(ctx, q);
    let days = num::<i64>(&q, "days").unwrap_or(30).clamp(1, 365);
    let key = format!("boards|{}|{days}", sc.key());
    let r = block_in_place(|| {
        cached_window(&ctx, key, Duration::from_secs(30), &sc, days, boards::leaderboards)
    });
    match r {
        Ok(v) => ok(&v),
        Err(e) => internal(e),
    }
}

async fn h_impacts(ctx: Arc<RangeCtx>, station: String, q: Q) -> Response {
    let sc = scope_or_400!(ctx, q);
    let station = decode_seg(&station);
    let days = num::<i64>(&q, "days").unwrap_or(30).clamp(1, 365);
    let pilot = nonempty(&q, "pilot").map(|s| s.to_string());
    let key = format!("impacts|{}|{days}|{station}|{pilot:?}", sc.key());
    let r = block_in_place(|| {
        cached_window(&ctx, key, Duration::from_secs(15), &sc, days, |rows| {
            boards::station_impacts(rows, &station, pilot.as_deref())
        })
    });
    match r {
        Ok(v) => ok(&v),
        Err(e) => internal(e),
    }
}

// ── handlers: live engine ──────────────────────────────────────────────

async fn h_catalog(ctx: Arc<RangeCtx>, q: Q) -> Response {
    let sc = scope_or_400!(ctx, q);
    let r = ctx
        .rpc_cached(
            sc.primary(),
            "query-range-catalog",
            Duration::from_secs(60),
            Duration::from_secs(8),
            Duration::from_secs(3600),
        )
        .await;
    match r {
        Ok(v) => ok(&v),
        Err(e) => err(StatusCode::BAD_GATEWAY, e),
    }
}

#[derive(Deserialize)]
struct SpawnBody {
    item: String,
    #[serde(default)]
    params: serde_json::Map<String, Value>,
}

#[derive(Deserialize)]
struct DespawnBody {
    spawn_id: String,
}

#[derive(Deserialize)]
struct StationBody {
    station: String,
}

fn parse_body<T: serde::de::DeserializeOwned>(b: &[u8]) -> Result<T, Response> {
    serde_json::from_slice(b)
        .map_err(|e| fail(StatusCode::BAD_REQUEST, format!("bad request body: {e}")))
}

/// An engine reply to a mutating RPC: 200 with the engine's own JSON, or 502
/// in the `{"ok":false,"message"}` shape.
fn engine_reply(r: Result<Value, String>) -> Response {
    match r {
        Ok(v) => ok(&v),
        Err(e) => fail(StatusCode::BAD_GATEWAY, e),
    }
}

const SPAWN_EVERY: Duration = Duration::from_secs(3);

async fn h_spawn(
    ctx: Arc<RangeCtx>,
    q: Q,
    sid: Option<Uuid>,
    body: bytes::Bytes,
) -> Response {
    let sc = scope_or_400!(ctx, q);
    let (admin, ucid) = match linked_player(&ctx, sid).await {
        Ok(p) => p,
        Err(r) => return r,
    };
    let b: SpawnBody = match parse_body(&body) {
        Ok(b) => b,
        Err(r) => return r,
    };
    if b.item.trim().is_empty() {
        return fail(StatusCode::BAD_REQUEST, "no item");
    }
    if let Err(wait) = ctx.rate_limit(&ucid, SPAWN_EVERY) {
        return fail(
            StatusCode::TOO_MANY_REQUESTS,
            format!("slow down -- try again in {wait:.1} s"),
        );
    }
    let params: BTreeMap<String, String> = b
        .params
        .into_iter()
        .map(|(k, v)| {
            let v = match v {
                Value::String(s) => s,
                Value::Null => String::new(),
                other => other.to_string(),
            };
            (k, v)
        })
        .collect();
    // ucid from the session, instructor from the dashboard role -- never from
    // the body.
    let req = SpawnRequest { ucid, item: b.item, params, instructor: admin };
    let js = match serde_json::to_string(&req) {
        Ok(s) => s,
        Err(e) => return internal(e.into()),
    };
    engine_reply(
        ctx.rpc_json(
            sc.primary(),
            "range-spawn",
            vec![("req", NValue::from(js))],
            Duration::from_secs(10),
        )
        .await,
    )
}

async fn h_despawn(
    ctx: Arc<RangeCtx>,
    q: Q,
    sid: Option<Uuid>,
    body: bytes::Bytes,
) -> Response {
    let sc = scope_or_400!(ctx, q);
    let (admin, ucid) = match linked_player(&ctx, sid).await {
        Ok(p) => p,
        Err(r) => return r,
    };
    let b: DespawnBody = match parse_body(&body) {
        Ok(b) => b,
        Err(r) => return r,
    };
    if let Err(wait) = ctx.rate_limit(&ucid, SPAWN_EVERY) {
        return fail(
            StatusCode::TOO_MANY_REQUESTS,
            format!("slow down -- try again in {wait:.1} s"),
        );
    }
    engine_reply(
        ctx.rpc_json(
            sc.primary(),
            "range-despawn",
            vec![
                ("ucid", NValue::from(ucid)),
                ("spawn_id", NValue::from(b.spawn_id)),
                ("instructor", NValue::from(admin)),
            ],
            Duration::from_secs(10),
        )
        .await,
    )
}

async fn h_reset_station(
    ctx: Arc<RangeCtx>,
    q: Q,
    sid: Option<Uuid>,
    body: bytes::Bytes,
) -> Response {
    let sc = scope_or_400!(ctx, q);
    let Some(sid) = sid else {
        return fail(StatusCode::UNAUTHORIZED, "not logged in");
    };
    let s = match block_in_place(|| ctx.db.get_session(sid)) {
        Ok(Some(s)) => s,
        Ok(None) => {
            return fail(StatusCode::UNAUTHORIZED, "session expired -- log in again")
        }
        Err(e) => return internal(e),
    };
    if !s.is_admin {
        return fail(StatusCode::FORBIDDEN, "admins only");
    }
    let b: StationBody = match parse_body(&body) {
        Ok(b) => b,
        Err(r) => return r,
    };
    // An admin may be a local (non-Discord) login with no linked ucid.
    let ucid = ctx.session_ucid(sid, &s.discord_id).await.unwrap_or_default();
    log::info!(
        "[{}] range: {} reset station {:?}",
        sc.primary().id,
        s.username,
        b.station
    );
    engine_reply(
        ctx.rpc_json(
            sc.primary(),
            "range-reset-station",
            vec![
                ("ucid", NValue::from(ucid)),
                ("station", NValue::from(b.station)),
                ("instructor", NValue::from(true)),
            ],
            Duration::from_secs(10),
        )
        .await,
    )
}

async fn h_weapons(ctx: Arc<RangeCtx>, q: Q) -> Response {
    let sc = scope_or_400!(ctx, q);
    let inst = sc.primary().clone();
    let snap_key = format!("weapons\0{}", inst.id);
    let live = ctx
        .rpc_cached(
            &inst,
            "query-weapons",
            Duration::from_secs(600),
            Duration::from_secs(10),
            Duration::from_secs(24 * 3600),
        )
        .await;
    // The weapon database only changes with DCS itself: keep the last one so
    // the calculator still works while the range server is down.
    let wdb: Option<WeaponDb> = match live {
        Ok(v) => {
            if let Ok(b) = serde_json::to_vec(&v) {
                let _ = ctx.store.meta_set(&snap_key, &b);
            }
            serde_json::from_value(v).ok()
        }
        Err(_) => ctx
            .store
            .meta_get(&snap_key)
            .ok()
            .flatten()
            .and_then(|b| serde_json::from_slice(&b).ok()),
    };
    let key = format!(
        "calibration|{}|{}",
        sc.key(),
        wdb.as_ref().map(|w| w.dcs_version.as_str()).unwrap_or("")
    );
    let cal = match ctx.agg_get(&key, Duration::from_secs(600)) {
        Some(v) => v,
        None => {
            let ids = sc.ids();
            let bombs = wdb.as_ref().map(|w| w.bombs.clone()).unwrap_or_default();
            let c2 = ctx.clone();
            let r = tokio::task::spawn_blocking(move || -> anyhow::Result<Value> {
                let rows = c2.store.window(&ids, since_days(Some(180)))?;
                let drops = boards::unguided_drops(&rows);
                Ok(serde_json::to_value(ballistics::calibrate(&bombs, &drops))?)
            })
            .await;
            match r {
                Ok(Ok(v)) => {
                    ctx.agg_put(key, v.clone());
                    v
                }
                Ok(Err(e)) => return internal(e),
                Err(e) => {
                    return internal(anyhow::anyhow!("calibration task failed: {e}"))
                }
            }
        }
    };
    ok(&json!({ "db": wdb, "calibration": cal }))
}

// ── tacview ────────────────────────────────────────────────────────────

/// The recording that contains `ts`: among the `.acmi` files written to
/// after the event, the one that was already being written when it happened
/// (created at or before it, a minute of slack); failing that, the first
/// file finished after it. (The newest file after the event is usually a
/// later mission, not this one.)
fn pick_acmi(dir: &std::path::Path, ts: SystemTime) -> Option<std::path::PathBuf> {
    let slack = Duration::from_secs(60);
    let mut cands = vec![];
    for e in std::fs::read_dir(dir).ok()?.flatten() {
        let p = e.path();
        let is_acmi = p
            .file_name()
            .and_then(|n| n.to_str())
            .map_or(false, |n| n.to_ascii_lowercase().ends_with(".acmi"));
        if !is_acmi {
            continue;
        }
        let Ok(md) = e.metadata() else { continue };
        let Ok(mtime) = md.modified() else { continue };
        if mtime + slack < ts {
            continue;
        }
        cands.push((p, md.created().ok(), mtime));
    }
    let containing = cands
        .iter()
        .filter(|(_, c, _)| c.map_or(false, |c| c <= ts + slack))
        .max_by_key(|(_, c, _)| *c)
        .map(|(p, _, _)| p.clone());
    containing
        .or_else(|| cands.iter().min_by_key(|(_, _, m)| *m).map(|(p, _, _)| p.clone()))
}

/// Whole-recording downloads streaming at once, across every client: each
/// holds a file open and a disk read in flight for minutes.
const TACVIEW_STREAMS: usize = 4;

/// Clip cuts running at once: each scans a Tacview recording end to end on
/// a blocking thread.
const CLIP_BUILDS: usize = 2;

async fn h_tacview(ctx: Arc<RangeCtx>, id: String) -> Response {
    static STREAMS: std::sync::LazyLock<Arc<tokio::sync::Semaphore>> =
        std::sync::LazyLock::new(|| {
            Arc::new(tokio::sync::Semaphore::new(TACVIEW_STREAMS))
        });
    use tokio::io::AsyncReadExt;
    let id = decode_seg(&id);
    let s = match block_in_place(|| ctx.store.get(&id)) {
        Ok(Some(s)) => s,
        Ok(None) => return err(StatusCode::NOT_FOUND, format!("no range result {id:?}")),
        Err(e) => return internal(e),
    };
    let Some(dir) =
        ctx.db.instances().get(&s.instance).and_then(|c| c.tacview_dir.clone())
    else {
        return err(
            StatusCode::NOT_FOUND,
            "no Tacview recordings are published for this range",
        );
    };
    let ts = SystemTime::UNIX_EPOCH + Duration::from_millis(s.ts_ms);
    let Some(path) = block_in_place(|| pick_acmi(&dir, ts)) else {
        return err(StatusCode::NOT_FOUND, "no Tacview recording covers this result");
    };
    let Ok(permit) = STREAMS.clone().try_acquire_owned() else {
        return err(
            StatusCode::SERVICE_UNAVAILABLE,
            "too many recordings are downloading right now -- try again in a minute",
        );
    };
    let file = match tokio::fs::File::open(&path).await {
        Ok(f) => f,
        Err(e) => {
            return err(StatusCode::NOT_FOUND, format!("cannot open the recording: {e}"))
        }
    };
    let len = file.metadata().await.map(|m| m.len()).ok();
    // the permit rides along with the file and is released with it
    let stream = futures::stream::unfold(Some((file, permit)), |f| async move {
        let (mut f, permit) = f?;
        let mut buf = vec![0u8; 256 * 1024];
        match f.read(&mut buf).await {
            Ok(0) => None,
            Ok(n) => {
                buf.truncate(n);
                Some((
                    Ok::<_, std::io::Error>(bytes::Bytes::from(buf)),
                    Some((f, permit)),
                ))
            }
            Err(e) => Some((Err(e), None)),
        }
    });
    let name = path
        .file_name()
        .and_then(|n| n.to_str())
        .unwrap_or("range.acmi")
        .replace('"', "");
    let mut r = warp::reply::stream(stream).into_response();
    let h = r.headers_mut();
    h.insert(header::CONTENT_TYPE, HeaderValue::from_static("application/octet-stream"));
    if let Ok(v) = HeaderValue::from_str(&format!("attachment; filename=\"{name}\"")) {
        h.insert(header::CONTENT_DISPOSITION, v);
    }
    if let Some(l) = len {
        h.insert(header::CONTENT_LENGTH, HeaderValue::from(l));
    }
    r
}

// ── debrief clips ──────────────────────────────────────────────────────

/// Seconds the result's own track covers (the earliest to the latest `t`
/// anywhere in it), for how far back the clip reaches.
fn track_span_s(v: &Value) -> Option<f64> {
    fn walk(v: &Value, lo: &mut f64, hi: &mut f64) {
        match v {
            Value::Object(m) => {
                if let Some(t) = m.get("t").and_then(Value::as_f64) {
                    *lo = lo.min(t);
                    *hi = hi.max(t);
                }
                m.values().for_each(|x| walk(x, lo, hi));
            }
            Value::Array(a) => a.iter().for_each(|x| walk(x, lo, hi)),
            _ => (),
        }
    }
    let (mut lo, mut hi) = (f64::INFINITY, f64::NEG_INFINITY);
    walk(v, &mut lo, &mut hi);
    (hi >= lo).then_some(hi - lo)
}

fn clip_cache_path(ctx: &RangeCtx, id: &str) -> std::path::PathBuf {
    let safe: String = id
        .chars()
        .map(|c| if c.is_ascii_alphanumeric() || c == '-' || c == '_' { c } else { '_' })
        .collect();
    ctx.clip_dir.join(format!("{safe}.json.gz"))
}

fn gzip(bytes: &[u8]) -> anyhow::Result<Vec<u8>> {
    use flate2::{write::GzEncoder, Compression};
    use std::io::Write;
    let mut gz = GzEncoder::new(Vec::with_capacity(bytes.len() / 4), Compression::new(6));
    gz.write_all(bytes)?;
    Ok(gz.finish()?)
}

/// The finished clip from the cache, as its gzip'd bytes and decoded.
fn cached_clip(path: &std::path::Path) -> Option<(Vec<u8>, clip::Clip)> {
    use std::io::Read;
    let gz = std::fs::read(path).ok()?;
    let mut raw = vec![];
    flate2::read::GzDecoder::new(&gz[..]).read_to_end(&mut raw).ok()?;
    let c: clip::Clip = serde_json::from_slice(&raw).ok()?;
    (c.v == clip::CLIP_VERSION).then_some((gz, c))
}

/// `/api/range/result/<id>/clip` (gzip'd JSON for the 3D debrief) and
/// `.../clip.acmi` (the same cut as a Tacview file). Cut on first ask from
/// the server's recording; kept once the recording has gone past the end of
/// the window, re-cut on every ask until then.
async fn h_clip(ctx: Arc<RangeCtx>, id: String, as_acmi: bool) -> Response {
    let id = decode_seg(&id);
    let s = match block_in_place(|| ctx.store.full(&id)) {
        Ok(Some(s)) => s,
        Ok(None) => return err(StatusCode::NOT_FOUND, format!("no range result {id:?}")),
        Err(e) => return internal(e),
    };
    let cache = clip_cache_path(&ctx, &id);
    let (gz, c) = match block_in_place(|| cached_clip(&cache)) {
        Some(hit) => hit,
        None => {
            let Some(dir) =
                ctx.db.instances().get(&s.instance).and_then(|c| c.tacview_dir.clone())
            else {
                return err(
                    StatusCode::NOT_FOUND,
                    "no Tacview recordings are published for this range",
                );
            };
            let ts = SystemTime::UNIX_EPOCH + Duration::from_millis(s.ts_ms);
            let Some(path) = block_in_place(|| pick_acmi(&dir, ts)) else {
                return err(
                    StatusCode::NOT_FOUND,
                    "no Tacview recording covers this result",
                );
            };
            let span = s.raw.get("track").and_then(track_span_s).unwrap_or(60.0);
            let req = clip::ClipReq {
                pilot: s.pilot_name().to_string(),
                unit_type: s.unit_type().to_string(),
                ts_ms: s.ts_ms as i64,
                mission_date: s
                    .raw
                    .get("mission_date")
                    .and_then(Value::as_str)
                    .unwrap_or("")
                    .to_string(),
                mission_time: s
                    .raw
                    .get("mission_time")
                    .and_then(Value::as_str)
                    .unwrap_or("")
                    .to_string(),
                lead_s: (span + 25.0).clamp(40.0, 900.0),
                tail_s: 15.0,
            };
            static BUILDS: std::sync::LazyLock<tokio::sync::Semaphore> =
                std::sync::LazyLock::new(|| tokio::sync::Semaphore::new(CLIP_BUILDS));
            // Wait a while for a slot rather than refusing at once: a debrief
            // page asks once and shows a spinner.
            let permit =
                match tokio::time::timeout(Duration::from_secs(30), BUILDS.acquire())
                    .await
                {
                    Ok(Ok(p)) => p,
                    _ => return err(
                        StatusCode::SERVICE_UNAVAILABLE,
                        "the server is busy cutting other clips -- try again in a minute",
                    ),
                };
            let built =
                tokio::task::spawn_blocking(move || clip::build(&path, &req)).await;
            drop(permit);
            let c = match built {
                Ok(Ok(c)) => c,
                // the outermost message is the one written for the reader
                // ("... is not in the recording"); the chain is for the log
                Ok(Err(e)) => {
                    log::info!("range: no clip for {id}: {e:#}");
                    return err(StatusCode::NOT_FOUND, format!("{e}"));
                }
                Err(e) => return internal(anyhow::anyhow!("clip task failed: {e}")),
            };
            let gz = match serde_json::to_vec(&c)
                .map_err(anyhow::Error::from)
                .and_then(|j| gzip(&j))
            {
                Ok(g) => g,
                Err(e) => return internal(e),
            };
            if c.complete {
                let (dir, tmp) = (
                    ctx.clip_dir.clone(),
                    cache.with_extension(format!("tmp{}", std::process::id())),
                );
                let written = block_in_place(|| {
                    std::fs::create_dir_all(&dir)?;
                    std::fs::write(&tmp, &gz)?;
                    std::fs::rename(&tmp, &cache)
                });
                if let Err(e) = written {
                    log::warn!("range: cannot cache the clip for {id}: {e}");
                }
            }
            (gz, c)
        }
    };
    let max_age = if c.complete { "public, max-age=86400" } else { "no-store" };
    let mut r = if as_acmi {
        let title = format!(
            "{} · {} · {}",
            s.pilot_name(),
            s.kind(),
            s.raw.get("mission_time").and_then(Value::as_str).unwrap_or("")
        );
        let text = clip::to_acmi(&c, &title);
        let mut r = Response::new(text.into());
        let name: String =
            format!("{}-{}-{}", s.pilot_name(), s.kind(), s.ts().format("%Y%m%d-%H%M%S"))
                .chars()
                .map(|ch| if ch.is_ascii_alphanumeric() || ch == '-' { ch } else { '_' })
                .collect();
        let h = r.headers_mut();
        h.insert(
            header::CONTENT_TYPE,
            HeaderValue::from_static("application/octet-stream"),
        );
        if let Ok(v) =
            HeaderValue::from_str(&format!("attachment; filename=\"{name}.txt.acmi\""))
        {
            h.insert(header::CONTENT_DISPOSITION, v);
        }
        r
    } else {
        let mut r = Response::new(gz.into());
        let h = r.headers_mut();
        h.insert(header::CONTENT_TYPE, HeaderValue::from_static("application/json"));
        h.insert(header::CONTENT_ENCODING, HeaderValue::from_static("gzip"));
        r
    };
    r.headers_mut().insert(header::CACHE_CONTROL, HeaderValue::from_static(max_age));
    r
}

// ── the dashboard's flight replay (web Tacview) ────────────────────────

/// `/api/range/result/<id>/replay`: where this result is in the dashboard's
/// flight replay -- the recording, the pilot's object in it, and a moment a
/// little before the result -- so the debrief can open the replay there.
/// 404 until the recording has been read into the replay (it is, once the
/// server stops writing it).
async fn h_replay_link(ctx: Arc<RangeCtx>, id: String) -> Response {
    let id = decode_seg(&id);
    let s = match block_in_place(|| ctx.store.get(&id)) {
        Ok(Some(s)) => s,
        Ok(None) => return err(StatusCode::NOT_FOUND, format!("no range result {id:?}")),
        Err(e) => return internal(e),
    };
    let Some(rp) = ctx.replay.get() else {
        return err(StatusCode::NOT_FOUND, "flight replay is off on this server");
    };
    let names = vec![s.pilot_name().to_string()];
    let flights = match block_in_place(|| rp.flights_for(&names, 400)) {
        Ok(f) => f,
        Err(e) => return internal(e),
    };
    let ts = s.ts_ms as i64;
    let slack = 60_000;
    let best = flights
        .iter()
        .filter(|f| {
            f.instance == s.instance && f.start_ms - slack <= ts && ts <= f.end_ms + slack
        })
        .min_by_key(|f| {
            (
                !f.aircraft.eq_ignore_ascii_case(s.unit_type()),
                (f.end_ms - f.start_ms).abs(),
            )
        });
    let Some(f) = best else {
        return err(StatusCode::NOT_FOUND, "the replay has not read the recording with this result yet: it does once the server stops writing it");
    };
    let rec_start = f.start_ms - f.t0;
    let t = (ts - rec_start - 25_000).max(f.t0);
    ok(&json!({
        "rec": f.rec,
        "focus": f.i,
        "t": t,
        "path": format!("/replay/{}?focus={}&t={}", urlencoding::encode(&f.rec), f.i, t),
    }))
}

/// `/api/range/replay/<rec>/results`: every range result graded during one
/// recording, with its moment in it (ms from the recording start), for the
/// dashboard's replay to list and jump to.
async fn h_replay_results(ctx: Arc<RangeCtx>, rec: String) -> Response {
    let rec = decode_seg(&rec);
    let Some(rp) = ctx.replay.get() else {
        return err(StatusCode::NOT_FOUND, "flight replay is off on this server");
    };
    let sum = match block_in_place(|| rp.summary(&rec)) {
        Ok(Some(s)) => s,
        Ok(None) => return err(StatusCode::NOT_FOUND, format!("no recording {rec:?}")),
        Err(e) => return internal(e),
    };
    let (from, to) =
        (sum.start_ms.max(0) as u64, (sum.start_ms + sum.duration_ms).max(0) as u64);
    let mut items = vec![];
    let r = block_in_place(|| {
        ctx.store.walk_newest(&[sum.instance.clone()], from, Some((to + 1, "")), |s| {
            let v = boards::summary(&ctx.store, &s);
            items.push(json!({
                "id": s.id,
                "kind": s.kind(),
                "pilot": s.pilot_name(),
                "ucid": s.ucid(),
                "unit_type": s.unit_type(),
                "t": s.ts_ms as i64 - sum.start_ms,
                "headline": v.get("headline").cloned().unwrap_or(Value::Null),
                "score": v.get("score").cloned().unwrap_or(Value::Null),
            }));
            items.len() < 500
        })
    });
    if let Err(e) = r {
        return internal(e);
    }
    items.reverse();
    ok(&json!({ "items": items, "site": ctx.site_url }))
}

/// The spoken ATC's radio transcript, frequency by frequency.
/// `?since=<unix ms>` returns only what is new.
async fn h_atc(ctx: Arc<RangeCtx>, q: Q) -> Response {
    let sc = scope_or_400!(ctx, q);
    let since = num::<i64>(&q, "since").unwrap_or(0);
    ok(&crate::worker::hub::atc_transcript(&sc.primary().id, since).await)
}

/// The live ATC feed (LiveATC style): every transmission on `?station=`
/// (a station id, several comma-separated, or `all`) as it is said, over a
/// WebSocket.
async fn h_atc_live(
    ws: warp::ws::Ws,
    ip: std::net::IpAddr,
    ctx: Arc<RangeCtx>,
    q: Q,
) -> Response {
    let sc = scope_or_400!(ctx, q);
    let inst = sc.primary().id.clone();
    let station = nonempty(&q, "station").unwrap_or("all").to_string();
    // Counted against the caller's per-IP WebSocket allowance as well as the
    // feed's own listener cap, so one address cannot take every listener.
    let Some(ip_slot) = crate::websec::ws_slot(ip) else {
        return crate::websec::ws_refused(
            StatusCode::TOO_MANY_REQUESTS,
            "too many open connections",
        );
    };
    let Some(slot) = crate::milatc::live::Slot::take() else {
        return crate::websec::ws_refused(
            StatusCode::SERVICE_UNAVAILABLE,
            "too many listeners on the ATC feed",
        );
    };
    ws.on_upgrade(move |socket| async move {
        let _slot = slot;
        let _ip_slot = ip_slot;
        atc_live(socket, inst, station).await
    })
    .into_response()
}

async fn atc_live(ws: warp::ws::WebSocket, inst: String, station: String) {
    use crate::milatc::live::{self, Wire};
    let tuned: Option<std::collections::HashSet<String>> =
        (station != "all").then(|| {
            station
                .split(',')
                .map(|s| s.trim().to_string())
                .filter(|s| !s.is_empty())
                .collect()
        });
    use futures::{SinkExt, StreamExt};
    use warp::ws::Message;
    let (mut sink, mut stream) = ws.split();
    let mut rx = live::subscribe(&inst);
    // proxies close a silent socket: keep it warm between transmissions
    let mut ping = tokio::time::interval(Duration::from_secs(20));
    loop {
        tokio::select! {
            f = rx.recv() => match f {
                Ok(f) => {
                    if tuned.as_ref().map_or(false, |t| !t.contains(f.station())) {
                        continue;
                    }
                    let m = match live::wire(&f) {
                        Wire::Text(t) => Message::text(t),
                        Wire::Binary(b) => Message::binary(b),
                    };
                    if sink.send(m).await.is_err() {
                        break;
                    }
                }
                Err(tokio::sync::broadcast::error::RecvError::Lagged(_)) => continue,
                Err(_) => break,
            },
            m = stream.next() => match m {
                Some(Ok(m)) if m.is_close() => break,
                Some(Ok(_)) => {}
                _ => break,
            },
            _ = ping.tick() => {
                if sink.send(Message::ping(Vec::<u8>::new())).await.is_err() {
                    break;
                }
            }
        }
    }
}

/// One ATC transmission's audio, as WAV. Kept for the last half hour or so.
async fn h_atc_audio(ctx: Arc<RangeCtx>, q: Q, id: u64) -> Response {
    let sc = scope_or_400!(ctx, q);
    match crate::worker::hub::atc_audio(&sc.primary().id, id).await {
        Some(w) => bytes_reply((*w).clone(), "audio/wav", 86_400),
        None => err(
            StatusCode::NOT_FOUND,
            "that transmission's audio is no longer kept".to_string(),
        ),
    }
}

// ── routes ─────────────────────────────────────────────────────────────

/// Every `/api/range/*` route as one boxed filter.
pub(crate) fn routes(ctx: Arc<RangeCtx>) -> BoxedFilter<(Response,)> {
    let c = warp::any().map(move || ctx.clone());
    let q = warp::query::<Q>();
    let sid = crate::extract_session_cookie();
    let body = warp::body::content_length_limit(16 * 1024).and(warp::body::bytes());

    let live =
        warp::path!("api" / "range" / "live").and(c.clone()).and(q.clone()).then(h_live);
    let feed =
        warp::path!("api" / "range" / "feed").and(c.clone()).and(q.clone()).then(h_feed);
    let results = warp::path!("api" / "range" / "results")
        .and(c.clone())
        .and(q.clone())
        .then(h_results);
    let result = warp::path!("api" / "range" / "result" / String)
        .and(c.clone())
        .then(|id: String, ctx: Arc<RangeCtx>| h_result(ctx, id));
    let card_svg = warp::path!("api" / "range" / "result" / String / "card.svg")
        .and(c.clone())
        .then(|id: String, ctx: Arc<RangeCtx>| h_card(ctx, id, false));
    let card_png = warp::path!("api" / "range" / "result" / String / "card.png")
        .and(c.clone())
        .then(|id: String, ctx: Arc<RangeCtx>| h_card(ctx, id, true));
    let embed = warp::path!("api" / "range" / "result" / String / "discord")
        .and(c.clone())
        .then(|id: String, ctx: Arc<RangeCtx>| h_discord(ctx, id));
    let me = warp::path!("api" / "range" / "me")
        .and(c.clone())
        .and(sid.clone())
        .and(q.clone())
        .then(h_me);
    let pilot = warp::path!("api" / "range" / "pilot" / String)
        .and(c.clone())
        .and(q.clone())
        .then(|u: String, ctx: Arc<RangeCtx>, q: Q| h_pilot(ctx, u, q));
    let pilots = warp::path!("api" / "range" / "pilots")
        .and(c.clone())
        .and(q.clone())
        .then(h_pilots);
    let greenie = warp::path!("api" / "range" / "greenie")
        .and(c.clone())
        .and(q.clone())
        .then(h_greenie);
    let boards_r = warp::path!("api" / "range" / "leaderboards")
        .and(c.clone())
        .and(q.clone())
        .then(h_leaderboards);
    let impacts = warp::path!("api" / "range" / "stations" / String / "impacts")
        .and(c.clone())
        .and(q.clone())
        .then(|st: String, ctx: Arc<RangeCtx>, q: Q| h_impacts(ctx, st, q));
    let catalog = warp::path!("api" / "range" / "catalog")
        .and(c.clone())
        .and(q.clone())
        .then(h_catalog);
    let atc =
        warp::path!("api" / "range" / "atc").and(c.clone()).and(q.clone()).then(h_atc);
    let atc_live = warp::path!("api" / "range" / "atc" / "live")
        .and(crate::websec::ws_limited())
        .and(c.clone())
        .and(q.clone())
        .then(|ws, ip, ctx: Arc<RangeCtx>, q: Q| h_atc_live(ws, ip, ctx, q));
    let atc_audio = warp::path!("api" / "range" / "atc" / "audio" / u64)
        .and(c.clone())
        .and(q.clone())
        .then(|id: u64, ctx: Arc<RangeCtx>, q: Q| h_atc_audio(ctx, q, id));
    let weapons = warp::path!("api" / "range" / "weapons")
        .and(c.clone())
        .and(q.clone())
        .then(h_weapons);
    let tacview = warp::path!("api" / "range" / "tacview" / String)
        .and(c.clone())
        .then(|id: String, ctx: Arc<RangeCtx>| h_tacview(ctx, id));
    let clip_json = warp::path!("api" / "range" / "result" / String / "clip")
        .and(c.clone())
        .then(|id: String, ctx: Arc<RangeCtx>| h_clip(ctx, id, false));
    let clip_acmi = warp::path!("api" / "range" / "result" / String / "clip.acmi")
        .and(c.clone())
        .then(|id: String, ctx: Arc<RangeCtx>| h_clip(ctx, id, true));
    let replay_link = warp::path!("api" / "range" / "result" / String / "replay")
        .and(c.clone())
        .then(|id: String, ctx: Arc<RangeCtx>| h_replay_link(ctx, id));
    let replay_results = warp::path!("api" / "range" / "replay" / String / "results")
        .and(c.clone())
        .then(|rec: String, ctx: Arc<RangeCtx>| h_replay_results(ctx, rec));

    let spawn = warp::path!("api" / "range" / "spawn")
        .and(warp::post())
        .and(c.clone())
        .and(q.clone())
        .and(sid.clone())
        .and(body.clone())
        .then(h_spawn);
    let despawn = warp::path!("api" / "range" / "despawn")
        .and(warp::post())
        .and(c.clone())
        .and(q.clone())
        .and(sid.clone())
        .and(body.clone())
        .then(h_despawn);
    let reset = warp::path!("api" / "range" / "admin" / "reset-station")
        .and(warp::post())
        .and(c.clone())
        .and(q.clone())
        .and(sid.clone())
        .and(body.clone())
        .then(h_reset_station);

    // Boxed in small groups: each `.or` nests the type one level deeper.
    let reads_a = live
        .or(feed)
        .unify()
        .or(results)
        .unify()
        .or(result)
        .unify()
        .or(card_svg)
        .unify()
        .or(card_png)
        .unify()
        .boxed();
    let reads_b = embed
        .or(me)
        .unify()
        .or(pilot)
        .unify()
        .or(pilots)
        .unify()
        .or(greenie)
        .unify()
        .or(boards_r)
        .unify()
        .or(impacts)
        .unify()
        .boxed();
    let reads_c = catalog
        .or(atc)
        .unify()
        .or(atc_live)
        .unify()
        .or(atc_audio)
        .unify()
        .or(weapons)
        .unify()
        .or(tacview)
        .unify()
        .or(clip_json)
        .unify()
        .or(clip_acmi)
        .unify()
        .or(replay_link)
        .unify()
        .or(replay_results)
        .unify()
        .boxed();
    let reads = warp::get().and(reads_a.or(reads_b).unify().or(reads_c).unify()).boxed();
    let writes = spawn.or(despawn).unify().or(reset).unify().boxed();
    // Anything else under /api/range/ is a JSON 404 rather than falling
    // through to the SPA catch-all's index.html.
    let unknown = warp::path!("api" / "range" / ..).and(warp::path::tail()).map(
        |t: warp::path::Tail| {
            err(StatusCode::NOT_FOUND, format!("unknown range route /{}", t.as_str()))
        },
    );
    reads.or(writes).unify().or(unknown).unify().boxed()
}

#[cfg(test)]
mod tests {
    use super::*;
    use bfprotocols::range::RangeRecord;

    #[test]
    fn acmi_pick_prefers_the_recording_in_progress() {
        let dir =
            std::env::temp_dir().join(format!("bfdb-range-acmi-{}", std::process::id()));
        std::fs::create_dir_all(&dir).unwrap();
        std::fs::write(dir.join("a.zip.acmi"), b"x").unwrap();
        std::fs::write(dir.join("notes.txt"), b"x").unwrap();
        let now = SystemTime::now();
        let p = pick_acmi(&dir, now - Duration::from_secs(10)).unwrap();
        assert!(p.ends_with("a.zip.acmi"));
        // an event long after every file was last written has no recording
        assert!(pick_acmi(&dir, now + Duration::from_secs(3600)).is_none());
        let _ = std::fs::remove_dir_all(&dir);
    }

    #[test]
    fn record_json_round_trips_through_the_embed_types() {
        let r = crate::range::cards::tests::bomb();
        let v = serde_json::to_value(&r).unwrap();
        let back: RangeRecord = serde_json::from_value(v).unwrap();
        assert_eq!(back.kind(), "bomb");
    }
}
