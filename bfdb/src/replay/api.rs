// Copyright (c) 2026 Dillen Weerasinghe. All rights reserved.
// Proprietary and confidential. No license is granted to use, copy, modify, or
// distribute this file. See NOTICE section 3 and the repository NOTICE file.
//! `/api/replay/*` -- the dashboard's flight replay viewer.
//!
//! Public, like the pilot pages: a replay shows the whole picture, no fog of
//! war. Recordings of a non-public instance (a test server) are admin-only.
//! Track files are served exactly as stored, gzip'd, with
//! `Content-Encoding: gzip`, so the browser inflates them for free.
//!
//! - `GET /api/replay/status`                      per-instance scanner state
//! - `GET /api/replay/recordings?instance=&before=&limit=`
//! - `GET /api/replay/rec/<id>`                    the recording's meta JSON
//! - `GET /api/replay/rec/<id>/chunk/<n>`          one five-minute track window
//! - `GET /api/replay/rec/<id>/object/<idx>?t0=&t1=`  one object's whole track
//!   (its lifetime from the meta, so the server reads only those chunks)
//! - `GET /api/replay/pilot/<ucid>?limit=`         a pilot's flights, newest first

use super::ReplayCtx;
use crate::instance::InstanceCfg;
use serde_json::json;
use std::{collections::HashMap, sync::Arc};
use tokio::task::block_in_place;
use uuid::Uuid;
use warp::{
    filters::BoxedFilter,
    http::{header, HeaderValue, StatusCode},
    reply::Response,
    Filter, Reply,
};

type Q = HashMap<String, String>;

fn reply_json(code: StatusCode, v: &impl serde::Serialize) -> Response {
    let mut r = warp::reply::with_status(warp::reply::json(v), code).into_response();
    r.headers_mut().insert(header::CACHE_CONTROL, HeaderValue::from_static("no-store"));
    r
}

fn err(code: StatusCode, msg: impl std::fmt::Display) -> Response {
    reply_json(code, &json!({ "error": msg.to_string() }))
}

fn internal(e: anyhow::Error) -> Response {
    log::warn!("replay api: {e:#}");
    err(StatusCode::INTERNAL_SERVER_ERROR, format!("{e:#}"))
}

/// May this caller see this instance's recordings?
fn visible(ctx: &ReplayCtx, cfg: &InstanceCfg, sid: Option<Uuid>) -> bool {
    cfg.public || crate::session_is_admin(&ctx.db, sid)
}

fn instance(ctx: &ReplayCtx, q: &Q) -> Result<Arc<InstanceCfg>, Response> {
    let reg = ctx.db.instances();
    if let Some(name) = q.get("server") {
        if let Some(c) = reg.by_dcs_server_name(name) {
            return Ok(c.clone());
        }
    }
    reg.resolve(q.get("instance").map(String::as_str))
        .cloned()
        .map_err(|e| err(StatusCode::BAD_REQUEST, e))
}

async fn h_status(ctx: Arc<ReplayCtx>, sid: Option<Uuid>) -> Response {
    let r = block_in_place(|| -> anyhow::Result<Vec<serde_json::Value>> {
        let mut out = vec![];
        for cfg in ctx.db.instances().all() {
            if cfg.is_range() || !visible(&ctx, cfg, sid) {
                continue;
            }
            out.push(ctx.status(cfg)?);
        }
        Ok(out)
    });
    match r {
        Ok(v) => reply_json(StatusCode::OK, &json!({ "instances": v, "retention_days": ctx.days })),
        Err(e) => internal(e),
    }
}

async fn h_recordings(ctx: Arc<ReplayCtx>, sid: Option<Uuid>, q: Q) -> Response {
    let cfg = match instance(&ctx, &q) {
        Ok(c) => c,
        Err(r) => return r,
    };
    if !visible(&ctx, &cfg, sid) {
        return err(StatusCode::NOT_FOUND, "no such server");
    }
    let before = q.get("before").and_then(|s| s.parse().ok());
    let limit = q.get("limit").and_then(|s| s.parse().ok()).unwrap_or(50usize).clamp(1, 500);
    match block_in_place(|| ctx.recordings(&cfg.id, before, limit)) {
        Ok(v) => reply_json(StatusCode::OK, &v),
        Err(e) => internal(e),
    }
}

/// Serve one of a recording's stored files.
async fn serve(ctx: Arc<ReplayCtx>, sid: Option<Uuid>, id: String, file: String, ctype: &'static str) -> Response {
    let sum = match block_in_place(|| ctx.summary(&id)) {
        Ok(Some(s)) => s,
        Ok(None) => return err(StatusCode::NOT_FOUND, format!("no recording {id:?}")),
        Err(e) => return internal(e),
    };
    let ok = ctx.db.instances().get(&sum.instance).map(|c| visible(&ctx, c, sid)).unwrap_or(false);
    if !ok {
        return err(StatusCode::NOT_FOUND, format!("no recording {id:?}"));
    }
    let Some(dir) = ctx.rec_dir(&id) else {
        return err(StatusCode::NOT_FOUND, "the recording's files are gone");
    };
    let bytes = match tokio::fs::read(dir.join(&file)).await {
        Ok(b) => b,
        Err(e) if e.kind() == std::io::ErrorKind::NotFound => {
            return err(StatusCode::NOT_FOUND, format!("no {file} in this recording"))
        }
        Err(e) => return internal(e.into()),
    };
    let mut r = Response::new(bytes.into());
    let h = r.headers_mut();
    h.insert(header::CONTENT_TYPE, HeaderValue::from_static(ctype));
    h.insert(header::CONTENT_ENCODING, HeaderValue::from_static("gzip"));
    // A processed recording never changes (a re-process gets a new file set
    // under the same id only if the source changed, which is rare); a day of
    // private caching makes scrubbing back and forth free.
    h.insert(header::CACHE_CONTROL, HeaderValue::from_static("private, max-age=86400"));
    r
}

/// Stitch (once) and serve one object's whole track.
async fn h_object(ctx: Arc<ReplayCtx>, sid: Option<Uuid>, id: String, idx: u32, q: Q) -> Response {
    let sum = match block_in_place(|| ctx.summary(&id)) {
        Ok(Some(s)) => s,
        Ok(None) => return err(StatusCode::NOT_FOUND, format!("no recording {id:?}")),
        Err(e) => return internal(e),
    };
    if sum.chunks == 0 {
        return err(StatusCode::NOT_FOUND, "empty recording");
    }
    let chunk_of = |k: &str| -> u32 {
        let t: i64 = q.get(k).and_then(|s| s.parse().ok()).unwrap_or(0);
        ((t.max(0) / super::ingest::CHUNK_MS) as u32).min(sum.chunks - 1)
    };
    let (first, last) = (chunk_of("t0"), chunk_of("t1"));
    if q.get("t1").is_none() || first > last {
        return err(StatusCode::BAD_REQUEST, "t0 and t1 (the object's lifetime, ms) are required");
    }
    if let Some(dir) = ctx.rec_dir(&id) {
        if let Err(e) = block_in_place(|| super::ingest::object_track(&dir, idx, first, last)) {
            return internal(e);
        }
    }
    serve(ctx, sid, id, format!("o{idx}.bin.gz"), "application/octet-stream").await
}

async fn h_pilot(ctx: Arc<ReplayCtx>, sid: Option<Uuid>, ucid: String, q: Q) -> Response {
    let Ok(u) = ucid.parse::<dcso3::net::Ucid>() else {
        return err(StatusCode::BAD_REQUEST, "not a valid ucid");
    };
    let limit = q.get("limit").and_then(|s| s.parse().ok()).unwrap_or(100usize).clamp(1, 1000);
    let r = block_in_place(|| {
        let names = ctx.db.pilot_names(&u);
        ctx.flights_for(&names, limit)
    });
    match r {
        Ok(mut v) => {
            let reg = ctx.db.instances();
            v.retain(|f| {
                // A same-named pilot is not this one.
                f.ucid.as_deref().map(|x| x == ucid).unwrap_or(true)
                    && reg.get(&f.instance).map(|c| visible(&ctx, c, sid)).unwrap_or(false)
            });
            reply_json(StatusCode::OK, &v)
        }
        Err(e) => internal(e),
    }
}

/// Every `/api/replay/*` route as one boxed filter.
pub(crate) fn routes(ctx: Arc<ReplayCtx>) -> BoxedFilter<(Response,)> {
    let c = warp::any().map(move || ctx.clone());
    let q = warp::query::<Q>();
    let sid = crate::extract_session_cookie();

    let status = warp::path!("api" / "replay" / "status").and(c.clone()).and(sid.clone()).then(h_status);
    let recs = warp::path!("api" / "replay" / "recordings")
        .and(c.clone())
        .and(sid.clone())
        .and(q.clone())
        .then(h_recordings);
    let meta = warp::path!("api" / "replay" / "rec" / String).and(c.clone()).and(sid.clone()).then(
        |id: String, ctx: Arc<ReplayCtx>, sid: Option<Uuid>| serve(ctx, sid, id, "meta.json.gz".into(), "application/json"),
    );
    let chunk = warp::path!("api" / "replay" / "rec" / String / "chunk" / u32).and(c.clone()).and(sid.clone()).then(
        |id: String, n: u32, ctx: Arc<ReplayCtx>, sid: Option<Uuid>| {
            serve(ctx, sid, id, format!("c{n}.bin.gz"), "application/octet-stream")
        },
    );
    let object = warp::path!("api" / "replay" / "rec" / String / "object" / u32)
        .and(c.clone())
        .and(sid.clone())
        .and(q.clone())
        .then(|id: String, idx: u32, ctx: Arc<ReplayCtx>, sid: Option<Uuid>, q: Q| h_object(ctx, sid, id, idx, q));
    let pilot = warp::path!("api" / "replay" / "pilot" / String).and(c.clone()).and(sid.clone()).and(q.clone()).then(
        |u: String, ctx: Arc<ReplayCtx>, sid: Option<Uuid>, q: Q| h_pilot(ctx, sid, u, q),
    );
    let reads = status.or(recs).unify().or(meta).unify().or(chunk).unify().or(object).unify().or(pilot).unify().boxed();
    // Anything else under /api/replay/ is a JSON 404 rather than the SPA.
    let unknown = warp::path!("api" / "replay" / ..)
        .and(warp::path::tail())
        .map(|t: warp::path::Tail| err(StatusCode::NOT_FOUND, format!("unknown replay route /{}", t.as_str())));
    warp::get().and(reads.or(unknown).unify()).boxed()
}
