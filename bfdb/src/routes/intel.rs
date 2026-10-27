//! Recon intel captures, markup and flight plans.

use crate::*;

// ── Recon intel (TARPS) API ────────────────────────────────────────────────
//
// A shared reconnaissance picture built from F-14 TARPS photos, contributed
// through the dashboard login and locked to the contributor's coalition.
// Everything is scoped to the active round and wiped by a campaign reset.

/// A TARPS frame is a PNG screenshot; 6 MiB covers a 1440p capture. Was 8,
/// which with 400 per side per round could put ~6 GB into one round.
pub(crate) const MAX_INTEL_IMAGE_BYTES: u64 = 6 * 1024 * 1024;
pub(crate) const MAX_INTEL_CAPTURES_PER_SIDE: usize = 400;
/// One pilot's share of a side's recon picture per round (admins exempt).
pub(crate) const MAX_INTEL_CAPTURES_PER_USER: usize = 120;

/// Recon uploads per Discord user: 40 in 10 minutes is a full TARPS run
/// uploaded in one go, and stops a script filling the disk.
pub(crate) static INTEL_UPLOADS: std::sync::LazyLock<websec::RateLimiter<std::string::String>> =
    std::sync::LazyLock::new(|| {
        websec::RateLimiter::new(
            40,
            std::time::Duration::from_secs(600),
            std::time::Duration::from_secs(300),
        )
    });

pub(crate) fn intel_json(id: &Uuid, c: &IntelCapture, viewer_discord: &str, viewer_admin: bool) -> serde_json::Value {
    serde_json::json!({
        "id":               id.to_string(),
        "side":             format!("{:?}", c.side),
        "image_url":        format!("/api/intel/images/{id}"),
        "uploaded_by_name": c.uploaded_by_name,
        "uploaded_at":      c.uploaded_at.to_rfc3339(),
        "captured_at":      c.captured_at.map(|t| t.to_rfc3339()),
        "filename":         c.filename,
        "placed":           c.placed,
        "lat":              c.lat,
        "lon":              c.lon,
        "alt_ft":           c.alt_ft,
        "heading_deg":      c.heading_deg,
        "pitch_deg":        c.pitch_deg,
        "roll_deg":         c.roll_deg,
        "adjust":           c.adjust,
        "note":             c.note,
        // may this viewer edit/delete it?
        "mine":             viewer_admin || c.uploaded_by == viewer_discord,
    })
}

/// GET /api/intel/captures — the caller's coalition captures for the active
/// round. A dashboard admin with no coalition of their own may pass
/// `?side=blue|red|all`; a registered player only ever sees their own side.
pub(crate) async fn api_intel_list(
    session_id: Option<Uuid>,
    query: std::collections::HashMap<std::string::String, std::string::String>,
    db: StatsDb,
    bot_cfg: Arc<Option<BotLinkConfig>>,
    inst: Inst,
) -> std::result::Result<impl warp::Reply, Error> {
    let c = require_coalition(&query, session_id, &db, &bot_cfg, &inst).await?;
    let round = active_round_or_err(&db, &inst)?;
    let filter = if c.god_mode {
        match query.get("side").map(|s| s.as_str()) {
            Some("all") => None,
            Some(s) => parse_side(s).or(Some(c.side)),
            None => Some(c.side),
        }
    } else {
        Some(c.side)
    };
    let caps = task::block_in_place(|| db.intel_list(round, filter))?;
    let json: Vec<_> = caps
        .iter()
        .map(|(id, cap)| intel_json(id, cap, &c.session.discord_id, c.session.is_admin))
        .collect();
    Ok(warp::reply::json(&json))
}

/// POST /api/intel/upload — body is the raw image bytes; the TARPS filename
/// comes in the `x-intel-filename` header (URL-encoded).
pub(crate) async fn api_intel_upload(
    session_id: Option<Uuid>,
    query: std::collections::HashMap<std::string::String, std::string::String>,
    filename_hdr: Option<std::string::String>,
    content_type: std::string::String,
    body: bytes::Bytes,
    db: StatsDb,
    bot_cfg: Arc<Option<BotLinkConfig>>,
    inst: Inst,
) -> std::result::Result<impl warp::Reply, Error> {
    let c = require_coalition(&query, session_id, &db, &bot_cfg, &inst).await?;
    // Any coalition member can upload, and every member of that coalition
    // loads the result -- so the type is decided by the bytes, and only
    // raster formats that cannot carry script are let in.
    let Some(sniffed) = websec::sniff_image(&body) else {
        return Err(websec::unsupported_media(format!(
            "only PNG, JPEG or WebP images are accepted (declared '{content_type}')"
        ))
        .into());
    };
    if !c.session.is_admin && !INTEL_UPLOADS.hit(&c.session.discord_id) {
        return Err(websec::too_many(
            "too many recon uploads in a short time -- wait a few minutes",
        )
        .into());
    }
    // A registered player uploads into their own side; only a no-coalition
    // admin may direct an upload with `?side=`.
    let side = c.side;
    let round = active_round_or_err(&db, &inst)?;
    let (side_count, mine) = task::block_in_place(|| {
        db.intel_counts(round, side, &c.session.discord_id)
    })?;
    if side_count >= MAX_INTEL_CAPTURES_PER_SIDE {
        return Err(websec::too_many(format!(
            "recon intel limit reached for this coalition this round ({MAX_INTEL_CAPTURES_PER_SIDE})"
        ))
        .into());
    }
    if !c.session.is_admin && mine >= MAX_INTEL_CAPTURES_PER_USER {
        return Err(websec::too_many(format!(
            "you have uploaded {mine} recon photos this round, the most one pilot may \
             ({MAX_INTEL_CAPTURES_PER_USER}) -- delete some first"
        ))
        .into());
    }
    let content_type = sniffed.to_string();
    let filename = filename_hdr
        .as_deref()
        .map(|h| urlencoding::decode(h).map(|c| c.into_owned()).unwrap_or_else(|_| h.to_string()))
        .unwrap_or_default();
    let parsed = intel::parse_filename(&filename);
    let name = c
        .ucid()
        .and_then(|u| db.pilot_name(u))
        .unwrap_or_else(|| c.session.username.clone());
    let image_id = Uuid::new_v4();
    let cap_id = Uuid::new_v4();
    let cap = IntelCapture {
        round,
        side,
        image_id,
        uploaded_by: c.session.discord_id.clone(),
        uploaded_by_name: name,
        uploaded_at: chrono::Utc::now(),
        captured_at: parsed.captured_at,
        filename: filename.clone(),
        placed: parsed.has_position(),
        lat: parsed.lat.unwrap_or(0.0),
        lon: parsed.lon.unwrap_or(0.0),
        alt_ft: parsed.alt_ft,
        heading_deg: parsed.heading_deg,
        pitch_deg: parsed.pitch_deg,
        roll_deg: parsed.roll_deg,
        adjust: None,
        note: None,
    };
    task::block_in_place(|| {
        db.intel_put_image(image_id, content_type, body.to_vec())?;
        db.intel_put(cap_id, cap.clone())
    })?;
    log::info!(
        "INTEL: {} uploaded capture {cap_id} ({filename:?}) for {side:?} (placed={})",
        c.session.discord_id,
        cap.placed
    );
    Ok(warp::reply::json(&intel_json(
        &cap_id,
        &cap,
        &c.session.discord_id,
        c.session.is_admin,
    )))
}

/// GET /api/intel/images/<capture-id> — serve the photo. A registered player
/// only ever sees their own side's photos; a no-coalition admin sees any.
pub(crate) async fn api_intel_get_image(
    id: Uuid,
    session_id: Option<Uuid>,
    query: std::collections::HashMap<std::string::String, std::string::String>,
    db: StatsDb,
    bot_cfg: Arc<Option<BotLinkConfig>>,
    inst: Inst,
) -> std::result::Result<impl warp::Reply, Error> {
    let c = require_coalition(&query, session_id, &db, &bot_cfg, &inst).await?;
    let cap = task::block_in_place(|| db.intel_get_by_id(&id))?
        .ok_or_else(|| websec::not_found("capture not found"))?;
    if !c.god_mode && cap.side != c.side {
        return Err(websec::forbidden("not authorized for this coalition's intel").into());
    }
    let (_stored_type, bytes) = task::block_in_place(|| db.intel_get_image(&cap.image_id))?
        .ok_or_else(|| websec::not_found("image not found"))?;
    // Served by what the bytes are, sandboxed: photos uploaded before the
    // upload check existed carry whatever type their uploader claimed.
    Ok(websec::serve_upload(bytes, "private, max-age=31536000, immutable"))
}

#[derive(Deserialize)]
pub(crate) struct IntelAdjustBody {
    pub(crate) id:     std::string::String,
    pub(crate) lat:    Option<f64>,
    pub(crate) lon:    Option<f64>,
    pub(crate) placed: Option<bool>,
    pub(crate) note:   Option<std::string::String>,
    pub(crate) adjust: Option<db::IntelAdjust>,
}

/// POST /api/intel/adjust — reposition / annotate a capture. Allowed for the
/// uploader, any admin moderating their own side, or a no-coalition admin.
pub(crate) async fn api_intel_adjust(
    session_id: Option<Uuid>,
    query: std::collections::HashMap<std::string::String, std::string::String>,
    body: IntelAdjustBody,
    db: StatsDb,
    bot_cfg: Arc<Option<BotLinkConfig>>,
    inst: Inst,
) -> std::result::Result<impl warp::Reply, Error> {
    let c = require_coalition(&query, session_id, &db, &bot_cfg, &inst).await?;
    let cap_id: Uuid = body.id.parse().map_err(|e| websec::bad_request(format!("bad id: {e}")))?;
    let mut cap = task::block_in_place(|| db.intel_get_by_id(&cap_id))?
        .ok_or_else(|| websec::not_found("capture not found"))?;
    let owns = cap.uploaded_by == c.session.discord_id;
    if !c.god_mode && cap.side != c.side {
        return Err(websec::forbidden("not authorized for this coalition's intel").into());
    }
    if !c.god_mode && !c.session.is_admin && !owns {
        return Err(websec::forbidden("only the uploader or an admin can edit this capture").into());
    }
    if let Some(lat) = body.lat {
        cap.lat = lat;
    }
    if let Some(lon) = body.lon {
        cap.lon = lon;
    }
    if let Some(p) = body.placed {
        cap.placed = p;
    }
    if body.lat.is_some() || body.lon.is_some() {
        cap.placed = true;
    }
    if body.note.is_some() {
        cap.note = body.note.filter(|n| !n.trim().is_empty());
    }
    if body.adjust.is_some() {
        cap.adjust = body.adjust;
    }
    task::block_in_place(|| db.intel_put(cap_id, cap.clone()))?;
    Ok(warp::reply::json(&intel_json(
        &cap_id,
        &cap,
        &c.session.discord_id,
        c.session.is_admin,
    )))
}

#[derive(Deserialize)]
pub(crate) struct IntelDeleteBody {
    pub(crate) id: std::string::String,
}

/// POST /api/intel/delete — remove a capture + its image (uploader or admin).
pub(crate) async fn api_intel_delete(
    session_id: Option<Uuid>,
    query: std::collections::HashMap<std::string::String, std::string::String>,
    body: IntelDeleteBody,
    db: StatsDb,
    bot_cfg: Arc<Option<BotLinkConfig>>,
    inst: Inst,
) -> std::result::Result<impl warp::Reply, Error> {
    let c = require_coalition(&query, session_id, &db, &bot_cfg, &inst).await?;
    let cap_id: Uuid = body.id.parse().map_err(|e| websec::bad_request(format!("bad id: {e}")))?;
    let cap = task::block_in_place(|| db.intel_get_by_id(&cap_id))?
        .ok_or_else(|| websec::not_found("capture not found"))?;
    let owns = cap.uploaded_by == c.session.discord_id;
    if !c.god_mode && cap.side != c.side {
        return Err(websec::forbidden("not authorized for this coalition's intel").into());
    }
    if !c.god_mode && !c.session.is_admin && !owns {
        return Err(websec::forbidden("only the uploader or an admin can delete this capture").into());
    }
    task::block_in_place(|| db.intel_delete(cap.round, &cap_id))?;
    log::info!("INTEL: {} deleted capture {cap_id}", c.session.discord_id);
    Ok(warp::reply::json(&serde_json::json!({"ok": true})))
}

/// POST /api/intel/purge — clear the whole recon picture (admin only).
pub(crate) async fn api_intel_purge(
    session_id: Option<Uuid>,
    db: StatsDb,
    inst: Inst,
) -> std::result::Result<impl warp::Reply, Error> {
    let who = require_role(session_id, db.clone(), admin::Role::MODERATION).await?;
    task::block_in_place(|| -> Result<()> {
        if db.instances().is_single() {
            db.intel_purge_all()
        } else {
            // Leave the other servers' recon pictures alone.
            let rounds = db.rounds_of(&inst.id)?;
            db.intel_purge_all_for(&rounds)
        }
    })?;
    audit(&db, &who, "purge recon intel", None, Some(&inst.id));
    Ok(warp::reply::json(&serde_json::json!({"ok": true})))
}

// ── Flight plans (the briefing Planner) ────────────────────────────────────

/// Plan saves per Discord user: generous for someone dragging waypoints and
/// saving as they go, short of a script filling the tree.
pub(crate) static PLAN_WRITES: std::sync::LazyLock<websec::RateLimiter<std::string::String>> =
    std::sync::LazyLock::new(|| {
        websec::RateLimiter::new(
            120,
            std::time::Duration::from_secs(600),
            std::time::Duration::from_secs(300),
        )
    });

pub(crate) fn plan_json(p: &flightplans::Stored, viewer_discord: &str, viewer_admin: bool) -> serde_json::Value {
    serde_json::json!({
        "id":      p.id.to_string(),
        "side":    p.side,
        "by_name": p.by_name,
        "created": p.created.to_rfc3339(),
        "updated": p.updated.to_rfc3339(),
        "mine":    viewer_admin || p.by == viewer_discord,
        "plan":    p.plan,
    })
}

/// The plan `id` on this instance, if the caller's coalition may see it.
pub(crate) fn plan_for_caller(db: &StatsDb, inst: &InstanceState, c: &Caller, id: &str) -> std::result::Result<flightplans::Stored, Error> {
    let id: Uuid = id.parse().map_err(|e| websec::bad_request(format!("bad id: {e}")))?;
    let p = task::block_in_place(|| db.flight_plans().get(&inst.id, &id))?
        .ok_or_else(|| websec::not_found("plan not found"))?;
    if !c.god_mode && p.side != side_name(c.side) {
        return Err(websec::not_found("plan not found").into());
    }
    Ok(p)
}

/// GET /api/plans -- the caller's coalition's flight plans, newest first.
pub(crate) async fn api_plans_list(
    session_id: Option<Uuid>,
    query: std::collections::HashMap<std::string::String, std::string::String>,
    db: StatsDb,
    bot_cfg: Arc<Option<BotLinkConfig>>,
    inst: Inst,
) -> std::result::Result<impl warp::Reply, Error> {
    let c = require_coalition(&query, session_id, &db, &bot_cfg, &inst).await?;
    let side = if c.god_mode && query.get("side").map(|s| s.as_str()) == Some("all") {
        None
    } else {
        Some(side_name(c.side))
    };
    let plans = task::block_in_place(|| db.flight_plans().list(&inst.id, side))?;
    let json: Vec<_> = plans.iter().map(|p| plan_json(p, &c.session.discord_id, c.session.is_admin)).collect();
    Ok(warp::reply::json(&json))
}

#[derive(Deserialize)]
pub(crate) struct PlanSaveBody {
    /// Absent to create a plan, set to overwrite one the caller owns.
    pub(crate) id: Option<std::string::String>,
    pub(crate) plan: flightplans::Plan,
}

/// POST /api/plans {id?, plan} -- create a plan, or update one (author or
/// admin). Shared with the author's coalition, fixed to the side it was made on.
pub(crate) async fn api_plans_save(
    session_id: Option<Uuid>,
    query: std::collections::HashMap<std::string::String, std::string::String>,
    body: PlanSaveBody,
    db: StatsDb,
    bot_cfg: Arc<Option<BotLinkConfig>>,
    inst: Inst,
) -> std::result::Result<impl warp::Reply, Error> {
    let c = require_coalition(&query, session_id, &db, &bot_cfg, &inst).await?;
    if !c.session.is_admin && !PLAN_WRITES.hit(&c.session.discord_id) {
        return Err(websec::too_many("too many plan saves; wait a few minutes").into());
    }
    let plan = flightplans::validate(body.plan).map_err(|e| websec::bad_request(e.to_string()))?;
    let now = chrono::Utc::now();
    let stored = match body.id {
        Some(id) => {
            let mut p = plan_for_caller(&db, &inst, &c, &id)?;
            if !c.session.is_admin && p.by != c.session.discord_id {
                return Err(websec::forbidden("only the author or an admin can change this plan").into());
            }
            p.plan = plan;
            p.updated = now;
            p
        }
        None => {
            let side = side_name(c.side);
            task::block_in_place(|| db.flight_plans().check_quota(&inst.id, side, &c.session.discord_id))
                .map_err(|e| websec::bad_request(e.to_string()))?;
            flightplans::Stored {
                id: Uuid::new_v4(),
                side: side.into(),
                by: c.session.discord_id.clone(),
                by_name: c.ucid().and_then(|u| db.pilot_name(u)).unwrap_or_else(|| c.session.username.clone()),
                created: now,
                updated: now,
                plan,
            }
        }
    };
    task::block_in_place(|| db.flight_plans().put(&inst.id, &stored))?;
    Ok(warp::reply::json(&plan_json(&stored, &c.session.discord_id, c.session.is_admin)))
}

/// POST /api/plans/delete {id} -- remove a plan (author or admin).
pub(crate) async fn api_plans_delete(
    session_id: Option<Uuid>,
    query: std::collections::HashMap<std::string::String, std::string::String>,
    body: IntelDeleteBody,
    db: StatsDb,
    bot_cfg: Arc<Option<BotLinkConfig>>,
    inst: Inst,
) -> std::result::Result<impl warp::Reply, Error> {
    let c = require_coalition(&query, session_id, &db, &bot_cfg, &inst).await?;
    let p = plan_for_caller(&db, &inst, &c, &body.id)?;
    if !c.god_mode && !c.session.is_admin && p.by != c.session.discord_id {
        return Err(websec::forbidden("only the author or an admin can delete this plan").into());
    }
    task::block_in_place(|| db.flight_plans().delete(&inst.id, &p.id))?;
    Ok(warp::reply::json(&serde_json::json!({"ok": true})))
}

// ── Recon intel markup ─────────────────────────────────────────────────────

pub(crate) fn markup_json(id: &Uuid, m: &IntelMarkup, viewer_discord: &str, viewer_admin: bool) -> serde_json::Value {
    serde_json::json!({
        "id":      id.to_string(),
        "kind":    m.kind,
        "points":  m.points,
        "color":   m.color,
        "width":   m.width,
        "text":    m.text,
        "by_name": m.by_name,
        "at":      m.at.to_rfc3339(),
        "mine":    viewer_admin || m.by == viewer_discord,
    })
}

/// GET /api/intel/markup — the caller's coalition markup for the active round.
pub(crate) async fn api_intel_markup_list(
    session_id: Option<Uuid>,
    query: std::collections::HashMap<std::string::String, std::string::String>,
    db: StatsDb,
    bot_cfg: Arc<Option<BotLinkConfig>>,
    inst: Inst,
) -> std::result::Result<impl warp::Reply, Error> {
    let c = require_coalition(&query, session_id, &db, &bot_cfg, &inst).await?;
    let round = active_round_or_err(&db, &inst)?;
    let filter = if c.god_mode {
        match query.get("side").map(|s| s.as_str()) {
            Some("all") => None,
            Some(s) => parse_side(s).or(Some(c.side)),
            None => Some(c.side),
        }
    } else {
        Some(c.side)
    };
    let items = task::block_in_place(|| db.intel_markup_list(round, filter))?;
    let json: Vec<_> = items
        .iter()
        .map(|(id, m)| markup_json(id, m, &c.session.discord_id, c.session.is_admin))
        .collect();
    Ok(warp::reply::json(&json))
}

#[derive(Deserialize)]
pub(crate) struct IntelMarkupBody {
    pub(crate) kind:   std::string::String,
    pub(crate) points: Vec<[f64; 2]>,
    pub(crate) color:  std::string::String,
    pub(crate) width:  f64,
    pub(crate) text:   Option<std::string::String>,
}

/// POST /api/intel/markup — add a markup shape (coalition-shared).
pub(crate) async fn api_intel_markup_add(
    session_id: Option<Uuid>,
    query: std::collections::HashMap<std::string::String, std::string::String>,
    body: IntelMarkupBody,
    db: StatsDb,
    bot_cfg: Arc<Option<BotLinkConfig>>,
    inst: Inst,
) -> std::result::Result<impl warp::Reply, Error> {
    let c = require_coalition(&query, session_id, &db, &bot_cfg, &inst).await?;
    let round = active_round_or_err(&db, &inst)?;
    if body.points.is_empty() || body.points.len() > 4000 {
        return Err(websec::bad_request("markup needs 1..4000 points").into());
    }
    // Stored and then drawn into every coalition member's map; only a plain
    // hex colour and a known shape name get through.
    if !websec::valid_hex_color(&body.color) {
        return Err(websec::bad_request("markup colour must be a hex colour like #ff0000").into());
    }
    if !matches!(body.kind.as_str(), "pencil" | "line" | "rect" | "circle" | "x" | "text") {
        return Err(websec::bad_request(format!("unknown markup kind {:?}", body.kind)).into());
    }
    if body.text.as_ref().map_or(false, |t| t.len() > 500) {
        return Err(websec::bad_request("markup text is limited to 500 characters").into());
    }
    let id = Uuid::new_v4();
    let name = c
        .ucid()
        .and_then(|u| db.pilot_name(u))
        .unwrap_or_else(|| c.session.username.clone());
    let m = IntelMarkup {
        round,
        side: c.side,
        kind: body.kind,
        points: body.points,
        color: body.color,
        width: body.width.clamp(1.0, 20.0),
        text: body.text.filter(|t| !t.trim().is_empty()),
        by: c.session.discord_id.clone(),
        by_name: name,
        at: chrono::Utc::now(),
    };
    task::block_in_place(|| db.intel_markup_put(id, m.clone()))?;
    Ok(warp::reply::json(&markup_json(&id, &m, &c.session.discord_id, c.session.is_admin)))
}

/// POST /api/intel/markup/delete — remove a markup shape (author or admin).
pub(crate) async fn api_intel_markup_delete(
    session_id: Option<Uuid>,
    query: std::collections::HashMap<std::string::String, std::string::String>,
    body: IntelDeleteBody,
    db: StatsDb,
    bot_cfg: Arc<Option<BotLinkConfig>>,
    inst: Inst,
) -> std::result::Result<impl warp::Reply, Error> {
    let c = require_coalition(&query, session_id, &db, &bot_cfg, &inst).await?;
    let id: Uuid = body.id.parse().map_err(|e| websec::bad_request(format!("bad id: {e}")))?;
    let m = task::block_in_place(|| db.intel_markup_get(&id))?
        .ok_or_else(|| websec::not_found("markup not found"))?;
    if !c.god_mode && m.side != c.side {
        return Err(websec::forbidden("not authorized for this coalition's intel").into());
    }
    if !c.god_mode && !c.session.is_admin && m.by != c.session.discord_id {
        return Err(websec::forbidden("only the author or an admin can delete this markup").into());
    }
    task::block_in_place(|| db.intel_markup_delete(m.round, &id))?;
    Ok(warp::reply::json(&serde_json::json!({"ok": true})))
}
