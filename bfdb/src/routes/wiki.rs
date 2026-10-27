//! Wiki pages, images and the war diary (news) routes.

use crate::*;

/// GET /api/wiki/pages  — list all pages (slug/title/section/order only, no
/// content) for building the sidebar. Public -- reading the wiki needs no
/// login, only editing does.
pub(crate) async fn api_wiki_list(db: StatsDb, q: Q) -> std::result::Result<impl warp::Reply, Error> {
    let pages = task::block_in_place(|| db.wiki_list_pages())?;
    let entries: Vec<_> = pages.iter().map(|(slug, p)| {
        serde_json::json!({
            "slug": slug,
            "title": p.title,
            "section": p.section,
            "order": p.order,
        })
    }).collect();
    Ok(paged_response(page_json(entries, &q, WIKI_LIST_CAP)?))
}

/// A wiki slug: `/`-separated segments of letters, digits, `-`, `_` and `.`,
/// no empty or dot-only segments. Slugs become URL paths in bfwiki and keys
/// here; anything else is either a mistake or someone probing.
pub(crate) fn valid_wiki_slug(slug: &str) -> bool {
    !slug.is_empty()
        && slug.len() <= 200
        && slug.split('/').all(|seg| {
            !seg.is_empty()
                && seg.chars().any(|c| c != '.')
                && seg.chars().all(|c| c.is_ascii_alphanumeric() || matches!(c, '-' | '_' | '.'))
        })
}

/// GET /api/wiki/pages/<slug>  — full content of one page. Public. `<slug>`
/// is itself multi-segment (e.g. "gameplay/objectives"), so this matches on
/// the path tail rather than a single `String` segment.
pub(crate) async fn api_wiki_get(
    slug: warp::path::Tail,
    db: StatsDb,
) -> std::result::Result<impl warp::Reply, Error> {
    let slug = slug.as_str();
    if !valid_wiki_slug(slug) {
        return Err(websec::not_found("page not found").into());
    }
    let page = task::block_in_place(|| db.wiki_get_page(slug))?
        .ok_or_else(|| websec::not_found("page not found"))?;
    Ok(warp::reply::json(&serde_json::json!({
        "slug": slug,
        "title": page.title,
        "section": page.section,
        "order": page.order,
        "content": page.content,
        "updated_at": page.updated_at,
        "updated_by": page.updated_by,
    })))
}

/// Top-level keys of a DCS server instance's engine config that the wiki is
/// allowed to publish. This is an allow-list on purpose: the config also holds
/// the admin UCID table, the ban list, the netidx base and the CheckWX API key,
/// none of which belong in a page anybody can read without logging in.
///
/// Deliberately excluded beyond the secrets: the very large per-unit maps
/// (`life_types`, `unit_classification`, `threatened_distance`,
/// `airborne_ewrs`, ...). They would multiply the payload by an order of
/// magnitude for values no wiki page quotes.
pub(crate) const WIKI_FACT_KEYS: &[&str] = &[
    "points",
    "limited_lives",
    "default_lives",
    "lock_sides",
    "one_player_per_ip",
    "side_switches",
    "repair_time",
    "repair_supply_cost",
    "deploy_supply_cost",
    "repair_crate",
    "emergency_repair",
    "population_scaling",
    "logistics_exclusion",
    "supply_alert_threshold",
    "objective_start_points",
    "capture_consolidation_secs",
    "consolidation_zone_grace_secs",
    "consolidation_squad_bonus",
    "consolidation_crate_progress_secs",
    "takeoff_delay_secs",
    "slot_leave_kill_radius_m",
    "threatened_cooldown",
    "crate_load_distance",
    "crate_spread",
    "max_crates",
    "cargo",
    "c130_cargo",
    "helo_cargo",
    "warehouse",
    "helo_insertion",
    "logi_from_scenery",
    "csar",
    "player_recon",
    "artillery",
    "artillery_mission_range",
    "artillery_min_range",
    "alcm_mission_range",
    "campaign_events",
    "smart_commander",
    "ground_war",
    "command",
    "garrison",
    "factory",
    "carrier",
    "frontline",
    "navaids",
    "ewr_mode",
    "ewr_delay",
    "gci_briefing",
    "situation_briefing",
    "comms_plan",
    "auto_reset",
    "elint",
    "iadn",
    "radar_physics",
    "unit_cull_distance",
    "ground_vehicle_cull_distance",
    "cull_after",
    "deployables",
    "troops",
    "air_life",
    "modern_war",
];

/// Reduce the `actions` block to what a wiki page ever quotes -- the menu name,
/// what it costs, and which kind of action it is. The full entry carries the
/// whole AI template (route, payload, callsigns), which is both large and of no
/// use to a reader.
pub(crate) fn wiki_summarize_actions(actions: &serde_json::Value) -> serde_json::Value {
    let mut out = serde_json::Map::new();
    let Some(sides) = actions.as_object() else { return serde_json::Value::Object(out) };
    for (side, by_name) in sides {
        let Some(by_name) = by_name.as_object() else { continue };
        let mut rows = Vec::with_capacity(by_name.len());
        for (name, action) in by_name {
            // `kind` is an externally tagged enum: either a bare string for a
            // unit variant, or a single-key object for everything else.
            let kind = action.get("kind").and_then(|k| match k {
                serde_json::Value::String(s) => Some(s.clone()),
                serde_json::Value::Object(o) => o.keys().next().cloned(),
                _ => None,
            });
            rows.push(serde_json::json!({
                "name": name,
                "cost": action.get("cost"),
                "penalty": action.get("penalty"),
                "limit": action.get("limit"),
                "kind": kind,
            }));
        }
        out.insert(side.clone(), serde_json::Value::Array(rows));
    }
    serde_json::Value::Object(out)
}

/// `GET /api/news` -- the campaign's daily war reports, newest day first.
///
/// Deliberately public and deliberately NOT fog-of-war scoped: both coalitions
/// read the same wire report, because in a real war the enemy reads the paper
/// too. `news.rs` has the reasoning for how a day is judged newsworthy.
///
/// Each day also carries `image` (a `/api/news/image/...` path, or null) and
/// `image_pending` (pictures are on and this day is still expected to get
/// one, or a filed day its final one) -- the latter is what the Discord feed
/// waits on -- and `image_provisional` (the picture is the running day's
/// early edition, redrawn once the day is filed).
pub(crate) async fn api_news(
    db: StatsDb,
    limit: Option<usize>,
    inst: Inst,
    images: Option<Arc<news_image::ImageCfg>>,
) -> std::result::Result<impl warp::Reply, Error> {
    let data = task::block_in_place(|| -> Result<serde_json::Value> {
        let enabled = images.is_some();
        let Some(rid) = news_round(&db, &inst)? else {
            return Ok(serde_json::json!({ "days": [], "images": enabled }));
        };
        let mut days = Vec::new();
        for d in db.news_history(rid, limit.unwrap_or(30))? {
            let img = news_image::api_fields(&db, &inst.id, rid, &d, enabled)?;
            let mut v = serde_json::to_value(&d)?;
            v["image"] = serde_json::json!(img.url);
            v["image_pending"] = serde_json::json!(img.pending);
            v["image_provisional"] = serde_json::json!(img.provisional);
            days.push(v);
        }
        Ok(serde_json::json!({ "days": days, "images": enabled }))
    })
    .map_err(Error)?;
    Ok(warp::reply::json(&data))
}

/// The round the war diary is showing for an instance: the open one, else
/// the most recent.
pub(crate) fn news_round(db: &StatsDb, inst: &Inst) -> Result<Option<db::RoundId>> {
    let rounds = db.latest_rounds_for(&inst.id)?;
    Ok(rounds
        .iter()
        .find(|(_, _, r)| r.end.is_none())
        .or_else(|| rounds.first())
        .map(|(_, rid, _)| *rid))
}

/// `GET /api/news/image/<YYYY-MM-DD>` -- the picture filed with that day's
/// dispatch, for the instance's current round. Public, like the diary. 404
/// when there is none (yet). Cached for a day; the `/api/news` URL carries a
/// version, so a regenerated picture is a new URL.
pub(crate) async fn api_news_image(
    day: std::string::String,
    db: StatsDb,
    inst: Inst,
) -> std::result::Result<impl warp::Reply, Error> {
    if !news_image::valid_day(&day) {
        return Err(websec::not_found("no such dispatch").into());
    }
    let bytes = task::block_in_place(|| -> Result<Option<Vec<u8>>> {
        match news_round(&db, &inst)? {
            Some(rid) => db.news_image_get(&inst.id, rid, &day),
            None => Ok(None),
        }
    })?
    .ok_or_else(|| websec::not_found("no picture for that dispatch"))?;
    Ok(websec::serve_upload(bytes, "public, max-age=86400"))
}

/// `POST /api/admin/news/regenerate-image?day=YYYY-MM-DD` (admin only) --
/// draw a new picture for a filed dispatch, replacing the stored one if the
/// call succeeds. Counts against the daily caps like any other call, and runs
/// in the background (a generation takes up to a minute or two): the answer is
/// 202 once it is queued, and the day's `image` URL changes when it lands.
pub(crate) async fn api_admin_news_regenerate_image(
    query: std::collections::HashMap<std::string::String, std::string::String>,
    session_id: Option<Uuid>,
    db: StatsDb,
    inst: Inst,
    images: Option<Arc<news_image::ImageCfg>>,
) -> std::result::Result<warp::reply::Response, Error> {
    use warp::Reply;
    let who = require_role(session_id, db.clone(), admin::Role::CONTENT).await?;
    let Some(cfg) = images else {
        return Err(websec::unavailable(
            "dispatch pictures are not configured (--news-image-url / --news-image-key)",
        )
        .into());
    };
    let day = query.get("day").cloned().unwrap_or_default();
    if !news_image::valid_day(&day) {
        return Err(websec::bad_request("day must be YYYY-MM-DD").into());
    }
    let (rid, digest) = task::block_in_place(|| -> Result<_> {
        let rid = news_round(&db, &inst)?.ok_or_else(|| websec::not_found("no round for this instance"))?;
        let d = db.news_get(rid, &day)?.ok_or_else(|| websec::not_found("no dispatch for that day"))?;
        if let Some(why) = news_image::capped(&db, &cfg, &inst.id)? {
            return Err(websec::too_many(why));
        }
        Ok((rid, d))
    })?;
    if !digest.final_ {
        return Err(websec::bad_request(
            "that day is still running -- pictures are drawn once it is filed",
        )
        .into());
    }
    audit(&db, &who, "regenerate news image", Some(&day), Some(&inst.id));
    let inst_cfg = inst.cfg.clone();
    let inst_id = inst.id.clone();
    let day2 = day.clone();
    tokio::task::spawn_blocking(move || {
        match news_image::illustrate(&db, &inst_cfg, rid, &digest, &cfg, news_image::Draw::Forced) {
            Ok(news_image::Outcome::Stored { version }) => {
                log::info!("ADMIN: [{inst_id}] news image for {day2} regenerated (v{version})")
            }
            Ok(other) => {
                log::warn!("ADMIN: [{inst_id}] news image for {day2} not regenerated: {other:?}")
            }
            Err(e) => log::warn!("ADMIN: [{inst_id}] news image for {day2}: {e}"),
        }
    });
    Ok(warp::reply::with_status(
        warp::reply::json(&serde_json::json!({
            "ok": true,
            "queued": true,
            "message": format!(
                "regenerating the picture for {day} -- it replaces the old one in a minute or two if the call succeeds"
            ),
        })),
        warp::http::StatusCode::ACCEPTED,
    )
    .into_response())
}

/// `GET /api/wiki/facts` -- the selected instance's campaign numbers, for the
/// `{{cfg:...}}` placeholders in wiki pages.
///
/// One bfdb can front several DCS servers and **each one runs its own engine
/// config**, so the same sentence ("an air kill is worth N points") has a
/// different answer per instance. Rather than forking the page per server, a
/// page writes `{{cfg:points.air_kill}}` and bfwiki resolves it against
/// whichever instance the reader has selected.
///
/// Public, like the rest of the read side of the wiki -- see `WIKI_FACT_KEYS`
/// for what that means it may and may not contain. An instance with no
/// `engine_config` configured returns an empty fact set rather than an error,
/// so placeholders fall back to the defaults written into the page.
pub(crate) async fn api_wiki_facts(_db: StatsDb, inst: Inst) -> std::result::Result<impl warp::Reply, Error> {
    let mut facts = serde_json::Map::new();
    let mut source: Option<chrono::DateTime<chrono::Utc>> = None;
    if let Some(path) = inst.cfg.engine_config.clone() {
        let read = task::block_in_place(|| -> Option<(std::string::String, Option<chrono::DateTime<chrono::Utc>>)> {
            let text = std::fs::read_to_string(&path).ok()?;
            let mtime = std::fs::metadata(&path)
                .and_then(|m| m.modified())
                .ok()
                .map(chrono::DateTime::<chrono::Utc>::from);
            Some((text, mtime))
        });
        match read {
            None => log::warn!(
                "wiki facts: cannot read engine config {:?} for instance {}",
                path, inst.cfg.id
            ),
            Some((text, mtime)) => match serde_json::from_str::<serde_json::Value>(&text) {
                Err(e) => log::warn!(
                    "wiki facts: engine config {:?} for instance {} is not valid JSON: {e}",
                    path, inst.cfg.id
                ),
                Ok(serde_json::Value::Object(map)) => {
                    source = mtime;
                    for key in WIKI_FACT_KEYS {
                        if let Some(v) = map.get(*key) {
                            facts.insert((*key).to_string(), v.clone());
                        }
                    }
                    if let Some(actions) = map.get("actions") {
                        facts.insert("actions".into(), wiki_summarize_actions(actions));
                    }
                    // `live_weather` is on the allow-list for lat/lon/station,
                    // but it also carries the CheckWX key.
                    if let Some(serde_json::Value::Object(o)) = map.get("live_weather").cloned() {
                        let mut o = o;
                        o.remove("checkwx_api_key");
                        facts.insert("live_weather".into(), serde_json::Value::Object(o));
                    }
                }
                Ok(_) => log::warn!(
                    "wiki facts: engine config {:?} for instance {} is not a JSON object",
                    path, inst.cfg.id
                ),
            },
        }
    }
    Ok(warp::reply::json(&serde_json::json!({
        "instance": {
            "id": inst.cfg.id,
            "label": inst.cfg.label(),
        },
        // When the config was last written -- lets the wiki say how fresh the
        // numbers on the page are.
        "updated_at": source,
        "facts": facts,
    })))
}

#[derive(Deserialize)]
pub(crate) struct SaveWikiPageBody {
    pub(crate) title:   std::string::String,
    pub(crate) section: std::string::String,
    pub(crate) order:   i32,
    pub(crate) content: std::string::String,
}

/// POST /api/wiki/pages/<slug>  — create or overwrite a page (admin only).
pub(crate) async fn api_wiki_save(
    slug: warp::path::Tail,
    session_id: Option<Uuid>,
    body: SaveWikiPageBody,
    db: StatsDb,
) -> std::result::Result<impl warp::Reply, Error> {
    let session = require_role(session_id, db.clone(), admin::Role::CONTENT).await?;
    let slug = slug.as_str().to_string();
    if !valid_wiki_slug(&slug) {
        return Err(websec::bad_request(
            "page slugs are letters, digits, '-', '_', '.' and '/' separated segments",
        )
        .into());
    }
    let page = WikiPage {
        title:      body.title,
        section:    body.section,
        order:      body.order,
        content:    body.content,
        updated_at: chrono::Utc::now(),
        updated_by: session.discord_id.clone(),
    };
    task::block_in_place(|| db.wiki_save_page(&slug, page))?;
    audit(&db, &session, "save wiki page", Some(&slug), None);
    Ok(warp::reply::json(&serde_json::json!({"ok": true})))
}

#[derive(Deserialize)]
pub(crate) struct DeleteWikiPageBody {
    pub(crate) slug: std::string::String,
}

/// POST /api/wiki/delete  — remove a page (admin only). Takes the slug in
/// the body rather than the path so a multi-segment slug can't collide with
/// a literal trailing path segment.
pub(crate) async fn api_wiki_delete(
    session_id: Option<Uuid>,
    body: DeleteWikiPageBody,
    db: StatsDb,
) -> std::result::Result<impl warp::Reply, Error> {
    let who = require_role(session_id, db.clone(), admin::Role::CONTENT).await?;
    task::block_in_place(|| db.wiki_delete_page(&body.slug, &who.discord_id))?;
    audit(&db, &who, "delete wiki page", Some(&body.slug), None);
    Ok(warp::reply::json(&serde_json::json!({"ok": true})))
}

pub(crate) const MAX_WIKI_IMAGE_BYTES: u64 = 8 * 1024 * 1024;

/// POST /api/wiki/images  — upload an image (admin only). Body is the raw
/// image bytes; the `content-type` request header determines how it's
/// served back and is validated to actually be an image type. Capped at
/// `MAX_WIKI_IMAGE_BYTES` via the route's `content_length_limit` filter.
pub(crate) async fn api_wiki_upload_image(
    session_id: Option<Uuid>,
    content_type: std::string::String,
    body: bytes::Bytes,
    db: StatsDb,
) -> std::result::Result<impl warp::Reply, Error> {
    let session = require_role(session_id, db.clone(), admin::Role::CONTENT).await?;
    // Judged by the bytes, not the claimed type: an SVG (script and all) or
    // HTML labelled image/png is refused.
    let Some(sniffed) = websec::sniff_image(&body) else {
        return Err(websec::unsupported_media(format!(
            "only PNG, JPEG or WebP images are accepted (declared '{content_type}')"
        ))
        .into());
    };
    let id = Uuid::new_v4();
    let image = WikiImage {
        content_type: sniffed.to_string(),
        data: body.to_vec(),
        uploaded_at: chrono::Utc::now(),
        uploaded_by: session.discord_id.clone(),
    };
    task::block_in_place(|| db.wiki_save_image(id, image))?;
    audit(&db, &session, "upload wiki image", Some(&id.to_string()), None);
    Ok(warp::reply::json(&serde_json::json!({
        "id": id.to_string(),
        "url": format!("/api/wiki/images/{id}"),
    })))
}

/// GET /api/wiki/images/<id>  — serve an uploaded image. Public -- images
/// embedded in wiki pages need to load for anonymous readers too. Cached
/// aggressively since an id's content never changes after upload.
pub(crate) async fn api_wiki_get_image(
    id: Uuid,
    db: StatsDb,
) -> std::result::Result<impl warp::Reply, Error> {
    let image = task::block_in_place(|| db.wiki_get_image(&id))?
        .ok_or_else(|| websec::not_found("image not found"))?;
    // Re-sniffed and sandboxed on the way out as well: rows uploaded before
    // the upload check existed may hold anything.
    Ok(websec::serve_upload(image.data, "public, max-age=31536000, immutable"))
}

// ── Recon intel (TARPS) API ────────────────────────────────────────────────
//
// A shared reconnaissance picture built from F-14 TARPS photos, contributed
// through the dashboard login and locked to the contributor's coalition.
// Everything is scoped to the active round and wiped by a campaign reset.
