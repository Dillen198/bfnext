//! Background tasks started by `main`: the war-diary generator and the
//! unit database refresher.

use crate::*;

/// Background task: pull the unit range database out of the engine and keep a
/// snapshot per DCS version.
///
/// This is the piece that replaces "datamine the game after every ED update":
/// the server harvests its own install on every boot, so instead of polling a
/// release feed and hoping a third-party dump catches up, we get told the
/// version that is actually running -- mods included -- and diff it.
///
/// Storage key used when the engine could not report the running DCS build.
pub(crate) const UNKNOWN_DCS_VERSION: &str = "unknown";

/// Keeps the war news current.
///
/// The digest for *today* is rebuilt on a timer while the day is running, so
/// the dashboard shows the war as it happens. Once the date rolls over the day
/// is written one last time with `final_` set and never touched again -- which
/// matters, because `news.rs` reads yesterday's digests both for its trend
/// comparisons and for the cooldown that stops a story leading twice.
///
/// Backfills any missing day since the round opened on first run, so turning
/// this on mid-campaign still produces a history rather than starting blank.
///
/// The prose is written by `news_llm` when an endpoint is configured, and only
/// when the analysis has actually moved: the digest carries a hash of its own
/// angles, so a day that is rebuilt every ten minutes is re-*written* only when
/// something happened. Rebuilding it otherwise would burn a model call every
/// ten minutes and, worse, reword the page under whoever is reading it.
///
/// After the text, the same tick illustrates at most one filed day when
/// pictures are configured (`news_image::tick`).
pub(crate) async fn news_generator(
    db: StatsDb,
    inst: Inst,
    writer: crate::worker::DiarySource,
    images: Option<Arc<news_image::ImageCfg>>,
) {
    use chrono::{Duration as ChronoDuration, Utc};
    // Calls are rationed process-wide by `news_llm::acquire` (one per tick
    // across every instance, pushed back further on a 429). On top of that a
    // day whose call failed backs off on its own, doubling per failure, so one
    // day the model cannot write does not take the slot every tick while the
    // rest of the backlog waits behind it.
    let mut day_backoff: std::collections::HashMap<
        std::string::String,
        (u32, std::time::Instant),
    > = std::collections::HashMap::new();
    // Let the stats reader catch up before judging what was newsworthy.
    tokio::time::sleep(std::time::Duration::from_secs(60)).await;
    let mut tick = tokio::time::interval(std::time::Duration::from_secs(10 * 60));
    tick.set_missed_tick_behavior(tokio::time::MissedTickBehavior::Delay);
    let mut first_pass = true;
    let mut waiting_noted = false;
    loop {
        tick.tick().await;
        if !writer.ready() {
            if !waiting_noted {
                waiting_noted = true;
                log::info!(
                    "[{}] news: waiting for the worker to connect before the first pass",
                    inst.id
                );
            }
            continue;
        }
        let inst_id = inst.id.clone();
        let db = db.clone();
        let inst = inst.clone();
        // External mode: whatever writer the worker has, this tick.
        let writer = writer.current();
        let images = images.clone();
        let day_backoff = &mut day_backoff;
        let res = task::block_in_place(move || -> Result<usize> {
            let rounds = db.latest_rounds_for(&inst.id)?;
            let Some((_, rid, round)) = rounds
                .iter()
                .find(|(_, _, r)| r.end.is_none())
                .or_else(|| rounds.first())
                .cloned()
            else {
                // Worth a line: an empty diary with no explanation looks
                // identical to a broken one.
                log::info!(
                    "[{}] news: no round recorded for this instance yet -- nothing to report on",
                    inst.id
                );
                return Ok(0);
            };
            // Who the two sides are, for this campaign. Per instance, because
            // the belligerents belong to the scenario rather than to the bfdb
            // process -- one server can be running a modern coalition war and
            // another the 2008 Caucasus. Unset leaves the diary saying Blue
            // and Red.
            let factions = news::Factions::new(
                inst.cfg.blue_faction.as_deref(),
                inst.cfg.red_faction.as_deref(),
                inst.cfg.blue_adjective.as_deref(),
                inst.cfg.red_adjective.as_deref(),
            );
            // The voices the day is written in: the diary's own, or for a
            // themed campaign its period voice and a second one telling the
            // same day from the other side (`NewsDigest::alt`).
            let (voice, alt_voice) = news_llm::voices_for(inst.cfg.theme());
            let today = Utc::now().date_naive();
            let first = round.start.date_naive().min(today);
            // Only go back as far as the history the module will ever read.
            let earliest = today - ChronoDuration::days(news::HISTORY_KEEP_DAYS);
            let mut day = first.max(earliest);
            let mut written = 0usize;
            if first_pass {
                log::info!(
                    "[{}] news: round {} opened {}, filing {} day(s) up to {}",
                    inst.id,
                    rid.0,
                    round.start.date_naive(),
                    (today - day).num_days() + 1,
                    today
                );
            }
            while day <= today {
                let key = day.format("%Y-%m-%d").to_string();
                let existing = db.news_get(rid, &key)?;
                // A finished day is immutable -- unless a writer has since been
                // configured and it never got a dispatch, in which case it is
                // still carrying template prose and deserves the real thing.
                let needs = match &existing {
                    Some(d) => !d.final_ || (writer.is_some() && d.body.is_empty()),
                    None => true,
                };
                if needs {
                    // History is "every digest before this day", newest first.
                    let hist: Vec<_> = db
                        .news_history(rid, 400)?
                        .into_iter()
                        .filter(|d| d.day < key)
                        .collect();
                    let mut digest = news::build(&db, rid, day, &hist, &factions)?;
                    // Carry the existing prose over unless there is a real
                    // reason to rewrite it. Two gates, and both matter:
                    //
                    //  * the analysis has to have moved (`facts_hash`), and
                    //  * not more than once an hour, because some of the inputs
                    //    (garrison strength, logistics health) drift all day and
                    //    would otherwise trigger a rewrite every tick.
                    //
                    // The exception is the last rebuild of a day, as it is
                    // frozen: that one always gets the full day's copy.
                    let becoming_final = digest.final_
                        && existing.as_ref().map(|p| !p.final_).unwrap_or(false);
                    let settled = existing.as_ref().filter(|p| {
                        !p.body.is_empty()
                            && !becoming_final
                            && (p.facts_hash == digest.facts_hash
                                || Utc::now().signed_duration_since(p.generated)
                                    < ChronoDuration::hours(1))
                    });
                    if let Some(prev) = settled {
                        digest.headline = prev.headline.clone();
                        digest.body = prev.body.clone();
                        digest.written_by = prev.written_by.clone();
                        digest.generated = prev.generated;
                        digest.alt = prev.alt.clone();
                    } else if let Some(w) = &writer {
                        let now = std::time::Instant::now();
                        let resting =
                            day_backoff.get(&key).map(|(_, t)| now < *t).unwrap_or(false);
                        if resting || !news_llm::acquire(&inst.id) {
                            // No call for this day this tick. A day that has
                            // prose keeps it untouched (and, if it is due to be
                            // frozen, stays open so the final copy still gets
                            // written); a day with only template text is stored
                            // with fresh facts, as it has nothing to lose.
                            if existing
                                .as_ref()
                                .map(|p| !p.body.is_empty())
                                .unwrap_or(false)
                                || becoming_final
                            {
                                day += ChronoDuration::days(1);
                                continue;
                            }
                        } else {
                            match w.write(voice, &digest, &hist) {
                                Ok(out) => {
                                    day_backoff.remove(&key);
                                    digest.headline = out.headline;
                                    digest.body = out.body;
                                    digest.written_by = w.model().to_string();
                                    // The second voice only ever with the
                                    // first, so both tell the same facts.
                                    if let Some(v) = alt_voice {
                                        digest.alt = write_alt(
                                            w, v, &digest, &hist, &inst.id, &key,
                                        );
                                    }
                                }
                                Err(e) => {
                                    if e.downcast_ref::<news_llm::RateLimited>().is_some()
                                    {
                                        // The quota, not the day: the gate is
                                        // already pushed back for every instance.
                                        log::info!(
                                            "[{}] news: writer for {key}: {e}",
                                            inst.id
                                        );
                                    } else {
                                        let fails = day_backoff
                                            .get(&key)
                                            .map(|(n, _)| *n)
                                            .unwrap_or(0)
                                            + 1;
                                        let wait = std::time::Duration::from_secs(
                                            ((20 * 60u64) << (fails - 1).min(5))
                                                .min(12 * 3600),
                                        );
                                        day_backoff
                                            .insert(key.clone(), (fails, now + wait));
                                        log::warn!(
                                        "[{}] news: writer failed for {key} (attempt {fails}, next try in {}m): {e}",
                                        inst.id,
                                        wait.as_secs() / 60
                                    );
                                    }
                                    // Better yesterday's dispatch than a downgrade
                                    // to templates on a day that already had one.
                                    if let Some(prev) =
                                        existing.as_ref().filter(|p| !p.body.is_empty())
                                    {
                                        digest.headline = prev.headline.clone();
                                        digest.body = prev.body.clone();
                                        digest.written_by = prev.written_by.clone();
                                        digest.generated = prev.generated;
                                        digest.alt = prev.alt.clone();
                                    }
                                }
                            }
                        }
                    }
                    db.news_put(rid, &digest)?;
                    written += 1;
                }
                day += ChronoDuration::days(1);
            }
            // The picture for a filed day. Its own failures are handled (and
            // logged) inside; only a DB error lands here, and it must not
            // cost the text pass its result.
            if let Some(ic) = &images {
                if let Err(e) =
                    news_image::tick(&db, &inst.cfg, rid, ic, writer.is_some())
                {
                    log::warn!("[{}] news image: {e}", inst.id);
                }
            }
            Ok(written)
        });
        match res {
            Ok(0) => {}
            // Routine: today's digest is rebuilt every tick.
            Ok(n) => log::debug!("[{inst_id}] news: wrote {n} digest(s)"),
            Err(e) => log::warn!("[{inst_id}] news: generation failed: {e}"),
        }
        first_pass = false;
    }
}

/// The second voice's version of a day the first has just been written for,
/// in the same writer slot (charged to the gate as one more call). A failure
/// costs only the second voice: the day is filed with the main dispatch and
/// no `alt`, and the next rewrite of the main one tries again.
fn write_alt(
    w: &crate::worker::DiaryWriter,
    voice: news_llm::Voice,
    digest: &news::NewsDigest,
    hist: &[news::NewsDigest],
    inst: &str,
    key: &str,
) -> Option<news::AltDispatch> {
    news_llm::charge_extra_call();
    match w.write(voice, digest, hist) {
        Ok(out) => Some(news::AltDispatch {
            voice: voice.id().to_string(),
            headline: out.headline,
            body: out.body,
            written_by: w.model().to_string(),
        }),
        Err(e) => {
            if e.downcast_ref::<news_llm::RateLimited>().is_some() {
                log::info!(
                    "[{inst}] news: {} voice for {key}: {e}; filed with the main dispatch only",
                    voice.id()
                );
            } else {
                log::warn!(
                    "[{inst}] news: {} voice failed for {key}, filed with the main dispatch only: {e}",
                    voice.id()
                );
            }
            None
        }
    }
}

/// Slow on purpose. The data only changes when DCS or a mod updates, which
/// means a restart, which means we re-read it anyway.
pub(crate) async fn unitdb_refresher(db: StatsDb, inst: Inst) {
    use netidx::publisher::Value;
    // Let the engine finish loading the mission before the first ask.
    tokio::time::sleep(std::time::Duration::from_secs(90)).await;
    let mut tick = tokio::time::interval(std::time::Duration::from_secs(30 * 60));
    tick.set_missed_tick_behavior(tokio::time::MissedTickBehavior::Delay);
    let mut last_err: Option<std::string::String> = None;
    let mut warned_no_version = false;
    loop {
        tick.tick().await;
        let res = tokio::time::timeout(
            std::time::Duration::from_secs(20),
            call_engine_rpc_str(&db, &inst, "query-unitdb", vec![("arg", Value::Null)]),
        )
        .await;
        let json = match res {
            Ok(Ok(json)) => json,
            Ok(Err(e)) => {
                let msg = format!("RPC error: {} -- is bflib.dll current?", e.0);
                if last_err.as_deref() != Some(msg.as_str()) {
                    log::warn!("[{}] unitdb_refresher: {msg}", inst.id);
                    last_err = Some(msg);
                }
                continue;
            }
            Err(_) => {
                let msg = "RPC timed out after 20s".to_string();
                if last_err.as_deref() != Some(msg.as_str()) {
                    log::warn!("[{}] unitdb_refresher: {msg}", inst.id);
                    last_err = Some(msg);
                }
                continue;
            }
        };
        last_err = None;
        let snap: db::UnitDbSnapshot = match serde_json::from_str(&json) {
            Ok(s) => s,
            Err(e) => {
                log::warn!("[{}] unitdb_refresher: unparseable snapshot: {e}", inst.id);
                continue;
            }
        };
        // The version is only the storage key and a label. bflib reads it from
        // the hooks Lua state, where `DCS.getVersion` is not always present
        // ("could not read the DCS version: error converting Lua nil to
        // function" in the engine log) -- and discarding the snapshot over that
        // threw away a complete, usable harvest (883 unit types) on every run,
        // leaving the dashboard and wiki with no unit data at all. Store it
        // under a placeholder instead and say so once.
        let version = match snap.dcs_version.clone() {
            Some(v) => v,
            None => {
                if !warned_no_version {
                    warned_no_version = true;
                    log::warn!(
                        "[{}] unitdb_refresher: snapshot has no DCS version -- storing it as \"{}\" (the engine could not read DCS.getVersion; the unit data itself is fine)",
                        inst.id,
                        UNKNOWN_DCS_VERSION
                    );
                }
                UNKNOWN_DCS_VERSION.to_string()
            }
        };
        let inst_id = inst.id.to_string();
        let stored = task::block_in_place(|| -> Result<_> {
            let prev = db.store_unit_db(&inst_id, &version, &json)?;
            let prev_snap = match &prev {
                Some(p) => db
                    .unit_db_json(&inst_id, p)?
                    .and_then(|j| serde_json::from_str::<db::UnitDbSnapshot>(&j).ok()),
                None => None,
            };
            Ok((prev, prev_snap))
        });
        let (prev, prev_snap) = match stored {
            Ok(v) => v,
            Err(e) => {
                log::warn!(
                    "[{}] unitdb_refresher: could not store snapshot: {e:?}",
                    inst.id
                );
                continue;
            }
        };
        let Some(prev_version) = prev else { continue };
        log::info!(
            "[{}] DCS version changed: {prev_version} -> {version} ({} unit types harvested)",
            inst.id,
            snap.by_type.len()
        );
        let Some(prev_snap) = prev_snap else {
            log::info!(
                "[{}] no readable snapshot for {prev_version}, nothing to diff against",
                inst.id
            );
            continue;
        };
        let changes = db::diff_unit_db(&prev_snap, &snap);
        if changes.is_empty() {
            log::info!("[{}] no unit ranges changed in {version}", inst.id);
        } else {
            log::info!(
                "[{}] {} unit range change(s) in {version}:",
                inst.id,
                changes.len()
            );
            for c in changes.iter().take(40) {
                match c {
                    db::UnitDbChange::RangeChanged { typ, field, from, to, .. } => {
                        log::info!(
                            "[{}]   {typ} {field}: {:?} -> {:?}",
                            inst.id,
                            from,
                            to
                        )
                    }
                    db::UnitDbChange::Added { typ, threat_range_m, .. } => {
                        log::info!(
                            "[{}]   + {typ} (threat range {:?})",
                            inst.id,
                            threat_range_m
                        )
                    }
                    db::UnitDbChange::Removed { typ, .. } => {
                        log::info!("[{}]   - {typ}", inst.id)
                    }
                }
            }
            if changes.len() > 40 {
                log::info!("[{}]   ... and {} more", inst.id, changes.len() - 40);
            }
        }
        // The expensive part of a DCS update is not the changed ranges, it is
        // the ones we override in config and would therefore never notice.
        match task::block_in_place(|| db.unit_db_stale_overrides(&inst_id)) {
            Ok(stale) if !stale.is_empty() => {
                log::warn!(
                    "[{}] {} artillery config override(s) no longer match DCS {version}:",
                    inst.id,
                    stale.len()
                );
                for o in &stale {
                    log::warn!(
                        "[{}]   {} cfg {}/{} vs dcs {:?}/{:?}",
                        inst.id,
                        o.typ,
                        o.cfg_max_range_m,
                        o.cfg_min_range_m,
                        o.dcs_max_range_m,
                        o.dcs_min_range_m
                    );
                }
            }
            Ok(_) => (),
            Err(e) => log::warn!("[{}] could not check config overrides: {e:?}", inst.id),
        }
    }
}
