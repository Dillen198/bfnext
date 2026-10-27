//! Health, metrics, performance and shutdown.

use crate::*;

/// Real health, as opposed to the string "ok".
///
/// The old endpoint was `.map(|| "ok")` -- it proved only that the HTTP
/// listener was accepting connections, which is the one thing that is almost
/// never what has broken. The interesting failure is bfdb up and healthy while
/// the DCS server behind it is gone, and that looked identical.
///
/// So this actually probes the engine: a cheap in-memory RPC
/// (`query-campaign-state`, which walks objectives and players and touches no
/// disk) on a short timeout, timed. `ok` is true only when the engine answered.
/// A dashboard can drive a three-state badge off this -- LIVE / ENGINE
/// UNREACHABLE / OFFLINE -- instead of inferring liveness from the mere
/// existence of a round row.
pub(crate) async fn health(
    db: StatsDb,
    inst: Inst,
    detail: bool,
) -> std::result::Result<warp::reply::Json, Error> {
    // Served from a short cache. Without one, every open dashboard tab fires
    // its own engine RPC every 30s, and this endpoint would itself become a
    // source of the poll contention it is meant to measure.
    const CACHE_FOR: std::time::Duration = std::time::Duration::from_secs(15);
    // 8s, not 3s: netidx RPC round-trips under a populated mission routinely
    // run past 3s with a perfectly healthy engine -- see the same note on
    // api_objectives. A 3s probe reported NO ENGINE on a server whose tacmap
    // and objective RPCs were answering fine every few seconds.
    const PROBE_TIMEOUT: std::time::Duration = std::time::Duration::from_secs(8);

    let cached = {
        let cache = inst.health_cache.lock().await;
        match &*cache {
            Some((at, ok, err)) if at.elapsed() < CACHE_FOR => {
                Some((*ok, err.clone(), at.elapsed().as_millis() as u64))
            }
            _ => None,
        }
    };

    let (engine_ok, engine_err, elapsed_ms) = match cached {
        Some((ok, err, _)) => (ok, err, 0u64),
        None => {
            let started = std::time::Instant::now();
            // a range server runs bfrange, which has no campaign state: its
            // equivalent cheap in-memory query is the live range picture
            let probe_rpc =
                if inst.cfg.is_range() { "query-range" } else { "query-campaign-state" };
            let probe = tokio::time::timeout(
                PROBE_TIMEOUT,
                call_engine_rpc_str(&db, &inst, probe_rpc, vec![]),
            )
            .await;
            let elapsed_ms = started.elapsed().as_millis() as u64;
            let (ok, err) = match probe {
                Ok(Ok(_)) => (true, None),
                Ok(Err(e)) => (false, Some(e.0.to_string())),
                Err(_) => (
                    false,
                    Some(format!(
                        "engine RPC timed out after {}s",
                        PROBE_TIMEOUT.as_secs()
                    )),
                ),
            };
            // Log it. The first version of this handler logged nothing, so a
            // persistent NO ENGINE left no trace in bfdb.log at all.
            // Warn on the transition only: the probe re-runs every 15s per
            // open dashboard, and during an outage each one said the same
            // thing (the RPC breaker logs its own reminders).
            let prev_ok = inst.health_cache.lock().await.as_ref().map(|c| c.1);
            if !ok {
                let msg = format!(
                    "[{}] api_health: {probe_rpc} probe failed after {}ms: {}",
                    inst.id,
                    elapsed_ms,
                    err.as_deref().unwrap_or("unknown")
                );
                if prev_ok == Some(false) {
                    log::debug!("{msg}");
                } else {
                    log::warn!("{msg}");
                }
            } else if prev_ok == Some(false) {
                log::info!("[{}] api_health: {probe_rpc} probe answering again", inst.id);
            }
            *inst.health_cache.lock().await =
                Some((std::time::Instant::now(), ok, err.clone()));
            (ok, err, elapsed_ms)
        }
    };

    // The round row is campaign state, NOT liveness -- it survives the DCS
    // server dying. Reported separately so a caller can tell the two apart.
    let round = task::block_in_place(|| -> Result<Option<serde_json::Value>> {
        let rounds = db.latest_rounds_for(&inst.id)?;
        Ok(rounds.iter().find(|(_, _, r)| r.end.is_none()).map(|(_, rid, r)| {
            serde_json::json!({
                "id": rid.0,
                "number": db.round_number(*rid),
                "start": r.start.to_rfc3339(),
            })
        }))
    })?;

    // Public: only what the site, the dashboard and the bot read. The
    // engine's own error text (netidx paths), database size, free disk and
    // the build are for `/api/admin/health`.
    let mut body = serde_json::json!({
        "ok": engine_ok,
        "bfdb": "ok",
        "instance": inst.id.to_string(),
        "engine": {
            "reachable": engine_ok,
            "latency_ms": if engine_ok && elapsed_ms > 0 { Some(elapsed_ms) } else { None },
            "error": engine_err.as_deref().map(public_engine_error),
        },
        "active_round": round,
    });
    if detail {
        body["engine"]["error"] = serde_json::json!(engine_err);
        // The engine answering says nothing about whether its stats are
        // reaching us: a stalled stats.jsonl reader looked perfectly healthy.
        body["ingest"] = serde_json::json!({
            "last_stat_at": inst.last_stat.lock().ok().and_then(|g| g.map(|(t, _)| t.to_rfc3339())),
            "lag_s": ingest_lag_secs(&inst),
        });
        body["db"] = serde_json::json!({
            "size_bytes": db.sled().size_on_disk().ok(),
            "disk_free_bytes": DB_PATH.get().and_then(|p| maint::disk_free(p)),
        });
        body["build"] = build_info_json();
    }
    Ok(warp::reply::json(&body))
}

/// The engine probe's failure as the public health check tells it: what
/// kind of failure, without the netidx paths and RPC detail in the error.
pub(crate) fn public_engine_error(e: &str) -> &'static str {
    if e.contains("timed out") || e.contains("did not answer") {
        "the engine did not answer in time"
    } else {
        "the engine is not reachable"
    }
}

/// GET /api/admin/health -- `/api/health` with everything: the engine's own
/// error, ingest lag, database size, free disk and the build (ops role).
pub(crate) async fn api_admin_health(
    session_id: Option<Uuid>,
    db: StatsDb,
    inst: Inst,
) -> std::result::Result<warp::reply::Response, Error> {
    require_role(session_id, db.clone(), admin::Role::OPS).await?;
    Ok(health(db, inst, true).await?.into_response())
}

pub(crate) async fn api_health(
    db: StatsDb,
    inst: Inst,
) -> std::result::Result<impl warp::Reply, Error> {
    health(db, inst, false).await
}

/// POST /api/admin/shutdown — flush the database and exit. See maint.rs for
/// the whole shutdown contract. Direct loopback connections only (never
/// through the proxy), and an admin session or the shutdown token besides.
pub(crate) async fn api_admin_shutdown(
    direct_loopback: bool,
    auth_header: Option<std::string::String>,
    session_id: Option<Uuid>,
    db: StatsDb,
    token: Arc<Option<std::string::String>>,
    shutdown: maint::Shutdown,
) -> std::result::Result<impl warp::Reply, Error> {
    if !direct_loopback {
        return Err(
            websec::forbidden("shutdown is only accepted from this machine").into()
        );
    }
    let bearer_ok = match (token.as_deref(), auth_header.as_deref()) {
        (Some(want), Some(h)) => h
            .strip_prefix("Bearer ")
            .or_else(|| h.strip_prefix("bearer "))
            .map_or(false, |got| websec::ct_eq(got.trim(), want)),
        _ => false,
    };
    if !bearer_ok {
        require_admin(session_id, db.clone()).await?;
    }
    // Answer first, then go: the caller learns it was accepted.
    let sd = shutdown.clone();
    tokio::spawn(async move {
        tokio::time::sleep(std::time::Duration::from_millis(500)).await;
        sd.trigger("POST /api/admin/shutdown");
    });
    Ok(warp::reply::with_status(
        warp::reply::json(&serde_json::json!({
            "ok": true,
            "message": "flushing the database and exiting",
        })),
        warp::http::StatusCode::ACCEPTED,
    ))
}

/// GET /api/metrics — Prometheus text exposition, gated by the log-read token
/// (`?token=` or `Authorization: Bearer`). 404 when no token is configured.
pub(crate) async fn api_metrics(
    q: std::collections::HashMap<std::string::String, std::string::String>,
    auth_header: Option<std::string::String>,
    tokens: Arc<(Option<std::string::String>, Option<std::string::String>)>,
    db: StatsDb,
    ip: std::net::IpAddr,
) -> Response {
    use std::fmt::Write as _;
    let text = |status: warp::http::StatusCode, body: std::string::String| {
        warp::http::Response::builder()
            .status(status)
            .header("content-type", "text/plain; version=0.0.4; charset=utf-8")
            .header("cache-control", "no-store")
            .body(body)
            .unwrap()
            .into_response()
    };
    // --metrics-token, or the log-read token as before.
    let accepted: Vec<&str> =
        [tokens.0.as_deref(), tokens.1.as_deref()].into_iter().flatten().collect();
    if accepted.is_empty() {
        return text(
            warp::http::StatusCode::NOT_FOUND,
            "metrics disabled (no --metrics-token or --log-read-token)\n".into(),
        );
    }
    if let Err((status, why)) =
        check_static_token(&q, auth_header.as_deref(), &accepted, ip)
    {
        return text(status, why.into());
    }
    let mut out = std::string::String::new();
    let size = db.sled().size_on_disk().unwrap_or(0);
    let _ = writeln!(out, "# TYPE bfdb_db_size_bytes gauge\nbfdb_db_size_bytes {size}");
    if let Some(free) = DB_PATH.get().and_then(|p| maint::disk_free(p)) {
        let _ = writeln!(
            out,
            "# TYPE bfdb_disk_free_bytes gauge\nbfdb_disk_free_bytes {free}"
        );
    }
    let _ = writeln!(out, "# TYPE bfdb_ingest_lag_seconds gauge");
    let _ = writeln!(out, "# TYPE bfdb_engine_reachable gauge");
    for cfg in db.instances().all() {
        let st = db.state(&Arc::from(cfg.id.as_str()));
        let id = cfg.id.replace(['"', '\\', '\n'], "_");
        if let Some(lag) = ingest_lag_secs(&st) {
            let _ = writeln!(out, "bfdb_ingest_lag_seconds{{instance=\"{id}\"}} {lag}");
        }
        let reachable = st.health_cache.lock().await.as_ref().map(|(_, ok, _)| *ok);
        if let Some(ok) = reachable {
            let _ =
                writeln!(out, "bfdb_engine_reachable{{instance=\"{id}\"}} {}", ok as u8);
        }
    }
    {
        let g = COCKPIT_PLUGINS.lock().unwrap_or_else(|e| e.into_inner());
        if !g.is_empty() {
            let _ = writeln!(out, "# TYPE bfdb_cockpit_plugin_requests_total counter");
            for (v, (n, _)) in g.iter() {
                let _ = writeln!(
                    out,
                    "bfdb_cockpit_plugin_requests_total{{version=\"{v}\"}} {n}"
                );
            }
        }
    }
    websec::write_request_metrics(&mut out);
    let _ = writeln!(out, "# TYPE bfdb_build_info gauge\nbfdb_build_info{{version=\"{BUILD_VERSION}\",git=\"{BUILD_GIT}\"}} 1");
    text(warp::http::StatusCode::OK, out)
}

/// Seconds since the newest ingested stat was *emitted* by the engine, as of
/// when bfdb read it plus the time since -- i.e. how stale the stats picture
/// is. `None` before the first stat this run.
pub(crate) fn ingest_lag_secs(st: &InstanceState) -> Option<i64> {
    let (stat_time, _read_at) = (*st.last_stat.lock().ok()?)?;
    Some((chrono::Utc::now() - stat_time).num_seconds().max(0))
}

/// Set once in main: where the database lives, for health/metrics.
pub(crate) static DB_PATH: std::sync::OnceLock<PathBuf> = std::sync::OnceLock::new();

/// GET /api/admin/perf  — last session's DCS engine performance stats (admin only)
/// Snapshot this host's CPU/RAM/disk/GPU usage and available temperature
/// sensors. Two CPU refreshes with a short sleep in between are required
/// because CPU usage in `sysinfo` is a delta measurement -- a single refresh
/// right after process start always reads 0.
pub(crate) fn collect_hardware() -> serde_json::Value {
    use sysinfo::{Components, Disks, System};

    let mut sys = System::new_all();
    sys.refresh_cpu_usage();
    std::thread::sleep(sysinfo::MINIMUM_CPU_UPDATE_INTERVAL);
    sys.refresh_cpu_usage();
    sys.refresh_memory();

    let cpus = sys.cpus();
    let cpu_usage: f32 = if cpus.is_empty() {
        0.
    } else {
        cpus.iter().map(|c| c.cpu_usage()).sum::<f32>() / cpus.len() as f32
    };

    let disks = Disks::new_with_refreshed_list();
    let disk_rows: Vec<serde_json::Value> = disks
        .iter()
        .map(|d| {
            let total = d.total_space();
            let avail = d.available_space();
            let used = total.saturating_sub(avail);
            serde_json::json!({
                "mount": d.mount_point().to_string_lossy(),
                "total_bytes": total,
                "used_bytes": used,
            })
        })
        .collect();

    // Best-effort: not every sensor DCS servers expose is visible to Windows,
    // so this can legitimately come back empty depending on drivers/hardware.
    let components = Components::new_with_refreshed_list();
    let temp_rows: Vec<serde_json::Value> = components
        .iter()
        .filter(|c| !c.temperature().is_nan())
        .map(|c| {
            serde_json::json!({
                "label": c.label(),
                "celsius": c.temperature(),
            })
        })
        .collect();

    let gpu = collect_gpu();

    serde_json::json!({
        "cpu_count": cpus.len(),
        "cpu_usage_pct": cpu_usage,
        "mem_total_bytes": sys.total_memory(),
        "mem_used_bytes": sys.used_memory(),
        "disks": disk_rows,
        "temps": temp_rows,
        "gpu": gpu,
    })
}

/// Best-effort NVIDIA GPU stats via NVML. Returns null if NVML isn't
/// available (no NVIDIA driver, or no GPU) rather than failing the whole
/// hardware snapshot -- this endpoint should degrade gracefully.
pub(crate) fn collect_gpu() -> serde_json::Value {
    use nvml_wrapper::{enum_wrappers::device::TemperatureSensor, Nvml};

    let result: anyhow::Result<serde_json::Value> = (|| {
        let nvml = Nvml::init()?;
        let device = nvml.device_by_index(0)?;
        let name = device.name()?;
        let util = device.utilization_rates()?;
        let mem = device.memory_info()?;
        let temp = device.temperature(TemperatureSensor::Gpu).ok();
        Ok(serde_json::json!({
            "available": true,
            "name": name,
            "usage_pct": util.gpu,
            "mem_usage_pct": util.memory,
            "mem_total_bytes": mem.total,
            "mem_used_bytes": mem.used,
            "celsius": temp,
        }))
    })();

    match result {
        Ok(v) => v,
        Err(e) => {
            // This endpoint is polled on a timer; a box with no NVIDIA GPU
            // would otherwise log an identical warning every cycle forever.
            // Say it once.
            use std::sync::atomic::{AtomicBool, Ordering};
            static WARNED: AtomicBool = AtomicBool::new(false);
            if !WARNED.swap(true, Ordering::Relaxed) {
                log::warn!("GPU stats unavailable (NVML not loadable): {e:?} -- not logging this again");
            }
            serde_json::json!({ "available": false })
        }
    }
}

pub(crate) async fn api_admin_perf(
    session_id: Option<Uuid>,
    db: StatsDb,
    inst: Inst,
) -> std::result::Result<impl warp::Reply, Error> {
    require_role(session_id, db.clone(), admin::Role::OPS).await?;

    // Prefer a live snapshot from the running engine (query-perf, added
    // alongside query-objectives) over the last *completed* session's
    // stats, so this has data throughout an active round instead of only
    // after it ends. Falls back to the persisted last-session data when
    // bflib isn't reachable (no active round, mission still loading, etc.)
    // -- same degraded behavior as before this existed.
    let live: Option<SessionEnd> = match tokio::time::timeout(
        std::time::Duration::from_secs(3),
        call_engine_rpc_str(&db, &inst, "query-perf", vec![]),
    )
    .await
    {
        Ok(Ok(json)) => match serde_json::from_str(&json) {
            Ok(e) => Some(e),
            Err(e) => {
                log::warn!("api_admin_perf: query-perf returned unparseable JSON: {e}");
                None
            }
        },
        Ok(Err(e)) => {
            log::warn!("api_admin_perf: query-perf RPC failed: {}", e.0);
            None
        }
        Err(_) => None, // timed out -- engine unreachable, fall back silently
    };

    let data = task::block_in_place(|| -> Result<String> {
        let hardware = collect_hardware();
        let end = match live {
            Some(e) => Some(e),
            None => db.latest_session_end()?,
        };
        let json = match end {
            None => serde_json::json!({ "available": false, "hardware": hardware }),
            Some(e) => {
                let ps = e.engine.stat(&e.frame);
                fn row(s: &dcso3::perf::HistStat) -> serde_json::Value {
                    serde_json::json!({
                        "name":  s.name,
                        "unit":  s.unit,
                        "n":     s.n,
                        "mean":  s.mean,
                        "p50":   s.fifty,
                        "p90":   s.ninety,
                        "p99":   s.ninety_nine,
                        "p999":  s.ninety_nine_nine,
                    })
                }
                let engine_rows: Vec<serde_json::Value> = vec![
                    row(&ps.frame),
                    row(&ps.timed_events),
                    row(&ps.slow_timed),
                    row(&ps.dcs_events),
                    row(&ps.dcs_hooks),
                    row(&ps.unit_positions),
                    row(&ps.player_positions),
                    row(&ps.ewr_tracks),
                    row(&ps.ewr_reports),
                    row(&ps.unit_culling),
                    row(&ps.remark_objectives),
                    row(&ps.update_jtac_contacts),
                    row(&ps.do_repairs),
                    row(&ps.spawn_queue),
                    row(&ps.spawn),
                    row(&ps.despawn),
                    row(&ps.advise_captured),
                    row(&ps.advise_capturable),
                    row(&ps.jtac_target_positions),
                    row(&ps.process_messages),
                    row(&ps.snapshot),
                    row(&ps.logistics),
                    row(&ps.logistics_distribute),
                    row(&ps.logistics_deliver),
                    row(&ps.logistics_transfer),
                    row(&ps.logistics_sync_from),
                    row(&ps.logistics_sync_to),
                    row(&ps.logistics_convoy),
                    row(&ps.logistics_air_routes),
                    row(&ps.logistics_sea_routes),
                    row(&ps.frontline),
                ];
                use dcso3::perf::HistStat as HS;
                let a = &e.api;
                let api_rows: Vec<serde_json::Value> = vec![
                    row(&HS::new(&a.get_position, "Unit.getPosition", false)),
                    row(&HS::new(&a.get_point, "Unit.getPoint", false)),
                    row(&HS::new(&a.get_velocity, "Unit.getVelocity", false)),
                    row(&HS::new(&a.in_air, "Unit.inAir", false)),
                    row(&HS::new(&a.get_ammo, "Unit.getAmmo", false)),
                    row(&HS::new(&a.add_group, "Coalition.addGroup", false)),
                    row(&HS::new(
                        &a.add_static_object,
                        "Coalition.addStaticObject",
                        false,
                    )),
                    row(&HS::new(&a.unit_is_exist, "Unit.isExist", false)),
                    row(&HS::new(&a.unit_get_by_name, "Unit.getByName", false)),
                    row(&HS::new(&a.unit_get_desc, "Unit.getDesc", false)),
                    row(&HS::new(&a.land_is_visible, "Land.isVisible", false)),
                    row(&HS::new(&a.land_get_height, "Land.getHeight", false)),
                    row(&HS::new(
                        &a.timer_schedule_function,
                        "Timer.scheduleFunction",
                        false,
                    )),
                    row(&HS::new(
                        &a.timer_remove_function,
                        "Timer.removeFunction",
                        false,
                    )),
                    row(&HS::new(&a.timer_get_time, "Timer.getTime", false)),
                    row(&HS::new(&a.timer_get_abs_time, "Timer.getAbsTime", false)),
                    row(&HS::new(&a.timer_get_time0, "Timer.getTime0", false)),
                ];
                let logistics_items = ps.logistics_items;
                serde_json::json!({
                    "available": true,
                    "time": e.time.to_rfc3339(),
                    "engine": engine_rows,
                    "api": api_rows,
                    "logistics_items": logistics_items,
                    "hardware": hardware,
                })
            }
        };
        Ok(serde_json::to_string(&json)?)
    })?;
    Ok(json_response(data))
}

/// GET /api/admin/perf-history  — per-session perf data for charts (admin only)
pub(crate) async fn api_admin_perf_history(
    session_id: Option<Uuid>,
    db: StatsDb,
) -> std::result::Result<impl warp::Reply, Error> {
    require_role(session_id, db.clone(), admin::Role::OPS).await?;
    let data = task::block_in_place(|| -> Result<String> {
        let history = db.session_perf_history(50)?;
        fn row(s: &dcso3::perf::HistStat) -> serde_json::Value {
            serde_json::json!({ "name": s.name, "mean": s.mean, "p50": s.fifty, "p90": s.ninety, "p99": s.ninety_nine, "p999": s.ninety_nine_nine, "n": s.n, "unit": s.unit })
        }
        let entries: Vec<_> = history.iter().map(|e| {
            let ps = e.engine.stat(&e.frame);
            serde_json::json!({
                "time": e.time.to_rfc3339(),
                "frame":             { "mean": ps.frame.mean,             "p99": ps.frame.ninety_nine },
                "timed_events":      { "mean": ps.timed_events.mean,      "p99": ps.timed_events.ninety_nine },
                "slow_timed":        { "mean": ps.slow_timed.mean,        "p99": ps.slow_timed.ninety_nine },
                "dcs_events":        { "mean": ps.dcs_events.mean,        "p99": ps.dcs_events.ninety_nine },
                "spawn":             { "mean": ps.spawn.mean,             "p99": ps.spawn.ninety_nine },
                "despawn":           { "mean": ps.despawn.mean,           "p99": ps.despawn.ninety_nine },
                "logistics":         { "mean": ps.logistics.mean,         "p99": ps.logistics.ninety_nine },
                "logistics_deliver": { "mean": ps.logistics_deliver.mean, "p99": ps.logistics_deliver.ninety_nine },
                "frontline":         { "mean": ps.frontline.mean,         "p99": ps.frontline.ninety_nine },
                "unit_positions":    { "mean": ps.unit_positions.mean,    "p99": ps.unit_positions.ninety_nine },
                "ewr_tracks":        { "mean": ps.ewr_tracks.mean,        "p99": ps.ewr_tracks.ninety_nine },
                "snapshot":          { "mean": ps.snapshot.mean,          "p99": ps.snapshot.ninety_nine },
            })
        }).collect();
        // Also include per-metric rows for full detail view
        let full: Vec<_> = history
            .iter()
            .map(|e| {
                let ps = e.engine.stat(&e.frame);
                let engine_rows: Vec<serde_json::Value> = vec![
                    row(&ps.frame),
                    row(&ps.timed_events),
                    row(&ps.slow_timed),
                    row(&ps.dcs_events),
                    row(&ps.dcs_hooks),
                    row(&ps.unit_positions),
                    row(&ps.player_positions),
                    row(&ps.spawn_queue),
                    row(&ps.spawn),
                    row(&ps.despawn),
                    row(&ps.logistics),
                    row(&ps.logistics_deliver),
                    row(&ps.logistics_distribute),
                    row(&ps.logistics_convoy),
                    row(&ps.logistics_air_routes),
                    row(&ps.logistics_sea_routes),
                    row(&ps.frontline),
                    row(&ps.snapshot),
                    row(&ps.ewr_tracks),
                    row(&ps.ewr_reports),
                    row(&ps.jtac_target_positions),
                    row(&ps.update_jtac_contacts),
                    row(&ps.do_repairs),
                ];
                serde_json::json!({ "time": e.time.to_rfc3339(), "metrics": engine_rows })
            })
            .collect();
        Ok(serde_json::to_string(
            &serde_json::json!({ "timeline": entries, "sessions": full }),
        )?)
    })?;
    Ok(json_response(data))
}
