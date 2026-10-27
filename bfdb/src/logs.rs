//! bfdb's own log broadcast, the engine log stream, and the log routes.

use crate::*;

// Large enough that `GET /api/logs/bfdb` (token-gated, for remote debugging)
// can hand back a meaningful window, not just the last few seconds.
pub(crate) const LOG_HISTORY_CAP: usize = 10_000;

pub(crate) type LogHistory = Arc<Mutex<VecDeque<String>>>;

#[derive(Debug, Clone, Serialize)]
pub(crate) struct LogLine {
    pub(crate) ts:     String,
    pub(crate) level:  String,
    pub(crate) target: String,
    pub(crate) msg:    String,
}

pub(crate) struct BroadcastLogger {
    pub(crate) inner:   env_logger::Logger,
    pub(crate) tx:      broadcast::Sender<String>,
    pub(crate) history: LogHistory,
}

impl log::Log for BroadcastLogger {
    fn enabled(&self, meta: &log::Metadata) -> bool {
        self.inner.enabled(meta)
    }

    fn log(&self, record: &log::Record) {
        if !self.enabled(record.metadata()) {
            return;
        }
        self.inner.log(record);
        let line = LogLine {
            ts:     chrono::Utc::now().to_rfc3339_opts(chrono::SecondsFormat::Millis, true),
            level:  record.level().to_string(),
            target: record.target().to_string(),
            msg:    record.args().to_string(),
        };
        if let Ok(json) = serde_json::to_string(&line) {
            let mut hist = self.history.lock().unwrap();
            if hist.len() >= LOG_HISTORY_CAP {
                hist.pop_front();
            }
            hist.push_back(json.clone());
            let _ = self.tx.send(json);
        }
    }

    fn flush(&self) {
        self.inner.flush();
    }
}

/// Move an existing log file aside as `<stem><UTC-timestamp>.<ext>` so each run
/// starts a fresh file. No-op if the file doesn't exist. Mirrors bflib's
/// `rotate_log`.
pub(crate) fn rotate_log(path: &std::path::Path) {
    if !path.exists() {
        return;
    }
    let ext = path.extension().and_then(|e| e.to_str()).unwrap_or("log");
    let stem = path.file_stem().and_then(|s| s.to_str()).unwrap_or("bfdb");
    let ts: std::string::String = chrono::Utc::now()
        .to_rfc3339_opts(chrono::SecondsFormat::Secs, true)
        .chars()
        .filter(|c| *c != '-' && *c != ':')
        .collect();
    let rotated = path.with_file_name(format!("{stem}{ts}.{ext}"));
    if let Err(e) = std::fs::rename(path, &rotated) {
        eprintln!("could not rotate log file {path:?} to {rotated:?}: {e}");
    }
}

/// GET /api/logs/:source  — plain-text tail of an in-memory log backlog.
///
/// `source` is `engine` (bflib's live engine log, streamed in over netidx),
/// `bfdb` (this process's own log), or one of the DCSServerBot-side sources
/// handled below: `issues` (the log analyzer's report), `archive` (the
/// persistent log archive), `bot` / `service` / `netidx` / `bfdb_boot`. Auth is a single static bearer token
/// (`--log-read-token`), passed as `?token=` or `Authorization: Bearer`. When
/// no token is configured the endpoint is disabled (404).
///
/// Query params: `lines` (tail length, default 500, capped at the buffer),
/// `level` (`all` default, or `error` — engine source only, ERROR/WARN lines),
/// `grep` (case-insensitive substring filter, applied before the tail).
pub(crate) async fn api_logs(
    source: String,
    q: std::collections::HashMap<std::string::String, std::string::String>,
    auth_header: Option<std::string::String>,
    expected_token: Arc<Option<std::string::String>>,
    db: StatsDb,
    bfdb_log: LogHistory,
    inst: Inst,
    bot_cfg: Arc<Option<BotLinkConfig>>,
    ip: std::net::IpAddr,
) -> Response {
    fn text(status: warp::http::StatusCode, body: impl Into<String>) -> Response {
        warp::http::Response::builder()
            .status(status)
            .header("content-type", "text/plain; charset=utf-8")
            .body(body.into())
            .unwrap()
            .into_response()
    }

    let Some(expected) = expected_token.as_ref() else {
        return text(warp::http::StatusCode::NOT_FOUND, "log endpoints are disabled (no --log-read-token)\n");
    };
    // `?token=` is kept on purpose (tooling that cannot set headers relies on
    // it); bfdb writes no access log, so it never lands in one of ours.
    if let Err((status, why)) = check_static_token(&q, auth_header.as_deref(), &[expected.as_str()], ip) {
        return text(status, why);
    }

    // Sources the DCSServerBot plugin owns (loganalyzer.py / opsapi.py):
    //   issues   the log analyzer's Markdown issue report
    //   archive  the persistent log archive -- index as JSON, or one day's
    //            text with ?source=<engine_X|dcs_X|bfdb|bot>&date=YYYY-MM-DD
    //   bot | service | netidx | bfdb_boot   tails of the bot-side logs
    // Survive bfdb restarts, unlike the in-memory `engine` / `bfdb` tails.
    if matches!(source.as_str(), "issues" | "archive" | "bot" | "service" | "netidx" | "bfdb_boot") {
        let Some(cfg) = bot_cfg.as_ref() else {
            return text(
                warp::http::StatusCode::SERVICE_UNAVAILABLE,
                "this source comes from DCSServerBot, which bfdb is not configured to reach \
                 (--dcsserverbot-url/--dcsserverbot-api-key)\n",
            );
        };
        let mut pairs: Vec<(std::string::String, std::string::String)> = q
            .iter()
            .filter(|(k, _)| k.as_str() != "token")
            .map(|(k, v)| (k.clone(), v.clone()))
            .collect();
        let path = match source.as_str() {
            "issues" => "issues/report",
            "archive" if q.contains_key("date") => {
                pairs.push(("format".into(), "text".into()));
                "archive/read"
            }
            "archive" => "archive",
            _ => {
                pairs.push(("which".into(), source.clone()));
                "logs"
            }
        };
        let http = http_client();
        let resp = http
            .get(format!("{}/fowlengine/ops/{path}", cfg.base_url))
            .header("X-API-Key", &cfg.ops_key)
            .query(&pairs)
            .timeout(std::time::Duration::from_secs(60))
            .send()
            .await;
        let resp = match resp {
            Ok(r) => r,
            Err(e) => {
                return text(
                    warp::http::StatusCode::BAD_GATEWAY,
                    format!("the bot is not answering: {e}\n"),
                )
            }
        };
        let status = resp.status().as_u16();
        let body = resp.text().await.unwrap_or_default();
        let body = if path == "logs" {
            // {"lines": [...]} -> plain text, like the other sources
            serde_json::from_str::<serde_json::Value>(&body)
                .ok()
                .and_then(|v| {
                    v.get("lines").and_then(|l| l.as_array()).map(|a| {
                        a.iter().filter_map(|x| x.as_str()).collect::<Vec<_>>().join("\n") + "\n"
                    })
                })
                .unwrap_or(body)
        } else {
            body
        };
        return text(
            warp::http::StatusCode::from_u16(status).unwrap_or(warp::http::StatusCode::BAD_GATEWAY),
            body,
        );
    }

    let level = q.get("level").map(|s| s.as_str()).unwrap_or("all");
    let grep = q.get("grep").map(|s| s.to_ascii_lowercase());
    let want = q
        .get("lines")
        .and_then(|s| s.parse::<usize>().ok())
        .unwrap_or(500)
        .clamp(1, 50_000);

    let mut lines: Vec<std::string::String> = match source.as_str() {
        "engine" if level == "error" => db.engine_error_snapshot(&inst),
        "engine" => db.engine_log_snapshot(&inst),
        "bfdb" => {
            // stored as JSON LogLine objects -- flatten to "ts LEVEL target: msg"
            bfdb_log
                .lock()
                .unwrap()
                .iter()
                .map(|j| {
                    serde_json::from_str::<serde_json::Value>(j)
                        .ok()
                        .map(|v| {
                            format!(
                                "{} {:>5} {}: {}",
                                v.get("ts").and_then(|x| x.as_str()).unwrap_or(""),
                                v.get("level").and_then(|x| x.as_str()).unwrap_or(""),
                                v.get("target").and_then(|x| x.as_str()).unwrap_or(""),
                                v.get("msg").and_then(|x| x.as_str()).unwrap_or(""),
                            )
                        })
                        .unwrap_or_else(|| j.clone())
                })
                .collect()
        }
        other => {
            return text(
                warp::http::StatusCode::BAD_REQUEST,
                format!(
                    "unknown log source {other:?} (want engine, bfdb, issues, archive, bot, \
                     service, netidx or bfdb_boot)\n"
                ),
            )
        }
    };

    if let Some(g) = grep {
        lines.retain(|l| l.to_ascii_lowercase().contains(&g));
    }
    let start = lines.len().saturating_sub(want);
    let out = lines[start..].join("\n");
    text(warp::http::StatusCode::OK, format!("{out}\n"))
}

/// WebSocket handler for `/ws/logs` — streams real-time bfdb log lines (ops role).
pub(crate) async fn ws_logs_handler(
    ws: warp::ws::Ws,
    ip: std::net::IpAddr,
    session_id: Option<Uuid>,
    db: StatsDb,
    tx: broadcast::Sender<String>,
    history: LogHistory,
) -> impl Reply {
    let slot = match admin_ws_gate(&db, session_id, ip) {
        Ok(s) => s,
        Err(r) => return r,
    };
    ws.on_upgrade(move |socket| async move {
        let _slot = slot;
        ws_logs(socket, tx.subscribe(), history).await
    })
    .into_response()
}

pub(crate) async fn ws_logs(
    ws: WebSocket,
    mut rx: broadcast::Receiver<String>,
    history: LogHistory,
) {
    let (mut sink, mut stream) = ws.split();
    // Collect history without holding the lock across await points
    let snapshot: Vec<String> = history.lock().unwrap().iter().cloned().collect();
    for line in snapshot {
        if sink.send(Message::text(line)).await.is_err() {
            return;
        }
    }
    loop {
        tokio::select! {
            msg = stream.next() => {
                match msg {
                    Some(Ok(m)) if m.is_close() => break,
                    None => break,
                    _ => {}
                }
            }
            msg = rx.recv() => {
                match msg {
                    Ok(json) => { if sink.send(Message::text(json)).await.is_err() { break; } }
                    Err(broadcast::error::RecvError::Lagged(_)) => continue,
                    Err(_) => break,
                }
            }
        }
    }
}

/// WebSocket handler for `/ws/engine-logs` — streams the live bflib engine
/// log (from the running DCS mission, via netidx) rather than bfdb's own
/// process log. No-op stream if bfdb wasn't started with --base (admin only).
pub(crate) async fn ws_engine_logs_handler(
    ws: warp::ws::Ws,
    ip: std::net::IpAddr,
    session_id: Option<Uuid>,
    db: StatsDb,
    inst: Inst,
) -> impl Reply {
    let slot = match admin_ws_gate(&db, session_id, ip) {
        Ok(s) => s,
        Err(r) => return r,
    };
    let (rx, history) = db.engine_log_subscribe(&inst);
    ws.on_upgrade(move |socket| async move {
        let _slot = slot;
        ws_engine_logs(socket, rx, history).await
    })
    .into_response()
}

pub(crate) async fn ws_engine_logs(
    ws: WebSocket,
    mut rx: broadcast::Receiver<String>,
    history: Vec<String>,
) {
    let (mut sink, mut stream) = ws.split();
    for line in history {
        if sink.send(Message::text(line)).await.is_err() {
            return;
        }
    }
    loop {
        tokio::select! {
            msg = stream.next() => {
                match msg {
                    Some(Ok(m)) if m.is_close() => break,
                    None => break,
                    _ => {}
                }
            }
            msg = rx.recv() => {
                match msg {
                    Ok(line) => { if sink.send(Message::text(line)).await.is_err() { break; } }
                    Err(broadcast::error::RecvError::Lagged(_)) => continue,
                    Err(_) => break,
                }
            }
        }
    }
}
