//! Engine RPCs over netidx and the cached objective list.

use crate::*;

/// `call_engine_rpc_str` for a procedure the engine on the server may not
/// publish yet (see `StatsDb::call_engine_rpc_optional`): unanswered calls
/// don't trip the instance's breaker, so it can afford to wait longer than
/// the counted calls -- 15s covers the first call's subscription to the
/// procedure (netidx gives up on that itself after 10s, with an error that
/// says so) plus an engine tick that runs long.
pub(crate) async fn call_engine_rpc_str_optional(
    db: &StatsDb,
    inst: &InstanceState,
    proc_name: &str,
    args: Vec<(&str, netidx::publisher::Value)>,
) -> std::result::Result<std::string::String, Error> {
    rpc_str_uncounted(db, inst, proc_name, args, false).await
}

/// `call_engine_rpc_str_optional` that is not refused while the breaker is
/// open (see `StatsDb::call_engine_rpc_patient`): orders, and the command
/// map a commander is giving them from.
pub(crate) async fn call_engine_rpc_str_patient(
    db: &StatsDb,
    inst: &InstanceState,
    proc_name: &str,
    args: Vec<(&str, netidx::publisher::Value)>,
) -> std::result::Result<std::string::String, Error> {
    rpc_str_uncounted(db, inst, proc_name, args, true).await
}

pub(crate) async fn rpc_str_uncounted(
    db: &StatsDb,
    inst: &InstanceState,
    proc_name: &str,
    args: Vec<(&str, netidx::publisher::Value)>,
    patient: bool,
) -> std::result::Result<std::string::String, Error> {
    use netidx::publisher::Value;
    const RPC_TIMEOUT: std::time::Duration = std::time::Duration::from_secs(15);
    let call = async {
        if patient {
            db.call_engine_rpc_patient(inst, proc_name, args).await
        } else {
            db.call_engine_rpc_optional(inst, proc_name, args).await
        }
    };
    let reply = match tokio::time::timeout(RPC_TIMEOUT, call).await {
        Ok(Ok(v)) => v,
        Ok(Err(e)) => {
            log::warn!("[{}] {proc_name}: {e}", inst.id);
            return Err(Error(websec::unavailable(format!("game server not reachable: {e}"))));
        }
        Err(_) => {
            log::warn!("[{}] {proc_name}: no reply in {}s", inst.id, RPC_TIMEOUT.as_secs());
            return Err(Error(websec::unavailable(format!(
                "the game server did not answer {proc_name} within {}s -- the engine may be busy; this page retries every minute",
                RPC_TIMEOUT.as_secs()
            ))))
        }
    };
    match reply {
        Value::Error(e) => Err(Error(websec::engine_refused(e.to_string()))),
        Value::String(s) => Ok(s.to_string()),
        other => Err(Error(anyhow::anyhow!("unexpected RPC reply: {other:?}"))),
    }
}

pub(crate) async fn call_engine_rpc_str(
    db: &StatsDb,
    inst: &InstanceState,
    proc_name: &str,
    args: Vec<(&str, netidx::publisher::Value)>,
) -> std::result::Result<std::string::String, Error> {
    use netidx::publisher::Value;
    // Bounded here, not only by callers: an RPC with no timeout of its own
    // hangs a request forever when the engine stalls mid-call, and because the
    // future is dropped by the timeout the instance's breaker hears about it.
    const RPC_TIMEOUT: std::time::Duration = std::time::Duration::from_secs(10);
    let reply = match tokio::time::timeout(RPC_TIMEOUT, db.call_engine_rpc(inst, proc_name, args)).await {
        Ok(Ok(v)) => v,
        // Transport trouble: the engine is down, restarting or not answering.
        // The detail names our own netidx paths, which is fine to show.
        Ok(Err(e)) => return Err(Error(websec::unavailable(format!("game server not reachable: {e}")))),
        Err(_) => {
            return Err(Error(websec::unavailable(format!(
                "game server did not answer {proc_name} within {}s",
                RPC_TIMEOUT.as_secs()
            ))))
        }
    };
    match reply {
        // The engine's own refusal ("you must be in a slot") is written for
        // the player, so it goes back as-is.
        Value::Error(e) => Err(Error(websec::engine_refused(e.to_string()))),
        Value::String(s) => Ok(s.to_string()),
        other => Err(Error(anyhow::anyhow!("unexpected RPC reply: {other:?}"))),
    }
}

/// The engine's own objective list for `inst` (`query-objectives`, JSON),
/// or `Err` when it can't be had right now.
///
/// Public routes polled by every dashboard and the Discord bot need it, and
/// each call is an RPC answered inside the DCS frame. One call per instance
/// per few seconds serves all of them (single-flight: callers arriving
/// mid-refresh wait for it rather than firing their own).
pub(crate) async fn engine_objectives(db: &StatsDb, inst: &Inst) -> Arc<std::result::Result<std::string::String, ()>> {
    static LIVE_OBJECTIVES: std::sync::LazyLock<
        websec::CacheMap<InstanceId, Arc<std::result::Result<std::string::String, ()>>>,
    > = std::sync::LazyLock::new(websec::CacheMap::new);
    LIVE_OBJECTIVES
        .entry(&inst.id)
        .get_or_refresh(std::time::Duration::from_secs(5), || async {
            let r = match tokio::time::timeout(
                // netidx RPC round-trips under a populated mission routinely run
                // past 3s even though the engine frame time is fine (batch commit
                // + poll contention with the tacmap/gci pollers). 8s matches the
                // gci client and stops the every-30s "timed out" spam; we still
                // fall back to persisted flags if it really is unreachable.
                std::time::Duration::from_secs(8),
                call_engine_rpc_str(db, inst, "query-objectives", vec![]),
            )
            .await
            {
                Ok(Ok(json)) => Ok(json),
                Ok(Err(e)) => {
                    log::warn!("api_objectives: query-objectives RPC failed: {}", e.0);
                    Err(())
                }
                Err(_) => {
                    log::warn!("api_objectives: query-objectives RPC timed out after 8s, engine may be unreachable");
                    Err(())
                }
            };
            Ok::<_, std::convert::Infallible>(Arc::new(r))
        })
        .await
        .unwrap_or_else(|e| match e {})
}

/// Names of the objectives the running engine has, for the active round's
/// maps; `None` when it isn't answering (then nothing can be ruled out).
pub(crate) async fn engine_objective_names(db: &StatsDb, inst: &Inst) -> Option<std::collections::HashSet<std::string::String>> {
    let live = engine_objectives(db, inst).await;
    let json = live.as_ref().as_ref().ok()?;
    let list = serde_json::from_str::<Vec<bfprotocols::api::ObjectiveInfo>>(json).ok()?;
    Some(list.into_iter().map(|o| o.name.to_string()).collect())
}
