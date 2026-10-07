//! Commander access (`Cfg::command`).
//!
//! A pilot's rank comes from their campaign score: the leaderboard's score,
//! all-time across every public server. Reaching the server's
//! `commander_rank` makes them a commander of whichever coalition they are on
//! there. An admin can grant it to a pilot of any rank, or take it from one
//! who has earned it.
//!
//! bfdb is where the scores are, so it decides. It gates the dashboard's
//! orders itself and pushes the roster to the engine (`set-commanders`) once
//! a minute and whenever an admin changes it, so the F10 menu and chat orders
//! in game follow the same rule. The Discord bot reads the roster to keep the
//! Discord "Commander" role in step.

use crate::db::{Aggregates, InstanceState, StatsDb};
use crate::instance::InstanceId;
use anyhow::Result;
use bfprotocols::{
    cfg::{rank_min_score, rank_tier, CommandCfg},
    command::{CommanderGrant, CommanderStatus, Commanders},
};
use chrono::{DateTime, Utc};
use dcso3::{coalition::Side, net::Ucid};
use serde_derive::{Deserialize, Serialize};
use std::{
    collections::HashMap,
    sync::{Arc, LazyLock, Mutex},
    time::{Duration, Instant},
};
use tokio::task;

/// How long a server's roster and command config are reused.
const ROSTER_TTL: Duration = Duration::from_secs(30);
const CFG_TTL: Duration = Duration::from_secs(60);
/// How often the roster is pushed to the engine.
const PUSH_EVERY: Duration = Duration::from_secs(60);

/// An admin's override, as stored.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub(crate) struct GrantRecord {
    pub(crate) grant: CommanderGrant,
    /// The admin's Discord name.
    pub(crate) by: String,
    pub(crate) at: DateTime<Utc>,
}

/// The leaderboard's campaign score. Must match `computeScore` in
/// `bfweb/src/pages/Leaderboard.tsx`, which shows it.
pub(crate) fn pilot_score(a: &Aggregates) -> f64 {
    a.air_kills as f64 * 3.
        + a.ground_kills as f64 * 2.
        + a.captures as f64 * 5.
        + a.repairs as f64
        + a.supply_transfers as f64
        + a.troops as f64 * 0.5
        + a.farps as f64 * 4.
        + a.deploys as f64 * 2.
        + a.actions as f64
        - a.deaths as f64 * 2.
}

fn side_name(s: Side) -> Option<String> {
    match s {
        Side::Blue => Some("Blue".into()),
        Side::Red => Some("Red".into()),
        Side::Neutral => None,
    }
}

static CFG: LazyLock<Mutex<HashMap<InstanceId, (Instant, CommandCfg)>>> =
    LazyLock::new(|| Mutex::new(HashMap::new()));

/// This server's `command` block, read from its engine config file (the same
/// file the wiki's numbers come from). Defaults if there is no file or no
/// block, or it doesn't parse.
pub(crate) fn command_cfg(inst: &InstanceState) -> CommandCfg {
    if let Some((at, c)) = CFG.lock().unwrap().get(&inst.id) {
        if at.elapsed() < CFG_TTL {
            return c.clone();
        }
    }
    let c = inst
        .cfg
        .engine_config
        .as_ref()
        .and_then(|p| std::fs::read_to_string(p).ok())
        .and_then(|t| serde_json::from_str::<serde_json::Value>(t.trim_start_matches('\u{feff}')).ok())
        .and_then(|v| v.get("command").cloned())
        .and_then(|v| match serde_json::from_value::<CommandCfg>(v) {
            Ok(c) => Some(c),
            Err(e) => {
                log::warn!("[{}] the engine config's command block doesn't parse ({e}); using defaults", inst.id);
                None
            }
        })
        .unwrap_or_default();
    CFG.lock().unwrap().insert(inst.id.clone(), (Instant::now(), c.clone()));
    c
}

/// Pilots linked to a dashboard admin's Discord account, noted as they use
/// the dashboard. Admins command whatever their rank; this is how the engine
/// learns that (it only knows its own config's admins).
static ADMIN_PILOTS: LazyLock<Mutex<std::collections::HashSet<Ucid>>> =
    LazyLock::new(|| Mutex::new(std::collections::HashSet::new()));

/// `ucid` belongs to a dashboard admin.
pub(crate) fn note_admin(ucid: Ucid) {
    if ADMIN_PILOTS.lock().unwrap().insert(ucid) {
        invalidate();
    }
}

type Roster = Arc<Vec<CommanderStatus>>;

static ROSTERS: LazyLock<Mutex<HashMap<InstanceId, (Instant, Roster)>>> =
    LazyLock::new(|| Mutex::new(HashMap::new()));

/// Forget the cached rosters (an admin changed a grant).
pub(crate) fn invalidate() {
    ROSTERS.lock().unwrap().clear();
}

/// Every pilot with a score, as seen from this server: their rank, side here
/// and whether they command it. Blocking; cached for `ROSTER_TTL`.
pub(crate) fn roster(db: &StatsDb, inst: &InstanceState) -> Result<Roster> {
    if let Some((at, r)) = ROSTERS.lock().unwrap().get(&inst.id) {
        if at.elapsed() < ROSTER_TTL {
            return Ok(r.clone());
        }
    }
    let cfg = command_cfg(inst);
    let need = rank_min_score(cfg.commander_rank);
    let sides: HashMap<Ucid, Side> = db.all_pilot_sides(&inst.id)?.into_iter().collect();
    let grants = db.commander_grants()?;
    let admins = ADMIN_PILOTS.lock().unwrap().clone();
    let mut out: Vec<CommanderStatus> = db
        .pilot_leaderboard(None)?
        .into_iter()
        .map(|(ucid, name, agg)| {
            let score = pilot_score(&agg);
            let side = sides.get(&ucid).copied().and_then(side_name);
            let g = grants.get(&ucid);
            let earned = score >= need;
            let admin = admins.contains(&ucid);
            let commander = side.is_some()
                && (admin
                    || match g.map(|g| g.grant) {
                        Some(CommanderGrant::Granted) => true,
                        Some(CommanderGrant::Revoked) => false,
                        None => earned,
                    });
            CommanderStatus {
                ucid,
                name: name.to_string(),
                side,
                score,
                tier: rank_tier(score),
                commander_rank: cfg.commander_rank,
                commander_score: need,
                grant: g.map(|g| g.grant),
                grant_by: g.map(|g| g.by.clone()),
                grant_at: g.map(|g| g.at.to_rfc3339()),
                admin,
                commander,
            }
        })
        .collect();
    // A pilot granted access, or an admin, who has never scored still
    // belongs on it.
    let unscored: Vec<Ucid> = grants
        .keys()
        .chain(admins.iter())
        .filter(|u| !out.iter().any(|s| s.ucid == **u))
        .copied()
        .collect::<std::collections::BTreeSet<_>>()
        .into_iter()
        .collect();
    for ucid in unscored {
        let g = grants.get(&ucid);
        let admin = admins.contains(&ucid);
        let side = sides.get(&ucid).copied().and_then(side_name);
        out.push(CommanderStatus {
            ucid,
            name: ucid.to_string(),
            commander: side.is_some() && (admin || g.map_or(false, |g| g.grant == CommanderGrant::Granted)),
            side,
            score: 0.,
            tier: 1,
            commander_rank: cfg.commander_rank,
            commander_score: need,
            grant: g.map(|g| g.grant),
            grant_by: g.map(|g| g.by.clone()),
            grant_at: g.map(|g| g.at.to_rfc3339()),
            admin,
        });
    }
    out.sort_by(|a, b| b.score.partial_cmp(&a.score).unwrap_or(std::cmp::Ordering::Equal));
    let r = Arc::new(out);
    ROSTERS.lock().unwrap().insert(inst.id.clone(), (Instant::now(), r.clone()));
    Ok(r)
}

/// One pilot's standing on this server, if they have ever scored or been
/// granted anything.
pub(crate) fn status_of(db: &StatsDb, inst: &InstanceState, ucid: &Ucid) -> Result<Option<CommanderStatus>> {
    Ok(roster(db, inst)?.iter().find(|s| s.ucid == *ucid).cloned())
}

/// May `ucid`, on `side`, give orders on this server? True for everyone when
/// the server doesn't require commanders.
pub(crate) fn may_command(db: &StatsDb, inst: &InstanceState, ucid: &Ucid, side: Side) -> Result<bool> {
    if !command_cfg(inst).require_commander {
        return Ok(true);
    }
    is_commander(db, inst, ucid, side)
}

/// Is `ucid` a commander of `side` here, whatever the server requires for
/// ground orders? The HQ override needs this.
pub(crate) fn is_commander(db: &StatsDb, inst: &InstanceState, ucid: &Ucid, side: Side) -> Result<bool> {
    let want = side_name(side);
    Ok(roster(db, inst)?.iter().any(|s| s.ucid == *ucid && s.commander && s.side == want))
}

/// Why `status` can't give orders, for the dashboard.
pub(crate) fn refusal(inst: &InstanceState, status: Option<&CommanderStatus>) -> String {
    let cfg = command_cfg(inst);
    let need = rank_min_score(cfg.commander_rank);
    let title = RANK_TITLES[(cfg.commander_rank.clamp(1, 8) - 1) as usize];
    match status {
        Some(s) if s.grant == Some(CommanderGrant::Revoked) => {
            "an admin has withdrawn your commander access on this server".into()
        }
        Some(s) => format!(
            "orders need a commander: reach {title} (campaign score {need:.0}; you have {:.0}), or ask an admin",
            s.score
        ),
        None => format!("orders need a commander: reach {title} (campaign score {need:.0}), or ask an admin"),
    }
}

/// Rank titles by tier, the NATO service's (the VKS ladder has the same
/// tiers). Mirrors `bfweb/src/ranks.ts`.
const RANK_TITLES: [&str; 8] = [
    "2nd Lieutenant",
    "1st Lieutenant",
    "Captain",
    "Major",
    "Lieutenant Colonel",
    "Colonel",
    "Brigadier General",
    "Major General",
];

/// The roster as the engine wants it.
pub(crate) fn commanders(roster: &[CommanderStatus]) -> Commanders {
    let mut c = Commanders::default();
    for s in roster.iter().filter(|s| s.commander) {
        match s.side.as_deref() {
            Some("Blue") => c.blue.push(s.ucid),
            Some("Red") => c.red.push(s.ucid),
            _ => (),
        }
    }
    c
}

/// Tell the engine who commands. Quietly does nothing if it isn't there or
/// is a DLL that predates `set-commanders`.
pub(crate) async fn push(db: &StatsDb, inst: &InstanceState) {
    let roster = match task::block_in_place(|| roster(db, inst)) {
        Ok(r) => r,
        Err(e) => {
            log::warn!("[{}] commanders: building the roster: {e:?}", inst.id);
            return;
        }
    };
    let json = match serde_json::to_string(&commanders(&roster)) {
        Ok(j) => j,
        Err(e) => {
            log::warn!("[{}] commanders: {e:?}", inst.id);
            return;
        }
    };
    let call = db.call_engine_rpc_optional(
        inst,
        "set-commanders",
        vec![("commanders", netidx::publisher::Value::from(json))],
    );
    match tokio::time::timeout(Duration::from_secs(10), call).await {
        Ok(Ok(_)) => (),
        Ok(Err(e)) => log::debug!("[{}] commanders: engine didn't take the roster: {e}", inst.id),
        Err(_) => log::debug!("[{}] commanders: engine didn't answer set-commanders", inst.id),
    }
}

/// Push the roster to one server's engine every `PUSH_EVERY`.
pub(crate) async fn pusher(db: StatsDb, inst: Arc<InstanceState>) {
    // Let the engine connection settle first.
    tokio::time::sleep(Duration::from_secs(20)).await;
    loop {
        push(&db, &inst).await;
        tokio::time::sleep(PUSH_EVERY).await;
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn score_matches_the_leaderboard() {
        let a = Aggregates {
            air_kills: 4,
            ground_kills: 10,
            captures: 2,
            repairs: 3,
            supply_transfers: 5,
            troops: 7,
            farps: 1,
            deploys: 2,
            actions: 6,
            deaths: 3,
            hours: 12.,
            donated_points: 0,
        };
        // 12 + 20 + 10 + 3 + 5 + 3.5 + 4 + 4 + 6 - 6
        assert_eq!(pilot_score(&a), 61.5);
        assert_eq!(rank_tier(pilot_score(&a)), 4);
    }
}
