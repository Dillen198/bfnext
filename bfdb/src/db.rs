use crate::db_id;
use anyhow::{anyhow, bail, Context, Result};
use arrayvec::ArrayVec;
use bfprotocols::{
    cfg::{Cfg, LifeType, UnitTag, UnitTags, Vehicle},
    db::{
        group::GroupId,
        objective::{ObjectiveId, ObjectiveKind},
    },
    perf::PerfInner,
    shots::{Dead, Who},
    stats::{DetectionSource, EnId, Pos, Stat},
};
use chrono::prelude::*;
use dcso3::{
    coalition::Side,
    coord::LLPos,
    net::{SlotId, Ucid},
    perf::{HistogramSer, PerfInner as ApiPerfInner},
    warehouse::LiquidType,
    String,
};
use enumflags2::BitFlags;
use log::{debug, error, info, warn};
use netidx::{path::Path as NetidxPath, subscriber::Subscriber};
use netidx_archive::{
    config::file::Config as ArchiveFileCfg,
    logfile_collection::{ArchiveCollectionReader, ArchiveIndex},
};
use regex::Regex;
use serde::{Deserialize, Serialize};
use sled::{transaction::TransactionError, Db};
use smallvec::SmallVec;
use std::{
    collections::{Bound, HashMap, HashSet, VecDeque},
    io::{Read as IoRead, Write as IoWrite},
    ops::Deref,
    path::{Path, PathBuf},
    str::FromStr,
    sync::{
        atomic::{AtomicBool, Ordering},
        Arc, Mutex as StdMutex, RwLock,
    },
    time::Duration,
};
use tokio::{sync::broadcast, task};
use uuid::Uuid;
use yats::Tree;

use crate::instance::{InstanceCfg, InstanceId, Registry};

db_id!(KillId);
db_id!(RoundId);
db_id!(SortieId);
db_id!(CaptureId);
db_id!(DeployId);

/// A recorded capture event -- who took an objective and for which side,
/// as opposed to objective_captures which only tracks a running count with
/// no attribution or timeline. Lets API consumers (e.g. the Discord live
/// capture alert) show who actually did it.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub(crate) struct CaptureRecord {
    pub(crate) time: DateTime<Utc>,
    pub(crate) objective_name: std::string::String,
    pub(crate) side: Side,
    pub(crate) by: SmallVec<[Ucid; 1]>,
}

/// A recorded deploy event -- who deployed what, from which aircraft (if
/// known), and by which method (air drop vs. manual unpack). Distinct from
/// the plain `deploys` counter on Aggregates, which has no attribution or
/// timeline; backs the pilot profile's deploy log.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub(crate) struct DeployRecord {
    pub(crate) time: DateTime<Utc>,
    pub(crate) by: Ucid,
    pub(crate) deployable: std::string::String,
    pub(crate) aircraft: Option<std::string::String>,
    pub(crate) method: Option<std::string::String>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub(crate) struct BanRecord {
    pub(crate) name:      std::string::String,
    pub(crate) banned_at: DateTime<Utc>,
    pub(crate) until:     Option<DateTime<Utc>>,
    pub(crate) reason:    std::string::String,
}

// ── Wiki (bfwiki) types ───────────────────────────────────────────────

/// A single wiki page, keyed by slug (e.g. "gameplay/objectives") in the
/// `wiki_pages` tree. `section`/`order` drive the sidebar grouping in
/// bfwiki -- there's no separate "page tree" structure, just these two
/// fields sorted client-side.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub(crate) struct WikiPage {
    pub(crate) title:      std::string::String,
    pub(crate) section:    std::string::String,
    pub(crate) order:      i32,
    pub(crate) content:    std::string::String,
    pub(crate) updated_at: DateTime<Utc>,
    pub(crate) updated_by: std::string::String,
}

/// An uploaded image (screenshot etc.), keyed by a generated Uuid in the
/// `wiki_images` tree and referenced from page Markdown as
/// `/api/wiki/images/<uuid>`. Content-addressed by nothing in particular --
/// just an opaque id -- since these are inserted once and never edited.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub(crate) struct WikiImage {
    pub(crate) content_type: std::string::String,
    pub(crate) data:         Vec<u8>,
    pub(crate) uploaded_at:  DateTime<Utc>,
    pub(crate) uploaded_by:  std::string::String,
}

// ── Recon intel (TARPS) types ─────────────────────────────────────────

/// One reconnaissance capture (a TARPS photo) contributed through the
/// dashboard. Keyed by `(RoundId, Uuid)` in `intel_captures` -- the intel
/// picture is per-round and per-coalition, and wiped by a campaign reset.
/// `side` is snapshotted at upload time so a pilot who later switches
/// coalitions doesn't drag their old photos across with them.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub(crate) struct IntelCapture {
    pub(crate) round:            RoundId,
    pub(crate) side:             Side,        // Blue | Red -- owning coalition
    pub(crate) image_id:         Uuid,        // -> intel_images
    pub(crate) uploaded_by:      std::string::String, // session discord_id
    pub(crate) uploaded_by_name: std::string::String, // pilot display name (best effort)
    pub(crate) uploaded_at:      DateTime<Utc>,
    pub(crate) captured_at:      Option<DateTime<Utc>>, // from the filename, if present
    pub(crate) filename:         std::string::String,
    /// false -> the filename couldn't be parsed for coordinates; the client
    /// must drop this capture on the map manually before it renders.
    pub(crate) placed:           bool,
    pub(crate) lat:              f64,
    pub(crate) lon:              f64,
    pub(crate) alt_ft:           Option<f64>,
    pub(crate) heading_deg:      Option<f64>,
    pub(crate) pitch_deg:        Option<f64>,
    pub(crate) roll_deg:         Option<f64>,
    /// Opaque manual-nudge state set by the client (Phase 2 warp editor).
    /// Persisted verbatim so edits survive reload; bfdb never interprets it.
    pub(crate) adjust:           Option<IntelAdjust>,
    pub(crate) note:             Option<std::string::String>,
}

/// Manual alignment set by the client's warp editor. `corners`, when present,
/// are the 4 ground corners (TL, TR, BR, BL as `[lat, lon]`) the photo should
/// be pinned to, overriding the automatic pinhole projection entirely.
/// `opacity` (0..1) lets stacked photos be peeled apart. bfdb stores these
/// verbatim -- all warp math lives in the client.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub(crate) struct IntelAdjust {
    pub(crate) corners: Option<[[f64; 2]; 4]>,
    pub(crate) opacity: Option<f64>,
}

/// One piece of map markup drawn on the recon picture (freehand line, shape,
/// or text). Coalition-shared, per-round, wiped on reset. bfdb stores the
/// geometry verbatim -- `points` are `[lat, lon]` pairs whose meaning depends
/// on `kind` (pencil: the path; line: two ends; rect/circle: two defining
/// points; x/text: a single anchor).
#[derive(Debug, Clone, Serialize, Deserialize)]
pub(crate) struct IntelMarkup {
    pub(crate) round:   RoundId,
    pub(crate) side:    Side,
    pub(crate) kind:    std::string::String,
    pub(crate) points:  Vec<[f64; 2]>,
    pub(crate) color:   std::string::String,
    pub(crate) width:   f64,
    pub(crate) text:    Option<std::string::String>,
    pub(crate) by:      std::string::String,      // discord_id
    pub(crate) by_name: std::string::String,
    pub(crate) at:      DateTime<Utc>,
}

/// Index row for an uploaded recon photo, keyed by `IntelCapture.image_id`
/// in `intel_images`. `data` holds the bytes when they live in the DB;
/// `None` means the bytes are a file at `<intel_dir>/<image_id>` on disk
/// (see `--intel-dir`). Served only to same-coalition viewers.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub(crate) struct IntelImage {
    pub(crate) content_type: std::string::String,
    pub(crate) data:         Option<Vec<u8>>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub(crate) struct WeatherSnapshot {
    pub(crate) temp_c: f64,
    pub(crate) wind_speed_kts: f64,
    pub(crate) wind_from_deg: f64,
    pub(crate) cloud_base_m: f64,
    pub(crate) qnh_hpa: f64,
    pub(crate) cloud_density: Option<u8>,
    pub(crate) visibility_m: Option<f64>,
}

// ── Auth / session types ─────────────────────────────────────────────

/// CSRF state for one in-flight Discord OAuth login, plus which frontend
/// origin initiated it (so the callback can send the browser back to the
/// right site -- bfweb/bfsite/bfwiki are all separate origins now, not
/// embedded in bfdb, so a bare "/" redirect only ever lands on bfdb itself).
#[derive(Debug, Clone, Serialize, Deserialize)]
pub(crate) struct OAuthState {
    pub(crate) expires:   DateTime<Utc>,
    pub(crate) return_to: Option<std::string::String>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub(crate) struct SessionData {
    pub(crate) discord_id: std::string::String,
    pub(crate) username:   std::string::String,
    pub(crate) avatar:     Option<std::string::String>,
    pub(crate) is_admin:   bool,
    pub(crate) expires:    DateTime<Utc>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub(crate) struct TrailPoint {
    pub(crate) unit_id: std::string::String,
    pub(crate) lat:     f64,
    pub(crate) lon:     f64,
    pub(crate) alt:     f64,
    pub(crate) hdg:     f64,
    pub(crate) ts:      i64,
}

#[derive(Debug, Clone, Copy, Default, Serialize, Deserialize)]
pub(crate) struct Aggregates {
    pub(crate) air_kills: u32,
    pub(crate) ground_kills: u32,
    pub(crate) captures: u32,
    pub(crate) repairs: u32,
    pub(crate) supply_transfers: u32,
    pub(crate) troops: u32,
    pub(crate) farps: u32,
    pub(crate) deploys: u32,
    pub(crate) actions: u32,
    pub(crate) deaths: u32,
    pub(crate) hours: f32,
    pub(crate) donated_points: u32,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub(crate) struct Pilot {
    pub(crate) name: ArrayVec<String, 8>,
    pub(crate) total: Aggregates,
    pub(crate) token: ArrayVec<Uuid, 4>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub(crate) struct PilotRoundInfo {
    pub(crate) points: i32,
    pub(crate) side: (DateTime<Utc>, Side),
    pub(crate) slot: Option<Slot>,
    pub(crate) lives: ArrayVec<(LifeType, DateTime<Utc>, u8), 5>,
    pub(crate) connected: Option<(DateTime<Utc>, String)>,
}

impl Default for PilotRoundInfo {
    fn default() -> Self {
        Self {
            points: 0,
            side: (Utc::now(), Side::Neutral),
            slot: None,
            lives: ArrayVec::new(),
            connected: None,
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub(crate) struct Sortie {
    pub(crate) vehicle: Vehicle,
    pub(crate) takeoff: DateTime<Utc>,
    pub(crate) land: Option<DateTime<Utc>>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub(crate) struct Slot {
    pub(crate) id: SlotId,
    pub(crate) time: DateTime<Utc>,
    pub(crate) vehicle: Option<Vehicle>,
    pub(crate) sortie: Option<SortieId>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub(crate) struct Unit {
    pub(crate) group: Option<GroupId>,
    pub(crate) owner: Side,
    pub(crate) typ: Vehicle,
    pub(crate) tags: UnitTags,
    pub(crate) pos: Pos,
    pub(crate) dead: bool,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub(crate) struct Objective {
    pub(crate) name: String,
    pub(crate) pos: LLPos,
    pub(crate) kind: ObjectiveKind,
    pub(crate) by: Option<Ucid>,
    pub(crate) owner: Side,
    pub(crate) last_change: DateTime<Utc>,
    pub(crate) health: u8,
    pub(crate) logi: u8,
    pub(crate) supply: u8,
    pub(crate) fuel: u8,
}

#[derive(Clone)]
struct Pilots {
    pilots: Tree<Ucid, Pilot>,
    aggregates: Tree<(Ucid, Vehicle, RoundId), Aggregates>,
    by_name: Tree<String, ArrayVec<Ucid, 8>>,
    by_token: Tree<Uuid, Ucid>,
    sortie: Tree<(Ucid, RoundId, SortieId), Sortie>,
    round_info: Tree<(Ucid, RoundId), PilotRoundInfo>,
}

impl Pilots {
    fn new(db: &Db) -> Result<Self> {
        Ok(Self {
            pilots: Tree::open(db, "pilots")?,
            aggregates: Tree::open(db, "aggregates")?,
            by_name: Tree::open(db, "by_name")?,
            by_token: Tree::open(db, "by_token")?,
            sortie: Tree::open(db, "sortie")?,
            round_info: Tree::open(db, "pilot_round_info")?,
        })
    }

    fn with_pilot<F: FnMut(&mut Pilot)>(&self, k: Ucid, mut f: F) -> Result<()> {
        self.pilots
            .fetch_and_update(&k, |o| match o {
                None => None,
                Some(mut p) => {
                    f(&mut p);
                    Some(p)
                }
            })?
            .ok_or_else(|| anyhow!("pilot {k:?} is missing"))?;
        Ok(())
    }

    fn with_aggregates<F: FnMut(&mut Aggregates)>(
        &self,
        k: (Ucid, Vehicle, RoundId),
        mut f: F,
    ) -> Result<()> {
        self.aggregates
            .fetch_and_update(&k, |a| {
                let mut a = a.unwrap_or_default();
                f(&mut a);
                Some(a)
            })?;
        Ok(())
    }

    /// Credit a pilot for something that happened in `round`.
    ///
    /// `f` updates their **lifetime** total, `g` the per-round, per-vehicle
    /// aggregate. `lifetime` is false for a round on a non-public (test)
    /// instance: the per-round row is still written -- so an admin looking at
    /// that server's round still sees real numbers -- but the lifetime total
    /// is left alone, which is what keeps the public leaderboard and pilot
    /// profiles free of test-server activity.
    ///
    /// Note the asymmetry this preserves: `Pilot.total` is written on every
    /// call, but the aggregate row only when the pilot is in a slot with a
    /// known vehicle. That is why the public total cannot be re-derived by
    /// summing `aggregates` -- it would silently under-count. `Pilot.total`
    /// IS the public total, by construction.
    fn with_pilot_and_aggregates<F, G>(
        &self,
        ucid: Ucid,
        round: RoundId,
        lifetime: bool,
        f: F,
        g: G,
    ) -> Result<()>
    where
        F: FnMut(&mut Pilot),
        G: FnMut(&mut Aggregates),
    {
        let vehicle = self
            .round_info
            .get(&(ucid, round))?
            .and_then(|ri| ri.slot.and_then(|s| s.vehicle));
        if lifetime {
            self.with_pilot(ucid, f)?;
        }
        if let Some(vehicle) = vehicle {
            self.with_aggregates((ucid, vehicle, round), g)?
        }
        Ok(())
    }

    fn with_pilot_round_info<F>(&self, ucid: Ucid, round: RoundId, mut f: F) -> Result<()>
    where
        F: FnMut(&mut PilotRoundInfo),
    {
        self.round_info.fetch_and_update(&(ucid, round), |ri| {
            let mut ri = ri.unwrap_or_default();
            f(&mut ri);
            Some(ri)
        })?;
        Ok(())
    }

    fn with_sortie<F>(&self, k: (Ucid, RoundId, SortieId), mut f: F) -> Result<()>
    where
        F: FnMut(&mut Sortie),
    {
        self.sortie
            .fetch_and_update(&k, |s| match s {
                None => None,
                Some(mut s) => {
                    f(&mut s);
                    Some(s)
                }
            })?
            .ok_or_else(|| anyhow!("sortie {k:?} is missing"))?;
        Ok(())
    }

    fn saw_pilot(&self, id: Ucid, name: String) -> Result<()> {
        self.pilots.fetch_and_update(&id, |pilot| match pilot {
            None => Some(Pilot {
                name: ArrayVec::from_iter([name.clone()]),
                total: Aggregates::default(),
                token: ArrayVec::new(),
            }),
            Some(mut pilot) => match pilot.name.iter().enumerate().find(|(_, n)| name == **n) {
                Some((i, _)) => {
                    let last = pilot.name.len() - 1;
                    pilot.name.swap(i, last);
                    Some(pilot)
                }
                None => {
                    if pilot.name.is_full() {
                        let _ = pilot.name.pop_at(0);
                    }
                    pilot.name.push(name.clone());
                    Some(pilot)
                }
            },
        })?;
        self.by_name.update_and_fetch(&name, |ids| match ids {
            None => Some(ArrayVec::from_iter([id])),
            Some(mut ids) if !ids.contains(&id) => {
                if ids.is_full() {
                    ids.pop_at(0);
                }
                ids.push(id);
                Some(ids)
            }
            Some(ids) => Some(ids),
        })?;
        Ok(())
    }
}

#[derive(Debug, Clone, Copy, Serialize, Deserialize)]
pub(crate) struct Round {
    pub(crate) start: DateTime<Utc>,
    pub(crate) end: Option<DateTime<Utc>>,
    pub(crate) winner: Option<Side>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub(crate) struct SessionEnd {
    pub(crate) time: DateTime<Utc>,
    pub(crate) frame: HistogramSer,
    pub(crate) api: ApiPerfInner,
    pub(crate) engine: PerfInner,
}

/// One unit type out of a harvested snapshot. Mirrors `bflib::unitdb::UnitInfo`
/// over JSON deliberately: bfdb must still be able to read old snapshots after
/// the engine's struct grows a field, so unknown keys are ignored and every
/// known one is optional.
#[derive(Debug, Clone, Default, Serialize, Deserialize, PartialEq)]
pub(crate) struct UnitDbEntry {
    #[serde(default)]
    pub(crate) display_name: Option<std::string::String>,
    #[serde(default)]
    pub(crate) category: Option<std::string::String>,
    #[serde(default)]
    pub(crate) attributes: Vec<std::string::String>,
    #[serde(default)]
    pub(crate) threat_range_m: Option<f64>,
    #[serde(default)]
    pub(crate) threat_range_min_m: Option<f64>,
    #[serde(default)]
    pub(crate) detection_range_m: Option<f64>,
    #[serde(default)]
    pub(crate) max_target_detection_range_m: Option<f64>,
}

/// A unit range database as harvested from one DCS install.
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub(crate) struct UnitDbSnapshot {
    #[serde(default)]
    pub(crate) dcs_version: Option<std::string::String>,
    #[serde(default)]
    pub(crate) harvested_at: Option<std::string::String>,
    #[serde(default)]
    pub(crate) by_type: HashMap<std::string::String, UnitDbEntry>,
}

/// What happened to one unit type between two snapshots.
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(tag = "kind", rename_all = "snake_case")]
pub(crate) enum UnitDbChange {
    Added {
        typ: std::string::String,
        display_name: Option<std::string::String>,
        threat_range_m: Option<f64>,
        detection_range_m: Option<f64>,
    },
    Removed {
        typ: std::string::String,
        display_name: Option<std::string::String>,
    },
    /// A range moved. This is the one that matters: it means a unit the
    /// campaign is balanced around now shoots or sees further than it did.
    RangeChanged {
        typ: std::string::String,
        display_name: Option<std::string::String>,
        field: &'static str,
        from: Option<f64>,
        to: Option<f64>,
    },
}

/// A `cfg.artillery.units` entry that disagrees with the harvested DCS values.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub(crate) struct StaleOverride {
    pub(crate) typ: std::string::String,
    pub(crate) cfg_max_range_m: f64,
    pub(crate) cfg_min_range_m: f64,
    pub(crate) dcs_max_range_m: Option<f64>,
    pub(crate) dcs_min_range_m: Option<f64>,
}

/// Compare two snapshots. Only range fields are compared -- display names and
/// attributes churn across DCS patches for reasons nobody needs paging about.
pub(crate) fn diff_unit_db(old: &UnitDbSnapshot, new: &UnitDbSnapshot) -> Vec<UnitDbChange> {
    let mut out = vec![];
    for (typ, n) in &new.by_type {
        match old.by_type.get(typ) {
            None => out.push(UnitDbChange::Added {
                typ: typ.clone(),
                display_name: n.display_name.clone(),
                threat_range_m: n.threat_range_m,
                detection_range_m: n.detection_range_m,
            }),
            Some(o) => {
                let fields: [(&'static str, Option<f64>, Option<f64>); 3] = [
                    ("threat_range_m", o.threat_range_m, n.threat_range_m),
                    ("threat_range_min_m", o.threat_range_min_m, n.threat_range_min_m),
                    ("detection_range_m", o.detection_range_m, n.detection_range_m),
                ];
                for (field, from, to) in fields {
                    if from != to {
                        out.push(UnitDbChange::RangeChanged {
                            typ: typ.clone(),
                            display_name: n.display_name.clone(),
                            field,
                            from,
                            to,
                        });
                    }
                }
            }
        }
    }
    for (typ, o) in &old.by_type {
        if !new.by_type.contains_key(typ) {
            out.push(UnitDbChange::Removed {
                typ: typ.clone(),
                display_name: o.display_name.clone(),
            });
        }
    }
    out.sort_by_key(|c| match c {
        UnitDbChange::Added { typ, .. }
        | UnitDbChange::Removed { typ, .. }
        | UnitDbChange::RangeChanged { typ, .. } => typ.clone(),
    });
    out
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub(crate) struct Session {
    pub(crate) stop_time: Option<DateTime<Utc>>,
    pub(crate) end: Option<SessionEnd>,
    pub(crate) cfg: Cfg,
}

pub(crate) type Scenario = String;

#[derive(Debug, Clone, Serialize, Deserialize)]
pub(crate) enum GroupKind {
    Deployed { name: String, by: Ucid },
    Troop { name: String, by: Ucid },
    Action { name: String, by: Ucid },
    Objective,
}

impl Default for GroupKind {
    fn default() -> Self {
        GroupKind::Objective
    }
}

#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub(crate) struct Group {
    pub(crate) owner: Side,
    pub(crate) units: SmallVec<[EnId; 16]>,
    pub(crate) kind: GroupKind,
}

#[derive(Debug, Clone)]
struct StatCtxInner {
    /// Whether this round's activity counts towards lifetime pilot totals --
    /// false for a round on an instance with `public: false`. Resolved once
    /// when the round context is established rather than per stat.
    public: bool,
    sortie: Scenario,
    round: RoundId,
    seq: DateTime<Utc>
}

#[derive(Debug, Clone, Default)]
struct StatCtx(Option<StatCtxInner>);

impl StatCtx {
    #[allow(dead_code)]
    fn get(&self) -> Result<&StatCtxInner> {
        match &self.0 {
            Some(t) => Ok(t),
            None => bail!("expected to see NewSession before stats"),
        }
    }

    fn get_mut(&mut self) -> Result<&mut StatCtxInner> {
        match &mut self.0 {
            Some(t) => Ok(t),
            None => bail!("expected to see NewSession before stats"),
        }
    }
}

/// Everything that is per-DCS-server rather than per-database.
///
/// One of these exists for each entry in the `--instances` file (or exactly one,
/// id [`DEFAULT_INSTANCE`], for a legacy single-server bfdb). It owns that
/// server's netidx subscription, its own copy of the ingestion loops' live
/// state, and its own engine-log ring buffers -- so two DCS servers fronted by
/// the same bfdb never share a sortie, a log stream or a weather snapshot.
///
/// Anything *persisted* stays on [`StatsDbInner`]: rounds are tagged with an
/// [`InstanceId`] in the `round_instance` tree rather than being stored in
/// separate per-instance trees, which keeps a pre-multi-instance DB readable
/// (an untagged round belongs to the default instance).
pub(crate) struct InstanceState {
    /// Static config this instance was started with.
    pub(crate) cfg: Arc<InstanceCfg>,
    /// `cfg.id`, pre-shared for cheap cloning into keys and log lines.
    pub(crate) id: InstanceId,
    #[allow(dead_code)]
    subscriber: Option<Subscriber>,
    /// `cfg.base` -- the netidx base this instance's bflib publishes under.
    base: Option<NetidxPath>,
    /// Per-instance `sortie` override: when set, the LIVE engine subscriptions
    /// (RPC + log) use this instead of the sortie learned from the stats stream.
    sortie_override: Option<Scenario>,
    stats_dir: Option<PathBuf>,
    stats_jsonl: Option<PathBuf>,
    /// The sortie name of this instance's currently/most-recently active round,
    /// learned from Stat::NewRound. bflib publishes its engine log and RPC procs
    /// under `<netidx_base>/<sortie>/...` (see bflib/src/bg/mod.rs), so this
    /// must be appended to `base` before subscribing -- a bare `base` path
    /// will never resolve.
    current_sortie: StdMutex<Option<Scenario>>,
    latest_weather: RwLock<Option<WeatherSnapshot>>,
    /// Live bflib engine log for this instance, streamed over netidx from its
    /// running DCS mission (distinct from bfdb's own process log).
    engine_log_tx: broadcast::Sender<std::string::String>,
    engine_log_history: StdMutex<VecDeque<std::string::String>>,
    /// Subset of engine_log_history matching an ERROR/WARN level tag -- kept
    /// separately so the admin dashboard can show a short, high-signal error
    /// feed without the client having to filter the full (much larger,
    /// frequently-scrolling) log history itself.
    engine_error_history: StdMutex<VecDeque<std::string::String>>,
    /// Set by `POST /api/admin/rebuild-stats`. This instance's JSONL reader
    /// loop notices it on its next tick, wipes the derived trees and re-ingests
    /// its `stats.jsonl` from the top.
    jsonl_reset: AtomicBool,
}

impl InstanceState {
    fn new(cfg: Arc<InstanceCfg>, subscriber: Option<Subscriber>) -> Self {
        let id: InstanceId = Arc::from(cfg.id.as_str());
        if let Some(s) = &cfg.sortie {
            log::info!("[{id}] live engine subscriptions pinned to sortie {s:?}");
        }
        Self {
            id,
            subscriber,
            base: cfg.base.clone(),
            sortie_override: cfg.sortie.as_deref().map(Scenario::from),
            stats_dir: cfg.stats_dir.clone(),
            stats_jsonl: cfg.stats_jsonl.clone(),
            current_sortie: StdMutex::new(None),
            latest_weather: RwLock::new(None),
            engine_log_tx: broadcast::channel(1024).0,
            engine_log_history: StdMutex::new(VecDeque::new()),
            engine_error_history: StdMutex::new(VecDeque::new()),
            jsonl_reset: AtomicBool::new(false),
            cfg,
        }
    }

    /// The live sortie as a plain String, for JSON responses.
    pub(crate) fn live_sortie_public(&self) -> Option<std::string::String> {
        self.live_sortie().map(|s| s.to_string())
    }

    /// The sortie to point live subscriptions at, or `None` while the mission
    /// hasn't reported in yet.
    fn live_sortie(&self) -> Option<Scenario> {
        self.sortie_override
            .clone()
            .or_else(|| self.current_sortie.lock().unwrap().clone())
    }
}

#[derive(Clone)]
pub(crate) struct StatsDbInner {
    /// Every DCS server instance this bfdb fronts, in configured order.
    instances: Registry,
    /// Runtime state per instance, keyed by `InstanceCfg::id`.
    states: Arc<HashMap<InstanceId, Arc<InstanceState>>>,
    #[allow(dead_code)]
    include: Option<Regex>,
    #[allow(dead_code)]
    exclude: Option<Regex>,
    db: Db,
    pilots: Pilots,
    seq: Tree<(Scenario, RoundId), DateTime<Utc>>,
    round: Tree<(Scenario, RoundId), Round>,
    session: Tree<(RoundId, DateTime<Utc>), Session>,
    kills: Tree<(EnId, RoundId, KillId), Dead>,
    shared_kills: Tree<KillId, SmallVec<[EnId; 2]>>,
    /// Content key for a kill -- (round, victim, death time in millis). Guards
    /// `record_kill` against the same `Stat::Kill` being applied twice (archive
    /// re-read after a restart before the round context is re-primed, a
    /// publisher retry, etc.), which otherwise mints a second KillId and shows
    /// the kill twice in the feed / a pilot's list.
    kill_seen: Tree<(RoundId, EnId, i64), KillId>,
    /// Idempotency key for a sortie -- (round, pilot, takeoff time in millis).
    /// Same rationale as `kill_seen`: a redelivered `Stat::Takeoff` otherwise
    /// mints a second SortieId and shows a phantom extra sortie (and, once its
    /// matching `Stat::Land` is likewise replayed, double-credits flight hours).
    sortie_seen: Tree<(RoundId, Ucid, i64), SortieId>,
    /// Idempotency key for a deploy -- (round, deployed group). A redelivered
    /// `Stat::DeployGroup` otherwise mints a second DeployId, doubling the row
    /// in the pilot deploy log and the `deploys` counter.
    deploy_seen: Tree<(RoundId, GroupId), DeployId>,
    units: Tree<(RoundId, EnId), Unit>,
    groups: Tree<(RoundId, GroupId), Group>,
    detected: Tree<(RoundId, EnId), BitFlags<DetectionSource, u8>>,
    objectives: Tree<(RoundId, ObjectiveId), Objective>,
    equipment: Tree<(RoundId, ObjectiveId, String), u32>,
    liquids: Tree<(RoundId, ObjectiveId, LiquidType), u32>,
    /// Which instance a round belongs to. Written by `new_round`; a round with
    /// no entry predates multi-instance support and is treated as the default
    /// instance's (see `round_instance_of`). This is how every round-scoped
    /// query -- kills, sorties, objectives, captures -- gets filtered per DCS
    /// server without changing any existing bincode record layout.
    round_instance: Tree<RoundId, std::string::String>,
    /// Byte offset of the last line consumed from each instance's
    /// `stats_jsonl`, persisted so a bfdb restart resumes there instead of
    /// re-reading the whole file from the top and re-applying every stat (which
    /// inflated every counter that has no idempotency guard -- captures,
    /// points, repairs, troops, ...). Keyed by instance id. Rewound by
    /// `rebuild_stats_from_archive`. Supersedes the pre-multi-instance
    /// `jsonl_cursor` tree, whose single `0u8` entry is migrated into this one
    /// under the default instance at startup.
    jsonl_cursor: Tree<std::string::String, u64>,
    /// The old single-server JSONL cursor, kept only long enough to migrate it.
    legacy_jsonl_cursor: Tree<u8, u64>,
    // Auth
    auth_sessions:    Tree<Uuid, SessionData>,
    auth_states:      Tree<Uuid, OAuthState>,
    // Trail history
    trail_points: Tree<(RoundId, std::string::String, i64), (f64, f64, f64, f64)>,
    // Capture counts per objective per round
    objective_captures: Tree<(RoundId, ObjectiveId), u32>,
    // Capture events (who, what, when) per round -- see CaptureRecord
    captures: Tree<(RoundId, CaptureId), CaptureRecord>,
    // Deploy events, keyed pilot-first (unlike captures) for efficient
    // per-pilot scans -- see DeployRecord and pilot_deploys_for.
    deploys: Tree<(Ucid, RoundId, DeployId), DeployRecord>,
    // Aircraft sortie counts per round: (RoundId, vehicle_type) -> (sortie_count, total_hours_f32)
    aircraft_sorties: Tree<(RoundId, std::string::String), (u32, f32)>,
    // Admin-managed ban list (bfdb-native, separate from bflib's cfg.banned)
    admin_bans: Tree<Ucid, BanRecord>,
    // bfwiki content, keyed by page slug (e.g. "gameplay/objectives")
    wiki_pages: Tree<std::string::String, WikiPage>,
    // bfwiki uploaded images (screenshots etc.), keyed by generated Uuid
    wiki_images: Tree<Uuid, WikiImage>,
    // Recon intel (TARPS) captures, keyed (RoundId, capture Uuid). Per-round,
    // per-coalition; wiped by reset_campaign_data.
    intel_captures: Tree<(RoundId, Uuid), IntelCapture>,
    // Recon intel photo index, keyed by IntelCapture.image_id. The bytes are
    // inline unless `intel_dir` is set, in which case they're files on disk.
    intel_images: Tree<Uuid, IntelImage>,
    // Coalition-shared map markup on the recon picture, keyed (RoundId, Uuid).
    intel_markup: Tree<(RoundId, Uuid), IntelMarkup>,
    // When set (--intel-dir), recon photos are stored as files here rather
    // than as blobs in the DB. Set once at startup via set_intel_dir.
    intel_dir: Arc<RwLock<Option<PathBuf>>>,
    // Timestamp of the last stats-archive batch fully processed by each
    // instance's background_loop, persisted so a restart resumes from there
    // instead of replaying the entire historical archive from the beginning
    // every time (see background_loop -- a corrupted/duplicate-spammed archive
    // segment otherwise gets re-read in full on every single bfdb startup).
    // Keyed by instance id; supersedes the single-entry `legacy_replay_cursor`,
    // which is migrated into it at startup.
    replay_cursor: Tree<std::string::String, DateTime<Utc>>,
    /// The old single-server archive cursor, kept only long enough to migrate it.
    legacy_replay_cursor: Tree<u8, DateTime<Utc>>,
    // Unit range databases harvested from the running DCS install, keyed
    // (instance id, DCS version). Stored as the raw JSON the engine sent
    // rather than a bincode struct: these are snapshots we diff across DCS
    // updates and want to still be readable if the struct changes shape.
    unit_db: Tree<(std::string::String, std::string::String), std::string::String>,
    // Most recently harvested DCS version per instance. Not derivable from
    // `unit_db`'s key order -- "2.9.3.100" sorts after "2.9.29.27468".
    unit_db_latest: Tree<std::string::String, std::string::String>,
}

// Kept deliberately large: the dashboard's Engine Log viewer only shows a
// tail, but `GET /api/logs/engine` (the token-gated plain-text endpoint used
// for remote debugging) needs enough backlog to cover a full contested-base
// fight or a slow leak -- ~20k lines is a few MB of Strings.
const ENGINE_LOG_HISTORY_CAP: usize = 20_000;
const ENGINE_ERROR_HISTORY_CAP: usize = 4_000;

/// Matches the `[ERROR]`/`[WARN]`/`[WARNING]` level tags bflib's engine log
/// lines carry -- mirrors ENGINE_LOG_LEVEL_RE in the fowlengine Discord plugin
/// so the dashboard's error feed and the Discord alert relay agree on what
/// counts as noteworthy.
fn is_engine_error_line(line: &str) -> bool {
    let upper = line.to_ascii_uppercase();
    upper.contains("[ERROR]") || upper.contains("[WARN]") || upper.contains("[WARNING]")
}

pub(crate) struct StatsDb(Arc<StatsDbInner>);

impl Clone for StatsDb {
    fn clone(&self) -> Self {
        Self(Arc::clone(&self.0))
    }
}

/// Copy a file that may be locked by another process (e.g., DCS holding an exclusive lock).
/// On Windows, uses CreateFileW with FILE_SHARE_READ | FILE_SHARE_WRITE | FILE_SHARE_DELETE.
fn copy_locked_file(src: &Path, dst: &Path) -> std::io::Result<()> {
    #[cfg(windows)]
    {
        use std::os::windows::io::FromRawHandle;
        use std::os::windows::ffi::OsStrExt;
        extern "system" {
            fn CreateFileW(
                lpFileName: *const u16,
                dwDesiredAccess: u32,
                dwShareMode: u32,
                lpSecurityAttributes: *mut u8,
                dwCreationDisposition: u32,
                dwFlagsAndAttributes: u32,
                hTemplateFile: *mut u8,
            ) -> isize;
        }
        const GENERIC_READ: u32 = 0x80000000;
        const FILE_SHARE_READ: u32 = 1;
        const FILE_SHARE_WRITE: u32 = 2;
        const FILE_SHARE_DELETE: u32 = 4;
        const OPEN_EXISTING: u32 = 3;
        const INVALID_HANDLE_VALUE: isize = -1;

        let wide_path: Vec<u16> = src.as_os_str().encode_wide().chain(std::iter::once(0)).collect();
        let handle = unsafe {
            CreateFileW(
                wide_path.as_ptr(),
                GENERIC_READ,
                FILE_SHARE_READ | FILE_SHARE_WRITE | FILE_SHARE_DELETE,
                std::ptr::null_mut(),
                OPEN_EXISTING,
                0,
                std::ptr::null_mut(),
            )
        };
        if handle == INVALID_HANDLE_VALUE {
            return Err(std::io::Error::last_os_error());
        }
        let mut src_file = unsafe { std::fs::File::from_raw_handle(handle as *mut std::ffi::c_void) };
        let mut buf = Vec::new();
        src_file.read_to_end(&mut buf)?;
        let mut dst_file = std::fs::File::create(dst)?;
        dst_file.write_all(&buf)?;
        Ok(())
    }
    #[cfg(not(windows))]
    {
        std::fs::copy(src, dst)?;
        Ok(())
    }
}

fn stat_variant_name(s: &Stat) -> &'static str {
    match s {
        Stat::NewRound { .. } => "NewRound",
        Stat::RoundEnd { .. } => "RoundEnd",
        Stat::SessionStart { .. } => "SessionStart",
        Stat::SessionEnd { .. } => "SessionEnd",
        Stat::Objective { .. } => "Objective",
        Stat::ObjectiveDestroyed { .. } => "ObjectiveDestroyed",
        Stat::ObjectiveHealth { .. } => "ObjectiveHealth",
        Stat::ObjectiveSupply { .. } => "ObjectiveSupply",
        Stat::Capture { .. } => "Capture",
        Stat::Repair { .. } => "Repair",
        Stat::SupplyTransfer { .. } => "SupplyTransfer",
        Stat::Kill(_) => "Kill",
        Stat::Unit { .. } => "Unit",
        Stat::Position { .. } => "Position",
        Stat::Detected { .. } => "Detected",
        Stat::EquipmentInventory { .. } => "EquipmentInventory",
        Stat::LiquidInventory { .. } => "LiquidInventory",
        Stat::Action { .. } => "Action",
        Stat::DeployTroop { .. } => "DeployTroop",
        Stat::DeployGroup { .. } => "DeployGroup",
        Stat::DeployFarp { .. } => "DeployFarp",
        Stat::Register { .. } => "Register",
        Stat::Sideswitch { .. } => "Sideswitch",
        Stat::Connect { .. } => "Connect",
        Stat::Disconnect { .. } => "Disconnect",
        Stat::Slot { .. } => "Slot",
        Stat::Deslot { .. } => "Deslot",
        Stat::GroupDeleted { .. } => "GroupDeleted",
        Stat::Takeoff { .. } => "Takeoff",
        Stat::Land { .. } => "Land",
        Stat::Life { .. } => "Life",
        Stat::Points { .. } => "Points",
        Stat::PointsTransfer { .. } => "PointsTransfer",
        Stat::PointsTransferToObjective { .. } => "PointsTransferToObjective",
        Stat::Bind { .. } => "Bind",
        Stat::ConvoyDestroyed { .. } => "ConvoyDestroyed",
        Stat::CampaignEvent { .. } => "CampaignEvent",
        Stat::PilotXp { .. } => "PilotXp",
        Stat::AirRouteDelivered { .. } => "AirRouteDelivered",
        Stat::AirRouteDestroyed { .. } => "AirRouteDestroyed",
        Stat::SeaRouteDelivered { .. } => "SeaRouteDelivered",
        Stat::SeaRouteDestroyed { .. } => "SeaRouteDestroyed",
        Stat::Weather { .. } => "Weather",
        Stat::GciPicture(_) => "GciPicture",
    }
}

impl Deref for StatsDb {
    type Target = StatsDbInner;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}


#[allow(dead_code)]
fn txn_err(e: TransactionError<anyhow::Error>) -> anyhow::Error {
    match e {
        TransactionError::Abort(e) => e,
        TransactionError::Storage(e) => e.into(),
    }
}

impl StatsDb {
    /// Open the database and start one ingestion pipeline per configured
    /// instance.
    ///
    /// `subscriber` is shared by every instance -- a netidx Subscriber
    /// multiplexes any number of paths, and the instances are kept apart by
    /// their distinct `base` paths, not by separate connections. It is `None`
    /// in offline mode (no instance has a `base`).
    pub(crate) fn new<P: AsRef<Path>>(
        subscriber: Option<Subscriber>,
        db: P,
        instances: Registry,
        include: Option<Regex>,
        exclude: Option<Regex>,
    ) -> Result<Self> {
        let db = sled::open(db.as_ref())?;
        let states: HashMap<InstanceId, Arc<InstanceState>> = instances
            .all()
            .iter()
            .map(|cfg| {
                let st = Arc::new(InstanceState::new(
                    cfg.clone(),
                    // Only hand the subscriber to instances that actually have
                    // a live engine to talk to.
                    cfg.base.as_ref().and(subscriber.clone()),
                ));
                (st.id.clone(), st)
            })
            .collect();
        for cfg in instances.all() {
            log::info!(
                "instance {:?} ({}): base={} sortie={} jsonl={} archive={} export_port={}",
                cfg.id,
                cfg.label(),
                cfg.base.as_ref().map(|b| format!("{b}")).unwrap_or_else(|| "-".into()),
                cfg.sortie.clone().unwrap_or_else(|| "auto".into()),
                cfg.stats_jsonl.as_ref().map(|p| p.display().to_string()).unwrap_or_else(|| "-".into()),
                cfg.stats_dir.as_ref().map(|p| p.display().to_string()).unwrap_or_else(|| "-".into()),
                cfg.export_port.map(|p| p.to_string()).unwrap_or_else(|| "-".into()),
            );
        }
        let t = Self(Arc::new(StatsDbInner {
            instances: instances.clone(),
            states: Arc::new(states),
            include,
            exclude,
            db: db.clone(),
            pilots: Pilots::new(&db)?,
            seq: Tree::open(&db, "seq")?,
            round: Tree::open(&db, "round")?,
            session: Tree::open(&db, "session")?,
            kills: Tree::open(&db, "kills")?,
            shared_kills: Tree::open(&db, "shared_kills")?,
            kill_seen: Tree::open(&db, "kill_seen")?,
            sortie_seen: Tree::open(&db, "sortie_seen")?,
            deploy_seen: Tree::open(&db, "deploy_seen")?,
            units: Tree::open(&db, "units")?,
            groups: Tree::open(&db, "groups")?,
            detected: Tree::open(&db, "detected")?,
            objectives: Tree::open(&db, "objectives")?,
            equipment: Tree::open(&db, "equipment")?,
            liquids: Tree::open(&db, "liquids")?,
            round_instance: Tree::open(&db, "round_instance")?,
            jsonl_cursor: Tree::open(&db, "jsonl_cursor_v2")?,
            legacy_jsonl_cursor: Tree::open(&db, "jsonl_cursor")?,
            auth_sessions: Tree::open(&db, "auth_sessions")?,
            auth_states: Tree::open(&db, "auth_states")?,
            trail_points: Tree::open(&db, "trail_points")?,
            objective_captures: Tree::open(&db, "objective_captures")?,
            captures: Tree::open(&db, "captures")?,
            deploys: Tree::open(&db, "deploys")?,
            aircraft_sorties: Tree::open(&db, "aircraft_sorties")?,
            admin_bans: Tree::open(&db, "admin_bans")?,
            wiki_pages: Tree::open(&db, "wiki_pages")?,
            wiki_images: Tree::open(&db, "wiki_images")?,
            intel_captures: Tree::open(&db, "intel_captures")?,
            intel_images: Tree::open(&db, "intel_images")?,
            intel_markup: Tree::open(&db, "intel_markup")?,
            intel_dir: Arc::new(RwLock::new(None)),
            replay_cursor: Tree::open(&db, "replay_cursor_v2")?,
            legacy_replay_cursor: Tree::open(&db, "replay_cursor")?,
            unit_db: Tree::open(&db, "unit_db")?,
            unit_db_latest: Tree::open(&db, "unit_db_latest")?,
        }));
        t.migrate_legacy_cursors()?;
        t.seed_wiki_if_empty()?;
        t.seed_wiki_images_if_empty()?;
        t.close_stale_open_rounds()?;
        for st in t.0.states.values() {
            let _t = t.clone();
            let _st = st.clone();
            task::spawn(async move {
                if let Err(e) = _t.background_loop(_st.clone()).await {
                    error!("[{}] background task failed {e:?}", _st.id)
                }
            });
            if st.base.is_some() {
                let _t = t.clone();
                let _st = st.clone();
                task::spawn(async move {
                    if let Err(e) = _t.engine_log_loop(_st.clone()).await {
                        error!("[{}] engine log subscription failed {e:?}", _st.id)
                    }
                });
            }
        }
        if t.0.states.values().all(|s| s.base.is_none()) {
            info!("running in offline mode (no instance has a netidx base)");
        }
        Ok(t)
    }

    /// Move the pre-multi-instance single-entry cursors into the per-instance
    /// trees under the default instance id, once. Without this, an upgraded
    /// bfdb would see an empty cursor and re-ingest the entire stats history
    /// from the top -- exactly the round-fragmentation/duplicate-kill problem
    /// the cursors were added to fix.
    fn migrate_legacy_cursors(&self) -> Result<()> {
        let default = self.0.instances.default_id().to_string();
        if let Some(pos) = self.0.legacy_jsonl_cursor.get(&0u8)? {
            if self.0.jsonl_cursor.get(&default)?.is_none() {
                info!("migrating legacy JSONL cursor (offset {pos}) to instance {default:?}");
                self.0.jsonl_cursor.insert(&default, &pos)?;
            }
            self.0.legacy_jsonl_cursor.remove(&0u8)?;
        }
        if let Some(ts) = self.0.legacy_replay_cursor.get(&0u8)? {
            if self.0.replay_cursor.get(&default)?.is_none() {
                info!("migrating legacy archive replay cursor ({ts}) to instance {default:?}");
                self.0.replay_cursor.insert(&default, &ts)?;
            }
            self.0.legacy_replay_cursor.remove(&0u8)?;
        }
        Ok(())
    }

    /// Exactly one round is ever legitimately open at a time *per instance*.
    /// Older builds could leave extras open: a placeholder round fabricated
    /// from a SessionStart that replayed without its NewRound (it took the last
    /// segment of the netidx base, e.g. "campaign" from "/local/fowl/campaign",
    /// as a stand-in sortie name), or -- worse -- the *real* round, which the
    /// previous name-based cleanup here closed on every restart whenever the
    /// genuine sortie happened to be named the same as that last path segment
    /// (the documented default is netidx_base "/local/fowl/campaign" + sortie
    /// "campaign"). Each time it closed the live round, the next SessionStart
    /// forked a brand-new one, so a single long campaign fragmented into dozens
    /// of near-identical rounds in the dashboard.
    ///
    /// Name-blind fix: per instance, keep only the newest open round (by start
    /// time) and close any older opens so they stop shadowing round selection.
    /// The survivor is whatever is actually live, regardless of its name.
    fn close_stale_open_rounds(&self) -> Result<()> {
        let mut open_by_instance: HashMap<InstanceId, Vec<(Scenario, RoundId, Round)>> =
            HashMap::new();
        for r in self.round.iter() {
            let ((s, rid), rd) = r?;
            if rd.end.is_some() {
                continue;
            }
            let inst = self.round_instance_of(rid);
            open_by_instance.entry(inst).or_default().push((s, rid, rd));
        }
        for (inst, mut open) in open_by_instance {
            open.sort_by(|a, b| a.2.start.cmp(&b.2.start).then(a.1.cmp(&b.1)));
            let keep = open.pop();
            for (s, rid, mut rd) in open {
                warn!(
                    "[{inst}] closing stale open round {rid:?} (scenario {s:?}) -- only the newest open round is kept"
                );
                rd.end = Some(chrono::Utc::now());
                let _ = self.round.insert(&(s, rid), &rd)?;
            }
            // Prime current_sortie from the surviving open round. On restart
            // the archive replay resumes from replay_cursor and may never
            // re-witness the NewRound/SessionStart that started this round, so
            // without this the engine log/RPC subscriptions would wait forever
            // for a sortie that already exists.
            if let (Some((sortie, _, _)), Some(st)) = (keep, self.0.states.get(&inst)) {
                info!("[{inst}] resuming with active round sortie={sortie:?}");
                *st.current_sortie.lock().unwrap() = Some(sortie);
            }
        }
        Ok(())
    }

    // ── instance accessors ────────────────────────────────────────────────

    /// Every instance this bfdb fronts, in configured order.
    pub(crate) fn instances(&self) -> &Registry {
        &self.0.instances
    }

    /// Runtime state for one instance id, or the default instance's when the
    /// id is unknown to us (which can only happen for a round tagged with an
    /// instance that has since been removed from the config).
    pub(crate) fn state(&self, id: &InstanceId) -> Arc<InstanceState> {
        self.0
            .states
            .get(id)
            .or_else(|| self.0.states.get(self.0.instances.default_id()))
            .expect("registry always has its default instance")
            .clone()
    }

    /// Resolve a `?instance=` query value to that instance's runtime state.
    pub(crate) fn resolve_state(&self, requested: Option<&str>) -> Result<Arc<InstanceState>> {
        let cfg = self.0.instances.resolve(requested)?;
        let id: InstanceId = Arc::from(cfg.id.as_str());
        Ok(self.state(&id))
    }

    /// Which instance owns a round. Rounds written before multi-instance
    /// support carry no tag and belong to the default instance.
    pub(crate) fn round_instance_of(&self, round: RoundId) -> InstanceId {
        match self.round_instance.get(&round) {
            Ok(Some(id)) => Arc::from(id.as_str()),
            _ => self.0.instances.default_id().clone(),
        }
    }

    /// The rounds that count towards the public, all-time picture: those
    /// belonging to an instance with `public: true`.
    ///
    /// `None` means "every round" -- returned whenever no instance is marked
    /// private, so the usual single-purpose deployment never pays for the
    /// filtering (and keeps using the pre-aggregated pilot totals).
    pub(crate) fn public_rounds(&self) -> Result<Option<HashSet<RoundId>>> {
        if !self.0.instances.has_private() {
            return Ok(None);
        }
        let private: HashSet<InstanceId> = self
            .0
            .instances
            .all()
            .iter()
            .filter(|i| !i.public)
            .map(|i| InstanceId::from(i.id.as_str()))
            .collect();
        let mut out = HashSet::new();
        for r in self.round.iter() {
            let ((_, rid), _) = r?;
            if !private.contains(&self.round_instance_of(rid)) {
                out.insert(rid);
            }
        }
        Ok(Some(out))
    }

    /// Every round id belonging to `inst`. Used to filter the round-scoped
    /// stats trees, which are keyed by RoundId and carry no instance of their
    /// own. Cheap: the round tree holds one entry per campaign round, not per
    /// event.
    pub(crate) fn rounds_of(&self, inst: &InstanceId) -> Result<HashSet<RoundId>> {
        let mut out = HashSet::new();
        for r in self.round.iter() {
            let ((_, rid), _) = r?;
            if &self.round_instance_of(rid) == inst {
                out.insert(rid);
            }
        }
        Ok(out)
    }
    /// A live subscription to the running bflib engine's log stream,
    /// published over netidx at `<base>/<sortie>/log` by `bflib::bg::logpub`
    /// (bflib appends its mission sortie name to `netidx_base` before
    /// publishing anything -- see `Task::CfgLoaded` in bflib/src/bg/mod.rs).
    /// No-op if bfdb wasn't started with --base. Each update from the
    /// publisher carries the *entire* accumulated log content (not just the
    /// new line), so we track how much we've already seen and only forward
    /// the newly-appended lines. Waits for the sortie to become known via
    /// Stat::NewRound, and resubscribes if it changes (new mission/round).
    async fn engine_log_loop(self, inst: Arc<InstanceState>) -> Result<()> {
        use futures::{channel::mpsc, StreamExt};
        use netidx::subscriber::{Event, UpdatesFlags};
        use netidx::publisher::Value;

        let (subscriber, base) = match (&inst.subscriber, &inst.base) {
            (Some(s), Some(b)) => (s.clone(), b.clone()),
            _ => return Ok(()),
        };
        loop {
            let sortie = loop {
                if let Some(s) = inst.live_sortie() {
                    break s;
                }
                tokio::time::sleep(std::time::Duration::from_secs(1)).await;
            };
            let dval = subscriber.subscribe(base.append(&sortie).append("log"));
            let (tx, mut rx) = mpsc::channel(10);
            dval.updates(UpdatesFlags::empty(), tx);
            let mut seen_len = 0usize;
            while let Some(batch) = rx.next().await {
                // With a --sortie override the target never changes; otherwise
                // resubscribe when the live sortie moves (new mission/round).
                if inst.sortie_override.is_none()
                    && inst.current_sortie.lock().unwrap().as_ref() != Some(&sortie)
                {
                    break;
                }
                for (_id, ev) in batch.iter() {
                    let Event::Update(Value::String(chars)) = ev else { continue };
                    let full: &str = chars.as_ref();
                    // publisher truncated/restarted (new mission) — resend everything as new
                    let start = if full.len() >= seen_len { seen_len } else { 0 };
                    let new_part = &full[start..];
                    seen_len = full.len();
                    for line in new_part.lines().filter(|l| !l.is_empty()) {
                        let line = std::string::String::from(line);
                        let mut hist = inst.engine_log_history.lock().unwrap();
                        if hist.len() >= ENGINE_LOG_HISTORY_CAP {
                            hist.pop_front();
                        }
                        hist.push_back(line.clone());
                        drop(hist);
                        if is_engine_error_line(&line) {
                            let mut errs = inst.engine_error_history.lock().unwrap();
                            if errs.len() >= ENGINE_ERROR_HISTORY_CAP {
                                errs.pop_front();
                            }
                            errs.push_back(line.clone());
                        }
                        let _ = inst.engine_log_tx.send(line);
                    }
                }
            }
            if inst.sortie_override.is_some() {
                // Sortie is pinned: the subscription just ended (engine restart
                // / mission reload). Pause, then resubscribe on the next loop.
                tokio::time::sleep(std::time::Duration::from_secs(5)).await;
                continue;
            }
            if inst.current_sortie.lock().unwrap().as_ref() == Some(&sortie) {
                // subscription itself ended (not a sortie change) -- nothing left to do
                return Ok(());
            }
        }
    }

    /// Subscribe to the live engine log stream, plus a snapshot of recent
    /// history for a newly-connected client to catch up with.
    pub(crate) fn engine_log_subscribe(
        &self,
        inst: &InstanceState,
    ) -> (broadcast::Receiver<std::string::String>, Vec<std::string::String>) {
        let rx = inst.engine_log_tx.subscribe();
        let hist = inst.engine_log_history.lock().unwrap().iter().cloned().collect();
        (rx, hist)
    }

    /// Recent ERROR/WARN lines from the engine log, oldest first -- backs the
    /// admin dashboard's error feed (see api_admin_engine_errors in main.rs).
    pub(crate) fn engine_error_snapshot(&self, inst: &InstanceState) -> Vec<std::string::String> {
        inst.engine_error_history.lock().unwrap().iter().cloned().collect()
    }

    /// Full in-memory engine-log backlog, oldest first (cap
    /// `ENGINE_LOG_HISTORY_CAP`). Backs `GET /api/logs/engine`.
    pub(crate) fn engine_log_snapshot(&self, inst: &InstanceState) -> Vec<std::string::String> {
        inst.engine_log_history.lock().unwrap().iter().cloned().collect()
    }

    /// Call one of bflib's netidx RPC procs (published under
    /// `<base>/<sortie>/api/<name>`, see bflib/src/bg/rpcs.rs -- bflib
    /// appends its mission sortie name to `netidx_base` before publishing,
    /// same as the engine log) and return its raw reply. Errors if bfdb
    /// wasn't started with --base (netidx disabled) or if the mission isn't
    /// running / hasn't published a sortie yet.
    ///
    /// A successful RPC call still returns `Ok` even when the *engine* reported
    /// a logical error (bflib replies with `Value::Error` in that case, per its
    /// `reply_err!` macro) -- callers should check the returned Value's variant.
    pub(crate) async fn call_engine_rpc(
        &self,
        inst: &InstanceState,
        proc_name: &str,
        args: Vec<(&str, netidx::publisher::Value)>,
    ) -> Result<netidx::publisher::Value> {
        use netidx_protocols::rpc::client::Proc;
        let (subscriber, base) = match (&inst.subscriber, &inst.base) {
            (Some(s), Some(b)) => (s, b),
            _ => bail!("instance {:?} has no netidx base configured", inst.cfg.id),
        };
        let sortie = inst.live_sortie().ok_or_else(|| {
            anyhow!(
                "instance {:?}: no active sortie yet (mission has not reported in)",
                inst.cfg.id
            )
        })?;
        let path = base.append(&sortie).append("api").append(proc_name);
        let proc = Proc::new(subscriber, path)?;
        proc.call(args).await
    }

    async fn background_loop(self, inst: Arc<InstanceState>) -> Result<()> {
        // If stats_jsonl is configured, use the JSONL reader instead of archive
        if let Some(jsonl_path) = inst.stats_jsonl.clone() {
            return self.jsonl_loop(inst, jsonl_path).await;
        }

        use arcstr::ArcStr;
        use netidx::subscriber::Event;
        use netidx_archive::logfile::BatchItem;
        use tokio::time;

        let stats_dir = match &inst.stats_dir {
            Some(d) => d.clone(),
            None => return Ok(()), // no archive configured
        };
        let inst_key = inst.id.to_string();

        let shard: ArcStr = "0".into();
        let mut archive_cfg = ArchiveFileCfg::default();
        archive_cfg.archive_directory = stats_dir;
        archive_cfg.archive_cmds = None;
        let archive_cfg = Arc::new(netidx_archive::config::Config::try_from(archive_cfg)?);

        let head_path = archive_cfg.archive_directory().join(shard.as_str()).join("current");
        let head_copy_path = archive_cfg.archive_directory().join(shard.as_str()).join("current_copy");
        // Resume from wherever we last left off instead of always replaying
        // the entire historical archive from the beginning -- see
        // replay_cursor's doc comment on StatsDbInner.
        let resume_from = self.0.replay_cursor.get(&inst_key)?;
        if let Some(ts) = resume_from {
            info!("[{}] resuming stats archive replay after {ts}", inst.id);
        }

        let mut ctx = StatCtx::default();
        let mut timer = time::interval(Duration::from_secs(5));
        let mut total_batches = 0u64;
        let mut total_items = 0u64;
        let mut last_seen_ts: Option<DateTime<Utc>> = resume_from;

        loop {
            timer.tick().await;
            // ArchiveCollectionReader caches its head-file DataSource the
            // first time it's derived and never refreshes it from later
            // set_head() calls (see ArchiveCollectionReader::source /
            // apply_read in netidx-archive) -- so reusing one reader across
            // ticks means it silently stops seeing new data the moment it
            // first catches up to the head file's end, forever, even though
            // bflib keeps appending. Building a fresh reader every tick,
            // seeded from our own persisted/tracked position, sidesteps that
            // by forcing a correct re-derivation from the current head
            // snapshot each time.
            let new_index = task::block_in_place(|| ArchiveIndex::new(&archive_cfg, &shard)).ok();
            let new_head = task::block_in_place(|| {
                match copy_locked_file(&head_path, &head_copy_path) {
                    Ok(()) => netidx_archive::logfile::ArchiveReader::open(&head_copy_path).ok(),
                    Err(_) => netidx_archive::logfile::ArchiveReader::open(&head_path).ok(),
                }
            });
            let Some(new_index) = new_index else { continue };
            let start_bound = match last_seen_ts {
                Some(ts) => Bound::Excluded(ts),
                None => Bound::Unbounded,
            };
            let mut reader = ArchiveCollectionReader::new(
                new_index,
                archive_cfg.clone(),
                shard.clone(),
                new_head,
                start_bound,
                Bound::Unbounded,
            );
            // Cap batches drained per tick and yield back to the runtime in
            // between -- a large backlog (e.g. replaying a big historical
            // archive on startup) would otherwise monopolize this worker
            // thread inside back-to-back block_in_place calls and starve the
            // warp HTTP handlers (e.g. /api/objectives), which is what made
            // external pollers like the Discord bot's FowlEngine plugin see
            // request timeouts while bfdb was catching up.
            const MAX_BATCHES_PER_TICK: u32 = 2_000;
            let mut batches_this_tick = 0u32;
            loop {
                if batches_this_tick >= MAX_BATCHES_PER_TICK {
                    break;
                }
                batches_this_tick += 1;
                let batch = task::block_in_place(|| reader.read_next(None));
                match batch {
                    Err(e) => {
                        // "no data source available" just means the head
                        // file copy transiently failed to open this tick
                        // (e.g. raced a write) with no unread historical
                        // files to fall back to -- expected and self-heals
                        // next tick, not worth error-level noise.
                        if e.to_string().contains("no data source available") {
                            debug!("archive read: nothing available this tick ({e})");
                        } else {
                            error!("archive read error: {e:?}");
                        }
                        break;
                    }
                    Ok(None) => break, // caught up to end of available historical files
                    Ok(Some((ts, items))) => {
                        total_batches += 1;
                        total_items += items.len() as u64;
                        // Coarser cadence past the first 100k batches so a
                        // large backlog (e.g. a corrupted archive segment
                        // full of duplicate records) doesn't blow the log
                        // file up while it's replayed.
                        let log_every = if total_batches <= 100_000 { 100 } else { 50_000 };
                        if total_batches <= 5 || total_batches % log_every == 0 {
                            info!("batch #{total_batches} ts={ts} items={} (total_items={total_items})", items.len());
                        }
                        last_seen_ts = Some(ts);
                        // ArchiveCollectionReader::read_next does NOT advance
                        // its own cursor -- per its docs it reads "without
                        // changing the cursor position." Without this, every
                        // call re-reads the same batch forever and this loop
                        // never terminates (this was the actual cause of the
                        // runaway duplicate-record replay we hit -- there was
                        // never any corrupted/duplicated archive data, just
                        // one record being read over and over).
                        reader.position_mut().set_current(ts);
                        for BatchItem(path_id, ev) in items.iter() {
                            if let Event::Update(v) = ev {
                                let s = match v {
                                    netidx::publisher::Value::String(s) => s.clone(),
                                    other => {
                                        if total_batches <= 5 {
                                            info!("  non-string value type for path_id={path_id:?}: {other:?}");
                                        }
                                        continue;
                                    }
                                };
                                if total_batches <= 3 {
                                    let preview: std::string::String = s.chars().take(100).collect();
                                    info!("  raw[path_id={path_id:?}]: {preview}");
                                }
                                let st: Stat = match serde_json::from_str::<Stat>(&s) {
                                    Ok(s) => s,
                                    Err(e) => {
                                        let preview: std::string::String = s.chars().take(200).collect();
                                        error!("failed to deserialize stat: {e}, raw: {preview}");
                                        continue;
                                    }
                                };
                                if total_batches <= 10 || total_batches % 100 == 0 {
                                    info!("adding stat variant={}", stat_variant_name(&st));
                                }
                                if let Err(e) =
                                    task::block_in_place(|| self.add_stat(&inst, &mut ctx, ts, st))
                                {
                                    error!("[{}] failed to add stat {e:?}", inst.id)
                                }
                            }
                        }
                    }
                }
            }
            // Persist how far we've gotten so a restart resumes here instead
            // of replaying the whole archive from scratch.
            if let Some(ts) = last_seen_ts {
                if let Err(e) = self.0.replay_cursor.insert(&inst_key, &ts) {
                    error!("failed to save replay cursor: {e:?}");
                }
            }
        }
    }

    /// Read stats from a JSONL file (one JSON object per line)
    async fn jsonl_loop(self, inst: Arc<InstanceState>, jsonl_path: PathBuf) -> Result<()> {
        use std::io::BufRead;
        use tokio::time;

        let inst_key = inst.id.to_string();
        let mut ctx = StatCtx::default();
        let mut timer = time::interval(Duration::from_secs(5));
        let mut last_pos: u64 = self.0.jsonl_cursor.get(&inst_key)?.unwrap_or(0);

        // Resume ctx from whatever round is still open, so stats that land
        // before the next SessionStart (e.g. after a bfdb-only restart) still
        // get attributed instead of being dropped as "no NewSession before
        // stats".
        if last_pos > 0 {
            if let Ok(rounds) = self.latest_rounds_for(&inst.id) {
                if let Some((sortie, round, _)) =
                    rounds.into_iter().find(|(_, _, r)| r.end.is_none())
                {
                    if let Ok(Some(seq)) = self.seq.get(&(sortie.clone(), round)) {
                        ctx.0 = Some(StatCtxInner { public: inst.cfg.public, sortie, round, seq });
                    }
                }
            }
        }

        info!("[{}] starting JSONL reader from {jsonl_path:?} at offset {last_pos}", inst.id);

        loop {
            timer.tick().await;

            // Live rebuild requested (POST /api/admin/rebuild-stats): wipe the
            // derived trees and start reading the file over from the top. Done
            // here, in the loop thread, so it can't interleave with add_stat.
            if inst.jsonl_reset.swap(false, Ordering::SeqCst) {
                warn!(
                    "[{}] jsonl rebuild requested -- wiping derived stats and re-ingesting {jsonl_path:?} from offset 0",
                    inst.id
                );
                if let Err(e) = task::block_in_place(|| self.wipe_stats_derived_trees()) {
                    error!(
                        "[{}] jsonl rebuild: wipe failed ({e:?}) -- aborting rebuild, keeping current data",
                        inst.id
                    );
                } else {
                    let _ = self.0.jsonl_cursor.insert(&inst_key, &0u64);
                    *inst.current_sortie.lock().unwrap() = None;
                    last_pos = 0;
                    ctx = StatCtx::default();
                }
            }

            let read_result = task::block_in_place(|| -> Result<(u64, Vec<(DateTime<Utc>, Stat)>)> {
                let file = match std::fs::File::open(&jsonl_path) {
                    Ok(f) => f,
                    Err(e) => {
                        if e.kind() != std::io::ErrorKind::NotFound {
                            error!("failed to open JSONL file: {e:?}");
                        }
                        return Ok((last_pos, vec![]));
                    }
                };
                let metadata = file.metadata()?;
                let file_len = metadata.len();
                if file_len <= last_pos {
                    return Ok((last_pos, vec![]));
                }
                use std::io::Seek;
                let mut reader = std::io::BufReader::new(file);
                reader.seek(std::io::SeekFrom::Start(last_pos))?;
                let mut line = std::string::String::new();
                let mut new_pos = last_pos;
                let mut stats = Vec::new();
                while reader.read_line(&mut line)? > 0 {
                    new_pos = reader.stream_position()?;
                    let trimmed = line.trim();
                    if trimmed.is_empty() {
                        line.clear();
                        continue;
                    }
                    match serde_json::from_str::<serde_json::Value>(trimmed) {
                        Ok(val) => {
                            let ts_str = val.get("ts").and_then(|v| v.as_str()).unwrap_or("");
                            let ts = ts_str.parse::<DateTime<Utc>>().unwrap_or_else(|_| Utc::now());
                            if let Some(stat_val) = val.get("stat") {
                                match serde_json::from_value::<Stat>(stat_val.clone()) {
                                    Ok(st) => stats.push((ts, st)),
                                    Err(e) => {
                                        let preview: std::string::String = trimmed.chars().take(200).collect();
                                        error!("failed to deserialize stat from JSONL: {e}, raw: {preview}");
                                    }
                                }
                            }
                        }
                        Err(e) => error!("failed to parse JSONL line: {e}"),
                    }
                    line.clear();
                }
                Ok((new_pos, stats))
            });
            match read_result {
                Ok((pos, stats)) => {
                    if !stats.is_empty() {
                        let count = stats.len();
                        for (ts, st) in stats {
                            if let Err(e) =
                                task::block_in_place(|| self.add_stat(&inst, &mut ctx, ts, st))
                            {
                                warn!("[{}] failed to add stat from JSONL: {e:?}", inst.id);
                            }
                        }
                        info!(
                            "[{}] processed {count} stats from JSONL (pos {last_pos} -> {pos})",
                            inst.id
                        );
                    }
                    if pos != last_pos {
                        if let Err(e) = self.0.jsonl_cursor.insert(&inst_key, &pos) {
                            error!("failed to persist JSONL cursor: {e:?}");
                        }
                    }
                    last_pos = pos;
                }
                Err(e) => error!("JSONL read error: {e:?}"),
            }
        }
    }

    fn new_round(
        &self,
        inst: &InstanceState,
        ctx: &mut StatCtx,
        start: DateTime<Utc>,
        sortie: String,
        seqnum: DateTime<Utc>,
    ) -> Result<()> {
        let id = RoundId::new(&self.db)?;
        let key = (sortie.clone(), id);
        let r = Round {
            start,
            end: None,
            winner: None,
        };
        info!("[{}] new_round: inserting round id={id:?} sortie={sortie:?}", inst.id);
        self.seq.insert(&key, &seqnum)?;
        self.round.insert(&key, &r)?;
        // Tag the round with the DCS server it belongs to. Every round-scoped
        // query filters through this -- see `round_instance_of`.
        self.round_instance.insert(&id, &inst.id.to_string())?;
        info!("new_round: round inserted successfully");
        *inst.current_sortie.lock().unwrap() = Some(sortie.clone());
        ctx.0 = Some(StatCtxInner {
            public: inst.cfg.public,
            sortie,
            round: id,
            seq: seqnum,
        });
        Ok(())
    }

    fn round_end(
        &self,
        ctx: &mut StatCtx,
        time: DateTime<Utc>,
        winner: Option<Side>,
    ) -> Result<()> {
        let inner = ctx.get_mut()?;
        let key = (inner.sortie.clone(), inner.round);
        let mut round = self
            .round
            .get(&key)?
            .ok_or_else(|| anyhow!("round not found"))?;
        round.end = Some(time);
        round.winner = winner;
        let _ = self.round.insert(&key, &round)?;
        ctx.0 = None;
        Ok(())
    }

    fn with_objective<F: FnMut(&mut Objective)>(
        &self,
        k: (RoundId, ObjectiveId),
        mut f: F,
    ) -> Result<()> {
        self.objectives
            .fetch_and_update(&k, |o| match o {
                None => None,
                Some(mut o) => {
                    f(&mut o);
                    Some(o)
                }
            })?
            .ok_or_else(|| anyhow!("objective {k:?} is missing"))?;
        Ok(())
    }

    fn with_group<F: FnMut(&mut Group)>(&self, k: (RoundId, GroupId), mut f: F) -> Result<()> {
        self.groups
            .fetch_and_update(&k, |g| match g {
                None => None,
                Some(mut g) => {
                    f(&mut g);
                    Some(g)
                }
            })?
            .ok_or_else(|| anyhow!("group {k:?} is missing"))?;
        Ok(())
    }

    fn with_unit<F: FnMut(&mut Unit)>(&self, k: (RoundId, EnId), mut f: F) -> Result<()> {
        self.units
            .fetch_and_update(&k, |g| match g {
                None => None,
                Some(mut u) => {
                    f(&mut u);
                    Some(u)
                }
            })?
            .ok_or_else(|| anyhow!("unit {k:?} is missing"))?;
        Ok(())
    }

    fn with_shared_kills<F: FnMut(&mut SmallVec<[EnId; 2]>)>(
        &self,
        k: KillId,
        mut f: F,
    ) -> Result<()> {
        self.shared_kills.update_and_fetch(&k, |sk| {
            let mut sk = sk.unwrap_or_default();
            f(&mut sk);
            Some(sk)
        })?;
        Ok(())
    }

    fn record_kill(&self, ctx: &mut StatCtxInner, dead: Dead) -> Result<()> {
        // Idempotency guard: one real kill = one (round, victim, death-time)
        // triple. If we've already recorded this exact kill, a redelivery is in
        // play (archive re-read, publisher retry) -- bail before minting a
        // second KillId that would double it in every kill view.
        let victim_enid = match &dead.victim {
            Who::Player { ucid, .. } => EnId::Player(*ucid),
            Who::AI { uid, .. } => EnId::Unit(*uid),
        };
        let dedup_key = (ctx.round, victim_enid, dead.time.timestamp_millis());
        if self.kill_seen.get(&dedup_key)?.is_some() {
            return Ok(());
        }
        let kid = KillId::new(&self.db)?;
        self.kill_seen.insert(&dedup_key, &kid)?;
        let air = match &dead.victim {
            Who::Player { ucid, .. } => {
                self.pilots.with_pilot_and_aggregates(
                    *ucid,
                    ctx.round,
                    ctx.public,
                    |p| p.total.deaths += 1,
                    |a| a.deaths += 1,
                )?;
                true
            }
            Who::AI { uid, .. } => {
                let tags = self
                    .units
                    .get(&(ctx.round, EnId::Unit(*uid)))?
                    .map(|u| u.tags)
                    .unwrap_or_default();
                tags.contains(UnitTag::Aircraft) || tags.contains(UnitTag::Helicopter)
            }
        };
        let any_hit = dead.shots.iter().any(|s| s.hit);
        let up = |a: &mut Aggregates| {
            if air {
                a.air_kills += 1
            } else {
                a.ground_kills += 1
            }
        };
        // A single kill can carry many qualifying shots (e.g. every round in a
        // cannon burst, or several missiles that all register as hits) — credit
        // each shooter's air/ground kill count at most once per kill, not once
        // per shot, or one kill inflates the stat by the shot count.
        let mut credited: SmallVec<[EnId; 2]> = SmallVec::new();
        for shot in dead.shots.iter() {
            if any_hit && !shot.hit {
                continue;
            }
            let enid = match &shot.shooter {
                Who::AI {
                    ucid: None, uid, ..
                } => EnId::Unit(*uid),
                Who::Player { ucid, .. }
                | Who::AI {
                    ucid: Some(ucid), ..
                } => {
                    if !credited.contains(&EnId::Player(*ucid)) {
                        self.pilots.with_pilot_and_aggregates(
                            *ucid,
                            ctx.round,
                            ctx.public,
                            |p| up(&mut p.total),
                            |a| up(a),
                        )?;
                    }
                    EnId::Player(*ucid)
                }
            };
            if !credited.contains(&enid) {
                credited.push(enid);
            }
            self.kills.insert(&(enid, ctx.round, kid), &dead)?;
            self.with_shared_kills(kid, |sk| {
                if !sk.contains(&enid) {
                    sk.push(enid)
                }
            })?;
        }
        Ok(())
    }

    #[allow(dead_code)]
    pub(crate) fn pilots(&self) -> impl Iterator<Item = Result<(Ucid, String)>> {
        self.pilots.pilots.iter().map(|r| {
            let (ucid, pilot) = r?;
            let name = pilot
                .name
                .last()
                .map(|s| s.clone())
                .unwrap_or(String::default());
            Ok((ucid, name))
        })
    }

    /// Get all pilots with their aggregate stats, sorted by total kills descending.
    /// If `round` is Some, only stats from that round are included; otherwise all-time.
    pub(crate) fn pilot_leaderboard(&self, round: Option<RoundId>) -> Result<Vec<(Ucid, String, Aggregates)>> {
        match round {
            None => {
                // All-time: use pre-aggregated totals. These are already the
                // *public* totals -- a non-public (test) instance's rounds
                // never touch `Pilot.total` (see with_pilot_and_aggregates).
                let mut entries = Vec::new();
                for r in self.pilots.pilots.iter() {
                    let (ucid, pilot) = r?;
                    let name = pilot.name.last().map(|s| s.clone()).unwrap_or_default();
                    entries.push((ucid, name, pilot.total));
                }
                entries.sort_by(|a, b| {
                    (b.2.air_kills + b.2.ground_kills).cmp(&(a.2.air_kills + a.2.ground_kills))
                });
                Ok(entries)
            }
            Some(rid) => {
                // Per-round: sum aggregates tree entries for this round across all vehicles
                let mut map: std::collections::HashMap<Ucid, Aggregates> = std::collections::HashMap::new();
                for r in self.pilots.aggregates.iter() {
                    let ((ucid, _vehicle, round_id), agg) = r?;
                    if round_id != rid { continue; }
                    let e = map.entry(ucid).or_insert_with(Aggregates::default);
                    e.air_kills       += agg.air_kills;
                    e.ground_kills    += agg.ground_kills;
                    e.captures        += agg.captures;
                    e.repairs         += agg.repairs;
                    e.supply_transfers += agg.supply_transfers;
                    e.troops          += agg.troops;
                    e.farps           += agg.farps;
                    e.deploys         += agg.deploys;
                    e.actions         += agg.actions;
                    e.deaths          += agg.deaths;
                    e.hours           += agg.hours;
                    e.donated_points  += agg.donated_points;
                }
                let mut entries: Vec<(Ucid, String, Aggregates)> = map
                    .into_iter()
                    .map(|(ucid, agg)| {
                        let name = self.pilots.pilots.get(&ucid)
                            .ok().flatten()
                            .and_then(|p| p.name.last().cloned())
                            .unwrap_or_default();
                        (ucid, name, agg)
                    })
                    .collect();
                entries.sort_by(|a, b| {
                    (b.2.air_kills + b.2.ground_kills).cmp(&(a.2.air_kills + a.2.ground_kills))
                });
                Ok(entries)
            }
        }
    }

    /// Get all pilot UCIDs and their most recent names (all-time, for name resolution)
    /// Latest known display name for a pilot, if we've ever seen them.
    pub(crate) fn pilot_name(&self, ucid: &Ucid) -> Option<std::string::String> {
        self.pilots
            .pilots
            .get(ucid)
            .ok()
            .flatten()
            .and_then(|p| p.name.last().map(|s| s.to_string()))
    }

    pub(crate) fn all_pilot_names(&self) -> Result<Vec<(Ucid, String)>> {
        let mut entries = Vec::new();
        for r in self.pilots.pilots.iter() {
            let (ucid, pilot) = r?;
            let name = pilot.name.last().map(|s| s.clone()).unwrap_or_default();
            entries.push((ucid, name));
        }
        Ok(entries)
    }

    /// Get the latest round for each scenario
    /// Pilot points for active round, sorted descending
    pub(crate) fn pilot_points(&self, round: RoundId) -> Result<Vec<(std::string::String, i32, std::string::String)>> {
        // Returns Vec<(name, points, side)>
        let mut result = Vec::new();
        for r in self.pilots.round_info.iter() {
            let ((ucid, rid), ri) = r?;
            if rid != round { continue; }
            if ri.points == 0 { continue; }
            let name = self.pilots.pilots.get(&ucid)?
                .and_then(|p| p.name.last().map(|s| s.to_string()))
                .unwrap_or_default();
            let side = format!("{:?}", ri.side.1);
            result.push((name, ri.points, side));
        }
        result.sort_by(|a, b| b.1.cmp(&a.1));
        Ok(result)
    }

    /// Most captured objectives for a round, sorted by capture count desc
    pub(crate) fn most_captured(&self, round: RoundId) -> Result<Vec<(std::string::String, u32)>> {
        // Returns Vec<(objective_name, capture_count)>
        let mut result = Vec::new();
        for r in self.objective_captures.scan_prefix(&round)? {
            let ((_, oid), count) = r?;
            // Look up objective name
            let name = self.objectives.get(&(round, oid))?
                .map(|o| o.name.to_string())
                .unwrap_or_else(|| format!("{:?}", oid));
            result.push((name, count));
        }
        result.sort_by(|a, b| b.1.cmp(&a.1));
        Ok(result)
    }

    /// Recent capture events for a round, newest first, with pilot
    /// attribution -- distinct from most_captured, which is just a count.
    pub(crate) fn recent_captures(&self, round: RoundId, limit: usize) -> Result<Vec<CaptureRecord>> {
        let mut result = Vec::new();
        for r in self.captures.scan_prefix(&round)?.rev() {
            let (_, rec) = r?;
            result.push(rec);
            if result.len() >= limit {
                break;
            }
        }
        Ok(result)
    }

    /// Aircraft usage stats for a round, sorted by sortie count desc
    pub(crate) fn aircraft_usage(&self, round: RoundId) -> Result<Vec<(std::string::String, u32, f32)>> {
        // Returns Vec<(vehicle_type, sortie_count, total_hours)>
        let mut result = Vec::new();
        for r in self.aircraft_sorties.scan_prefix(&round)? {
            let ((_, vehicle), (count, hours)) = r?;
            result.push((vehicle, count, hours));
        }
        result.sort_by(|a, b| b.1.cmp(&a.1));
        Ok(result)
    }

    /// Get connected pilots for a round with name, side, and current aircraft type
    pub(crate) fn connected_pilots(&self, round: RoundId) -> Result<Vec<(std::string::String, std::string::String, Side, Option<std::string::String>)>> {
        // Returns Vec<(ucid, name, side, aircraft_type)> for currently connected pilots
        let mut result = Vec::new();
        for r in self.pilots.round_info.iter() {
            let ((ucid, rid), ri) = r?;
            if rid != round { continue; }
            if ri.connected.is_none() { continue; }
            let name = self.pilots.pilots.get(&ucid)?
                .and_then(|p| p.name.last().map(|s| s.to_string()))
                .unwrap_or_default();
            let aircraft = ri.slot.and_then(|s| s.vehicle).map(|v| format!("{}", v));
            result.push((ucid.to_string(), name, ri.side.1, aircraft));
        }
        result.sort_by(|a, b| a.2.cmp(&b.2).then(a.1.cmp(&b.1)));
        Ok(result)
    }

    /// Count registered pilots per side and online pilots for a round
    pub(crate) fn pilot_side_counts(&self, round: RoundId) -> Result<(u32, u32, u32, u32)> {
        // Returns (blue_registered, red_registered, blue_online, red_online)
        let mut blue_reg = 0u32;
        let mut red_reg  = 0u32;
        let mut blue_online = 0u32;
        let mut red_online  = 0u32;
        for r in self.pilots.round_info.iter() {
            let ((_, rid), ri) = r?;
            if rid != round { continue; }
            match ri.side.1 {
                Side::Blue => {
                    blue_reg += 1;
                    if ri.connected.is_some() { blue_online += 1; }
                }
                Side::Red => {
                    red_reg += 1;
                    if ri.connected.is_some() { red_online += 1; }
                }
                _ => {}
            }
        }
        Ok((blue_reg, red_reg, blue_online, red_online))
    }

    pub(crate) fn latest_weather(&self, inst: &InstanceState) -> Option<WeatherSnapshot> {
        inst.latest_weather.read().ok()?.clone()
    }

    pub(crate) fn latest_session_end(&self) -> Result<Option<SessionEnd>> {
        // Walk all sessions, newest last, return the most recent one that has a SessionEnd.
        // Skip individual records that fail to deserialize (e.g. written by an
        // older/incompatible build, or left partially-written by an unclean
        // shutdown) instead of letting one bad entry permanently break
        // /api/admin/perf for every session that comes after it.
        let mut latest: Option<SessionEnd> = None;
        for r in self.session.iter() {
            let session = match r {
                Ok((_, session)) => session,
                Err(e) => {
                    log::warn!("latest_session_end: skipping unreadable session record: {e:?}");
                    continue;
                }
            };
            if let Some(end) = session.end {
                latest = Some(end);
            }
        }
        Ok(latest)
    }

    // ── Admin ban management ─────────────────────────────────────────────────

    pub(crate) fn ban_player(&self, ucid: Ucid, record: BanRecord) -> Result<()> {
        self.admin_bans.insert(&ucid, &record)?;
        Ok(())
    }

    pub(crate) fn unban_player(&self, ucid: &Ucid) -> Result<bool> {
        let had = self.admin_bans.remove(ucid)?.is_some();
        Ok(had)
    }

    pub(crate) fn list_admin_bans(&self) -> Result<Vec<(Ucid, BanRecord)>> {
        let mut out = Vec::new();
        for r in self.admin_bans.iter() {
            let (ucid, rec) = r?;
            out.push((ucid, rec));
        }
        Ok(out)
    }

    /// Bans recorded by bflib in the latest session's Cfg (read-only mirror)
    pub(crate) fn session_bans_from_cfg(&self) -> Result<Vec<(Ucid, std::string::String, Option<DateTime<Utc>>)>> {
        // Skip records that fail to deserialize (e.g. written by an older/
        // incompatible build) instead of letting one bad entry break this
        // for every session that comes after it -- see latest_session_end.
        let mut latest_cfg: Option<Cfg> = None;
        for r in self.session.iter() {
            let (_, s) = match r {
                Ok(v) => v,
                Err(e) => {
                    log::warn!("session_bans_from_cfg: skipping unreadable session record: {e:?}");
                    continue;
                }
            };
            latest_cfg = Some(s.cfg);
        }
        let mut out = Vec::new();
        if let Some(cfg) = latest_cfg {
            for (ucid, (until, name)) in &cfg.banned {
                out.push((*ucid, name.to_string(), *until));
            }
        }
        Ok(out)
    }


    // ── DCS unit range database snapshots ────────────────────────────────────

    /// Store a snapshot harvested by the engine. Returns the previous version
    /// when this one is different, i.e. "DCS changed under us, go diff".
    pub(crate) fn store_unit_db(
        &self,
        inst: &str,
        version: &str,
        json: &str,
    ) -> Result<Option<std::string::String>> {
        let key = (inst.to_string(), version.to_string());
        self.unit_db.insert(&key, &json.to_string())?;
        let prev = self.unit_db_latest.get(&inst.to_string())?;
        self.unit_db_latest
            .insert(&inst.to_string(), &version.to_string())?;
        Ok(prev.filter(|p| p != version))
    }

    pub(crate) fn unit_db_json(&self, inst: &str, version: &str) -> Result<Option<std::string::String>> {
        Ok(self.unit_db.get(&(inst.to_string(), version.to_string()))?)
    }

    /// Every DCS version we have a snapshot for, oldest insertion first is not
    /// knowable, so these come back in key order.
    pub(crate) fn unit_db_versions(&self, inst: &str) -> Result<Vec<std::string::String>> {
        let mut out = vec![];
        for r in self.unit_db.scan_prefix(&inst.to_string())? {
            let ((_, version), _) = r?;
            out.push(version);
        }
        Ok(out)
    }

    pub(crate) fn unit_db_latest(&self, inst: &str) -> Result<Option<(std::string::String, std::string::String)>> {
        let Some(version) = self.unit_db_latest.get(&inst.to_string())? else {
            return Ok(None);
        };
        match self.unit_db_json(inst, &version)? {
            Some(json) => Ok(Some((version, json))),
            None => Ok(None),
        }
    }

    /// Artillery ranges in the campaign config that no longer agree with what
    /// DCS says. An entry in `cfg.artillery.units` overrides the harvest, so a
    /// disagreement is either a patch worth absorbing or an override worth
    /// keeping -- but it should never be a surprise.
    pub(crate) fn unit_db_stale_overrides(&self, inst: &str) -> Result<Vec<StaleOverride>> {
        let Some((_, json)) = self.unit_db_latest(inst)? else {
            return Ok(vec![]);
        };
        let snap: UnitDbSnapshot = serde_json::from_str(&json)?;
        let mut latest_cfg: Option<Cfg> = None;
        for r in self.session.iter() {
            match r {
                Ok((_, s)) => latest_cfg = Some(s.cfg),
                Err(e) => {
                    log::warn!("unit_db_stale_overrides: skipping unreadable session: {e:?}");
                    continue;
                }
            }
        }
        let Some(art) = latest_cfg.and_then(|c| c.artillery) else {
            return Ok(vec![]);
        };
        let mut out = vec![];
        for (typ, over) in &art.units {
            let Some(info) = snap.by_type.get(typ.as_str()) else {
                out.push(StaleOverride {
                    typ: typ.to_string(),
                    cfg_max_range_m: over.max_range_m,
                    cfg_min_range_m: over.min_range_m,
                    dcs_max_range_m: None,
                    dcs_min_range_m: None,
                });
                continue;
            };
            let dcs_max = info.threat_range_m;
            let dcs_min = info.threat_range_min_m.unwrap_or(0.0);
            if dcs_max != Some(over.max_range_m) || dcs_min != over.min_range_m {
                out.push(StaleOverride {
                    typ: typ.to_string(),
                    cfg_max_range_m: over.max_range_m,
                    cfg_min_range_m: over.min_range_m,
                    dcs_max_range_m: dcs_max,
                    dcs_min_range_m: info.threat_range_min_m,
                });
            }
        }
        out.sort_by(|a, b| a.typ.cmp(&b.typ));
        Ok(out)
    }

    // ── bfwiki content management ────────────────────────────────────────────

    pub(crate) fn wiki_get_page(&self, slug: &str) -> Result<Option<WikiPage>> {
        self.wiki_pages.get(&slug.to_string())
    }

    /// All pages, sorted by (section, order) -- the order bfwiki's sidebar
    /// renders them in.
    pub(crate) fn wiki_list_pages(&self) -> Result<Vec<(std::string::String, WikiPage)>> {
        let mut out = Vec::new();
        for r in self.wiki_pages.iter() {
            let (slug, page) = r?;
            out.push((slug, page));
        }
        // Sections read top-to-bottom in a deliberate order, not alphabetically
        // ("Advanced Topics" would otherwise sort before "Introduction"). Any
        // section an admin types that isn't in this built-in list just falls
        // in after the known ones, alphabetically among themselves.
        fn section_rank(section: &str) -> i32 {
            match section {
                "Introduction" => 0,
                "Getting Started" => 1,
                "Playbooks" => 2,
                "Core Gameplay" => 3,
                "F10 Menu Systems" => 4,
                "Advanced Topics" => 5,
                "Reference" => 6,
                _ => 100,
            }
        }
        out.sort_by(|(_, a), (_, b)| {
            section_rank(&a.section).cmp(&section_rank(&b.section))
                .then(a.section.cmp(&b.section))
                .then(a.order.cmp(&b.order))
        });
        Ok(out)
    }

    pub(crate) fn wiki_save_page(&self, slug: &str, page: WikiPage) -> Result<()> {
        self.wiki_pages.insert(&slug.to_string(), &page)?;
        Ok(())
    }

    pub(crate) fn wiki_delete_page(&self, slug: &str) -> Result<bool> {
        Ok(self.wiki_pages.remove(&slug.to_string())?.is_some())
    }

    pub(crate) fn wiki_save_image(&self, id: Uuid, image: WikiImage) -> Result<()> {
        self.wiki_images.insert(&id, &image)?;
        Ok(())
    }

    pub(crate) fn wiki_get_image(&self, id: &Uuid) -> Result<Option<WikiImage>> {
        self.wiki_images.get(id)
    }

    // ── Recon intel (TARPS) ─────────────────────────────────────────────────

    /// The coalition a pilot belongs to this campaign, for the recon-intel
    /// gate. Prefers the active round's registration, then falls back to the
    /// most recent Blue/Red side the pilot held in *any* round on record.
    ///
    /// The campaign-level "locked to one coalition until reset" lock lives in
    /// bflib's game DB, not here -- bfdb only learns a pilot's side from the
    /// `Register`/`Sideswitch` stats bflib emits when they connect to the
    /// server, and each mission restart opens a fresh round. But
    /// `reset_campaign_data` wipes `round_info`, so "any round still on
    /// record" == "this campaign", which is the same lock. Without this
    /// fallback a registered pilot who simply hasn't rejoined the server
    /// since the last restart would be walled out of their own coalition's
    /// intel.
    pub(crate) fn pilot_current_side(
        &self,
        inst: &InstanceId,
        ucid: &Ucid,
    ) -> Result<Option<Side>> {
        let active = self.active_round_id(inst)?;
        let mut best: Option<(DateTime<Utc>, Side)> = None;
        for r in self.pilots.round_info.scan_prefix(ucid)? {
            let ((_, rid), ri) = r?;
            if !matches!(ri.side.1, Side::Blue | Side::Red) {
                continue;
            }
            if Some(rid) == active {
                return Ok(Some(ri.side.1));
            }
            if best.map_or(true, |(t, _)| ri.side.0 > t) {
                best = Some(ri.side);
            }
        }
        Ok(best.map(|(_, s)| s))
    }

    /// The active round id for one instance, if any.
    pub(crate) fn active_round_id(&self, inst: &InstanceId) -> Result<Option<RoundId>> {
        Ok(self
            .latest_rounds_for(inst)?
            .into_iter()
            .find(|(_, _, r)| r.end.is_none())
            .map(|(_, rid, _)| rid))
    }

    /// All intel captures for `round`, optionally filtered to one `side`.
    /// Also opportunistically drops captures from any *other* round so a
    /// mission restart leaves no stale imagery behind.
    pub(crate) fn intel_list(
        &self,
        round: RoundId,
        side: Option<Side>,
    ) -> Result<Vec<(Uuid, IntelCapture)>> {
        let mut stale: Vec<(RoundId, Uuid)> = Vec::new();
        let mut out = Vec::new();
        for r in self.intel_captures.iter() {
            let ((rid, id), cap) = r?;
            if rid != round {
                stale.push((rid, id));
                continue;
            }
            if let Some(s) = side {
                if cap.side != s {
                    continue;
                }
            }
            out.push((id, cap));
        }
        for (rid, id) in stale {
            if let Some(cap) = self.intel_captures.remove(&(rid, id))? {
                self.intel_remove_image(&cap.image_id)?;
            }
        }
        out.sort_by_key(|(_, c)| c.captured_at.unwrap_or(c.uploaded_at));
        Ok(out)
    }

    /// Look up a capture by its id alone (scanning rounds) -- the image and
    /// adjust/delete endpoints only carry the capture id, not the round.
    pub(crate) fn intel_get_by_id(&self, id: &Uuid) -> Result<Option<IntelCapture>> {
        for r in self.intel_captures.iter() {
            let ((_, cid), cap) = r?;
            if cid == *id {
                return Ok(Some(cap));
            }
        }
        Ok(None)
    }

    pub(crate) fn intel_count_side(&self, round: RoundId, side: Side) -> Result<usize> {
        Ok(self.intel_list(round, Some(side))?.len())
    }

    pub(crate) fn intel_put(&self, id: Uuid, cap: IntelCapture) -> Result<()> {
        self.intel_captures.insert(&(cap.round, id), &cap)?;
        Ok(())
    }

    /// Current on-disk photo directory, if `--intel-dir` was set.
    pub(crate) fn intel_dir(&self) -> Option<PathBuf> {
        self.intel_dir.read().ok().and_then(|g| g.clone())
    }

    /// Point recon-photo storage at a directory on disk (creating it), or
    /// `None` to keep photos as blobs in the DB. Called once at startup.
    pub(crate) fn set_intel_dir(&self, dir: Option<PathBuf>) -> Result<()> {
        if let Some(d) = &dir {
            std::fs::create_dir_all(d)
                .with_context(|| format!("creating --intel-dir {}", d.display()))?;
        }
        *self.intel_dir.write().unwrap() = dir;
        Ok(())
    }

    pub(crate) fn intel_put_image(
        &self,
        id: Uuid,
        content_type: std::string::String,
        bytes: Vec<u8>,
    ) -> Result<()> {
        let row = match self.intel_dir() {
            Some(dir) => {
                std::fs::create_dir_all(&dir)?;
                std::fs::write(dir.join(id.to_string()), &bytes)
                    .with_context(|| format!("writing intel image {id} to {}", dir.display()))?;
                IntelImage { content_type, data: None }
            }
            None => IntelImage { content_type, data: Some(bytes) },
        };
        self.intel_images.insert(&id, &row)?;
        Ok(())
    }

    /// Resolve an image to `(content_type, bytes)`, reading from disk if it
    /// was stored there.
    pub(crate) fn intel_get_image(&self, id: &Uuid) -> Result<Option<(std::string::String, Vec<u8>)>> {
        let Some(row) = self.intel_images.get(id)? else { return Ok(None) };
        let bytes = match row.data {
            Some(b) => b,
            None => {
                let dir = self
                    .intel_dir()
                    .ok_or_else(|| anyhow!("intel image {id} is on disk but --intel-dir is not set"))?;
                std::fs::read(dir.join(id.to_string()))
                    .with_context(|| format!("reading intel image {id} from {}", dir.display()))?
            }
        };
        Ok(Some((row.content_type, bytes)))
    }

    /// Drop an image row and its on-disk file, if any. Best effort on the file.
    fn intel_remove_image(&self, image_id: &Uuid) -> Result<()> {
        if let Some(row) = self.intel_images.remove(image_id)? {
            if row.data.is_none() {
                if let Some(dir) = self.intel_dir() {
                    let _ = std::fs::remove_file(dir.join(image_id.to_string()));
                }
            }
        }
        Ok(())
    }

    pub(crate) fn intel_delete(&self, round: RoundId, id: &Uuid) -> Result<bool> {
        match self.intel_captures.remove(&(round, *id))? {
            Some(cap) => {
                self.intel_remove_image(&cap.image_id)?;
                Ok(true)
            }
            None => Ok(false),
        }
    }

    /// Purge only the recon intel belonging to `rounds` -- the per-instance
    /// counterpart of `intel_purge_all`, used when one server's campaign is
    /// reset while another's keeps running.
    pub(crate) fn intel_purge_all_for(&self, rounds: &HashSet<RoundId>) -> Result<()> {
        if rounds.is_empty() {
            return Ok(());
        }
        let dead: Vec<((RoundId, Uuid), IntelCapture)> = self
            .intel_captures
            .iter()
            .filter_map(|r| r.ok())
            .filter(|((rid, _), _)| rounds.contains(rid))
            .collect();
        for (key, cap) in dead {
            self.intel_remove_image(&cap.image_id)?;
            self.intel_captures.remove(&key)?;
        }
        let markup: Vec<(RoundId, Uuid)> = self
            .intel_markup
            .iter()
            .filter_map(|r| r.ok())
            .map(|(k, _)| k)
            .filter(|(rid, _)| rounds.contains(rid))
            .collect();
        for k in markup {
            self.intel_markup.remove(&k)?;
        }
        Ok(())
    }

    pub(crate) fn intel_purge_all(&self) -> Result<()> {
        // Remove on-disk files before clearing the index.
        for r in self.intel_images.iter() {
            let (id, row) = r?;
            if row.data.is_none() {
                if let Some(dir) = self.intel_dir() {
                    let _ = std::fs::remove_file(dir.join(id.to_string()));
                }
            }
        }
        self.intel_captures.clear()?;
        self.intel_images.clear()?;
        self.intel_markup.clear()?;
        Ok(())
    }

    // ── Recon intel markup ─────────────────────────────────────────────────

    pub(crate) fn intel_markup_list(
        &self,
        round: RoundId,
        side: Option<Side>,
    ) -> Result<Vec<(Uuid, IntelMarkup)>> {
        let mut out = Vec::new();
        for r in self.intel_markup.iter() {
            let ((rid, id), m) = r?;
            if rid != round {
                continue;
            }
            if let Some(s) = side {
                if m.side != s {
                    continue;
                }
            }
            out.push((id, m));
        }
        out.sort_by_key(|(_, m)| m.at);
        Ok(out)
    }

    pub(crate) fn intel_markup_put(&self, id: Uuid, m: IntelMarkup) -> Result<()> {
        self.intel_markup.insert(&(m.round, id), &m)?;
        Ok(())
    }

    /// Look up a markup item by id alone (scanning rounds).
    pub(crate) fn intel_markup_get(&self, id: &Uuid) -> Result<Option<IntelMarkup>> {
        for r in self.intel_markup.iter() {
            let ((_, mid), m) = r?;
            if mid == *id {
                return Ok(Some(m));
            }
        }
        Ok(None)
    }

    pub(crate) fn intel_markup_delete(&self, round: RoundId, id: &Uuid) -> Result<bool> {
        Ok(self.intel_markup.remove(&(round, *id))?.is_some())
    }

    /// JSON payload of the active round's markup (both coalitions) for the
    /// engine's F10-map feed. `None` when there's no active round.
    pub(crate) fn intel_marks_payload(
        &self,
        inst: &InstanceId,
    ) -> Result<Option<std::string::String>> {
        let Some(round) = self.active_round_id(inst)? else {
            return Ok(None);
        };
        let items = self.intel_markup_list(round, None)?;
        let marks: Vec<serde_json::Value> = items
            .iter()
            .map(|(id, m)| {
                serde_json::json!({
                    "id":      id.to_string(),
                    "side":    format!("{:?}", m.side),
                    "kind":    m.kind,
                    "points":  m.points,
                    "color":   m.color,
                    "by_name": m.by_name,
                })
            })
            .collect();
        Ok(Some(serde_json::json!({ "marks": marks }).to_string()))
    }

    /// Seed / refresh the built-in gameplay wiki content (compiled in from
    /// `bfdb/seed_wiki/`). A page is (re)written from the compiled-in source
    /// only when it is missing or still at its seed version (`updated_by ==
    /// "seed"`) -- once an admin edits a page through bfwiki its
    /// `updated_by` changes and it is never overwritten again. So a bfdb
    /// deploy carrying updated seed markdown pushes those updates to every
    /// page nobody has hand-edited, without clobbering admin edits or
    /// resurrecting admin-deleted pages that were themselves edited first.
    fn seed_wiki_if_empty(&self) -> Result<()> {
        let empty = self.wiki_pages.iter().next().is_none();
        if empty {
            info!("seeding bfwiki with default gameplay content");
        }
        let now = Utc::now();
        let seed: &[(&str, &str, &str, i32, &str)] = &[
            ("introduction", "What is Fowl Engine?", "Introduction", 0, include_str!("../seed_wiki/introduction.md")),
            ("getting-started/welcome", "Welcome", "Getting Started", 0, include_str!("../seed_wiki/getting-started/welcome.md")),
            ("getting-started/joining-team", "Joining a Team", "Getting Started", 1, include_str!("../seed_wiki/getting-started/joining-team.md")),
            ("getting-started/hud-and-menus", "Understanding the Menus", "Getting Started", 2, include_str!("../seed_wiki/getting-started/hud-and-menus.md")),
            ("playbooks/first-sortie", "Your First Sortie", "Playbooks", 0, include_str!("../seed_wiki/playbooks/first-sortie.md")),
            ("playbooks/reading-live-ops", "Reading the Live Ops Dashboard", "Playbooks", 1, include_str!("../seed_wiki/playbooks/reading-live-ops.md")),
            ("playbooks/capturing-a-base", "Capturing a Base", "Playbooks", 2, include_str!("../seed_wiki/playbooks/capturing-a-base.md")),
            ("playbooks/cas-with-jtac", "Flying CAS With a JTAC", "Playbooks", 3, include_str!("../seed_wiki/playbooks/cas-with-jtac.md")),
            ("playbooks/running-crates", "Running Crates & Building a Base", "Playbooks", 4, include_str!("../seed_wiki/playbooks/running-crates.md")),
            ("playbooks/calling-support", "Calling AWACS, Tankers & CAP", "Playbooks", 5, include_str!("../seed_wiki/playbooks/calling-support.md")),
            ("gameplay/objectives", "Objectives", "Core Gameplay", 0, include_str!("../seed_wiki/gameplay/objectives.md")),
            ("gameplay/capturing-objectives", "Capturing Objectives", "Core Gameplay", 1, include_str!("../seed_wiki/gameplay/capturing-objectives.md")),
            ("gameplay/logistics", "Logistics & Supply", "Core Gameplay", 2, include_str!("../seed_wiki/gameplay/logistics.md")),
            ("gameplay/points-and-lives", "Points and Lives", "Core Gameplay", 3, include_str!("../seed_wiki/gameplay/points-and-lives.md")),
            ("gameplay/chat-commands", "Chat Commands", "Core Gameplay", 4, include_str!("../seed_wiki/gameplay/chat-commands.md")),
            ("gameplay/gci", "Live GCI (AWACS Calls)", "Core Gameplay", 5, include_str!("../seed_wiki/gameplay/gci.md")),
            ("gameplay/briefing", "The Auto-Generated Briefing", "Core Gameplay", 6, include_str!("../seed_wiki/gameplay/briefing.md")),
            ("gameplay/comms-plan", "Comms Plan", "Core Gameplay", 7, include_str!("../seed_wiki/gameplay/comms-plan.md")),
            ("gameplay/tasking-board", "The Tasking Board", "Core Gameplay", 8, include_str!("../seed_wiki/gameplay/tasking-board.md")),
            ("gameplay/war-economy", "Materiel & the War Economy", "Core Gameplay", 9, include_str!("../seed_wiki/gameplay/war-economy.md")),
            ("gameplay/navaids", "Navaids & Approaches", "Core Gameplay", 10, include_str!("../seed_wiki/gameplay/navaids.md")),
            ("gameplay/carrier-ops", "Carrier Operations", "Core Gameplay", 11, include_str!("../seed_wiki/gameplay/carrier-ops.md")),
            ("f10-menu/overview", "Overview", "F10 Menu Systems", 0, include_str!("../seed_wiki/f10-menu/overview.md")),
            ("f10-menu/actions", "Actions Menu", "F10 Menu Systems", 1, include_str!("../seed_wiki/f10-menu/actions.md")),
            ("f10-menu/jtac", "JTAC System", "F10 Menu Systems", 2, include_str!("../seed_wiki/f10-menu/jtac.md")),
            ("f10-menu/cargo", "Cargo Operations", "F10 Menu Systems", 3, include_str!("../seed_wiki/f10-menu/cargo.md")),
            ("f10-menu/troops", "Troop Transport", "F10 Menu Systems", 4, include_str!("../seed_wiki/f10-menu/troops.md")),
            ("f10-menu/ewr", "GCI / EWR", "F10 Menu Systems", 5, include_str!("../seed_wiki/f10-menu/ewr.md")),
            ("f10-menu/recon", "Reconnaissance", "F10 Menu Systems", 6, include_str!("../seed_wiki/f10-menu/recon.md")),
            ("f10-menu/objectives", "Objectives Menu", "F10 Menu Systems", 7, include_str!("../seed_wiki/f10-menu/objectives.md")),
            ("f10-menu/info", "Info Menu", "F10 Menu Systems", 8, include_str!("../seed_wiki/f10-menu/info.md")),
            ("f10-menu/csar", "Combat Search & Rescue", "F10 Menu Systems", 9, include_str!("../seed_wiki/f10-menu/csar.md")),
            ("advanced/artillery", "Artillery Missions", "Advanced Topics", 0, include_str!("../seed_wiki/advanced/artillery.md")),
            ("advanced/alcm", "Air-Launched Cruise Missiles", "Advanced Topics", 1, include_str!("../seed_wiki/advanced/alcm.md")),
            ("reference/chat-commands", "Chat Command List", "Reference", 0, include_str!("../seed_wiki/reference/chat-commands.md")),
            ("reference/action-types", "Action Types", "Reference", 1, include_str!("../seed_wiki/reference/action-types.md")),
            ("reference/deployables", "Deployable Units", "Reference", 2, include_str!("../seed_wiki/reference/deployables.md")),
            ("reference/faq", "FAQ", "Reference", 3, include_str!("../seed_wiki/reference/faq.md")),
            ("reference/aircraft-roster", "Aircraft Roster", "Reference", 4, include_str!("../seed_wiki/reference/aircraft-roster.md")),
            ("reference/tips", "Tips & Best Practices", "Reference", 5, include_str!("../seed_wiki/reference/tips.md")),
            ("reference/changelog", "Changelog — v2.0", "Reference", 6, include_str!("../seed_wiki/reference/changelog.md")),
            ("advanced/c130-airdrop", "C-130 Hercules & Airdrop", "Advanced Topics", 2, include_str!("../seed_wiki/advanced/c130-airdrop.md")),
            ("advanced/deployables-guide", "Deployables Guide", "Advanced Topics", 3, include_str!("../seed_wiki/advanced/deployables-guide.md")),
            ("advanced/recon-intel-map", "Recon Intel Map (TARPS)", "Advanced Topics", 4, include_str!("../seed_wiki/advanced/recon-intel-map.md")),
            ("advanced/helo-missions", "AI Helo Missions", "Advanced Topics", 5, include_str!("../seed_wiki/advanced/helo-missions.md")),
        ];
        let mut refreshed = 0u32;
        for (slug, title, section, order, content) in seed {
            match self.wiki_pages.get(&slug.to_string())? {
                // admin-edited (or newer): leave it alone
                Some(existing) if existing.updated_by != "seed" => continue,
                // still at seed version and unchanged: nothing to do
                Some(existing)
                    if existing.content == *content
                        && existing.title == *title
                        && existing.section == *section
                        && existing.order == *order =>
                {
                    continue
                }
                Some(_) => refreshed += 1,
                None => {}
            }
            self.wiki_pages.insert(&slug.to_string(), &WikiPage {
                title: title.to_string(),
                section: section.to_string(),
                order: *order,
                content: content.to_string(),
                updated_at: now,
                updated_by: "seed".to_string(),
            })?;
        }
        if refreshed > 0 {
            info!("refreshed {refreshed} un-edited bfwiki page(s) from updated seed content");
        }
        Ok(())
    }

    /// One-time seed of the images referenced by the built-in gameplay wiki
    /// content (pulled from bfsite's marketing screenshots). Fixed ids so
    /// every fresh deployment gets the same URLs the seed Markdown embeds --
    /// only runs if the images tree is completely empty.
    fn seed_wiki_images_if_empty(&self) -> Result<()> {
        if self.wiki_images.iter().next().is_some() {
            return Ok(());
        }
        info!("seeding bfwiki with default screenshots");
        let now = Utc::now();
        let seed: &[(&str, &[u8])] = &[
            ("cf08da53-e826-49b2-8b67-7ddded3cbb74", include_bytes!("../seed_wiki/images/server-browser.jpeg")),
            ("934a41dc-d2ad-4b52-8dfd-60cbdc4deb78", include_bytes!("../seed_wiki/images/objective-types.jpeg")),
            ("0ce0f6d9-ab3e-4f5b-87e3-799b0e2964cd", include_bytes!("../seed_wiki/images/patriot-site.jpeg")),
            ("f2ad53a9-2e98-4925-bced-de97267fc7e6", include_bytes!("../seed_wiki/images/carrier-group.jpeg")),
            ("f0af8d1d-8ae3-4a78-a361-27d14e55aa33", include_bytes!("../seed_wiki/images/csar-rescue.jpeg")),
            ("7d6453ff-8e35-4304-8a34-d61e680b7f83", include_bytes!("../seed_wiki/images/convoy-interdiction.jpeg")),
            ("1e34aa38-f253-4652-b725-c30cc1553a38", include_bytes!("../seed_wiki/images/nine-line-brief.jpeg")),
            ("76185372-d487-422b-a6ea-89de0da561d8", include_bytes!("../seed_wiki/images/c130-hero.jpeg")),
            ("61a90050-7359-4ada-b0c9-41f09dc26a34", include_bytes!("../seed_wiki/images/airdrop-parachute.jpeg")),
            ("fab0866b-a54d-4e0a-9647-d246a700d5a6", include_bytes!("../seed_wiki/images/lapes-extraction.jpeg")),
            ("3ce5c418-9d0b-429a-be7e-687032cb147f", include_bytes!("../seed_wiki/images/objective-capture.jpeg")),
            ("b7c7cf5d-7559-4a43-b97e-b304cc4d8ccb", include_bytes!("../seed_wiki/images/himars-strike.jpeg")),
            ("441295e3-48c3-4737-b308-e5f91812884c", include_bytes!("../seed_wiki/images/f10-menu-overview.jpeg")),
            ("ccf9a836-dfdb-4bf6-a6cd-1fb77a55ac9a", include_bytes!("../seed_wiki/images/actions-menu.jpeg")),
            ("fe035065-f175-4f73-babe-9681513a3b93", include_bytes!("../seed_wiki/images/cargo-menu.jpeg")),
            ("a645d067-42f4-4059-a31a-e2e5e378cfa0", include_bytes!("../seed_wiki/images/troop-transport.jpeg")),
            ("b68ad72b-f981-4957-986d-b301edcc4216", include_bytes!("../seed_wiki/images/ewr-report.jpeg")),
            ("7fc9ec2b-79a6-46d1-b61f-27dd1a313ee9", include_bytes!("../seed_wiki/images/alcm-strike.jpeg")),
            ("ed912e81-5a74-493c-a691-2bfff1541054", include_bytes!("../seed_wiki/images/kneeboard-hud.jpeg")),
            ("d7eee98b-0f14-4ed3-a1e0-3bf815ffde9a", include_bytes!("../seed_wiki/images/aircraft-roster.jpeg")),
            ("f5701fbb-6bec-4db2-b96b-7bd33d41880f", include_bytes!("../seed_wiki/images/sa22-pantsir.jpeg")),
        ];
        for (id, data) in seed {
            self.wiki_images.insert(&Uuid::parse_str(id)?, &WikiImage {
                content_type: "image/jpeg".to_string(),
                data: data.to_vec(),
                uploaded_at: now,
                uploaded_by: "seed".to_string(),
            })?;
        }
        Ok(())
    }

    // ── Perf history ─────────────────────────────────────────────────────────

    pub(crate) fn session_perf_history(&self, limit: usize) -> Result<Vec<SessionEnd>> {
        // Skip records that fail to deserialize (e.g. written by an older/
        // incompatible build) instead of letting one bad entry break this
        // for every session that comes after it -- see latest_session_end.
        let mut ends: Vec<SessionEnd> = Vec::new();
        for r in self.session.iter() {
            let (_, s) = match r {
                Ok(v) => v,
                Err(e) => {
                    log::warn!("session_perf_history: skipping unreadable session record: {e:?}");
                    continue;
                }
            };
            if let Some(end) = s.end {
                ends.push(end);
            }
        }
        if ends.len() > limit {
            ends.drain(0..ends.len() - limit);
        }
        Ok(ends)
    }

    pub(crate) fn active_session_stop(&self, round: RoundId) -> Option<DateTime<Utc>> {
        self.session
            .scan_prefix(&round)
            .ok()?
            .next_back()
            .and_then(|r| r.ok())
            .and_then(|(_, s)| s.stop_time)
    }

    /// `latest_rounds` restricted to one DCS server instance. Two instances can
    /// legitimately run the same scenario name, so filtering by scenario alone
    /// is not enough -- the round tag is authoritative.
    pub(crate) fn latest_rounds_for(
        &self,
        inst: &InstanceId,
    ) -> Result<Vec<(Scenario, RoundId, Round)>> {
        self.latest_rounds_inner(Some(inst))
    }

    fn latest_rounds_inner(
        &self,
        inst: Option<&InstanceId>,
    ) -> Result<Vec<(Scenario, RoundId, Round)>> {
        let mut rounds = Vec::new();
        let mut seen_scenarios = std::collections::HashSet::new();
        // Scan all rounds, keep the latest per scenario
        for r in self.round.iter() {
            let ((scenario, rid), round) = r?;
            if let Some(inst) = inst {
                if &self.round_instance_of(rid) != inst {
                    continue;
                }
            }
            if !seen_scenarios.contains(&scenario) || round.end.is_none() {
                seen_scenarios.insert(scenario.clone());
                // Remove previous entry for this scenario if exists
                rounds.retain(|(s, _, _): &(Scenario, RoundId, Round)| s != &scenario);
                rounds.push((scenario, rid, round));
            }
        }
        Ok(rounds)
    }

    /// Every round ever recorded, not just the latest per scenario. Used for
    /// the round-history selector -- `latest_rounds` intentionally discards
    /// history and can't serve that purpose.
    pub(crate) fn all_rounds(&self) -> Result<Vec<(Scenario, RoundId, Round)>> {
        let mut rounds = Vec::new();
        for r in self.round.iter() {
            let ((scenario, rid), round) = r?;
            rounds.push((scenario, rid, round));
        }
        rounds.sort_by(|a, b| b.2.start.cmp(&a.2.start));
        Ok(rounds)
    }

    /// Get objectives for a given round
    pub(crate) fn objectives_for_round(&self, round: RoundId) -> Result<Vec<(ObjectiveId, Objective)>> {
        let mut objs = Vec::new();
        for r in self.objectives.scan_prefix(&round)? {
            let ((_, oid), obj) = r?;
            objs.push((oid, obj));
        }
        Ok(objs)
    }

    /// Get all detected, alive units for a given round
    pub(crate) fn detected_units_for_round(
        &self,
        round: RoundId,
    ) -> Result<Vec<(EnId, Unit, BitFlags<DetectionSource, u8>)>> {
        let mut results = Vec::new();
        for r in self.detected.scan_prefix(&round)? {
            let ((_, eid), flags) = r?;
            if flags.is_empty() {
                continue;
            }
            if let Some(unit) = self.units.get(&(round, eid))? {
                if !unit.dead {
                    results.push((eid, unit, flags));
                }
            }
        }
        Ok(results)
    }

    /// Get recent kills for a round (last N)
    /// A pilot's lifetime totals as the public sees them. Test-instance rounds
    /// are already excluded: they never reach `Pilot.total` in the first place
    /// (see `with_pilot_and_aggregates`).
    pub(crate) fn pilot_detail(&self, ucid: &Ucid) -> Result<Option<(String, Aggregates)>> {
        match self.pilots.pilots.get(ucid)? {
            None => Ok(None),
            Some(pilot) => {
                let name = pilot.name.last().cloned().unwrap_or_default();
                Ok(Some((name, pilot.total)))
            }
        }
    }

    /// All sorties for a pilot across all rounds, sorted chronologically
    pub(crate) fn pilot_sorties(&self, ucid: &Ucid) -> Result<Vec<(RoundId, SortieId, Sortie)>> {
        let mut result = Vec::new();
        for r in self.pilots.sortie.scan_prefix(ucid)? {
            let ((_, round_id, sortie_id), sortie) = r?;
            result.push((round_id, sortie_id, sortie));
        }
        // Sort chronologically
        result.sort_by(|a, b| a.2.takeoff.cmp(&b.2.takeoff));
        Ok(result)
    }

    /// Per-round aggregates for a pilot, enriched with scenario name
    pub(crate) fn pilot_round_breakdown(&self, ucid: &Ucid) -> Result<Vec<(Scenario, RoundId, Aggregates)>> {
        // Build a round_id → scenario lookup
        let mut rid_to_scenario: std::collections::HashMap<RoundId, Scenario> = std::collections::HashMap::new();
        for r in self.round.iter() {
            let ((scenario, rid), _) = r?;
            rid_to_scenario.insert(rid, scenario);
        }
        // Sum aggregates per round for this pilot
        let mut map: std::collections::HashMap<RoundId, Aggregates> = std::collections::HashMap::new();
        for r in self.pilots.aggregates.iter() {
            let ((u, _vehicle, round_id), agg) = r?;
            if u != *ucid { continue; }
            let e = map.entry(round_id).or_insert_with(Aggregates::default);
            e.air_kills        += agg.air_kills;
            e.ground_kills     += agg.ground_kills;
            e.captures         += agg.captures;
            e.repairs          += agg.repairs;
            e.supply_transfers += agg.supply_transfers;
            e.troops           += agg.troops;
            e.farps            += agg.farps;
            e.deploys          += agg.deploys;
            e.actions          += agg.actions;
            e.deaths           += agg.deaths;
            e.hours            += agg.hours;
            e.donated_points   += agg.donated_points;
        }
        let mut result: Vec<(Scenario, RoundId, Aggregates)> = map
            .into_iter()
            .map(|(rid, agg)| {
                let scenario = rid_to_scenario.get(&rid).cloned().unwrap_or_default();
                (scenario, rid, agg)
            })
            .collect();
        // Sort by round id ascending (oldest first)
        result.sort_by(|a, b| a.1.cmp(&b.1));
        Ok(result)
    }

    /// All kills made by a specific pilot (killer = Player(ucid)), all rounds
    pub(crate) fn pilot_kills_for(&self, ucid: &Ucid) -> Result<Vec<(RoundId, Dead)>> {
        let prefix_key = EnId::Player(*ucid);
        let mut result = Vec::new();
        for r in self.kills.scan_prefix(&prefix_key)? {
            let ((_, round_id, _), dead) = r?;
            result.push((round_id, dead));
        }
        // Sort newest first
        result.sort_by(|a, b| b.1.time.cmp(&a.1.time));
        Ok(result)
    }

    /// All deploys done by a specific pilot, all rounds, newest first.
    pub(crate) fn pilot_deploys_for(&self, ucid: &Ucid) -> Result<Vec<(RoundId, DeployRecord)>> {
        let mut result = Vec::new();
        for r in self.deploys.scan_prefix(ucid)? {
            let ((_, round_id, _), rec) = r?;
            result.push((round_id, rec));
        }
        result.sort_by(|a, b| b.1.time.cmp(&a.1.time));
        Ok(result)
    }

    pub(crate) fn recent_kills(&self, round: RoundId, limit: usize) -> Result<Vec<Dead>> {
        // The kills tree is keyed (killer EnId, round, KillId), so iterating it
        // (even reversed) orders by *killer*, not time -- a naive `.rev().take(n)`
        // returns only AI-made kills (EnId::Unit sorts after EnId::Player) and
        // never reaches player kills once the cap is hit. Collect the whole
        // round, dedupe multi-shooter kills by KillId, then sort by time.
        let mut seen: std::collections::HashSet<KillId> = std::collections::HashSet::new();
        let mut kills = Vec::new();
        for r in self.kills.iter() {
            let ((_, rid, kid), dead) = r?;
            if rid == round && seen.insert(kid) {
                kills.push(dead);
            }
        }
        kills.sort_by(|a, b| b.time.cmp(&a.time));
        kills.truncate(limit);
        Ok(kills)
    }

    /// Same classification record_kill uses for air_kills vs ground_kills:
    /// a player death always counts as air (players are always in aircraft),
    /// an AI death counts as air only if the unit is tagged Aircraft or
    /// Helicopter. Exposed separately so API consumers (e.g. the Discord
    /// kill-streak/achievement poller) can filter on the same definition
    /// instead of guessing from the raw DCS unit-type string.
    pub(crate) fn victim_is_air(&self, round: RoundId, victim: &Who) -> Result<bool> {
        Ok(match victim {
            Who::Player { .. } => true,
            Who::AI { uid, .. } => {
                let tags = self
                    .units
                    .get(&(round, EnId::Unit(*uid)))?
                    .map(|u| u.tags)
                    .unwrap_or_default();
                tags.contains(UnitTag::Aircraft) || tags.contains(UnitTag::Helicopter)
            }
        })
    }

    /// The newest `seq` entry for `sortie` that belongs to `inst`.
    ///
    /// Two DCS instances fronted by the same bfdb can legitimately run missions
    /// with the same Sortie name, and `seq`/`round` are keyed by scenario name
    /// alone -- so a bare `scan_prefix(sortie).next_back()` would happily hand
    /// instance A the other server's round and then "end the stale open round"
    /// out from under it. Walking back to the newest entry tagged with our own
    /// instance keeps them apart without changing any stored key.
    fn last_seq_for(
        &self,
        inst: &InstanceState,
        sortie: &Scenario,
    ) -> Result<Option<(RoundId, DateTime<Utc>)>> {
        for r in self.seq.scan_prefix(sortie)?.rev() {
            let ((_, round), seq) = r?;
            if self.round_instance_of(round) == inst.id {
                return Ok(Some((round, seq)));
            }
        }
        Ok(None)
    }

    fn add_stat(
        &self,
        inst: &InstanceState,
        ctx: &mut StatCtx,
        time: DateTime<Utc>,
        stat: Stat,
    ) -> Result<()> {
        if let Some(ctx) = &ctx.0 {
            if time <= ctx.seq {
                return Ok(());
            }
        }
        if let Stat::NewRound { sortie } = &stat {
            ctx.0 = None; // reset on session restart so we re-attach or create a new round
            info!("[{}] processing NewRound sortie={sortie:?}", inst.id);
            match self.last_seq_for(inst, sortie)? {
                None => {
                    info!("NewRound: no existing seq, creating new round");
                    return self.new_round(inst, ctx, time, sortie.clone(), time);
                }
                Some((round, _seq)) => match self.round.get(&(sortie.clone(), round))? {
                    Some(r) if r.end.is_none() => {
                        info!("NewRound: ending stale open round {round:?}, creating new round");
                        let key = (sortie.clone(), round);
                        let mut stale = r;
                        stale.end = Some(time);
                        let _ = self.round.insert(&key, &stale)?;
                        return self.new_round(inst, ctx, time, sortie.clone(), time);
                    }
                    Some(_) => {
                        info!("NewRound: existing round is ended, creating new round");
                        return self.new_round(inst, ctx, time, sortie.clone(), time);
                    }
                    None => {
                        info!("NewRound: seq entry exists but round missing, creating new round");
                        return self.new_round(inst, ctx, time, sortie.clone(), time);
                    }
                },
            }
        }
        // If we see a SessionStart but have no round context, auto-create a round.
        // This happens when reading archives where the NewRound is in a locked/missing file.
        // Only do this when a real sortie can be derived from netidx_base --
        // new_round() unconditionally overwrites the *live* current_sortie
        // (used for real-time engine log/RPC subscriptions) as a side effect,
        // so fabricating a placeholder name here would silently redirect
        // live subscriptions onto a sortie that doesn't exist. Without a real
        // sortie, just skip: the caller below already handles "no round
        // context yet" by dropping the stat gracefully.
        if let Stat::SessionStart { cfg, .. } = &stat {
            if ctx.0.is_none() {
                // Prefer the real sortie already primed from the open round in
                // our DB (see the "resuming with active round" prime at startup).
                // `netidx_base` is the BASE, not `base/sortie` -- its last path
                // segment (e.g. "campaign" from "/local/fowl/campaign") is NOT a
                // sortie, and new_round() would clobber the live current_sortie
                // with it, silently redirecting RPC/log subscriptions to a path
                // that doesn't exist. Only fall back to that guess with nothing.
                let sortie = inst
                    .current_sortie
                    .lock()
                    .unwrap()
                    .clone()
                    .or_else(|| {
                        cfg.netidx_base.as_ref().map(|p| {
                            let s = format!("{p}");
                            String::from(s.rsplit('/').next().unwrap_or("unknown"))
                        })
                    });
                match sortie {
                    Some(sortie) => {
                        // Reattach to the sortie's existing open round if there is
                        // one, instead of spawning a duplicate. A second round id
                        // fragments stats -- deploys/kills/health land in a round
                        // the dashboard never queries.
                        let open = self.last_seq_for(inst, &sortie)?.and_then(|(round, seq)| {
                            match self.round.get(&(sortie.clone(), round)) {
                                Ok(Some(r)) if r.end.is_none() => Some((round, seq)),
                                _ => None,
                            }
                        });
                        match open {
                            Some((round, seq)) => {
                                info!(
                                    "[{}] SessionStart: reattaching to open round {round:?} for sortie {sortie:?}",
                                    inst.id
                                );
                                *inst.current_sortie.lock().unwrap() = Some(sortie.clone());
                                ctx.0 = Some(StatCtxInner { public: inst.cfg.public, sortie, round, seq });
                            }
                            None => {
                                info!(
                                    "[{}] auto-creating round from SessionStart, sortie={sortie:?}",
                                    inst.id
                                );
                                self.new_round(inst, ctx, time, sortie, time)?;
                            }
                        }
                    }
                    None => {
                        warn!("SessionStart with no round context and no sortie to derive -- skipping instead of fabricating a placeholder round");
                    }
                }
            }
        }
        if let Stat::RoundEnd { winner } = &stat {
            return self.round_end(ctx, time, *winner);
        }
        let ctx = match ctx.get_mut() {
            Ok(c) => c,
            Err(_) => return Ok(()), // no NewRound seen yet, skip
        };
        match stat {
            Stat::NewRound { .. } | Stat::RoundEnd { .. } => unreachable!(),
            Stat::SessionStart { stop, cfg } => {
                self.session.insert(
                    &(ctx.round, time),
                    &Session {
                        cfg: (*cfg).clone(),
                        stop_time: stop,
                        end: None,
                    },
                )?;
                // A fresh engine session means no DCS client can possibly be
                // connected yet (they haven't done onPlayerTryConnect this
                // process). Clear any "connected" left over from a previous
                // run that ended without a clean Disconnect for every pilot
                // (crash, kill -9, abrupt DCS server shutdown) -- otherwise
                // the dashboard shows players online forever after a restart.
                let stale: Vec<Ucid> = self
                    .pilots
                    .round_info
                    .iter()
                    .filter_map(|r| r.ok())
                    .filter(|((_, rid), ri)| *rid == ctx.round && ri.connected.is_some())
                    .map(|((ucid, _), _)| ucid)
                    .collect();
                info!(
                    "SessionStart: clearing {} stale connected flag(s) in round {:?}",
                    stale.len(), ctx.round
                );
                for ucid in stale {
                    self.pilots
                        .with_pilot_round_info(ucid, ctx.round, |ri| ri.connected = None)?;
                }
            }
            Stat::SessionEnd {
                api_perf,
                perf,
                frame,
            } => {
                match self
                    .session
                    .scan_prefix(&ctx.round)?
                    .next_back()
                    .transpose()?
                {
                    None => bail!("no session for {} is in progress", &ctx.sortie),
                    Some((k, mut session)) => {
                        session.end = Some(SessionEnd {
                            api: api_perf,
                            engine: perf,
                            frame,
                            time,
                        });
                        self.session.insert(&k, &session)?;
                    }
                }
            }
            Stat::Objective {
                name,
                id,
                pos,
                owner,
                kind,
            } => {
                // bflib re-emits Stat::Objective for every objective on a mission
                // reload. If we already have this objective in the current round,
                // keep its health/logi/supply/fuel rather than resetting to 100 --
                // a fresh ObjectiveHealth only follows when those values change,
                // so clobbering here left the tactical map stuck at 100%.
                let prev = self.objectives.get(&(ctx.round, id))?;
                let (health, logi, supply, fuel, last_change) = match &prev {
                    Some(o) => (o.health, o.logi, o.supply, o.fuel, o.last_change),
                    None => (100, 100, 100, 100, time),
                };
                self.objectives.insert(
                    &(ctx.round, id),
                    &Objective {
                        name,
                        pos,
                        kind,
                        owner,
                        by: None,
                        last_change,
                        health,
                        logi,
                        supply,
                        fuel,
                    },
                )?;
            }
            Stat::ObjectiveDestroyed { id } => {
                self.objectives.remove(&(ctx.round, id))?;
            }
            Stat::ObjectiveHealth {
                id,
                last_change,
                health,
                logi,
            } => {
                self.with_objective((ctx.round, id), |o| {
                    o.last_change = last_change;
                    o.health = health;
                    o.logi = logi
                })?;
            }
            Stat::ObjectiveSupply { id, supply, fuel } => {
                self.with_objective((ctx.round, id), |o| {
                    o.supply = supply;
                    o.fuel = fuel
                })?;
            }
            Stat::Capture { id, by, side } => {
                let objective_name = self
                    .objectives
                    .get(&(ctx.round, id))?
                    .map(|o| o.name.to_string())
                    .unwrap_or_else(|| format!("{:?}", id));
                self.with_objective((ctx.round, id), |o| o.owner = side)?;
                // Track capture count per objective
                let cap_key = (ctx.round, id);
                let prev = self.objective_captures.get(&cap_key)?.unwrap_or(0);
                self.objective_captures.insert(&cap_key, &(prev + 1))?;
                // Record the event itself (who/what/when) -- objective_captures
                // above is just a running total with no attribution or timeline.
                let cid = CaptureId::new(&self.db)?;
                self.captures.insert(
                    &(ctx.round, cid),
                    &CaptureRecord { time, objective_name, side, by: by.clone() },
                )?;
                for ucid in by {
                    self.pilots.with_pilot_and_aggregates(
                        ucid,
                        ctx.round,
                        ctx.public,
                        |pilot| pilot.total.captures += 1,
                        |agg| agg.captures += 1,
                    )?
                }
            }
            Stat::Repair { id: _, by } => {
                self.pilots.with_pilot_and_aggregates(
                    by,
                    ctx.round,
                    ctx.public,
                    |pilot| pilot.total.repairs += 1,
                    |agg| agg.repairs += 1,
                )?;
            }
            Stat::SupplyTransfer { from: _, to: _, by } => {
                self.pilots.with_pilot_and_aggregates(
                    by,
                    ctx.round,
                    ctx.public,
                    |pilot| pilot.total.supply_transfers += 1,
                    |agg| agg.supply_transfers += 1,
                )?;
            }
            Stat::EquipmentInventory { id, item, amount } => {
                self.equipment
                    .fetch_and_update(&(ctx.round, id, item), |_| Some(amount))?;
            }
            Stat::LiquidInventory { id, item, amount } => {
                self.liquids
                    .fetch_and_update(&(ctx.round, id, item), |_| Some(amount))?;
            }
            Stat::Action { by, gid, action } => {
                self.pilots.with_pilot_and_aggregates(
                    by,
                    ctx.round,
                    ctx.public,
                    |p| p.total.actions += 1,
                    |a| a.actions += 1,
                )?;
                if let Some(gid) = gid {
                    self.with_group((ctx.round, gid), |group| {
                        group.kind = GroupKind::Action {
                            by,
                            name: action.clone(),
                        }
                    })?;
                }
            }
            Stat::DeployTroop { by, troop, gid } => {
                self.pilots.with_pilot_and_aggregates(
                    by,
                    ctx.round,
                    ctx.public,
                    |p| p.total.troops += 1,
                    |a| a.troops += 1,
                )?;
                // The group row is created later (from Stat::Unit once the units
                // actually spawn in DCS -- the deploy is queued), so tag it if
                // present but never fail the whole stat over a missing row.
                if let Err(e) = self.with_group((ctx.round, gid), |group| {
                    group.kind = GroupKind::Troop {
                        by,
                        name: troop.clone(),
                    }
                }) {
                    debug!("DeployTroop: group {gid:?} not tracked yet ({e})");
                }
            }
            Stat::DeployGroup {
                by,
                gid,
                deployable,
                aircraft,
                method,
            } => {
                // Idempotency guard -- see `deploy_seen`. One deployed group =
                // one (round, gid). A redelivered `Stat::DeployGroup` bails here
                // before bumping the counter or minting a second DeployId.
                if self.deploy_seen.get(&(ctx.round, gid))?.is_some() {
                    return Ok(());
                }
                self.pilots.with_pilot_and_aggregates(
                    by,
                    ctx.round,
                    ctx.public,
                    |p| p.total.deploys += 1,
                    |a| a.deploys += 1,
                )?;
                // See DeployTroop above -- the group row may not exist yet.
                // Recording the deploy (counter + log) must not depend on it.
                if let Err(e) = self.with_group((ctx.round, gid), |group| {
                    group.kind = GroupKind::Deployed {
                        by,
                        name: deployable.clone(),
                    }
                }) {
                    debug!("DeployGroup: group {gid:?} not tracked yet ({e})");
                }
                let did = DeployId::new(&self.db)?;
                self.deploy_seen.insert(&(ctx.round, gid), &did)?;
                self.deploys.insert(
                    &(by, ctx.round, did),
                    &DeployRecord {
                        time,
                        by,
                        deployable: deployable.to_string(),
                        aircraft: aircraft.map(|a| a.to_string()),
                        method: method.map(|m| m.to_string()),
                    },
                )?;
            }
            Stat::DeployFarp {
                by,
                oid,
                deployable: _,
            } => {
                self.pilots.with_pilot_and_aggregates(
                    by,
                    ctx.round,
                    ctx.public,
                    |p| p.total.farps += 1,
                    |a| a.farps += 1,
                )?;
                self.with_objective((ctx.round, oid), |o| o.by = Some(by))?;
            }
            Stat::Register {
                name,
                id,
                side,
                initial_points,
            } => {
                self.pilots.saw_pilot(id, name)?;
                self.pilots.with_pilot_round_info(id, ctx.round, |ri| {
                    ri.side = (time, side);
                    ri.points = initial_points;
                })?;
            }
            Stat::Sideswitch { id, side } => {
                self.pilots
                    .with_pilot_round_info(id, ctx.round, |ri| ri.side = (time, side))?;
            }
            Stat::Connect { id, addr, name } => {
                self.pilots.saw_pilot(id, name)?;
                self.pilots.with_pilot_round_info(id, ctx.round, |ri| {
                    ri.connected = Some((time, addr.clone()))
                })?;
            }
            Stat::Disconnect { id } => {
                self.pilots
                    .with_pilot_round_info(id, ctx.round, |ri| ri.connected = None)?;
            }
            Stat::Slot { id, slot, typ } => {
                self.pilots.with_pilot_round_info(id, ctx.round, |ri| {
                    ri.slot = Some(Slot {
                        time,
                        id: slot,
                        vehicle: typ.as_ref().map(|u| u.typ.clone()),
                        sortie: None,
                    })
                })?;
            }
            Stat::Deslot { id } => {
                self.pilots
                    .with_pilot_round_info(id, ctx.round, |ri| ri.slot = None)?;
                self.units.remove(&(ctx.round, EnId::Player(id)))?;
            }
            Stat::Unit {
                id,
                gid,
                owner,
                typ,
                pos,
            } => {
                self.units.fetch_and_update(&(ctx.round, id), |_| {
                    Some(Unit {
                        dead: false,
                        group: gid,
                        owner,
                        typ: typ.typ.clone(),
                        tags: typ.tags,
                        pos,
                    })
                })?;
                if let Some(gid) = gid {
                    self.groups.fetch_and_update(&(ctx.round, gid), |g| {
                        let mut g = g.unwrap_or_default();
                        g.owner = owner;
                        if !g.units.contains(&id) {
                            g.units.push(id);
                        }
                        Some(g)
                    })?;
                }
            }
            Stat::Position { id, pos } => {
                self.with_unit((ctx.round, id), |u| u.pos = pos)?;
            }
            Stat::GroupDeleted { id } => {
                if let Some(group) = self.groups.remove(&(ctx.round, id))? {
                    for uid in group.units {
                        self.units.remove(&(ctx.round, uid))?;
                    }
                }
            }
            Stat::Detected {
                id,
                detected,
                source,
            } => {
                self.detected.update_and_fetch(&(ctx.round, id), |d| {
                    let mut d = d.unwrap_or_default();
                    if detected {
                        d.insert(source);
                    } else {
                        d.remove(source);
                    }
                    if d.is_empty() {
                        None
                    } else {
                        Some(d)
                    }
                })?;
            }
            Stat::Takeoff { id } => {
                // Idempotency guard -- see `sortie_seen`. `Stat::Takeoff` carries
                // no timestamp of its own, so a redelivery replays with the same
                // `time` (the archive batch stamp); (round, pilot, takeoff millis)
                // is a stable content key. Bail before minting a second SortieId.
                let dedup_key = (ctx.round, id, time.timestamp_millis());
                if self.sortie_seen.get(&dedup_key)?.is_some() {
                    return Ok(());
                }
                let sid = SortieId::new(&self.db)?;
                let mut vehicle = None;
                self.pilots.with_pilot_round_info(id, ctx.round, |ri| {
                    if let Some(sl) = ri.slot.as_mut() {
                        sl.sortie = Some(sid);
                        vehicle = sl.vehicle.clone()
                    }
                })?;
                let vehicle = vehicle.ok_or_else(|| anyhow!("{id} takeoff without slotting"))?;
                // Only commit the dedup key once the sortie is definitely going
                // to be recorded -- a "takeoff without slotting" bail above may
                // just be a replay running ahead of its `Stat::Slot`, and must
                // stay retryable.
                self.sortie_seen.insert(&dedup_key, &sid)?;
                // Track sortie count per aircraft type
                let ac_key = (ctx.round, vehicle.to_string());
                let (prev_cnt, prev_hrs) = self.aircraft_sorties.get(&ac_key)?.unwrap_or((0, 0.0));
                self.aircraft_sorties.insert(&ac_key, &(prev_cnt + 1, prev_hrs))?;
                self.pilots.sortie.insert(
                    &(id, ctx.round, sid),
                    &Sortie {
                        takeoff: time,
                        land: None,
                        vehicle,
                    },
                )?;
            }
            Stat::Land { id } => {
                let mut sid: Option<SortieId> = None;
                self.pilots.with_pilot_round_info(id, ctx.round, |ri| {
                    if let Some(sl) = ri.slot.as_mut() {
                        sid = sl.sortie.take();
                    }
                })?;
                // No sortie parked in the slot: either a genuinely orphaned
                // landing, or -- now that `Stat::Takeoff` is deduped -- a
                // redelivered `Stat::Land` whose real partner already consumed
                // the slot. Neither is actionable and neither should credit
                // hours, so drop it quietly rather than failing the stat.
                let Some(sid) = sid else {
                    debug!("{id} landed with no active sortie -- orphan or replay, ignoring");
                    return Ok(());
                };
                // Belt and braces: if the parked sortie is somehow already
                // landed, a redelivery is in play -- don't credit hours twice.
                if self.pilots.sortie.get(&(id, ctx.round, sid))?.is_some_and(|s| s.land.is_some()) {
                    return Ok(());
                }
                // Add flight hours to aircraft sortie totals
                let mut vehicle_str: Option<std::string::String> = None;
                self.pilots.with_sortie((id, ctx.round, sid), |s| {
                    s.land = Some(time);
                    vehicle_str = Some(s.vehicle.to_string());
                })?;
                if let Some(v) = vehicle_str {
                    let hours = (time - self.pilots.sortie.get(&(id, ctx.round, sid))?
                        .map(|s| s.takeoff).unwrap_or(time))
                        .num_seconds() as f32 / 3600.0;
                    let ac_key = (ctx.round, v);
                    let (cnt, prev_hrs) = self.aircraft_sorties.get(&ac_key)?.unwrap_or((0, 0.0));
                    self.aircraft_sorties.insert(&ac_key, &(cnt, prev_hrs + hours))?;
                    // Also credit hours to pilot total and per-round aggregates
                    self.pilots.with_pilot_and_aggregates(
                        id,
                        ctx.round,
                        ctx.public,
                        |p| p.total.hours += hours,
                        |a| a.hours += hours,
                    )?;
                }
            }
            Stat::Life { id, lives } => {
                self.pilots.with_pilot_round_info(id, ctx.round, |ri| {
                    ri.lives.clear();
                    ri.lives
                        .extend(lives.into_iter().map(|(lt, (dt, n))| (*lt, *dt, *n)));
                })?;
            }
            Stat::Kill(dead) => self.record_kill(ctx, dead)?,
            Stat::Points {
                id,
                points,
                reason: _,
            } => {
                self.pilots
                    .with_pilot_round_info(id, ctx.round, |ri| ri.points += points)?;
            }
            Stat::PointsTransfer { from, to, points } => {
                self.pilots
                    .with_pilot_round_info(from, ctx.round, |ri| ri.points -= points as i32)?;
                self.pilots.with_pilot_and_aggregates(
                    from,
                    ctx.round,
                    ctx.public,
                    |p| p.total.donated_points += points,
                    |a| a.donated_points += points,
                )?;
                self.pilots
                    .with_pilot_round_info(to, ctx.round, |ri| ri.points += points as i32)?;
            }
            Stat::Bind { id, token } => {
                let token = Uuid::from_str(&token)?;
                let mut remove = None;
                self.pilots.with_pilot(id, |p| {
                    if p.token.is_full() {
                        remove = p.token.pop_at(0);
                    }
                    p.token.push(token)
                })?;
                self.pilots.by_token.insert(&token, &id)?;
                if let Some(token) = remove {
                    self.pilots.by_token.remove(&token)?;
                }
            }
            Stat::PointsTransferToObjective { from: _, to: _, points: _ } => {
                // Not currently tracked in database
            }
            Stat::Weather { temp_c, wind_speed_kts, wind_from_deg, cloud_base_m, qnh_hpa, cloud_density, visibility_m } => {
                if let Ok(mut w) = inst.latest_weather.write() {
                    *w = Some(WeatherSnapshot {
                        temp_c,
                        wind_speed_kts,
                        wind_from_deg,
                        cloud_base_m,
                        qnh_hpa,
                        cloud_density,
                        visibility_m,
                    });
                }
            }
            Stat::ConvoyDestroyed { .. }
            | Stat::CampaignEvent { .. }
            | Stat::PilotXp { .. }
            | Stat::AirRouteDelivered { .. }
            | Stat::AirRouteDestroyed { .. }
            | Stat::SeaRouteDelivered { .. }
            | Stat::SeaRouteDestroyed { .. }
            | Stat::GciPicture(_) => {
                // Future: track in dedicated tables
            }
        };
        self.seq
            .insert(&(ctx.sortie.clone(), ctx.round), &time)?;
        ctx.seq = time;
        Ok(())
    }

    // ── Auth session methods ─────────────────────────────────────────

    pub(crate) fn create_session(&self, id: Uuid, data: SessionData) -> Result<()> {
        self.auth_sessions.insert(&id, &data)?;
        Ok(())
    }

    pub(crate) fn get_session(&self, id: Uuid) -> Result<Option<SessionData>> {
        match self.auth_sessions.get(&id)? {
            None => Ok(None),
            Some(s) if s.expires < Utc::now() => {
                let _ = self.auth_sessions.remove(&id);
                Ok(None)
            }
            Some(s) => Ok(Some(s)),
        }
    }

    pub(crate) fn delete_session(&self, id: Uuid) -> Result<()> {
        self.auth_sessions.remove(&id)?;
        Ok(())
    }

    pub(crate) fn store_oauth_state(&self, state: Uuid, return_to: Option<std::string::String>) -> Result<()> {
        let expires = Utc::now() + chrono::Duration::minutes(10);
        self.auth_states.insert(&state, &OAuthState { expires, return_to })?;
        Ok(())
    }

    /// Consumes the one-time state, returning the stored `return_to` (which
    /// may itself be `None`, if the login started without one) if it was
    /// valid and unexpired -- outer `None` means reject the callback outright.
    pub(crate) fn take_oauth_state(&self, state: Uuid) -> Result<Option<Option<std::string::String>>> {
        match self.auth_states.remove(&state)? {
            None => Ok(None),
            Some(s) if s.expires > Utc::now() => Ok(Some(s.return_to)),
            Some(_) => Ok(None),
        }
    }

    pub(crate) fn list_sessions(&self) -> Result<Vec<(Uuid, SessionData)>> {
        let now = Utc::now();
        let mut out = Vec::new();
        for item in self.auth_sessions.iter() {
            let (id, data) = item?;
            if data.expires > now {
                out.push((id, data));
            }
        }
        Ok(out)
    }

    // ── Trail point methods ──────────────────────────────────────────

    pub(crate) fn append_trail_point(
        &self,
        round_id: RoundId,
        unit_id: &std::string::String,
        ts: i64,
        lat: f64,
        lon: f64,
        alt: f64,
        hdg: f64,
    ) -> Result<()> {
        self.trail_points.insert(&(round_id, unit_id.clone(), ts), &(lat, lon, alt, hdg))?;
        Ok(())
    }

    pub(crate) fn get_trail_points(&self, round_id: RoundId) -> Result<Vec<TrailPoint>> {
        // Keep last 30 minutes of trail history
        let cutoff = Utc::now().timestamp() - 1800;
        let mut points = Vec::new();
        for item in self.trail_points.range(
            (round_id, std::string::String::new(), cutoff)..,
        )? {
            let ((rid, unit_id, ts), (lat, lon, alt, hdg)) = item?;
            if rid != round_id {
                break;
            }
            if ts >= cutoff {
                points.push(TrailPoint { unit_id, lat, lon, alt, hdg, ts });
            }
        }
        Ok(points)
    }

    /// Clear only the `session` tree (per-round Cfg snapshot + perf history),
    /// leaving rounds/kills/objectives/pilots untouched. Use this to recover
    /// from old `Session` records that predate a bincode-incompatible change
    /// to `Cfg`/`Deployable` (mid-struct field insertions break positional
    /// decoding for anything serialized under the old layout, surfacing as
    /// "string is not valid utf8" errors from /api/admin/perf and
    /// /api/admin/banned, which both read the latest session's Cfg).
    pub(crate) fn clear_stale_sessions(&self) -> Result<()> {
        self.session.clear()?;
        Ok(())
    }

    /// Wipe campaign data for one DCS server instance -- rounds, kills,
    /// objectives, pilot stats, trails, captures, sorties, weather -- while
    /// preserving auth sessions and Discord links so admins remain logged in
    /// and pilot linking is not lost.
    ///
    /// With a single instance configured this is the whole database (the
    /// original behaviour, and much cheaper than a per-round sweep). With
    /// several, only the named instance's rounds are purged, so resetting one
    /// server's campaign leaves the other's history intact.
    pub(crate) fn reset_campaign_data_for(&self, inst: &InstanceState) -> Result<()> {
        if self.0.instances.is_single() {
            return self.reset_campaign_data(inst);
        }
        let rounds = self.rounds_of(&inst.id)?;
        warn!(
            "[{}] resetting campaign: purging {} round(s), other instances untouched",
            inst.id,
            rounds.len()
        );
        self.purge_rounds(&rounds)?;
        self.intel_purge_all_for(&rounds)?;
        if let Ok(mut w) = inst.latest_weather.write() {
            *w = None;
        }
        Ok(())
    }

    /// Delete every trace of `rounds` from the round-scoped trees, then rebuild
    /// each pilot's lifetime totals by re-summing the aggregates that survived.
    /// `Aggregates` is a plain bag of additive counters, so a re-sum is exact.
    fn purge_rounds(&self, rounds: &HashSet<RoundId>) -> Result<()> {
        if rounds.is_empty() {
            return Ok(());
        }
        // Trees keyed (RoundId, ..) -- scan the round prefix directly.
        macro_rules! purge_prefixed {
            ($tree:expr) => {
                for round in rounds.iter() {
                    let keys: Vec<_> = $tree
                        .scan_prefix(round)?
                        .filter_map(|r| r.ok())
                        .map(|(k, _)| k)
                        .collect();
                    for k in keys {
                        $tree.remove(&k)?;
                    }
                }
            };
        }
        purge_prefixed!(self.session);
        purge_prefixed!(self.kill_seen);
        purge_prefixed!(self.sortie_seen);
        purge_prefixed!(self.deploy_seen);
        purge_prefixed!(self.units);
        purge_prefixed!(self.groups);
        purge_prefixed!(self.detected);
        purge_prefixed!(self.objectives);
        purge_prefixed!(self.equipment);
        purge_prefixed!(self.liquids);
        purge_prefixed!(self.trail_points);
        purge_prefixed!(self.objective_captures);
        purge_prefixed!(self.captures);
        purge_prefixed!(self.aircraft_sorties);

        // Trees where RoundId is NOT the leading key component -- full scan.
        let mut dead_kill_ids: Vec<KillId> = Vec::new();
        let kill_keys: Vec<_> = self
            .kills
            .iter()
            .filter_map(|r| r.ok())
            .map(|(k, _)| k)
            .filter(|(_, rid, _)| rounds.contains(rid))
            .collect();
        for k in kill_keys {
            dead_kill_ids.push(k.2);
            self.kills.remove(&k)?;
        }
        for kid in dead_kill_ids {
            self.shared_kills.remove(&kid)?;
        }
        let deploy_keys: Vec<_> = self
            .deploys
            .iter()
            .filter_map(|r| r.ok())
            .map(|(k, _)| k)
            .filter(|(_, rid, _)| rounds.contains(rid))
            .collect();
        for k in deploy_keys {
            self.deploys.remove(&k)?;
        }
        // Before the aggregate rows go: they are what tells us how much of
        // each pilot's lifetime total came from these rounds.
        self.subtract_rounds_from_totals(rounds)?;
        let agg_keys: Vec<_> = self
            .pilots
            .aggregates
            .iter()
            .filter_map(|r| r.ok())
            .map(|(k, _)| k)
            .filter(|(_, _, rid)| rounds.contains(rid))
            .collect();
        for k in agg_keys {
            self.pilots.aggregates.remove(&k)?;
        }
        let sortie_keys: Vec<_> = self
            .pilots
            .sortie
            .iter()
            .filter_map(|r| r.ok())
            .map(|(k, _)| k)
            .filter(|(_, rid, _)| rounds.contains(rid))
            .collect();
        for k in sortie_keys {
            self.pilots.sortie.remove(&k)?;
        }
        let ri_keys: Vec<_> = self
            .pilots
            .round_info
            .iter()
            .filter_map(|r| r.ok())
            .map(|(k, _)| k)
            .filter(|(_, rid)| rounds.contains(rid))
            .collect();
        for k in ri_keys {
            self.pilots.round_info.remove(&k)?;
        }

        // Round bookkeeping itself.
        let seq_keys: Vec<_> = self
            .seq
            .iter()
            .filter_map(|r| r.ok())
            .map(|(k, _)| k)
            .filter(|(_, rid)| rounds.contains(rid))
            .collect();
        for k in seq_keys {
            self.seq.remove(&k)?;
        }
        let round_keys: Vec<_> = self
            .round
            .iter()
            .filter_map(|r| r.ok())
            .map(|(k, _)| k)
            .filter(|(_, rid)| rounds.contains(rid))
            .collect();
        for k in round_keys {
            self.round.remove(&k)?;
        }
        for rid in rounds.iter() {
            self.round_instance.remove(rid)?;
        }

        Ok(())
    }

    /// Deduct the purged rounds' contribution from each pilot's lifetime
    /// total, after a partial (per-instance) purge.
    ///
    /// Deliberately a subtraction and not a recompute-from-scratch: an
    /// `aggregates` row only exists for stats earned while the pilot was in a
    /// slot with a known vehicle, so re-summing the survivors would silently
    /// shrink every pilot's record by whatever was earned outside one. All
    /// counters saturate at zero, so an under-recorded round can leave a small
    /// residual -- much the better failure than deleting real history.
    fn subtract_rounds_from_totals(&self, rounds: &HashSet<RoundId>) -> Result<()> {
        let mut deltas: HashMap<Ucid, Aggregates> = HashMap::new();
        for r in self.pilots.aggregates.iter() {
            let ((ucid, _, rid), a) = r?;
            if !rounds.contains(&rid) {
                continue;
            }
            let t = deltas.entry(ucid).or_default();
            t.air_kills += a.air_kills;
            t.ground_kills += a.ground_kills;
            t.captures += a.captures;
            t.repairs += a.repairs;
            t.supply_transfers += a.supply_transfers;
            t.troops += a.troops;
            t.farps += a.farps;
            t.deploys += a.deploys;
            t.actions += a.actions;
            t.deaths += a.deaths;
            t.hours += a.hours;
            t.donated_points += a.donated_points;
        }
        for (ucid, d) in deltas {
            self.pilots.with_pilot(ucid, |p| {
                let t = &mut p.total;
                t.air_kills = t.air_kills.saturating_sub(d.air_kills);
                t.ground_kills = t.ground_kills.saturating_sub(d.ground_kills);
                t.captures = t.captures.saturating_sub(d.captures);
                t.repairs = t.repairs.saturating_sub(d.repairs);
                t.supply_transfers = t.supply_transfers.saturating_sub(d.supply_transfers);
                t.troops = t.troops.saturating_sub(d.troops);
                t.farps = t.farps.saturating_sub(d.farps);
                t.deploys = t.deploys.saturating_sub(d.deploys);
                t.actions = t.actions.saturating_sub(d.actions);
                t.deaths = t.deaths.saturating_sub(d.deaths);
                t.hours = (t.hours - d.hours).max(0.0);
                t.donated_points = t.donated_points.saturating_sub(d.donated_points);
            })?;
        }
        Ok(())
    }

    /// The whole-database campaign wipe. Only reachable when this bfdb fronts a
    /// single instance -- see `reset_campaign_data_for`.
    fn reset_campaign_data(&self, inst: &InstanceState) -> Result<()> {
        // Pilot stat trees
        self.pilots.pilots.clear()?;
        self.pilots.aggregates.clear()?;
        self.pilots.by_name.clear()?;
        self.pilots.sortie.clear()?;
        self.pilots.round_info.clear()?;
        // Round / mission trees
        self.seq.clear()?;
        self.round.clear()?;
        self.session.clear()?;
        // Combat trees
        self.kills.clear()?;
        self.shared_kills.clear()?;
        self.units.clear()?;
        self.groups.clear()?;
        self.detected.clear()?;
        // Objectives
        self.objectives.clear()?;
        self.equipment.clear()?;
        self.liquids.clear()?;
        // Captures & sorties
        self.objective_captures.clear()?;
        self.aircraft_sorties.clear()?;
        // Idempotency keys -- meaningless once their rounds are gone
        self.kill_seen.clear()?;
        self.sortie_seen.clear()?;
        self.deploy_seen.clear()?;
        // Trails & weather
        self.trail_points.clear()?;
        self.round_instance.clear()?;
        if let Ok(mut w) = inst.latest_weather.write() { *w = None; }
        // Recon intel (TARPS) -- per-round picture, gone on reset (incl. any
        // on-disk photo files)
        self.intel_purge_all()?;
        // auth_sessions, auth_states → preserved
        Ok(())
    }

    /// One-off maintenance for the `--rebuild-stats` flag. Wipes every tree that
    /// is *derived* from replaying the stats archive -- rounds, sessions, pilot
    /// stats, kills, sorties, deploys, objectives, trails -- and rewinds the
    /// replay cursor so the next startup re-ingests the whole archive from the
    /// beginning. Unlike `reset_campaign_data` this is not a campaign wipe: it
    /// exists to repair accumulated damage from redelivered stats (phantom
    /// duplicate sorties/kills/deploys and the inflated counters that came with
    /// them) by rebuilding from the source of truth with the idempotency guards
    /// now in place.
    ///
    /// Preserves everything the archive does NOT own: auth sessions, Discord
    /// links (`pilots.by_token`), the admin-managed ban list, wiki content, and
    /// recon intel (TARPS) photos and markup.
    ///
    /// Requires the full historical archive to still be present under
    /// `--stats-dir`; if older segments have been pruned, history before the
    /// oldest surviving segment will not come back.
    pub(crate) fn rebuild_stats_from_archive(&self) -> Result<()> {
        self.wipe_stats_derived_trees()?;
        // Rewind the replay so a normal restart re-ingests from the top --
        // both the netidx archive cursor and the stats.jsonl byte offset, for
        // every instance (a rebuild is inherently whole-database: the derived
        // trees are shared).
        self.replay_cursor.clear()?;
        self.jsonl_cursor.clear()?;
        for st in self.0.states.values() {
            *st.current_sortie.lock().unwrap() = None;
        }
        Ok(())
    }

    /// Clear every tree that is *derived* from replaying the stats stream
    /// (rounds, sessions, pilot stats, kills, sorties, deploys, objectives,
    /// trails). Does NOT touch replay position or anything the stream doesn't
    /// own (auth, Discord links, bans, wiki, recon intel). Shared by the
    /// `--rebuild-stats` flag and the live `/api/admin/rebuild-stats` path.
    fn wipe_stats_derived_trees(&self) -> Result<()> {
        self.pilots.pilots.clear()?;
        self.pilots.aggregates.clear()?;
        self.pilots.by_name.clear()?;
        self.pilots.sortie.clear()?;
        self.pilots.round_info.clear()?;
        self.seq.clear()?;
        self.round.clear()?;
        self.session.clear()?;
        self.kills.clear()?;
        self.shared_kills.clear()?;
        self.kill_seen.clear()?;
        self.units.clear()?;
        self.groups.clear()?;
        self.detected.clear()?;
        self.objectives.clear()?;
        self.equipment.clear()?;
        self.liquids.clear()?;
        self.objective_captures.clear()?;
        self.captures.clear()?;
        self.aircraft_sorties.clear()?;
        self.sortie_seen.clear()?;
        self.deploys.clear()?;
        self.deploy_seen.clear()?;
        self.trail_points.clear()?;
        self.round_instance.clear()?;
        for st in self.0.states.values() {
            if let Ok(mut w) = st.latest_weather.write() {
                *w = None;
            }
        }
        Ok(())
    }

    /// Queue an in-process stats rebuild: the JSONL reader loop wipes the
    /// derived trees and re-ingests `stats.jsonl` from offset 0 on its next
    /// tick. Returns an error in netidx-archive mode (no live reset there --
    /// use the `--rebuild-stats` flag). See `jsonl_reset`.
    ///
    /// The derived trees are shared across instances, so a rebuild rewinds and
    /// re-ingests *every* instance that reads a JSONL -- rebuilding only one
    /// would delete the others' data with nothing to replay it back from.
    pub(crate) fn request_jsonl_rebuild(&self) -> Result<()> {
        let readers: Vec<_> = self
            .0
            .states
            .values()
            .filter(|st| st.stats_jsonl.is_some())
            .collect();
        if readers.is_empty() {
            bail!("no instance is reading from a stats.jsonl -- use the --rebuild-stats flag offline instead");
        }
        for st in readers {
            self.jsonl_cursor.insert(&st.id.to_string(), &0u64)?;
            st.jsonl_reset.store(true, Ordering::SeqCst);
        }
        Ok(())
    }

    /// One-off maintenance for `--merge-rounds <sortie>`: collapse every round
    /// recorded under `sortie` into a single round, re-keying every
    /// round-scoped tree onto the earliest round's id.
    ///
    /// Repairs the fragmentation left by an older bug: bfdb used to close the
    /// live round on every restart whenever the sortie was named the same as
    /// the last path segment of `--base` (the documented default is
    /// netidx_base `/local/fowl/campaign` + sortie `campaign`), and the next
    /// `SessionStart` then forked a fresh round -- so one long campaign showed
    /// up as dozens of near-identical rounds in the dashboard. The startup
    /// sweep is now name-blind (see `StatsDb::new`), so this only has to clean
    /// up the rounds already on disk.
    ///
    /// Merge rules: counters that were split across the forks (per-pilot /
    /// per-vehicle aggregates, objective capture counts, aircraft sortie
    /// counts) are summed. The per-pilot round snapshot (points / side / slot
    /// / lives) keeps the newest fork's value. Everything else is re-keyed
    /// as-is; on a key collision the newest fork wins.
    ///
    /// Leaves the replay cursor, auth, Discord links, bans, wiki, weather and
    /// the stats archive itself untouched. Idempotent -- a sortie that already
    /// has a single round is a no-op.
    pub(crate) fn merge_rounds(&self, sortie: &str) -> Result<std::string::String> {
        let scenario = Scenario::from(sortie);

        let mut rounds: Vec<(RoundId, Round)> = self
            .round
            .scan_prefix(&scenario)?
            .collect::<Result<Vec<((Scenario, RoundId), Round)>>>()?
            .into_iter()
            .map(|((_, rid), rd)| (rid, rd))
            .collect();
        if rounds.is_empty() {
            bail!("no rounds recorded under sortie {sortie:?} -- check the exact name in GET /api/rounds");
        }
        rounds.sort_by(|a, b| a.1.start.cmp(&b.1.start).then(a.0.cmp(&b.0)));
        let canonical = rounds[0].0;
        if rounds.len() == 1 {
            return Ok(format!(
                "sortie {sortie:?} already has a single round (#{}); nothing to merge",
                canonical.0
            ));
        }
        let fork_ids: std::collections::HashSet<RoundId> =
            rounds.iter().skip(1).map(|(rid, _)| *rid).collect();
        let n_forks = fork_ids.len();

        // ── round / seq: collapse to one ────────────────────────────────
        let start = rounds.iter().map(|(_, r)| r.start).min().unwrap();
        let any_open = rounds.iter().any(|(_, r)| r.end.is_none());
        let end = if any_open {
            None
        } else {
            rounds.iter().filter_map(|(_, r)| r.end).max()
        };
        let winner = rounds.iter().rev().find_map(|(_, r)| r.winner);
        for (rid, _) in &rounds {
            self.round.remove(&(scenario.clone(), *rid))?;
            self.seq.remove(&(scenario.clone(), *rid))?;
        }
        self.round
            .insert(&(scenario.clone(), canonical), &Round { start, end, winner })?;
        self.seq.insert(&(scenario.clone(), canonical), &start)?;

        let moved = self.collapse_round_data(canonical, &fork_ids)?;
        Ok(format!(
            "merged {n_forks} fork round(s) into #{} for sortie {sortie:?} \
             ({moved} round-scoped rows re-keyed). Restart bfdb normally.",
            canonical.0
        ))
    }

    /// Re-key every round-scoped data tree so rows whose `RoundId` is in
    /// `fork_ids` move onto `canonical`. Summable counters (per-pilot/vehicle
    /// aggregates, objective capture counts, aircraft sortie counts) are
    /// summed; the per-pilot round snapshot keeps the newest fork's value;
    /// everything else is re-keyed as-is with the newest fork winning a key
    /// collision. Does NOT touch `round` / `seq` -- the caller owns those.
    /// Returns the number of rows moved.
    fn collapse_round_data(
        &self,
        canonical: RoundId,
        fork_ids: &std::collections::HashSet<RoundId>,
    ) -> Result<usize> {
        let touched = |rid: RoundId| rid == canonical || fork_ids.contains(&rid);
        let mut moved = 0usize;

        // ── pilots.aggregates (Ucid, Vehicle, RoundId): sum ─────────────
        for ((u, v, rid), agg) in self
            .pilots
            .aggregates
            .iter()
            .collect::<Result<Vec<((Ucid, Vehicle, RoundId), Aggregates)>>>()?
        {
            if !fork_ids.contains(&rid) {
                continue;
            }
            self.pilots.aggregates.remove(&(u.clone(), v.clone(), rid))?;
            self.pilots
                .aggregates
                .fetch_and_update(&(u.clone(), v.clone(), canonical), |cur| {
                    let mut c = cur.unwrap_or_default();
                    c.air_kills += agg.air_kills;
                    c.ground_kills += agg.ground_kills;
                    c.captures += agg.captures;
                    c.repairs += agg.repairs;
                    c.supply_transfers += agg.supply_transfers;
                    c.troops += agg.troops;
                    c.farps += agg.farps;
                    c.deploys += agg.deploys;
                    c.actions += agg.actions;
                    c.deaths += agg.deaths;
                    c.hours += agg.hours;
                    c.donated_points += agg.donated_points;
                    Some(c)
                })?;
            moved += 1;
        }

        // ── pilots.round_info (Ucid, RoundId): newest fork wins per pilot ─
        {
            let all = self
                .pilots
                .round_info
                .iter()
                .collect::<Result<Vec<((Ucid, RoundId), PilotRoundInfo)>>>()?;
            let mut newest: std::collections::HashMap<Ucid, (RoundId, PilotRoundInfo)> =
                std::collections::HashMap::new();
            for ((u, rid), ri) in all {
                if !touched(rid) {
                    continue;
                }
                self.pilots.round_info.remove(&(u.clone(), rid))?;
                moved += 1;
                match newest.get(&u) {
                    Some((best, _)) if *best >= rid => {}
                    _ => {
                        newest.insert(u, (rid, ri));
                    }
                }
            }
            for (u, (_, ri)) in newest {
                self.pilots.round_info.insert(&(u, canonical), &ri)?;
            }
        }

        // ── pilots.sortie (Ucid, RoundId, SortieId): re-key (id unique) ──
        for ((u, rid, sid), s) in self
            .pilots
            .sortie
            .iter()
            .collect::<Result<Vec<((Ucid, RoundId, SortieId), Sortie)>>>()?
        {
            if !fork_ids.contains(&rid) {
                continue;
            }
            self.pilots.sortie.remove(&(u.clone(), rid, sid))?;
            self.pilots.sortie.insert(&(u, canonical, sid), &s)?;
            moved += 1;
        }

        // ── kills (EnId, RoundId, KillId): re-key (id unique) ───────────
        for ((en, rid, kid), dead) in self
            .kills
            .iter()
            .collect::<Result<Vec<((EnId, RoundId, KillId), Dead)>>>()?
        {
            if !fork_ids.contains(&rid) {
                continue;
            }
            self.kills.remove(&(en, rid, kid))?;
            self.kills.insert(&(en, canonical, kid), &dead)?;
            moved += 1;
        }

        // ── deploys (Ucid, RoundId, DeployId): re-key (id unique) ───────
        for ((u, rid, did), rec) in self
            .deploys
            .iter()
            .collect::<Result<Vec<((Ucid, RoundId, DeployId), DeployRecord)>>>()?
        {
            if !fork_ids.contains(&rid) {
                continue;
            }
            self.deploys.remove(&(u.clone(), rid, did))?;
            self.deploys.insert(&(u, canonical, did), &rec)?;
            moved += 1;
        }

        // ── objective_captures (RoundId, ObjectiveId): sum ─────────────
        for ((rid, oid), count) in self
            .objective_captures
            .iter()
            .collect::<Result<Vec<((RoundId, ObjectiveId), u32)>>>()?
        {
            if !fork_ids.contains(&rid) {
                continue;
            }
            self.objective_captures.remove(&(rid, oid))?;
            self.objective_captures.fetch_and_update(&(canonical, oid), |cur| {
                Some(cur.unwrap_or(0) + count)
            })?;
            moved += 1;
        }

        // ── aircraft_sorties (RoundId, type): sum (count, hours) ───────
        for ((rid, ty), (cnt, hrs)) in self
            .aircraft_sorties
            .iter()
            .collect::<Result<Vec<((RoundId, std::string::String), (u32, f32))>>>()?
        {
            if !fork_ids.contains(&rid) {
                continue;
            }
            self.aircraft_sorties.remove(&(rid, ty.clone()))?;
            self.aircraft_sorties.fetch_and_update(&(canonical, ty.clone()), |cur| {
                let (c, h) = cur.unwrap_or((0, 0.0));
                Some((c + cnt, h + hrs))
            })?;
            moved += 1;
        }

        // ── intel_captures / intel_markup: re-key + patch the value ────
        for ((rid, id), mut cap) in self
            .intel_captures
            .iter()
            .collect::<Result<Vec<((RoundId, Uuid), IntelCapture)>>>()?
        {
            if !fork_ids.contains(&rid) {
                continue;
            }
            self.intel_captures.remove(&(rid, id))?;
            cap.round = canonical;
            self.intel_captures.insert(&(canonical, id), &cap)?;
            moved += 1;
        }
        for ((rid, id), mut mk) in self
            .intel_markup
            .iter()
            .collect::<Result<Vec<((RoundId, Uuid), IntelMarkup)>>>()?
        {
            if !fork_ids.contains(&rid) {
                continue;
            }
            self.intel_markup.remove(&(rid, id))?;
            mk.round = canonical;
            self.intel_markup.insert(&(canonical, id), &mk)?;
            moved += 1;
        }

        // ── remaining (RoundId, …)-keyed trees: re-key, newest fork wins
        //    on collision (rows come back RoundId-ascending, so the highest
        //    fork id is written last) ───────────────────────────────────
        moved += rekey_round_first(&self.session, canonical, fork_ids)?;
        moved += rekey_round_first(&self.deploy_seen, canonical, fork_ids)?;
        moved += rekey_round_first(&self.units, canonical, fork_ids)?;
        moved += rekey_round_first(&self.groups, canonical, fork_ids)?;
        moved += rekey_round_first(&self.detected, canonical, fork_ids)?;
        moved += rekey_round_first(&self.objectives, canonical, fork_ids)?;
        moved += rekey_round_first(&self.captures, canonical, fork_ids)?;
        moved += rekey_round_first3(&self.kill_seen, canonical, fork_ids)?;
        moved += rekey_round_first3(&self.sortie_seen, canonical, fork_ids)?;
        moved += rekey_round_first3(&self.equipment, canonical, fork_ids)?;
        moved += rekey_round_first3(&self.liquids, canonical, fork_ids)?;
        moved += rekey_round_first3(&self.trail_points, canonical, fork_ids)?;

        Ok(moved)
    }

    /// One-off maintenance: collapse **every** round id referenced anywhere in
    /// the stats DB into a single round. Unlike `merge_rounds`, this doesn't
    /// need a sortie name and also sweeps up "orphan" round ids that have kill
    /// / sortie / deploy rows but no `round`-tree entry at all (which is how
    /// the fork bug's later rounds show up -- "Round 48000000" with no
    /// scenario in the dashboard). Same merge rules as `merge_rounds`.
    ///
    /// `dry_run` reports what it would do and changes nothing. Safe to run
    /// against a live bfdb (used by `POST /api/admin/merge-rounds`), though
    /// quietest right after a mission restart.
    pub(crate) fn merge_all_rounds(&self, dry_run: bool) -> Result<std::string::String> {
        use std::collections::HashSet;
        let round_entries: Vec<(Scenario, RoundId, Round)> = self
            .round
            .iter()
            .collect::<Result<Vec<((Scenario, RoundId), Round)>>>()?
            .into_iter()
            .map(|((s, rid), rd)| (s, rid, rd))
            .collect();

        let round_tree_ids: HashSet<RoundId> =
            round_entries.iter().map(|(_, rid, _)| *rid).collect();
        let mut ids: HashSet<RoundId> = round_tree_ids.clone();
        for r in self.seq.iter() { ids.insert(r?.0 .1); }
        for r in self.pilots.aggregates.iter() { ids.insert(r?.0 .2); }
        for r in self.pilots.sortie.iter() { ids.insert(r?.0 .1); }
        for r in self.pilots.round_info.iter() { ids.insert(r?.0 .1); }
        for r in self.session.iter() { ids.insert(r?.0 .0); }
        for r in self.kills.iter() { ids.insert(r?.0 .1); }
        for r in self.kill_seen.iter() { ids.insert(r?.0 .0); }
        for r in self.sortie_seen.iter() { ids.insert(r?.0 .0); }
        for r in self.deploy_seen.iter() { ids.insert(r?.0 .0); }
        for r in self.units.iter() { ids.insert(r?.0 .0); }
        for r in self.groups.iter() { ids.insert(r?.0 .0); }
        for r in self.detected.iter() { ids.insert(r?.0 .0); }
        for r in self.objectives.iter() { ids.insert(r?.0 .0); }
        for r in self.equipment.iter() { ids.insert(r?.0 .0); }
        for r in self.liquids.iter() { ids.insert(r?.0 .0); }
        for r in self.trail_points.iter() { ids.insert(r?.0 .0); }
        for r in self.objective_captures.iter() { ids.insert(r?.0 .0); }
        for r in self.captures.iter() { ids.insert(r?.0 .0); }
        for r in self.deploys.iter() { ids.insert(r?.0 .1); }
        for r in self.aircraft_sorties.iter() { ids.insert(r?.0 .0); }
        for r in self.intel_captures.iter() { ids.insert(r?.0 .0); }
        for r in self.intel_markup.iter() { ids.insert(r?.0 .0); }

        if ids.len() <= 1 {
            return Ok(format!(
                "nothing to merge -- {} round id(s) referenced on disk",
                ids.len()
            ));
        }

        // Canonical: prefer a still-open `round` entry (newest by start), then
        // the earliest-start entry, then just the lowest id seen anywhere.
        let canonical = round_entries
            .iter()
            .filter(|(_, _, r)| r.end.is_none())
            .max_by_key(|(_, _, r)| r.start)
            .map(|(_, rid, _)| *rid)
            .or_else(|| {
                round_entries
                    .iter()
                    .min_by_key(|(_, _, r)| r.start)
                    .map(|(_, rid, _)| *rid)
            })
            .unwrap_or_else(|| *ids.iter().min().unwrap());

        // Surviving scenario name: the canonical's own, else the most common
        // non-empty one, else "campaign".
        let scenario = round_entries
            .iter()
            .find(|(_, rid, _)| *rid == canonical)
            .map(|(s, _, _)| s.clone())
            .or_else(|| {
                let mut counts: std::collections::HashMap<Scenario, usize> =
                    std::collections::HashMap::new();
                for (s, _, _) in &round_entries {
                    if !s.as_str().is_empty() {
                        *counts.entry(s.clone()).or_insert(0) += 1;
                    }
                }
                counts.into_iter().max_by_key(|(_, n)| *n).map(|(s, _)| s)
            })
            .unwrap_or_else(|| Scenario::from("campaign"));

        let mut fork_ids: HashSet<RoundId> = ids.clone();
        fork_ids.remove(&canonical);

        let canon_entry = round_entries.iter().find(|(_, rid, _)| *rid == canonical);
        let start = round_entries
            .iter()
            .map(|(_, _, r)| r.start)
            .min()
            .unwrap_or_else(Utc::now);
        // Keep the campaign open unless the canonical round is itself a
        // genuinely-ended round.
        let end = canon_entry.and_then(|(_, _, r)| r.end);
        let winner = canon_entry.and_then(|(_, _, r)| r.winner);

        if dry_run {
            let mut sorted: Vec<u64> = ids.iter().map(|r| r.0).collect();
            sorted.sort_unstable();
            let orphans = ids.iter().filter(|rid| !round_tree_ids.contains(rid)).count();
            return Ok(format!(
                "DRY RUN: {} round ids on disk ({} with no round-tree entry). \
                 Would merge {} of them into #{} (scenario {:?}), keeping it {}. \
                 ids: {:?}",
                ids.len(),
                orphans,
                fork_ids.len(),
                canonical.0,
                scenario.as_str(),
                if end.is_none() { "open" } else { "closed" },
                sorted,
            ));
        }

        // Wipe every round/seq entry, write one canonical row.
        for (s, rid, _) in &round_entries {
            self.round.remove(&(s.clone(), *rid))?;
        }
        for r in self
            .seq
            .iter()
            .collect::<Result<Vec<((Scenario, RoundId), DateTime<Utc>)>>>()?
        {
            self.seq.remove(&r.0)?;
        }
        self.round
            .insert(&(scenario.clone(), canonical), &Round { start, end, winner })?;
        self.seq.insert(&(scenario.clone(), canonical), &start)?;

        let moved = self.collapse_round_data(canonical, &fork_ids)?;
        Ok(format!(
            "merged {} round id(s) into #{} (scenario {:?}); {} round-scoped rows re-keyed. \
             Reload the dashboard.",
            fork_ids.len(),
            canonical.0,
            scenario.as_str(),
            moved
        ))
    }
}

/// Re-key every row of a `(RoundId, Rest)`-keyed tree whose round id is in
/// `fork_ids` onto `canonical`. Rows come out of the tree RoundId-ascending,
/// so on a key collision the highest fork id is written last and wins.
fn rekey_round_first<Rest, V>(
    tree: &Tree<(RoundId, Rest), V>,
    canonical: RoundId,
    fork_ids: &std::collections::HashSet<RoundId>,
) -> Result<usize>
where
    Rest: serde::Serialize + serde::de::DeserializeOwned + Clone,
    V: serde::Serialize + serde::de::DeserializeOwned,
{
    let all: Vec<((RoundId, Rest), V)> = tree.iter().collect::<Result<_>>()?;
    let mut moved = 0;
    for ((rid, rest), v) in all {
        if !fork_ids.contains(&rid) {
            continue;
        }
        tree.remove(&(rid, rest.clone()))?;
        tree.insert(&(canonical, rest), &v)?;
        moved += 1;
    }
    Ok(moved)
}

/// Same as [`rekey_round_first`] for trees with a three-part `(RoundId, A, B)`
/// key.
fn rekey_round_first3<A, B, V>(
    tree: &Tree<(RoundId, A, B), V>,
    canonical: RoundId,
    fork_ids: &std::collections::HashSet<RoundId>,
) -> Result<usize>
where
    A: serde::Serialize + serde::de::DeserializeOwned + Clone,
    B: serde::Serialize + serde::de::DeserializeOwned + Clone,
    V: serde::Serialize + serde::de::DeserializeOwned,
{
    let all: Vec<((RoundId, A, B), V)> = tree.iter().collect::<Result<_>>()?;
    let mut moved = 0;
    for ((rid, a, b), v) in all {
        if !fork_ids.contains(&rid) {
            continue;
        }
        tree.remove(&(rid, a.clone(), b.clone()))?;
        tree.insert(&(canonical, a, b), &v)?;
        moved += 1;
    }
    Ok(moved)
}

#[cfg(test)]
mod merge_rounds_tests {
    use super::*;

    #[tokio::test(flavor = "multi_thread")]
    async fn collapses_forked_campaign_rounds() {
        let tmp = std::env::temp_dir().join(format!("bfdb-merge-test-{}", Uuid::new_v4()));
        let db = StatsDb::new_offline(tmp.join("db"), None, None).unwrap();
        let scen = Scenario::from("campaign");
        let t0 = Utc::now();

        // Three "rounds" for the same sortie -- the fragmentation the old
        // startup sweep produced. Oldest (r1) is the canonical target.
        let r1 = RoundId(10);
        let r2 = RoundId(2_000_010);
        let r3 = RoundId(4_000_010);
        for (rid, mins, open) in [(r1, 0i64, false), (r2, 60, false), (r3, 120, true)] {
            let start = t0 + chrono::Duration::minutes(mins);
            db.round
                .insert(
                    &(scen.clone(), rid),
                    &Round { start, end: if open { None } else { Some(start + chrono::Duration::minutes(30)) }, winner: None },
                )
                .unwrap();
            db.seq.insert(&(scen.clone(), rid), &start).unwrap();
        }

        let ucid = Ucid::default();
        let veh = Vehicle(String::from("F-16C_50"));
        for (rid, ak) in [(r1, 2u32), (r2, 3), (r3, 5)] {
            db.pilots
                .aggregates
                .insert(&(ucid, veh.clone(), rid), &Aggregates { air_kills: ak, ..Default::default() })
                .unwrap();
        }
        let oid = ObjectiveId::from(1i64);
        for (rid, n) in [(r1, 1u32), (r2, 2), (r3, 4)] {
            db.objective_captures.insert(&(rid, oid), &n).unwrap();
        }
        // round_info: newest fork (r3) should win
        for (rid, pts) in [(r1, 5i32), (r3, 42)] {
            db.pilots
                .round_info
                .insert(&(ucid, rid), &PilotRoundInfo { points: pts, ..Default::default() })
                .unwrap();
        }

        let report = db.merge_rounds("campaign").unwrap();
        assert!(report.contains("into #10"), "{report}");

        // one round left, still open (r3 was open), earliest start
        let left: Vec<_> = db.all_rounds().unwrap();
        assert_eq!(left.len(), 1);
        assert_eq!(left[0].1, r1);
        assert_eq!(left[0].2.start, t0);
        assert!(left[0].2.end.is_none());

        // aggregates summed onto canonical
        let agg = db.pilots.aggregates.get(&(ucid, veh.clone(), r1)).unwrap().unwrap();
        assert_eq!(agg.air_kills, 10);
        assert!(db.pilots.aggregates.get(&(ucid, veh.clone(), r2)).unwrap().is_none());
        assert!(db.pilots.aggregates.get(&(ucid, veh, r3)).unwrap().is_none());

        // capture counts summed
        assert_eq!(db.objective_captures.get(&(r1, oid)).unwrap().unwrap(), 7);

        // round_info: newest fork's value kept
        assert_eq!(db.pilots.round_info.get(&(ucid, r1)).unwrap().unwrap().points, 42);

        // idempotent
        let again = db.merge_rounds("campaign").unwrap();
        assert!(again.contains("already has a single round"), "{again}");

        let _ = std::fs::remove_dir_all(&tmp);
    }

    #[tokio::test(flavor = "multi_thread")]
    async fn merge_all_sweeps_orphan_round_ids() {
        let tmp = std::env::temp_dir().join(format!("bfdb-mergeall-test-{}", Uuid::new_v4()));
        let db = StatsDb::new_offline(tmp.join("db"), None, None).unwrap();
        let scen = Scenario::from("campaign");
        let t0 = Utc::now();

        // One real (open) round in the `round` tree...
        let real = RoundId(10);
        db.round
            .insert(&(scen.clone(), real), &Round { start: t0, end: None, winner: None })
            .unwrap();
        db.seq.insert(&(scen.clone(), real), &t0).unwrap();

        // ...plus two "orphan" round ids that only exist in the data trees
        // (this is how the fork bug's later rounds show up -- "Round 48000000"
        // with no scenario in the dashboard).
        let orphan_a = RoundId(48_000_000);
        let orphan_b = RoundId(50_000_000);
        let oid = ObjectiveId::from(7i64);
        for (rid, n) in [(real, 2u32), (orphan_a, 3), (orphan_b, 4)] {
            db.objective_captures.insert(&(rid, oid), &n).unwrap();
        }

        let dry = db.merge_all_rounds(true).unwrap();
        assert!(dry.contains("DRY RUN"), "{dry}");
        assert!(dry.contains("3 round ids on disk"), "{dry}");
        assert!(dry.contains("2 with no round-tree entry"), "{dry}");

        let done = db.merge_all_rounds(false).unwrap();
        assert!(done.contains("into #10"), "{done}");

        // orphan capture counts summed onto the real round, orphans cleared
        assert_eq!(db.objective_captures.get(&(real, oid)).unwrap().unwrap(), 9);
        assert!(db.objective_captures.get(&(orphan_a, oid)).unwrap().is_none());
        assert!(db.objective_captures.get(&(orphan_b, oid)).unwrap().is_none());

        // one round left, still open
        let left = db.all_rounds().unwrap();
        assert_eq!(left.len(), 1);
        assert_eq!(left[0].1, real);
        assert!(left[0].2.end.is_none());

        let _ = std::fs::remove_dir_all(&tmp);
    }
}

#[cfg(test)]
mod intel_storage_tests {
    use super::*;

    #[tokio::test(flavor = "multi_thread")]
    async fn on_disk_photo_round_trip() {
        let tmp = std::env::temp_dir().join(format!("bfdb-intel-test-{}", Uuid::new_v4()));
        let db_path = tmp.join("db");
        let img_dir = tmp.join("photos");
        let db = StatsDb::new_offline(&db_path, None, None).unwrap();
        db.set_intel_dir(Some(img_dir.clone())).unwrap();
        assert!(img_dir.is_dir(), "--intel-dir should be created");

        let id = Uuid::new_v4();
        let bytes = vec![0x89, b'P', b'N', b'G', 1, 2, 3, 4];
        db.intel_put_image(id, "image/png".into(), bytes.clone()).unwrap();

        // the row carries no inline bytes, the file holds them
        let row = db.intel_images.get(&id).unwrap().unwrap();
        assert!(row.data.is_none());
        assert_eq!(std::fs::read(img_dir.join(id.to_string())).unwrap(), bytes);

        // reads resolve back to the same (content_type, bytes)
        let (ct, got) = db.intel_get_image(&id).unwrap().unwrap();
        assert_eq!(ct, "image/png");
        assert_eq!(got, bytes);

        // purge removes both the row and the file
        db.intel_purge_all().unwrap();
        assert!(db.intel_images.get(&id).unwrap().is_none());
        assert!(!img_dir.join(id.to_string()).exists());

        let _ = std::fs::remove_dir_all(&tmp);
    }

    #[tokio::test(flavor = "multi_thread")]
    async fn in_db_photo_when_no_dir() {
        let tmp = std::env::temp_dir().join(format!("bfdb-intel-test-{}", Uuid::new_v4()));
        let db = StatsDb::new_offline(tmp.join("db"), None, None).unwrap();
        let id = Uuid::new_v4();
        db.intel_put_image(id, "image/jpeg".into(), vec![1, 2, 3]).unwrap();
        let row = db.intel_images.get(&id).unwrap().unwrap();
        assert_eq!(row.data.as_deref(), Some(&[1, 2, 3][..]));
        let (ct, got) = db.intel_get_image(&id).unwrap().unwrap();
        assert_eq!((ct.as_str(), got), ("image/jpeg", vec![1, 2, 3]));
        let _ = std::fs::remove_dir_all(&tmp);
    }
}
