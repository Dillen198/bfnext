use crate::db_id;
use anyhow::{anyhow, bail, Result};
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
    collections::{Bound, VecDeque},
    io::{Read as IoRead, Write as IoWrite},
    ops::Deref,
    path::{Path, PathBuf},
    str::FromStr,
    sync::{Arc, Mutex as StdMutex, RwLock},
    time::Duration,
};
use tokio::{sync::broadcast, task};
use uuid::Uuid;
use yats::Tree;

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

    fn with_pilot_and_aggregates<F, G>(&self, ucid: Ucid, round: RoundId, f: F, g: G) -> Result<()>
    where
        F: FnMut(&mut Pilot),
        G: FnMut(&mut Aggregates),
    {
        let vehicle = self
            .round_info
            .get(&(ucid, round))?
            .and_then(|ri| ri.slot.and_then(|s| s.vehicle));
        self.with_pilot(ucid, f)?;
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

#[derive(Clone)]
pub(crate) struct StatsDbInner {
    #[allow(dead_code)]
    subscriber: Option<Subscriber>,
    #[allow(dead_code)]
    base: Option<NetidxPath>,
    stats_dir: Option<PathBuf>,
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
    units: Tree<(RoundId, EnId), Unit>,
    groups: Tree<(RoundId, GroupId), Group>,
    detected: Tree<(RoundId, EnId), BitFlags<DetectionSource, u8>>,
    objectives: Tree<(RoundId, ObjectiveId), Objective>,
    equipment: Tree<(RoundId, ObjectiveId, String), u32>,
    liquids: Tree<(RoundId, ObjectiveId, LiquidType), u32>,
    stats_jsonl: Option<PathBuf>,
    // Auth
    auth_sessions:    Tree<Uuid, SessionData>,
    auth_states:      Tree<Uuid, OAuthState>,
    // Trail history
    trail_points: Tree<(RoundId, std::string::String, i64), (f64, f64, f64, f64)>,
    latest_weather: Arc<RwLock<Option<WeatherSnapshot>>>,
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
    // Live bflib engine log, streamed over netidx from the running DCS mission
    // (distinct from bfdb's own process log)
    engine_log_tx: broadcast::Sender<std::string::String>,
    engine_log_history: Arc<StdMutex<VecDeque<std::string::String>>>,
    // Subset of engine_log_history matching an ERROR/WARN level tag -- kept
    // separately so the admin dashboard can show a short, high-signal error
    // feed without the client having to filter the full (much larger,
    // frequently-scrolling) log history itself.
    engine_error_history: Arc<StdMutex<VecDeque<std::string::String>>>,
    // The sortie name of the currently/most-recently active round, learned
    // from Stat::NewRound. bflib publishes its engine log and RPC procs
    // under `<netidx_base>/<sortie>/...` (see bflib/src/bg/mod.rs), so this
    // must be appended to `base` before subscribing -- a bare `base` path
    // will never resolve.
    current_sortie: Arc<StdMutex<Option<Scenario>>>,
    // Timestamp of the last stats-archive batch fully processed by
    // background_loop, persisted so a restart resumes from there instead of
    // replaying the entire historical archive from the beginning every time
    // (see background_loop -- a corrupted/duplicate-spammed archive segment
    // otherwise gets re-read in full on every single bfdb startup).
    replay_cursor: Tree<u8, DateTime<Utc>>,
}

const ENGINE_LOG_HISTORY_CAP: usize = 500;
const ENGINE_ERROR_HISTORY_CAP: usize = 200;

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
    pub(crate) fn new<P: AsRef<Path>>(
        subscriber: Subscriber,
        db: P,
        base: NetidxPath,
        stats_dir: Option<PathBuf>,
        include: Option<Regex>,
        exclude: Option<Regex>,
    ) -> Result<Self> {
        let db = sled::open(db.as_ref())?;
        let t = Self(Arc::new(StatsDbInner {
            subscriber: Some(subscriber),
            base: Some(base),
            stats_dir,
            include,
            exclude,
            db: db.clone(),
            pilots: Pilots::new(&db)?,
            seq: Tree::open(&db, "seq")?,
            round: Tree::open(&db, "round")?,
            session: Tree::open(&db, "session")?,
            kills: Tree::open(&db, "kills")?,
            shared_kills: Tree::open(&db, "shared_kills")?,
            units: Tree::open(&db, "units")?,
            groups: Tree::open(&db, "groups")?,
            detected: Tree::open(&db, "detected")?,
            objectives: Tree::open(&db, "objectives")?,
            equipment: Tree::open(&db, "equipment")?,
            liquids: Tree::open(&db, "liquids")?,
            stats_jsonl: None,
            auth_sessions: Tree::open(&db, "auth_sessions")?,
            auth_states: Tree::open(&db, "auth_states")?,
            trail_points: Tree::open(&db, "trail_points")?,
            latest_weather: Arc::new(RwLock::new(None)),
            objective_captures: Tree::open(&db, "objective_captures")?,
            captures: Tree::open(&db, "captures")?,
            deploys: Tree::open(&db, "deploys")?,
            aircraft_sorties: Tree::open(&db, "aircraft_sorties")?,
            admin_bans: Tree::open(&db, "admin_bans")?,
            wiki_pages: Tree::open(&db, "wiki_pages")?,
            wiki_images: Tree::open(&db, "wiki_images")?,
            engine_log_tx: broadcast::channel(1024).0,
            engine_log_history: Arc::new(StdMutex::new(VecDeque::new())),
            engine_error_history: Arc::new(StdMutex::new(VecDeque::new())),
            replay_cursor: Tree::open(&db, "replay_cursor")?,
            current_sortie: Arc::new(StdMutex::new(None)),
        }));
        t.seed_wiki_if_empty()?;
        t.seed_wiki_images_if_empty()?;
        // A older bug fabricated a round named after the last segment of the
        // netidx base (e.g. "campaign" from "/local/fowl/campaign") whenever a
        // SessionStart was replayed without a NewRound. Those bogus rounds are
        // never the real sortie and, being left open, hijack round selection.
        // Close any that are still open so they stop shadowing the real round.
        let base_tail = t
            .0
            .base
            .as_ref()
            .and_then(|p| format!("{p}").rsplit('/').next().map(String::from));
        if let Some(bogus) = &base_tail {
            let open_bogus: Vec<(RoundId, Round)> = t
                .round
                .scan_prefix(bogus)?
                .filter_map(|r| r.ok())
                .filter(|((_, _), rd)| rd.end.is_none())
                .map(|((_, rid), rd)| (rid, rd))
                .collect();
            for (rid, mut rd) in open_bogus {
                warn!("closing bogus open round {rid:?} (scenario {bogus:?} == netidx base tail, not a real sortie)");
                rd.end = Some(chrono::Utc::now());
                let _ = t.round.insert(&(bogus.clone(), rid), &rd)?;
            }
        }
        // Prime current_sortie from whatever *real* round is already open in the
        // DB. On restart, the archive replay resumes from replay_cursor and may
        // never re-witness the NewRound/SessionStart stat that originally
        // started the active round (they're before the cursor) -- without this,
        // the engine log/RPC subscriptions would wait forever for a sortie that
        // already exists.
        if let Some((sortie, _, _)) = t
            .latest_rounds()?
            .into_iter()
            .filter(|(s, _, _)| base_tail.as_ref().map(|t| t.as_str()) != Some(s.as_str()))
            .find(|(_, _, r)| r.end.is_none())
        {
            info!("resuming with active round sortie={sortie:?}");
            *t.current_sortie.lock().unwrap() = Some(sortie);
        }
        let _t = t.clone();
        task::spawn(async move {
            if let Err(e) = _t.background_loop().await {
                error!("background task failed {e:?}")
            }
        });
        let _t = t.clone();
        task::spawn(async move {
            if let Err(e) = _t.engine_log_loop().await {
                error!("engine log subscription failed {e:?}")
            }
        });
        Ok(t)
    }

    /// Create a database in offline mode (no Netidx subscription)
    pub(crate) fn new_offline<P: AsRef<Path>>(db: P, stats_dir: Option<PathBuf>, stats_jsonl: Option<PathBuf>) -> Result<Self> {
        let db = sled::open(db.as_ref())?;
        let t = Self(Arc::new(StatsDbInner {
            subscriber: None,
            base: None,
            stats_dir,
            include: None,
            exclude: None,
            db: db.clone(),
            pilots: Pilots::new(&db)?,
            seq: Tree::open(&db, "seq")?,
            round: Tree::open(&db, "round")?,
            session: Tree::open(&db, "session")?,
            kills: Tree::open(&db, "kills")?,
            shared_kills: Tree::open(&db, "shared_kills")?,
            units: Tree::open(&db, "units")?,
            groups: Tree::open(&db, "groups")?,
            detected: Tree::open(&db, "detected")?,
            objectives: Tree::open(&db, "objectives")?,
            equipment: Tree::open(&db, "equipment")?,
            liquids: Tree::open(&db, "liquids")?,
            stats_jsonl,
            auth_sessions: Tree::open(&db, "auth_sessions")?,
            auth_states: Tree::open(&db, "auth_states")?,
            trail_points: Tree::open(&db, "trail_points")?,
            latest_weather: Arc::new(RwLock::new(None)),
            objective_captures: Tree::open(&db, "objective_captures")?,
            captures: Tree::open(&db, "captures")?,
            deploys: Tree::open(&db, "deploys")?,
            aircraft_sorties: Tree::open(&db, "aircraft_sorties")?,
            admin_bans: Tree::open(&db, "admin_bans")?,
            wiki_pages: Tree::open(&db, "wiki_pages")?,
            wiki_images: Tree::open(&db, "wiki_images")?,
            engine_log_tx: broadcast::channel(1024).0,
            engine_log_history: Arc::new(StdMutex::new(VecDeque::new())),
            engine_error_history: Arc::new(StdMutex::new(VecDeque::new())),
            replay_cursor: Tree::open(&db, "replay_cursor")?,
            current_sortie: Arc::new(StdMutex::new(None)),
        }));
        t.seed_wiki_if_empty()?;
        t.seed_wiki_images_if_empty()?;
        let _t = t.clone();
        task::spawn(async move {
            if let Err(e) = _t.background_loop().await {
                error!("background task failed {e:?}")
            }
        });
        info!("running in offline mode (no Netidx subscription)");
        Ok(t)
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
    async fn engine_log_loop(self) -> Result<()> {
        use futures::{channel::mpsc, StreamExt};
        use netidx::subscriber::{Event, UpdatesFlags};
        use netidx::publisher::Value;

        let (subscriber, base) = match (&self.0.subscriber, &self.0.base) {
            (Some(s), Some(b)) => (s.clone(), b.clone()),
            _ => return Ok(()),
        };
        loop {
            let sortie = loop {
                if let Some(s) = self.0.current_sortie.lock().unwrap().clone() {
                    break s;
                }
                tokio::time::sleep(std::time::Duration::from_secs(1)).await;
            };
            let dval = subscriber.subscribe(base.append(&sortie).append("log"));
            let (tx, mut rx) = mpsc::channel(10);
            dval.updates(UpdatesFlags::empty(), tx);
            let mut seen_len = 0usize;
            while let Some(batch) = rx.next().await {
                if self.0.current_sortie.lock().unwrap().as_ref() != Some(&sortie) {
                    break; // sortie changed -- resubscribe under the new one
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
                        let mut hist = self.0.engine_log_history.lock().unwrap();
                        if hist.len() >= ENGINE_LOG_HISTORY_CAP {
                            hist.pop_front();
                        }
                        hist.push_back(line.clone());
                        drop(hist);
                        if is_engine_error_line(&line) {
                            let mut errs = self.0.engine_error_history.lock().unwrap();
                            if errs.len() >= ENGINE_ERROR_HISTORY_CAP {
                                errs.pop_front();
                            }
                            errs.push_back(line.clone());
                        }
                        let _ = self.0.engine_log_tx.send(line);
                    }
                }
            }
            if self.0.current_sortie.lock().unwrap().as_ref() == Some(&sortie) {
                // subscription itself ended (not a sortie change) -- nothing left to do
                return Ok(());
            }
        }
    }

    /// Subscribe to the live engine log stream, plus a snapshot of recent
    /// history for a newly-connected client to catch up with.
    pub(crate) fn engine_log_subscribe(&self) -> (broadcast::Receiver<std::string::String>, Vec<std::string::String>) {
        let rx = self.0.engine_log_tx.subscribe();
        let hist = self.0.engine_log_history.lock().unwrap().iter().cloned().collect();
        (rx, hist)
    }

    /// Recent ERROR/WARN lines from the engine log, oldest first -- backs the
    /// admin dashboard's error feed (see api_admin_engine_errors in main.rs).
    pub(crate) fn engine_error_snapshot(&self) -> Vec<std::string::String> {
        self.0.engine_error_history.lock().unwrap().iter().cloned().collect()
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
        proc_name: &str,
        args: Vec<(&str, netidx::publisher::Value)>,
    ) -> Result<netidx::publisher::Value> {
        use netidx_protocols::rpc::client::Proc;
        let (subscriber, base) = match (&self.0.subscriber, &self.0.base) {
            (Some(s), Some(b)) => (s, b),
            _ => bail!("netidx is disabled (bfdb started without --base)"),
        };
        let sortie = self.0.current_sortie.lock().unwrap().clone()
            .ok_or_else(|| anyhow!("no active sortie yet (mission hasn't reported in)"))?;
        let path = base.append(&sortie).append("api").append(proc_name);
        let proc = Proc::new(subscriber, path)?;
        proc.call(args).await
    }

    async fn background_loop(self) -> Result<()> {
        // If stats_jsonl is configured, use the JSONL reader instead of archive
        if let Some(jsonl_path) = self.stats_jsonl.clone() {
            return self.jsonl_loop(jsonl_path).await;
        }

        use arcstr::ArcStr;
        use netidx::subscriber::Event;
        use netidx_archive::logfile::BatchItem;
        use tokio::time;

        let stats_dir = match &self.stats_dir {
            Some(d) => d.clone(),
            None => return Ok(()), // no archive configured
        };

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
        let resume_from = self.0.replay_cursor.get(&0u8)?;
        if let Some(ts) = resume_from {
            info!("resuming stats archive replay after {ts}");
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
                                if let Err(e) = task::block_in_place(|| self.add_stat(&mut ctx, ts, st)) {
                                    error!("failed to add stat {e:?}")
                                }
                            }
                        }
                    }
                }
            }
            // Persist how far we've gotten so a restart resumes here instead
            // of replaying the whole archive from scratch.
            if let Some(ts) = last_seen_ts {
                if let Err(e) = self.0.replay_cursor.insert(&0u8, &ts) {
                    error!("failed to save replay cursor: {e:?}");
                }
            }
        }
    }

    /// Read stats from a JSONL file (one JSON object per line)
    async fn jsonl_loop(self, jsonl_path: PathBuf) -> Result<()> {
        use std::io::BufRead;
        use tokio::time;

        let mut ctx = StatCtx::default();
        let mut timer = time::interval(Duration::from_secs(5));
        let mut last_pos: u64 = 0;

        info!("starting JSONL reader from {jsonl_path:?}");

        loop {
            timer.tick().await;
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
                            if let Err(e) = task::block_in_place(|| self.add_stat(&mut ctx, ts, st)) {
                                warn!("failed to add stat from JSONL: {e:?}");
                            }
                        }
                        info!("processed {count} stats from JSONL (pos {last_pos} -> {pos})");
                    }
                    last_pos = pos;
                }
                Err(e) => error!("JSONL read error: {e:?}"),
            }
        }
    }

    fn new_round(
        &self,
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
        info!("new_round: inserting round id={id:?} sortie={sortie:?}");
        self.seq.insert(&key, &seqnum)?;
        self.round.insert(&key, &r)?;
        info!("new_round: round inserted successfully");
        *self.current_sortie.lock().unwrap() = Some(sortie.clone());
        ctx.0 = Some(StatCtxInner {
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
        let kid = KillId::new(&self.db)?;
        let air = match &dead.victim {
            Who::Player { ucid, .. } => {
                self.pilots.with_pilot_and_aggregates(
                    *ucid,
                    ctx.round,
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
                // All-time: use pre-aggregated totals
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

    pub(crate) fn latest_weather(&self) -> Option<WeatherSnapshot> {
        self.latest_weather.read().ok()?.clone()
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
                "Core Gameplay" => 2,
                "F10 Menu Systems" => 3,
                "Advanced Topics" => 4,
                "Reference" => 5,
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
            ("gameplay/objectives", "Objectives", "Core Gameplay", 0, include_str!("../seed_wiki/gameplay/objectives.md")),
            ("gameplay/capturing-objectives", "Capturing Objectives", "Core Gameplay", 1, include_str!("../seed_wiki/gameplay/capturing-objectives.md")),
            ("gameplay/logistics", "Logistics & Supply", "Core Gameplay", 2, include_str!("../seed_wiki/gameplay/logistics.md")),
            ("gameplay/points-and-lives", "Points and Lives", "Core Gameplay", 3, include_str!("../seed_wiki/gameplay/points-and-lives.md")),
            ("gameplay/chat-commands", "Chat Commands", "Core Gameplay", 4, include_str!("../seed_wiki/gameplay/chat-commands.md")),
            ("f10-menu/overview", "Overview", "F10 Menu Systems", 0, include_str!("../seed_wiki/f10-menu/overview.md")),
            ("f10-menu/actions", "Actions Menu", "F10 Menu Systems", 1, include_str!("../seed_wiki/f10-menu/actions.md")),
            ("f10-menu/jtac", "JTAC System", "F10 Menu Systems", 2, include_str!("../seed_wiki/f10-menu/jtac.md")),
            ("f10-menu/cargo", "Cargo Operations", "F10 Menu Systems", 3, include_str!("../seed_wiki/f10-menu/cargo.md")),
            ("f10-menu/troops", "Troop Transport", "F10 Menu Systems", 4, include_str!("../seed_wiki/f10-menu/troops.md")),
            ("f10-menu/ewr", "Early Warning Radar", "F10 Menu Systems", 5, include_str!("../seed_wiki/f10-menu/ewr.md")),
            ("f10-menu/recon", "Reconnaissance", "F10 Menu Systems", 6, include_str!("../seed_wiki/f10-menu/recon.md")),
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

    pub(crate) fn latest_rounds(&self) -> Result<Vec<(Scenario, RoundId, Round)>> {
        let mut rounds = Vec::new();
        let mut seen_scenarios = std::collections::HashSet::new();
        // Scan all rounds, keep the latest per scenario
        for r in self.round.iter() {
            let ((scenario, rid), round) = r?;
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

    fn add_stat(&self, ctx: &mut StatCtx, time: DateTime<Utc>, stat: Stat) -> Result<()> {
        if let Some(ctx) = &ctx.0 {
            if time <= ctx.seq {
                return Ok(());
            }
        }
        if let Stat::NewRound { sortie } = &stat {
            ctx.0 = None; // reset on session restart so we re-attach or create a new round
            info!("processing NewRound sortie={sortie:?}");
            match self.seq.scan_prefix(sortie)?.next_back().transpose()? {
                None => {
                    info!("NewRound: no existing seq, creating new round");
                    return self.new_round(ctx, time, sortie.clone(), time);
                }
                Some(((_, round), _seq)) => match self.round.get(&(sortie.clone(), round))? {
                    Some(r) if r.end.is_none() => {
                        info!("NewRound: ending stale open round {round:?}, creating new round");
                        let key = (sortie.clone(), round);
                        let mut stale = r;
                        stale.end = Some(time);
                        let _ = self.round.insert(&key, &stale)?;
                        return self.new_round(ctx, time, sortie.clone(), time);
                    }
                    Some(_) => {
                        info!("NewRound: existing round is ended, creating new round");
                        return self.new_round(ctx, time, sortie.clone(), time);
                    }
                    None => {
                        info!("NewRound: seq entry exists but round missing, creating new round");
                        return self.new_round(ctx, time, sortie.clone(), time);
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
                let sortie = self
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
                        let open = self
                            .seq
                            .scan_prefix(&sortie)?
                            .next_back()
                            .transpose()?
                            .and_then(|((_, round), seq)| {
                                match self.round.get(&(sortie.clone(), round)) {
                                    Ok(Some(r)) if r.end.is_none() => Some((round, seq)),
                                    _ => None,
                                }
                            });
                        match open {
                            Some((round, seq)) => {
                                info!("SessionStart: reattaching to open round {round:?} for sortie {sortie:?}");
                                *self.current_sortie.lock().unwrap() = Some(sortie.clone());
                                ctx.0 = Some(StatCtxInner { sortie, round, seq });
                            }
                            None => {
                                info!("auto-creating round from SessionStart, sortie={sortie:?}");
                                self.new_round(ctx, time, sortie, time)?;
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
                        |pilot| pilot.total.captures += 1,
                        |agg| agg.captures += 1,
                    )?
                }
            }
            Stat::Repair { id: _, by } => {
                self.pilots.with_pilot_and_aggregates(
                    by,
                    ctx.round,
                    |pilot| pilot.total.repairs += 1,
                    |agg| agg.repairs += 1,
                )?;
            }
            Stat::SupplyTransfer { from: _, to: _, by } => {
                self.pilots.with_pilot_and_aggregates(
                    by,
                    ctx.round,
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
                self.pilots.with_pilot_and_aggregates(
                    by,
                    ctx.round,
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
                let sid = SortieId::new(&self.db)?;
                let mut vehicle = None;
                self.pilots.with_pilot_round_info(id, ctx.round, |ri| {
                    if let Some(sl) = ri.slot.as_mut() {
                        sl.sortie = Some(sid);
                        vehicle = sl.vehicle.clone()
                    }
                })?;
                let vehicle = vehicle.ok_or_else(|| anyhow!("{id} takeoff without slotting"))?;
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
                let sid = sid.ok_or_else(|| anyhow!("{id} landed without taking off"))?;
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
                if let Ok(mut w) = self.latest_weather.write() {
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

    /// Wipe all campaign data — rounds, kills, objectives, pilot stats, trails,
    /// captures, sorties, weather — while preserving auth sessions and Discord links
    /// so admins remain logged in and pilot linking is not lost.
    pub(crate) fn reset_campaign_data(&self) -> Result<()> {
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
        // Trails & weather
        self.trail_points.clear()?;
        if let Ok(mut w) = self.latest_weather.write() { *w = None; }
        // auth_sessions, auth_states → preserved
        Ok(())
    }
}
