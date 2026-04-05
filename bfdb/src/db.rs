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
use log::{error, info};
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
    collections::Bound,
    io::{Read as IoRead, Write as IoWrite},
    ops::Deref,
    path::{Path, PathBuf},
    str::FromStr,
    sync::Arc,
    time::Duration,
};
use tokio::task;
use uuid::Uuid;
use yats::Tree;

db_id!(KillId);
db_id!(RoundId);
db_id!(SortieId);

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
            .fetch_and_update(&k, |a| match a {
                None => None,
                Some(mut a) => {
                    f(&mut a);
                    Some(a)
                }
            })?
            .ok_or_else(|| anyhow!("aggregates {k:?} is missing"))?;
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
        }));
        let _t = t.clone();
        task::spawn(async move {
            if let Err(e) = _t.background_loop().await {
                error!("background task failed {e:?}")
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
        }));
        let _t = t.clone();
        task::spawn(async move {
            if let Err(e) = _t.background_loop().await {
                error!("background task failed {e:?}")
            }
        });
        info!("running in offline mode (no Netidx subscription)");
        Ok(t)
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

        let index = task::block_in_place(|| ArchiveIndex::new(&archive_cfg, &shard))?;
        let head_path = archive_cfg.archive_directory().join(shard.as_str()).join("current");
        let head_copy_path = archive_cfg.archive_directory().join(shard.as_str()).join("current_copy");
        // Try to copy the current file so we can open it without lock conflicts
        let head = task::block_in_place(|| {
            match copy_locked_file(&head_path, &head_copy_path) {
                Ok(()) => match netidx_archive::logfile::ArchiveReader::open(&head_copy_path) {
                    Ok(r) => {
                        info!("opened head file copy at {head_copy_path:?}");
                        Some(r)
                    }
                    Err(e) => {
                        error!("could not open head file copy: {e:?}");
                        None
                    }
                },
                Err(e) => {
                    error!("could not copy head file {head_path:?}: {e:?}");
                    // Fall back to direct open (works when DCS is not running)
                    match netidx_archive::logfile::ArchiveReader::open(&head_path) {
                        Ok(r) => {
                            info!("opened head file directly at {head_path:?}");
                            Some(r)
                        }
                        Err(e2) => {
                            error!("could not open head file directly: {e2:?}");
                            None
                        }
                    }
                }
            }
        });
        if head.is_none() {
            info!("reading historical files only");
        }
        let mut reader = ArchiveCollectionReader::new(
            index,
            archive_cfg.clone(),
            shard.clone(),
            head,
            Bound::Unbounded,
            Bound::Unbounded,
        );

        let mut ctx = StatCtx::default();
        let mut timer = time::interval(Duration::from_secs(5));
        let mut total_batches = 0u64;
        let mut total_items = 0u64;

        loop {
            timer.tick().await;
            // Refresh index to pick up newly rotated files
            if let Ok(new_index) = task::block_in_place(|| ArchiveIndex::new(&archive_cfg, &shard)) {
                reader.log_rotated(Utc::now(), new_index);
            }
            // Try to re-read the head file for new data
            let new_head = task::block_in_place(|| {
                match copy_locked_file(&head_path, &head_copy_path) {
                    Ok(()) => netidx_archive::logfile::ArchiveReader::open(&head_copy_path).ok(),
                    Err(_) => netidx_archive::logfile::ArchiveReader::open(&head_path).ok(),
                }
            });
            if let Some(h) = new_head {
                reader.set_head(h);
            }
            loop {
                let batch = task::block_in_place(|| reader.read_next(None));
                match batch {
                    Err(e) => {
                        error!("archive read error: {e:?}");
                        break;
                    }
                    Ok(None) => break, // caught up to end of available historical files
                    Ok(Some((ts, items))) => {
                        total_batches += 1;
                        total_items += items.len() as u64;
                        if total_batches <= 5 || total_batches % 100 == 0 {
                            info!("batch #{total_batches} ts={ts} items={} (total_items={total_items})", items.len());
                        }
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
                                error!("failed to add stat from JSONL: {e:?}");
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
        let no_hit = dead.shots.iter().any(|s| s.hit);
        let up = |a: &mut Aggregates| {
            if air {
                a.air_kills += 1
            } else {
                a.ground_kills += 1
            }
        };
        for shot in dead.shots.iter() {
            if no_hit && !shot.hit {
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
                    self.pilots.with_pilot_and_aggregates(
                        *ucid,
                        ctx.round,
                        |p| up(&mut p.total),
                        |a| up(a),
                    )?;
                    EnId::Player(*ucid)
                }
            };
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

    /// Get all pilots with their aggregate stats, sorted by total kills descending
    pub(crate) fn pilot_leaderboard(&self) -> Result<Vec<(Ucid, String, Aggregates)>> {
        let mut entries = Vec::new();
        for r in self.pilots.pilots.iter() {
            let (ucid, pilot) = r?;
            let name = pilot
                .name
                .last()
                .map(|s| s.clone())
                .unwrap_or(String::default());
            entries.push((ucid, name, pilot.total));
        }
        entries.sort_by(|a, b| {
            let a_kills = a.2.air_kills + a.2.ground_kills;
            let b_kills = b.2.air_kills + b.2.ground_kills;
            b_kills.cmp(&a_kills)
        });
        Ok(entries)
    }

    /// Get the latest round for each scenario
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

    pub(crate) fn recent_kills(&self, round: RoundId, limit: usize) -> Result<Vec<Dead>> {
        let mut kills = Vec::new();
        for r in self.kills.iter().rev() {
            let ((_, rid, _), dead) = r?;
            if rid == round {
                kills.push(dead);
                if kills.len() >= limit {
                    break;
                }
            }
        }
        Ok(kills)
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
                Some(((_, round), seq)) => match self.round.get(&(sortie.clone(), round))? {
                    Some(r) if r.end.is_none() => {
                        info!("NewRound: re-attaching to existing open round {round:?} seq={seq:?}");
                        ctx.0 = Some(StatCtxInner {
                            round,
                            seq,
                            sortie: sortie.clone(),
                        });
                        return Ok(());
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
        if let Stat::SessionStart { cfg, .. } = &stat {
            if ctx.0.is_none() {
                let sortie = cfg.netidx_base
                    .as_ref()
                    .map(|p| {
                        let s = format!("{p}");
                        s.rsplit('/').next().unwrap_or("unknown").to_string()
                    })
                    .unwrap_or_else(|| "campaign".to_string());
                let sortie = String::from(sortie);
                info!("auto-creating round from SessionStart, sortie={sortie:?}");
                self.new_round(ctx, time, sortie, time)?;
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
                self.objectives.insert(
                    &(ctx.round, id),
                    &Objective {
                        name,
                        pos,
                        kind,
                        owner,
                        by: None,
                        last_change: time,
                        health: 100,
                        logi: 100,
                        supply: 100,
                        fuel: 100,
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
                    o.logi = fuel
                })?;
            }
            Stat::Capture { id, by, side } => {
                self.with_objective((ctx.round, id), |o| o.owner = side)?;
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
                self.with_group((ctx.round, gid), |group| {
                    group.kind = GroupKind::Troop {
                        by,
                        name: troop.clone(),
                    }
                })?;
            }
            Stat::DeployGroup {
                by,
                gid,
                deployable,
            } => {
                self.pilots.with_pilot_and_aggregates(
                    by,
                    ctx.round,
                    |p| p.total.troops += 1,
                    |a| a.troops += 1,
                )?;
                self.with_group((ctx.round, gid), |group| {
                    group.kind = GroupKind::Deployed {
                        by,
                        name: deployable.clone(),
                    }
                })?;
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
                self.pilots
                    .with_sortie((id, ctx.round, sid), |s| s.land = Some(time))?;
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
            Stat::ConvoyDestroyed { .. }
            | Stat::CampaignEvent { .. }
            | Stat::PilotXp { .. }
            | Stat::AirRouteDelivered { .. }
            | Stat::AirRouteDestroyed { .. }
            | Stat::SeaRouteDelivered { .. }
            | Stat::SeaRouteDestroyed { .. } => {
                // Future: track in dedicated tables
            }
        };
        self.seq
            .insert(&(ctx.sortie.clone(), ctx.round), &time)?;
        ctx.seq = time;
        Ok(())
    }
}
