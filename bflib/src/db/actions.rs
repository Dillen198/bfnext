use super::{Db, MapM, objective::Objective};
use crate::{
    admin,
    db::{cargo::Oldest, group::DeployKind},
    group, group_mut,
    jtac::{JtId, Jtacs},
    objective,
    spawnctx::{SpawnCtx, SpawnLoc},
    unit,
};
use anyhow::{Context, Ok, Result, anyhow, bail};
use bfprotocols::{
    cfg::{
        Action, ActionGeoLimit, ActionKind, AiPlaneCfg, AiPlaneKind, ArtilleryCfg, AwacsCfg, BomberCfg,
        DeployableCfg, DeployableKind, DroneCfg, LimitEnforceTyp, MoveCfg, NavalCruiseMissileCfg,
        NukeCfg, ReconCfg, UnitTag,
    },
    db::{
        group::GroupId,
        objective::{ObjectiveId, ObjectiveKind},
    },
    perf::PerfInner,
    stats::Stat,
};
use chrono::{Duration, prelude::*};
use compact_str::format_compact;
use dcso3::{
    LuaVec2, LuaVec3, MizLua, String, Time, Vector2, Vector3,
    attribute::Attribute,
    centroid2d, change_heading,
    coalition::Side,
    controller::{
        ActionTyp, AiOption, AlarmState, AltType, AttackParams, BeaconSystem, BeaconType, Command,
        GroundOption, MissionPoint, OrbitPattern, PointType, Task, TacanBand, TurnMethod,
        VehicleFormation, WeaponExpend,
    },
    env::miz::MizIndex,
    group::Group,
    land::Land,
    net::Ucid,
    object::DcsObject,
    pointing_towards2,
    trigger::{MarkId, Modulation, Trigger},
    unit::Unit,
    world::World,
};
use enumflags2::BitFlags;
use fxhash::FxHashSet;
use log::{error, info, warn};
use rand::{Rng, thread_rng};
use smallvec::{SmallVec, smallvec};
use std::{cmp::max, f64, vec};

/// Build the task that activates a TACAN beacon for an AI aircraft, if the
/// action's config asked for one. Falls back to the first 3 alphanumeric
/// characters of the action's name (uppercased) as the morse callsign when
/// `tacan_callsign` isn't set. `frequency` is a placeholder within the
/// TACAN L-band -- DCS derives the actual beacon frequency from `channel`
/// and `mode_channel` when both are present.
fn tacan_beacon_task<'lua>(
    pl: &AiPlaneCfg,
    name: &str,
    system: BeaconSystem,
) -> Option<Task<'lua>> {
    let channel = pl.tacan_channel?;
    let band = pl.tacan_band.clone().unwrap_or(TacanBand::Y);
    let callsign = pl.tacan_callsign.clone().unwrap_or_else(|| {
        name.chars()
            .filter(|c| c.is_ascii_alphanumeric())
            .take(3)
            .collect::<std::string::String>()
            .to_uppercase()
            .into()
    });
    Some(Task::WrappedCommand(Command::ActivateBeacon {
        typ: BeaconType::TACAN,
        system,
        name: None,
        callsign,
        frequency: 1_088_000_000,
        channel: Some(channel as i64),
        mode_channel: Some(band),
        aa: Some(true),
        bearing: Some(true),
    }))
}

/// Build the task that sets an AI aircraft's DCS callsign, if the action's
/// config asked for one. `callsign_id` is DCS's own numeric callsign-family
/// id (the same one in the Mission Editor's group "Callsign" dropdown for
/// that aircraft category) -- we don't map friendly names to ids ourselves
/// since the valid set differs by aircraft category and has changed across
/// DCS versions.
fn callsign_command_task<'lua>(pl: &AiPlaneCfg) -> Option<Task<'lua>> {
    let callname = pl.callsign_id?;
    let number = pl.callsign_number.unwrap_or(1);
    Some(Task::WrappedCommand(Command::SetCallsign { callname, number }))
}

#[derive(Debug, Clone)]
pub struct WithPos<T> {
    pub cfg: T,
    pub pos: Vector2,
}

#[derive(Debug, Clone)]
pub struct WithObj<T> {
    pub cfg: T,
    pub oid: ObjectiveId,
}

#[derive(Debug, Clone)]
pub struct WithFromTo<T> {
    pub cfg: T,
    pub from: ObjectiveId,
    pub to: ObjectiveId,
}

#[derive(Debug, Clone)]
pub struct WithPosAndGroup<T> {
    pub cfg: T,
    pub pos: Vector2,
    pub group: GroupId,
}

#[derive(Debug, Clone)]
pub struct WithJtac<T> {
    pub cfg: T,
    pub jtac: JtId,
}

#[derive(Debug, Clone)]
pub enum ActionArgs {
    Tanker(WithPos<AiPlaneCfg>),
    Awacs(WithPos<AwacsCfg>),
    Bomber(WithJtac<BomberCfg>),
    CruiseMissileSpawn(WithPos<AiPlaneCfg>),
    Fighters(WithPos<AiPlaneCfg>),
    FightersWaypoint(WithPosAndGroup<()>),
    Attackers(WithPos<AiPlaneCfg>),
    AttackersWaypoint(WithPosAndGroup<()>),
    Sead(WithPos<AiPlaneCfg>),
    SeadWaypoint(WithPosAndGroup<()>),
    Drone(WithPos<DroneCfg>),
    DroneWaypoint(WithPosAndGroup<()>),
    Nuke(WithPos<NukeCfg>),
    TankerWaypoint(WithPosAndGroup<()>),
    AwacsWaypoint(WithPosAndGroup<()>),
    CruiseMissileWaypoint(WithPosAndGroup<()>),
    Paratrooper(WithPos<DeployableCfg>),
    Deployable(WithPos<DeployableCfg>),
    LogisticsRepair(WithObj<AiPlaneCfg>),
    LogisticsTransfer(WithFromTo<AiPlaneCfg>),
    Move(WithPosAndGroup<MoveCfg>),
    Rtb(WithPosAndGroup<()>),
    CarrierWaypoint(WithPosAndGroup<()>),
    CarrierRepair(WithObj<()>),
    CarrierRespawn(WithObj<()>),
    NavalCruiseMissileStrike(WithObj<NavalCruiseMissileCfg>),
    /// Player-triggered indirect fire support (artillery / armor barrage at a map-mark position).
    Artillery(WithPos<ArtilleryCfg>),
    /// Player-triggered reconnaissance flight over a map-mark position.
    Recon(WithPos<ReconCfg>),
}

impl ActionArgs {
    pub fn parse(
        db: &mut Db,
        action: &ActionKind,
        lua: MizLua,
        side: Side,
        s: &str,
    ) -> Result<Self> {
        fn get_key_pos(db: &mut Db, lua: MizLua, side: Side, key: &str) -> Result<Vector2> {
            let mut found: SmallVec<[(MarkId, Vector2); 4]> = smallvec![];
            for mk in World::singleton(lua)?.get_mark_panels()? {
                let mk = mk?;
                if mk.side.is_match(&side) && mk.text.as_str() == key {
                    let pos = Vector2::new(mk.pos.0.x, mk.pos.0.z);
                    found.push((mk.id, pos));
                }
            }
            if found.len() == 0 {
                Err(anyhow!("key {key} was not found"))
            } else if found.len() > 1 {
                Err(anyhow!(
                    "key {key} was found {} times, make sure to choose a unique key",
                    found.len()
                ))
            } else {
                db.ephemeral.msgs().delete_mark(found[0].0);
                Ok(found[0].1)
            }
        }
        fn get_closest_base(db: &mut Db, lua: MizLua, side: Side, key: &str) -> Result<Vector2> {
            let mut found: SmallVec<[(MarkId, Vector2); 4]> = smallvec![];
            for mk in World::singleton(lua)?.get_mark_panels()? {
                let mk = mk?;
                if mk.side.is_match(&side) && mk.text.as_str() == key {
                    let pos = Vector2::new(mk.pos.0.x, mk.pos.0.z);
                    found.push((mk.id, pos));
                }
            }
            if found.len() == 0 {
                Err(anyhow!("key {key} was not found"))
            } else if found.len() > 1 {
                Err(anyhow!(
                    "key {key} was found {} times, make sure to choose a unique key",
                    found.len()
                ))
            } else {
                db.ephemeral.msgs().delete_mark(found[0].0);
                let key_pos = found[0].1;
                let mut min_dist = f64::MAX;
                {
                    let mut closest_base = None;
                    for (_id, obj) in db.objectives() {
                        if obj.is_airbase() {
                            let obj_pos = obj.zone.pos();
                            let dist = na::distance_squared(&obj_pos.into(), &key_pos.into());
                            if dist < min_dist {
                                min_dist = dist;
                                closest_base = Some(obj);
                            };
                        }
                    }
                    match closest_base {
                        Some(o) => Ok(o.zone.pos()),
                        None => bail!("no bases to rtb!"),
                    }
                }
            }
        }
        fn pos_group<T>(
            db: &mut Db,
            lua: MizLua,
            side: Side,
            c: T,
            s: &str,
        ) -> Result<WithPosAndGroup<T>> {
            match s.split_once(" ") {
                None => Err(anyhow!("expected <gid> <key>")),
                Some((gid, key)) => Ok(WithPosAndGroup {
                    cfg: c,
                    pos: get_key_pos(db, lua, side, key)?,
                    group: gid.parse()?,
                }),
            }
        }
        fn pos_closest_base<T>(
            db: &mut Db,
            lua: MizLua,
            side: Side,
            c: T,
            s: &str,
        ) -> Result<WithPosAndGroup<T>> {
            match s.split_once(" ") {
                None => Err(anyhow!("expected <gid> <key>")),
                Some((gid, key)) => Ok(WithPosAndGroup {
                    cfg: c,
                    pos: get_closest_base(db, lua, side, key)?,
                    group: gid.parse()?,
                }),
            }
        }
        fn pos<T>(db: &mut Db, lua: MizLua, side: Side, cfg: T, s: &str) -> Result<WithPos<T>> {
            let pos = get_key_pos(db, lua, side, s)?;
            Ok(WithPos { cfg, pos })
        }
        fn jtac<T>(cfg: T, s: &str) -> Result<WithJtac<T>> {
            Ok(WithJtac {
                cfg,
                jtac: s.parse()?,
            })
        }
        fn obj<T>(db: &Db, cfg: T, s: &str) -> Result<WithObj<T>> {
            Ok(WithObj {
                cfg,
                oid: admin::get_airbase(db, s)?,
            })
        }
        fn from_to<T>(db: &Db, cfg: T, s: &str) -> Result<WithFromTo<T>> {
            match s.split_once(" ") {
                None => Err(anyhow!("expected two objectives <from> <to>")),
                Some((from, to)) => Ok(WithFromTo {
                    cfg,
                    from: admin::get_airbase(db, from).context("getting from airbase")?,
                    to: admin::get_airbase(db, to).context("getting to airbase")?,
                }),
            }
        }
        match action.clone() {
            ActionKind::Tanker(c) => Ok(Self::Tanker(pos(db, lua, side, c, s)?)),
            ActionKind::Awacs(c) => Ok(Self::Awacs(pos(db, lua, side, c, s)?)),
            ActionKind::Fighters(c) => Ok(Self::Fighters(pos(db, lua, side, c, s)?)),
            ActionKind::FighersWaypoint => {
                Ok(Self::FightersWaypoint(pos_group(db, lua, side, (), s)?))
            }
            ActionKind::Attackers(c) => Ok(Self::Attackers(pos(db, lua, side, c, s)?)),
            ActionKind::AttackersWaypoint => {
                Ok(Self::AttackersWaypoint(pos_group(db, lua, side, (), s)?))
            }
            ActionKind::Sead(c) => Ok(Self::Sead(pos(db, lua, side, c, s)?)),
            ActionKind::SeadWaypoint => {
                Ok(Self::SeadWaypoint(pos_group(db, lua, side, (), s)?))
            }
            ActionKind::Drone(c) => Ok(Self::Drone(pos(db, lua, side, c, s)?)),
            ActionKind::DroneWaypoint => Ok(Self::DroneWaypoint(pos_group(db, lua, side, (), s)?)),
            ActionKind::Nuke(c) => Ok(Self::Nuke(pos(db, lua, side, c, s)?)),
            ActionKind::Paratrooper(c) => Ok(Self::Paratrooper(pos(db, lua, side, c, s)?)),
            ActionKind::Deployable(c) => Ok(Self::Deployable(pos(db, lua, side, c, s)?)),
            ActionKind::LogisticsRepair(c) => Ok(Self::LogisticsRepair(obj(db, c, s)?)),
            ActionKind::LogisticsTransfer(c) => Ok(Self::LogisticsTransfer(from_to(db, c, s)?)),
            ActionKind::AwacsWaypoint => Ok(Self::AwacsWaypoint(pos_group(db, lua, side, (), s)?)),
            ActionKind::TankerWaypoint => {
                Ok(Self::TankerWaypoint(pos_group(db, lua, side, (), s)?))
            }
            ActionKind::Bomber(c) => Ok(Self::Bomber(jtac(c, s)?)),
            ActionKind::Move(c) => Ok(Self::Move(pos_group(db, lua, side, c, s)?)),
            ActionKind::CruiseMissileSpawn(c) => {
                Ok(Self::CruiseMissileSpawn(pos(db, lua, side, c, s)?))
            }
            ActionKind::Rtb => Ok(Self::Rtb(pos_closest_base(db, lua, side, (), s)?)),
            ActionKind::CruiseMissileWaypoint => Ok(Self::CruiseMissileWaypoint(pos_group(
                db,
                lua,
                side,
                (),
                s,
            )?)),
            ActionKind::CarrierWaypoint => Ok(Self::CarrierWaypoint(pos_group(db, lua, side, (), s)?)),
            ActionKind::CarrierRepair => Ok(Self::CarrierRepair(obj(db, (), s)?)),
            ActionKind::CarrierRespawn => Ok(Self::CarrierRespawn(obj(db, (), s)?)),
            ActionKind::NavalCruiseMissileStrike(c) => Ok(Self::NavalCruiseMissileStrike(obj(db, c, s)?)),
            ActionKind::Artillery(c) => Ok(Self::Artillery(pos(db, lua, side, c, s)?)),
            ActionKind::Recon(c) => Ok(Self::Recon(pos(db, lua, side, c, s)?)),
        }
    }

    fn pos(&self) -> Option<Vector2> {
        match self {
            Self::Attackers(c) => Some(c.pos),
            Self::AttackersWaypoint(c) => Some(c.pos),
            Self::Sead(c) => Some(c.pos),
            Self::SeadWaypoint(c) => Some(c.pos),
            Self::Awacs(c) => Some(c.pos),
            Self::AwacsWaypoint(c) => Some(c.pos),
            Self::CruiseMissileSpawn(c) => Some(c.pos),
            Self::CruiseMissileWaypoint(c) => Some(c.pos),
            Self::Bomber(_) => None,
            Self::Rtb(c) => Some(c.pos),
            Self::Deployable(c) => Some(c.pos),
            Self::Drone(c) => Some(c.pos),
            Self::DroneWaypoint(c) => Some(c.pos),
            Self::Fighters(c) => Some(c.pos),
            Self::FightersWaypoint(c) => Some(c.pos),
            Self::LogisticsRepair(_) => None,
            Self::LogisticsTransfer(_) => None,
            Self::Move(c) => Some(c.pos),
            Self::Nuke(c) => Some(c.pos),
            Self::Paratrooper(c) => Some(c.pos),
            Self::Tanker(c) => Some(c.pos),
            Self::TankerWaypoint(c) => Some(c.pos),
            Self::CarrierWaypoint(c) => Some(c.pos),
            Self::CarrierRepair(_) => None,
            Self::CarrierRespawn(_) => None,
            Self::NavalCruiseMissileStrike(_) => None,
            Self::Artillery(c) => Some(c.pos),
            Self::Recon(c) => Some(c.pos),
        }
    }
}

#[derive(Debug, Clone)]
pub struct ActionCmd {
    pub name: String,
    pub action: Action,
    pub args: ActionArgs,
}

impl ActionCmd {
    pub fn parse(db: &mut Db, lua: MizLua, side: Side, s: &str) -> Result<Self> {
        match s.split_once(" ") {
            None => Err(anyhow!("expected <action> <args>")),
            Some((name, args)) => {
                let action = db
                    .ephemeral
                    .cfg
                    .actions
                    .get(&side)
                    .and_then(|actions| actions.get(name))
                    .ok_or_else(|| anyhow!("no such action {name}"))?
                    .clone();
                let args = ActionArgs::parse(db, &action.kind, lua, side, args)?;
                Ok(Self {
                    name: name.into(),
                    action,
                    args,
                })
            }
        }
    }
}

// setup the awacs race track 90 degrees offset from the heading
// to the nearest enemy objective
fn racetrack_dist_and_heading(
    obj: &MapM<ObjectiveId, Objective>,
    pos: Vector2,
    enemy: Side,
) -> (f64, f64) {
    match Db::objective_near_point(obj, pos, |o| o.owner == enemy) {
        None => (9999999., 0.),
        Some((dist, hd, _)) => (dist, change_heading(hd, f64::consts::FRAC_PI_2)),
    }
}

fn group_position(lua: MizLua, name: &str) -> Result<Vector2> {
    let pos = Group::get_by_name(lua, name)
        .context("getting group")?
        .get_unit(1)
        .context("getting unit")?
        .get_point()?;
    Ok(Vector2::new(pos.x, pos.z))
}

impl Db {
    pub fn start_action(
        &mut self,
        lua: MizLua,
        perf: &mut PerfInner,
        spctx: &SpawnCtx,
        idx: &MizIndex,
        jtacs: &Jtacs,
        side: Side,
        ucid: Option<Ucid>,
        cmd: ActionCmd,
    ) -> Result<()> {
        let cost = match &cmd.action.kind {
            ActionKind::Nuke(nc) => {
                let div = max(1, self.persisted.nukes_used * nc.cost_scale as u32);
                max(1, cmd.action.cost / div)
            }
            ActionKind::Paratrooper(p) => {
                let sq = self
                    .ephemeral
                    .deployable_idx
                    .get(&side)
                    .and_then(|idx| idx.squads_by_name.get(&p.name))
                    .ok_or_else(|| anyhow!("missin squad"))?;
                sq.cost + cmd.action.cost
            }
            ActionKind::Deployable(d) => {
                let dp = self
                    .ephemeral
                    .deployable_idx
                    .get(&side)
                    .and_then(|idx| idx.deployables_by_name.get(&d.name))
                    .ok_or_else(|| anyhow!("missing deployable"))?;
                dp.cost + cmd.action.cost
            }
            ActionKind::Move(_) => match &cmd.args {
                ActionArgs::Move(a) => {
                    let pos = self.group_center(&a.group)?;
                    let dist = na::distance(&pos.into(), &a.pos.into());
                    let group = group!(self, a.group)?;
                    let step = match &group.origin {
                        DeployKind::Deployed { .. } => a.cfg.deployable,
                        DeployKind::Objective { origin } => {
                            let obj = objective!(self, origin)?;
                            match obj.kind {
                                ObjectiveKind::Farp { mobile: true, .. } => a.cfg.deployable,
                                _ => bail!("can't move this unit type"),
                            }
                        }
                        DeployKind::Troop { .. } => a.cfg.troop,
                        _ => bail!("can't move this unit type"),
                    };
                    let steps = dist / (step as f64);
                    steps as u32 * cmd.action.cost
                }
                _ => cmd.action.cost,
            },
            _ => cmd.action.cost,
        };
        if let Some(ucid) = ucid.as_ref() {
            if !self.ephemeral.cfg.rules.actions.check(ucid) {
                bail!("you are not authorized for actions")
            }
            match self.persisted.players.get(ucid) {
                None => bail!("unknown player {ucid}"),
                Some(player) => {
                    if cost > 0 && player.points < cost as i32 {
                        bail!(
                            "{ucid}({}) this action costs {} points and you have {} points",
                            player.name,
                            cost,
                            player.points
                        )
                    }
                    if side != player.side {
                        bail!(
                            "mismatched action side {side} vs player side {}",
                            player.side
                        )
                    }
                }
            }
        }
        let n = self
            .ephemeral
            .actions_taken
            .entry(side)
            .or_default()
            .entry(cmd.name.clone())
            .or_default();
        if let Some(limit) = cmd.action.limit {
            if *n >= limit {
                bail!("{side} is out of {} actions", cmd.name)
            }
        }
        match cmd.action.geo_limit {
            ActionGeoLimit::Unlimited => (),
            ActionGeoLimit::NearFriendlyObjective { max } => {
                if let Some(pos) = cmd.args.pos()
                    && let Some((dist, _, _)) =
                        Db::objective_near_point(&self.persisted.objectives, pos, |obj| {
                            obj.owner == side
                        })
                    && dist > max as f64
                {
                    bail!(
                        "this action point is {dist} meters but can only be targeted within {max} meters of a friendly objective"
                    )
                }
            }
        }
        let name = cmd.name.clone();
        let gid = match cmd.args {
            ActionArgs::Awacs(args) => self
                .awacs(perf, spctx, idx, side, ucid.clone(), name, cmd.action, args)
                .context("calling awacs")?,
            ActionArgs::AwacsWaypoint(args) => self
                .move_awacs(spctx, side, ucid.clone(), args)
                .context("moving awacs")?,
            ActionArgs::Bomber(args) => self
                .bomber_strike(
                    perf,
                    jtacs,
                    spctx,
                    idx,
                    side,
                    ucid.clone(),
                    name,
                    cmd.action,
                    args,
                )
                .context("calling bomber strike")?,
            ActionArgs::CruiseMissileWaypoint(args) => self
                .move_cruise_missile(spctx, side, ucid.clone(), args)
                .context("moving tanker")?,
            ActionArgs::CruiseMissileSpawn(args) => self
                .cruise_missile(perf, spctx, idx, side, ucid.clone(), name, cmd.action, args)
                .context("calling cruise missile bomber")?,
            ActionArgs::Deployable(args) => self
                .ai_deploy(
                    lua,
                    perf,
                    spctx,
                    idx,
                    side,
                    ucid.clone(),
                    name,
                    cmd.action,
                    args,
                )
                .context("calling ai deployment")?,
            ActionArgs::Fighters(args) => self
                .ai_fighters(perf, spctx, idx, side, ucid.clone(), name, cmd.action, args)
                .context("calling ai fighters")?,
            ActionArgs::FightersWaypoint(args) => self
                .move_ai_fighters(spctx, side, ucid.clone(), args)
                .context("moving ai fighters")?,
            ActionArgs::Attackers(args) => {
                self.ai_attackers(perf, spctx, idx, side, ucid.clone(), name, cmd.action, args)?
            }
            ActionArgs::AttackersWaypoint(args) => {
                self.move_ai_attackers(spctx, side, ucid.clone(), args)?
            }
            ActionArgs::Sead(args) => {
                self.ai_sead(perf, spctx, idx, side, ucid.clone(), name, cmd.action, args)?
            }
            ActionArgs::SeadWaypoint(args) => {
                self.move_ai_sead(spctx, side, ucid.clone(), args)?
            }
            ActionArgs::Rtb(args) => self.rtb(spctx, args).context("rtbing unit")?,
            ActionArgs::CarrierWaypoint(args) => self
                .carrier_waypoint(lua, args)
                .context("setting carrier waypoint")?,
            ActionArgs::CarrierRepair(args) => self
                .carrier_repair(args)
                .context("repairing carrier")?,
            ActionArgs::CarrierRespawn(args) => self
                .carrier_respawn(lua, spctx, idx, args)
                .context("respawning carrier")?,
            ActionArgs::NavalCruiseMissileStrike(args) => self
                .naval_cruise_missile_strike(lua, side, args)
                .context("naval cruise missile strike")?,
            ActionArgs::Drone(args) => self
                .drone(perf, spctx, idx, side, ucid.clone(), name, cmd.action, args)
                .context("calling drone")?,
            ActionArgs::DroneWaypoint(args) => self
                .move_drone(spctx, side, ucid.clone(), args)
                .context("moving drone")?,
            ActionArgs::LogisticsRepair(args) => self
                .ai_logistics_repair(perf, spctx, idx, side, ucid.clone(), name, cmd.action, args)
                .context("calling ai logi repair")?,
            ActionArgs::LogisticsTransfer(args) => self
                .ai_logistics_transfer(perf, spctx, idx, side, ucid.clone(), name, cmd.action, args)
                .context("calling ai log transfer")?,
            ActionArgs::Nuke(args) => self.nuke(spctx, args).context("calling nuke")?,
            ActionArgs::Paratrooper(args) => self
                .paratroops(perf, spctx, idx, side, ucid.clone(), name, cmd.action, args)
                .context("calling paratroops")?,
            ActionArgs::Tanker(args) => self
                .tanker(perf, spctx, idx, side, ucid.clone(), name, cmd.action, args)
                .context("calling tanker")?,
            ActionArgs::TankerWaypoint(args) => self
                .move_tanker(spctx, side, ucid.clone(), args)
                .context("moving tanker")?,
            ActionArgs::Move(args) => match &ucid {
                None => bail!("ucid is required for move"),
                Some(ucid) => self
                    .move_group(spctx, side, ucid, cmd.action.penalty.unwrap_or(0), args)
                    .context("moving unit")?,
            },
            ActionArgs::Artillery(args) => self
                .artillery_strike(lua, side, ucid.clone(), args)
                .context("calling artillery fire support")?,
            ActionArgs::Recon(args) => self
                .recon_flight(perf, spctx, idx, side, ucid.clone(), name, cmd.action, args)
                .context("calling recon flight")?,
        };
        if let Some(ucid) = ucid.as_ref() {
            self.ephemeral.stat(Stat::Action {
                by: *ucid,
                action: cmd.name.clone(),
                gid,
            });
            self.adjust_points(
                ucid,
                -(cost as i32),
                &format!("perform action {}", cmd.name),
            );
        }
        *self
            .ephemeral
            .actions_taken
            .entry(side)
            .or_default()
            .entry(cmd.name.clone())
            .or_default() += 1;
        Ok(())
    }

    pub(super) fn respawn_action(
        &mut self,
        perf: &mut PerfInner,
        spctx: &SpawnCtx,
        idx: &MizIndex,
        gid: GroupId,
    ) -> Result<()> {
        let now = Utc::now();
        let spawn_pos = self.group_center(&gid)?;
        let group = group!(self, gid)?;
        let side = group.side;
        if let DeployKind::Action {
            loc,
            player,
            spec,
            time,
            ..
        } = &group.origin
        {
            if let SpawnLoc::InAir { pos, .. } = loc {
                let args = WithPosAndGroup {
                    pos: *pos,
                    group: gid,
                    cfg: (),
                };
                macro_rules! delete_expired {
                    ($ai:expr) => {
                        if let Some(d) = $ai.duration {
                            if now - *time > Duration::hours(d as i64) {
                                self.delete_group(&gid)?;
                                return Ok(());
                            }
                        }
                    };
                }
                if let ActionKind::Awacs(ai) = &spec.kind {
                    delete_expired!(ai.plane);
                    let player = *player;
                    let mission = self
                        .awacs_mission(side, player, spawn_pos, args)
                        .context("generating awacs mission")?;
                    let group = group!(self, gid)?;
                    self.ephemeral.spawn_group(
                        perf,
                        &self.persisted,
                        idx,
                        spctx,
                        group,
                        mission,
                    )?;
                    return Ok(());
                }
                if let ActionKind::Tanker(ai) = &spec.kind {
                    delete_expired!(ai);
                    let player = *player;
                    let mission = self
                        .tanker_mission(side, player, spawn_pos, args)
                        .context("generate tanker mission")?;
                    let group = group!(self, gid)?;
                    self.ephemeral.spawn_group(
                        perf,
                        &self.persisted,
                        idx,
                        spctx,
                        group,
                        mission,
                    )?;
                    return Ok(());
                }
                if let ActionKind::CruiseMissileSpawn(ai) = &spec.kind {
                    delete_expired!(ai);
                    let player = *player;
                    let mission = self
                        .cruise_missile_mission(side, player, spawn_pos, args)
                        .context("generate alcm mission")?;
                    let group = group!(self, gid)?;
                    self.ephemeral.spawn_group(
                        perf,
                        &self.persisted,
                        idx,
                        spctx,
                        group,
                        mission,
                    )?;
                    return Ok(());
                }
                if let ActionKind::Drone(ai) = &spec.kind {
                    delete_expired!(ai.plane);
                    let player = *player;
                    let mission = self
                        .drone_mission(side, player, spawn_pos, args)
                        .context("generate drone mission")?;
                    let group = group!(self, gid)?;
                    self.ephemeral.spawn_group(
                        perf,
                        &self.persisted,
                        idx,
                        spctx,
                        group,
                        mission,
                    )?;
                    return Ok(());
                }
                if let ActionKind::Fighters(ai) = &spec.kind {
                    delete_expired!(ai);
                    let player = *player;
                    let mission = self
                        .ai_fighters_mission(side, player, spawn_pos, args)
                        .context("generate fighters mission")?;
                    let group = group!(self, gid)?;
                    self.ephemeral.spawn_group(
                        perf,
                        &self.persisted,
                        idx,
                        spctx,
                        group,
                        mission,
                    )?;
                    return Ok(());
                }
                if let ActionKind::Attackers(ai) = &spec.kind {
                    delete_expired!(ai);
                    let player = *player;
                    let mission = self
                        .ai_attackers_mission(side, player, spawn_pos, args)
                        .context("generate ai attackers mission")?;
                    let group = group!(self, gid)?;
                    self.ephemeral.spawn_group(
                        perf,
                        &self.persisted,
                        idx,
                        spctx,
                        group,
                        mission,
                    )?;
                    return Ok(());
                }
                if let ActionKind::Sead(ai) = &spec.kind {
                    delete_expired!(ai);
                    let player = *player;
                    let mission = self
                        .ai_sead_mission(side, player, spawn_pos, args)
                        .context("generate ai sead mission")?;
                    let group = group!(self, gid)?;
                    self.ephemeral.spawn_group(
                        perf,
                        &self.persisted,
                        idx,
                        spctx,
                        group,
                        mission,
                    )?;
                    return Ok(());
                }
            }
        }
        self.delete_group(&gid)
    }

    fn drone_mission<'lua>(
        &mut self,
        side: Side,
        ucid: Option<Ucid>,
        spawn_point: Vector2,
        args: WithPosAndGroup<()>,
    ) -> Result<Vec<MissionPoint<'lua>>> {
        self.ai_loiter_point_mission(
            side,
            ucid,
            args,
            OrbitPattern::Circle,
            spawn_point,
            |k| match k {
                ActionKind::Drone(_) => true,
                _ => false,
            },
            || Task::ComboTask(vec![]),
            || vec![],
        )
    }

    fn move_drone(
        &mut self,
        spctx: &SpawnCtx,
        side: Side,
        ucid: Option<Ucid>,
        args: WithPosAndGroup<()>,
    ) -> Result<Option<GroupId>> {
        let gid = args.group;
        let group = group!(self, gid)?;
        let pos = group_position(spctx.lua(), &group.name).context("getting pos")?;
        let mission = self
            .drone_mission(side, ucid, pos, args)
            .context("generate drone mission")?;
        self.set_ai_mission(spctx, gid, mission)
            .context("setting ai mission")?;
        Ok(None)
    }

    fn drone(
        &mut self,
        perf: &mut PerfInner,
        spctx: &SpawnCtx,
        idx: &MizIndex,
        side: Side,
        ucid: Option<Ucid>,
        name: String,
        action: Action,
        args: WithPos<DroneCfg>,
    ) -> Result<Option<GroupId>> {
        Ok(Some(self.add_and_spawn_ai_air(
            perf,
            spctx,
            idx,
            side,
            &ucid,
            name,
            action,
            0.,
            &WithPos {
                pos: args.pos,
                cfg: args.cfg.plane,
            },
            None,
            UnitTag::HotStart.into(),
            move |db, gid, pos| {
                db.drone_mission(
                    side,
                    ucid,
                    pos,
                    WithPosAndGroup {
                        group: gid,
                        pos: args.pos,
                        cfg: (),
                    },
                )
            },
        )?))
    }

    fn recon_flight(
        &mut self,
        perf: &mut PerfInner,
        spctx: &SpawnCtx,
        idx: &MizIndex,
        side: Side,
        ucid: Option<Ucid>,
        name: String,
        action: Action,
        args: WithPos<ReconCfg>,
    ) -> Result<Option<GroupId>> {
        use crate::db::intel::{IntelSource, IntelUnitClass};
        use bfprotocols::cfg::UnitTag;
        let target_pos = args.pos;
        let scan_radius = args.cfg.scan_radius_m;
        let ucid_for_report = ucid.clone();
        let gid = self.add_and_spawn_ai_air(
            perf,
            spctx,
            idx,
            side,
            &ucid,
            name,
            action,
            0.,
            &WithPos {
                pos: args.pos,
                cfg: args.cfg.plane,
            },
            None,
            BitFlags::empty(),
            move |db, gid, spawn_pos| {
                let enemy_side = match side {
                    Side::Blue => Side::Red,
                    Side::Red => Side::Blue,
                    Side::Neutral => Side::Neutral,
                };
                let now = Utc::now();
                // Collect enemy units within scan radius with their positions and tags.
                let detected: Vec<(dcso3::Vector2, IntelUnitClass)> = db
                    .objectives()
                    .filter(|(_, obj)| obj.owner() == enemy_side)
                    .flat_map(|(_, obj)| obj.groups.get(&enemy_side).into_iter().flat_map(|gs| gs.into_iter()))
                    .filter_map(|gid| db.persisted.groups.get(gid))
                    .flat_map(|g| g.units.into_iter())
                    .filter_map(|uid| db.persisted.units.get(uid))
                    .filter(|u| {
                        !u.dead
                            && na::distance_squared(&target_pos.into(), &u.pos.into())
                                <= scan_radius.powi(2)
                    })
                    .map(|u| {
                        let unit_class = db.ephemeral.cfg.unit_classification
                            .get(&u.typ)
                            .map(|tags| {
                                if tags.0.contains(UnitTag::SAM) || tags.0.contains(UnitTag::AAA) {
                                    IntelUnitClass::AirDefense
                                } else if tags.0.contains(UnitTag::Armor) || tags.0.contains(UnitTag::APC) {
                                    IntelUnitClass::Armor
                                } else if tags.0.contains(UnitTag::Artillery) {
                                    IntelUnitClass::Artillery
                                } else if tags.0.contains(UnitTag::Infantry) {
                                    IntelUnitClass::Infantry
                                } else if tags.0.contains(UnitTag::Boat) {
                                    IntelUnitClass::Naval
                                } else {
                                    IntelUnitClass::Unknown
                                }
                            })
                            .unwrap_or(IntelUnitClass::Unknown);
                        (dcso3::Vector2::new(u.pos.x, u.pos.y), unit_class)
                    })
                    .collect();

                // Cluster detected units and insert into IntelDatabase.
                if let Some(elint_cfg) = db.ephemeral.cfg.elint.as_ref() {
                    let cluster_sq = elint_cfg.contact_cluster_radius_m.powi(2);
                    // Group by class then cluster spatially.
                    let mut clusters: Vec<(dcso3::Vector2, IntelUnitClass, u8)> = Vec::new();
                    for (pos, class) in &detected {
                        let existing = clusters.iter_mut().find(|(cpos, cclass, _)| {
                            *cclass == *class
                                && na::distance_squared(&(*cpos).into(), &(*pos).into()) <= cluster_sq
                        });
                        if let Some((cpos, _, count)) = existing {
                            cpos.x = cpos.x * 0.7 + pos.x * 0.3;
                            cpos.y = cpos.y * 0.7 + pos.y * 0.3;
                            *count += 1;
                        } else {
                            clusters.push((*pos, *class, 1));
                        }
                    }
                    let elint_cfg = elint_cfg.clone();
                    for (pos, class, count) in clusters {
                        db.ephemeral.intel_db.upsert(
                            side,
                            enemy_side,
                            pos,
                            class,
                            count,
                            IntelSource::ReconFlight,
                            &elint_cfg,
                            now,
                        );
                    }
                }

                // Legacy map layer report (count only, 120 s).
                db.ephemeral.on_recon_result(target_pos, scan_radius, detected.len(), side, now);
                db.drone_mission(
                    side,
                    ucid_for_report,
                    spawn_pos,
                    WithPosAndGroup {
                        group: gid,
                        pos: target_pos,
                        cfg: (),
                    },
                )
            },
        )?;
        Ok(Some(gid))
    }

    fn ai_fighters_mission<'lua>(
        &mut self,
        side: Side,
        ucid: Option<Ucid>,
        spawn_pos: Vector2,
        args: WithPosAndGroup<()>,
    ) -> Result<Vec<MissionPoint<'lua>>> {
        let (freq, tacan, callsign) = match &group!(self, args.group)?.origin {
            DeployKind::Action { spec, name, .. } => match &spec.kind {
                ActionKind::Fighters(pl) => (
                    pl.freq,
                    tacan_beacon_task(pl, name.as_ref(), BeaconSystem::TACAN),
                    callsign_command_task(pl),
                ),
                _ => (None, None, None),
            },
            _ => (None, None, None),
        };
        let main_task = Task::EngageTargets {
            target_types: vec![
                Attribute::Fighters,
                Attribute::MultiroleFighters,
                Attribute::BattleAirplanes,
                Attribute::Battleplanes,
                Attribute::Helicopters,
                Attribute::AttackHelicopters,
            ],
            max_dist: Some(30_000.),
            priority: None,
        };
        let init_task = Task::ComboTask({
            let mut tasks = vec![Task::WrappedCommand(Command::SetUnlimitedFuel(true))];
            if let Some(f) = freq {
                tasks.push(Task::WrappedCommand(Command::SetFrequency {
                    frequency: f,
                    modulation: Modulation::AM,
                    power: 25,
                }));
            }
            if let Some(t) = tacan {
                tasks.push(t);
            }
            if let Some(t) = callsign {
                tasks.push(t);
            }
            tasks.push(main_task.clone());
            tasks
        });
        self.ai_loiter_point_mission(
            side,
            ucid,
            args,
            OrbitPattern::Circle,
            spawn_pos,
            |k| match k {
                ActionKind::Fighters(_) => true,
                _ => false,
            },
            move || init_task.clone(),
            move || vec![main_task.clone()],
        )
    }

    fn move_ai_fighters(
        &mut self,
        spctx: &SpawnCtx,
        side: Side,
        ucid: Option<Ucid>,
        args: WithPosAndGroup<()>,
    ) -> Result<Option<GroupId>> {
        let gid = args.group;
        let group = group!(self, gid)?;
        let pos = group_position(spctx.lua(), &group.name)?;
        let mission = self
            .ai_fighters_mission(side, ucid, pos, args)
            .context("generate fighters mission")?;
        self.set_ai_mission(spctx, gid, mission)
            .context("setting fighters mission")?;
        Ok(None)
    }

    fn ai_fighters(
        &mut self,
        perf: &mut PerfInner,
        spctx: &SpawnCtx,
        idx: &MizIndex,
        side: Side,
        ucid: Option<Ucid>,
        name: String,
        action: Action,
        args: WithPos<AiPlaneCfg>,
    ) -> Result<Option<GroupId>> {
        Ok(Some(self.add_and_spawn_ai_air(
            perf,
            spctx,
            idx,
            side,
            &ucid,
            name,
            action,
            0.,
            &args,
            None,
            BitFlags::empty(),
            move |db, gid, pos| {
                db.ai_fighters_mission(
                    side,
                    ucid,
                    pos,
                    WithPosAndGroup {
                        cfg: (),
                        pos: args.pos,
                        group: gid,
                    },
                )
            },
        )?))
    }

    fn ai_attackers_mission<'lua>(
        &mut self,
        side: Side,
        ucid: Option<Ucid>,
        spawn_pos: Vector2,
        args: WithPosAndGroup<()>,
    ) -> Result<Vec<MissionPoint<'lua>>> {
        let (freq, tacan, callsign) = match &group!(self, args.group)?.origin {
            DeployKind::Action { spec, name, .. } => match &spec.kind {
                ActionKind::Attackers(pl) => (
                    pl.freq,
                    tacan_beacon_task(pl, name.as_ref(), BeaconSystem::TACAN),
                    callsign_command_task(pl),
                ),
                _ => (None, None, None),
            },
            _ => (None, None, None),
        };
        let main_task = Task::EngageTargets {
            target_types: vec![
                Attribute::Fighters,
                Attribute::MultiroleFighters,
                Attribute::BattleAirplanes,
                Attribute::Battleplanes,
                Attribute::Helicopters,
                Attribute::AttackHelicopters,
                Attribute::GroundUnits,
                Attribute::GroundVehicles,
                Attribute::ArmedGroundUnits,
            ],
            max_dist: Some(15_000.),
            priority: None,
        };
        let init_task = Task::ComboTask({
            let mut tasks = vec![Task::WrappedCommand(Command::SetUnlimitedFuel(true))];
            if let Some(f) = freq {
                tasks.push(Task::WrappedCommand(Command::SetFrequency {
                    frequency: f,
                    modulation: Modulation::AM,
                    power: 25,
                }));
            }
            if let Some(t) = tacan {
                tasks.push(t);
            }
            if let Some(t) = callsign {
                tasks.push(t);
            }
            tasks.push(main_task.clone());
            tasks
        });
        self.ai_loiter_point_mission(
            side,
            ucid,
            args,
            OrbitPattern::Circle,
            spawn_pos,
            |k| match k {
                ActionKind::Attackers(_) => true,
                _ => false,
            },
            move || init_task.clone(),
            move || vec![main_task.clone()],
        )
    }

    fn ai_sead_mission<'lua>(
        &mut self,
        side: Side,
        ucid: Option<Ucid>,
        spawn_pos: Vector2,
        args: WithPosAndGroup<()>,
    ) -> Result<Vec<MissionPoint<'lua>>> {
        let (freq, tacan, callsign) = match &group!(self, args.group)?.origin {
            DeployKind::Action { spec, name, .. } => match &spec.kind {
                ActionKind::Sead(pl) => (
                    pl.freq,
                    tacan_beacon_task(pl, name.as_ref(), BeaconSystem::TACAN),
                    callsign_command_task(pl),
                ),
                _ => (None, None, None),
            },
            _ => (None, None, None),
        };
        let main_task = Task::EngageTargets {
            target_types: vec![
                // Radar-guided SAM systems
                Attribute::SAM_SR,      // SAM Search Radar
                Attribute::SAM_TR,      // SAM Tracking Radar
                Attribute::SAM_LL,      // SAM Launcher
                Attribute::SAM_CC,      // SAM Command Center
                Attribute::SR_SAM,      // Short Range SAM
                Attribute::MR_SAM,      // Medium Range SAM
                Attribute::LR_SAM,      // Long Range SAM
                Attribute::SAMElements, // SAM elements
                Attribute::SAM,         // General SAM
                Attribute::SAMRelated,  // SAM related
                Attribute::AirDefence,  // Air Defence
                Attribute::ArmedAirDefence, // Armed Air Defence
                Attribute::AirDefenceVehicles, // Air Defence vehicles
                // EWR (Early Warning Radar) systems
                Attribute::EWR,         // Early Warning Radar
                // Static and Mobile AAA that might have radar
                Attribute::StaticAAA,   // Static AAA
                Attribute::MobileAAA,   // Mobile AAA
            ],
            max_dist: Some(15_000.), // Same range as Attackers
            priority: None,
        };
        let init_task = Task::ComboTask({
            let mut tasks = vec![Task::WrappedCommand(Command::SetUnlimitedFuel(true))];
            if let Some(f) = freq {
                tasks.push(Task::WrappedCommand(Command::SetFrequency {
                    frequency: f,
                    modulation: Modulation::AM,
                    power: 25,
                }));
            }
            if let Some(t) = tacan {
                tasks.push(t);
            }
            if let Some(t) = callsign {
                tasks.push(t);
            }
            tasks.push(main_task.clone());
            tasks
        });
        self.ai_loiter_point_mission(
            side,
            ucid,
            args,
            OrbitPattern::Circle,
            spawn_pos,
            |k| match k {
                ActionKind::Sead(_) => true,
                _ => false,
            },
            move || init_task.clone(),
            move || vec![main_task.clone()],
        )
    }

    fn move_ai_attackers(
        &mut self,
        spctx: &SpawnCtx,
        side: Side,
        ucid: Option<Ucid>,
        args: WithPosAndGroup<()>,
    ) -> Result<Option<GroupId>> {
        let gid = args.group;
        let group = group!(self, gid)?;
        let pos = group_position(spctx.lua(), &group.name)?;
        let mission = self
            .ai_attackers_mission(side, ucid, pos, args)
            .context("generate attackers mission")?;
        self.set_ai_mission(spctx, gid, mission)
            .context("setting ai mission")?;
        Ok(None)
    }

    fn move_ai_sead(
        &mut self,
        spctx: &SpawnCtx,
        side: Side,
        ucid: Option<Ucid>,
        args: WithPosAndGroup<()>,
    ) -> Result<Option<GroupId>> {
        let gid = args.group;
        let group = group!(self, gid)?;
        let pos = group_position(spctx.lua(), &group.name)?;
        let mission = self
            .ai_sead_mission(side, ucid, pos, args)
            .context("generate sead mission")?;
        self.set_ai_mission(spctx, gid, mission)
            .context("setting ai mission")?;
        Ok(None)
    }

    fn ai_attackers(
        &mut self,
        perf: &mut PerfInner,
        spctx: &SpawnCtx,
        idx: &MizIndex,
        side: Side,
        ucid: Option<Ucid>,
        name: String,
        action: Action,
        args: WithPos<AiPlaneCfg>,
    ) -> Result<Option<GroupId>> {
        Ok(Some(self.add_and_spawn_ai_air(
            perf,
            spctx,
            idx,
            side,
            &ucid,
            name,
            action,
            0.,
            &args,
            None,
            BitFlags::empty(),
            move |db, group, pos| {
                db.ai_attackers_mission(
                    side,
                    ucid,
                    pos,
                    WithPosAndGroup {
                        cfg: (),
                        pos: args.pos,
                        group,
                    },
                )
            },
        )?))
    }

    fn ai_sead(
        &mut self,
        perf: &mut PerfInner,
        spctx: &SpawnCtx,
        idx: &MizIndex,
        side: Side,
        ucid: Option<Ucid>,
        name: String,
        action: Action,
        args: WithPos<AiPlaneCfg>,
    ) -> Result<Option<GroupId>> {
        Ok(Some(self.add_and_spawn_ai_air(
            perf,
            spctx,
            idx,
            side,
            &ucid,
            name,
            action,
            0.,
            &args,
            None,
            BitFlags::empty(),
            move |db, group, pos| {
                db.ai_sead_mission(
                    side,
                    ucid,
                    pos,
                    WithPosAndGroup {
                        cfg: (),
                        pos: args.pos,
                        group,
                    },
                )
            },
        )?))
    }

    fn move_group(
        &mut self,
        spctx: &SpawnCtx,
        side: Side,
        ucid: &Ucid,
        penalty: u32,
        args: WithPosAndGroup<MoveCfg>,
    ) -> Result<Option<GroupId>> {
        let pos = self.group_center(&args.group)?;
        let group = group_mut!(self, args.group)?;
        if group.side != side {
            bail!("can't move an enemy unit")
        }
        self.ephemeral
            .groups_with_move_missions
            .insert(args.group, args.pos);
        for uid in &group.units {
            self.ephemeral.units_able_to_move.insert(*uid);
        }
        if penalty > 0 {
            match &mut group.origin {
                DeployKind::Deployed {
                    player, moved_by, ..
                }
                | DeployKind::Troop {
                    player, moved_by, ..
                } if ucid != player => *moved_by = Some((ucid.clone(), penalty)),
                DeployKind::Action { .. }
                | DeployKind::Crate { .. }
                | DeployKind::Objective { .. }
                | DeployKind::ObjectiveDeprecated
                | DeployKind::Troop { .. }
                | DeployKind::Deployed { .. }
                | DeployKind::DownedPilot { .. }
                | DeployKind::Dismount { .. } => (),
            }
        }
        let land = Land::singleton(spctx.lua())?;
        let alt0 = land.get_height(LuaVec2(pos))?;
        let alt1 = land.get_height(LuaVec2(args.pos))?;
        let group = Group::get_by_name(spctx.lua(), &group.name).context("getting group")?;
        let con = group.get_controller()?;
        let att = Task::EngageTargets {
            target_types: vec![
                Attribute::Fighters,
                Attribute::MultiroleFighters,
                Attribute::BattleAirplanes,
                Attribute::Battleplanes,
                Attribute::Helicopters,
                Attribute::AttackHelicopters,
                Attribute::GroundUnits,
                Attribute::GroundVehicles,
                Attribute::ArmedGroundUnits,
            ],
            max_dist: Some(2_000.),
            priority: None,
        };
        con.set_task(Task::Mission {
            airborne: Some(false),
            route: vec![
                MissionPoint {
                    action: Some(ActionTyp::Ground(VehicleFormation::OffRoad)),
                    airdrome_id: None,
                    helipad: None,
                    typ: PointType::TurningPoint,
                    link_unit: None,
                    pos: LuaVec2(pos),
                    alt: alt0,
                    alt_typ: Some(AltType::BARO),
                    time_re_fu_ar: None,
                    eta: Some(Time(0.)),
                    eta_locked: Some(true),
                    speed: 20.,
                    speed_locked: Some(true),
                    name: None,
                    task: Box::new(Task::ComboTask(vec![
                        Task::WrappedOption(AiOption::Ground(GroundOption::AlarmState(
                            AlarmState::Green,
                        ))),
                        Task::WrappedOption(AiOption::Ground(GroundOption::AlarmState(
                            AlarmState::Auto,
                        ))),
                        att.clone(),
                    ])),
                },
                MissionPoint {
                    action: Some(ActionTyp::Ground(VehicleFormation::OffRoad)),
                    airdrome_id: None,
                    helipad: None,
                    typ: PointType::TurningPoint,
                    time_re_fu_ar: None,
                    link_unit: None,
                    pos: LuaVec2(args.pos),
                    alt: alt1,
                    alt_typ: Some(AltType::BARO),
                    speed: 20.,
                    speed_locked: None,
                    eta: None,
                    eta_locked: None,
                    name: Some(String::from("move")),
                    task: Box::new(Task::ComboTask(vec![
                        Task::WrappedOption(AiOption::Ground(GroundOption::AlarmState(
                            AlarmState::Red,
                        ))),
                        att,
                    ])),
                },
            ],
        })?;
        Ok(None)
    }

    fn rtb(&mut self, spctx: &SpawnCtx, mut args: WithPosAndGroup<()>) -> Result<Option<GroupId>> {
        let gid = args.group;
        let mission = self
            .ai_rtb_mission(&mut args, || Task::ComboTask(vec![]))
            .context("generate rtb mission")?;
        self.set_ai_mission(spctx, gid, mission)?;
        Ok(Some(gid))
    }

    fn carrier_waypoint(&mut self, lua: MizLua, args: WithPosAndGroup<()>) -> Result<Option<GroupId>> {
        info!("[CARRIER_WAYPOINT] Received waypoint command for group {:?} to position {:?}", args.group, args.pos);

        // Find carrier objective by group ID
        let mut carrier_info: Option<(ObjectiveId, dcso3::String)> = None;
        for (id, obj) in &self.persisted.objectives {
            if let ObjectiveKind::CarrierGroup { carrier_template: template, .. } = &obj.kind {
                info!("[CARRIER_WAYPOINT] Checking carrier group {} with template {}", obj.name, template);
                info!("[CARRIER_WAYPOINT] Objective owner: {:?}, all groups: {:?}", obj.owner, obj.groups);
                if !template.is_empty() {
                    if let Some(groups) = obj.groups.get(&obj.owner) {
                        info!("[CARRIER_WAYPOINT] Carrier has groups: {:?}", groups);
                        if groups.contains(&args.group) {
                            info!("[CARRIER_WAYPOINT] MATCH! Found carrier group for {:?}", args.group);
                            carrier_info = Some((*id, template.clone()));
                            break;
                        }
                    }
                }
            }
        }

        if let Some((obj_id, template)) = carrier_info {
            info!("[CARRIER_WAYPOINT] Setting waypoint for carrier template: {}", template);
            // Update waypoint in database
            if let Some(obj) = self.persisted.objectives.get_mut_cow(&obj_id) {
                if let ObjectiveKind::CarrierGroup { waypoint, .. } = &mut obj.kind {
                    *waypoint = Some(args.pos);
                }
            }

            // Command carrier group to move
            // When using Group.activate(), the unit keeps its original name (e.g. "BCARRIER-1")
            // Try to get the group directly by the template name first (for activated groups),
            // then fall back to getting unit by name (for spawned groups)
            let speed = self.ephemeral.cfg.carrier.as_ref().map(|c| c.movement_speed).unwrap_or(5.0);

            // First try to get group directly by template name (works for activated carriers)
            let group_result = Group::get_by_name(lua, &template)
                .or_else(|_| {
                    // Fall back to getting unit then group (works for spawned carriers)
                    Unit::get_by_name(lua, &template)
                        .and_then(|u| u.get_group())
                });

            match group_result {
                Result::Ok(dcs_group) => {
                    // Ensure carrier units are registered for position tracking.
                    // The spawn-time registration may not have worked (e.g. units not ready
                    // immediately after Group.activate()), so register them now.
                    if let Some(group) = self.persisted.groups.get(&args.group) {
                        match dcs_group.get_units() {
                            Result::Ok(dcs_units) => {
                                for dcs_unit_res in dcs_units {
                                    let dcs_unit = match dcs_unit_res {
                                        Result::Ok(u) => u,
                                        Result::Err(_) => continue,
                                    };
                                    let dcs_unit_name = match dcs_unit.get_name() {
                                        Result::Ok(n) => n,
                                        Result::Err(_) => continue,
                                    };
                                    let uid_match = group.units.into_iter().find_map(|uid| {
                                        self.persisted.units.get(uid).and_then(|u| {
                                            if !u.dead && u.template_name.as_str() == dcs_unit_name.as_str() {
                                                Some((*uid, u))
                                            } else {
                                                None
                                            }
                                        })
                                    });
                                    if let Some((uid, unit)) = uid_match {
                                        match dcs_unit.object_id() {
                                            Result::Ok(unit_oid) => {
                                                if !self.ephemeral.object_id_by_uid.contains_key(&uid) {
                                                    info!("[CARRIER_WAYPOINT] Registering carrier unit '{}' (uid={:?}) with object_id {:?}",
                                                          unit.name, uid, unit_oid);
                                                    self.ephemeral.uid_by_object_id.insert(unit_oid.clone(), uid);
                                                    self.ephemeral.object_id_by_uid.insert(uid, unit_oid);
                                                }
                                                self.ephemeral.units_able_to_move.insert(uid);
                                                self.ephemeral.units_potentially_close_to_enemies.insert(uid);
                                            }
                                            Result::Err(_) => {}
                                        }
                                    }
                                }
                            }
                            Result::Err(_) => {
                                // Fallback: just add to units_able_to_move without object_id registration
                                for uid in &group.units {
                                    self.ephemeral.units_able_to_move.insert(*uid);
                                }
                            }
                        }
                    }

                    info!("[CARRIER_WAYPOINT] Found group {}, commanding move to {:?} at speed {}", template, args.pos, speed);
                    let controller = dcs_group.get_controller()?;
                    controller.set_task(Task::Mission {
                        route: vec![MissionPoint {
                            action: None,
                            airdrome_id: None,
                            helipad: None,
                            typ: PointType::TurningPoint,
                            time_re_fu_ar: None,
                            link_unit: None,
                            pos: LuaVec2(args.pos),
                            alt: 0.,
                            alt_typ: None,
                            speed,
                            speed_locked: None,
                            eta: None,
                            eta_locked: None,
                            name: Some(dcso3::String::from("waypoint")),
                            task: Box::new(Task::ComboTask(vec![])),
                        }],
                        airborne: Some(false),
                    })?;
                    info!("[CARRIER_WAYPOINT] Successfully set waypoint task for carrier group");
                }
                Result::Err(e) => {
                    error!("[CARRIER_WAYPOINT] Failed to get group {}: {:?}", template, e);
                }
            }
        }
        Ok(None)
    }

    fn carrier_repair(&mut self, args: WithObj<()>) -> Result<Option<GroupId>> {
        // First collect all needed info without borrowing
        let (nb_id, repair_cost, available) = {
            let cg = objective!(self, &args.oid)?;
            match &cg.kind {
                ObjectiveKind::CarrierGroup { parent_naval_base: Some(nb_id), .. } => {
                    let repair_cost = self.ephemeral.cfg.carrier.as_ref().map(|c| c.repair_cost).unwrap_or(5000);
                    let nb = objective!(self, nb_id)?;
                    let available = nb.warehouse.equipment.get("SUPPLIES").map(|inv| inv.stored).unwrap_or(0);
                    (*nb_id, repair_cost, available)
                }
                _ => bail!("Objective is not a carrier group")
            }
        };

        if available >= repair_cost {
            // Now mutate
            if let Some(nb_mut) = self.persisted.objectives.get_mut_cow(&nb_id) {
                if let Some(inv) = nb_mut.warehouse.equipment.get_mut_cow("SUPPLIES") {
                    inv.stored -= repair_cost;
                }
            }
            if let Some(cg_mut) = self.persisted.objectives.get_mut_cow(&args.oid) {
                cg_mut.warehouse.damaged = false;
                cg_mut.health = 100;
            }
        } else {
            bail!("Not enough supplies at Naval Base to repair carrier (need {}, have {})", repair_cost, available);
        }
        Ok(None)
    }

    fn carrier_respawn(&mut self, _lua: MizLua, _spctx: &SpawnCtx, _idx: &MizIndex, args: WithObj<()>) -> Result<Option<GroupId>> {
        // First collect all needed info without borrowing
        let (nb_id, respawn_cost, available, health) = {
            let cg = objective!(self, &args.oid)?;
            match &cg.kind {
                ObjectiveKind::CarrierGroup { parent_naval_base: Some(nb_id), .. } => {
                    let respawn_cost = self.ephemeral.cfg.carrier.as_ref().map(|c| c.respawn_cost).unwrap_or(15000);
                    let nb = objective!(self, nb_id)?;
                    let available = nb.warehouse.equipment.get("SUPPLIES").map(|inv| inv.stored).unwrap_or(0);
                    (*nb_id, respawn_cost, available, cg.health)
                }
                _ => bail!("Objective is not a carrier group")
            }
        };

        if health > 0 {
            bail!("Carrier is not destroyed (health: {}%)", health);
        }

        if available >= respawn_cost {
            // Now mutate
            if let Some(nb_mut) = self.persisted.objectives.get_mut_cow(&nb_id) {
                if let Some(inv) = nb_mut.warehouse.equipment.get_mut_cow("SUPPLIES") {
                    inv.stored -= respawn_cost;
                }
            }
            if let Some(cg_mut) = self.persisted.objectives.get_mut_cow(&args.oid) {
                cg_mut.health = 100;
                cg_mut.warehouse.damaged = false;
            }
        } else {
            bail!("Not enough supplies at Naval Base to respawn carrier (need {}, have {})", respawn_cost, available);
        }
        Ok(None)
    }

    fn naval_cruise_missile_strike(
        &mut self,
        lua: MizLua,
        side: Side,
        args: WithObj<NavalCruiseMissileCfg>,
    ) -> Result<Option<GroupId>> {
        // 1. Validate target is enemy objective, get target position
        let target_pos = {
            let target_obj = objective!(self, &args.oid)?;
            if target_obj.owner == side {
                bail!("Cannot strike a friendly objective");
            }
            target_obj.zone.pos()
        };

        // 2. Find nearest friendly carrier group in range with ammo
        let mut best_carrier: Option<(ObjectiveId, f64, dcso3::String)> = None;
        for cg_id in &self.persisted.carrier_groups {
            let cg = objective!(self, cg_id)?;
            if cg.owner != side || cg.health == 0 {
                continue;
            }
            if let ObjectiveKind::CarrierGroup { carrier_template, .. } = &cg.kind {
                let dist = na::distance(&cg.zone.pos().into(), &target_pos.into());
                if dist <= args.cfg.max_range as f64 {
                    match &best_carrier {
                        None => best_carrier = Some((*cg_id, dist, carrier_template.clone())),
                        Some((_, best_dist, _)) if dist < *best_dist => {
                            best_carrier = Some((*cg_id, dist, carrier_template.clone()));
                        }
                        _ => {}
                    }
                }
            }
        }

        let (carrier_oid, _dist, template) = best_carrier
            .ok_or_else(|| anyhow!("No friendly carrier group in range"))?;

        // 3. Get the DCS group and check ammo
        let dcs_group = Group::get_by_name(lua, &template)
            .or_else(|_| {
                Unit::get_by_name(lua, &template)
                    .and_then(|u| u.get_group())
            })?;

        let mut available_missiles: u8 = 0;
        for unit_res in dcs_group.get_units()? {
            let dcs_unit = unit_res?;
            let first = dcs_unit.get_ammo()?.first();
            match first {
                std::result::Result::Ok(ammo) => {
                    available_missiles = ammo.count()? as u8;
                    break;
                }
                std::result::Result::Err(_) => continue,
            }
        }

        if available_missiles < args.cfg.missiles_per_strike {
            bail!(
                "Carrier has only {} missiles remaining, need {} for strike",
                available_missiles,
                args.cfg.missiles_per_strike
            );
        }


        // 5. Build Task::Bombing and command the carrier group
        let expend = match args.cfg.missiles_per_strike {
            1 => WeaponExpend::One,
            2 => WeaponExpend::Two,
            4 => WeaponExpend::Four,
            _ => WeaponExpend::Two,
        };
        let attack_params = AttackParams {
            altitude: Some(9000.),
            attack_qty: Some(1),
            direction: None,
            expend: Some(expend),
            group_attack: Some(false),
            weapon_type: Some(2097152),
            attack_qty_limit: None,
            altitude_enabled: Some(false),
            direction_enabled: Some(false),
            point: None,
            x: Some(target_pos.x),
            y: Some(target_pos.y),
        };

        let task = Task::Bombing {
            point: LuaVec2(target_pos),
            params: attack_params,
        };

        let controller = dcs_group.get_controller()?;
        controller.push_task(task)?;

        info!(
            "Naval cruise missile strike: carrier {:?} firing {} missiles at objective {:?}",
            carrier_oid, args.cfg.missiles_per_strike, args.oid
        );

        Ok(None)
    }

    fn tanker_mission<'lua>(
        &mut self,
        side: Side,
        ucid: Option<Ucid>,
        spawn_pos: Vector2,
        args: WithPosAndGroup<()>,
    ) -> Result<Vec<MissionPoint<'lua>>> {
        let (freq, tacan, callsign) = match &group!(self, args.group)?.origin {
            DeployKind::Action { spec, name, .. } => match &spec.kind {
                ActionKind::Tanker(pl) => (
                    pl.freq,
                    tacan_beacon_task(pl, name.as_ref(), BeaconSystem::TACANTanker),
                    callsign_command_task(pl),
                ),
                _ => (None, None, None),
            },
            _ => (None, None, None),
        };
        self.ai_loiter_point_mission(
            side,
            ucid,
            args,
            OrbitPattern::RaceTrack,
            spawn_pos,
            |k| match k {
                ActionKind::Tanker(_) => true,
                _ => false,
            },
            move || {
                let mut tasks = vec![
                    Task::Tanker,
                    Task::WrappedCommand(Command::SetUnlimitedFuel(true)),
                    Task::WrappedCommand(Command::SetFrequency {
                        frequency: freq.unwrap_or(264000000),
                        modulation: Modulation::AM,
                        power: 25,
                    }),
                ];
                if let Some(t) = tacan.clone() {
                    tasks.push(t);
                }
                if let Some(t) = callsign.clone() {
                    tasks.push(t);
                }
                Task::ComboTask(tasks)
            },
            || vec![Task::Tanker],
        )
    }

    fn move_tanker(
        &mut self,
        spctx: &SpawnCtx,
        side: Side,
        ucid: Option<Ucid>,
        args: WithPosAndGroup<()>,
    ) -> Result<Option<GroupId>> {
        let gid = args.group;
        let group = group!(self, gid)?;
        let pos = group_position(spctx.lua(), &group.name)?;
        let mission = self
            .tanker_mission(side, ucid, pos, args)
            .context("generate tanker mission")?;
        self.set_ai_mission(spctx, gid, mission)?;
        Ok(None)
    }

    fn tanker(
        &mut self,
        perf: &mut PerfInner,
        spctx: &SpawnCtx,
        idx: &MizIndex,
        side: Side,
        ucid: Option<Ucid>,
        name: String,
        action: Action,
        args: WithPos<AiPlaneCfg>,
    ) -> Result<Option<GroupId>> {
        Ok(Some(self.add_and_spawn_ai_air(
            perf,
            spctx,
            idx,
            side,
            &ucid,
            name,
            action,
            0.,
            &args,
            None,
            BitFlags::empty(),
            move |db, gid, pos| {
                db.tanker_mission(
                    side,
                    ucid,
                    pos,
                    WithPosAndGroup {
                        cfg: (),
                        pos: args.pos,
                        group: gid,
                    },
                )
            },
        )?))
    }

    fn move_cruise_missile<'lua>(
        &mut self,
        spctx: &SpawnCtx<'lua>,
        side: Side,
        ucid: Option<Ucid>,
        args: WithPosAndGroup<()>,
    ) -> Result<Option<GroupId>> {
        let gid = args.group;
        let group = group!(self, gid)?;
        let pos = group_position(spctx.lua(), &group.name)?;
        let mission = self
            .cruise_missile_mission(side, ucid, pos, args)
            .context("generating CruiseMissile mission")?;
        self.set_ai_mission(spctx, gid, mission)
            .context("setting ai mission")?;
        Ok(Some(gid))
    }

    fn cruise_missile(
        &mut self,
        perf: &mut PerfInner,
        spctx: &SpawnCtx,
        idx: &MizIndex,
        side: Side,
        ucid: Option<Ucid>,
        name: String,
        action: Action,
        args: WithPos<AiPlaneCfg>,
    ) -> Result<Option<GroupId>> {
        let gid = self.add_and_spawn_ai_air(
            perf,
            spctx,
            idx,
            side,
            &ucid,
            name,
            action,
            0.,
            &args,
            None,
            BitFlags::empty(),
            move |db, gid, pos| {
                db.cruise_missile_mission(
                    side,
                    ucid,
                    pos,
                    WithPosAndGroup {
                        cfg: (),
                        pos: args.pos,
                        group: gid,
                    },
                )
            },
        )?;
        self.persisted.actions.insert_cow(gid);
        Ok(Some(gid))
    }

    fn paratroops(
        &mut self,
        perf: &mut PerfInner,
        spctx: &SpawnCtx,
        idx: &MizIndex,
        side: Side,
        ucid: Option<Ucid>,
        name: String,
        action: Action,
        args: WithPos<DeployableCfg>,
    ) -> Result<Option<GroupId>> {
        Ok(Some(
            self.add_and_spawn_ai_air(
                perf,
                spctx,
                idx,
                side,
                &ucid,
                name,
                action,
                0.,
                &WithPos {
                    pos: args.pos,
                    cfg: args
                        .cfg
                        .plane
                        .clone()
                        .ok_or_else(|| anyhow!("paratrooper missing plane config"))?,
                },
                Some(args.pos),
                BitFlags::empty(),
                |db, gid, _pos| db.ai_point_to_point_mission(gid, || Task::ComboTask(vec![])),
            )?,
        ))
    }

    fn nuke(&mut self, spctx: &SpawnCtx, args: WithPos<NukeCfg>) -> Result<Option<GroupId>> {
        let land = Land::singleton(spctx.lua())?;
        let act = Trigger::singleton(spctx.lua())?.action()?;
        let alt = land.get_height(LuaVec2(args.pos))? + 500.;
        let pos = Vector3::new(args.pos.x, alt, args.pos.y);
        act.explosion(LuaVec3(pos), args.cfg.power as f32)?;
        self.persisted.nukes_used += 1;
        self.ephemeral.dirty();
        Ok(None)
    }

    fn ai_logistics_transfer(
        &mut self,
        perf: &mut PerfInner,
        spctx: &SpawnCtx,
        idx: &MizIndex,
        side: Side,
        ucid: Option<Ucid>,
        name: String,
        action: Action,
        args: WithFromTo<AiPlaneCfg>,
    ) -> Result<Option<GroupId>> {
        let from = objective!(self, args.from)?.zone.pos();
        let to = objective!(self, args.to)?.zone.pos();
        Ok(Some(self.add_and_spawn_ai_air(
            perf,
            spctx,
            idx,
            side,
            &ucid,
            name,
            action,
            0.,
            &WithPos {
                cfg: args.cfg.clone(),
                pos: from,
            },
            Some(to),
            BitFlags::empty(),
            |db, gid, _pos| db.ai_point_to_point_mission(gid, || Task::ComboTask(vec![])),
        )?))
    }

    fn ai_logistics_repair(
        &mut self,
        perf: &mut PerfInner,
        spctx: &SpawnCtx,
        idx: &MizIndex,
        side: Side,
        ucid: Option<Ucid>,
        name: String,
        action: Action,
        args: WithObj<AiPlaneCfg>,
    ) -> Result<Option<GroupId>> {
        let pos = objective!(self, args.oid)?.zone.pos();
        Ok(Some(self.add_and_spawn_ai_air(
            perf,
            spctx,
            idx,
            side,
            &ucid,
            name,
            action,
            0.,
            &WithPos {
                pos,
                cfg: args.cfg.clone(),
            },
            Some(pos),
            BitFlags::empty(),
            |db, gid, _pos| db.ai_point_to_point_mission(gid, || Task::ComboTask(vec![])),
        )?))
    }

    fn ai_deploy(
        &mut self,
        lua: MizLua,
        perf: &mut PerfInner,
        spctx: &SpawnCtx,
        idx: &MizIndex,
        side: Side,
        ucid: Option<Ucid>,
        name: String,
        action: Action,
        args: WithPos<DeployableCfg>,
    ) -> Result<Option<GroupId>> {
        match args.cfg.plane.as_ref() {
            Some(plane) => Ok(Some(self.add_and_spawn_ai_air(
                perf,
                spctx,
                idx,
                side,
                &ucid,
                name,
                action,
                0.,
                &WithPos {
                    cfg: plane.clone(),
                    pos: args.pos,
                },
                Some(args.pos),
                BitFlags::empty(),
                |db, gid, _pos| db.ai_point_to_point_mission(gid, || Task::ComboTask(vec![])),
            )?)),
            None => {
                self.deployable_to_point(
                    lua,
                    idx,
                    args.pos,
                    args.cfg.name,
                    side,
                    ucid.unwrap_or_default(),
                )?;
                Ok(None)
            }
        }
    }

    fn ai_point_to_point_mission<'lua>(
        &mut self,
        gid: GroupId,
        task: impl Fn() -> Task<'lua> + 'static,
    ) -> Result<Vec<MissionPoint<'lua>>> {
        let group = group!(self, gid)?;
        let (src, tgt, alt, alt_typ, speed) = match &group.origin {
            DeployKind::Action {
                spec,
                destination: Some(tgt),
                rtb: Some(src),
                ..
            } => match &spec.kind {
                ActionKind::Bomber(b) => (
                    *src,
                    *tgt,
                    b.plane.altitude,
                    b.plane.altitude_typ.clone(),
                    b.plane.speed,
                ),
                ActionKind::LogisticsRepair(p)
                | ActionKind::LogisticsTransfer(p)
                | ActionKind::Paratrooper(DeployableCfg {
                    name: _,
                    plane: Some(p),
                })
                | ActionKind::Deployable(DeployableCfg {
                    name: _,
                    plane: Some(p),
                }) => (*src, *tgt, p.altitude, p.altitude_typ.clone(), p.speed),
                _ => bail!("expected a point to point action"),
            },
            _ => bail!("expected action group with rtb and destination"),
        };
        macro_rules! wpt {
            ($name:expr, $pos:expr) => {
                MissionPoint {
                    action: Some(ActionTyp::Air(TurnMethod::FlyOverPoint)),
                    typ: PointType::TurningPoint,
                    airdrome_id: None,
                    helipad: None,
                    time_re_fu_ar: None,
                    link_unit: None,
                    pos: LuaVec2($pos),
                    alt,
                    alt_typ: Some(alt_typ.clone()),
                    speed,
                    eta: None,
                    speed_locked: None,
                    eta_locked: None,
                    name: Some($name.into()),
                    task: Box::new(task()),
                }
            };
        }
        Ok(vec![wpt!("tgt", tgt), wpt!("rtb", src)])
    }

    fn ai_rtb_mission<'lua>(
        &mut self,
        args: &mut WithPosAndGroup<()>,
        task: impl Fn() -> Task<'lua> + 'static,
    ) -> Result<Vec<MissionPoint<'lua>>> {
        let gid = args.group;
        let group = group!(self, gid)?;
        let mut min_dist = f64::MAX;
        let rtb_pos = {
            let mut closest_base = None;
            for (_id, obj) in self.objectives() {
                if obj.is_airbase() {
                    let obj_pos = obj.zone.pos();
                    let dist = na::distance_squared(&obj_pos.into(), &args.pos.into());
                    if dist < min_dist {
                        min_dist = dist;
                        closest_base = Some(obj);
                    };
                }
            }
            match closest_base {
                Some(o) => o.zone.pos(),
                None => bail!("no bases to rtb!"),
            }
        };

        let (alt, alt_typ, speed) = match &group.origin {
            DeployKind::Action {
                marks: _,
                loc: _,
                player: _,
                name: _,
                spec,
                time: _,
                destination: _,
                rtb: _,
                origin: _,
                ammo: _,
                jtac: _,
            } => match &spec.kind {
                ActionKind::Tanker(ai_plane_cfg) => (
                    ai_plane_cfg.altitude,
                    ai_plane_cfg.altitude_typ.clone(),
                    ai_plane_cfg.speed,
                ),
                ActionKind::Awacs(awacs_cfg) => (
                    awacs_cfg.plane.altitude,
                    awacs_cfg.plane.altitude_typ.clone(),
                    awacs_cfg.plane.speed,
                ),
                ActionKind::Bomber(bomber_cfg) => (
                    bomber_cfg.plane.altitude,
                    bomber_cfg.plane.altitude_typ.clone(),
                    bomber_cfg.plane.speed,
                ),
                ActionKind::CruiseMissileSpawn(ai_plane_cfg) => (
                    ai_plane_cfg.altitude,
                    ai_plane_cfg.altitude_typ.clone(),
                    ai_plane_cfg.speed,
                ),
                ActionKind::Drone(drone_cfg) => (
                    drone_cfg.plane.altitude,
                    drone_cfg.plane.altitude_typ.clone(),
                    drone_cfg.plane.speed,
                ),
                ActionKind::Recon(recon_cfg) => (
                    recon_cfg.plane.altitude,
                    recon_cfg.plane.altitude_typ.clone(),
                    recon_cfg.plane.speed,
                ),
                ActionKind::Sead(ai_plane_cfg) => (
                    ai_plane_cfg.altitude,
                    ai_plane_cfg.altitude_typ.clone(),
                    ai_plane_cfg.speed,
                ),
                ActionKind::Fighters(ai_plane_cfg) => (
                    ai_plane_cfg.altitude,
                    ai_plane_cfg.altitude_typ.clone(),
                    ai_plane_cfg.speed,
                ),
                ActionKind::Attackers(ai_plane_cfg) => (
                    ai_plane_cfg.altitude,
                    ai_plane_cfg.altitude_typ.clone(),
                    ai_plane_cfg.speed,
                ),
                _ => bail!("not a valid type"),
            },
            _ => bail!("not the right action kind"),
        };

        let group = group_mut!(self, gid)?;

        match &mut group.origin {
            DeployKind::Action {
                marks, rtb, spec, ..
            } => {
                *rtb = Some(rtb_pos);
                (*spec).kind = ActionKind::Rtb;
                for id in marks.iter() {
                    self.ephemeral.msgs().delete_mark(*id);
                }
            }
            _ => bail!("only works with some action deployed groups."),
        }

        Ok(vec![MissionPoint {
            action: Some(ActionTyp::Air(TurnMethod::FlyOverPoint)),
            typ: PointType::TurningPoint,
            airdrome_id: None,
            helipad: None,
            time_re_fu_ar: None,
            link_unit: None,
            pos: LuaVec2(rtb_pos),
            alt,
            alt_typ: Some(alt_typ.clone()),
            speed,
            eta: None,
            speed_locked: None,
            eta_locked: None,
            name: Some("rtb".to_owned().into()),
            task: Box::new(task()),
        }])
    }

    fn bomber_strike(
        &mut self,
        perf: &mut PerfInner,
        jtacs: &Jtacs,
        spctx: &SpawnCtx,
        idx: &MizIndex,
        side: Side,
        ucid: Option<Ucid>,
        name: String,
        action: Action,
        args: WithJtac<BomberCfg>,
    ) -> Result<Option<GroupId>> {
        let jt = jtacs.get(&args.jtac)?;
        let tgt = jt
            .target()
            .as_ref()
            .map(|t| Vector2::new(t.pos.x, t.pos.z))
            .unwrap_or(jt.location().pos);
        Ok(Some(self.add_and_spawn_ai_air(
            perf,
            spctx,
            idx,
            side,
            &ucid,
            name,
            action,
            0.,
            &WithPos {
                cfg: args.cfg.plane,
                pos: tgt,
            },
            Some(tgt),
            BitFlags::empty(),
            |db, gid, _pos| db.ai_point_to_point_mission(gid, || Task::ComboTask(vec![])),
        )?))
    }

    fn add_and_spawn_ai_air<'lua>(
        &mut self,
        perf: &mut PerfInner,
        spctx: &SpawnCtx<'lua>,
        idx: &MizIndex,
        side: Side,
        ucid: &Option<Ucid>,
        name: String,
        action: Action,
        heading: f64,
        args: &WithPos<AiPlaneCfg>,
        destination: Option<Vector2>,
        tags: BitFlags<UnitTag>,
        gen_mission: impl FnOnce(&mut Db, GroupId, Vector2) -> Result<Vec<MissionPoint<'lua>>> + 'static,
    ) -> Result<GroupId> {
        let (_, _, obj) = Self::objective_near_point(&self.persisted.objectives, args.pos, |o| {
            o.owner == side
                && !o.captureable()
                && match args.cfg.kind {
                    AiPlaneKind::Helicopter => true,
                    AiPlaneKind::FixedWing => {
                        o.is_airbase()
                            || self
                                .ephemeral
                                .cfg
                                .extra_fixed_wing_objectives
                                .contains(&o.name)
                    }
                }
                && na::distance_squared(&args.pos.into(), &o.zone.pos().into()) > 100_000_000.
        })
        .ok_or_else(|| anyhow!("no objectives available for the ai mission"))?;
        let pos = obj.zone.pos();
        let sloc = SpawnLoc::InAir {
            pos,
            heading,
            altitude: args.cfg.altitude,
            speed: args.cfg.speed,
        };
        let origin = DeployKind::Action {
            marks: FxHashSet::default(),
            loc: sloc.clone(),
            player: ucid.clone(),
            name,
            spec: action,
            time: Utc::now(),
            destination,
            rtb: Some(pos),
            origin: Some(obj.id),
            ammo: 0,
            jtac: None,
        };
        let gid = self
            .add_group(
                spctx,
                idx,
                side,
                sloc,
                &args.cfg.template,
                origin,
                tags | UnitTag::Driveable,
            )
            .context("creating group")?;
        let mission = gen_mission(self, gid, pos).context("generating mission for new unit")?;
        self.ephemeral
            .spawn_group(
                perf,
                &self.persisted,
                idx,
                spctx,
                group!(self, gid)?,
                mission,
            )
            .context("spawning group")?;
        Ok(gid)
    }

    fn awacs_mission<'lua>(
        &mut self,
        side: Side,
        ucid: Option<Ucid>,
        spawn_pos: Vector2,
        args: WithPosAndGroup<()>,
    ) -> Result<Vec<MissionPoint<'lua>>> {
        let group = group!(self, args.group)?;
        let (freq, tacan, callsign) = match &group.origin {
            DeployKind::Action { spec, name, .. } => match &spec.kind {
                ActionKind::Awacs(aw) => (
                    aw.plane.freq,
                    tacan_beacon_task(&aw.plane, name.as_ref(), BeaconSystem::TACAN),
                    callsign_command_task(&aw.plane),
                ),
                _ => (None, None, None),
            },
            _ => (None, None, None),
        };
        let init_task = if group.tags.contains(UnitTag::Link16) {
            let mut tasks = vec![
                Task::AWACS,
                Task::WrappedCommand(Command::SetUnlimitedFuel(true)),
                Task::WrappedCommand(Command::SetFrequency {
                    frequency: freq.unwrap_or(264000000),
                    modulation: Modulation::AM,
                    power: 25,
                }),
                Task::WrappedCommand(Command::EPLRS {
                    enable: true,
                    group: Some(dcso3::env::miz::GroupId::from(1)),
                }),
            ];
            if let Some(t) = tacan.clone() {
                tasks.push(t);
            }
            if let Some(t) = callsign.clone() {
                tasks.push(t);
            }
            Task::ComboTask(tasks)
        } else {
            let mut tasks = vec![
                Task::AWACS,
                Task::WrappedCommand(Command::SetFrequency {
                    frequency: freq.unwrap_or(125000000),
                    modulation: Modulation::AM,
                    power: 25,
                }),
                Task::WrappedCommand(Command::SetUnlimitedFuel(true)),
            ];
            if let Some(t) = tacan {
                tasks.push(t);
            }
            if let Some(t) = callsign {
                tasks.push(t);
            }
            Task::ComboTask(tasks)
        };
        let main_task = vec![Task::AWACS];
        self.ai_loiter_point_mission(
            side,
            ucid,
            args,
            OrbitPattern::RaceTrack,
            spawn_pos,
            |k| match k {
                ActionKind::Awacs(_) => true,
                _ => false,
            },
            move || init_task.clone(),
            move || main_task.clone(),
        )
    }

    fn move_awacs<'lua>(
        &mut self,
        spctx: &SpawnCtx<'lua>,
        side: Side,
        ucid: Option<Ucid>,
        args: WithPosAndGroup<()>,
    ) -> Result<Option<GroupId>> {
        let gid = args.group;
        let group = group!(self, gid)?;
        let pos = group_position(spctx.lua(), &group.name)?;
        let mission = self
            .awacs_mission(side, ucid, pos, args)
            .context("generating awacs mission")?;
        self.set_ai_mission(spctx, gid, mission)
            .context("setting ai mission")?;
        Ok(None)
    }

    fn awacs(
        &mut self,
        perf: &mut PerfInner,
        spctx: &SpawnCtx,
        idx: &MizIndex,
        side: Side,
        ucid: Option<Ucid>,
        name: String,
        action: Action,
        args: WithPos<AwacsCfg>,
    ) -> Result<Option<GroupId>> {
        Ok(Some(self.add_and_spawn_ai_air(
            perf,
            spctx,
            idx,
            side,
            &ucid,
            name,
            action,
            0.,
            &WithPos {
                cfg: args.cfg.plane,
                pos: args.pos,
            },
            None,
            UnitTag::AWACS.into(),
            move |db, gid, pos| {
                db.awacs_mission(
                    side,
                    ucid,
                    pos,
                    WithPosAndGroup {
                        cfg: (),
                        pos: args.pos,
                        group: gid,
                    },
                )
            },
        )?))
    }

    fn ai_loiter_point_mission<'lua>(
        &mut self,
        side: Side,
        ucid: Option<Ucid>,
        args: WithPosAndGroup<()>,
        pattern: OrbitPattern,
        spawn_point: Vector2,
        validator: impl Fn(&ActionKind) -> bool,
        init_task: impl Fn() -> Task<'lua> + 'static,
        main_task: impl Fn() -> Vec<Task<'lua>> + 'static,
    ) -> Result<Vec<MissionPoint<'lua>>> {
        let enemy = side.opposite();
        let heading = match pattern {
            OrbitPattern::Circle => 0.,
            OrbitPattern::RaceTrack => {
                racetrack_dist_and_heading(&self.persisted.objectives, args.pos, enemy).1
            }
            OrbitPattern::Custom(x) => bail!("invalid orbit pattern {x}"),
        };
        let group = group_mut!(self, args.group)?;
        if group.side != side {
            bail!("can't move the other team's awacs")
        }
        let (altitude, alt_typ, speed, marks, player) = match &mut group.origin {
            DeployKind::Action {
                marks,
                spec,
                loc,
                player,
                ..
            } => {
                if !validator(&spec.kind) {
                    bail!("this move action is not compatible with the selected group")
                }
                match &mut spec.kind {
                    ActionKind::Awacs(AwacsCfg { plane: a, .. })
                    | ActionKind::Tanker(a)
                    | ActionKind::Drone(DroneCfg { plane: a, .. })
                    | ActionKind::Recon(ReconCfg { plane: a, .. })
                    | ActionKind::CruiseMissileSpawn(a)
                    | ActionKind::Fighters(a)
                    | ActionKind::Attackers(a)
                    | ActionKind::Sead(a) => {
                        match loc {
                            SpawnLoc::InAir { pos: oldpos, .. } => {
                                let dir = *oldpos - args.pos;
                                let step = dir.magnitude() / 4.;
                                let dir = dir.normalize();
                                let (old_dist, _) = racetrack_dist_and_heading(
                                    &self.persisted.objectives,
                                    *oldpos,
                                    enemy,
                                );
                                for i in 1..4 {
                                    let pos = *oldpos + dir * (step * i as f64);
                                    let (dist, _) = racetrack_dist_and_heading(
                                        &self.persisted.objectives,
                                        pos,
                                        enemy,
                                    );
                                    if old_dist < dist && dist - old_dist >= 500. {
                                        *player = ucid.clone();
                                    }
                                }
                                *oldpos = args.pos;
                                for id in marks.drain() {
                                    self.ephemeral.msgs().delete_mark(id)
                                }
                            }
                            SpawnLoc::AtPos { .. }
                            | SpawnLoc::AtPosExact { .. }
                            | SpawnLoc::AtPosWithCenter { .. }
                            | SpawnLoc::AtPosWithComponents { .. }
                            | SpawnLoc::AtTrigger { .. } => {
                                bail!("race tracker not spawning in air")
                            }
                        }
                        (a.altitude, a.altitude_typ.clone(), a.speed, marks, player)
                    }
                    ActionKind::AttackersWaypoint
                    | ActionKind::SeadWaypoint
                    | ActionKind::AwacsWaypoint
                    | ActionKind::DroneWaypoint
                    | ActionKind::CruiseMissileWaypoint
                    | ActionKind::TankerWaypoint
                    | ActionKind::FighersWaypoint
                    | ActionKind::Move(_)
                    | ActionKind::Rtb
                    | ActionKind::Deployable(_)
                    | ActionKind::Paratrooper(_)
                    | ActionKind::Bomber(_)
                    | ActionKind::Nuke(_)
                    | ActionKind::LogisticsRepair(_)
                    | ActionKind::LogisticsTransfer(_)
                    | ActionKind::CarrierWaypoint
                    | ActionKind::CarrierRepair
                    | ActionKind::CarrierRespawn
                    | ActionKind::Artillery(_)
                    | ActionKind::NavalCruiseMissileStrike(_) => bail!("not a race tracker"),
                }
            }
            DeployKind::Crate { .. }
            | DeployKind::Deployed { .. }
            | DeployKind::Objective { .. }
            | DeployKind::ObjectiveDeprecated
            | DeployKind::Troop { .. }
            | DeployKind::DownedPilot { .. }
            | DeployKind::Dismount { .. } => bail!("not a race tracker"),
        };
        let responsible = player
            .as_ref()
            .and_then(|u| self.persisted.players.get(u))
            .map(|p| p.name.clone())
            .unwrap_or(String::from(""));
        let (point1, point2) = match pattern {
            OrbitPattern::Circle => {
                marks.insert(self.ephemeral.msgs().mark_to_side(
                    side,
                    args.pos,
                    true,
                    format_compact!(
                        "{} orbit point 1\nresponsible party: {}",
                        args.group,
                        responsible
                    ),
                ));
                (args.pos, None)
            }
            OrbitPattern::RaceTrack => {
                let point1 = args.pos
                    + pointing_towards2(change_heading(heading, -f64::consts::PI)) * 30_000.;
                let point2 = args.pos + pointing_towards2(heading) * 30_000.;
                marks.insert(self.ephemeral.msgs().mark_to_side(
                    side,
                    point1,
                    true,
                    format_compact!(
                        "{} race point 1\nresponsible party: {}",
                        args.group,
                        responsible
                    ),
                ));
                marks.insert(self.ephemeral.msgs().mark_to_side(
                    side,
                    point2,
                    true,
                    format_compact!(
                        "{} race point 2\nresponsible party: {}",
                        args.group,
                        responsible
                    ),
                ));
                (point1, Some(point2))
            }
            OrbitPattern::Custom(x) => bail!("invalid orbit pattern {x}"),
        };
        self.ephemeral.dirty();
        macro_rules! wpt {
            ($name:expr, $pos:expr, $task:expr) => {
                MissionPoint {
                    action: Some(ActionTyp::Air(TurnMethod::FlyOverPoint)),
                    typ: PointType::TurningPoint,
                    airdrome_id: None,
                    helipad: None,
                    time_re_fu_ar: None,
                    link_unit: None,
                    pos: LuaVec2($pos),
                    alt: altitude,
                    alt_typ: Some(alt_typ.clone()),
                    speed,
                    eta: None,
                    speed_locked: None,
                    eta_locked: None,
                    name: Some($name.into()),
                    task: Box::new($task),
                }
            };
        }
        match &pattern {
            OrbitPattern::Circle => {
                let mut tlist = vec![Task::Orbit {
                    pattern: OrbitPattern::Circle,
                    point: Some(LuaVec2(point1)),
                    point2: None,
                    speed: Some(speed),
                    altitude: Some(altitude),
                }];
                for t in main_task() {
                    tlist.push(t);
                }
                Ok(vec![
                    wpt!("ip", spawn_point, init_task()),
                    wpt!("orbit", point1, Task::ComboTask(tlist)),
                ])
            }
            OrbitPattern::RaceTrack => {
                let pt2 = point2.ok_or_else(|| anyhow!("racetrack requires point2"))?;
                let mut tlist = vec![Task::Orbit {
                    pattern: OrbitPattern::RaceTrack,
                    point: Some(LuaVec2(point1)),
                    point2: Some(LuaVec2(pt2)),
                    speed: Some(speed),
                    altitude: Some(altitude),
                }];
                for t in main_task() {
                    tlist.push(t);
                }
                Ok(vec![
                    wpt!("ip", spawn_point, init_task()),
                    wpt!("point1", point1, Task::ComboTask(tlist.clone())),
                    wpt!("point2", pt2, Task::ComboTask(tlist)),
                ])
            }
            OrbitPattern::Custom(x) => bail!("invalid orbit pattern {x}"),
        }
    }

    fn cruise_missile_mission<'lua>(
        &mut self,
        side: Side,
        ucid: Option<Ucid>,
        spawn_pos: Vector2,
        args: WithPosAndGroup<()>,
    ) -> Result<Vec<MissionPoint<'lua>>> {
        let group = group!(self, args.group)?;
        let (freq, tacan, callsign) = match &group.origin {
            DeployKind::Action { spec, name, .. } => match &spec.kind {
                ActionKind::CruiseMissileSpawn(pl) => (
                    pl.freq,
                    tacan_beacon_task(pl, name.as_ref(), BeaconSystem::TACAN),
                    callsign_command_task(pl),
                ),
                _ => (None, None, None),
            },
            _ => (None, None, None),
        };
        let mut init_tasks = vec![Task::WrappedCommand(Command::SetUnlimitedFuel(true))];
        if let Some(f) = freq {
            init_tasks.push(Task::WrappedCommand(Command::SetFrequency {
                frequency: f,
                modulation: Modulation::AM,
                power: 25,
            }));
        }
        if group.tags.contains(UnitTag::Link16) {
            init_tasks.push(Task::WrappedCommand(Command::EPLRS {
                enable: true,
                group: Some(dcso3::env::miz::GroupId::from(1)),
            }));
        }
        if let Some(t) = tacan {
            init_tasks.push(t);
        }
        if let Some(t) = callsign {
            init_tasks.push(t);
        }
        let init_task = Task::ComboTask(init_tasks);
        self.ai_loiter_point_mission(
            side,
            ucid,
            args,
            OrbitPattern::Circle,
            spawn_pos,
            |k| match k {
                ActionKind::CruiseMissileSpawn(_) => true,
                _ => false,
            },
            move || init_task.clone(),
            || vec![],
        )
    }

    fn set_ai_mission<'lua>(
        &mut self,
        spctx: &SpawnCtx<'lua>,
        gid: GroupId,
        mission: Vec<MissionPoint<'lua>>,
    ) -> Result<()> {
        let group = group!(self, gid)?;
        let group = Group::get_by_name(spctx.lua(), &group.name)?;
        let con = group.get_controller().context("getting controller")?;
        con.set_task(Task::Mission {
            airborne: Some(true),
            route: mission.clone(),
        })?;
        con.set_task(Task::Mission {
            airborne: Some(true),
            route: mission,
        })
        .context("setting mission")
    }

    fn bomb_targets(
        &self,
        lua: MizLua,
        side: Side,
        jtacs: &Jtacs,
        cfg: &BomberCfg,
        target: Vector2,
    ) -> Result<()> {
        let mut rng = thread_rng();
        let land = Land::singleton(lua)?;
        let act = Trigger::singleton(lua)?.action()?;
        for (i, (_, ct)) in jtacs.contacts_near_point(side, target, 15_000.).enumerate() {
            if i < cfg.targets as usize {
                let dir = Vector2::new(rng.gen_range(0. ..1.), rng.gen_range(0. ..1.)).normalize();
                let mag = rng.gen_range(0. ..cfg.accuracy as f64);
                let pos = Vector2::new(ct.pos.x, ct.pos.z) + dir * mag;
                let alt = land.get_height(LuaVec2(pos))?;
                let pos = Vector3::new(pos.x, alt, pos.y);
                act.explosion(LuaVec3(pos), cfg.power as f32)?
            }
        }
        Ok(())
    }

    fn repair_target(&mut self, target: Vector2, ucid: Option<Ucid>, side: Side) -> Result<()> {
        let (dist, _, obj) =
            Self::objective_near_point(&self.persisted.objectives, target, |o| o.owner == side)
                .ok_or_else(|| anyhow!("no friendly objective near drop off point"))?;
        if dist > 5_000. {
            bail!("no friendly objective near drop off point")
        }
        let oid = obj.id;
        if let Some(ucid) = ucid {
            self.ephemeral.stat(Stat::Repair { id: oid, by: ucid });
        }
        self.repair_one_logi_step(side, Utc::now(), oid)?;
        Ok(())
    }

    fn transfer_to_target(
        &mut self,
        lua: MizLua,
        src: Vector2,
        target: Vector2,
        ucid: Option<Ucid>,
        side: Side,
    ) -> Result<()> {
        let (dist, _, src) =
            Self::objective_near_point(&self.persisted.objectives, src, |o| o.owner == side)
                .ok_or_else(|| anyhow!("no friendly objective near source point"))?;
        if dist > 5_000. {
            bail!("no friendly objective near source point")
        }
        let (dist, _, tgt) =
            Self::objective_near_point(&self.persisted.objectives, target, |o| o.owner == side)
                .ok_or_else(|| anyhow!("no friendly objective near target point"))?;
        if dist > 5_000. {
            bail!("no friendly objective near target point")
        }
        let src = src.id;
        let tgt = tgt.id;
        if let Some(ucid) = ucid {
            self.ephemeral.stat(Stat::SupplyTransfer {
                from: src,
                to: tgt,
                by: ucid,
            });
        }
        self.transfer_supplies(lua, src, tgt)
    }

    fn deployable_to_point(
        &mut self,
        lua: MizLua,
        idx: &MizIndex,
        pos: Vector2,
        dep: String,
        side: Side,
        ucid: Ucid,
    ) -> Result<()> {
        let spec = self
            .ephemeral
            .deployable_idx
            .get(&side)
            .ok_or_else(|| anyhow!("no such deployable {dep} for {side}"))?
            .deployables_by_name
            .get(dep.as_str())
            .ok_or_else(|| anyhow!("no such deployable {dep} for {side}"))?
            .clone();
        let (n, oldest) = self.number_deployed(side, &**dep)?;
        if n >= spec.limit as usize {
            match spec.limit_enforce {
                LimitEnforceTyp::DenyCrate => {
                    bail!("the max number of {:?} are already deployed", dep)
                }
                LimitEnforceTyp::DeleteOldest => match oldest {
                    Some(Oldest::Group(gid)) => self.delete_group(&gid)?,
                    Some(Oldest::Objective(oid)) => self.delete_objective(&oid)?,
                    None => (),
                },
            }
        }
        let spctx = SpawnCtx::new(lua)?;
        if let Err(e) = spctx.remove_scenery(pos, 50.) {
            warn!("could not clear scenery at deploy point: {e:?}");
        }
        let spawnloc = SpawnLoc::AtPos {
            pos,
            offset_direction: Vector2::new(1., 0.),
            group_heading: 0.,
        };
        match &spec.kind {
            DeployableKind::Objective(parts) => {
                let oid = self.add_farp(lua, &spctx, idx, side, pos, &spec, parts)?;
                self.ephemeral.stat(Stat::DeployFarp {
                    by: ucid,
                    oid,
                    deployable: spec.path.last()
                        .ok_or_else(|| anyhow!("deployable has empty path"))?.clone(),
                });
                Ok(())
            }
            DeployableKind::Group { template } => {
                let origin = DeployKind::Deployed {
                    player: ucid,
                    moved_by: None,
                    spec: spec.clone(),
                    cost_fraction: 1.,
                    origin: None,
                    jtac: None,
                };
                let gid = self.add_and_queue_group(
                    &spctx,
                    idx,
                    side,
                    spawnloc,
                    template,
                    origin,
                    BitFlags::empty(),
                    None,
                )?;
                self.ephemeral.stat(Stat::DeployGroup {
                    gid,
                    deployable: dep,
                    by: ucid,
                });
                Ok(())
            }
        }
    }

    fn paratroops_to_point(
        &mut self,
        lua: MizLua,
        idx: &MizIndex,
        pos: Vector2,
        troop: String,
        side: Side,
        ucid: Ucid,
        origin: ObjectiveId,
    ) -> Result<()> {
        let troop_cfg = self
            .ephemeral
            .deployable_idx
            .get(&side)
            .ok_or_else(|| anyhow!("no such troop {troop} for {side}"))?
            .squads_by_name
            .get(troop.as_str())
            .ok_or_else(|| anyhow!("no such troop {troop} for {side}"))?
            .clone();
        let spawnpos = SpawnLoc::AtPos {
            pos,
            offset_direction: Vector2::new(1., 0.),
            group_heading: 0.,
        };
        let dk = DeployKind::Troop {
            player: ucid.clone(),
            moved_by: None,
            spec: troop_cfg.clone(),
            origin: Some(origin),
            cost_fraction: 1.,
            jtac: None,
        };
        let spctx = SpawnCtx::new(lua)?;
        let (n, oldest) = self.number_troops_deployed(side, troop_cfg.name.as_str())?;
        let to_delete = if n < troop_cfg.limit as usize {
            None
        } else {
            match troop_cfg.limit_enforce {
                LimitEnforceTyp::DeleteOldest => oldest,
                LimitEnforceTyp::DenyCrate => {
                    bail!(
                        "the maximum number of {} troops are already deployed",
                        troop_cfg.name
                    )
                }
            }
        };
        if let Some(gid) = to_delete {
            self.delete_group(&gid)?
        }
        let gid = self.add_and_queue_group(
            &spctx,
            idx,
            side,
            spawnpos,
            &*troop_cfg.template,
            dk,
            BitFlags::empty(),
            None,
        )?;
        self.ephemeral.stat(Stat::DeployTroop {
            troop,
            by: ucid,
            gid,
        });
        Ok(())
    }


    pub fn advance_actions(
        &mut self,
        lua: MizLua,
        idx: &MizIndex,
        jtacs: &Jtacs,
        now: DateTime<Utc>,
    ) -> Result<()> {
        let mut to_delete: SmallVec<[GroupId; 4]> = smallvec![];
        let mut to_bomb: SmallVec<[(BomberCfg, Vector2, Side); 2]> = smallvec![];
        let mut to_repair: SmallVec<[(Vector2, Option<Ucid>, Side); 2]> = smallvec![];
        let mut to_transfer: SmallVec<[(Vector2, Vector2, Option<Ucid>, Side); 2]> = smallvec![];
        let mut to_deploy: SmallVec<[(Vector2, String, Side, Ucid); 2]> = smallvec![];
        let mut to_paratroop: SmallVec<[(Vector2, String, Side, Ucid, ObjectiveId); 2]> =
            smallvec![];
        macro_rules! at_dest {
            ($group:expr, $dest:expr, $radius:expr) => {{
                let r2 = f64::powi($radius, 2);
                let mut iter = $group.units.into_iter();
                loop {
                    match iter.next() {
                        None => break false,
                        Some(uid) => {
                            let unit = unit!(self, uid)?;
                            if na::distance_squared(&unit.pos.into(), &$dest.into()) <= r2 {
                                break true;
                            }
                        }
                    }
                }
            }};
        }


        for gid in &self.persisted.actions {
            let group = group_mut!(self, gid)?;

            if let DeployKind::Action {
                spec,
                time,
                destination,
                rtb,
                player,
                origin,
                ..
            } = &mut group.origin
            {
                match &spec.kind {
                    ActionKind::Awacs(AwacsCfg { plane: ai, .. })
                    | ActionKind::Fighters(ai)
                    | ActionKind::Attackers(ai)
                    | ActionKind::CruiseMissileSpawn(ai)
                    | ActionKind::Drone(DroneCfg { plane: ai, .. })
                    | ActionKind::Recon(ReconCfg { plane: ai, .. })
                    | ActionKind::Tanker(ai) => {
                        if let Some(d) = ai.duration {
                            if now - *time > Duration::hours(d as i64) {
                                to_delete.push(*gid);
                            }
                        }
                    }
                    ActionKind::Sead(ai) => {
                        // Check duration first
                        if let Some(d) = ai.duration {
                            if now - *time > Duration::hours(d as i64) {
                                to_delete.push(*gid);
                                continue;
                            }
                        }
                        // SEAD groups now require manual RTB - no automatic RTB based on ammunition
                    }
                    ActionKind::Bomber(b) => {
                        if let Some(target) = *destination {
                            if at_dest!(group, target, 10_000.) {
                                destination.take();
                                to_bomb.push((b.clone(), target, group.side));
                            }
                        }
                        if destination.is_none() {
                            if let Some(target) = *rtb {
                                if at_dest!(group, target, 10_000.) {
                                    to_delete.push(*gid);
                                }
                            }
                        }
                    }
                    ActionKind::Rtb => {
                        if let Some(target) = *rtb {
                            if at_dest!(group, target, 10_000.) {
                                to_delete.push(*gid);
                                match player {
                                    Some(u) => match self.persisted.players.get_mut_cow(u) {
                                        Some(p) => {
                                            p.points += (spec.cost as f64 * 0.25).ceil() as i32;
                                            self.ephemeral.msgs().panel_to_side(
                                                5,
                                                false,
                                                p.side,
                                                format_compact!(
                                                    "{}'s {} has RTB'd. points refunded: {}",
                                                    p.name,
                                                    group.name,
                                                    (spec.cost as f64 * 0.25).ceil() as i32,
                                                ),
                                            );
                                        }
                                        None => (),
                                    },
                                    None => (),
                                }
                            }
                        }
                    }
                    ActionKind::LogisticsRepair(_) => {
                        if let Some(target) = *destination {
                            if at_dest!(group, target, 800.) {
                                destination.take();
                                to_repair.push((target, *player, group.side));
                                to_delete.push(group.id);
                            }
                        }
                    }
                    ActionKind::LogisticsTransfer(_) => {
                        if let Some(target) = *destination {
                            if at_dest!(group, target, 800.) {
                                destination.take();
                                if let Some(rtb) = *rtb {
                                    to_transfer.push((rtb, target, *player, group.side));
                                    to_delete.push(group.id);
                                }
                            }
                        }
                    }
                    ActionKind::Paratrooper(t) => {
                        if let Some(target) = *destination {
                            if at_dest!(group, target, 800.) {
                                destination.take();
                                let ucid = player
                                    .ok_or_else(|| anyhow!("paratroop missions require a ucid"))?;
                                let origin = (*origin).ok_or_else(|| {
                                    anyhow!("objective origin is required for paratroops")
                                })?;
                                to_paratroop.push((
                                    target,
                                    t.name.clone(),
                                    group.side,
                                    ucid,
                                    origin,
                                ));
                            }
                        }
                        if destination.is_none() {
                            if let Some(target) = *rtb {
                                if at_dest!(group, target, 800.) {
                                    to_delete.push(*gid);
                                }
                            }
                        }
                    }
                    ActionKind::Deployable(d) => {
                        if let Some(target) = *destination {
                            if at_dest!(group, target, 800.) {
                                destination.take();
                                let ucid = player.as_ref().map(|u| u.clone()).ok_or_else(|| {
                                    anyhow!("deployables missions require a ucid")
                                })?;
                                to_deploy.push((target, d.name.clone(), group.side, ucid));
                            }
                        }
                        if destination.is_none() {
                            if let Some(target) = *rtb {
                                if at_dest!(group, target, 800.) {
                                    to_delete.push(*gid);
                                }
                            }
                        }
                    }
                    ActionKind::Move(_) => {
                        self.ephemeral.groups_with_move_missions.retain(|gid, dst| {
                            match self.persisted.groups.get(gid) {
                                None => false,
                                Some(group) => {
                                    let pos = centroid2d(
                                        group
                                            .units
                                            .into_iter()
                                            .filter_map(|uid| self.persisted.units.get(uid))
                                            .map(|u| u.pos),
                                    );
                                    if (pos - *dst).magnitude() > 100. {
                                        true
                                    } else {
                                        for uid in &group.units {
                                            match self.persisted.units.get(uid) {
                                                None => {
                                                    self.ephemeral
                                                        .units_able_to_move
                                                        .swap_remove(uid);
                                                }
                                                Some(unit) => {
                                                    if !unit.tags.contains(UnitTag::Driveable) && !unit.tags.contains(UnitTag::Boat) {
                                                        self.ephemeral
                                                            .units_able_to_move
                                                            .swap_remove(uid);
                                                    }
                                                }
                                            }
                                        }
                                        false
                                    }
                                }
                            }
                        });
                    }
                    ActionKind::AwacsWaypoint
                    | ActionKind::FighersWaypoint
                    | ActionKind::AttackersWaypoint
                    | ActionKind::SeadWaypoint
                    | ActionKind::CruiseMissileWaypoint
                    | ActionKind::TankerWaypoint
                    | ActionKind::DroneWaypoint
                    | ActionKind::Nuke(_)
                    | ActionKind::CarrierWaypoint
                    | ActionKind::CarrierRepair
                    | ActionKind::CarrierRespawn
                    | ActionKind::Artillery(_)
                    | ActionKind::NavalCruiseMissileStrike(_) => {
                        bail!("should not be a group")
                    }
                }
            }
        }

        for gid in to_delete {
            if let Err(e) = self.delete_group(&gid) {
                error!("delete action group failed {e:?}")
            }
        }
        for (cfg, target, side) in to_bomb {
            if let Err(e) = self.bomb_targets(lua, side, jtacs, &cfg, target) {
                error!("bomb targets failed {e:?}")
            }
        }
        for (target, ucid, side) in to_repair {
            if let Err(e) = self.repair_target(target, ucid, side) {
                self.ephemeral.msgs().panel_to_side(
                    10,
                    false,
                    side,
                    format_compact!("repair mission failed {e:?}"),
                );
            }
        }
        for (src, target, ucid, side) in to_transfer {
            if let Err(e) = self.transfer_to_target(lua, src, target, ucid, side) {
                self.ephemeral.msgs().panel_to_side(
                    10,
                    false,
                    side,
                    format_compact!("transfer mission failed {e:?}"),
                );
            }
        }
        for (dst, troop, side, ucid, origin) in to_paratroop {
            if let Err(e) =
                self.paratroops_to_point(lua, idx, dst, troop, side, ucid.clone(), origin)
            {
                self.ephemeral.panel_to_player(
                    &self.persisted,
                    10,
                    &ucid,
                    format_compact!("paratroop mission failed {e:?}"),
                )
            }
        }
        for (dst, dep, side, ucid) in to_deploy {
            if let Err(e) = self.deployable_to_point(lua, idx, dst, dep, side, ucid.clone()) {
                self.ephemeral.panel_to_player(
                    &self.persisted,
                    10,
                    &ucid,
                    format_compact!("deploy mission failed {e:?}"),
                )
            }
        }
        Ok(())
    }

    /// Player-requested indirect fire support. Finds nearby friendly Armor/Mr/Lr
    /// groups within `cfg.max_range_m` and issues `Task::FireAtPoint` toward
    /// `args.pos`. Up to `cfg.max_groups` groups fire simultaneously.
    fn artillery_strike(
        &mut self,
        lua: MizLua,
        side: Side,
        _ucid: Option<Ucid>,
        args: WithPos<ArtilleryCfg>,
    ) -> Result<Option<GroupId>> {
        let cfg = args.cfg.clone();
        let target_pos = args.pos;

        let land = Land::singleton(lua)?;
        let alt = land.get_height(LuaVec2(target_pos)).unwrap_or(0.);

        // Collect groups with alive Artillery/Launcher units within their configured range.
        // Works for ground artillery, missile launchers, and naval units — any unit type
        // listed in cfg.units or tagged Artillery/Launcher uses its configured range.
        let mut candidates: Vec<(GroupId, f64)> = Vec::new();
        let mut too_close = false;
        {
            let group_ids: Vec<GroupId> = self
                .persisted
                .groups_by_side
                .get(&side)
                .map(|s| s.into_iter().copied().collect())
                .unwrap_or_default();

            for gid in group_ids {
                let group = match self.persisted.groups.get(&gid) {
                    Some(g) => g,
                    None => continue,
                };
                // Find the best (longest) range from alive Artillery/Launcher units in this group.
                let best_range = group
                    .units
                    .into_iter()
                    .filter_map(|uid| self.persisted.units.get(uid))
                    .filter(|u| {
                        !u.dead
                            && (u.tags.0.contains(UnitTag::Artillery)
                                || u.tags.0.contains(UnitTag::Launcher))
                    })
                    .map(|u| {
                        cfg.units
                            .get(u.typ.as_str())
                            .map(|r| (r.max_range_m, r.min_range_m))
                            .unwrap_or((cfg.default_max_range_m, cfg.default_min_range_m))
                    })
                    .max_by(|a, b| a.0.partial_cmp(&b.0).unwrap_or(std::cmp::Ordering::Equal));

                let (max_range, min_range) = match best_range {
                    None => continue, // no alive artillery/launcher units in this group
                    Some(r) => r,
                };
                let center = self.group_center(&gid).unwrap_or_default();
                let dist = na::distance(&center.into(), &target_pos.into());
                if min_range > 0.0 && dist < min_range {
                    too_close = true;
                } else if dist <= max_range {
                    candidates.push((gid, dist));
                }
            }
        }

        if candidates.is_empty() {
            if too_close {
                bail!("target is too close for available artillery");
            }
            bail!("no friendly artillery/missiles in range of that position");
        }

        // Sort by distance (closest first) and cap at max_groups.
        candidates.sort_by(|a, b| a.1.partial_cmp(&b.1).unwrap_or(std::cmp::Ordering::Equal));
        candidates.truncate(cfg.max_groups);

        let fire_task = Task::FireAtPoint {
            point: LuaVec2(target_pos),
            radius: Some(cfg.radius_m),
            expend_qty: None,
            weapon_type: None,
            altitude: Some(alt),
            altitude_type: Some(AltType::BARO),
        };

        let mut fired = 0u32;
        for (gid, _) in &candidates {
            let group_name = match self.persisted.groups.get(gid) {
                Some(g) => g.name.clone(),
                None => continue,
            };
            let dcs_group = match Group::get_by_name(lua, group_name.as_str()) {
                std::result::Result::Ok(g) => g,
                std::result::Result::Err(_) => continue,
            };
            let controller = match dcs_group.get_controller() {
                std::result::Result::Ok(c) => c,
                std::result::Result::Err(e) => {
                    error!("artillery_strike: get_controller {group_name}: {e}");
                    continue;
                }
            };
            match controller.set_task(fire_task.clone()) {
                std::result::Result::Err(e) => {
                    error!("artillery_strike: set_task {group_name}: {e}");
                }
                std::result::Result::Ok(()) => {
                    info!(
                        "artillery_strike: ordered {:?} group {} to fire at {:?}",
                        side, group_name, target_pos
                    );
                    fired += 1;
                }
            }
        }

        if fired == 0 {
            bail!("all artillery groups are unavailable or out of range right now");
        }

        // Place a temporary F10 overlay: trajectory line + impact circle + label.
        // Gun centroid is the average position of the firing candidates.
        let gun_pos = {
            let sum = candidates.iter().fold(Vector2::zeros(), |acc, (gid, _)| {
                acc + self.group_center(gid).unwrap_or_default()
            });
            sum / candidates.len() as f64
        };
        self.ephemeral.on_fire_mission(
            gun_pos,
            target_pos,
            cfg.radius_m.max(500.),
            fired,
            side,
            Utc::now(),
        );

        // Wake up any culled objective whose zone contains the target position so its
        // units are present in DCS to absorb the incoming fire.  We search for the
        // objective (owned by the enemy side) that either contains target_pos directly
        // or is closest to it within a generous 3 km fallback radius.
        {
            let now = Utc::now();
            let fallback_sq = 3000.0_f64.powi(2);
            let mut best: Option<(ObjectiveId, f64)> = None;
            for (oid, obj) in &self.persisted.objectives {
                if obj.owner == side {
                    continue;
                }
                let d = if obj.zone.contains(target_pos) {
                    0.0_f64
                } else {
                    let d = na::distance_squared(&obj.zone.pos().into(), &target_pos.into());
                    if d > fallback_sq {
                        continue;
                    }
                    d
                };
                if best.as_ref().map(|(_, bd)| d < *bd).unwrap_or(true) {
                    best = Some((*oid, d));
                }
            }
            if let Some((oid, _)) = best {
                self.ephemeral.artillery_targeted.insert(oid, now);
                info!("artillery_strike: marking objective {:?} as targeted for respawn", oid);
            }
        }

        Ok(None)
    }
}
