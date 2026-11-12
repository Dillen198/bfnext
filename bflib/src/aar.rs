//! Air-to-air refuelling cues for the cockpit plugin's director-light
//! overlay (see `bfprotocols::aar` for what a cue is and where it comes
//! from).
//!
//! Five times a second: every friendly tanker DCS lists as a tanker service
//! (AI tankers on their Tanker task, whoever spawned them), every player in
//! the air near one, and for a KC-135 its director-light model arguments. The
//! cues go to the background thread, which publishes them at
//! `<base>/<sortie>/aar-cues` for bfdb. Nothing is read from DCS for players
//! nowhere near a tanker, and nothing is sent while nobody is near one.
//!
//! REFUELING events mark a receiver connected, and each new contact teaches
//! the tracker where that receiver type sits behind that tanker type.
//! What it has learned is kept in `<Saved Games profile>/fowl_aar_contacts.json`.

use crate::{Context, bg::Task};
use bfprotocols::aar::{self, Learned, ReceiverObs, TankerObs, Tracker};
use dcso3::{
    MizLua,
    coalition::{Coalition, Service, Side},
    lfs::Lfs,
    timer::Timer,
    unit::Unit,
};
use log::{error, warn};
use std::path::PathBuf;

const HZ: f64 = 5.;
/// How often the tanker list is read from DCS, seconds.
const TANKER_LIST_SECS: f64 = 2.;
/// Players further than this (by their last known position, which the
/// incremental position update can leave a few seconds old) are not looked
/// at.
const PREFILTER_M: f64 = 8000.;
/// Learned contacts are written at most this often, seconds.
const SAVE_SECS: f64 = 60.;

#[derive(Debug, Clone)]
struct TankerRef {
    unit: String,
    callsign: String,
    typ: String,
    side: Side,
}

#[derive(Debug, Default)]
pub(crate) struct AarState {
    tracker: Option<Tracker>,
    path: Option<PathBuf>,
    tankers: Vec<TankerRef>,
    tankers_at: f64,
    saved_at: f64,
    unsaved: bool,
    /// The last tick published something: one empty update follows, then
    /// silence.
    published: bool,
}

pub(crate) fn side_u8(s: Side) -> u8 {
    match s {
        Side::Neutral => 0,
        Side::Red => 1,
        Side::Blue => 2,
    }
}

fn read_tankers(lua: MizLua) -> Vec<TankerRef> {
    let mut out = vec![];
    let Ok(co) = Coalition::singleton(lua) else { return out };
    for side in [Side::Red, Side::Blue] {
        let Ok(units) = co.get_service_providers(side, Service::Tanker) else { continue };
        for u in units.into_iter().flatten() {
            let (Ok(unit), Ok(typ)) = (u.get_name(), u.get_type_name()) else { continue };
            if bfprotocols::range::cfg::tanker_type(&typ).is_none() {
                continue;
            }
            let callsign = u
                .get_callsign()
                .map(|c| aar::pretty_callsign(&c))
                .unwrap_or_else(|_| unit.to_string());
            out.push(TankerRef {
                unit: unit.to_string(),
                callsign,
                typ: typ.to_string(),
                side,
            });
        }
    }
    out
}

fn observe_tanker(lua: MizLua, t: &TankerRef) -> Option<TankerObs> {
    let u = Unit::get_by_name(lua, &t.unit).ok()?;
    let pos = u.get_position().ok()?;
    let vel = u.get_velocity().ok()?;
    let pdl = aar::has_pdl_args(&t.typ).then(|| {
        (
            u.get_draw_argument_value(aar::PDL_UD_ARG).unwrap_or(0.),
            u.get_draw_argument_value(aar::PDL_FA_ARG).unwrap_or(0.),
        )
    });
    let v = |v: dcso3::Vector3| [v.x, v.y, v.z];
    Some(TankerObs {
        unit: t.unit.clone(),
        callsign: t.callsign.clone(),
        typ: t.typ.clone(),
        side: side_u8(t.side),
        p: v(pos.p.0),
        x: v(pos.x.0),
        y: v(pos.y.0),
        z: v(pos.z.0),
        vel: v(vel.0),
        pdl,
    })
}

fn tick(lua: MizLua, ctx: &mut Context, now: f64) {
    let st = &mut ctx.aar;
    if st.tracker.is_none() {
        st.path = Lfs::singleton(lua)
            .and_then(|l| l.writedir())
            .ok()
            .map(|d| PathBuf::from(d.as_str()).join("fowl_aar_contacts.json"));
        let learned = st.path.as_deref().map(Learned::load).unwrap_or_default();
        st.tracker = Some(Tracker::new(learned));
    }
    if now - st.tankers_at >= TANKER_LIST_SECS || now < st.tankers_at {
        st.tankers = read_tankers(lua);
        st.tankers_at = now;
    }
    let tankers: Vec<TankerObs> =
        st.tankers.iter().filter_map(|t| observe_tanker(lua, t)).collect();
    let mut receivers = vec![];
    if !tankers.is_empty() {
        for (ucid, player, inst) in ctx.db.instanced_players() {
            if !inst.in_air {
                continue;
            }
            let side = side_u8(player.side);
            let p = inst.position.p.0;
            let near = tankers.iter().any(|t| {
                t.side == side
                    && ((p.x - t.p[0]).powi(2) + (p.z - t.p[2]).powi(2)).sqrt()
                        < PREFILTER_M
            });
            if !near {
                continue;
            }
            let Ok(u) = Unit::get_by_name(lua, &inst.unit_name) else { continue };
            let (Ok(pos), Ok(vel)) = (u.get_point(), u.get_velocity()) else { continue };
            receivers.push(ReceiverObs {
                key: ucid.to_string(),
                unit: inst.unit_name.to_string(),
                typ: inst.typ.to_string(),
                side,
                p: [pos.0.x, pos.0.y, pos.0.z],
                vel: [vel.0.x, vel.0.y, vel.0.z],
            });
        }
    }
    let st = &mut ctx.aar;
    let Some(tracker) = st.tracker.as_mut() else { return };
    let cues = tracker.update(now, &tankers, &receivers);
    if tracker.take_dirty() {
        st.unsaved = true;
    }
    if st.unsaved && now - st.saved_at >= SAVE_SECS {
        if let Some(path) = &st.path
            && let Err(e) = tracker.learned.save(path)
        {
            warn!("aar: saving learned contacts to {path:?}: {e:?}");
        }
        st.unsaved = false;
        st.saved_at = now;
    }
    let any = !cues.cues.is_empty();
    if any || st.published {
        st.published = any;
        match serde_json::to_string(&cues) {
            Ok(json) => ctx.do_bg_task(Task::AarCues(json)),
            Err(e) => error!("aar: serialising cues: {e:?}"),
        }
    }
}

/// REFUELING (`on`) or REFUELING_STOP, by the initiator's unit name.
pub(crate) fn refuel_event(ctx: &mut Context, unit: &str, on: bool) {
    if let Some(t) = ctx.aar.tracker.as_mut() {
        t.refuel_event(unit, on);
    }
}

/// Start the 5 Hz timer.
pub(crate) fn start(lua: MizLua) -> anyhow::Result<()> {
    let timer = Timer::singleton(lua)?;
    let period = 1. / HZ;
    timer.schedule_function(
        timer.get_time()? + period,
        mlua::Value::Nil,
        move |lua, _, now| {
            let Some(mut ctx) = Context::borrow_logged("aar cues") else {
                return Ok(Some(now + period));
            };
            if ctx.exit_attempts.is_some() {
                return Ok(None);
            }
            let ctx = &mut *ctx;
            if let Err(e) = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
                tick(lua, ctx, now.0)
            })) {
                error!("aar cues panicked: {}", crate::bg::panic_msg(&e));
            }
            Ok(Some(now + period))
        },
    )?;
    Ok(())
}
