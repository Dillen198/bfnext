use super::ArgTriple;
use crate::{Context, db::{group::DeployKind, Db}};
use anyhow::{Context as ErrContext, Result};
use bfprotocols::cfg::{ActionKind, AwacsCfg};
use compact_str::format_compact;
use dcso3::{
    coalition::Side,
    env::miz::GroupId,
    mission_commands::{GroupSubMenu, MissionCommands},
    net::SlotId,
    MizLua,
};
use std::fmt::Write;

fn build_sitrep(db: &Db, side: Side) -> compact_str::CompactString {
    let enemy_side = side.opposite();
    let friendly_primary = db
        .objectives()
        .filter(|(_, o)| {
            o.owner() == side
                && matches!(
                    o.kind(),
                    bfprotocols::db::objective::ObjectiveKind::Airbase
                        | bfprotocols::db::objective::ObjectiveKind::NavalBase
                        | bfprotocols::db::objective::ObjectiveKind::Farp { .. }
                )
        })
        .count();
    let enemy_primary = db
        .objectives()
        .filter(|(_, o)| {
            o.owner() == enemy_side
                && matches!(
                    o.kind(),
                    bfprotocols::db::objective::ObjectiveKind::Airbase
                        | bfprotocols::db::objective::ObjectiveKind::NavalBase
                        | bfprotocols::db::objective::ObjectiveKind::Farp { .. }
                )
        })
        .count();
    let total_friendly = db.objectives().filter(|(_, o)| o.owner() == side).count();
    let total_enemy = db.objectives().filter(|(_, o)| o.owner() == enemy_side).count();

    let mut report = format_compact!("=== Situation Report ===\n");
    let _ = write!(
        report,
        "{side:?}: {total_friendly} objectives ({friendly_primary} primary)\n\
         {enemy_side:?}: {total_enemy} objectives ({enemy_primary} primary)\n"
    );

    // Last stand timer status
    if let Some((arm_time, losing_side)) = db.ephemeral.last_stand_state {
        if let Some(cfg) = &db.ephemeral.cfg.last_stand {
            let elapsed = chrono::Utc::now() - arm_time;
            let remaining = chrono::Duration::seconds(cfg.countdown_secs as i64) - elapsed;
            let remaining_secs = remaining.num_seconds().max(0);
            let _ = write!(
                report,
                "LAST STAND: {losing_side:?} — {remaining_secs}s remaining\n"
            );
        }
    }

    // Supply-critical objectives (lowest 5)
    let mut low_supply: Vec<_> = db
        .objectives()
        .filter(|(_, o)| o.owner() == side)
        .map(|(_, o)| (o.name().to_string(), o.supply()))
        .collect();
    low_supply.sort_by_key(|(_, s)| *s);
    low_supply.truncate(5);
    if !low_supply.is_empty() {
        let _ = write!(report, "\nLowest supply:\n");
        for (name, supply) in &low_supply {
            let _ = write!(report, "  {name}: {supply}%\n");
        }
    }
    report
}

fn build_frequencies(db: &Db, side: Side) -> compact_str::CompactString {
    let mut report = format_compact!("=== Frequencies ===\n");
    let mut found_any = false;
    for (_, group) in &db.persisted.groups {
        if group.side != side {
            continue;
        }
        if let DeployKind::Action { spec, .. } = &group.origin {
            match &spec.kind {
                ActionKind::Awacs(AwacsCfg { plane, .. }) => {
                    if let Some(freq) = plane.freq {
                        let mhz = freq as f64 / 1_000_000.0;
                        let _ = write!(report, "AWACS [{name}]: {mhz:.3} MHz\n", name = group.name);
                        found_any = true;
                    }
                }
                ActionKind::Tanker(plane) => {
                    if let Some(freq) = plane.freq {
                        let mhz = freq as f64 / 1_000_000.0;
                        let _ = write!(report, "TANKER [{name}]: {mhz:.3} MHz\n", name = group.name);
                        found_any = true;
                    }
                }
                _ => {}
            }
        }
    }
    if !found_any {
        let _ = write!(report, "No active AWACS or tankers.\n");
    }
    report
}

fn sitrep(_lua: MizLua, arg: ArgTriple<GroupId, Side, u8>) -> Result<()> {
    let ctx = unsafe { Context::get_mut() };
    let report = build_sitrep(&ctx.db, arg.snd);
    ctx.db.ephemeral.msgs().panel_to_group(20, false, arg.fst, report);
    Ok(())
}

fn frequencies(_lua: MizLua, arg: ArgTriple<GroupId, Side, u8>) -> Result<()> {
    let ctx = unsafe { Context::get_mut() };
    let report = build_frequencies(&ctx.db, arg.snd);
    ctx.db.ephemeral.msgs().panel_to_group(20, false, arg.fst, report);
    Ok(())
}

pub(super) fn init_info_menu_for_slot(
    ctx: &mut Context,
    lua: MizLua,
    slot: &SlotId,
) -> Result<()> {
    let mc = MissionCommands::singleton(lua)?;
    let si = ctx
        .db
        .ephemeral
        .get_slot_info(slot)
        .context("getting slot info")?;
    let miz_gid = si.miz_gid;
    let side = si.side;

    mc.remove_submenu_for_group(miz_gid, GroupSubMenu::from(vec!["Info".into()]))?;
    let root = mc.add_submenu_for_group(miz_gid, "Info".into(), None)?;

    mc.add_command_for_group(
        miz_gid,
        "Situation Report".into(),
        Some(root.clone()),
        sitrep,
        ArgTriple { fst: miz_gid, snd: side, trd: 0u8 },
    )?;

    mc.add_command_for_group(
        miz_gid,
        "Frequencies".into(),
        Some(root.clone()),
        frequencies,
        ArgTriple { fst: miz_gid, snd: side, trd: 0u8 },
    )?;

    Ok(())
}
