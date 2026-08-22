use super::{ArgTriple, ArgTuple};
use crate::{atis, Context, db::{group::DeployKind, Db}};
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
use log::error;
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

fn weather(lua: MizLua, arg: ArgTuple<GroupId, SlotId>) -> Result<()> {
    if let Err(e) = atis::send_full_weather(lua, arg.snd) {
        error!("full weather report failed for slot {:?}: {:?}", arg.snd, e);
    }
    Ok(())
}

// ── Help ─────────────────────────────────────────────────────────────────
// Static reference text, split into topics so each panel message stays
// readable instead of one huge wall of text. Content here should only state
// things confirmed against the actual campaign code/config -- if a mechanic
// changes, update the matching source rather than letting this drift.

const HELP_GETTING_STARTED: &str = "\
=== Getting Started ===
Slot into any aircraft to join the fight for Blue or Red.

Your F10 radio menu is the main toolkit:
 - Cargo / C-130 Cargo: deliver crates -- repairs, fuel/weapons resupply, deployable defenses
 - Troops: load and deploy ground troops
 - Actions: call in AI support, artillery fires, and special missions
 - JTAC: request 9-lines and target info from ground controllers
 - Objectives: check nearby base status
 - Info: this Help menu, Situation Report, Frequencies, Weather
 - EWR: early warning radar contact reports

You earn points for kills, captures, repairs, and deployments -- spend them
on deployables and actions. Check your total with -balance in chat, and
your remaining lives for this round with -lives.";

const HELP_CHAT_COMMANDS: &str = "\
=== Chat Commands ===
Type these directly in the DCS chat window (F10 default key, or your bound key).
 -switch <color>   side switch to blue/red (spectators only, limited per round)
 -lives            your remaining lives this round
 -time             time until next server restart
 -weather          full weather report for your slot (winds/temp aloft)
 -balance          your points balance
 -status           campaign status: side, points, kill streak, objectives, convoys
 -transfer <amt> <player>            send points to another player
 -transfer <amt> objective:<name>    donate points to fund an objective's logistics
 -delete <groupid> delete a group you deployed, partial refund
 -action <name> <args>   run a commander action; \"-action help\" lists them
 -jtac <id> status       request a 9-line from that JTAC
 -jtac <id> shift        manually shift the JTAC to its next target
 -jtac <id> autoshift    toggle automatic target shifting
 -jtac <id> smoke        have the JTAC smoke the current target
 -bind <token>           bind your account to the web dashboard
 -help                   show this list in chat";

const HELP_CARGO_LOGISTICS: &str = "\
=== Cargo & Logistics ===
Every base tracks four numbers, and they are NOT the same thing:
 - Health / Logistics: % of defending units still alive (defense strength)
 - Supply / Fuel: % of warehouse stock remaining (a resource level)
Logistics is not fuel -- it tracks whether the base's logistics-defense
group is alive, not how much fuel is sitting in the warehouse.

Secure rear bases resupply automatically. Forward/contested bases are
resupplied by physical truck convoys instead -- protect your own, or hunt
the enemy's on the map.

Crates (Cargo / C-130 Cargo menu): spawn one, carry it to the target base,
then Unpack it there.
 - Logistics Crate: revives dead logistics-defense units at a base
 - Fuel/Weapons Transfer: tops off warehouse stock directly
 - Deployable crates: build SAM sites, vehicles, and other defenses
A crate delivered with no effect (e.g. nothing left to revive) will tell you
so honestly instead of just saying \"delivered\".";

const HELP_OBJECTIVES: &str = "\
=== Objectives & Capturing ===
Each base's F10 map label shows Health, Logistics(Logi), Supply, and Fuel,
plus a live \"Repairing: X% (ETA ...)\" line while it's actively healing.
The inner ring on the map marker turns white once a base is capturable.

HOW TO CAPTURE A BASE:
1. Reduce it to capturable: Health at or below 20% AND zero infantry
   defenders left (some servers also require a further share of total
   defenders destroyed).
2. Deploy capture-capable troops (Troops menu) and get them physically
   alive into the objective's zone. All your troops in the zone must be
   the same side.
3. A capture timer starts (length set by the server) -- both sides get a
   warning message. Killing your troops before it completes resets your
   progress, so the enemy can still stop you.

Special SAM sites work differently: they're always eligible once fully
destroyed (Health 0), and capture there is INSTANT once your troops are in
the zone -- no timer.

F10 map icons: aircraft = Airbase, tent = FOB, \"H\" = FARP,
hexagon = Logistics Hub, factory silhouette = Factory, star = Command
Center, anchor = Naval Base, diamond = SAM site.";

const HELP_CARRIER_GROUPS: &str = "\
=== Carrier Groups ===
Carriers are NOT captured the normal way -- health/defenses alone never
make one capturable. It's a two-step process:

1. SINK THE SUPPLY SHIP. Every carrier group has one escort vessel with
   \"SUPPLY\" in its name -- that's the ship that actually matters. Destroy
   it and the carrier's Logistics (Logi) stat drops to 0%, meaning the
   carrier is now \"dead in the water\" and eligible for boarding. The
   carrier's own health/defenses are irrelevant until this happens.

2. BOARD IT. Once Logi is 0%, get capture-capable troops physically into
   the carrier group's zone, same side only (mixed-side troops in the zone
   block the attempt). A boarding timer starts -- same length as a normal
   base capture -- and both sides get a warning message. Killing the
   boarding troops before the timer completes stops it.

A carrier with an intact supply ship cannot be boarded no matter how much
damage its escorts take -- sink the supply ship first.";

const HELP_COMBAT_JTAC: &str = "\
=== Combat & JTAC ===
JTAC ground controllers: use the JTAC menu or chat commands to request a
9-line (status), have the target smoked (smoke), or shift to the next
target manually or automatically (shift / autoshift).

EWR: check the EWR menu for early warning radar contact reports on
approaching enemy aircraft.

Artillery: if your side has active batteries, \"Request Fires\" appears
automatically in your Actions menu -- no separate setup needed.";

fn help_getting_started(_lua: MizLua, gid: GroupId) -> Result<()> {
    let ctx = unsafe { Context::get_mut() };
    ctx.db.ephemeral.msgs().panel_to_group(60, false, gid, HELP_GETTING_STARTED);
    Ok(())
}

fn help_chat_commands(_lua: MizLua, gid: GroupId) -> Result<()> {
    let ctx = unsafe { Context::get_mut() };
    ctx.db.ephemeral.msgs().panel_to_group(60, false, gid, HELP_CHAT_COMMANDS);
    Ok(())
}

fn help_cargo_logistics(_lua: MizLua, gid: GroupId) -> Result<()> {
    let ctx = unsafe { Context::get_mut() };
    ctx.db.ephemeral.msgs().panel_to_group(60, false, gid, HELP_CARGO_LOGISTICS);
    Ok(())
}

fn help_objectives(_lua: MizLua, gid: GroupId) -> Result<()> {
    let ctx = unsafe { Context::get_mut() };
    ctx.db.ephemeral.msgs().panel_to_group(60, false, gid, HELP_OBJECTIVES);
    Ok(())
}

fn help_carrier_groups(_lua: MizLua, gid: GroupId) -> Result<()> {
    let ctx = unsafe { Context::get_mut() };
    ctx.db.ephemeral.msgs().panel_to_group(60, false, gid, HELP_CARRIER_GROUPS);
    Ok(())
}

fn help_combat_jtac(_lua: MizLua, gid: GroupId) -> Result<()> {
    let ctx = unsafe { Context::get_mut() };
    ctx.db.ephemeral.msgs().panel_to_group(60, false, gid, HELP_COMBAT_JTAC);
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

    mc.add_command_for_group(
        miz_gid,
        "Weather".into(),
        Some(root.clone()),
        weather,
        ArgTuple { fst: miz_gid, snd: *slot },
    )?;

    let help_root = mc.add_submenu_for_group(miz_gid, "Help".into(), Some(root.clone()))?;
    mc.add_command_for_group(
        miz_gid,
        "Getting Started".into(),
        Some(help_root.clone()),
        help_getting_started,
        miz_gid,
    )?;
    mc.add_command_for_group(
        miz_gid,
        "Chat Commands".into(),
        Some(help_root.clone()),
        help_chat_commands,
        miz_gid,
    )?;
    mc.add_command_for_group(
        miz_gid,
        "Cargo & Logistics".into(),
        Some(help_root.clone()),
        help_cargo_logistics,
        miz_gid,
    )?;
    mc.add_command_for_group(
        miz_gid,
        "Objectives & Capturing".into(),
        Some(help_root.clone()),
        help_objectives,
        miz_gid,
    )?;
    mc.add_command_for_group(
        miz_gid,
        "Carrier Groups".into(),
        Some(help_root.clone()),
        help_carrier_groups,
        miz_gid,
    )?;
    mc.add_command_for_group(
        miz_gid,
        "Combat & JTAC".into(),
        Some(help_root.clone()),
        help_combat_jtac,
        miz_gid,
    )?;

    Ok(())
}
