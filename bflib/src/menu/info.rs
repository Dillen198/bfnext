use super::{brg_rng, player_world_pos, slot_for_group, ArgTriple, ArgTuple, Pager};
use crate::{
    atis,
    db::{group::DeployKind, logistics::ConvoyState, Db},
    Context,
};
use anyhow::{Context as ErrContext, Result};
use bfprotocols::{
    cfg::{ActionKind, AwacsCfg},
    db::objective::ObjectiveId,
};
use compact_str::{format_compact, CompactString};
use dcso3::{
    coalition::Side,
    env::miz::GroupId,
    mission_commands::{GroupSubMenu, MissionCommands},
    net::SlotId,
    MizLua, Vector2,
};
use log::error;
use std::fmt::Write;

fn from_pos(ctx: &Context, lua: MizLua, gid: &GroupId) -> Option<Vector2> {
    let (_, slot) = slot_for_group(lua, ctx, gid).ok()?;
    player_world_pos(ctx, &slot)
}

fn obj_name(db: &Db, oid: &ObjectiveId) -> CompactString {
    db.persisted
        .objectives
        .get(oid)
        .map(|o| CompactString::from(o.name()))
        .unwrap_or_else(|| CompactString::from("?"))
}

fn brg_rng_str(from: Option<Vector2>, to: Vector2) -> CompactString {
    match from {
        Some(p) => {
            let (b, r) = brg_rng(p, to);
            format_compact!(" ({b:03}\u{b0}/{r:.0}nm)")
        }
        None => CompactString::from(""),
    }
}

// ── Situation report ────────────────────────────────────────────────────────
//
// The whole report is built by `crate::situation` and rendered a page at a
// time. DCS radio menus can't be relabelled in place, so each page gets its
// own command rather than a Next/Prev cursor.

// ── My status ──────────────────────────────────────────────────────────────

fn build_my_status(ctx: &mut Context, slot: &SlotId) -> CompactString {
    let ucid = match ctx.db.ephemeral.player_in_slot(slot).copied() {
        Some(u) => u,
        None => return CompactString::from("You are not registered in this slot."),
    };
    let (name, side, points, streak, kills) = match ctx.db.player(&ucid) {
        Some(p) => (p.name.clone(), p.side, p.points, p.kill_streak, p.total_kills),
        None => return CompactString::from("No player record found."),
    };
    let mut s = format_compact!("=== {name} ===\n");
    let _ = write!(
        s,
        "Side: {side:?}\nPoints: {points}\nKill streak: {streak}\nCareer kills: {kills}\n"
    );
    let cfg = &ctx.db.ephemeral.cfg;
    if cfg.lock_sides {
        let _ = write!(s, "Sides are LOCKED this round\n");
    } else if let Some(n) = cfg.side_switches {
        let _ = write!(s, "Side switches allowed: {n}/round\n");
    }
    if ctx.db.ephemeral.cfg.limited_lives {
        let _ = write!(s, "\nLives:\n");
        match crate::lives(&mut ctx.db, &ucid, None) {
            Ok(l) => s.push_str(&l),
            Err(_) => s.push_str("(unavailable)\n"),
        }
    }
    s
}

// ── Support & radios ───────────────────────────────────────────────────────

fn build_support(ctx: &Context, side: Side, from: Option<Vector2>) -> CompactString {
    let db = &ctx.db;
    let mut report = CompactString::from("=== Support & Radios ===\n");
    let mut found = false;
    for (_, group) in &db.persisted.groups {
        if group.side != side {
            continue;
        }
        let DeployKind::Action { spec, .. } = &group.origin else { continue };
        let plane = match &spec.kind {
            ActionKind::Awacs(AwacsCfg { plane, .. }) => plane,
            ActionKind::Tanker(plane) => plane,
            _ => continue,
        };
        let kind = if matches!(&spec.kind, ActionKind::Tanker(_)) { "TANKER" } else { "AWACS" };
        let mut line = format_compact!("{kind} [{}]", group.name);
        if let Some(freq) = plane.freq {
            let _ = write!(line, "  {:.3} MHz", freq as f64 / 1_000_000.0);
        }
        if let Some(ch) = plane.tacan_channel {
            let band = plane
                .tacan_band
                .as_ref()
                .map(|b| format_compact!("{b:?}"))
                .unwrap_or_else(|| CompactString::from("X"));
            let cs = plane
                .tacan_callsign
                .as_ref()
                .map(|c| format_compact!(" {c}"))
                .unwrap_or_default();
            let _ = write!(line, "  TACAN {ch}{band}{cs}");
        }
        report.push_str(&line);
        report.push('\n');
        found = true;
    }

    let jtacs: Vec<_> = ctx.jtac.jtacs().filter(|j| j.side() == side).collect();
    if !jtacs.is_empty() {
        report.push_str("\nJTACs:\n");
        for j in jtacs {
            let loc = j.location();
            let near = obj_name(db, &loc.oid);
            let br = brg_rng_str(from, loc.pos);
            let tgt = if j.target().is_some() { " [lasing]" } else { "" };
            let _ = write!(
                report,
                "  {:?} code {} near {near}{br}{tgt}\n",
                j.gid(),
                j.code()
            );
            found = true;
        }
    }

    if !found {
        report.push_str("No active AWACS, tankers, or JTACs.\n");
    }
    report
}

// ── Supply convoys ─────────────────────────────────────────────────────────

fn build_convoys(ctx: &Context, side: Side, from: Option<Vector2>) -> CompactString {
    let db = &ctx.db;
    let now = chrono::Utc::now();
    let mut report = CompactString::from("=== Supply Convoys ===\n");
    let mut any = false;
    for c in db.convoys_for_side(side) {
        if c.state != ConvoyState::InTransit {
            continue;
        }
        any = true;
        let age = (now - c.spawn_time).num_minutes().max(0);
        let br = brg_rng_str(from, c.last_pos);
        let _ = write!(
            report,
            "{} -> {}  ({}){br}  {}m en route\n",
            obj_name(db, &c.origin),
            obj_name(db, &c.destination),
            c.cargo_type.as_str(),
            age,
        );
    }
    if !any {
        report.push_str("No friendly supply convoys are moving right now.\n");
    }
    report
}

// ── Navaids directory ──────────────────────────────────────────────────────

fn build_navaids(ctx: &Context, side: Side, from: Option<Vector2>) -> CompactString {
    let db = &ctx.db;
    if !db.ephemeral.cfg.navaids.enabled {
        return CompactString::from("Auto-navaids are disabled on this server.");
    }
    let mut rows: Vec<(f64, CompactString)> = vec![];
    for (oid, obj) in db.objectives() {
        if obj.owner() != side {
            continue;
        }
        let Some(navs) = db.persisted.navaids.get(oid) else { continue };
        if navs.is_empty() {
            continue;
        }
        let r = from.map(|p| brg_rng(p, obj.pos()).1).unwrap_or(f64::MAX);
        for nav in navs {
            let who = match &nav.label {
                Some(l) => format_compact!("{} [{}]", obj.name(), l),
                None => CompactString::from(obj.name()),
            };
            rows.push((
                r,
                format_compact!("{who}{}: {}\n", brg_rng_str(from, obj.pos()), nav.summary()),
            ));
        }
    }
    rows.sort_by(|a, b| a.0.total_cmp(&b.0));
    let mut report = CompactString::from("=== Friendly Navaids ===\n");
    for (_, l) in &rows {
        report.push_str(l);
    }
    if rows.is_empty() {
        report.push_str("No generated navaids yet.\n");
    }
    report
}

// ── callbacks ──────────────────────────────────────────────────────────────

fn my_status(_lua: MizLua, arg: ArgTuple<GroupId, SlotId>) -> Result<()> {
    let ctx = unsafe { Context::get_mut() };
    let report = build_my_status(ctx, &arg.snd);
    ctx.db.ephemeral.msgs().panel_to_group(30, false, arg.fst, report);
    Ok(())
}

/// Render one page of the auto-generated situational briefing. `trd` is the
/// page index into [`crate::situation::PAGES`].
fn situation_page(lua: MizLua, arg: ArgTriple<GroupId, Side, u8>) -> Result<()> {
    let ctx = unsafe { Context::get_mut() };
    let from = from_pos(ctx, lua, &arg.fst);
    let rep = crate::situation::build(
        ctx,
        lua,
        arg.snd,
        crate::situation::Opts { include_map: false, from },
    );
    let pages = crate::situation::render_pages(&rep);
    let page = pages
        .get(arg.trd as usize)
        .cloned()
        .unwrap_or_else(|| String::from("That briefing page is not available."));
    // Long pages -- give them time to read rather than the usual 30s.
    ctx.db.ephemeral.msgs().panel_to_group(60, false, arg.fst, page);
    Ok(())
}

fn support(lua: MizLua, arg: ArgTriple<GroupId, Side, u8>) -> Result<()> {
    let ctx = unsafe { Context::get_mut() };
    let from = from_pos(ctx, lua, &arg.fst);
    let report = build_support(ctx, arg.snd, from);
    ctx.db.ephemeral.msgs().panel_to_group(30, false, arg.fst, report);
    Ok(())
}

fn convoys(lua: MizLua, arg: ArgTriple<GroupId, Side, u8>) -> Result<()> {
    let ctx = unsafe { Context::get_mut() };
    let from = from_pos(ctx, lua, &arg.fst);
    let report = build_convoys(ctx, arg.snd, from);
    ctx.db.ephemeral.msgs().panel_to_group(30, false, arg.fst, report);
    Ok(())
}

fn navaids_directory(lua: MizLua, arg: ArgTriple<GroupId, Side, u8>) -> Result<()> {
    let ctx = unsafe { Context::get_mut() };
    let from = from_pos(ctx, lua, &arg.fst);
    let report = build_navaids(ctx, arg.snd, from);
    ctx.db.ephemeral.msgs().panel_to_group(45, false, arg.fst, report);
    Ok(())
}

fn time_and_server(_lua: MizLua, gid: GroupId) -> Result<()> {
    let ctx = unsafe { Context::get_mut() };
    let now = chrono::Utc::now();
    let msg = match ctx.shutdown.as_ref() {
        None => CompactString::from("Server restart: not scheduled automatically."),
        Some(asd) => {
            let d = asd.when - now;
            let secs = d.num_seconds().max(0);
            format_compact!(
                "Server restarts in {:02}:{:02}:{:02}",
                secs / 3600,
                (secs % 3600) / 60,
                secs % 60
            )
        }
    };
    ctx.db.ephemeral.msgs().panel_to_group(20, false, gid, msg);
    Ok(())
}

fn weather(lua: MizLua, arg: ArgTuple<GroupId, SlotId>) -> Result<()> {
    let ctx = unsafe { Context::get_mut() };

    // Server weather from DCSServerBot (pushed in by bfdb) is the source of
    // truth when we have it -- it agrees with the dashboard and doesn't
    // depend on the mission's live-weather sync.
    if let Some(bw) = ctx.bot_weather {
        let vis_km = bw.visibility_m / 1000.0;
        let vis_sm = bw.visibility_m / 1609.34;
        let cover = if bw.cloud_density > 0.0 {
            format_compact!("{:.0}/10", bw.cloud_density)
        } else {
            CompactString::from("clear")
        };
        let msg = format_compact!(
            "SERVER WEATHER\n\
             Temp: {c:.0}°C / {f:.0}°F\n\
             Surface wind: {wdir:03}° at {wkt:.0} kt\n\
             Visibility: {vk:.0} km / {vs:.0} SM\n\
             Clouds: base {ft:.0} ft AGL, {cover}\n\
             QNH: {hpa:.0} hPa / {inhg:.2} inHg",
            c = bw.temp_c,
            f = bw.temp_c * 1.8 + 32.0,
            wdir = bw.wind_from_deg as u32,
            wkt = bw.wind_speed_kts,
            vk = vis_km,
            vs = vis_sm,
            ft = bw.cloud_base_m * 3.281,
            cover = cover,
            hpa = bw.qnh_hpa,
            inhg = bw.qnh_hpa / 33.8639,
        );
        ctx.db.ephemeral.msgs().panel_to_group(30, false, arg.fst, msg);
        return Ok(());
    }

    // Otherwise: full field ATIS + winds aloft when the player is in a slot.
    match atis::send_full_weather(lua, arg.snd) {
        Ok(true) => return Ok(()),
        Ok(false) => {}
        Err(e) => error!("full weather report failed for slot {:?}: {:?}", arg.snd, e),
    }
    // No slot context (F10 map, spectator, ground unit) -- a general brief at
    // wherever the player is, falling back to the map origin.
    let pos = from_pos(ctx, lua, &arg.fst).unwrap_or_else(|| dcso3::Vector2::new(0.0, 0.0));
    if let Err(e) = atis::send_weather_brief(lua, arg.fst, pos) {
        error!("weather brief failed for group {:?}: {:?}", arg.fst, e);
    }
    Ok(())
}

// ── Help ───────────────────────────────────────────────────────────────────
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
 - Objectives: base status, nearest-base detail, capture/threat lists
 - Info: My Status, Situation Report, Support & Radios, Convoys, Weather, Help
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
the enemy's on the map. The Info > Supply Convoys menu lists yours.

Crates: Cargo menu -> Crates -> Base Supply. Spawn one (or pick \"xN\" to
spawn several at once), carry it to the target base, then Unpack it there.
 - Logistics Repair Kit: revives the base's dead logistics-defense units
   (the trucks/depot that make it a working base again)
 - Base Fuel Resupply / Base Ammo Resupply: adds fuel / weapons stock to
   the base's warehouse -- use these to top up a starved forward base
 - Deployable crates (their own categories): build SAM sites, vehicles,
   and other defenses
A crate delivered with no effect (e.g. nothing left to revive) will tell you
so honestly instead of just saying \"delivered\".";

const HELP_OBJECTIVES: &str = "\
=== Objectives & Capturing ===
Each base's F10 map label shows Health, Logistics(Logi), Supply, and Fuel,
plus a live \"Repairing: X% (ETA ...)\" line while it's actively healing.
The inner ring on the map marker turns white once a base is capturable.
Objectives > Base Detail gives the full card for any friendly base, incl.
LL/MGRS, bearing/range from you, repair state, and capture requirements.

HOW TO CAPTURE A BASE:
1. Reduce it to capturable: Health at or below 20% AND zero infantry
   defenders left (some servers also require a further share of total
   defenders destroyed). A base bombed all the way to Health 0 flips
   straight to Neutral on its own.
2. Deploy capture-capable troops (Troops menu) and get them physically
   alive into the objective's zone. All your troops in the zone must be
   the same side. Note: you can't load troops or pull crates FROM a base
   that's itself capturable -- bring them from another friendly objective.
3. A capture timer starts (~180s, faster with more squads) -- both sides
   get a warning. Killing your troops resets your progress.
4. CONSOLIDATION HOLD: the base flips to you the instant the timer ends,
   but it is not a working base yet -- the map label runs a live
   \"CONSOLIDATING X% (ETA ...)\" countdown, normally ~5 min, and you cannot
   slot in there until it finishes.
   Your troops must physically STAY IN THE ZONE. Step outside and the
   clock stops (\"CONSOLIDATION PAUSED\") -- progress is never lost, it just
   stops accruing until they are back in.
   You can beat the clock. Bring a second squad into the zone and it
   consolidates faster (each extra squad adds 50%), and landing a
   Logistics Repair Kit or a supply crate at the base jumps it forward
   ~2 min outright -- so flying the logistics sortie is quicker than
   orbiting and waiting.
   During the hold the enemy CANNOT start a fresh capture timer -- to take
   it back they have to wipe out your holding troops. If they do, and the
   base is still shot up (Health <= 20%), it drops to Neutral. If your new
   garrison is standing (Health > 20%), it holds and consolidates even if
   the assault troops die.
5. For ~2 minutes after any base changes hands (or goes Neutral) no new
   capture timer can start against it -- the new owner gets a breather.

AFTER CAPTURE: the enemy garrison is wiped and you get back only a LIGHT
garrison -- AAA and infantry, ~25% health, NO SAMs. The SAM cover and the
rest of the garrison rebuild slowly via auto-repair or have to be flown in
as deployable crates. Warehouse stock and supply lines transfer to you and
logistics/services get one repair step. A freshly-taken base is a soft
target you have to invest in.

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
target manually or automatically (shift / autoshift). Info > Support &
Radios lists every active JTAC with its laser code and rough location.

EWR: check the EWR menu for early warning radar contact reports on
approaching enemy aircraft.

Artillery: if your side has active batteries, \"Request Fires\" appears
automatically in your Actions menu -- no separate setup needed.";

fn help_topic(text: &'static str) -> impl Fn(MizLua, GroupId) -> Result<()> {
    move |_lua, gid| {
        let ctx = unsafe { Context::get_mut() };
        ctx.db.ephemeral.msgs().panel_to_group(60, false, gid, text);
        Ok(())
    }
}

pub(super) fn init_info_menu_for_slot(ctx: &mut Context, lua: MizLua, slot: &SlotId) -> Result<()> {
    let mc = MissionCommands::singleton(lua)?;
    let si = ctx.db.ephemeral.get_slot_info(slot).context("getting slot info")?;
    let miz_gid = si.miz_gid;
    let side = si.side;

    mc.remove_submenu_for_group(miz_gid, GroupSubMenu::from(vec!["Info".into()]))?;
    let root = mc.add_submenu_for_group(miz_gid, "Info".into(), None)?;
    // Eight entries today, and this menu has only ever grown -- everything on
    // it claims a slot from the pager so the next report added here spills to
    // a "More >>" page instead of being dropped by DCS.
    let mut p = Pager::new(miz_gid, root);

    p.command(
        &mc,
        "My Status".into(),
        my_status,
        ArgTuple { fst: miz_gid, snd: *slot },
    )?;
    let sit_root = p.submenu(&mc, "Situation".into())?;
    let mut sp = Pager::new(miz_gid, sit_root);
    for (i, title) in crate::situation::PAGES.iter().enumerate() {
        sp.command(
            &mc,
            format_compact!("{}. {title}", i + 1).as_str().into(),
            situation_page,
            ArgTriple { fst: miz_gid, snd: side, trd: i as u8 },
        )?;
    }
    for (label, cb) in [
        ("Support & Radios", support as fn(MizLua, ArgTriple<GroupId, Side, u8>) -> Result<()>),
        ("Supply Convoys", convoys),
        ("Navaids Directory", navaids_directory),
    ] {
        p.command(
            &mc,
            label.into(),
            cb,
            ArgTriple { fst: miz_gid, snd: side, trd: 0u8 },
        )?;
    }
    p.command(&mc, "Time & Server".into(), time_and_server, miz_gid)?;
    p.command(
        &mc,
        "Weather".into(),
        weather,
        ArgTuple { fst: miz_gid, snd: *slot },
    )?;

    let help_root = p.submenu(&mc, "Help".into())?;
    let mut hp = Pager::new(miz_gid, help_root);
    for (label, text) in [
        ("Getting Started", HELP_GETTING_STARTED),
        ("Chat Commands", HELP_CHAT_COMMANDS),
        ("Cargo & Logistics", HELP_CARGO_LOGISTICS),
        ("Objectives & Capturing", HELP_OBJECTIVES),
        ("Carrier Groups", HELP_CARRIER_GROUPS),
        ("Combat & JTAC", HELP_COMBAT_JTAC),
    ] {
        hp.command(&mc, label.into(), help_topic(text), miz_gid)?;
    }

    Ok(())
}
