/*
Copyright 2026 Dillen Weerasinghe.

This file is part of bflib.

bflib is free software: you can redistribute it and/or modify it under
the terms of the GNU Affero Public License as published by the Free
Software Foundation, either version 3 of the License, or (at your
option) any later version.

bflib is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero Public License
for more details.
*/

//! F10 > Ground Forces: command the side's ground formations.
//!
//! The root entry is a single `Ground Forces>>` command that builds the menu
//! when it is opened, so the formations and objectives in it are the ones in
//! the field now, not the ones there were when the player slotted in (the
//! JTAC menu's old staleness problem). "Refresh" rebuilds it. Every command
//! only queues a `groundwar::Cmd`; the slow tick carries it out and replies.

use super::{ArgQuad, ArgTriple, ArgTuple, Pager};
use crate::{
    Context,
    db::formation::{Order, Posture},
    groundwar::{Cmd, CmdKind, at_sea},
};
use anyhow::{Result, anyhow};
use bfprotocols::db::objective::ObjectiveId;
use compact_str::format_compact;
use dcso3::{
    MizLua, String, Vector2,
    env::miz::GroupId,
    mission_commands::{GroupCommandItem, GroupSubMenu, MissionCommands},
    net::Ucid,
};

const ROOT: &str = "Ground Forces";
const ROOT_CMD: &str = "Ground Forces>>";
/// Objectives offered per Attack / Defend / Raise list.
const CHOICES: usize = 8;

fn dist(a: Vector2, b: Vector2) -> f64 {
    (a - b).norm()
}

fn queue(ucid: Ucid, group: GroupId, kind: CmdKind) -> Result<()> {
    // no DCS calls here, so nothing to defer
    let mut ctx = Context::borrow("ground forces menu")?;
    ctx.groundwar.queue(Cmd { ucid, group, kind });
    Ok(())
}

fn status(_: MizLua, a: ArgTuple<Ucid, GroupId>) -> Result<()> {
    queue(a.fst, a.snd, CmdKind::Status)?;
    Ok(())
}

fn refresh(lua: MizLua, a: ArgTuple<Ucid, GroupId>) -> Result<()> {
    build(lua, a.fst, a.snd)
}

fn open(lua: MizLua, a: ArgTuple<Ucid, GroupId>) -> Result<()> {
    build(lua, a.fst, a.snd)
}

fn raise(_: MizLua, a: ArgTriple<Ucid, GroupId, ObjectiveId>) -> Result<()> {
    queue(a.fst, a.snd, CmdKind::Raise(a.trd))?;
    Ok(())
}

fn attack(_: MizLua, a: ArgQuad<Ucid, GroupId, u32, ObjectiveId>) -> Result<()> {
    queue(a.fst, a.snd, CmdKind::Order(a.trd, Order::Attack(a.fth)))?;
    Ok(())
}

fn defend(_: MizLua, a: ArgQuad<Ucid, GroupId, u32, ObjectiveId>) -> Result<()> {
    queue(a.fst, a.snd, CmdKind::Order(a.trd, Order::Defend(a.fth)))?;
    Ok(())
}

fn withdraw(_: MizLua, a: ArgQuad<Ucid, GroupId, u32, ObjectiveId>) -> Result<()> {
    queue(a.fst, a.snd, CmdKind::Order(a.trd, Order::Withdraw(a.fth)))?;
    Ok(())
}

fn hold(_: MizLua, a: ArgTriple<Ucid, GroupId, u32>) -> Result<()> {
    queue(a.fst, a.snd, CmdKind::Order(a.trd, Order::Hold))?;
    Ok(())
}

fn release(_: MizLua, a: ArgTriple<Ucid, GroupId, u32>) -> Result<()> {
    queue(a.fst, a.snd, CmdKind::Release(a.trd))?;
    Ok(())
}

fn form_deployed(_: MizLua, a: ArgTuple<Ucid, GroupId>) -> Result<()> {
    queue(a.fst, a.snd, CmdKind::FormDeployed)?;
    Ok(())
}

fn disband(_: MizLua, a: ArgTriple<Ucid, GroupId, u32>) -> Result<()> {
    queue(a.fst, a.snd, CmdKind::Disband(a.trd))?;
    Ok(())
}

/// Put the closed `Ground Forces>>` entry on a slot's F10 root.
pub(super) fn init_ground_menu_for_slot(
    mc: &MissionCommands,
    group: GroupId,
    ucid: Ucid,
) -> Result<()> {
    mc.remove_command_for_group(group, GroupCommandItem::from(vec![ROOT_CMD.into()]))?;
    mc.remove_submenu_for_group(group, GroupSubMenu::from(vec![ROOT.into()]))?;
    mc.add_command_for_group(
        group,
        ROOT_CMD.into(),
        None,
        open,
        ArgTuple { fst: ucid, snd: group },
    )?;
    Ok(())
}

fn build(lua: MizLua, ucid: Ucid, group: GroupId) -> Result<()> {
    let mut entered = crate::Entered::enter(lua)?;
    let ctx = &mut *entered;
    let mc = MissionCommands::singleton(lua)?;
    mc.remove_command_for_group(group, GroupCommandItem::from(vec![ROOT_CMD.into()]))?;
    mc.remove_submenu_for_group(group, GroupSubMenu::from(vec![ROOT.into()]))?;
    let root = mc.add_submenu_for_group(group, ROOT.into(), None)?;
    let player = ctx.db.player(&ucid).ok_or_else(|| anyhow!("unknown player"))?;
    let side = player.side;
    let me: Option<Vector2> = ctx
        .db
        .instanced_players()
        .find(|(u, _, _)| **u == ucid)
        .map(|(_, _, i)| Vector2::new(i.position.p.x, i.position.p.z));
    let mut p = Pager::new(group, root);
    p.command(&mc, "Status report".into(), status, ArgTuple { fst: ucid, snd: group })?;
    p.command(
        &mc,
        "Refresh this menu".into(),
        refresh,
        ArgTuple { fst: ucid, snd: group },
    )?;
    // (id, name, pos, posture, strength %, home)
    let mut forms: Vec<_> = ctx
        .db
        .formations()
        .filter(|f| f.side == side)
        .map(|f| {
            (
                f.id,
                f.name.clone(),
                f.pos,
                f.posture,
                ctx.db.formation_strength_pct(f),
                f.home,
            )
        })
        .collect();
    forms.sort_by_key(|f| f.0);
    // (id, name, pos, ours)
    let objs: Vec<(ObjectiveId, String, Vector2, bool)> = ctx
        .db
        .objectives()
        .filter(|(_, o)| !at_sea(o.kind()))
        .map(|(id, o)| (*id, o.name.clone(), o.pos(), o.owner() == side))
        .collect();
    let nearest = |at: Vector2, ours: bool| {
        let mut v: Vec<&(ObjectiveId, String, Vector2, bool)> =
            objs.iter().filter(|o| o.3 == ours).collect();
        v.sort_by(|a, b| dist(a.2, at).total_cmp(&dist(b.2, at)));
        v.truncate(CHOICES);
        v
    };
    // Form and Raise go before the formation list, so they stay on the first
    // page however many formations a side has. Raise: friendly bases that can
    // spare troops, nearest the player (or the front, for someone not in an
    // aircraft).
    let cfg = ctx.db.ephemeral.cfg.ground_war.clone();
    if let Some(cfg) = cfg.as_ref().filter(|c| c.enabled && c.form_from_deployed) {
        p.command(
            &mc,
            format_compact!(
                "Form a formation from my deployed units (within {:.0} km)",
                cfg.form_radius_m / 1000.
            )
            .into(),
            form_deployed,
            ArgTuple { fst: ucid, snd: group },
        )?;
    }
    if let Some(cfg) = cfg.filter(|c| c.enabled) {
        let at = me.or_else(|| forms.first().map(|f| f.2)).unwrap_or_default();
        let mut offers = vec![];
        for (oid, oname, opos, _) in nearest(at, true).into_iter().take(CHOICES * 2) {
            let n = ctx
                .db
                .formation_candidates(&cfg, oid, side)
                .map(|g| g.len())
                .unwrap_or(0);
            if n > 0 {
                offers.push((*oid, oname.clone(), dist(*opos, at) / 1000., n));
            }
            if offers.len() >= CHOICES {
                break;
            }
        }
        if !offers.is_empty() {
            let rs = p.submenu(&mc, "Raise a formation".into())?;
            let mut pr = Pager::new(group, rs);
            for (oid, oname, km, n) in offers {
                pr.command(
                    &mc,
                    format_compact!("From {oname} ({n} groups, {km:.0} km)").into(),
                    raise,
                    ArgTriple { fst: ucid, snd: group, trd: oid },
                )?;
            }
        }
    }
    for (id, name, pos, posture, pct, home) in &forms {
        let state = match posture {
            Posture::Moving => "moving",
            Posture::Holding => "holding",
            Posture::Assaulting => "assaulting",
        };
        let sub = p.submenu(&mc, format_compact!("{name} ({state}, {pct}%)").into())?;
        // Six entries today; paged anyway so a seventh can't fall off.
        let mut fp = Pager::new(group, sub);
        let atk = fp.submenu(&mc, "Attack".into())?;
        let mut pa = Pager::new(group, atk);
        for (oid, oname, opos, _) in nearest(*pos, false) {
            let km = dist(*opos, *pos) / 1000.;
            pa.command(
                &mc,
                format_compact!("{oname} ({km:.0} km)").into(),
                attack,
                ArgQuad { fst: ucid, snd: group, trd: *id, fth: *oid },
            )?;
        }
        let def = fp.submenu(&mc, "Defend / move to".into())?;
        let mut pd = Pager::new(group, def);
        for (oid, oname, opos, _) in nearest(*pos, true) {
            let km = dist(*opos, *pos) / 1000.;
            pd.command(
                &mc,
                format_compact!("{oname} ({km:.0} km)").into(),
                defend,
                ArgQuad { fst: ucid, snd: group, trd: *id, fth: *oid },
            )?;
        }
        fp.command(
            &mc,
            "Hold position".into(),
            hold,
            ArgTriple { fst: ucid, snd: group, trd: *id },
        )?;
        // Home if it is still ours, else the nearest base that is.
        let back = objs
            .iter()
            .find(|o| o.0 == *home && o.3)
            .or_else(|| nearest(*pos, true).into_iter().next());
        if let Some((oid, oname, _, _)) = back {
            fp.command(
                &mc,
                format_compact!("Withdraw to {oname} and refit").into(),
                withdraw,
                ArgQuad { fst: ucid, snd: group, trd: *id, fth: *oid },
            )?;
        }
        fp.command(
            &mc,
            "Hand back to AI command".into(),
            release,
            ArgTriple { fst: ucid, snd: group, trd: *id },
        )?;
        // formed from deployed units: they become deployed units again;
        // raised from a garrison: it goes straight back to its posts
        let formed = ctx.db.formation(*id).is_some_and(|f| f.formed);
        fp.command(
            &mc,
            if formed {
                "Disband into deployed units here".into()
            } else {
                "Disband (back to the garrison)".into()
            },
            disband,
            ArgTriple { fst: ucid, snd: group, trd: *id },
        )?;
    }
    Ok(())
}
