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

//! The HQ as one side sees it: the `query-hq` view for the dashboard and
//! the strategist, and the F10 / chat text for players.

use super::{
    cfg, dist, picture, planner,
    requests::RequestStatus,
    OpStatus,
};
use crate::Context;
use bfprotocols::{
    db::objective::ObjectiveId,
    hq::{
        ContactInfo, DirectiveInfo, HqView, LatLon, LogInfo, ObjInfo, ObjRef, OpInfo, OverrideInfo,
        PictureInfo, RecordInfo, RequestInfo,
    },
};
use chrono::prelude::*;
use compact_str::{format_compact, CompactString};
use dcso3::{coalition::Side, coord::Coord, LuaVec3, MizLua, Vector2, Vector3};
use std::fmt::Write;

fn ts(t: DateTime<Utc>) -> String {
    t.to_rfc3339_opts(SecondsFormat::Secs, true)
}

/// The view of `side`'s HQ, for `viewer` (whether they may command it).
/// `lua` is only for map coordinates.
pub(crate) fn view(ctx: &Context, lua: MizLua, side: Side, viewer: Option<&dcso3::net::Ucid>) -> HqView {
    let now = Utc::now();
    let coord = Coord::singleton(lua).ok();
    let ll = |p: Vector2| -> LatLon {
        coord
            .as_ref()
            .and_then(|c| c.lo_to_ll(LuaVec3(Vector3::new(p.x, 0., p.y))).ok())
            .map(|l| [l.latitude, l.longitude])
            .unwrap_or([0., 0.])
    };
    let mut v = HqView { side: format!("{side:?}"), ..Default::default() };
    let Some((sc, hq)) = cfg(ctx) else { return v };
    if !hq.sides.contains(&side) {
        return v;
    }
    v.enabled = true;
    v.can_command = viewer.map_or(false, |u| super::may_command(ctx, &hq, Some(u)));
    let pic = picture::build(ctx, &hq, side, now);
    let objref = |id: &ObjectiveId| {
        pic.get(id).map(|o| ObjRef { id: o.id.inner() as u64, name: o.name.to_string(), pos: ll(o.pos) })
    };
    let rt = ctx.hq.sides.get(&side);
    if let Some(plan) = rt.and_then(|r| r.plan.as_ref()) {
        v.posture = Some(plan.posture);
        v.source = plan.source.label().into();
        v.main_effort = plan.main_effort.as_ref().and_then(objref);
        v.defend = plan.defend.iter().filter_map(objref).collect();
        v.supply_priority = plan.supply.iter().filter_map(objref).collect();
        v.avoid = plan.avoid.iter().filter_map(objref).collect();
        v.weights = plan.weights.clone();
        v.intent = plan.intent.to_string();
        v.reasons = plan.reasons.iter().map(|r| r.to_string()).collect();
        v.paused = plan.paused;
    }
    let saved = ctx.db.persisted.hq.side(side);
    v.directive = saved.directive.as_ref().map(|d| DirectiveInfo {
        directive: d.directive.clone(),
        received: ts(d.received),
        expires: ts(d.expires),
    });
    v.override_ = saved.human.as_ref().map(|o| OverrideInfo {
        by: o.by.to_string(),
        set: ts(o.set),
        expires: ts(o.expires),
        directive: o.directive.clone(),
        disabled_ops: o.disabled_ops.clone(),
        paused: o.paused,
    });
    v.record = saved
        .record
        .iter()
        .map(|(k, r)| RecordInfo { kind: *k, launched: r.launched, succeeded: r.succeeded, failed: r.failed })
        .collect();
    v.treasury = ctx.db.persisted.treasury(side);
    v.reserve = sc.action_reserve;
    v.available = planner::available(ctx, &hq, side).into_iter().map(|(k, a)| (k, a.cost)).collect();
    if let Some(rt) = rt {
        v.gap_factor = rt.gap;
        v.ops = rt
            .ops
            .iter()
            .rev()
            .map(|o| OpInfo {
                id: o.id,
                kind: o.kind,
                line: o.kind.line(),
                target: o.target.map(|t| t.inner() as u64),
                target_name: o.target_name.to_string(),
                pos: ll(o.pos),
                started: ts(o.started),
                cost: o.cost,
                status: o.status.label().into(),
                detail: o.detail.to_string(),
                request: o.request,
                support: o.support.iter().map(|(_, r)| r.to_string()).collect(),
            })
            .collect();
        v.requests = rt
            .requests
            .iter()
            .rev()
            .map(|r| RequestInfo {
                id: r.id,
                kind: r.kind,
                by: r.by.to_string(),
                target: r.target.inner() as u64,
                target_name: r.target_name.to_string(),
                created: ts(r.created),
                status: r.status.label().into(),
                answer: r.answer.to_string(),
            })
            .collect();
        v.log = rt.log.iter().rev().map(|(t, s)| LogInfo { at: ts(*t), text: s.to_string() }).collect();
        let every = if pic.own().any(|o| o.being_captured) { hq.emergency_think_secs } else { hq.think_secs };
        v.next_think_secs = rt
            .last_think
            .map(|t| (every as i64 - (now - t).num_seconds()).max(0))
            .unwrap_or(0);
    }
    let near = |p: Vector2| -> (String, f64) {
        pic.objs
            .iter()
            .min_by(|a, b| dist(a.pos, p).total_cmp(&dist(b.pos, p)))
            .map(|o| (o.name.to_string(), (dist(o.pos, p) / 100.).round() / 10.))
            .unwrap_or_default()
    };
    let contact = |c: &picture::Contact| {
        let (n, km) = near(c.pos);
        ContactInfo {
            pos: ll(c.pos),
            class: c.class.label().to_ascii_lowercase(),
            count: c.count,
            near: n,
            near_km: km,
            age_mins: c.age_mins,
        }
    };
    let own = pic.own().count() as u32;
    let enemy = pic.enemy().count() as u32;
    v.picture = PictureInfo {
        territory_pct: (own as f64 * 1000. / (own + enemy).max(1) as f64).round() / 10.,
        own_objectives: own,
        enemy_objectives: enemy,
        objectives: pic
            .objs
            .iter()
            .map(|o| ObjInfo {
                id: o.id.inner() as u64,
                name: o.name.to_string(),
                kind: o.kind_label().into(),
                owner: if o.own { "own" } else if o.neutral { "neutral" } else { "enemy" }.into(),
                pos: ll(o.pos),
                health: o.health,
                logi: o.logi,
                supply: o.supply,
                fuel: o.fuel,
                threatened: o.threatened,
                being_captured: o.being_captured,
                capturable: o.capturable,
                front_km: if o.front_m.is_finite() { (o.front_m / 100.).round() / 10. } else { -1. },
                inbound: o.inbound,
            })
            .collect(),
        humans: pic.humans,
        humans_fixed_wing_airborne: pic.humans_fw_air,
        humans_helo_airborne: pic.humans_helo_air,
        enemy_air_detected: pic.enemy_air.len() as u32,
        enemy_ground: pic
            .enemy_ground
            .iter()
            .filter(|c| c.class != crate::db::intel::IntelUnitClass::AirDefense)
            .map(contact)
            .collect(),
        enemy_sams: pic.sams().map(contact).collect(),
        formations: pic.formations,
        formations_idle: pic.formations_idle,
        enemy_formations_in_contact: pic.enemy_formations.len() as u32,
        ai_air_up: v
            .ops
            .iter()
            .filter(|o| o.status == "active" && o.line == bfprotocols::hq::Line::Air)
            .count() as u32,
        logistics_out: v
            .ops
            .iter()
            .filter(|o| o.status == "active" && o.line == bfprotocols::hq::Line::Logistics)
            .count() as u32,
        troops_out: v
            .ops
            .iter()
            .filter(|o| o.status == "active" && o.line == bfprotocols::hq::Line::Troops)
            .count() as u32,
    };
    v
}

fn name(ctx: &Context, id: &ObjectiveId) -> CompactString {
    ctx.db.persisted.objectives.get(id).map(|o| o.name().into()).unwrap_or_else(|| "?".into())
}

/// The commander's intent, for F10 > Info > HQ and `-hq`.
pub(crate) fn intent_text(ctx: &Context, side: Side) -> CompactString {
    let Some((_, hq)) = cfg(ctx) else {
        return "There is no HQ on this server.".into();
    };
    if !hq.sides.contains(&side) {
        return "Your side's HQ is not run by the engine on this server.".into();
    }
    let Some(rt) = ctx.hq.sides.get(&side) else {
        return "HQ is still assessing the situation.".into();
    };
    let Some(plan) = rt.plan.as_ref() else {
        return "HQ is still assessing the situation.".into();
    };
    let mut s = format_compact!("HQ -- COMMANDER'S INTENT ({})\n", plan.posture.label());
    let _ = writeln!(s, "{}", plan.intent);
    if let Some(me) = plan.main_effort {
        let _ = writeln!(s, "Main effort: {}", name(ctx, &me));
    }
    if !plan.defend.is_empty() {
        let v: Vec<CompactString> = plan.defend.iter().map(|id| name(ctx, id)).collect();
        let _ = writeln!(s, "Hold: {}", v.join(", "));
    }
    if !plan.supply.is_empty() {
        let v: Vec<CompactString> = plan.supply.iter().take(4).map(|id| name(ctx, id)).collect();
        let _ = writeln!(s, "Resupply first: {}", v.join(", "));
    }
    let by = match plan.source {
        super::strategy::Source::Human => ctx
            .db
            .persisted
            .hq
            .side(side)
            .human
            .as_ref()
            .map(|o| format_compact!("orders from {}", o.by))
            .unwrap_or_else(|| "human orders".into()),
        super::strategy::Source::Strategist => "strategist's directive".into(),
        super::strategy::Source::Rules => "HQ's own assessment".into(),
    };
    let _ = writeln!(s, "({by}{})", if plan.paused { ", HQ planning paused" } else { "" });
    let active = rt.active().count();
    let open = rt.requests.iter().filter(|r| r.status == RequestStatus::Open).count();
    let _ = write!(
        s,
        "{active} operation(s) under way, {open} request(s) waiting. Treasury {}.",
        ctx.db.persisted.treasury(side)
    );
    s
}

/// The operations under way and the latest finished ones.
pub(crate) fn ops_text(ctx: &Context, side: Side) -> CompactString {
    let Some(rt) = ctx.hq.sides.get(&side) else {
        return "HQ has run no operations yet.".into();
    };
    let now = Utc::now();
    let mut s = CompactString::from("HQ -- OPERATIONS\n");
    let mut any = false;
    for o in rt.ops.iter().rev().take(12) {
        any = true;
        let mins = (now - o.started).num_minutes();
        let _ = writeln!(
            s,
            "#{} {} on {}: {}{} ({mins}m ago)",
            o.id,
            o.kind.label(),
            o.target_name,
            o.status.label(),
            if o.status == OpStatus::Active { CompactString::new("") } else { format_compact!(", {}", o.detail) },
        );
    }
    for r in rt.requests.iter().filter(|r| r.status == RequestStatus::Open) {
        any = true;
        let _ = writeln!(s, "request #{} {} at {} by {}: waiting", r.id, r.kind.label(), r.target_name, r.by);
    }
    if !any {
        s.push_str("Nothing under way.");
    }
    s
}
