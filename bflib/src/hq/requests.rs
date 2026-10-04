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

//! Players asking the HQ for support. A request does not buy anything by
//! itself: it raises the value of the operations that would answer it, so
//! the HQ answers it the next time it plans if the side can afford it and it
//! is not plainly worse than what else is on the list. The player is told
//! either way.

use super::{dist, OpStatus};
use crate::Context;
use bfprotocols::{cfg::HqCfg, db::objective::ObjectiveId, hq::RequestKind};
use chrono::{prelude::*, Duration};
use compact_str::{format_compact, CompactString};
use dcso3::{coalition::Side, net::Ucid, Vector2};

/// How far from the player a request with no objective named may look for
/// one.
const NEAREST_M: f64 = 80_000.;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum RequestStatus {
    Open,
    Answered,
    Declined,
    Expired,
}

impl RequestStatus {
    pub(crate) fn label(self) -> &'static str {
        match self {
            Self::Open => "open",
            Self::Answered => "answered",
            Self::Declined => "declined",
            Self::Expired => "expired",
        }
    }
}

#[derive(Debug, Clone)]
pub(crate) struct Request {
    pub(crate) id: u64,
    pub(crate) kind: RequestKind,
    pub(crate) ucid: Ucid,
    pub(crate) by: CompactString,
    pub(crate) target: ObjectiveId,
    pub(crate) target_name: CompactString,
    pub(crate) created: DateTime<Utc>,
    pub(crate) status: RequestStatus,
    pub(crate) answer: CompactString,
}

#[allow(clippy::too_many_arguments)]
pub(crate) fn submit(
    ctx: &mut Context,
    cfg: &HqCfg,
    side: Side,
    ucid: Ucid,
    kind: RequestKind,
    objective: Option<ObjectiveId>,
    from: Option<Vector2>,
    now: DateTime<Utc>,
) -> Result<CompactString, CompactString> {
    if !cfg.requests.enabled {
        return Err("this server's HQ does not take support requests".into());
    }
    let name: CompactString = ctx
        .db
        .player(&ucid)
        .map(|p| p.name.as_str().into())
        .ok_or_else(|| CompactString::from("unknown player"))?;
    let cooldown = Duration::seconds(cfg.requests.cooldown_secs as i64);
    if let Some(t) = ctx.hq.side(side).last_request.get(&ucid) {
        let left = cooldown - (now - *t);
        if left > Duration::zero() {
            return Err(format_compact!("you can ask again in {}s", left.num_seconds()));
        }
    }
    let open = ctx.hq.side(side).requests.iter().filter(|r| r.status == RequestStatus::Open).count();
    if open >= cfg.requests.max_open_per_side as usize {
        return Err(format_compact!("the HQ already has {open} requests waiting, try again shortly"));
    }
    let friendly = kind.on_friendly();
    let fits = |o: &crate::db::objective::Objective| {
        if friendly {
            o.owner() == side
        } else {
            o.owner() != side && o.owner() != Side::Neutral
        }
    };
    let target = match objective {
        Some(id) => {
            let o = ctx
                .db
                .persisted
                .objectives
                .get(&id)
                .ok_or_else(|| CompactString::from("no such objective"))?;
            if !fits(o) {
                return Err(format_compact!(
                    "{} is {}: {} goes on {} objectives",
                    o.name(),
                    if o.owner() == side { "ours" } else { "not ours" },
                    kind.label(),
                    if friendly { "friendly" } else { "enemy" }
                ));
            }
            id
        }
        None => {
            let p = from.ok_or_else(|| CompactString::from("name an objective (couldn't find where you are)"))?;
            let id = super::nearest_objective(ctx, p, fits)
                .ok_or_else(|| CompactString::from("there is no objective that fits"))?;
            let far = ctx.db.persisted.objectives.get(&id).map_or(f64::INFINITY, |o| dist(o.pos(), p));
            if far > NEAREST_M {
                return Err(format_compact!(
                    "the nearest {} objective is {:.0} km away, name one",
                    if friendly { "friendly" } else { "enemy" },
                    far / 1000.
                ));
            }
            id
        }
    };
    let target_name: CompactString = ctx
        .db
        .persisted
        .objectives
        .get(&target)
        .map(|o| o.name().into())
        .unwrap_or_default();
    let rt = ctx.hq.side(side);
    if let Some(dup) = rt
        .requests
        .iter()
        .find(|r| r.status == RequestStatus::Open && r.kind == kind && r.target == target)
    {
        return Err(format_compact!("{} already asked for {} at {target_name}", dup.by, kind.label()));
    }
    let id = ctx.hq.next_id();
    let rt = ctx.hq.side(side);
    rt.last_request.insert(ucid, now);
    rt.requests.push(Request {
        id,
        kind,
        ucid,
        by: name.clone(),
        target,
        target_name: target_name.clone(),
        created: now,
        status: RequestStatus::Open,
        answer: CompactString::new(""),
    });
    let text = format_compact!("HQ: {name} requests {} at {target_name}", kind.label());
    rt.note(now, text);
    // Answer at the next planning pass rather than a full interval away.
    if rt.last_think.is_some() {
        let soon = Duration::seconds(cfg.think_secs.saturating_sub(10) as i64);
        rt.last_think = Some(now - soon);
    }
    Ok(format_compact!(
        "HQ copies: {} at {target_name}. You will hear back when it is tasked (request #{id}).",
        kind.label()
    ))
}

pub(crate) fn cancel(
    ctx: &mut Context,
    side: Side,
    ucid: Option<&Ucid>,
    admin: bool,
    id: u64,
    now: DateTime<Utc>,
) -> Result<CompactString, CompactString> {
    let rt = ctx.hq.side(side);
    let r = rt
        .requests
        .iter_mut()
        .find(|r| r.id == id && r.status == RequestStatus::Open)
        .ok_or_else(|| format_compact!("no open request #{id}"))?;
    if !admin && ucid != Some(&r.ucid) {
        return Err("that is someone else's request".into());
    }
    r.status = RequestStatus::Declined;
    r.answer = "withdrawn".into();
    rt.note(now, format_compact!("HQ: request #{id} withdrawn"));
    Ok("request withdrawn".into())
}

/// An operation answering request `rid` has been launched.
pub(crate) fn answered(ctx: &mut Context, side: Side, rid: u64, what: &str) {
    let Some(r) = ctx.hq.side(side).requests.iter_mut().find(|r| r.id == rid) else { return };
    if r.status != RequestStatus::Open {
        return;
    }
    r.status = RequestStatus::Answered;
    r.answer = what.into();
    let (ucid, text) = (r.ucid, format_compact!("HQ: your {} request at {}: {what}", r.kind.label(), r.target_name));
    ctx.db.ephemeral.panel_to_player(&ctx.db.persisted, 15, &ucid, text);
}

/// The operation answering request `rid` has ended.
pub(crate) fn settle(ctx: &mut Context, side: Side, rid: u64, status: OpStatus, _now: DateTime<Utc>) {
    let Some(r) = ctx.hq.side(side).requests.iter().find(|r| r.id == rid) else { return };
    let (ucid, text) = (
        r.ucid,
        format_compact!("HQ: the {} you asked for at {} has {}", r.kind.label(), r.target_name, status.label()),
    );
    ctx.db.ephemeral.panel_to_player(&ctx.db.persisted, 10, &ucid, text);
}

/// Requests nobody has answered within the ttl are closed, with word to the
/// player, and old closed ones are forgotten.
pub(crate) fn expire(ctx: &mut Context, cfg: &HqCfg, side: Side, now: DateTime<Utc>) {
    let ttl = Duration::seconds(cfg.requests.ttl_secs as i64);
    let mut told: Vec<(Ucid, CompactString)> = vec![];
    let rt = ctx.hq.side(side);
    for r in rt.requests.iter_mut().filter(|r| r.status == RequestStatus::Open) {
        if now - r.created >= ttl {
            r.status = RequestStatus::Expired;
            r.answer = "not tasked: nothing the side could afford or reach".into();
            told.push((
                r.ucid,
                format_compact!(
                    "HQ: sorry, nothing available for your {} request at {}",
                    r.kind.label(),
                    r.target_name
                ),
            ));
        }
    }
    rt.requests.retain(|r| r.status == RequestStatus::Open || now - r.created < ttl * 3);
    for (u, t) in told {
        ctx.db.ephemeral.panel_to_player(&ctx.db.persisted, 10, &u, t);
    }
}
