/*
Copyright 2024 Eric Stokes.

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

use dcso3::{
    Color, LuaVec3, String, Vector2, Vector3,
    coalition::Side,
    env::miz::{GroupId, UnitId},
    net::{Net, PlayerId},
    trigger::{
        Action, ArrowSpec, CircleSpec, LineSpec, MarkId, PolylineSpec, QuadSpec, RectSpec,
        SideFilter, TextSpec,
    },
};
use fxhash::FxHashMap;
use log::error;
use std::collections::VecDeque;

#[derive(Debug, Clone, Copy)]
pub enum PanelDest {
    All,
    Side(Side),
    Group(GroupId),
    Unit(UnitId),
}

#[derive(Debug, Clone, Copy)]
pub enum MarkDest {
    All,
    Side(Side),
    Group(GroupId),
}

#[derive(Debug, Clone)]
pub enum MsgTyp {
    Chat(Option<PlayerId>),
    Panel {
        to: PanelDest,
        display_time: i64,
        clear_view: bool,
    },
    Mark {
        id: MarkId,
        to: MarkDest,
        position: LuaVec3,
        read_only: bool,
    },
}

#[derive(Debug, Clone)]
pub enum Msg {
    Message {
        typ: MsgTyp,
        text: String,
    },
    Circle {
        id: MarkId,
        to: SideFilter,
        spec: CircleSpec,
        message: Option<String>,
    },
    Rect {
        id: MarkId,
        to: SideFilter,
        spec: RectSpec,
        message: Option<String>,
    },
    Quad {
        id: MarkId,
        to: SideFilter,
        spec: QuadSpec,
        message: Option<String>,
    },
    Text {
        id: MarkId,
        to: SideFilter,
        spec: TextSpec,
    },
    Arrow {
        id: MarkId,
        to: SideFilter,
        spec: ArrowSpec,
        message: Option<String>,
    },
    Line {
        id: MarkId,
        to: SideFilter,
        spec: LineSpec,
        message: Option<String>,
    },
    /// A connected multi-point shape drawn as ONE mark (markupToAll shapeId 7).
    Freeform {
        id: MarkId,
        to: SideFilter,
        spec: PolylineSpec,
        message: Option<String>,
    },
    SetMarkupColor {
        id: MarkId,
        color: Color,
    },
    SetMarkupFillColor {
        id: MarkId,
        color: Color,
    },
    SetMarkupText {
        id: MarkId,
        text: String,
    },
    SetMarkupStart {
        id: MarkId,
        pos: LuaVec3,
    },
    SetMarkupEnd {
        id: MarkId,
        pos: LuaVec3,
    },
}

#[derive(Debug, Clone)]
pub enum Cmd {
    Send(Msg),
    DeleteMark(MarkId),
}

impl Cmd {
    /// The mark this command draws or mutates. `DeleteMark` is deliberately
    /// not included: a delete is never itself cancelled by a later delete.
    fn mark_id(&self) -> Option<MarkId> {
        match self {
            Cmd::DeleteMark(_) => None,
            Cmd::Send(msg) => match msg {
                Msg::Message {
                    typ: MsgTyp::Mark { id, .. },
                    ..
                } => Some(*id),
                Msg::Message { .. } => None,
                Msg::Circle { id, .. }
                | Msg::Rect { id, .. }
                | Msg::Quad { id, .. }
                | Msg::Text { id, .. }
                | Msg::Arrow { id, .. }
                | Msg::Line { id, .. }
                | Msg::Freeform { id, .. }
                | Msg::SetMarkupColor { id, .. }
                | Msg::SetMarkupFillColor { id, .. }
                | Msg::SetMarkupText { id, .. }
                | Msg::SetMarkupStart { id, .. }
                | Msg::SetMarkupEnd { id, .. } => Some(*id),
            },
        }
    }

    /// Does this command create the mark (as opposed to mutating one)?
    fn is_create(&self) -> bool {
        match self {
            Cmd::Send(Msg::Message {
                typ: MsgTyp::Mark { .. },
                ..
            })
            | Cmd::Send(Msg::Circle { .. })
            | Cmd::Send(Msg::Rect { .. })
            | Cmd::Send(Msg::Quad { .. })
            | Cmd::Send(Msg::Text { .. })
            | Cmd::Send(Msg::Arrow { .. })
            | Cmd::Send(Msg::Line { .. })
            | Cmd::Send(Msg::Freeform { .. }) => true,
            Cmd::Send(_) | Cmd::DeleteMark(_) => false,
        }
    }

    fn mut_kind(&self) -> Option<MutKind> {
        match self {
            Cmd::Send(Msg::SetMarkupColor { .. }) => Some(MutKind::Color),
            Cmd::Send(Msg::SetMarkupFillColor { .. }) => Some(MutKind::FillColor),
            Cmd::Send(Msg::SetMarkupText { .. }) => Some(MutKind::Text),
            Cmd::Send(Msg::SetMarkupStart { .. }) => Some(MutKind::Start),
            Cmd::Send(Msg::SetMarkupEnd { .. }) => Some(MutKind::End),
            _ => None,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
enum MutKind {
    Color,
    FillColor,
    Text,
    Start,
    End,
}

#[derive(Debug, Clone)]
struct Queued {
    /// global issue order, across all three queues
    seq: u64,
    cmd: Cmd,
}

/// The F10 command queue: three priorities (chat/panels, marks, markup).
///
/// Deleting a mark cancels everything still queued for it, and a markup
/// mutation replaces the one still queued for the same mark. Both used to be
/// done by scanning the queues -- `retain` over all three for every delete,
/// a linear search of the markup queue for every mutation -- which went
/// quadratic exactly when it mattered, with thousands of commands backed up.
/// Now a delete leaves a tombstone (entries for that mark issued before it are
/// skipped when they reach the front) and pending mutations are found through
/// an index, so both are O(1).
#[derive(Debug, Clone, Default)]
pub struct MsgQ {
    q: [VecDeque<Queued>; 3],
    /// entries ever pushed to / popped from each queue; an entry's position
    /// is its per-queue sequence number minus `popped`
    pushed: [u64; 3],
    popped: [u64; 3],
    seq: u64,
    /// queued, not cancelled, commands per mark
    live: FxHashMap<MarkId, u32>,
    /// cancelled commands still sitting in the queues, per mark
    dead_by_id: FxHashMap<MarkId, u32>,
    /// total cancelled commands still sitting in the queues
    dead: usize,
    /// mark -> `seq` of its latest delete. Queued commands for the mark with
    /// a lower `seq` are cancelled.
    deleted: FxHashMap<MarkId, u64>,
    /// creates not yet sent to DCS: mark -> `seq`
    pending_create: FxHashMap<MarkId, u64>,
    /// mutations not yet sent: (mark, kind) -> (per-queue sequence, `seq`)
    pending_mut: FxHashMap<(MarkId, MutKind), (u64, u64)>,
}

impl MsgQ {
    fn is_dead(&self, e: &Queued) -> bool {
        match e.cmd.mark_id() {
            None => false,
            Some(id) => self.deleted.get(&id).map(|d| e.seq < *d).unwrap_or(false),
        }
    }

    fn push(&mut self, p: usize, cmd: Cmd) {
        let seq = self.seq;
        self.seq += 1;
        let qseq = self.pushed[p];
        self.pushed[p] += 1;
        if let Some(id) = cmd.mark_id() {
            *self.live.entry(id).or_default() += 1;
            if cmd.is_create() {
                self.pending_create.insert(id, seq);
            }
            if let Some(kind) = cmd.mut_kind() {
                self.pending_mut.insert((id, kind), (qseq, seq));
            }
        }
        self.q[p].push_back(Queued { seq, cmd })
    }

    /// Pop the next command of priority `p` that hasn't been cancelled.
    fn pop(&mut self, p: usize) -> Option<Cmd> {
        loop {
            let e = self.q[p].pop_front()?;
            let qseq = self.popped[p];
            self.popped[p] += 1;
            let Some(id) = e.cmd.mark_id() else {
                return Some(e.cmd);
            };
            if let Some(kind) = e.cmd.mut_kind() {
                if self.pending_mut.get(&(id, kind)) == Some(&(qseq, e.seq)) {
                    self.pending_mut.remove(&(id, kind));
                }
            }
            if self.is_dead(&e) {
                self.dead = self.dead.saturating_sub(1);
                let gone = match self.dead_by_id.get_mut(&id) {
                    Some(n) => {
                        *n = n.saturating_sub(1);
                        *n == 0
                    }
                    None => true,
                };
                if gone {
                    // nothing older than the delete is left, the tombstone
                    // has done its job
                    self.dead_by_id.remove(&id);
                    self.deleted.remove(&id);
                }
                continue;
            }
            if let Some(n) = self.live.get_mut(&id) {
                *n = n.saturating_sub(1);
                if *n == 0 {
                    self.live.remove(&id);
                }
            }
            if self.pending_create.get(&id) == Some(&e.seq) {
                self.pending_create.remove(&id);
            }
            return Some(e.cmd);
        }
    }

    /// Replace a markup mutation that is still sitting in the queue for the
    /// same mark, instead of queueing a second one behind it.
    ///
    /// Every one of these means "make mark N look like this now", so only the
    /// last one for a given mark carries any information -- the ones in front
    /// of it just spend the per-second budget replaying states the campaign
    /// has already moved past. That is what produced labels contradicting
    /// themselves (a base reading "Health: 100" and ">> CAPTURABLE" at once,
    /// from two different renders) once the queue got behind. Collapsing them
    /// also stops a backlog from growing without bound: the queue holds at
    /// most one pending update per mark per kind.
    fn coalesce(&mut self, cmd: Cmd) {
        let (Some(id), Some(kind)) = (cmd.mark_id(), cmd.mut_kind()) else {
            return self.push(2, cmd);
        };
        if let Some(&(qseq, seq)) = self.pending_mut.get(&(id, kind)) {
            let cancelled = self.deleted.get(&id).map(|d| seq < *d).unwrap_or(false);
            if !cancelled && qseq >= self.popped[2] {
                let i = (qseq - self.popped[2]) as usize;
                if let Some(e) = self.q[2].get_mut(i) {
                    if e.seq == seq {
                        e.cmd = cmd;
                        return;
                    }
                }
            }
        }
        self.push(2, cmd)
    }

    fn send_with_priority<S: Into<String>>(&mut self, p: usize, typ: MsgTyp, text: S) {
        self.push(
            p,
            Cmd::Send(Msg::Message {
                typ,
                text: text.into(),
            }),
        )
    }

    pub fn send<S: Into<String>>(&mut self, typ: MsgTyp, text: S) {
        self.send_with_priority(0, typ, text)
    }

    pub fn delete_mark(&mut self, did: MarkId) {
        // A create that has not drained yet is pointless once the same mark is
        // being deleted -- cancel BOTH. Without this a re-dropped pin left a
        // dead create and a dead delete in the queue behind it, which is how
        // pin churn doubled its own cost.
        let unsent = self.pending_create.remove(&did).is_some();
        if let Some(n) = self.live.remove(&did) {
            // everything still queued for this mark is now cancelled
            self.dead += n as usize;
            *self.dead_by_id.entry(did).or_default() += n;
            self.deleted.insert(did, self.seq);
            self.seq += 1;
        }
        if !unsent {
            self.push(1, Cmd::DeleteMark(did))
        }
    }

    pub fn mark_to_all<S: Into<String>>(
        &mut self,
        position: Vector2,
        read_only: bool,
        text: S,
    ) -> MarkId {
        let id = MarkId::new();
        self.send_with_priority(
            1,
            MsgTyp::Mark {
                id,
                to: MarkDest::All,
                position: LuaVec3(Vector3::new(position.x, 0., position.y)),
                read_only,
            },
            text,
        );
        id
    }

    pub fn mark_to_side<S: Into<String>>(
        &mut self,
        side: Side,
        position: Vector2,
        read_only: bool,
        text: S,
    ) -> MarkId {
        let id = MarkId::new();
        self.send_with_priority(
            1,
            MsgTyp::Mark {
                id,
                to: MarkDest::Side(side),
                position: LuaVec3(Vector3::new(position.x, 0., position.y)),
                read_only,
            },
            text,
        );
        id
    }

    pub fn mark_to_group<S: Into<String>>(
        &mut self,
        group: GroupId,
        position: Vector2,
        read_only: bool,
        text: S,
    ) -> MarkId {
        let id = MarkId::new();
        self.send_with_priority(
            1,
            MsgTyp::Mark {
                id,
                to: MarkDest::Group(group),
                position: LuaVec3(Vector3::new(position.x, 0., position.y)),
                read_only,
            },
            text,
        );
        id
    }

    pub fn panel_to_all<S: Into<String>>(&mut self, display_time: i64, clear_view: bool, text: S) {
        self.send_with_priority(
            0,
            MsgTyp::Panel {
                to: PanelDest::All,
                display_time,
                clear_view,
            },
            text,
        )
    }

    pub fn panel_to_side<S: Into<String>>(
        &mut self,
        display_time: i64,
        clear_view: bool,
        side: Side,
        text: S,
    ) {
        self.send_with_priority(
            0,
            MsgTyp::Panel {
                to: PanelDest::Side(side),
                display_time,
                clear_view,
            },
            text,
        )
    }

    pub fn panel_to_group<S: Into<String>>(
        &mut self,
        display_time: i64,
        clear_view: bool,
        group: GroupId,
        text: S,
    ) {
        self.send_with_priority(
            0,
            MsgTyp::Panel {
                to: PanelDest::Group(group),
                display_time,
                clear_view,
            },
            text,
        )
    }

    pub fn panel_to_unit<S: Into<String>>(
        &mut self,
        display_time: i64,
        clear_view: bool,
        unit: UnitId,
        text: S,
    ) {
        self.send_with_priority(
            0,
            MsgTyp::Panel {
                to: PanelDest::Unit(unit),
                display_time,
                clear_view,
            },
            text,
        )
    }

    pub fn circle_to_all(
        &mut self,
        to: SideFilter,
        id: MarkId,
        spec: CircleSpec,
        message: Option<String>,
    ) {
        self.push(2, Cmd::Send(Msg::Circle {
            id,
            to,
            spec,
            message,
        }))
    }

    pub fn rect_to_all(
        &mut self,
        to: SideFilter,
        id: MarkId,
        spec: RectSpec,
        message: Option<String>,
    ) {
        self.push(2, Cmd::Send(Msg::Rect {
            id,
            to,
            spec,
            message,
        }))
    }

    pub fn quad_to_all(
        &mut self,
        to: SideFilter,
        id: MarkId,
        spec: QuadSpec,
        message: Option<String>,
    ) {
        self.push(2, Cmd::Send(Msg::Quad {
            id,
            to,
            spec,
            message,
        }))
    }

    /// Text markup goes in the markup queue, not the mark queue, even though it
    /// reads like a label. It has to: `set_markup_text` and friends queue the
    /// *mutations* of a text mark at priority 2, and a mutation that overtakes
    /// its own create addresses a mark DCS hasn't drawn yet -- the update is
    /// lost and the label sits on whatever text it was created with. Keeping
    /// the create and its mutations in one queue keeps them in issue order.
    pub fn text_to_all(&mut self, to: SideFilter, id: MarkId, spec: TextSpec) {
        self.push(2, Cmd::Send(Msg::Text { id, to, spec }))
    }

    pub fn line_to_all(
        &mut self,
        to: SideFilter,
        id: MarkId,
        spec: LineSpec,
        message: Option<String>,
    ) {
        self.push(2, Cmd::Send(Msg::Line { id, to, spec, message }))
    }

    pub fn arrow_to(
        &mut self,
        to: SideFilter,
        id: MarkId,
        spec: ArrowSpec,
        message: Option<String>,
    ) {
        self.push(2, Cmd::Send(Msg::Arrow {
            id,
            to,
            spec,
            message,
        }))
    }

    /// Queue a connected multi-point shape. Priority 2, same as every other
    /// markup draw, so it shares the objective-markup budget rather than
    /// competing with group pins.
    pub fn freeform_to_all(
        &mut self,
        to: SideFilter,
        id: MarkId,
        spec: PolylineSpec,
        message: Option<String>,
    ) {
        self.push(2, Cmd::Send(Msg::Freeform {
            id,
            to,
            spec,
            message,
        }))
    }

    pub fn set_markup_color(&mut self, id: MarkId, color: Color) {
        self.coalesce(Cmd::Send(Msg::SetMarkupColor { id, color }))
    }

    pub fn set_markup_fill_color(&mut self, id: MarkId, color: Color) {
        self.coalesce(Cmd::Send(Msg::SetMarkupFillColor { id, color }))
    }

    pub fn set_markup_text(&mut self, id: MarkId, text: String) {
        self.coalesce(Cmd::Send(Msg::SetMarkupText { id, text }))
    }

    pub fn set_markup_pos_start(&mut self, id: MarkId, pos: LuaVec3) {
        self.coalesce(Cmd::Send(Msg::SetMarkupStart { id, pos }))
    }

    pub fn set_markup_pos_end(&mut self, id: MarkId, pos: LuaVec3) {
        self.coalesce(Cmd::Send(Msg::SetMarkupEnd { id, pos }))
    }

    /// Commands still to be sent (cancelled ones don't count).
    pub fn len(&self) -> usize {
        self.q
            .iter()
            .fold(0usize, |acc, q| acc + q.len())
            .saturating_sub(self.dead)
    }

    /// Queue depth per priority (chat/panels, marks, markup) plus a breakdown
    /// by command kind, biggest first. The depth on its own only says the F10
    /// map is behind; this says what is holding it up, which is the difference
    /// between raising the rate and stopping whatever keeps redrawing itself.
    pub fn depth_report(&self) -> ([usize; 3], std::string::String) {
        use std::fmt::Write;
        let mut counts: Vec<(&'static str, usize)> = vec![];
        let mut depth = [0usize; 3];
        for (p, q) in self.q.iter().enumerate() {
            for e in q {
                if self.is_dead(e) {
                    continue;
                }
                depth[p] += 1;
                let kind = Self::kind(&e.cmd);
                match counts.iter_mut().find(|(k, _)| *k == kind) {
                    Some((_, n)) => *n += 1,
                    None => counts.push((kind, 1)),
                }
            }
        }
        counts.sort_by(|a, b| b.1.cmp(&a.1));
        let mut by_kind = std::string::String::new();
        for (kind, n) in counts.iter().take(6) {
            if !by_kind.is_empty() {
                by_kind.push_str(", ");
            }
            let _ = write!(by_kind, "{kind}={n}");
        }
        (depth, by_kind)
    }

    fn kind(cmd: &Cmd) -> &'static str {
        match cmd {
            Cmd::DeleteMark(_) => "DeleteMark",
            Cmd::Send(Msg::Message {
                typ: MsgTyp::Mark { .. },
                ..
            }) => "Mark",
            Cmd::Send(Msg::Message {
                typ: MsgTyp::Chat(_),
                ..
            }) => "Chat",
            Cmd::Send(Msg::Message {
                typ: MsgTyp::Panel { .. },
                ..
            }) => "Panel",
            Cmd::Send(Msg::Circle { .. }) => "Circle",
            Cmd::Send(Msg::Rect { .. }) => "Rect",
            Cmd::Send(Msg::Quad { .. }) => "Quad",
            Cmd::Send(Msg::Text { .. }) => "Text",
            Cmd::Send(Msg::Arrow { .. }) => "Arrow",
            Cmd::Send(Msg::Line { .. }) => "Line",
            Cmd::Send(Msg::Freeform { .. }) => "Freeform",
            Cmd::Send(Msg::SetMarkupColor { .. }) => "SetMarkupColor",
            Cmd::Send(Msg::SetMarkupFillColor { .. }) => "SetMarkupFillColor",
            Cmd::Send(Msg::SetMarkupText { .. }) => "SetMarkupText",
            Cmd::Send(Msg::SetMarkupStart { .. }) => "SetMarkupStart",
            Cmd::Send(Msg::SetMarkupEnd { .. }) => "SetMarkupEnd",
        }
    }

    /// The commands of one drain pass of `max_rate`, in send order.
    fn take_batch(&mut self, max_rate: usize) -> Vec<Cmd> {
        // Draining in strict priority order starves the markup queue. Chat,
        // panels and group pins (priorities 0 and 1) are produced continuously
        // -- every event sends a panel, every vehicle under way re-pins itself
        // -- so while those are drained first and without limit, the F10 markup
        // behind them never gets a turn and the map's labels, rings and supply
        // arrows stop tracking the campaign altogether. Reserve a share of
        // every pass for the markup queue so it always makes progress.
        //
        // A budget of one leaves nothing to split -- reserving it would starve
        // chat and panels instead, which is the worse trade.
        // (Cancelled entries in the markup queue only cost a skip inside
        // `pop`, and when nothing real is left in it the budget falls back to
        // the other two.)
        let reserved = if self.q[2].is_empty() || max_rate < 2 {
            0
        } else {
            (max_rate / 3).max(1)
        };
        let urgent_budget = max_rate.saturating_sub(reserved);
        let mut urgent = 0;
        let mut batch = Vec::with_capacity(max_rate.min(64));
        for _ in 0..max_rate {
            let cmd = if urgent < urgent_budget {
                match self.pop(0).or_else(|| self.pop(1)) {
                    Some(cmd) => {
                        urgent += 1;
                        Some(cmd)
                    }
                    None => self.pop(2),
                }
            } else {
                match self.pop(2) {
                    Some(cmd) => Some(cmd),
                    None => self.pop(0).or_else(|| self.pop(1)),
                }
            };
            match cmd {
                Some(cmd) => batch.push(cmd),
                None => break,
            }
        }
        batch
    }

    pub fn process(&mut self, max_rate: usize, net: &Net, act: &Action) {
        for cmd in self.take_batch(max_rate) {
            let kind = Self::kind(&cmd);
            let res = match cmd {
                Cmd::DeleteMark(id) => act.remove_mark(id),
                Cmd::Send(Msg::Message { typ, text }) => match typ {
                    MsgTyp::Mark {
                        id,
                        to,
                        position,
                        read_only,
                    } => match to {
                        MarkDest::All => act.mark_to_all(id, text, position, read_only, None),
                        MarkDest::Side(side) => {
                            act.mark_to_coalition(id, text, position, side, read_only, None)
                        }
                        MarkDest::Group(group) => {
                            act.mark_to_group(id, text, position, group, read_only, None)
                        }
                    },
                    MsgTyp::Chat(to) => match to {
                        None => net.send_chat(text, true),
                        Some(id) => net.send_chat_to(text, id, Some(PlayerId::from(1))),
                    },
                    MsgTyp::Panel {
                        to,
                        display_time,
                        clear_view,
                    } => match to {
                        PanelDest::All => act.out_text(text, display_time, clear_view),
                        PanelDest::Group(gid) => {
                            act.out_text_for_group(gid, text, display_time, clear_view)
                        }
                        PanelDest::Side(side) => {
                            act.out_text_for_coalition(side, text, display_time, clear_view)
                        }
                        PanelDest::Unit(uid) => {
                            act.out_text_for_unit(uid, text, display_time, clear_view)
                        }
                    },
                },
                Cmd::Send(Msg::Circle {
                    id,
                    to,
                    spec,
                    message,
                }) => act.circle_to_all(to, id, spec, message),
                Cmd::Send(Msg::Rect {
                    id,
                    to,
                    spec,
                    message,
                }) => act.rect_to_all(to, id, spec, message),
                Cmd::Send(Msg::Quad {
                    id,
                    to,
                    spec,
                    message,
                }) => act.quad_to_all(to, id, spec, message),
                Cmd::Send(Msg::Text { id, to, spec }) => act.text_to_all(to, id, spec),
                Cmd::Send(Msg::Arrow {
                    id,
                    to,
                    spec,
                    message,
                }) => act.arrow_to_all(to, id, spec, message),
                Cmd::Send(Msg::Line {
                    id,
                    to,
                    spec,
                    message,
                }) => act.line_to_all(to, id, spec, message),
                Cmd::Send(Msg::Freeform {
                    id,
                    to,
                    spec,
                    message,
                }) => act.freeform_to_all(to, id, spec, message),
                Cmd::Send(Msg::SetMarkupColor { id, color }) => act.set_markup_color(id, color),
                Cmd::Send(Msg::SetMarkupFillColor { id, color }) => {
                    act.set_markup_fill_color(id, color)
                }
                Cmd::Send(Msg::SetMarkupStart { id, pos }) => {
                    act.set_markup_position_start(id, pos)
                }
                Cmd::Send(Msg::SetMarkupEnd { id, pos }) => act.set_markup_position_end(id, pos),
                Cmd::Send(Msg::SetMarkupText { id, text }) => act.set_markup_text(id, text),
            };
            if let Err(e) = res {
                error!("could not send message ({kind}) {:?}", e)
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn pin(q: &mut MsgQ, text: &str) -> MarkId {
        q.mark_to_all(Vector2::new(0., 0.), true, text)
    }

    fn texts(cmds: &[Cmd]) -> Vec<std::string::String> {
        cmds.iter()
            .map(|c| match c {
                Cmd::DeleteMark(_) => "delete".into(),
                Cmd::Send(Msg::Message { text, .. }) => text.as_str().into(),
                Cmd::Send(Msg::SetMarkupText { text, .. }) => format!("set:{}", text.as_str()),
                _ => "other".into(),
            })
            .collect()
    }

    #[test]
    fn msgq_delete_before_drain_cancels_create_and_delete() {
        let mut q = MsgQ::default();
        let a = pin(&mut q, "a");
        let _b = pin(&mut q, "b");
        q.delete_mark(a);
        assert_eq!(q.len(), 1);
        let out = q.take_batch(10);
        assert_eq!(texts(&out), vec!["b"]);
        assert_eq!(q.len(), 0);
        // the tombstone is gone once the cancelled entry drained
        assert!(q.deleted.is_empty() && q.dead_by_id.is_empty() && q.dead == 0);
    }

    #[test]
    fn msgq_delete_after_drain_queues_delete_and_cancels_mutations() {
        let mut q = MsgQ::default();
        let a = pin(&mut q, "a");
        assert_eq!(q.take_batch(10).len(), 1);
        q.set_markup_text(a, "one".into());
        q.delete_mark(a);
        assert_eq!(texts(&q.take_batch(10)), vec!["delete"]);
    }

    #[test]
    fn msgq_mutations_coalesce() {
        let mut q = MsgQ::default();
        let a = pin(&mut q, "a");
        q.take_batch(10);
        q.set_markup_text(a, "one".into());
        q.set_markup_text(a, "two".into());
        q.set_markup_text(a, "three".into());
        assert_eq!(q.len(), 1);
        assert_eq!(texts(&q.take_batch(10)), vec!["set:three"]);
        // once sent, a new mutation queues again
        q.set_markup_text(a, "four".into());
        assert_eq!(texts(&q.take_batch(10)), vec!["set:four"]);
    }

    #[test]
    fn msgq_mutation_after_delete_is_kept() {
        let mut q = MsgQ::default();
        let a = pin(&mut q, "a");
        q.take_batch(10);
        q.set_markup_text(a, "old".into());
        q.delete_mark(a);
        // issued after the delete: not cancelled by it, and not folded into
        // the cancelled one either
        q.set_markup_text(a, "new".into());
        assert_eq!(q.len(), 2);
        assert_eq!(texts(&q.take_batch(10)), vec!["delete", "set:new"]);
    }

    #[test]
    fn msgq_coalesce_survives_partial_drain() {
        let mut q = MsgQ::default();
        let a = pin(&mut q, "a");
        let b = pin(&mut q, "b");
        q.take_batch(10);
        q.set_markup_text(a, "a1".into());
        q.set_markup_text(b, "b1".into());
        // drain only the first mutation, then update the second in place
        let first = q.pop(2).unwrap();
        assert_eq!(texts(&[first]), vec!["set:a1"]);
        q.set_markup_text(b, "b2".into());
        assert_eq!(texts(&q.take_batch(10)), vec!["set:b2"]);
    }
}
