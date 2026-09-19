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

#[derive(Debug, Clone)]
pub struct MsgQ(Vec<VecDeque<Cmd>>);

impl Default for MsgQ {
    fn default() -> Self {
        MsgQ(vec![
            VecDeque::default(),
            VecDeque::default(),
            VecDeque::default(),
        ])
    }
}

/// Replace a markup mutation that is still sitting in the queue for the same
/// mark, instead of queueing a second one behind it.
///
/// Every one of these means "make mark N look like this now", so only the last
/// one for a given mark carries any information -- the ones in front of it just
/// spend the per-second budget replaying states the campaign has already moved
/// past. That is what produced labels contradicting themselves (a base reading
/// "Health: 100" and ">> CAPTURABLE" at once, from two different renders) once
/// the queue got behind. Collapsing them also stops a backlog from growing
/// without bound: the queue holds at most one pending update per mark per kind.
macro_rules! coalesce {
    ($self:ident, $variant:ident { $id:ident, $field:ident }, $new:expr) => {{
        for cmd in $self.0[2].iter_mut() {
            if let Cmd::Send(Msg::$variant {
                id: qid,
                $field: qval,
            }) = cmd
                && *qid == $id
            {
                *qval = $new;
                return;
            }
        }
        $self.0[2].push_back(Cmd::Send(Msg::$variant {
            id: $id,
            $field: $new,
        }))
    }};
}

impl MsgQ {
    fn send_with_priority<S: Into<String>>(&mut self, p: usize, typ: MsgTyp, text: S) {
        self.0[p].push_back(Cmd::Send(Msg::Message {
            typ,
            text: text.into(),
        }))
    }

    pub fn send<S: Into<String>>(&mut self, typ: MsgTyp, text: S) {
        self.send_with_priority(0, typ, text)
    }

    pub fn delete_mark(&mut self, did: MarkId) {
        let mut push = true;
        let mut remove = |pri: usize| {
            self.0[pri].retain(|cmd| match cmd {
                Cmd::DeleteMark(_) => true,
                Cmd::Send(msg) => match msg {
                    // A pin create that has not drained yet is pointless once
                    // the same mark is being deleted -- drop BOTH. Without
                    // this a re-dropped pin left a dead create and a dead
                    // delete in the queue behind it, which is how pin churn
                    // doubled its own cost.
                    Msg::Message {
                        typ: MsgTyp::Mark { id, .. },
                        ..
                    } => {
                        if *id == did {
                            push = false;
                            false
                        } else {
                            true
                        }
                    }
                    Msg::Message { .. } => true,
                    Msg::Circle { id, .. }
                    | Msg::Rect { id, .. }
                    | Msg::Quad { id, .. }
                    | Msg::Text { id, .. }
                    | Msg::Arrow { id, .. }
                    | Msg::Line { id, .. }
                    | Msg::Freeform { id, .. } => {
                        if *id == did {
                            push = false;
                            false
                        } else {
                            true
                        }
                    }
                    Msg::SetMarkupColor { id, .. }
                    | Msg::SetMarkupFillColor { id, .. }
                    | Msg::SetMarkupText { id, .. }
                    | Msg::SetMarkupStart { id, .. }
                    | Msg::SetMarkupEnd { id, .. } => *id != did,
                },
            })
        };
        remove(0);
        remove(1);
        remove(2);
        if push {
            self.0[1].push_back(Cmd::DeleteMark(did))
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
        self.0[2].push_back(Cmd::Send(Msg::Circle {
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
        self.0[2].push_back(Cmd::Send(Msg::Rect {
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
        self.0[2].push_back(Cmd::Send(Msg::Quad {
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
        self.0[2].push_back(Cmd::Send(Msg::Text { id, to, spec }))
    }

    pub fn line_to_all(
        &mut self,
        to: SideFilter,
        id: MarkId,
        spec: LineSpec,
        message: Option<String>,
    ) {
        self.0[2].push_back(Cmd::Send(Msg::Line { id, to, spec, message }))
    }

    pub fn arrow_to(
        &mut self,
        to: SideFilter,
        id: MarkId,
        spec: ArrowSpec,
        message: Option<String>,
    ) {
        self.0[2].push_back(Cmd::Send(Msg::Arrow {
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
        self.0[2].push_back(Cmd::Send(Msg::Freeform {
            id,
            to,
            spec,
            message,
        }))
    }

    pub fn set_markup_color(&mut self, id: MarkId, color: Color) {
        coalesce!(self, SetMarkupColor { id, color }, color)
    }

    pub fn set_markup_fill_color(&mut self, id: MarkId, color: Color) {
        coalesce!(self, SetMarkupFillColor { id, color }, color)
    }

    pub fn set_markup_text(&mut self, id: MarkId, text: String) {
        coalesce!(self, SetMarkupText { id, text }, text)
    }

    pub fn set_markup_pos_start(&mut self, id: MarkId, pos: LuaVec3) {
        coalesce!(self, SetMarkupStart { id, pos }, pos)
    }

    pub fn set_markup_pos_end(&mut self, id: MarkId, pos: LuaVec3) {
        coalesce!(self, SetMarkupEnd { id, pos }, pos)
    }

    pub fn len(&self) -> usize {
        self.0.iter().fold(0, |acc, q| acc + q.len())
    }

    /// Queue depth per priority (chat/panels, marks, markup) plus a breakdown
    /// by command kind, biggest first. The depth on its own only says the F10
    /// map is behind; this says what is holding it up, which is the difference
    /// between raising the rate and stopping whatever keeps redrawing itself.
    pub fn depth_report(&self) -> ([usize; 3], std::string::String) {
        use std::fmt::Write;
        let mut counts: Vec<(&'static str, usize)> = vec![];
        for q in &self.0 {
            for cmd in q {
                let kind = Self::kind(cmd);
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
        ([self.0[0].len(), self.0[1].len(), self.0[2].len()], by_kind)
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

    pub fn process(&mut self, max_rate: usize, net: &Net, act: &Action) {
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
        let reserved = if self.0[2].is_empty() || max_rate < 2 {
            0
        } else {
            (max_rate / 3).max(1)
        };
        let urgent_budget = max_rate.saturating_sub(reserved);
        let mut urgent = 0;
        for _ in 0..max_rate {
            let cmd = if urgent < urgent_budget {
                match self.0[0].pop_front().or_else(|| self.0[1].pop_front()) {
                    Some(cmd) => {
                        urgent += 1;
                        Some(cmd)
                    }
                    None => self.0[2].pop_front(),
                }
            } else {
                match self.0[2].pop_front() {
                    Some(cmd) => Some(cmd),
                    None => self.0[0].pop_front().or_else(|| self.0[1].pop_front()),
                }
            };
            let cmd = match cmd {
                Some(cmd) => cmd,
                None => return,
            };
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
