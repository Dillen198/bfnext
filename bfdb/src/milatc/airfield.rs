// Copyright (c) 2026 Dillen Weerasinghe. All rights reserved.
// Proprietary and confidential. No license is granted to use, copy, modify, or
// distribute this file or the `milatc` module. See the repository NOTICE file.

//! An airfield's controllers ([`Controller`]) as a [`Facility`]: finds the
//! field in the engine's picture, lines the taxi network up with DCS once, and
//! hands everything else to the controller.

use super::{
    airport::P2,
    controller::{type_label, Caller, Controller, Tx},
    facility::{Facility, Picture},
    intent::Parsed,
    llm, Pos,
};
use crate::voice::FreqSpec;

pub(crate) struct Airfield {
    pub ctl: Controller,
    inst: String,
    align_tried: bool,
    missing_logged: bool,
}

impl Airfield {
    pub(crate) fn new(ctl: Controller, inst: &str) -> Self {
        Airfield {
            ctl,
            inst: inst.to_string(),
            align_tried: false,
            missing_logged: false,
        }
    }
}

impl Facility for Airfield {
    fn name(&self) -> String {
        format!("{} ATC", self.ctl.ap.callsign)
    }

    fn radios(&self) -> Vec<(Pos, Vec<FreqSpec>)> {
        Pos::ALL
            .iter()
            .filter_map(|p| self.ctl.ap.positions.get(p).map(|f| (*p, f.clone())))
            .collect()
    }

    fn facility(&self, pos: Pos) -> String {
        self.ctl.facility(pos)
    }

    fn label(&self, pos: Pos) -> String {
        pos.title().to_string()
    }

    fn wake(&self) -> String {
        self.ctl.ap.callsign.clone()
    }

    fn update(&mut self, pic: &Picture) -> Vec<Tx> {
        let Some(field) =
            pic.atc.airfields.iter().find(|a| self.ctl.ap.is_named(&a.name)).cloned()
        else {
            if !self.missing_logged {
                self.missing_logged = true;
                log::warn!(
                    "[{}] range ATC: {} is not in the engine's picture (has: {})",
                    self.inst,
                    self.ctl.ap.dcs_name,
                    pic.atc
                        .airfields
                        .iter()
                        .map(|a| a.name.as_str())
                        .collect::<Vec<_>>()
                        .join(", ")
                );
            }
            return vec![];
        };
        if self.ctl.ap.aligned.is_none() && !self.align_tried {
            let dcs: Vec<(String, P2)> = field
                .runways
                .iter()
                .filter_map(|r| {
                    Some((r.name.clone(), self.ctl.ap.local(r.thr_lat?, r.thr_lon?)))
                })
                .collect();
            if !dcs.is_empty() {
                self.align_tried = true;
                let shift = self.ctl.ap.alignment(&dcs);
                match self.ctl.ap.align(&dcs) {
                    Some(m) => log::info!(
                        "[{}] range ATC: {} ({}) taxi network aligned to DCS ({m:.0} m shift)",
                        self.inst,
                        self.ctl.ap.callsign,
                        self.ctl.ap.icao
                    ),
                    None => log::warn!(
                        "[{}] range ATC: {} ({}) taxi network is {} off the DCS runways ({}); taxi routes are given from the unshifted network",
                        self.inst,
                        self.ctl.ap.callsign,
                        self.ctl.ap.icao,
                        shift.map(|d| format!("{:.0} m", d.dist(P2::default()))).unwrap_or_else(|| "nowhere near".into()),
                        dcs.iter().map(|(n, _)| n.as_str()).collect::<Vec<_>>().join(", ")
                    ),
                }
            }
        }
        self.ctl.update(&field, &pic.atc.traffic, pic.now)
    }

    fn respond(
        &mut self,
        pos: Pos,
        who: &Caller,
        heard: &str,
        parsed: Option<Parsed>,
        now: f64,
    ) -> Vec<Tx> {
        self.ctl.respond(pos, who, heard, parsed, now)
    }

    fn caller(
        &mut self,
        who: &Caller,
        parsed: Option<&Parsed>,
    ) -> (Option<String>, Option<String>) {
        match self.ctl.identify(who, parsed).and_then(|k| self.ctl.flights.get(&k)) {
            Some(f) => (Some(f.call.clone()), type_label(&f.ac.typ)),
            None => (None, None),
        }
    }

    fn llm_context(&mut self, pos: Pos, who: &Caller, heard: &str) -> llm::Context {
        let key = self.ctl.identify(who, None);
        self.ctl.llm_context(pos, key, heard)
    }

    fn vocabulary(&self) -> Vec<String> {
        let mut v: Vec<String> = Pos::ALL.iter().map(|p| self.ctl.facility(*p)).collect();
        v.extend(self.ctl.flights.values().map(|f| f.call.clone()));
        v.extend(self.ctl.ap.runways.iter().map(|r| format!("runway {}", r.name)));
        v
    }
}
