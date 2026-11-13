//! Story events (`Cfg::story`, Red Storm Rising ENGINE.md 3.4).
//!
//! A campaign's story is a list of events. Each one waits for something
//! players or the AI do (an objective destroyed, captured or held, a unit
//! type shot down, supply routes cut, a stock or the will to fight crossing a
//! line, another event firing), then fires: it tells everyone what happened,
//! credits whoever caused it and changes the war through mechanisms the
//! engine already models (will, production, stocks, actions, points, the HQ,
//! intel, the end of the round).
//!
//! Nothing here runs on a clock or in a fixed order. Every trigger reads an
//! action or the state actions leave, and a state trigger (fuel, will) has to
//! be seen on the far side of its line before it can fire, so a side that
//! starts a round short of fuel does not set one off at the first tick.
//!
//! Events fire once per round unless `repeat` gives them a cooldown.
//! `requires` and `excludes` chain and branch them: two events that exclude
//! each other let whichever side gets there first decide which way the story
//! goes. The state machine lives in bflib (`db::story`); this module holds the
//! shape of the config and its checks.
//!
//! Enums are externally tagged (`{"ObjectiveDestroyed": {"objective": ...}}`):
//! serde's internally tagged enums buffer their fields, and with serde_json's
//! `arbitrary_precision` on a buffered float does not read back.

use super::{default_true, UnitTags};
use crate::hq::{Line, Posture};
use anyhow::{bail, Result};
use dcso3::coalition::Side;
use fxhash::{FxHashMap, FxHashSet};
use log::warn;
use serde_derive::{Deserialize, Serialize};
use std::collections::BTreeMap;

/// The longest trigger window kept, in minutes (a day).
pub const STORY_MAX_WINDOW_MINUTES: u32 = 24 * 60;

#[derive(Debug, Clone, Serialize, Deserialize, schemars::JsonSchema)]
pub struct StoryCfg {
    /// Off switch that keeps the block around. Default true.
    #[serde(default = "default_true")]
    pub enabled: bool,
    /// Actions (by name, per side, from `actions`) that start each round
    /// locked. An event's `UnlockAction` opens one: "B-52 raids authorised".
    #[serde(default)]
    pub locked_actions: FxHashMap<Side, Vec<String>>,
    /// The events, in no particular order: which fire, and when, is decided
    /// by play.
    #[serde(default)]
    pub events: Vec<StoryEventCfg>,
}

#[derive(Debug, Clone, Serialize, Deserialize, schemars::JsonSchema)]
pub struct StoryEventCfg {
    /// Unique within the campaign: "tuapse_burns".
    pub id: String,
    /// The headline: "Tuapse burns".
    pub title: String,
    /// What players are told. `{by}` is replaced by who caused it ("Shade,
    /// Kilo"), `{objective}` by the trigger's objective, `{side}` by the
    /// event's side and `{enemy}` by the other one.
    #[serde(default)]
    pub text: String,
    /// The side the event concerns, for the dashboard and `{side}`.
    #[serde(default)]
    pub side: Option<Side>,
    pub trigger: StoryTrigger,
    /// Events that must have fired this round before this one can.
    #[serde(default)]
    pub requires: Vec<String>,
    /// Events that must not have fired this round: a branch.
    #[serde(default)]
    pub excludes: Vec<String>,
    /// None: fires once per round. Some(cooldown): may fire again, at most
    /// once per cooldown, in seconds.
    #[serde(default)]
    pub repeat: Option<u32>,
    #[serde(default)]
    pub effects: Vec<StoryEffect>,
}

/// What sets an event off. Objectives are named as in the mission.
#[derive(Debug, Clone, Serialize, Deserialize, schemars::JsonSchema)]
pub enum StoryTrigger {
    /// The objective's health falls to 0 (its last unit or building
    /// destroyed). `owner`: only while that side held it.
    ObjectiveDestroyed {
        objective: String,
        #[serde(default)]
        owner: Option<Side>,
    },
    /// The objective is captured, by `by` if given.
    ObjectiveCaptured {
        objective: String,
        #[serde(default)]
        by: Option<Side>,
    },
    /// `by` captures the objective and holds it for `minutes` (campaign
    /// time). Holding it from the round's start does not count.
    ObjectiveHeld { objective: String, by: Side, minutes: u32 },
    /// The objective's health falls below `pct` percent (from at or above
    /// it, with the same owner): "the carrier under 75%".
    ObjectiveHealthBelow {
        objective: String,
        pct: u8,
        #[serde(default)]
        owner: Option<Side>,
    },
    /// `count` units killed within `window_minutes`. A unit matches when
    /// its DCS type is in `types` or it carries one of `tags`; both empty,
    /// any unit matches. `side`: only that side's units. `objective`: only
    /// units killed inside its zone, or within `radius_m` of its centre.
    Kills {
        count: u32,
        window_minutes: u32,
        #[serde(default)]
        types: Vec<String>,
        #[serde(default)]
        tags: UnitTags,
        #[serde(default)]
        side: Option<Side>,
        #[serde(default)]
        objective: Option<String>,
        #[serde(default)]
        radius_m: Option<f64>,
    },
    /// `count` convoys, supply flights or supply ships of `side` (any if
    /// None) destroyed within `window_minutes`.
    SupplyRoutesDestroyed {
        count: u32,
        window_minutes: u32,
        #[serde(default)]
        side: Option<Side>,
    },
    /// The side's fuel stock (all warehouse liquids it holds against their
    /// capacity) falls below `pct` percent.
    FuelBelow { side: Side, pct: f64 },
    /// The side's war will (`war_will`) falls below `value`.
    WillBelow { side: Side, value: f64 },
    /// The side holds fewer than `count` objectives (carrier groups
    /// included): "down to its last bases".
    ObjectivesHeldBelow { side: Side, count: u32 },
    /// The side's war will rises above `value`.
    WillAbove { side: Side, value: f64 },
    /// The side's airframes of `types` (DCS type names), summed over the
    /// warehouses of every base it holds, fall below `below`: "the Backfire
    /// regiment is down to its last aircraft". A type an `unlimited_aircraft`
    /// base holds never runs low. Meant for `ai_air_stock`, where AI losses
    /// come out of the same stock.
    StockBelow { side: Side, types: Vec<String>, below: u32 },
    /// Another event fired; this one is credited to the same people.
    EventFired { id: String },
}

/// What an event changes. Each maps onto something the engine already
/// models.
#[derive(Debug, Clone, Serialize, Deserialize, schemars::JsonSchema)]
pub enum StoryEffect {
    /// Move the side's war will (`war_will`, if on) by `delta`.
    Will { side: Side, delta: f64 },
    /// Scale the side's production from here on: `fuel_pct` for liquids,
    /// `equipment_pct` for equipment and materiel (-40 = 40% less). Effects
    /// compound.
    Production {
        side: Side,
        #[serde(default)]
        fuel_pct: f64,
        #[serde(default)]
        equipment_pct: f64,
    },
    /// Remove `pct` percent of `item` from `objective`'s warehouse, or from
    /// every logistics hub of `side` when no objective is given. `item` is
    /// "fuel" (all liquids), "equipment" (all equipment), "all", or one
    /// equipment item's name. Default "fuel".
    Stock {
        #[serde(default)]
        side: Option<Side>,
        #[serde(default)]
        objective: Option<String>,
        #[serde(default = "default_stock_item")]
        item: String,
        pct: f64,
    },
    /// Withdraw an action (`actions`) from the side for the rest of the
    /// round.
    LockAction { side: Side, action: String },
    /// Make an action available: one listed in `locked_actions`, or one an
    /// earlier event locked.
    UnlockAction { side: Side, action: String },
    /// Change an action's cost for the rest of the round.
    PriceAction { side: Side, action: String, cost: u32 },
    /// Points to every player credited with causing the event.
    Points { points: i32 },
    /// Point the side's theatre HQ (`smart_commander.hq`) for `hours`: a
    /// posture, a main effort (an enemy objective), objectives to defend or
    /// resupply first, ones to stay away from, line weights, an intent. A
    /// human commander's override still wins over it.
    Hq {
        side: Side,
        #[serde(default)]
        #[schemars(with = "Option<String>")]
        posture: Option<Posture>,
        #[serde(default)]
        main_effort: Option<String>,
        #[serde(default)]
        defend: Vec<String>,
        #[serde(default)]
        supply_priority: Vec<String>,
        #[serde(default)]
        avoid: Vec<String>,
        #[serde(default)]
        #[schemars(with = "BTreeMap<String, f64>")]
        weights: BTreeMap<Line, f64>,
        #[serde(default)]
        intent: Option<String>,
        #[serde(default = "default_hq_hours")]
        hours: f64,
    },
    /// Give `side` intel on every enemy unit within `radius_m` of the
    /// objective (needs `elint` or `player_recon` for the map contacts).
    RevealIntel {
        side: Side,
        objective: String,
        #[serde(default = "default_intel_radius")]
        radius_m: f64,
    },
    /// Credit `count` airframes of `type` (a DCS type name) to the warehouse
    /// of `objective`, for whichever side holds it: a squadron's
    /// replacements. Up to the base's capacity for the type; a base that
    /// never stocked it takes all of them.
    ReinforceAirframes {
        objective: String,
        #[serde(rename = "type")]
        typ: String,
        count: u32,
    },
    /// End the round: `winner` wins (through the usual victory countdown).
    EndRound { winner: Side },
    /// An extra in-game message, to one side or (None) both. The event's own
    /// title and text always go to everyone.
    Message {
        #[serde(default)]
        side: Option<Side>,
        text: String,
    },
    /// The side's last card (`db::nuclear`): an AI aircraft flies a nuclear
    /// weapon at the enemy's most dangerous push. The enemy is warned when it
    /// takes off and can shoot it down; if it gets there, the engine sets off
    /// the blast. Once a round, whichever event orders it first.
    NuclearStrike(NuclearStrikeCfg),
}

/// See `StoryEffect::NuclearStrike`.
#[derive(Debug, Clone, Serialize, Deserialize, schemars::JsonSchema)]
pub struct NuclearStrikeCfg {
    /// The side that strikes.
    pub side: Side,
    /// The aircraft's group in the .miz (plane section, late activation),
    /// e.g. a MiG-21bis with the RN-28. Its loadout is for show: the engine
    /// does the damage.
    pub template: String,
    /// Fighters that fly the same route as cover (group in the .miz).
    #[serde(default)]
    pub escort_template: Option<String>,
    /// Shown in the warnings, e.g. "MiG-21bis".
    #[serde(default = "default_nuke_aircraft")]
    pub aircraft: String,
    /// Metres above sea level for the run in (a low run is harder to
    /// catch). Default 600.
    #[serde(default = "default_nuke_alt")]
    pub altitude_m: f64,
    /// Metres per second. Default 220.
    #[serde(default = "default_nuke_speed")]
    pub speed_mps: f64,
    /// How far from an enemy-held objective's centre it releases. Default
    /// 3000 m.
    #[serde(default = "default_nuke_release")]
    pub release_m: f64,
    /// Everything within this of the burst is destroyed, both sides'
    /// ground units and ships. Default 1200 m.
    #[serde(default = "default_nuke_kill")]
    pub kill_radius_m: f64,
    /// Between `kill_radius_m` and this, each unit is destroyed with
    /// `damage_chance`. Default 2500 m.
    #[serde(default = "default_nuke_damage")]
    pub damage_radius_m: f64,
    /// Default 0.5.
    #[serde(default = "default_nuke_chance")]
    pub damage_chance: f64,
    /// Only enemy objectives within this of one of the side's own are
    /// targets. Default 150 km.
    #[serde(default = "default_nuke_reach")]
    pub max_range_m: f64,
    /// War will changes when it goes off: the striking side's (a reprieve)
    /// and the target side's.
    #[serde(default)]
    pub will_on_detonation: f64,
    #[serde(default)]
    pub enemy_will_on_detonation: f64,
    /// The striking side's war will change if the aircraft is shot down.
    #[serde(default)]
    pub will_on_intercept: f64,
    /// Points to every enemy pilot in the air when it is shot down.
    #[serde(default)]
    pub intercept_points: i32,
}

fn default_nuke_aircraft() -> String {
    "strike aircraft".into()
}
fn default_nuke_alt() -> f64 {
    600.
}
fn default_nuke_speed() -> f64 {
    220.
}
fn default_nuke_release() -> f64 {
    3000.
}
fn default_nuke_kill() -> f64 {
    1200.
}
fn default_nuke_damage() -> f64 {
    2500.
}
fn default_nuke_chance() -> f64 {
    0.5
}
fn default_nuke_reach() -> f64 {
    150_000.
}

fn default_stock_item() -> String {
    "fuel".into()
}
fn default_hq_hours() -> f64 {
    4.
}
fn default_intel_radius() -> f64 {
    15_000.
}

impl StoryTrigger {
    /// The objective the trigger is about, if any.
    pub fn objective(&self) -> Option<&str> {
        match self {
            Self::ObjectiveDestroyed { objective, .. }
            | Self::ObjectiveCaptured { objective, .. }
            | Self::ObjectiveHeld { objective, .. }
            | Self::ObjectiveHealthBelow { objective, .. } => Some(objective),
            Self::Kills { objective, .. } => objective.as_deref(),
            Self::SupplyRoutesDestroyed { .. }
            | Self::FuelBelow { .. }
            | Self::WillBelow { .. }
            | Self::ObjectivesHeldBelow { .. }
            | Self::WillAbove { .. }
            | Self::StockBelow { .. }
            | Self::EventFired { .. } => None,
        }
    }

    /// The trigger's window in minutes, for the ones that count.
    pub fn window_minutes(&self) -> Option<u32> {
        match self {
            Self::Kills { window_minutes, .. }
            | Self::SupplyRoutesDestroyed { window_minutes, .. } => {
                Some((*window_minutes).clamp(1, STORY_MAX_WINDOW_MINUTES))
            }
            _ => None,
        }
    }
}

impl StoryEffect {
    /// Every objective name the effect refers to.
    pub fn objectives(&self) -> Vec<&str> {
        match self {
            Self::Stock { objective, .. } => {
                objective.iter().map(|s| s.as_str()).collect()
            }
            Self::Hq { main_effort, defend, supply_priority, avoid, .. } => main_effort
                .iter()
                .chain(defend)
                .chain(supply_priority)
                .chain(avoid)
                .map(|s| s.as_str())
                .collect(),
            Self::RevealIntel { objective, .. }
            | Self::ReinforceAirframes { objective, .. } => vec![objective.as_str()],
            _ => vec![],
        }
    }
}

impl StoryCfg {
    pub fn event(&self, id: &str) -> Option<&StoryEventCfg> {
        self.events.iter().find(|e| e.id == id)
    }

    /// The longest window any trigger counts over, in minutes; at least an
    /// hour, so the "who caused it" lookups always have some history.
    pub fn max_window_minutes(&self) -> u32 {
        self.events
            .iter()
            .filter_map(|e| e.trigger.window_minutes())
            .max()
            .unwrap_or(0)
            .max(60)
    }

    /// Every objective name the story refers to.
    pub fn objective_names(&self) -> FxHashSet<&str> {
        let mut names = FxHashSet::default();
        for e in &self.events {
            names.extend(e.trigger.objective());
            for fx in &e.effects {
                names.extend(fx.objectives());
            }
        }
        names
    }

    /// Hard errors for what can never work (an empty or duplicate id);
    /// warnings for what is merely suspicious.
    pub fn validate(&self) -> Result<()> {
        let mut ids: FxHashSet<&str> = FxHashSet::default();
        for e in &self.events {
            if e.id.trim().is_empty() {
                bail!("story: an event has an empty id")
            }
            if !ids.insert(e.id.as_str()) {
                bail!("story: event id '{}' is used twice", e.id)
            }
        }
        for e in &self.events {
            for r in e.requires.iter().chain(&e.excludes) {
                if !ids.contains(r.as_str()) {
                    warn!("story: event '{}' refers to unknown event '{r}'", e.id)
                }
            }
            if let StoryTrigger::EventFired { id } = &e.trigger {
                if !ids.contains(id.as_str()) {
                    warn!("story: event '{}' waits for unknown event '{id}'", e.id)
                } else if id == &e.id {
                    bail!("story: event '{}' is triggered by itself", e.id)
                }
            }
            match &e.trigger {
                StoryTrigger::Kills { count: 0, .. }
                | StoryTrigger::SupplyRoutesDestroyed { count: 0, .. } => {
                    warn!(
                        "story: event '{}' counts to 0: it fires on the first one",
                        e.id
                    )
                }
                StoryTrigger::ObjectivesHeldBelow { count: 0, .. } => {
                    warn!("story: event '{}' waits for fewer than 0 objectives: it can never fire", e.id)
                }
                StoryTrigger::StockBelow { types, below, .. }
                    if types.is_empty() || *below == 0 =>
                {
                    warn!(
                        "story: event '{}' waits for a stock below {below} of {types:?}: it can never fire",
                        e.id
                    )
                }
                _ => (),
            }
            for fx in &e.effects {
                if let StoryEffect::ReinforceAirframes { count: 0, .. } = fx {
                    warn!("story: event '{}' reinforces 0 airframes", e.id)
                }
            }
            if e.requires.iter().any(|r| e.excludes.contains(r)) {
                warn!("story: event '{}' both requires and excludes an event: it can never fire", e.id)
            }
            if e.effects.is_empty() {
                warn!("story: event '{}' has no effects: it changes nothing", e.id)
            }
        }
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::cfg::UnitTag;

    #[test]
    fn parses_every_trigger_and_effect() {
        let c: StoryCfg = serde_json::from_str(
            r#"{
            "locked_actions": {"Blue": ["B-52 raid"]},
            "events": [
              {"id": "a", "title": "A", "trigger": {"ObjectiveDestroyed": {"objective": "Tuapse refinery"}},
               "effects": [{"Will": {"side": "Red", "delta": -6}},
                           {"Production": {"side": "Red", "fuel_pct": -40}},
                           {"Stock": {"objective": "Tuapse refinery", "pct": 50}},
                           {"Hq": {"side": "Red", "posture": "defensive", "defend": ["Krasnodar refinery"],
                                   "weights": {"air": 1.5}}}]},
              {"id": "b", "title": "B", "trigger": {"ObjectiveCaptured": {"objective": "Senaki-Kolkhi", "by": "Blue"}},
               "excludes": ["c"], "effects": [{"UnlockAction": {"side": "Blue", "action": "F-4E"}}]},
              {"id": "c", "title": "C", "trigger": {"ObjectiveHeld": {"objective": "Senaki-Kolkhi", "by": "Red", "minutes": 120}},
               "effects": [{"LockAction": {"side": "Blue", "action": "x"}}, {"PriceAction": {"side": "Red", "action": "y", "cost": 10}}]},
              {"id": "d", "title": "D", "trigger": {"ObjectiveHealthBelow": {"objective": "CVBG", "pct": 75, "owner": "Blue"}},
               "effects": [{"Points": {"points": 200}}]},
              {"id": "e", "title": "E", "repeat": 3600,
               "trigger": {"Kills": {"count": 6, "window_minutes": 60, "types": ["Tu-22M3"], "tags": ["Aircraft"], "side": "Red"}},
               "effects": [{"RevealIntel": {"side": "Blue", "objective": "Kutaisi"}}]},
              {"id": "f", "title": "F", "trigger": {"SupplyRoutesDestroyed": {"count": 3, "window_minutes": 600}},
               "effects": [{"Message": {"side": "Red", "text": "x"}}]},
              {"id": "g", "title": "G", "trigger": {"FuelBelow": {"side": "Red", "pct": 30}}, "effects": [{"EndRound": {"winner": "Blue"}}]},
              {"id": "h", "title": "H", "trigger": {"WillBelow": {"side": "Red", "value": 25}}, "effects": [{"Will": {"side": "Blue", "delta": 1}}]},
              {"id": "i", "title": "I", "trigger": {"WillAbove": {"side": "Red", "value": 80}}, "effects": [{"Will": {"side": "Blue", "delta": 1}}]},
              {"id": "j", "title": "J", "requires": ["a"], "trigger": {"EventFired": {"id": "a"}}, "effects": [{"Will": {"side": "Red", "delta": -1.5}}]},
              {"id": "k", "title": "K", "trigger": {"StockBelow": {"side": "Red", "types": ["Tu-22M3"], "below": 2}},
               "effects": [{"ReinforceAirframes": {"objective": "Mozdok", "type": "Tu-22M3", "count": 6}}]}
            ]}"#,
        )
        .unwrap();
        assert!(c.enabled);
        c.validate().unwrap();
        assert_eq!(c.events.len(), 11);
        match (&c.events[10].trigger, &c.events[10].effects[0]) {
            (
                StoryTrigger::StockBelow { side: Side::Red, types, below: 2 },
                StoryEffect::ReinforceAirframes { objective, typ, count: 6 },
            ) => {
                assert_eq!(types, &vec!["Tu-22M3".to_string()]);
                assert_eq!((objective.as_str(), typ.as_str()), ("Mozdok", "Tu-22M3"));
            }
            other => panic!("{other:?}"),
        }
        assert_eq!(c.locked_actions[&Side::Blue], vec!["B-52 raid".to_string()]);
        match &c.events[0].effects[3] {
            StoryEffect::Hq { posture, defend, weights, hours, .. } => {
                assert_eq!(*posture, Some(Posture::Defensive));
                assert_eq!(defend, &vec!["Krasnodar refinery".to_string()]);
                assert_eq!(weights[&Line::Air], 1.5);
                assert_eq!(*hours, 4.);
            }
            other => panic!("{other:?}"),
        }
        match &c.events[0].effects[2] {
            StoryEffect::Stock { item, pct, .. } => {
                assert_eq!((item.as_str(), *pct), ("fuel", 50.))
            }
            other => panic!("{other:?}"),
        }
        match &c.events[4].trigger {
            StoryTrigger::Kills { tags, .. } => assert!(tags.contains(UnitTag::Aircraft)),
            other => panic!("{other:?}"),
        }
        assert_eq!(c.max_window_minutes(), 600);
        assert_eq!(c.events[4].repeat, Some(3600));
        let names = c.objective_names();
        for n in [
            "Tuapse refinery",
            "Senaki-Kolkhi",
            "CVBG",
            "Kutaisi",
            "Krasnodar refinery",
            "Mozdok",
        ] {
            assert!(names.contains(n), "{n}");
        }
    }

    /// The Southern Flank sample (`miz/Scenarios/Red Storm Rising`) is a
    /// valid `story` block that names only objectives of CAUCASUS.md 2.1.
    #[test]
    fn the_southern_flank_sample_parses() {
        let raw =
            include_str!("../../../miz/Scenarios/Red Storm Rising/story.sample.json");
        let v: serde_json::Value = serde_json::from_str(raw).unwrap();
        let c: StoryCfg = serde_json::from_value(v["story"].clone()).unwrap();
        c.validate().unwrap();
        assert!(c.enabled);
        for id in [
            "tuapse_burns",
            "carrier_hit",
            "backfire_regiment_broken",
            "backfire_reserve",
            "surami_cut",
            "surami_breakthrough",
            "zestafoni_falls",
            "sochi_falls",
            "kutaisi_in_danger",
            "front_commander_lost",
            "fuel_crisis",
        ] {
            assert!(c.event(id).is_some(), "{id}");
        }
        const MAP: &[&str] = &[
            "Batumi",
            "Kobuleti",
            "Poti",
            "Batumi oil terminal",
            "Ozurgeti",
            "CVBG",
            "Senaki-Kolkhi",
            "Samtredia",
            "Zugdidi",
            "Enguri dam",
            "Kutaisi",
            "Zestafoni",
            "Chiatura",
            "Sachhere",
            "Surami tunnel",
            "Gori",
            "Tskhinvali",
            "Tbilisi",
            "Sukhumi-Babushara",
            "Gudauta",
            "Ochamchire",
            "Gagra",
            "Sochi-Adler",
            "Tuapse refinery",
            "Novorossiysk",
            "Krasnodar refinery",
            "Mozdok",
            "Ordzhonikidze",
        ];
        for n in c.objective_names() {
            assert!(MAP.contains(&n), "{n} is not on the Southern Flank map");
        }
        // war will never ends the round, so no event does either, and every
        // event changes the war
        assert!(c.events.iter().all(|e| {
            !e.effects.iter().any(|f| matches!(f, StoryEffect::EndRound { .. }))
        }));
        assert!(c.events.iter().all(|e| !e.effects.is_empty()));
        // and the whole sample is a valid part of a config
        let mut cfg = crate::cfg::Cfg::default();
        cfg.story = Some(c);
        cfg.validate().unwrap();
        // the short example in SAMPLE_CFG.json too
        let raw = include_str!("../../../miz/SAMPLE_CFG.json");
        let v: serde_json::Value =
            serde_json::from_str(raw.trim_start_matches('\u{feff}')).unwrap();
        let c: StoryCfg = serde_json::from_value(v["story"].clone()).unwrap();
        c.validate().unwrap();
        assert_eq!(c.events.len(), 1);
    }

    #[test]
    fn duplicate_or_self_triggered_ids_are_refused() {
        let dup: StoryCfg = serde_json::from_str(
            r#"{"events": [
              {"id": "a", "title": "A", "trigger": {"FuelBelow": {"side": "Red", "pct": 30}}},
              {"id": "a", "title": "B", "trigger": {"FuelBelow": {"side": "Red", "pct": 20}}}]}"#,
        )
        .unwrap();
        assert!(dup.validate().is_err());
        let own: StoryCfg = serde_json::from_str(
            r#"{"events": [{"id": "a", "title": "A", "trigger": {"EventFired": {"id": "a"}}}]}"#,
        )
        .unwrap();
        assert!(own.validate().is_err());
    }
}
