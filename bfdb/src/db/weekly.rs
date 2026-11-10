//! The weekly campaign report: what a server's war looked like over the last
//! few days, for the poster admins post to Discord (bfweb and bfrsr-web,
//! `/admin/report`; the FowlEngine plugin's weekly post).
//!
//! Built on read from the trees the rest of the stats come from, never
//! stored. Everything in it is already public elsewhere -- objectives held,
//! captures, kills, story events, the leaderboard -- except the per-side
//! count of hidden SAM sites, which is a count only: no site, no position.

use super::*;
use std::collections::{HashMap, HashSet};

/// One side's column on the FORCES board.
#[derive(Debug, Clone, Default, PartialEq, Serialize)]
pub(crate) struct SideForces {
    /// Objectives held, SAM sites left out (as `/api/stats` counts them).
    pub(crate) objectives: u32,
    pub(crate) airfields: u32,
    pub(crate) fobs: u32,
    pub(crate) farps: u32,
    /// The carrier and amphibious groups it still has, by name.
    pub(crate) fleets: Vec<std::string::String>,
    pub(crate) naval_bases: u32,
    pub(crate) sam_sites: u32,
    pub(crate) command_centres: u32,
    pub(crate) factories: u32,
    /// Units its pilots deployed in the window (crates unpacked, troops,
    /// FARPs).
    pub(crate) deployed: u32,
    /// Objectives it took in the window.
    pub(crate) captured: u32,
    pub(crate) air_kills: u32,
    pub(crate) ground_kills: u32,
}

#[derive(Debug, Clone, Default, PartialEq, Serialize)]
pub(crate) struct Forces {
    pub(crate) blue: SideForces,
    pub(crate) red: SideForces,
    pub(crate) neutral: u32,
}

#[derive(Debug, Clone, PartialEq, Serialize)]
pub(crate) struct WeeklyCapture {
    pub(crate) time: DateTime<Utc>,
    pub(crate) objective: std::string::String,
    pub(crate) side: std::string::String,
    pub(crate) by: Vec<std::string::String>,
}

#[derive(Debug, Clone, Default, PartialEq, Serialize)]
pub(crate) struct WeeklyPilot {
    pub(crate) name: std::string::String,
    pub(crate) side: Option<std::string::String>,
    pub(crate) squadron: Option<std::string::String>,
    pub(crate) air_kills: u32,
    pub(crate) ground_kills: u32,
    pub(crate) captures: u32,
    pub(crate) deploys: u32,
    pub(crate) score: u64,
}

#[derive(Debug, Clone, Default, PartialEq, Serialize)]
pub(crate) struct WeeklySquadron {
    pub(crate) tag: std::string::String,
    pub(crate) name: std::string::String,
    pub(crate) pilots: u32,
    pub(crate) air_kills: u32,
    pub(crate) ground_kills: u32,
    pub(crate) captures: u32,
    pub(crate) score: u64,
}

/// The Last Stand: a side's nuclear strike (bflib `db/nuclear.rs`). Ordered
/// by a story event whose effects say "orders a nuclear strike"; its outcome
/// is the story event `last_stand_intercepted` or `last_stand_detonated`.
#[derive(Debug, Clone, PartialEq, Serialize)]
pub(crate) struct LastStand {
    pub(crate) side: Option<std::string::String>,
    pub(crate) ordered: Option<DateTime<Utc>>,
    /// "ordered" (nothing decided yet), "intercepted" or "detonated".
    pub(crate) outcome: std::string::String,
    /// When the outcome was decided (or the order, while it is pending).
    pub(crate) time: DateTime<Utc>,
    pub(crate) title: std::string::String,
    /// Who shot it down.
    pub(crate) by: Vec<std::string::String>,
    /// Whether any of it fell inside the report's window.
    pub(crate) this_week: bool,
}

#[derive(Debug, Clone, Serialize)]
pub(crate) struct WeeklyReport {
    pub(crate) round: Option<u64>,
    pub(crate) round_start: Option<DateTime<Utc>>,
    pub(crate) since: DateTime<Utc>,
    pub(crate) until: DateTime<Utc>,
    pub(crate) forces: Forces,
    /// The window's captures, newest first, at most `MAX_CAPTURES`.
    pub(crate) captures: Vec<WeeklyCapture>,
    pub(crate) capture_count: u32,
    /// Most fought-over objectives in the window.
    pub(crate) contested: Vec<ContestedObjective>,
    /// Story events fired in the window, oldest first.
    pub(crate) story: Vec<StoryEventStat>,
    pub(crate) last_stand: Option<LastStand>,
    pub(crate) will: Option<WarWillStat>,
    /// The window's best pilots and squadrons, best first.
    pub(crate) pilots: Vec<WeeklyPilot>,
    pub(crate) squadrons: Vec<WeeklySquadron>,
    /// Pilots who scored anything in the window.
    pub(crate) active_pilots: u32,
}

pub(crate) const MAX_CAPTURES: usize = 12;
pub(crate) const TOP_PILOTS: usize = 5;
pub(crate) const TOP_SQUADRONS: usize = 3;

/// The same all-round score the round summary ranks by: kills, captures and
/// logistics all count.
pub(crate) fn score(air: u32, ground: u32, captures: u32, deploys: u32) -> u64 {
    (air + ground) as u64 * 10 + captures as u64 * 30 + deploys as u64 * 5
}

fn side_name(s: Side) -> std::string::String {
    format!("{s:?}")
}

/// Count one objective into the forces board.
pub(crate) fn tally(f: &mut Forces, owner: Side, name: &str, kind: &ObjectiveKind) {
    let s = match owner {
        Side::Blue => &mut f.blue,
        Side::Red => &mut f.red,
        _ => {
            if !kind.is_special_sam_site() {
                f.neutral += 1;
            }
            return;
        }
    };
    match kind {
        ObjectiveKind::SpecialSamSite => {
            s.sam_sites += 1;
            return;
        }
        ObjectiveKind::Airbase => s.airfields += 1,
        ObjectiveKind::Fob => s.fobs += 1,
        ObjectiveKind::Farp { .. } => s.farps += 1,
        ObjectiveKind::CarrierGroup { .. } => s.fleets.push(name.to_string()),
        ObjectiveKind::NavalBase => s.naval_bases += 1,
        ObjectiveKind::CommandCenter => s.command_centres += 1,
        ObjectiveKind::Factory { .. } => s.factories += 1,
        ObjectiveKind::Logistics => (),
    }
    s.objectives += 1;
}

/// The Last Stand from a round's story events, if one was ordered.
pub(crate) fn last_stand(
    story: &[StoryEventStat],
    since: DateTime<Utc>,
    until: DateTime<Utc>,
) -> Option<LastStand> {
    let order = story.iter().find(|e| {
        e.effects.iter().any(|x| x.to_ascii_lowercase().contains("nuclear strike"))
    });
    let outcome = story.iter().rev().find(|e| {
        e.id.as_str() == "last_stand_intercepted"
            || e.id.as_str() == "last_stand_detonated"
    });
    if order.is_none() && outcome.is_none() {
        return None;
    }
    let ordered = order.map(|e| e.time);
    let (kind, time, title, by, side) = match outcome {
        Some(e) => (
            e.id.trim_start_matches("last_stand_").to_string(),
            e.time,
            e.title.to_string(),
            e.caused_by.iter().map(|s| s.to_string()).collect(),
            e.side.or(order.and_then(|o| o.side)),
        ),
        None => {
            let o = order.expect("one of the two is there");
            ("ordered".to_string(), o.time, o.title.to_string(), vec![], o.side)
        }
    };
    let within = |t: DateTime<Utc>| t >= since && t < until;
    let this_week = within(time) || ordered.is_some_and(within);
    Some(LastStand {
        side: side.map(side_name),
        ordered,
        outcome: kind,
        time,
        title,
        by,
        this_week,
    })
}

#[derive(Default)]
struct Tally {
    air: u32,
    ground: u32,
    captures: u32,
    deploys: u32,
}

impl StatsDb {
    /// The weekly report for `rid` over `[since, until)`.
    pub(crate) fn weekly_report(
        &self,
        rid: RoundId,
        since: DateTime<Utc>,
        until: DateTime<Utc>,
    ) -> Result<WeeklyReport> {
        let round_start = self
            .all_rounds()?
            .into_iter()
            .find(|(_, id, _)| *id == rid)
            .map(|(_, _, r)| r.start);
        let within = |t: DateTime<Utc>| t >= since && t < until;

        let mut forces = Forces::default();
        for (_, o) in self.objectives_for_round(rid)? {
            tally(&mut forces, o.owner, &o.name, &o.kind);
        }
        for s in [&mut forces.blue, &mut forces.red] {
            s.fleets.sort();
        }

        // Each pilot's side this round, for their kills, captures and deploys.
        let mut side_of: HashMap<Ucid, Side> = HashMap::new();
        for r in self.pilots.round_info.iter() {
            let ((ucid, r_id), ri) = r?;
            if r_id == rid {
                side_of.insert(ucid, ri.side.1);
            }
        }
        let mut per: HashMap<Ucid, Tally> = HashMap::new();

        // Kills: one per KillId, credited to the shooter of the hit.
        let mut seen: HashSet<KillId> = HashSet::new();
        for r in self.kills.iter() {
            let ((_, r_id, kid), dead) = r?;
            if r_id != rid || !within(dead.time) || !seen.insert(kid) {
                continue;
            }
            let Some(shot) =
                dead.shots.iter().find(|s| s.hit).or_else(|| dead.shots.first())
            else {
                continue;
            };
            let air = self.victim_is_air(rid, &dead.victim)?;
            let s = match shot.shooter.side() {
                Side::Blue => &mut forces.blue,
                Side::Red => &mut forces.red,
                _ => continue,
            };
            if air {
                s.air_kills += 1
            } else {
                s.ground_kills += 1
            }
            if let Some(u) = shot.shooter.ucid() {
                if *u != Ucid::default() {
                    let t = per.entry(*u).or_default();
                    if air {
                        t.air += 1
                    } else {
                        t.ground += 1
                    }
                }
            }
        }

        // Captures.
        let mut caps: Vec<CaptureRecord> = Vec::new();
        for r in self.captures.scan_prefix(&rid)? {
            let c = r?.1;
            if within(c.time) {
                caps.push(c);
            }
        }
        caps.sort_by_key(|c| std::cmp::Reverse(c.time));
        let mut contested: HashMap<std::string::String, u32> = HashMap::new();
        for c in &caps {
            *contested.entry(c.objective_name.clone()).or_default() += 1;
            match c.side {
                Side::Blue => forces.blue.captured += 1,
                Side::Red => forces.red.captured += 1,
                _ => (),
            }
            for u in &c.by {
                per.entry(*u).or_default().captures += 1;
            }
        }
        let mut contested: Vec<ContestedObjective> = contested
            .into_iter()
            .filter(|(_, n)| *n > 1)
            .map(|(name, captures)| ContestedObjective { name, captures })
            .collect();
        contested.sort_by(|a, b| b.captures.cmp(&a.captures).then(a.name.cmp(&b.name)));
        contested.truncate(3);
        let name_of = |u: &Ucid| self.pilot_name(u).unwrap_or_else(|| "Unknown".into());
        let captures: Vec<WeeklyCapture> = caps
            .iter()
            .take(MAX_CAPTURES)
            .map(|c| WeeklyCapture {
                time: c.time,
                objective: c.objective_name.clone(),
                side: side_name(c.side),
                by: c.by.iter().map(name_of).collect(),
            })
            .collect();

        // Deploys.
        for r in self.deploys.iter() {
            let ((ucid, r_id, _), d) = r?;
            if r_id != rid || !within(d.time) {
                continue;
            }
            per.entry(ucid).or_default().deploys += 1;
            match side_of.get(&ucid) {
                Some(Side::Blue) => forces.blue.deployed += 1,
                Some(Side::Red) => forces.red.deployed += 1,
                _ => (),
            }
        }

        // Pilots and squadrons.
        let sq = self.squadrons();
        let mut pilots: Vec<WeeklyPilot> = Vec::new();
        let mut squads: HashMap<std::string::String, WeeklySquadron> = HashMap::new();
        for (ucid, t) in &per {
            let s = score(t.air, t.ground, t.captures, t.deploys);
            if s == 0 {
                continue;
            }
            let tag = sq.of(ucid).ok().flatten().map(|t| t.to_string());
            if let Some(tag) = &tag {
                let e = squads.entry(tag.clone()).or_insert_with(|| WeeklySquadron {
                    tag: tag.clone(),
                    name: sq
                        .get(tag)
                        .ok()
                        .flatten()
                        .map(|s| s.name.to_string())
                        .unwrap_or_default(),
                    ..Default::default()
                });
                e.pilots += 1;
                e.air_kills += t.air;
                e.ground_kills += t.ground;
                e.captures += t.captures;
                e.score += s;
            }
            pilots.push(WeeklyPilot {
                name: name_of(ucid),
                side: side_of.get(ucid).map(|s| side_name(*s)),
                squadron: tag,
                air_kills: t.air,
                ground_kills: t.ground,
                captures: t.captures,
                deploys: t.deploys,
                score: s,
            });
        }
        let active_pilots = pilots.len() as u32;
        pilots.sort_by(|a, b| b.score.cmp(&a.score).then(a.name.cmp(&b.name)));
        pilots.truncate(TOP_PILOTS);
        let mut squadrons: Vec<WeeklySquadron> = squads.into_values().collect();
        squadrons.sort_by(|a, b| b.score.cmp(&a.score).then(a.tag.cmp(&b.tag)));
        squadrons.truncate(TOP_SQUADRONS);

        let all_story = self.story_events(rid).unwrap_or_default();
        let last_stand = last_stand(&all_story, since, until);
        let story = all_story.into_iter().filter(|e| within(e.time)).collect();

        Ok(WeeklyReport {
            round: Some(rid.0),
            round_start,
            since,
            until,
            forces,
            capture_count: caps.len() as u32,
            captures,
            contested,
            story,
            last_stand,
            will: self.war_will(rid),
            pilots,
            squadrons,
            active_pilots,
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn ev(id: &str, mins: i64, effects: &[&str]) -> StoryEventStat {
        StoryEventStat {
            id: id.into(),
            time: Utc.timestamp_opt(1_760_000_000 + mins * 60, 0).unwrap(),
            title: format!("title {id}").into(),
            text: "".into(),
            caused_by: vec!["Viper".into()],
            side: Some(Side::Red),
            effects: effects.iter().map(|s| String::from(*s)).collect(),
        }
    }

    #[test]
    fn the_board_counts_objectives_by_kind() {
        let mut f = Forces::default();
        tally(&mut f, Side::Blue, "Batumi", &ObjectiveKind::Airbase);
        tally(&mut f, Side::Blue, "FOB 1", &ObjectiveKind::Fob);
        tally(&mut f, Side::Blue, "SAM", &ObjectiveKind::SpecialSamSite);
        tally(
            &mut f,
            Side::Red,
            "Moskva group",
            &ObjectiveKind::CarrierGroup {
                carrier_template: "".into(),
                waypoint: None,
                parent_naval_base: None,
                repair_start_time: None,
            },
        );
        tally(&mut f, Side::Neutral, "Nowhere", &ObjectiveKind::Logistics);
        assert_eq!(f.blue.objectives, 2, "the SAM site is not an objective here");
        assert_eq!((f.blue.airfields, f.blue.fobs, f.blue.sam_sites), (1, 1, 1));
        assert_eq!(f.red.fleets, vec!["Moskva group".to_string()]);
        assert_eq!(f.red.objectives, 1);
        assert_eq!(f.neutral, 1);
    }

    #[test]
    fn the_last_stand_is_read_from_the_story() {
        let since = Utc.timestamp_opt(1_760_000_000 + 30 * 60, 0).unwrap();
        let until = since + chrono::Duration::days(7);
        assert_eq!(
            last_stand(&[ev("aleppo_falls", 0, &["Blue will +5"])], since, until),
            None
        );

        let ordered = [ev("collapse", 10, &["Red orders a nuclear strike"])];
        let ls = last_stand(&ordered, since, until).unwrap();
        assert_eq!(ls.outcome, "ordered");
        assert!(!ls.this_week, "ordered before the window");

        let shot = [
            ev("collapse", 10, &["Red orders a nuclear strike"]),
            ev("last_stand_intercepted", 50, &[]),
        ];
        let ls = last_stand(&shot, since, until).unwrap();
        assert_eq!(ls.outcome, "intercepted");
        assert_eq!(ls.by, vec!["Viper".to_string()]);
        assert_eq!(ls.side.as_deref(), Some("Red"));
        assert!(ls.this_week);
        assert_eq!(ls.ordered, Some(shot[0].time));
    }

    #[test]
    fn the_score_weights_captures_most() {
        assert_eq!(score(1, 1, 1, 1), 20 + 30 + 5);
    }
}
