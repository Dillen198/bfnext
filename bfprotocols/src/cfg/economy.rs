//! The fairness layer over player points (`Cfg::economy`).
//!
//! `PointsCfg` says what things are worth; this says who gets them and how
//! much a given pilot actually banks:
//!
//! - **Underdog pay.** A side behind on territory, or outnumbered in the
//!   air, earns more for the same work -- player points and (optionally) the
//!   commander's treasury income -- so the losing side can still afford to
//!   fight back instead of spiralling.
//! - **Wealth taper.** Earnings that take a pilot past a multiple of their
//!   side's typical balance are paid at a reduced rate, so a veteran's lead
//!   stops compounding without anything being taken away.
//! - **Late joiners** start at a share of their side's typical balance rather
//!   than at the bare `new_player_join`. That head start can be spent but not
//!   `-transfer`red, so it can't be farmed with alt accounts.
//! - **Role pay.** Ground kills are priced by what was destroyed (a tank is
//!   worth more than a rifleman); logistics pay goes to the pilot who hauled
//!   the crate and scales with the haul's distance and how close to the front
//!   it landed; kills made by a player's deployed AI pay a fraction, less
//!   again while the owner isn't flying.
//!
//! The block is on with moderate defaults when absent. The arithmetic is kept
//! here as pure functions so it can be unit tested without a mission.

use super::{default_true, UnitTag};
use enumflags2::BitFlags;
use serde_derive::{Deserialize, Serialize};

#[derive(Debug, Clone, Serialize, Deserialize, schemars::JsonSchema)]
pub struct EconomyCfg {
    /// Off switch. With it off points are paid exactly as `PointsCfg` says,
    /// to whoever the engine credited, with no adjustment. Default true.
    #[serde(default = "default_true")]
    pub enabled: bool,
    /// The most an underdog side's earnings are raised by: 0.5 = up to 1.5x.
    /// 0 disables underdog pay. Default 0.5.
    #[serde(default = "default_underdog_max_bonus")]
    pub underdog_max_bonus: f64,
    /// How much a territory deficit counts. A side holding share `s` of the
    /// contested (non-neutral) objectives has a deficit of `2 * (0.5 - s)`,
    /// 0 at parity and 1 holding nothing. Default 1.0.
    #[serde(default = "default_one")]
    pub underdog_territory_weight: f64,
    /// How much being outnumbered counts. A side with share `p` of the
    /// players in a slot has a deficit of `2 * (0.5 - p)`. Default 1.0.
    #[serde(default = "default_one")]
    pub underdog_population_weight: f64,
    /// Population is only counted once at least this many players are in a
    /// slot across both sides, so one pilot logging off at 3am doesn't swing
    /// everyone's pay. Default 4.
    #[serde(default = "default_underdog_min_players")]
    pub underdog_min_players: u32,
    /// Also scale the smart commander's treasury income by the underdog
    /// multiplier. Default true.
    #[serde(default = "default_true")]
    pub underdog_treasury: bool,
    /// Earnings above this multiple of the side's reference balance (the
    /// median balance of the side's pilots, never less than
    /// `new_player_join`) are paid at `wealth_taper`. 0 disables. Default 4.
    #[serde(default = "default_wealth_cap_ratio")]
    pub wealth_cap_ratio: f64,
    /// The rate earnings above the wealth cap are paid at. Default 0.5.
    #[serde(default = "default_half")]
    pub wealth_taper: f64,
    /// A new pilot starts with at least this share of their side's median
    /// balance (never less than `new_player_join`). The part above
    /// `new_player_join` can't be transferred. 0 disables. Default 0.5.
    #[serde(default = "default_half")]
    pub late_joiner_fraction: f64,
    /// Share of a kill's points paid for a kill made by a player's deployed
    /// AI (SAMs, troops, action groups) while the owner is in a slot.
    /// Default 0.5.
    #[serde(default = "default_half")]
    pub owned_ai_kill_fraction: f64,
    /// The same while the owner is not in a slot (spectating or offline).
    /// Default 0.25.
    #[serde(default = "default_owned_ai_unattended_fraction")]
    pub owned_ai_unattended_fraction: f64,
    /// Kill credit is split by hits; the shooter whose hit came last (the
    /// killing blow) counts this many extra hits. Default 1.0.
    #[serde(default = "default_one")]
    pub killing_blow_weight: f64,
    /// Ground kill value by what was destroyed, as a multiplier of
    /// `points.ground_kill`. A unit takes the highest multiplier among its
    /// tags; a unit with none of them is worth 1x. The long-range SAM bonus
    /// is still added on top. Default: see `default_ground_kill_values`.
    #[serde(default = "default_ground_kill_values")]
    pub ground_kill_values: Vec<(UnitTag, f64)>,
    /// A logistics haul this long (origin objective to delivery point, km)
    /// earns the full `delivery_max_bonus`; shorter hauls earn a share of it.
    /// Default 40.
    #[serde(default = "default_delivery_ref_km")]
    pub delivery_ref_km: f64,
    /// Most a long haul raises logistics pay by: 1.0 = up to 2x. Default 1.0.
    #[serde(default = "default_one")]
    pub delivery_max_bonus: f64,
    /// A delivery to an objective within this many km of an enemy-held
    /// objective, or one that is currently threatened, counts as front line.
    /// Default 25.
    #[serde(default = "default_front_line_km")]
    pub front_line_km: f64,
    /// Extra logistics pay for a front-line delivery: 0.5 = +50%. Default 0.5.
    #[serde(default = "default_half")]
    pub front_line_bonus: f64,
    /// When the player who unpacks a crate isn't the one who loaded it, the
    /// unpacker gets this share of the logistics pay and the hauler the rest.
    /// Default 0.25.
    #[serde(default = "default_unpacker_share")]
    pub unpacker_share: f64,
    /// Share of a captured base's fund the captor keeps (capped at the fund
    /// ceiling the captor's own bases get). The rest was the loser's money
    /// and is lost with the base. Default 0.25.
    #[serde(default = "default_capture_fund_keep")]
    pub capture_fund_keep: f64,
}

impl Default for EconomyCfg {
    fn default() -> Self {
        Self {
            enabled: true,
            underdog_max_bonus: default_underdog_max_bonus(),
            underdog_territory_weight: 1.0,
            underdog_population_weight: 1.0,
            underdog_min_players: default_underdog_min_players(),
            underdog_treasury: true,
            wealth_cap_ratio: default_wealth_cap_ratio(),
            wealth_taper: 0.5,
            late_joiner_fraction: 0.5,
            owned_ai_kill_fraction: 0.5,
            owned_ai_unattended_fraction: default_owned_ai_unattended_fraction(),
            killing_blow_weight: 1.0,
            ground_kill_values: default_ground_kill_values(),
            delivery_ref_km: default_delivery_ref_km(),
            delivery_max_bonus: 1.0,
            front_line_km: default_front_line_km(),
            front_line_bonus: 0.5,
            unpacker_share: default_unpacker_share(),
            capture_fund_keep: default_capture_fund_keep(),
        }
    }
}

fn default_one() -> f64 {
    1.0
}
fn default_half() -> f64 {
    0.5
}
fn default_underdog_max_bonus() -> f64 {
    0.5
}
fn default_underdog_min_players() -> u32 {
    4
}
fn default_wealth_cap_ratio() -> f64 {
    4.0
}
fn default_owned_ai_unattended_fraction() -> f64 {
    0.25
}
fn default_delivery_ref_km() -> f64 {
    40.0
}
fn default_front_line_km() -> f64 {
    25.0
}
fn default_unpacker_share() -> f64 {
    0.25
}
fn default_capture_fund_keep() -> f64 {
    0.25
}

/// Soft targets and anything without a weapon are worth less than a plain
/// ground kill; armour, guns and air defence more; radars most, because
/// killing one blinds a whole site.
pub fn default_ground_kill_values() -> Vec<(UnitTag, f64)> {
    vec![
        (UnitTag::Infantry, 0.5),
        (UnitTag::Unarmed, 0.5),
        (UnitTag::Logistics, 0.75),
        (UnitTag::APC, 1.0),
        (UnitTag::AAA, 1.0),
        (UnitTag::Armor, 1.5),
        (UnitTag::Artillery, 1.5),
        (UnitTag::SAM, 1.5),
        (UnitTag::Boat, 2.0),
        (UnitTag::EWR, 2.0),
        (UnitTag::SearchRadar, 2.0),
        (UnitTag::TrackRadar, 2.5),
    ]
}

impl EconomyCfg {
    /// Earnings multiplier (>= 1) for a side holding `territory_share` of
    /// the contested objectives with `side_players` of `total_players` in a
    /// slot.
    pub fn underdog_multiplier(
        &self,
        territory_share: f64,
        side_players: u32,
        total_players: u32,
    ) -> f64 {
        if !self.enabled || self.underdog_max_bonus <= 0. {
            return 1.;
        }
        let deficit = |share: f64| (2. * (0.5 - share.clamp(0., 1.))).max(0.);
        let mut d = self.underdog_territory_weight.max(0.) * deficit(territory_share);
        if total_players >= self.underdog_min_players.max(1) {
            let p = side_players as f64 / total_players as f64;
            d += self.underdog_population_weight.max(0.) * deficit(p);
        }
        1. + self.underdog_max_bonus * d.min(1.)
    }

    /// The balance above which earnings taper, given the side's reference
    /// balance. `None` when the taper is off.
    pub fn wealth_cap(&self, reference: i64) -> Option<i64> {
        if !self.enabled || self.wealth_cap_ratio <= 0. {
            return None;
        }
        Some((reference.max(1) as f64 * self.wealth_cap_ratio) as i64)
    }

    /// What `amount` of earnings is worth to a pilot holding `balance`: the
    /// part that stays under `cap` is paid in full, the rest at the taper.
    pub fn taper(&self, balance: i64, amount: i64, cap: Option<i64>) -> i64 {
        let Some(cap) = cap else { return amount };
        if amount <= 0 {
            return amount;
        }
        let room = (cap - balance).max(0);
        let full = amount.min(room);
        let over = amount - full;
        full + (over as f64 * self.wealth_taper.clamp(0., 1.)).round() as i64
    }

    /// Multiplier of `ground_kill` for a unit with `tags`.
    pub fn ground_kill_multiplier(&self, tags: BitFlags<UnitTag>) -> f64 {
        if !self.enabled {
            return 1.;
        }
        self.ground_kill_values
            .iter()
            .filter(|(t, _)| tags.contains(*t))
            .map(|(_, m)| *m)
            .fold(None, |acc: Option<f64>, m| Some(acc.map_or(m, |a| a.max(m))))
            .unwrap_or(1.)
            .max(0.)
    }

    /// Logistics pay multiplier for a haul of `distance_m` that landed at the
    /// front line or not.
    pub fn delivery_multiplier(&self, distance_m: f64, front_line: bool) -> f64 {
        if !self.enabled {
            return 1.;
        }
        let haul = if self.delivery_ref_km > 0. {
            (distance_m.max(0.) / (self.delivery_ref_km * 1000.)).min(1.)
        } else {
            0.
        };
        let front = if front_line { self.front_line_bonus.max(0.) } else { 0. };
        1. + self.delivery_max_bonus.max(0.) * haul + front
    }

    /// Share of a kill's points that a player-owned AI kill pays.
    pub fn owned_ai_fraction(&self, owner_slotted: bool) -> f64 {
        if !self.enabled {
            return 1.;
        }
        let f = if owner_slotted {
            self.owned_ai_kill_fraction
        } else {
            self.owned_ai_unattended_fraction
        };
        f.clamp(0., 1.)
    }
}

/// Split `total` points by `weights` so the parts add up to exactly `total`
/// (largest remainder). Everyone with a positive weight gets at least what
/// rounding gives them; nobody gets points that weren't there to give.
pub fn split_by_weight(total: i32, weights: &[f64]) -> Vec<i32> {
    let n = weights.len();
    if n == 0 {
        return vec![];
    }
    let sum: f64 = weights.iter().map(|w| w.max(0.)).sum();
    if sum <= 0. || total <= 0 {
        // Nothing to weigh by: split evenly.
        let w = vec![1.; n];
        return if total <= 0 { vec![0; n] } else { split_by_weight(total, &w) };
    }
    let exact: Vec<f64> = weights.iter().map(|w| total as f64 * w.max(0.) / sum).collect();
    let mut parts: Vec<i32> = exact.iter().map(|e| e.floor() as i32).collect();
    let mut left = total - parts.iter().sum::<i32>();
    let mut order: Vec<usize> = (0..n).collect();
    order.sort_by(|a, b| {
        let ra = exact[*a] - exact[*a].floor();
        let rb = exact[*b] - exact[*b].floor();
        rb.partial_cmp(&ra).unwrap_or(std::cmp::Ordering::Equal).then(a.cmp(b))
    });
    for i in order {
        if left <= 0 {
            break;
        }
        parts[i] += 1;
        left -= 1;
    }
    parts
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn underdog_is_one_at_parity_and_capped() {
        let c = EconomyCfg::default();
        assert_eq!(c.underdog_multiplier(0.5, 5, 10), 1.0);
        assert_eq!(c.underdog_multiplier(0.8, 8, 10), 1.0);
        // 30% of the map: deficit 0.4 -> +20%
        assert!((c.underdog_multiplier(0.3, 5, 10) - 1.2).abs() < 1e-9);
        // nothing held and 1 v 9: capped at +50%
        assert!((c.underdog_multiplier(0.0, 1, 10) - 1.5).abs() < 1e-9);
    }

    #[test]
    fn underdog_ignores_population_on_a_quiet_server() {
        let c = EconomyCfg::default();
        assert_eq!(c.underdog_multiplier(0.5, 0, 3), 1.0);
        assert!((c.underdog_multiplier(0.5, 1, 4) - 1.25).abs() < 1e-9);
    }

    #[test]
    fn disabled_economy_changes_nothing() {
        let c = EconomyCfg { enabled: false, ..EconomyCfg::default() };
        assert_eq!(c.underdog_multiplier(0.0, 0, 10), 1.0);
        assert_eq!(c.wealth_cap(1000), None);
        assert_eq!(c.delivery_multiplier(100_000., true), 1.0);
        assert_eq!(c.owned_ai_fraction(false), 1.0);
    }

    #[test]
    fn taper_only_touches_the_part_over_the_cap() {
        let c = EconomyCfg::default();
        let cap = c.wealth_cap(1000);
        assert_eq!(cap, Some(4000));
        assert_eq!(c.taper(1000, 500, cap), 500);
        assert_eq!(c.taper(3800, 400, cap), 200 + 100);
        assert_eq!(c.taper(10_000, 400, cap), 200);
        assert_eq!(c.taper(10_000, -400, cap), -400);
    }

    #[test]
    fn ground_kill_takes_the_best_tag() {
        let c = EconomyCfg::default();
        assert_eq!(c.ground_kill_multiplier(BitFlags::from(UnitTag::Infantry)), 0.5);
        assert_eq!(c.ground_kill_multiplier(UnitTag::Infantry | UnitTag::SAM), 1.5);
        assert_eq!(
            c.ground_kill_multiplier(UnitTag::SAM | UnitTag::TrackRadar | UnitTag::LR),
            2.5
        );
        assert_eq!(c.ground_kill_multiplier(BitFlags::from(UnitTag::Driveable)), 1.0);
    }

    #[test]
    fn delivery_scales_with_haul_and_front() {
        let c = EconomyCfg::default();
        assert_eq!(c.delivery_multiplier(0., false), 1.0);
        assert!((c.delivery_multiplier(20_000., false) - 1.5).abs() < 1e-9);
        assert!((c.delivery_multiplier(400_000., true) - 2.5).abs() < 1e-9);
    }

    #[test]
    fn split_adds_up_exactly() {
        assert_eq!(split_by_weight(25, &[1., 1., 1.]), vec![9, 8, 8]);
        assert_eq!(split_by_weight(25, &[2., 1.]).iter().sum::<i32>(), 25);
        assert_eq!(split_by_weight(2, &[1., 1., 1.]), vec![1, 1, 0]);
        assert_eq!(split_by_weight(10, &[0., 0.]), vec![5, 5]);
        assert_eq!(split_by_weight(0, &[1., 1.]), vec![0, 0]);
        assert_eq!(split_by_weight(7, &[]), Vec::<i32>::new());
    }
}
