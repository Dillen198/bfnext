//! Flight plans: routes pilots build on the dashboard's Planner and share
//! with their own coalition. The browser turns a plan into a DCS-DTC route
//! import or a native DCS cartridge; this module only stores and checks it.
//!
//! Stored as JSON in a plain sled tree, not yats: yats values are positional
//! bincode, so the plan gaining a field would make every row an older build
//! wrote undecodable (see the note on `news` in db.rs). Keyed
//! `<instance>\0<id>` rather than by round: a route over the map is still good
//! after a campaign reset, so the reset leaves these alone.

use anyhow::{bail, Result};
use chrono::{DateTime, Utc};
use serde::{Deserialize, Serialize};
use uuid::Uuid;
use yats::{Db, RawTree as Tree};

/// The F/A-18C holds 59 waypoints and DCS-DTC's F-16 page 699; 99 is plenty
/// for a planned route and keeps one plan well under the request limit.
pub(crate) const MAX_WAYPOINTS: usize = 99;
/// Presets per radio. Both native cartridges carry 20.
pub(crate) const MAX_CHANNELS: usize = 20;
pub(crate) const MAX_PER_AUTHOR: usize = 40;
pub(crate) const MAX_PER_SIDE: usize = 400;

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub(crate) struct Waypoint {
    pub(crate) name: String,
    pub(crate) lat: f64,
    pub(crate) lon: f64,
    /// Planned altitude, feet MSL: the route card and the leg plan.
    pub(crate) alt_ft: f64,
    /// Steerpoint elevation, feet MSL: what the jet's nav system is given.
    /// Matters for a target point, where it should be the ground.
    #[serde(default)]
    pub(crate) elev_ft: f64,
    #[serde(default)]
    pub(crate) target: bool,
    #[serde(default)]
    pub(crate) note: String,
    /// Offset aimpoints from this point (the F-16 takes two, the Hornet one).
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub(crate) oa: Vec<Offset>,
    /// This steerpoint is the VIP: offsets to the target and the pull-up point.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub(crate) vip: Option<OffsetPair>,
    /// This steerpoint is the target: offsets to the VRP and the pull-up point.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub(crate) vrp: Option<OffsetPair>,
}

/// A point given relative to a waypoint.
#[derive(Debug, Clone, Copy, Serialize, Deserialize, PartialEq)]
pub(crate) struct Offset {
    pub(crate) range_ft: f64,
    /// Degrees true from the waypoint.
    pub(crate) bearing_deg: f64,
    /// Feet MSL.
    pub(crate) elev_ft: f64,
}

#[derive(Debug, Clone, Copy, Serialize, Deserialize, PartialEq)]
pub(crate) struct OffsetPair {
    pub(crate) point: Offset,
    pub(crate) pup: Offset,
}

fn offset(o: Offset, what: &str) -> Result<Offset> {
    let ok = |v: f64, lo: f64, hi: f64| v.is_finite() && (lo..=hi).contains(&v);
    // 80 NM is the longest VIP/VRP leg the F-16 takes.
    if !ok(o.range_ft, 0.0, 486_090.0)
        || !ok(o.bearing_deg, 0.0, 360.0)
        || !ok(o.elev_ft, -1500.0, 80_000.0)
    {
        bail!("{what}: range, bearing or elevation out of range")
    }
    Ok(Offset {
        range_ft: o.range_ft.round(),
        bearing_deg: (o.bearing_deg * 10.0).round() / 10.0,
        elev_ft: o.elev_ft.round(),
    })
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub(crate) struct Channel {
    pub(crate) preset: u8,
    pub(crate) freq_mhz: f64,
    #[serde(default)]
    pub(crate) label: String,
    /// "AM" or "FM".
    #[serde(default = "am")]
    pub(crate) modulation: String,
}

fn am() -> String {
    "AM".into()
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub(crate) struct Plan {
    pub(crate) name: String,
    /// DCS unit type, e.g. "F-16C_50". Picks the export formats offered.
    pub(crate) aircraft: String,
    #[serde(default)]
    pub(crate) callsign: String,
    /// Planning ground speed for the leg times.
    pub(crate) speed_kts: f64,
    /// Mission time at the first waypoint, "HH:MM" or "HH:MM:SS".
    #[serde(default)]
    pub(crate) start_time: Option<String>,
    pub(crate) waypoints: Vec<Waypoint>,
    #[serde(default)]
    pub(crate) comm1: Vec<Channel>,
    #[serde(default)]
    pub(crate) comm2: Vec<Channel>,
    /// Write the presets into the native cartridge. Off, the jet keeps the
    /// mission's own radio presets.
    #[serde(default)]
    pub(crate) include_comms: bool,
    #[serde(default)]
    pub(crate) notes: String,
    /// Avionics settings beyond the route, per export target ("native:F-16C_50",
    /// "dcsdtc:F16C", ...): only what the author changed from that target's
    /// defaults. Opaque here -- the browser owns the per-aircraft layouts --
    /// so it is held to a size and depth, not a schema.
    #[serde(default)]
    pub(crate) systems: serde_json::Map<String, serde_json::Value>,
}

/// Serialised size cap for `Plan::systems`. A full F-16 CMS + HARM + MFD
/// override set is a few KB; this leaves room for an Apache's points.
pub(crate) const MAX_SYSTEMS_BYTES: usize = 48 * 1024;
const MAX_SYSTEMS_DEPTH: usize = 12;

fn depth(v: &serde_json::Value) -> usize {
    match v {
        serde_json::Value::Array(a) => 1 + a.iter().map(depth).max().unwrap_or(0),
        serde_json::Value::Object(o) => 1 + o.values().map(depth).max().unwrap_or(0),
        _ => 0,
    }
}

fn check_systems(s: &serde_json::Map<String, serde_json::Value>) -> Result<()> {
    if s.len() > 32 {
        bail!("too many avionics targets in one plan")
    }
    for k in s.keys() {
        if k.len() > 48
            || !k.chars().all(|c| c.is_ascii_alphanumeric() || "-_.: ".contains(c))
        {
            bail!("bad avionics target {k:?}")
        }
    }
    let v = serde_json::Value::Object(s.clone());
    if serde_json::to_vec(&v)?.len() > MAX_SYSTEMS_BYTES {
        bail!("avionics settings are over {} KB", MAX_SYSTEMS_BYTES / 1024)
    }
    if depth(&v) > MAX_SYSTEMS_DEPTH {
        bail!("avionics settings are nested too deep")
    }
    Ok(())
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub(crate) struct Stored {
    pub(crate) id: Uuid,
    /// "Blue" or "Red", fixed when the plan is made.
    pub(crate) side: String,
    /// The author's session discord id: who may edit or delete it.
    pub(crate) by: String,
    pub(crate) by_name: String,
    pub(crate) created: DateTime<Utc>,
    pub(crate) updated: DateTime<Utc>,
    pub(crate) plan: Plan,
}

fn clean(s: &str, max: usize, what: &str) -> Result<String> {
    let s = s.trim();
    if s.chars().count() > max {
        bail!("{what} is limited to {max} characters")
    }
    if s.chars().any(|c| c.is_control() && c != '\n') {
        bail!("{what} has control characters in it")
    }
    Ok(s.to_string())
}

fn valid_time(t: &str) -> bool {
    let parts: Vec<_> = t.split(':').collect();
    let n = |s: &str, max: u32| s.len() == 2 && s.parse::<u32>().is_ok_and(|v| v <= max);
    match parts.as_slice() {
        [h, m] => n(h, 23) && n(m, 59),
        [h, m, s] => n(h, 23) && n(m, 59) && n(s, 59),
        _ => false,
    }
}

fn channels(list: Vec<Channel>, radio: &str) -> Result<Vec<Channel>> {
    if list.len() > MAX_CHANNELS {
        bail!("{radio} has more than {MAX_CHANNELS} presets")
    }
    let mut seen = [false; MAX_CHANNELS + 1];
    list.into_iter()
        .map(|c| {
            let p = c.preset as usize;
            if !(1..=MAX_CHANNELS).contains(&p) || std::mem::replace(&mut seen[p], true) {
                bail!("{radio} preset {p} is out of range or repeated")
            }
            if !c.freq_mhz.is_finite() || !(30.0..=400.0).contains(&c.freq_mhz) {
                bail!("{radio} preset {p}: {} MHz is not a radio frequency", c.freq_mhz)
            }
            let modulation = match c.modulation.to_ascii_uppercase().as_str() {
                "AM" => "AM",
                "FM" => "FM",
                _ => bail!("{radio} preset {p}: modulation is AM or FM"),
            };
            Ok(Channel {
                preset: c.preset,
                freq_mhz: (c.freq_mhz * 1000.0).round() / 1000.0,
                label: clean(&c.label, 24, "a preset label")?,
                modulation: modulation.into(),
            })
        })
        .collect()
}

/// Check and normalise a plan from the browser. Everything stored here is
/// drawn into other pilots' dashboards and written into their cartridges, so
/// only known fields of sane size get through.
pub(crate) fn validate(p: Plan) -> Result<Plan> {
    let name = clean(&p.name, 60, "the plan name")?;
    if name.is_empty() {
        bail!("the plan needs a name")
    }
    let aircraft = p.aircraft.trim().to_string();
    if aircraft.is_empty()
        || aircraft.len() > 32
        || !aircraft.chars().all(|c| c.is_ascii_alphanumeric() || "-_. ".contains(c))
    {
        bail!("unknown aircraft type {aircraft:?}")
    }
    if !p.speed_kts.is_finite() || !(40.0..=1500.0).contains(&p.speed_kts) {
        bail!("planning speed must be 40 to 1500 kts")
    }
    let start_time = match p.start_time.as_deref().map(str::trim) {
        None | Some("") => None,
        Some(t) if valid_time(t) => Some(t.to_string()),
        Some(t) => bail!("start time {t:?} is not HH:MM or HH:MM:SS"),
    };
    if p.waypoints.is_empty() || p.waypoints.len() > MAX_WAYPOINTS {
        bail!("a plan has 1 to {MAX_WAYPOINTS} waypoints")
    }
    let waypoints = p
        .waypoints
        .into_iter()
        .enumerate()
        .map(|(i, w)| {
            let n = i + 1;
            let ok = |v: f64, lo: f64, hi: f64| v.is_finite() && (lo..=hi).contains(&v);
            if !ok(w.lat, -90.0, 90.0) || !ok(w.lon, -180.0, 180.0) {
                bail!("waypoint {n} is not a position")
            }
            if !ok(w.alt_ft, -1500.0, 99_999.0) || !ok(w.elev_ft, -1500.0, 30_000.0) {
                bail!("waypoint {n}: altitude or elevation out of range")
            }
            Ok(Waypoint {
                name: clean(&w.name, 24, "a waypoint name")?,
                lat: w.lat,
                lon: w.lon,
                alt_ft: w.alt_ft.round(),
                elev_ft: w.elev_ft.round(),
                target: w.target,
                note: clean(&w.note, 200, "a waypoint note")?,
                oa: {
                    if w.oa.len() > 2 {
                        bail!("waypoint {n}: at most two offset aimpoints")
                    }
                    w.oa.into_iter()
                        .map(|o| offset(o, "an offset aimpoint"))
                        .collect::<Result<_>>()?
                },
                vip: w
                    .vip
                    .map(|p| -> Result<_> {
                        Ok(OffsetPair {
                            point: offset(p.point, "the VIP target")?,
                            pup: offset(p.pup, "the VIP pull-up")?,
                        })
                    })
                    .transpose()?,
                vrp: w
                    .vrp
                    .map(|p| -> Result<_> {
                        Ok(OffsetPair {
                            point: offset(p.point, "the VRP")?,
                            pup: offset(p.pup, "the VRP pull-up")?,
                        })
                    })
                    .transpose()?,
            })
        })
        .collect::<Result<Vec<_>>>()?;
    check_systems(&p.systems)?;
    Ok(Plan {
        name,
        aircraft,
        callsign: clean(&p.callsign, 24, "the callsign")?,
        speed_kts: p.speed_kts.round(),
        start_time,
        waypoints,
        comm1: channels(p.comm1, "COMM1")?,
        comm2: channels(p.comm2, "COMM2")?,
        include_comms: p.include_comms,
        notes: clean(&p.notes, 2000, "the notes")?,
        systems: p.systems,
    })
}

#[derive(Clone)]
pub(crate) struct FlightPlans {
    plans: Tree,
}

fn key(instance: &str, id: &Uuid) -> Vec<u8> {
    let mut k = instance.as_bytes().to_vec();
    k.push(0);
    k.extend_from_slice(id.as_bytes());
    k
}

impl FlightPlans {
    pub(crate) fn new(db: &Db) -> Result<Self> {
        Ok(Self { plans: db.open_tree("flight_plans")? })
    }

    /// Every plan on `instance`, or only `side`'s, newest edit first. A row
    /// this build cannot read is skipped rather than failing the list.
    pub(crate) fn list(&self, instance: &str, side: Option<&str>) -> Result<Vec<Stored>> {
        let mut prefix = instance.as_bytes().to_vec();
        prefix.push(0);
        let mut out = Vec::new();
        for r in self.plans.scan_prefix(&prefix) {
            let (_, v) = r?;
            let Ok(p) = serde_json::from_slice::<Stored>(&v) else { continue };
            if side.is_none_or(|s| p.side == s) {
                out.push(p);
            }
        }
        out.sort_by(|a, b| b.updated.cmp(&a.updated));
        Ok(out)
    }

    pub(crate) fn get(&self, instance: &str, id: &Uuid) -> Result<Option<Stored>> {
        match self.plans.get(key(instance, id))? {
            Some(v) => Ok(Some(serde_json::from_slice(&v)?)),
            None => Ok(None),
        }
    }

    pub(crate) fn put(&self, instance: &str, p: &Stored) -> Result<()> {
        self.plans.insert(key(instance, &p.id), serde_json::to_vec(p)?)?;
        Ok(())
    }

    pub(crate) fn delete(&self, instance: &str, id: &Uuid) -> Result<()> {
        self.plans.remove(key(instance, id))?;
        Ok(())
    }

    /// Refuse a new plan past the per-author or per-side cap.
    pub(crate) fn check_quota(&self, instance: &str, side: &str, by: &str) -> Result<()> {
        let all = self.list(instance, Some(side))?;
        if all.len() >= MAX_PER_SIDE {
            bail!("your coalition has {MAX_PER_SIDE} saved plans; delete some old ones first")
        }
        if all.iter().filter(|p| p.by == by).count() >= MAX_PER_AUTHOR {
            bail!("you have {MAX_PER_AUTHOR} saved plans; delete some old ones first")
        }
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn wpt(name: &str) -> Waypoint {
        Waypoint {
            name: name.into(),
            lat: 41.6,
            lon: 41.6,
            alt_ft: 15000.4,
            elev_ft: 0.0,
            target: false,
            note: String::new(),
            oa: vec![],
            vip: None,
            vrp: None,
        }
    }

    #[test]
    fn validate_offsets() {
        let o = Offset { range_ft: 3000.4, bearing_deg: 90.04, elev_ft: 120.0 };
        let mut p = plan();
        p.waypoints[1].oa = vec![o, o];
        p.waypoints[1].vip = Some(OffsetPair { point: o, pup: o });
        let v = validate(p).unwrap();
        assert_eq!(
            v.waypoints[1].oa[0],
            Offset { range_ft: 3000.0, bearing_deg: 90.0, elev_ft: 120.0 }
        );
        assert!(v.waypoints[1].vip.is_some());
        let mut bad = plan();
        bad.waypoints[0].oa = vec![o, o, o];
        assert!(validate(bad).is_err());
        let mut bad = plan();
        bad.waypoints[0].vrp =
            Some(OffsetPair { point: Offset { bearing_deg: 400.0, ..o }, pup: o });
        assert!(validate(bad).is_err());
    }

    fn plan() -> Plan {
        Plan {
            name: "  CAS north ".into(),
            aircraft: "F-16C_50".into(),
            callsign: "Enfield 1".into(),
            speed_kts: 420.0,
            start_time: Some("08:30".into()),
            waypoints: vec![wpt("HOME"), wpt("IP")],
            comm1: vec![Channel {
                preset: 1,
                freq_mhz: 251.0004,
                label: "AWACS".into(),
                modulation: "am".into(),
            }],
            comm2: vec![],
            include_comms: true,
            notes: String::new(),
            systems: serde_json::Map::new(),
        }
    }

    #[test]
    fn validate_normalises() {
        let p = validate(plan()).unwrap();
        assert_eq!(p.name, "CAS north");
        assert_eq!(p.waypoints[0].alt_ft, 15000.0);
        assert_eq!(p.comm1[0].modulation, "AM");
        assert_eq!(p.comm1[0].freq_mhz, 251.0);
    }

    #[test]
    fn validate_rejects_bad_input() {
        let bad = |f: fn(&mut Plan)| {
            let mut p = plan();
            f(&mut p);
            validate(p).is_err()
        };
        assert!(bad(|p| p.name = "   ".into()));
        assert!(bad(|p| p.waypoints.clear()));
        assert!(bad(|p| p.waypoints[0].lat = f64::NAN));
        assert!(bad(|p| p.waypoints[1].lon = 200.0));
        assert!(bad(|p| p.aircraft = "<script>".into()));
        assert!(bad(|p| p.start_time = Some("25:00".into())));
        assert!(bad(|p| p.comm1[0].freq_mhz = 1000.0));
        assert!(bad(|p| p.comm1.push(p.comm1[0].clone())));
        assert!(bad(|p| p.waypoints = (0..=MAX_WAYPOINTS).map(|_| wpt("X")).collect()));
        assert!(bad(|p| p.notes = "\u{7}".into()));
        assert!(bad(|p| {
            p.systems.insert(
                "native:F-16C_50".into(),
                serde_json::json!({ "x": "a".repeat(MAX_SYSTEMS_BYTES) }),
            );
        }));
        assert!(bad(|p| {
            let mut v = serde_json::json!(1);
            for _ in 0..20 {
                v = serde_json::json!([v]);
            }
            p.systems.insert("dcsdtc:F16C".into(), v);
        }));
        assert!(bad(|p| {
            p.systems.insert("<bad>".into(), serde_json::json!({}));
        }));
        let mut ok = plan();
        ok.systems.insert(
            "native:F-16C_50".into(),
            serde_json::json!({ "MPD": { "CMDS": { "x": 1 } } }),
        );
        assert_eq!(validate(ok).unwrap().systems.len(), 1);
    }

    #[test]
    fn plans_are_per_instance_and_side() {
        let db = yats::Db::temporary_sled().unwrap();
        let fp = FlightPlans::new(&db).unwrap();
        let now = Utc::now();
        let mk = |side: &str, by: &str| Stored {
            id: Uuid::new_v4(),
            side: side.into(),
            by: by.into(),
            by_name: by.into(),
            created: now,
            updated: now,
            plan: validate(plan()).unwrap(),
        };
        let a = mk("Blue", "1");
        let b = mk("Red", "2");
        fp.put("main", &a).unwrap();
        fp.put("main", &b).unwrap();
        fp.put("other", &mk("Blue", "1")).unwrap();
        assert_eq!(fp.list("main", None).unwrap().len(), 2);
        let blue = fp.list("main", Some("Blue")).unwrap();
        assert_eq!(blue.len(), 1);
        assert_eq!(blue[0].id, a.id);
        assert_eq!(fp.get("main", &b.id).unwrap().unwrap().plan, b.plan);
        assert!(fp.get("other", &b.id).unwrap().is_none());
        fp.delete("main", &a.id).unwrap();
        assert!(fp.list("main", Some("Blue")).unwrap().is_empty());
        assert!(fp.check_quota("main", "Blue", "1").is_ok());
    }
}
