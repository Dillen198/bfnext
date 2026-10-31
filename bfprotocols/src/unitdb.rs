//! The DCS unit database the engine harvests from the hooks state's `db`
//! global (`bflib/src/unitdb.rs`), written to `Logs/unitdb.json`, served by
//! bfdb at `/api/unitdb` and read by `bftools unit-db`.
//!
//! One definition for all of them. Every field is optional on read and
//! unknown keys are ignored, so a reader keeps working on snapshots written
//! before or after a field was added.

use serde_derive::{Deserialize, Serialize};
use std::collections::BTreeMap;

/// What the harvest records for one DCS unit type. A missing field means
/// "DCS doesn't say", never zero: a supply truck has no `ThreatRange`, a
/// launcher no `DetectionRange`.
#[derive(Debug, Clone, Default, PartialEq, Serialize, Deserialize)]
pub struct UnitInfo {
    /// DCS type name, i.e. what `Unit.getTypeName()` returns.
    #[serde(default)]
    pub typ: String,
    #[serde(default)]
    pub display_name: Option<String>,
    /// `db.Units.<Category>` this came from, e.g. "Cars/Car".
    #[serde(default)]
    pub category: Option<String>,
    /// The named DCS attributes ("Artillery", "SAM SR", "Indirect fire", ...).
    #[serde(default)]
    pub attributes: Vec<String>,
    /// Maximum engagement range in metres. Guns and launchers only.
    #[serde(default)]
    pub threat_range_m: Option<f64>,
    /// Minimum engagement range in metres.
    #[serde(default)]
    pub threat_range_min_m: Option<f64>,
    /// Radar detection range in metres, for units that have a radar.
    #[serde(default)]
    pub detection_range_m: Option<f64>,
    #[serde(default)]
    pub max_target_detection_range_m: Option<f64>,
    #[serde(default)]
    pub max_fuel_kg: Option<f64>,
}

impl UnitInfo {
    pub fn has_attribute(&self, attr: &str) -> bool {
        self.attributes.iter().any(|a| a == attr)
    }

    pub fn is_indirect_fire(&self) -> bool {
        self.has_attribute("Indirect fire")
    }
}

/// A whole harvest, as `Logs/unitdb.json` and `/api/unitdb` carry it.
#[derive(Debug, Clone, Default, PartialEq, Serialize, Deserialize)]
pub struct UnitDbSnapshot {
    #[serde(default)]
    pub dcs_version: Option<String>,
    #[serde(default)]
    pub harvested_at: Option<String>,
    #[serde(default)]
    pub by_type: BTreeMap<String, UnitInfo>,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn reads_snapshots_with_missing_and_unknown_fields() {
        let snap: UnitDbSnapshot = serde_json::from_str(
            r#"{"dcs_version":"2.9","by_type":{"M-109":{"typ":"M-109",
                "attributes":["Artillery","Indirect fire"],"threat_range_m":22000,
                "some_future_field":1}}}"#,
        )
        .unwrap();
        let u = &snap.by_type["M-109"];
        assert!(u.is_indirect_fire());
        assert_eq!(u.threat_range_m, Some(22000.));
        assert_eq!(u.detection_range_m, None);
        assert_eq!(snap.harvested_at, None);
    }
}
