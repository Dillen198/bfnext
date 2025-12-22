use crate::cfg::Deployable;
use chrono::{DateTime, Utc};
use dcso3::{atomic_id, String, Vector2};
use serde_derive::{Deserialize, Serialize};

atomic_id!(ObjectiveId);

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum ObjectiveKind {
    Airbase,
    Fob,
    Logistics,
    Farp {
        spec: Deployable,
        pad_template: String,
        #[serde(default)]
        mobile: bool,
    },
    NavalBase,
    CarrierGroup {
        #[serde(default)]
        carrier_template: String,
        #[serde(default)]
        waypoint: Option<Vector2>,
        #[serde(default)]
        parent_naval_base: Option<ObjectiveId>,
        #[serde(default)]
        repair_start_time: Option<DateTime<Utc>>,
    },
    Factory {
        #[serde(default)]
        production_rate: u32,
        #[serde(default)]
        last_production_ts: Option<DateTime<Utc>>,
    },
}

impl ObjectiveKind {
    pub fn is_airbase(&self) -> bool {
        match self {
            Self::Airbase => true,
            Self::Farp { .. } | Self::Fob | Self::Logistics | Self::NavalBase | Self::CarrierGroup { .. } | Self::Factory { .. } => false,
        }
    }

    pub fn is_farp(&self) -> bool {
        match self {
            Self::Farp { .. } => true,
            Self::Airbase | Self::Fob | Self::Logistics | Self::NavalBase | Self::CarrierGroup { .. } | Self::Factory { .. } => false,
        }
    }

    pub fn is_hub(&self) -> bool {
        match self {
            Self::Logistics => true,
            Self::Airbase | Self::Farp { .. } | Self::Fob | Self::NavalBase | Self::CarrierGroup { .. } | Self::Factory { .. } => false,
        }
    }

    pub fn is_naval_base(&self) -> bool {
        matches!(self, Self::NavalBase)
    }

    pub fn is_carrier_group(&self) -> bool {
        matches!(self, Self::CarrierGroup { .. })
    }

    pub fn is_factory(&self) -> bool {
        matches!(self, Self::Factory { .. })
    }

    pub fn is_mobile(&self) -> bool {
        match self {
            Self::Farp { mobile: true, .. } => true,
            Self::CarrierGroup { .. } => true,
            _ => false,
        }
    }

    pub fn name(&self) -> &'static str {
        match self {
            Self::Airbase => "Airbase",
            Self::Fob => "FOB",
            Self::Farp { .. } => "FARP",
            Self::Logistics => "Logistics Hub",
            Self::NavalBase => "Naval Base",
            Self::CarrierGroup { .. } => "Carrier Group",
            Self::Factory { .. } => "Factory",
        }
    }
}
