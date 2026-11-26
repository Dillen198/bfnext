//! Real orders of battle for AI aircraft (`Cfg::ai_air_stock`, Red Storm
//! Rising ENGINE.md 3.1).
//!
//! With it on, an AI flight -- an HQ package, an `air_life` package, or any
//! air action a player or commander buys (fighters, strikes, SEAD, AWACS,
//! tankers, bombers, transports, drones) -- takes its airframes out of the
//! warehouse of the field it launches from, the same stock players fly
//! from. A field without enough of the type can't launch it, so the HQ
//! sends what its bases still hold, and a side whose Tu-22M3s are gone flies
//! no more Backfire raids until replacements arrive by production, supply
//! transfer, convoy, ferry flight or a story event (`ReinforceAirframes`).
//!
//! What happens to the airframes afterwards:
//!
//! - shot down, crashed or ejected: gone;
//! - landed at a friendly base (`return_on_land`): back in that base's stock
//!   once every live aircraft of the flight is down (the engine then removes
//!   the flight);
//! - removed by the engine at the end of its life (package lifetime, the
//!   return-to-base cull, a restart dropping it, an admin delete): every
//!   aircraft still alive goes back to the field it launched from while its
//!   side holds it, else to the side's nearest field that can operate it,
//!   else it is lost.
//!
//! Players and the AI share the stock, first come first served: nothing is
//! reserved, so a player can take the last airframe a flight was about to
//! use and the flight then launches from another field or not at all.
//! `unlimited_aircraft` fields and `warehouse.exempt_airframes` types are
//! never drawn from or counted, exactly as for players. The block absent
//! (or `enabled: false`, or no `warehouse`) is the old behaviour: AI
//! aircraft cost treasury points only.

use super::default_true;
use serde_derive::{Deserialize, Serialize};

#[derive(Debug, Clone, Serialize, Deserialize, schemars::JsonSchema)]
pub struct AiAirStockCfg {
    /// Off switch that keeps the block around. Default true.
    #[serde(default = "default_true")]
    pub enabled: bool,
    /// An AI flight that lands at a friendly base puts its airframes back
    /// into that base's stock. With false, landing does nothing and the
    /// airframes only go back when the engine removes the flight (to the
    /// field it launched from). Default true.
    #[serde(default = "default_true")]
    pub return_on_land: bool,
    /// Action names (as in `actions`) or .miz template names whose flights
    /// never draw on stock, e.g. a campaign's tanker and AWACS.
    #[serde(default)]
    pub exempt_actions: Vec<String>,
}

impl Default for AiAirStockCfg {
    fn default() -> Self {
        Self { enabled: true, return_on_land: true, exempt_actions: vec![] }
    }
}

impl AiAirStockCfg {
    /// Is a flight from this action (or template) free of the stock?
    pub fn exempt(&self, action: &str, template: &str) -> bool {
        self.exempt_actions.iter().any(|n| n.as_str() == action || n.as_str() == template)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn defaults_and_exemptions() {
        let c: AiAirStockCfg = serde_json::from_str("{}").unwrap();
        assert!(c.enabled && c.return_on_land && c.exempt_actions.is_empty());
        let c: AiAirStockCfg = serde_json::from_str(
            r#"{"enabled": true, "return_on_land": false, "exempt_actions": ["Tanker", "BAWACS"]}"#,
        )
        .unwrap();
        assert!(!c.return_on_land);
        assert!(c.exempt("Tanker", "BKC135"));
        assert!(c.exempt("HQ CAP 3", "BAWACS"));
        assert!(!c.exempt("Fighters", "BF15"));
    }

    /// The block in `miz/SAMPLE_CFG.json` reads.
    #[test]
    fn the_sample_parses() {
        let raw = include_str!("../../../miz/SAMPLE_CFG.json");
        let v: serde_json::Value =
            serde_json::from_str(raw.trim_start_matches('\u{feff}')).unwrap();
        let c: AiAirStockCfg = serde_json::from_value(v["ai_air_stock"].clone()).unwrap();
        assert!(c.enabled && c.return_on_land);
        assert!(!c.exempt_actions.is_empty());
    }
}
