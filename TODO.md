# Carriers

- Confirm spawning aircraft on carriers works. ROADMAP's Done section notes it as verified in game in Oct 2026; no automated test covers it.

# Interface

- Vector Strike cockpit plugin (`bfcockpit/`, 3.3.0): deploy the bflib and bfdb it needs and do a final in-game pass. The F10 menu is not replaced yet. (ROADMAP: Client-Side Plugin/UI)

# Lua API

- AI orders/missions beyond `move_group`
- Actions (tanker, AWACS, CAP, ...) callable from the API
- Replicate the rest of the netidx RPC interface (queries, spawn_deployable/spawn_troop, move_group and add_points already exist in `bflib/src/api.rs`)

(ROADMAP: Public Lua API for External Tools)

# Gameplay

- Ground pathfinding that avoids known threats (ROADMAP: Enhanced Ground AI)
- Amphibious ops, shore bombardment, anti-ship tracking (ROADMAP: Naval Warfare System)
- Fuel pipelines, player emergency resupply, ammunition types (ROADMAP: Advanced Logistics)
- Read the config-only `WeatherEffectsCfg` fields; seasonal progression (ROADMAP: Weather & Time Effects)
- VIP extraction and civilian evacuation events, high-value targets (ROADMAP: Dynamic Campaign Events)
- Player strike packages, TOT and waypoint sharing (ROADMAP: Mission Planning Tools)

# Performance and code health

- Frontline: spatial index and incremental updates (ROADMAP: Frontline Calculation)
- Replace the remaining non-test `unwrap()` calls, starting with `bflib/src/db/formation.rs` (ROADMAP: Technical Debt)

See ROADMAP.md for the full status of each item, its "Audit Sept 2026" section for new ideas, and its "Done" section for what has shipped.
