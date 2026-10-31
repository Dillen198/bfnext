# Fowl Engine Roadmap

This document tracks open features, improvements and technical debt for the BFNEXT project.

- **Open work** is everything above the "Done" section. A **Partly done** item starts with a line that says what has shipped and what remains.
- **Done** items are listed at the bottom, before "Contributing", with where they live in the code.
- "(status unverified)" means the item has not been checked against the current code.
- Paths, counts and line numbers are as of the last review.

TODO.md is the short version of this list.

*Last reviewed: 2026-10-25*

---

## Feature Opportunities

### High-Value New Features

---

#### 1. Public Lua API for External Tools

- **Priority**: High
- **Status**: Partly done. Done: `bflib/src/api.rs` registers a `vector_strike` Lua global with 10 functions (`get_objectives`, `get_objective`, `get_players`, `get_player`, `get_groups`, `get_campaign_state`, `spawn_deployable`, `spawn_troop`, `move_group` and `add_points`) and an `on_event` callback hook. `bfprotocols/src/api.rs` holds the shared types, and the netidx RPCs in `bflib/src/bg/rpcs.rs` feed bfdb and the bot. **Remaining:** AI orders beyond `move_group` (missions, tasking), triggering actions (tanker, AWACS, CAP, etc.) from the API, and the rest of the netidx RPC interface.
- **Description**: Enable external tools (Discord bots, web interfaces) to interact with the campaign

**Capabilities needed**:
- Spawn deployables
- Issue AI orders
- Control actions
- Extract campaign info (objectives, players, logistics)

**Use cases**: Discord bot integration, external mission planning tools

**Implementation Details**:

The API should expose these endpoints via the existing netidx RPC system in `bflib/src/bg/rpcs.rs`:

```rust
// Suggested API structure for bfprotocols/src/api.rs

/// Get current campaign state
pub struct GetCampaignState;
pub struct CampaignStateResponse {
    pub objectives: Vec<ObjectiveInfo>,
    pub players: Vec<PlayerInfo>,
    pub logistics: LogisticsInfo,
    pub frontline: Option<FrontlineData>,
}

/// Spawn a deployable at location
pub struct SpawnDeployable {
    pub side: Side,
    pub deployable_name: String,
    pub position: Vector2,
    pub player_ucid: Option<Ucid>,  // For point deduction
}

/// Issue move order to AI group
pub struct IssueMoveOrder {
    pub group_id: GroupId,
    pub destination: Vector2,
    pub formation: Option<Formation>,
}

/// Trigger an action (tanker, AWACS, etc.)
pub struct TriggerAction {
    pub side: Side,
    pub action_name: String,
    pub target: Option<Vector2>,
    pub player_ucid: Option<Ucid>,
}
```

**Files to modify**:
- `bfprotocols/src/api.rs` - Define request/response types
- `bfprotocols/src/lib.rs` - Export the api module
- `bflib/src/bg/rpcs.rs` - Add RPC handlers (~line 150-300)
- `bflib/src/bg/mod.rs` - Register new RPC endpoints

**Step-by-step implementation**:
1. Define all API types in `bfprotocols/src/api.rs`
2. Add serialization derives (Serialize, Deserialize, Pack, Unpack)
3. Create handler functions in `rpcs.rs` that call into `Db` methods
4. Register handlers in the background task initialization
5. Add authentication/authorization checks (admin-only vs public endpoints)
6. Document API in user guide

---

#### 2. Enhanced Ground AI

- **Priority**: Medium
- **Status**: Partly done. Done:
  - Ground formations raised from base garrisons, which move, fight and go home (`bflib/src/db/formation.rs`, `bflib/src/db/battle.rs`).
  - An AI ground commander per side. It pushes along the offensive axis, counter-attacks threatened bases and pulls battered formations back (`bflib/src/groundwar/ai.rs`). Players can order any formation from F10 > Ground Forces.
  - Garrisons that drive out to meet an intruder (`bflib/src/db/garrison.rs`), and the Reinforce action, a road convoy of replacement vehicles (`bflib/src/db/reinforce.rs`).
  - Interdiction checks on ground routes (`route_interdicted` in `bflib/src/db/logistics.rs`), road routing for convoys, and threat-aware routes for AI helo missions (`bflib/src/db/helo_route.rs`).

  **Remaining:** A* ground pathfinding that avoids known threats, as sketched below. Ground routes follow the road network but do not route around air defence or the front.
- **Description**: Improve ground unit behavior and logistics

**Features still open**:
- Convoy pathfinding that avoids threats (routes follow the roads today)
- Dynamic route selection based on threats

**Implementation Details**:

**A* Pathfinding System**:
```rust
// New file: bflib/src/db/pathfinding.rs

use std::collections::{BinaryHeap, HashMap};
use nalgebra::Vector2;

pub struct PathGrid {
    cells: Vec<Vec<CellType>>,
    cell_size: f64,  // meters per cell
    origin: Vector2<f64>,
}

#[derive(Clone, Copy)]
pub enum CellType {
    Passable { cost: f32 },  // Roads = 1.0, Terrain = 2.0
    Impassable,               // Water, mountains
    Threatened { cost: f32 }, // Enemy SAM range, frontline
}

impl PathGrid {
    /// Build grid from terrain and threat data
    pub fn build(
        land: &Land,
        threats: &[(Vector2<f64>, f64)],  // (position, radius)
        bounds: (Vector2<f64>, Vector2<f64>),
        cell_size: f64,
    ) -> Self { ... }

    /// A* pathfinding
    pub fn find_path(
        &self,
        start: Vector2<f64>,
        goal: Vector2<f64>,
    ) -> Option<Vec<Vector2<f64>>> {
        // Standard A* with terrain cost heuristic
        // Returns waypoints in world coordinates
    }

    /// Update threat cells when objectives change hands
    pub fn update_threats(&mut self, threats: &[(Vector2<f64>, f64)]) { ... }
}
```

**Integration points**:
- `logistics.rs:spawn_convoy()` - Use pathfinding for waypoints
- `objective.rs` - Rebuild threat grid on capture
- `db/mod.rs` - Store PathGrid in Db struct

**Threat avoidance**:
```rust
// In logistics.rs, modify convoy spawning:
fn spawn_convoy(db: &mut Db, from: ObjectiveId, to: ObjectiveId) -> Result<()> {
    let from_pos = db.objective(&from)?.pos;
    let to_pos = db.objective(&to)?.pos;

    // NEW: Use pathfinding instead of direct line
    let path = db.path_grid.find_path(from_pos, to_pos)
        .ok_or_else(|| anyhow!("No safe path found"))?;

    // Convert path to DCS waypoints
    let waypoints: Vec<Waypoint> = path.iter()
        .map(|pos| Waypoint {
            pos: *pos,
            action: WaypointAction::OnRoad,
            speed: convoy_cfg.speed,
            ..Default::default()
        })
        .collect();

    // ... spawn with new waypoints
}
```

**Files to create/modify**:
- `bflib/src/db/pathfinding.rs` - New pathfinding module
- `bflib/src/db/mod.rs` - Add PathGrid to Db, export module
- `bflib/src/db/logistics.rs` - Use pathfinding in convoy spawning
- `bflib/src/db/objective.rs` - Trigger threat grid updates

**Performance considerations**:
- Pre-compute path grid on mission start
- Cache frequently-used routes
- Only rebuild affected cells on objective capture
- Use hierarchical pathfinding for long distances

---

#### 3. Naval Warfare System

- **Priority**: Medium
- **Status**: Partly done. Done: carrier groups (`CarrierCfg`) that move with a `CarrierWaypoint` action; auto-generated TACAN, ICLS, ACLS and Link-4 for carriers and amphibious flat-tops (`bflib/src/navaids.rs`); sea logistics routes (`spawn_sea_logistics_route` in `bflib/src/db/logistics.rs`); a naval cruise missile strike action (`ActionKind::NavalCruiseMissileStrike`), which the AI HQ also uses (`bflib/src/hq/planner.rs`); and sea-drone raids on enemy ships (`bflib/src/modern_war/boats.rs`). Player aircraft spawn on carriers (see Done). **Remaining:** amphibious operations, shore bombardment beyond the cruise missile strike, and anti-ship missions with proper tracking.
- **Description**: Expand naval operations

**Features still open**:
- Naval resupply of carrier groups (sea logistics routes exist; carrier resupply status unverified)
- Amphibious operations
- Shore bombardment missions
- Anti-ship missions with proper tracking

**Implementation Details**:

**Naval Logistics**:
```rust
// New objective kind in bfprotocols/src/db/objective.rs
pub enum ObjectiveKind {
    Airbase,
    Fob,
    Farp,
    Port,
    CarrierGroup,  // NEW: Mobile objective
    SupplyShip,    // NEW: Can resupply carrier groups
}

// In logistics.rs, add naval supply routes:
fn tick_naval_logistics(db: &mut Db) -> Result<()> {
    for carrier in db.persisted.objectives.values()
        .filter(|o| o.kind == ObjectiveKind::CarrierGroup)
    {
        // Find nearest friendly port
        let nearest_port = find_nearest_objective(db, carrier.pos, |o| {
            o.kind == ObjectiveKind::Port && o.owner == carrier.owner
        });

        if let Some(port) = nearest_port {
            // Check if supply ship route exists, create if not
            // Transfer supplies from port to carrier
        }
    }
    Ok(())
}
```

**Files to modify**:
- `bfprotocols/src/db/objective.rs` - New objective kinds
- `bflib/src/db/logistics.rs` - Naval supply routes
- `bflib/src/db/objective.rs` - Carrier group movement
- `bfprotocols/src/cfg/mod.rs` - Naval config options

---

#### 4. Client-Side Plugin/UI

- **Priority**: Medium
- **Status**: Partly done. Done:
  - `bfcockpit/`: the Vector Strike cockpit plugin (3.3.0), a client-side DCS hook drawn with DCS's own dxgui widgets, plus a Win32 installer. It calls the bfdb API over HTTPS through `bfcockpit_net.dll`, mirrors the F10 menu, and has CARGO, STATUS, EWR and LOG tabs and a radial wheel (head, mouse or keys; VR). Its settings are on an Options -> Special page. The installer is the GitHub release `cockpit-v<version>`, and from 3.2 the plugin updates itself.
  - F10 answers reach the plugin through the message queue feed (`/api/cockpit/messages` in `bfdb/src/main.rs`).
  - A fog-of-war tactical map fed by `/ws/tacmap` (`bfprotocols/src/tacmap.rs`).
  - The bfweb pages `CockpitPage`, `KneeboardTab` and `BriefingPage`, and the web dashboard, which covers the territory map, logistics, points and briefing.

  **Remaining:** deploy the bflib and bfdb the plugin needs, and do a final in-game pass. The F10 menu has not been replaced. JTAC target visualization in the plugin (status unverified).
- **Description**: Dedicated UI beyond F10 radio menus. The first design options (Scratchpad globals, an external overlay app, a web dashboard) are in git history; the cockpit plugin and bfweb took their place.

---

#### 5. Weather & Time Effects

- **Priority**: Low
- **Status**: Partly done. Done: real-world weather and time from CheckWX METAR and open-meteo, written into the mission before it next loads (the `livewx` crate, started from `bflib/src/bg/mod.rs`; `LiveWeatherConfig`). `TimeOfDayEffectsCfg` sets the night kill bonus (`bflib/src/db/player.rs`). `WeatherEffectsCfg` sets the convoy speed multipliers for rain, storm and snow (`bflib/src/db/logistics.rs`). **Remaining:**
  - Three `WeatherEffectsCfg` fields are parsed but never read by bflib, so today they are config-only: `min_visibility_fixed_wing`, `no_helo_in_thunderstorm` and `ewr_weather_range_multiplier`.
  - Seasonal campaign progression has not been started.
- **Description**: Environmental effects on gameplay

**Features still open**:
- Weather-dependent spawn restrictions
- Weather-reduced EWR range
- Seasonal campaign progression

**Spawn restriction sketch** (for the config-only fields):
```rust
fn can_spawn_aircraft(weather: &Weather, aircraft_type: &Vehicle) -> bool {
    // Helicopters can't fly in thunderstorms
    if aircraft_type.is_helicopter() && weather.precipitation == 2 {
        return false;
    }

    // Fixed-wing needs minimum visibility for takeoff
    if !aircraft_type.is_helicopter() && weather.visibility < 800.0 {
        return false;
    }

    true
}
```

**Files to modify**:
- `bfprotocols/src/cfg/mod.rs` - `WeatherEffectsCfg`
- `bflib/src/ewr.rs` - Weather range multiplier
- The slot-entry checks in bflib - Weather spawn restrictions (location unverified)

---

#### 6. Mission Planning Tools

- **Priority**: Low
- **Status**: Partly done. Done: a per-coalition auto briefing and sitrep with a server-wide comms plan (`bflib/src/situation.rs`); the F10 tasking board with CAP/CAS/CAPTURE/SUPPLY tasks (`bflib/src/db/tasks.rs`); paid tasking, which gives each pilot a task zone at slot entry (`bflib/src/db/tasking.rs`); and the bfweb `BriefingPage`. **Remaining:** strike package assembly for players, multi-flight coordination, waypoint sharing and time-on-target coordination.
- **Description**: Coordinated mission support

**Features still open**:
- Coordinated multi-flight missions
- Strike package assembly
- Waypoint sharing between flights
- Time-on-target coordination

**Implementation Details**:

**Mission data structure**:
```rust
// New file: bflib/src/db/missions.rs

use chrono::{DateTime, Utc};
use compact_str::CompactString;

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PlannedMission {
    pub id: MissionId,
    pub name: CompactString,
    pub mission_type: MissionType,
    pub creator: Ucid,
    pub side: Side,
    pub created_at: DateTime<Utc>,
    pub scheduled_time: Option<DateTime<Utc>>,  // Optional TOT
    pub status: MissionStatus,
    pub flights: Vec<FlightPlan>,
    pub target: Option<MissionTarget>,
    pub briefing: CompactString,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum MissionType {
    Strike,           // Ground attack
    Sweep,            // Fighter sweep
    Escort,           // Protect other flights
    Sead,             // Suppress enemy air defenses
    Cap,              // Combat air patrol
    Reconnaissance,   // Intel gathering
    Transport,        // Cargo/troop movement
    Refueling,        // Tanker support
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FlightPlan {
    pub callsign: CompactString,
    pub aircraft_type: Vehicle,
    pub num_aircraft: u8,
    pub role: FlightRole,
    pub waypoints: Vec<PlanWaypoint>,
    pub assigned_players: Vec<Ucid>,
    pub loadout_suggestion: Option<CompactString>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PlanWaypoint {
    pub name: CompactString,
    pub pos: Vector2<f64>,
    pub altitude: f64,
    pub speed: f64,
    pub tot: Option<DateTime<Utc>>,  // Time on target
    pub action: WaypointAction,
}

#[derive(Debug, Clone, Copy, Serialize, Deserialize)]
pub enum MissionStatus {
    Planning,
    Briefing,
    Active,
    Completed,
    Cancelled,
}
```

**F10 menu integration**:
```rust
// In bflib/src/menu/mission.rs (new file):

pub fn build_mission_menu(db: &Db, player: &Player) -> Result<Menu> {
    let mut menu = Menu::new("Mission Planning");

    // View active missions
    menu.add_submenu("Active Missions", build_active_missions_menu(db, player)?);

    // Create new mission (if authorized)
    if player_can_create_missions(db, player) {
        menu.add_submenu("Create Mission", build_create_mission_menu(db, player)?);
    }

    // Join a mission
    menu.add_submenu("Join Mission", build_join_mission_menu(db, player)?);

    // View mission briefings
    menu.add_submenu("Briefings", build_briefings_menu(db, player)?);

    Ok(menu)
}

fn execute_create_mission(db: &mut Db, player: &Player, mission_type: MissionType) -> Result<()> {
    let mission = PlannedMission {
        id: MissionId::new(),
        name: format!("{} {}", mission_type, db.next_mission_number()).into(),
        mission_type,
        creator: player.ucid.clone(),
        side: player.side,
        created_at: Utc::now(),
        scheduled_time: None,
        status: MissionStatus::Planning,
        flights: vec![],
        target: None,
        briefing: "".into(),
    };

    db.persisted.missions.insert(mission.id, mission);

    // Notify player with mission ID
    msg!(db, player.ucid, "Mission created: {}", mission.name);

    Ok(())
}
```

**Waypoint sharing**:
```rust
// Allow players to copy waypoints from a flight plan to their aircraft

fn share_waypoints_to_player(
    lua: &Lua,
    flight: &FlightPlan,
    player_unit: &Unit,
) -> Result<()> {
    // Get player's group
    let group = player_unit.get_group()?;
    let controller = group.get_controller()?;

    // Build route from flight plan waypoints
    let route = Route {
        points: flight.waypoints.iter().map(|wp| {
            Waypoint {
                pos: Vector3::new(wp.pos.x, wp.altitude, wp.pos.y),
                speed: wp.speed,
                action: wp.action,
                name: Some(wp.name.clone()),
                ..Default::default()
            }
        }).collect(),
    };

    // Set the route (this updates the player's F10 map waypoints)
    controller.set_task(Task::Mission { route })?;

    Ok(())
}
```

**Files to create/modify**:
- `bflib/src/db/missions.rs` - New mission planning module
- `bflib/src/menu/mission.rs` - F10 menu for missions
- `bflib/src/db/mod.rs` - Add missions to Db
- `bfprotocols/src/db/mod.rs` - Mission types for serialization

---

#### 7. Advanced Logistics

- **Priority**: Medium
- **Status**: Partly done. Done: supply interdiction (interdicted ground routes, and points for convoy kills via `convoy_interdiction_points`); the materiel war economy, storage depth and front-line supply routing; supply-transfer crates for fuel; AI helo resource delivery; air and sea logistics routes; the emergency repair crate (`bflib/src/db/emergency_repair.rs`); and finite SAM magazines (`bflib/src/modern_war/sam_stock.rs`). **Remaining:** the fuel pipeline system (no pipeline code exists in bflib or bfprotocols), player emergency resupply missions, and ammunition types affecting availability beyond SAM magazines.
- **Description**: Deeper supply chain mechanics

**Features still open**:
- Multiple supply sources with different capabilities (status unverified)
- Emergency resupply missions
- Fuel pipeline system for forward bases
- Ammunition types affecting availability

**Implementation Details**:

**Fuel pipeline system**:
```rust
// Pipelines provide continuous fuel flow, cheaper than convoys but vulnerable

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FuelPipeline {
    pub id: PipelineId,
    pub from: ObjectiveId,  // Must be hub or port
    pub to: ObjectiveId,
    pub capacity: f64,      // Liters per tick
    pub segments: Vec<PipelineSegment>,
    pub operational: bool,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PipelineSegment {
    pub start: Vector2<f64>,
    pub end: Vector2<f64>,
    pub health: f32,  // 0.0 - 1.0
}

impl FuelPipeline {
    pub fn tick(&mut self, db: &mut Db) -> Result<()> {
        if !self.operational {
            return Ok(());
        }

        // Check if any segment is destroyed
        let min_health = self.segments.iter()
            .map(|s| s.health)
            .min_by(|a, b| a.partial_cmp(b).unwrap())
            .unwrap_or(1.0);

        if min_health <= 0.0 {
            self.operational = false;
            return Ok(());
        }

        // Flow rate reduced by damage
        let effective_capacity = self.capacity * min_health as f64;

        // Transfer fuel
        let source = db.persisted.objectives.get_mut(&self.from)?;
        let available = source.warehouse.fuel.min(effective_capacity);
        source.warehouse.fuel -= available;

        let dest = db.persisted.objectives.get_mut(&self.to)?;
        dest.warehouse.fuel += available;

        Ok(())
    }
}

// Pipeline damage from bombs/missiles
pub fn damage_pipeline_at(db: &mut Db, pos: Vector2<f64>, damage: f32) -> Result<()> {
    for pipeline in db.persisted.pipelines.values_mut() {
        for segment in &mut pipeline.segments {
            let dist = distance_to_line_segment(pos, segment.start, segment.end);
            if dist < 50.0 {  // 50m damage radius
                segment.health = (segment.health - damage).max(0.0);
            }
        }
    }
    Ok(())
}
```

**Emergency resupply missions**:
```rust
// Player-flyable emergency resupply using C-130 or helicopters

pub fn request_emergency_resupply(
    db: &mut Db,
    player: &Player,
    target: ObjectiveId,
) -> Result<()> {
    // Check if player has appropriate aircraft
    let unit = db.player_unit(player)?;
    if !unit.vehicle.can_carry_cargo() {
        return Err(anyhow!("Aircraft cannot carry cargo"));
    }

    // Create emergency resupply mission
    let mission = EmergencyResupply {
        id: ResupplyId::new(),
        player: player.ucid.clone(),
        target,
        cargo: generate_emergency_cargo(unit.vehicle),
        status: ResupplyStatus::InProgress,
        created_at: db.mission_time(),
    };

    // Award bonus points on successful delivery
    // Tracked in cargo.rs delivery logic

    db.ephemeral.emergency_resupplies.insert(mission.id, mission);

    msg!(db, player.ucid, "Emergency resupply mission started. Deliver to {}",
         db.objective(&target)?.name);

    Ok(())
}
```

**Files to modify**:
- `bflib/src/db/logistics.rs` - Convoy tracking, interdiction, pipelines
- `bflib/src/shots.rs` - Convoy kill detection
- `bflib/src/db/cargo.rs` - Emergency resupply delivery
- `bfprotocols/src/cfg/mod.rs` - Pipeline configuration

---

#### 8. Dynamic Campaign Events

- **Priority**: Low
- **Status**: Partly done. Done: `bflib/src/db/events.rs` runs artillery barrages, missile strikes, convoy ambushes, reactive enemy CAP and helo patrols, and commander CAP. Counter-attacks on threatened bases come from the AI ground commander (`bflib/src/groundwar/ai.rs`), and reinforcement convoys from the Reinforce action (`bflib/src/db/reinforce.rs`). **Remaining:** VIP extraction and civilian evacuation are config-only: `CampaignEventsCfg` has `vip_reward_points` and `evacuation_reward_per_civilian`, but nothing in bflib reads them. Time-limited high-value targets, scheduled reinforcement waves and counter-offensive events have not been started.
- **Description**: Random/scheduled events that affect gameplay

**Features still open**:
- Civilian evacuation missions
- VIP extraction/insertion
- Time-limited high-value targets
- Reinforcement waves
- Counter-offensive events

**Implementation Details**:

**Event system architecture**:
```rust
// Sketch: the new event kinds, extending bflib/src/db/events.rs

use rand::Rng;

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum CampaignEvent {
    HighValueTarget {
        id: EventId,
        target_group: GroupId,
        location: ObjectiveId,
        expires_at: f64,
        reward_points: i32,
        announced: bool,
    },
    CivilianEvacuation {
        id: EventId,
        from: ObjectiveId,
        to: ObjectiveId,
        civilians_remaining: u32,
        deadline: f64,
        reward_per_civilian: i32,
    },
    VipExtraction {
        id: EventId,
        vip_location: Vector2<f64>,
        extraction_zone: ObjectiveId,
        vip_group: Option<GroupId>,
        status: VipStatus,
        reward: i32,
    },
    ReinforcementWave {
        id: EventId,
        side: Side,
        objective: ObjectiveId,
        units: Vec<Vehicle>,
        arrival_time: f64,
    },
    CounterOffensive {
        id: EventId,
        attacking_side: Side,
        targets: Vec<ObjectiveId>,
        strength: f64,  // Multiplier for AI aggressiveness
        duration: f64,
    },
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct EventScheduler {
    pub active_events: Vec<CampaignEvent>,
    pub event_cooldowns: HashMap<EventType, f64>,
    pub last_event_check: f64,
}

impl EventScheduler {
    pub fn tick(&mut self, db: &mut Db, mission_time: f64) -> Result<()> {
        // Check for expired events
        self.cleanup_expired_events(db, mission_time)?;

        // Maybe spawn new events
        if mission_time - self.last_event_check > 300.0 {  // Every 5 minutes
            self.maybe_spawn_event(db, mission_time)?;
            self.last_event_check = mission_time;
        }

        // Update active events
        for event in &mut self.active_events {
            self.update_event(db, event, mission_time)?;
        }

        Ok(())
    }

    fn maybe_spawn_event(&mut self, db: &mut Db, time: f64) -> Result<()> {
        let mut rng = rand::thread_rng();

        // 10% chance per check to spawn an event
        if rng.gen::<f64>() > 0.1 {
            return Ok(());
        }

        // Choose event type based on campaign state
        let event_type = self.choose_event_type(db, &mut rng);

        match event_type {
            EventType::CivilianEvacuation => {
                self.spawn_evacuation_event(db, time)?;
            }
            // ... etc
        }

        Ok(())
    }


}
```

**VIP extraction implementation**:
```rust
fn spawn_vip_extraction(&mut self, db: &mut Db, time: f64) -> Result<()> {
    // Find location behind enemy lines (friendly territory surrounded by enemy)
    let vip_location = find_isolated_friendly_territory(db)?;

    // Spawn VIP group (infantry squad)
    let vip_pos = jitter_position(vip_location, 500.0);  // Random offset
    let vip_group = spawn_infantry_group(db, vip_pos, "VIP_Extraction")?;

    // Find extraction zone (nearest friendly airbase)
    let extraction = find_nearest_friendly_airbase(db, vip_pos)?;

    let event = CampaignEvent::VipExtraction {
        id: EventId::new(),
        vip_location: vip_pos,
        extraction_zone: extraction,
        vip_group: Some(vip_group),
        status: VipStatus::Awaiting,
        reward: 300,
    };

    self.active_events.push(event);

    // Mark on F10 map with smoke
    trigger_smoke(db, vip_pos, SmokeColor::Green)?;

    broadcast_message(db, format!(
        "URGENT: Friendly personnel stranded at grid {}. Extract to {}.",
        format_grid(vip_pos),
        db.objective(&extraction)?.name
    ))?;

    Ok(())
}

// In cargo.rs, detect VIP pickup:
fn on_troops_loaded(db: &mut Db, player: &Player, troops: &[TroopId]) -> Result<()> {
    // Check if any loaded troop is a VIP
    for event in &mut db.events.active_events {
        if let CampaignEvent::VipExtraction { vip_group, status, .. } = event {
            if troops.iter().any(|t| Some(*t) == vip_group.map(|g| g.into())) {
                *status = VipStatus::InTransit;
                msg!(db, player.ucid, "VIP secured! Deliver to extraction zone.");
            }
        }
    }
    Ok(())
}
```

**Files to create/modify**:
- `bflib/src/db/events.rs` - New event kinds
- `bflib/src/db/mod.rs` - Add EventScheduler to Db
- `bflib/src/lib.rs` - Call event tick in slow_timed_events
- `bflib/src/db/cargo.rs` - VIP pickup detection
- `bfprotocols/src/cfg/mod.rs` - Event configuration

---

## Performance Improvements

### Critical Path Optimizations

---

#### 1. Frontline Calculation

- **Status**: Partly done. Done: the frontline is now an influence field contoured with marching squares (`bfprotocols/src/frontline.rs`, drawn by `bflib/src/frontline.rs`). It is recomputed only when an objective changes hands, and the F10 redraw is simplified to at most 250 segments. **Remaining:** the field still sums every objective and pressure point for each cell of a 160×160 grid, O(grid² × points), with no spatial index. `rstar` is declared in `bflib/Cargo.toml`, but no code uses it. There is no incremental update.
- **Improvement**: Use a spatial index (R-tree or KD-tree) so each cell visits only the points within the 3σ cutoff, and recompute only the cells near a changed objective.

**Spatial index sketch** (written for the old nearest-objective grid; the same tree serves a radius query):
```rust
use rstar::{RTree, AABB};

// Build R-tree once when objectives change
let rtree: RTree<ObjectivePoint> = RTree::bulk_load(
    objectives.iter()
        .map(|obj| ObjectivePoint {
            pos: [obj.pos.x, obj.pos.y],
            side: obj.owner
        })
        .collect()
);

// O(log n) nearest-neighbor lookup per cell
for x in 0..grid_width {
    for y in 0..grid_height {
        let pos = grid_to_world(x, y);
        // O(log n) instead of O(n)
        let nearest = rtree.nearest_neighbor(&[pos.x, pos.y]).unwrap();
        grid[x][y] = nearest.side;
    }
}
```

**Incremental update optimization**:
```rust
// Only recalculate cells near changed objectives
fn update_frontline_incremental(
    grid: &mut FrontlineGrid,
    changed_objectives: &[ObjectiveId],
    all_objectives: &Map<ObjectiveId, Objective>,
) {
    let max_influence_radius = 50000.0;  // 50km

    for obj_id in changed_objectives {
        let obj = &all_objectives[obj_id];

        // Only update cells within influence radius
        let (min_x, min_y) = world_to_grid(obj.pos - max_influence_radius);
        let (max_x, max_y) = world_to_grid(obj.pos + max_influence_radius);

        for x in min_x..=max_x {
            for y in min_y..=max_y {
                // Recalculate just this cell
                grid[x][y] = find_nearest_side(grid_to_world(x, y), all_objectives);
            }
        }
    }
}
```

---

#### 2. JTAC Contact Scanning

- **Status**: Open (status unverified)
- **Location**: `bflib/src/jtac.rs`
- **Current complexity**: O(num_jtacs × num_ground_units)
- **Impact**: Scales poorly with large unit counts
- **Improvement**: Spatial partitioning
- **Expected gain**: O(num_jtacs × nearby_units)

**Current pattern**:
```rust
fn update_jtac_contacts(db: &mut Db) {
    for jtac in &db.jtacs {
        for unit in &db.all_ground_units {  // O(n) scan
            if is_enemy(jtac, unit) && in_range(jtac, unit) && has_los(jtac, unit) {
                jtac.contacts.insert(unit.id);
            }
        }
    }
}
```

**Optimized with spatial grid**:
```rust
// Spatial hash grid for unit positions
pub struct SpatialGrid {
    cells: HashMap<(i32, i32), Vec<UnitId>>,
    cell_size: f64,  // e.g., 5000m = 5km cells
}

impl SpatialGrid {
    pub fn cell_for_pos(&self, pos: Vector2<f64>) -> (i32, i32) {
        (
            (pos.x / self.cell_size).floor() as i32,
            (pos.y / self.cell_size).floor() as i32,
        )
    }

    pub fn units_in_radius(&self, center: Vector2<f64>, radius: f64) -> impl Iterator<Item = UnitId> {
        let cell_radius = (radius / self.cell_size).ceil() as i32;
        let (cx, cy) = self.cell_for_pos(center);

        // Only check nearby cells
        (cx - cell_radius..=cx + cell_radius)
            .flat_map(move |x| {
                (cy - cell_radius..=cy + cell_radius)
                    .filter_map(move |y| self.cells.get(&(x, y)))
                    .flatten()
                    .copied()
            })
    }
}

// Optimized JTAC scan
fn update_jtac_contacts(db: &mut Db) {
    let max_jtac_range = 20000.0;  // 20km

    for jtac in &db.jtacs {
        // Only check units in nearby cells - O(nearby) instead of O(all)
        for unit_id in db.spatial_grid.units_in_radius(jtac.pos, max_jtac_range) {
            let unit = &db.units[&unit_id];
            if is_enemy(jtac, unit) && in_range(jtac, unit) && has_los(jtac, unit) {
                jtac.contacts.insert(unit.id);
            }
        }
    }
}

// Update spatial grid when units move (in position update code)
fn update_unit_position(db: &mut Db, unit_id: UnitId, new_pos: Vector2<f64>) {
    let old_cell = db.spatial_grid.cell_for_pos(db.units[&unit_id].pos);
    let new_cell = db.spatial_grid.cell_for_pos(new_pos);

    if old_cell != new_cell {
        db.spatial_grid.cells.get_mut(&old_cell).map(|c| c.retain(|&id| id != unit_id));
        db.spatial_grid.cells.entry(new_cell).or_default().push(unit_id);
    }

    db.units.get_mut(&unit_id).unwrap().pos = new_pos;
}
```

---

#### 3. Logistics Warehouse Sync

- **Status**: Open (status unverified)
- **Location**: `bflib/src/db/logistics.rs`
- **Current complexity**: Potential O(n²) for transfers between objectives
- **Impact**: Slow with many objectives
- **Improvement**: Priority queue, batch transfers
- **Expected gain**: O(n log n)

**Current pattern** (simplified):
```rust
fn sync_warehouses(db: &mut Db) {
    // Phase 1: Calculate needs
    for obj in &db.objectives {
        let needs = calculate_needs(obj);
        // ...
    }

    // Phase 2: Find sources (potentially O(n²))
    for dest in &db.objectives {
        for source in &db.objectives {  // Nested loop
            if can_transfer(source, dest) {
                transfers.push((source, dest, amount));
            }
        }
    }

    // Phase 3: Execute
    for transfer in transfers {
        execute_transfer(transfer);
    }
}
```

**Optimized implementation**:
```rust
use std::collections::BinaryHeap;

#[derive(Eq, PartialEq)]
struct TransferPriority {
    urgency: i32,  // Higher = more urgent
    dest: ObjectiveId,
    source: ObjectiveId,
    amount: u32,
}

impl Ord for TransferPriority {
    fn cmp(&self, other: &Self) -> Ordering {
        self.urgency.cmp(&other.urgency)
    }
}

fn sync_warehouses_optimized(db: &mut Db) {
    let mut transfer_queue: BinaryHeap<TransferPriority> = BinaryHeap::new();

    // Phase 1: Build priority queue of needs
    for dest in db.objectives.values() {
        if dest.warehouse.needs_supplies() {
            let urgency = calculate_urgency(dest);  // Lower stock = higher urgency

            // Find nearest source with supplies (use spatial index)
            if let Some(source) = find_nearest_source(db, dest) {
                transfer_queue.push(TransferPriority {
                    urgency,
                    dest: dest.id,
                    source,
                    amount: calculate_transfer_amount(dest),
                });
            }
        }
    }

    // Phase 2: Process in priority order, respecting capacity
    let mut processed_this_tick = 0;
    let max_transfers_per_tick = 10;

    while let Some(transfer) = transfer_queue.pop() {
        if processed_this_tick >= max_transfers_per_tick {
            break;  // Spread across multiple ticks
        }

        if execute_transfer(db, &transfer).is_ok() {
            processed_this_tick += 1;
        }
    }
}

// Cache source availability to avoid repeated lookups
fn find_nearest_source(db: &Db, dest: &Objective) -> Option<ObjectiveId> {
    // Use pre-computed supply network graph
    db.supply_network
        .sources_for(dest.id)
        .filter(|src| db.objectives[src].warehouse.has_surplus())
        .min_by_key(|src| db.supply_network.distance(dest.id, *src))
}
```

---

#### 4. Unit Position Updates

- **Status**: Open (status unverified)
- **Location**: `bflib/src/db/group.rs`, `bflib/src/db/player.rs`
- **Current**: Incremental updates (good), but still iterates all units
- **Improvement**: Dirty flag system, batch Lua calls

**Current pattern**:
```rust
fn update_unit_positions_incremental(db: &mut Db, lua: &Lua) -> Result<()> {
    for (idx, (unit_id, unit)) in db.units.iter().enumerate().skip(db.position_index) {
        let pos = unit.get_position(lua)?;  // Individual Lua call
        // Update position...
    }
}
```

**Optimized with batching**:
```rust
fn update_unit_positions_batched(db: &mut Db, lua: &Lua) -> Result<()> {
    // Batch position queries via custom Lua function
    let batch_size = 50;
    let unit_ids: Vec<_> = db.units.keys()
        .skip(db.position_index)
        .take(batch_size)
        .collect();

    if unit_ids.is_empty() {
        db.position_index = 0;
        return Ok(());
    }

    // Single Lua call to get multiple positions
    let positions: HashMap<UnitId, Vector3<f64>> = lua
        .globals()
        .get::<_, Function>("FOWL_GetUnitPositions")?
        .call(unit_ids.clone())?;

    for unit_id in unit_ids {
        if let Some(pos) = positions.get(&unit_id) {
            db.units.get_mut(&unit_id).map(|u| u.pos = *pos);
        }
    }

    db.position_index += batch_size;
    Ok(())
}

// Add to Lua initialization:
fn init_lua_helpers(lua: &Lua) -> Result<()> {
    lua.globals().set("FOWL_GetUnitPositions", lua.create_function(|_, unit_ids: Vec<u32>| {
        let mut result = HashMap::new();
        for id in unit_ids {
            if let Some(unit) = Unit::get_by_id(id) {
                result.insert(id, unit.get_position()?);
            }
        }
        Ok(result)
    })?)?;
    Ok(())
}
```

---

### Memory & Allocation Optimizations

#### 5. Reduce SmallVec/CompactString Churn

- **Status**: Open
- **Issue**: Frequent small allocations create GC pressure
- **Locations**: Throughout codebase, especially in hot paths

**Object pooling pattern**:
```rust
// New file: bflib/src/pool.rs

use std::cell::RefCell;

thread_local! {
    static VEC_POOL: RefCell<Vec<Vec<u8>>> = RefCell::new(Vec::new());
    static STRING_POOL: RefCell<Vec<String>> = RefCell::new(Vec::new());
}

pub fn get_vec() -> PooledVec {
    VEC_POOL.with(|pool| {
        pool.borrow_mut().pop().unwrap_or_default()
    }).into()
}

pub fn get_string() -> PooledString {
    STRING_POOL.with(|pool| {
        pool.borrow_mut().pop().unwrap_or_default()
    }).into()
}

pub struct PooledVec(Vec<u8>);

impl Drop for PooledVec {
    fn drop(&mut self) {
        self.0.clear();
        VEC_POOL.with(|pool| {
            if pool.borrow().len() < 100 {  // Cap pool size
                pool.borrow_mut().push(std::mem::take(&mut self.0));
            }
        });
    }
}
```

---

#### 6. Expand Lua API Caching

- **Status**: Open
- **Current**: `LandCache` exists for terrain data (`bflib/src/landcache.rs`)
- **Location**: `bflib/src/db/ephemeral.rs`

**Additional caching**:
```rust
// Add to Ephemeral struct in ephemeral.rs:

pub struct Ephemeral {
    // ... existing fields ...

    /// Cache of unit positions, updated incrementally
    pub position_cache: HashMap<UnitId, CachedPosition>,

    /// Cache of coalition data (rarely changes)
    pub coalition_cache: Option<CoalitionCache>,

    /// Cache of group compositions (only changes on spawn/death)
    pub group_cache: HashMap<GroupId, CachedGroupComposition>,
}

#[derive(Clone)]
pub struct CachedPosition {
    pub pos: Vector3<f64>,
    pub heading: f64,
    pub velocity: Vector3<f64>,
    pub updated_at: f64,  // Mission time
}

impl CachedPosition {
    /// Extrapolate position based on velocity
    pub fn extrapolate(&self, current_time: f64) -> Vector3<f64> {
        let dt = current_time - self.updated_at;
        self.pos + self.velocity * dt
    }

    /// Check if cache is stale
    pub fn is_stale(&self, current_time: f64, max_age: f64) -> bool {
        current_time - self.updated_at > max_age
    }
}

// Use cached position with extrapolation:
fn get_unit_position(db: &Db, unit_id: UnitId, time: f64) -> Result<Vector3<f64>> {
    if let Some(cached) = db.ephemeral.position_cache.get(&unit_id) {
        if !cached.is_stale(time, 1.0) {  // 1 second max age
            return Ok(cached.extrapolate(time));
        }
    }

    // Cache miss - fetch from Lua and update cache
    let unit = db.unit(unit_id)?;
    let pos = unit.get_position()?;
    let vel = unit.get_velocity()?;

    db.ephemeral.position_cache.insert(unit_id, CachedPosition {
        pos,
        heading: unit.get_heading()?,
        velocity: vel,
        updated_at: time,
    });

    Ok(pos)
}
```

---

### Quick Wins

| Improvement | Effort | Impact | Location | Implementation |
|-------------|--------|--------|----------|----------------|
| Incremental frontline updates | Medium | High | `bfprotocols/src/frontline.rs` | Track changed objectives, update locally (see Frontline Calculation) |

---

## Technical Debt

### Error Handling (`unwrap()` calls)

- **Status**: Partly done. The files named in the first audit are clean outside tests: `bflib/src/db/cargo.rs` has 2 (`partial_cmp(..).unwrap()` in sorts), and `bflib/src/db/actions.rs`, `bflib/src/admin.rs` and `bflib/src/bg/logpub.rs` have none. bflib has about 64 non-test `unwrap()` calls in all, most of them in newer code. (Counted 2026-10-25, up to each file's first `#[cfg(test)]`.)

**Risk Level**: High for production stability

**Files with the most non-test `unwrap()` calls**:

| File | Count | Risk Areas |
|------|-------|------------|
| `bflib/src/db/formation.rs` | 26 | `formations.get_mut_cow(&id).unwrap()` after a lookup |
| `bflib/src/db/player.rs` | 8 | Not reviewed |
| `bflib/src/menu/cargo.rs` | 7 | Not reviewed |
| `bflib/src/db/group.rs` | 4 | Not reviewed |

**Systematic fix approach**:
```rust
// Before (risky):
let obj = db.objectives.get(&obj_id).unwrap();
let crate_data = cargo.crates.pop().unwrap();

// After (safe):
let obj = db.objectives.get(&obj_id)
    .ok_or_else(|| anyhow!("Objective {} not found", obj_id))?;
let crate_data = cargo.crates.pop()
    .ok_or_else(|| anyhow!("No crates available"))?;

// For non-critical paths, use unwrap_or_default or if-let:
let name = unit.get_name().unwrap_or_default();

if let Some(obj) = db.objectives.get(&obj_id) {
    // Process objective
} else {
    log::warn!("Objective {} not found, skipping", obj_id);
}
```

**Priority order for fixes**:
1. `formation.rs` - Runs on every ground war tick
2. `menu/cargo.rs` and `player.rs` - Affect player interactions
3. `group.rs` - Affects group spawning and tracking

---

### Data Consistency

- **Status**: Open (status unverified)

**Issue**: Multi-step operations can leave corrupted state if interrupted

**Example problematic pattern** (in `logistics.rs`):
```rust
fn transfer_supplies(db: &mut Db, from: ObjectiveId, to: ObjectiveId, amount: u32) -> Result<()> {
    // Step 1: Remove from source
    db.objectives.get_mut(&from)?.warehouse.supplies -= amount;

    // Step 2: Add to destination (what if this fails?)
    db.objectives.get_mut(&to)?.warehouse.supplies += amount;
    // If step 2 fails, supplies are lost!

    Ok(())
}
```

**Safe pattern with validation**:
```rust
fn transfer_supplies(db: &mut Db, from: ObjectiveId, to: ObjectiveId, amount: u32) -> Result<()> {
    // Validate both objectives exist first
    let _ = db.objectives.get(&from)
        .ok_or_else(|| anyhow!("Source {} not found", from))?;
    let _ = db.objectives.get(&to)
        .ok_or_else(|| anyhow!("Destination {} not found", to))?;

    // Now perform atomic-style update
    {
        let source = db.objectives.get_mut_cow(&from)?;
        if source.warehouse.supplies < amount {
            return Err(anyhow!("Insufficient supplies"));
        }
        source.warehouse.supplies -= amount;
    }

    {
        let dest = db.objectives.get_mut_cow(&to)?;
        dest.warehouse.supplies += amount;
    }

    Ok(())
}
```

---

### Code Organization

- **Status**: Open. The large files have grown since the first audit (line counts from 2026-10-25).

**Large files needing refactoring**:

| File | Lines | Suggested Split |
|------|-------|-----------------|
| `bflib/src/db/logistics.rs` | 8051 | `logistics/warehouse.rs`, `logistics/convoy.rs`, `logistics/transfer.rs` |
| `bflib/src/db/cargo.rs` | 5601 | Not planned yet |
| `bflib/src/db/actions.rs` | 5009 | Not planned yet |
| `bflib/src/db/objective.rs` | 4654 | `objective/capture.rs`, `objective/spawning.rs`, `objective/state.rs` |
| `bflib/src/admin.rs` | 4123 | Not planned yet |
| `bflib/src/jtac.rs` | 2997 | `jtac/targeting.rs`, `jtac/laser.rs`, `jtac/reports.rs` |

**Refactoring approach**:
```rust
// bflib/src/db/logistics/mod.rs
mod warehouse;
mod convoy;
mod transfer;
mod factory;

pub use warehouse::*;
pub use convoy::*;
pub use transfer::*;
pub use factory::*;

// Re-export main tick function
pub fn tick_logistics(db: &mut Db) -> Result<()> {
    warehouse::sync_warehouses(db)?;
    convoy::update_convoys(db)?;
    transfer::process_transfers(db)?;
    factory::tick_production(db)?;
    Ok(())
}
```

---

## Scalability Testing Needed

- **Status**: Open (status unverified). No load test results are recorded.

| Scenario | Target | Current Status | Test Method | Concerns |
|----------|--------|----------------|-------------|----------|
| Objectives | 100+ | Untested | Create test mission with 100 objectives | Frontline calculation, O(n²) transfers |
| Active units | 1000+ | Untested | Spawn 1000 AI units, measure frame time | JTAC scanning, position updates |
| Concurrent players | 50+ | Untested | Load test with dummy connections | Message rate limiting, slot management |
| Campaign duration | 24h+ | Unknown | Run campaign for extended period | Memory leaks, state bloat, save file growth |
| Save file size | Large campaigns | Unknown | Monitor `.state` file size over time | Load/save performance, disk usage |

**Profiling**: per-stage timings already exist. `bfprotocols/src/perf.rs` keeps HDR histograms for the frame, slow timed events, the logistics phases, JTAC contacts, the frontline and more, and `bflib/src/bg/perf.rs` publishes them over netidx. Use them to measure these scenarios.

---

## Implementation Priority

Open work only. Done items are in the Done section.

### Phase 1: Stability
1. Replace critical `unwrap()` calls - Production stability (see Technical Debt)

### Phase 2: Performance
1. Frontline: spatial index and incremental updates
2. Implement JTAC spatial partitioning - Add spatial grid
3. Expand caching - Position extrapolation, group caching

### Phase 3: Features
1. Finish the public Lua API - AI orders and actions
2. Ground pathfinding with threat avoidance
3. Naval warfare - Amphibious ops, shore bombardment, anti-ship tracking
4. Advanced logistics - Pipelines, emergency resupply, ammunition types

### Phase 4: Polish & Expansion
1. Cockpit plugin - Deploy, final in-game pass
2. Mission planning tools - Strike packages, TOT, waypoint sharing
3. Weather effects - The config-only fields, seasons
4. Dynamic campaign events - VIP, evacuation, high-value targets

---

## Audit Sept 2026

These ideas came out of the September 2026 audit. The ones that shipped are in the Done section.

- **Engine-generated contracts**: Partly done. Paid tasking gives each pilot a task zone at slot entry, with a points multiplier inside it (`bflib/src/db/tasking.rs`). Remaining: posted jobs with a fixed payout (such as "deliver 4 crates to X" or "kill the SA-11 at Y"), built from what the campaign needs.
- **SEAD/assist credit**: Partly done. Kill points are split by weight among everyone who hit the target, with a bonus for the killing blow (`bflib/src/db/player.rs`). Remaining: credit for players who suppressed the defences without hitting the target, and shared capture credit.
- **Contested consolidation**: enemy presence in the zone pauses or reverses post-capture consolidation progress. Today progress pauses only when the holding troops leave, and it is never rolled back (`capture_hold_progress` in `bflib/src/db/objective.rs`).
- **Rookie mode**: Partly done. A pilot with fewer than 5 sorties gets a safer task and a pointer to the getting-started help in the slot briefing (`ROOKIE_SORTIES` in `bflib/src/db/player.rs`, used by `bflib/src/situation.rs`). Remaining: cheaper sorties and no life loss.
- **Squadrons**: Partly done. bfdb has squadrons with a tag, a name, an owner, a member roster, squadron XP and a leaderboard (`bfdb/src/squadrons.rs`, `/api/squadrons`). Remaining: a home base (status unverified).
- **Package planner**: build a strike package with roles, a TOT and shared waypoints (see Mission Planning).
- **VIP/evac events**: wire the existing `CampaignEventsCfg` VIP and evacuation fields to real events.
- **Amphibious ops**: landing craft or helo assault from the sea to capture coastal objectives.

---

## Done

Shipped items, each with where it lives in the code. The design sketches these items used to carry are in git history (`git log -p -- ROADMAP.md`).

### From Known Issues

- **Default JTAC laser codes**: `default_laser_code` (serde default 1688) is applied when a JTAC spawns. `bfprotocols/src/cfg/mod.rs`, `bflib/src/jtac.rs`.
- **JTAC settings persistence**: `InternalTroop.jtac` carries the `JtacState` through troop load and unload. `bflib/src/db/cargo.rs`.
- **Troop carrier system**: mechanized infantry ride IFVs and APCs (`GroundVehiclePassengers`). `bflib/src/db/cargo.rs`.
- **Points message spam**: the points message and stat are sent only when `amount != 0`. `bflib/src/db/player.rs`.
- **C-130 internal crates blocking**: `spawn_crate` counts only the player's ground crates (`max_crates`) and free space, so internally loaded crates no longer block spawning. `bflib/src/db/cargo.rs`.
- **Carrier aircraft spawning**: player aircraft spawn on carriers. The evidence is an in-game check (Oct 2026), not a specific code change.

### Performance

- **Profiling instrumentation**: per-stage HDR histograms (frame, slow timed events, logistics phases, JTAC contacts, frontline and more), published over netidx. `bfprotocols/src/perf.rs`, `bflib/src/bg/perf.rs`.
- **Batch message queue processing**: each pass drains up to `max_msgs_per_second` commands, with a share kept for F10 markup so it is not starved. `take_batch` in `bflib/src/msgq.rs`.

### From the Sept 2026 audit

- **Off-peak protection**: while a base's owner has fewer than `min_defenders` players in a slot, a capture there takes longer (`Cfg::population_scaling`). `bflib/src/db/balance.rs`.
- **Underdog balancing**: underdog pay raises a side's earnings by head count and territory (`underdog_max_bonus`, `underdog_territory_weight`). `bfprotocols/src/cfg/economy.rs`, `bflib/src/db/economy.rs`.
- **Sortie debrief**: a per-flight tally (kills, contributions, weapons fired) shown on landing, and a full-page debrief on the dashboard. `SortieTally` in `bflib/src/db/player.rs`, `bfweb/src/lib/sortie.ts`.
- **AI ground offensives**: each side's AI ground commander raises formations near the front, sends them along the offensive axis, counter-attacks threatened bases and pulls battered formations back. `bflib/src/groundwar/ai.rs`, `bflib/src/db/formation.rs`, `bflib/src/hq/`.

---

## Contributing

When working on items from this roadmap:
1. Create a branch named `feature/<feature-name>` or `fix/<issue-name>`
2. Update this document to reflect current status
3. Add tests where possible
4. Document any new configuration options in the user guide
5. Run `cargo clippy` and fix warnings before submitting

---

*Last reviewed: 2026-10-25*
