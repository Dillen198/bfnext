# Fowl Engine Roadmap

This document tracks potential features, improvements, and technical debt for the BFNEXT project.

---

## Feature Opportunities

### High-Value New Features

---

#### 1. Public Lua API for External Tools

- **Priority**: High
- **Status**: In Progress (`bfprotocols/src/api.rs` started)
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
- **Status**: Not Started
- **Description**: Improve ground unit behavior and logistics

**Features**:
- Convoy pathfinding (currently no actual pathfinding)
- Dynamic route selection based on threats
- Ground unit formations
- Defensive positioning AI
- Retreat/reinforcement logic

**Current convoy implementation** (`bflib/src/db/logistics.rs:1650-1750`):
```rust
// Current: Convoys spawn and move point-to-point with no pathfinding
// They use direct waypoints between objectives
fn spawn_convoy(...) {
    // Creates waypoints as direct line between source and destination
    // No terrain awareness, no threat avoidance
}
```

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
- **Status**: Partial (carrier groups exist in `bfprotocols/src/cfg/mod.rs:CarrierCfg`)
- **Description**: Expand naval operations

**Features**:
- Fix aircraft spawn on carriers (known issue)
- Naval logistics and resupply
- Amphibious operations
- Shore bombardment missions
- Anti-ship missions with proper tracking

**Current carrier implementation** (`bflib/src/admin.rs:800-900`):
```rust
// Carrier repair/respawn exists but aircraft spawning has issues
pub fn repair_carrier(...) { ... }
pub fn respawn_carrier(...) { ... }
```

**Implementation Details**:

**Aircraft Carrier Spawn Fix**:
The issue is likely in spawn positioning. DCS requires specific spawn points on carriers.

```rust
// In bflib/src/spawnctx.rs, add carrier-aware spawning:

impl SpawnCtx {
    pub fn spawn_on_carrier(
        &self,
        carrier_unit: &Unit,
        aircraft_type: Vehicle,
        slot_number: u8,
    ) -> Result<Group> {
        // Get carrier's current position and heading
        let carrier_pos = carrier_unit.get_position()?;
        let carrier_heading = carrier_unit.get_heading()?;

        // Calculate spawn position relative to carrier deck
        // Slot positions are carrier-type specific
        let slot_offset = get_carrier_slot_offset(
            carrier_unit.get_type_name()?,
            slot_number
        )?;

        // Transform to world coordinates
        let spawn_pos = carrier_pos + rotate_vector(slot_offset, carrier_heading);

        // Use "TakeOffParkingHot" for carrier spawns
        let route = Route {
            points: vec![Waypoint {
                action: WaypointAction::TakeOffParkingHot,
                airdromeId: None,
                helipadId: Some(carrier_unit.get_id()?),
                pos: spawn_pos,
                ..Default::default()
            }],
        };

        // ... continue spawn
    }
}

fn get_carrier_slot_offset(carrier_type: &str, slot: u8) -> Result<Vector3<f64>> {
    // Carrier-specific slot positions
    match carrier_type {
        "CVN_74_John_C__Stennis" => match slot {
            1 => Ok(Vector3::new(-20.0, 20.0, 0.0)),
            2 => Ok(Vector3::new(-40.0, 20.0, 0.0)),
            // ... etc
        },
        "CV_1143_5_Admiral_Kuznetsov" => { ... },
        _ => Err(anyhow!("Unknown carrier type: {}", carrier_type)),
    }
}
```

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
- `bflib/src/spawnctx.rs` - Carrier spawn fixes
- `bfprotocols/src/db/objective.rs` - New objective kinds
- `bflib/src/db/logistics.rs` - Naval supply routes
- `bflib/src/db/objective.rs` - Carrier group movement
- `bfprotocols/src/cfg/mod.rs` - Naval config options

---

#### 4. Client-Side Plugin/UI

- **Priority**: Medium
- **Status**: Not Started
- **Description**: Dedicated UI beyond F10 radio menus

**Features**:
- Real-time territory map overlay
- Logistics status dashboard
- Point balance and transaction history
- JTAC target visualization
- Mission briefing display

**Implementation options**: DCS Scratchpad integration, external overlay app, or web-based

**Implementation Details**:

**Option A: DCS Scratchpad Integration** (Easiest)
```lua
-- Scratchpad displays text, we can format campaign data as text
-- In bflib, expose data via trigger.action.outText or custom Lua global

-- Add to lib.rs Lua initialization:
lua.globals().set("FOWL_ENGINE", lua.create_table()?)?;

-- Periodically update the global table with campaign state:
fn update_lua_globals(lua: &Lua, db: &Db) -> Result<()> {
    let fowl = lua.globals().get::<_, Table>("FOWL_ENGINE")?;

    // Player's current points
    fowl.set("my_points", player.points)?;

    // Nearby objectives as formatted string
    fowl.set("objectives_text", format_objectives(db, player.pos)?)?;

    // JTAC targets
    fowl.set("jtac_text", format_jtac_targets(db, player.side)?)?;

    Ok(())
}
```

**Option B: External Overlay App** (Most powerful)
```rust
// New crate: bfoverlay/
// Connects to bfdb web server, renders overlay using egui or similar

// bfdb already has a web server, add WebSocket endpoint for real-time updates:
// In bfdb/src/main.rs:

async fn campaign_ws(
    ws: warp::ws::Ws,
    db: Arc<Database>,
) -> impl Reply {
    ws.on_upgrade(|socket| async move {
        let (mut tx, mut rx) = socket.split();

        // Send initial state
        let state = db.get_campaign_state().await;
        tx.send(Message::text(serde_json::to_string(&state)?)).await?;

        // Subscribe to updates
        let mut updates = db.subscribe_updates();
        while let Some(update) = updates.next().await {
            tx.send(Message::text(serde_json::to_string(&update)?)).await?;
        }
    })
}

// Overlay client renders:
// - Mini-map with objectives (colored by ownership)
// - Frontline visualization
// - Player position and nearby threats
// - Point balance
// - Active missions
```

**Option C: Web-based Dashboard** (Most accessible)
```rust
// Extend bfdb web server with a full SPA dashboard
// Already has warp server, add:

// Static file serving for React/Vue app
let static_files = warp::path("dashboard")
    .and(warp::fs::dir("./dashboard/dist"));

// API endpoints for dashboard
let api = warp::path("api")
    .and(
        campaign_state()
            .or(player_stats())
            .or(objective_details())
            .or(logistics_status())
    );
```

**Files to create/modify**:
- `bfdb/src/web/` - New web module with dashboard routes
- `bfdb/dashboard/` - Frontend SPA (React/Vue/Svelte)
- `bflib/src/lib.rs` - Expose Lua globals for Scratchpad
- `bfoverlay/` - Optional: Native overlay application

---

#### 5. Weather & Time Effects

- **Priority**: Low
- **Status**: Not Started
- **Description**: Environmental effects on gameplay

**Features**:
- Logistics affected by weather (slower convoys in bad weather)
- Night operations bonuses/penalties
- Seasonal campaign progression
- Weather-dependent spawn restrictions

**Implementation Details**:

**Weather data extraction** (from DCS mission):
```rust
// In dcso3/src/env.rs, add weather queries:

impl Env {
    pub fn get_weather(&self) -> Result<Weather> {
        let weather_table = self.lua.globals()
            .get::<_, Table>("env")?
            .get::<_, Table>("mission")?
            .get::<_, Table>("weather")?;

        Ok(Weather {
            cloud_density: weather_table.get("clouds")?.get("density")?,
            cloud_base: weather_table.get("clouds")?.get("base")?,
            precipitation: weather_table.get("type_weather")?,
            visibility: weather_table.get("visibility")?.get("distance")?,
            wind_speed: weather_table.get("wind")?.get("atGround")?.get("speed")?,
            fog_enabled: weather_table.get("fog")?.get("enabled")?,
            fog_visibility: weather_table.get("fog")?.get("visibility")?,
        })
    }

    pub fn get_mission_time(&self) -> Result<MissionTime> {
        let env = self.lua.globals().get::<_, Table>("env")?;
        Ok(MissionTime {
            elapsed: env.call_function("getValueDictByKey", "time")?,
            start_time: env.get::<_, Table>("mission")?.get("start_time")?,
        })
    }
}

pub struct Weather {
    pub cloud_density: u8,      // 0-10
    pub cloud_base: f64,        // meters
    pub precipitation: u8,      // 0=clear, 1=rain, 2=thunderstorm, 3=snow
    pub visibility: f64,        // meters
    pub wind_speed: f64,        // m/s
    pub fog_enabled: bool,
    pub fog_visibility: f64,    // meters
}
```

**Weather effects on logistics**:
```rust
// In bflib/src/db/logistics.rs:

fn calculate_convoy_speed(base_speed: f64, weather: &Weather) -> f64 {
    let mut multiplier = 1.0;

    // Rain/snow slows convoys
    multiplier *= match weather.precipitation {
        0 => 1.0,      // Clear
        1 => 0.8,      // Rain
        2 => 0.6,      // Thunderstorm
        3 => 0.5,      // Snow
        _ => 1.0,
    };

    // Low visibility slows convoys
    if weather.visibility < 5000.0 {
        multiplier *= weather.visibility / 5000.0;
    }

    // Fog
    if weather.fog_enabled && weather.fog_visibility < 1000.0 {
        multiplier *= 0.7;
    }

    base_speed * multiplier.max(0.3)  // Minimum 30% speed
}

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

**Night operations**:
```rust
// Add to configuration (bfprotocols/src/cfg/mod.rs):

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TimeOfDayEffects {
    /// Points multiplier for night kills
    pub night_kill_bonus: f64,  // e.g., 1.5 for 50% bonus
    /// Hour when "night" begins (0-23)
    pub night_start_hour: u8,
    /// Hour when "night" ends
    pub night_end_hour: u8,
    /// Disable certain actions at night
    pub night_restricted_actions: Vec<String>,
}

// In points calculation:
fn calculate_kill_points(base_points: i32, time: &MissionTime, cfg: &TimeOfDayEffects) -> i32 {
    let hour = (time.start_time + time.elapsed) / 3600 % 24;
    let is_night = if cfg.night_start_hour > cfg.night_end_hour {
        // Night spans midnight (e.g., 22:00 - 06:00)
        hour >= cfg.night_start_hour || hour < cfg.night_end_hour
    } else {
        hour >= cfg.night_start_hour && hour < cfg.night_end_hour
    };

    if is_night {
        (base_points as f64 * cfg.night_kill_bonus) as i32
    } else {
        base_points
    }
}
```

**Files to modify**:
- `dcso3/src/env.rs` - Weather/time queries
- `bfprotocols/src/cfg/mod.rs` - Weather effect config
- `bflib/src/db/logistics.rs` - Weather-modified convoy speed
- `bflib/src/db/actions.rs` - Weather spawn restrictions
- `bflib/src/db/player.rs` - Night kill bonuses

---

#### 6. Mission Planning Tools

- **Priority**: Low
- **Status**: Not Started
- **Description**: Coordinated mission support

**Features**:
- Pre-flight briefing system
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
- **Status**: Partial
- **Description**: Deeper supply chain mechanics

**Features**:
- Multiple supply sources with different capabilities
- Supply interdiction (destroy convoys to affect enemy)
- Emergency resupply missions
- Fuel pipeline system for forward bases
- Ammunition types affecting availability

**Implementation Details**:

**Supply interdiction tracking**:
```rust
// Track convoy destruction and its effects
// In bflib/src/db/logistics.rs:

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ConvoyState {
    pub id: ConvoyId,
    pub from: ObjectiveId,
    pub to: ObjectiveId,
    pub cargo: WarehouseContents,
    pub group_id: GroupId,
    pub spawn_time: f64,
    pub expected_arrival: f64,
}

// In shots.rs or a new interdiction.rs:
pub fn on_convoy_destroyed(db: &mut Db, convoy_id: ConvoyId, killer: Option<Ucid>) -> Result<()> {
    let convoy = db.ephemeral.active_convoys.remove(&convoy_id)
        .ok_or_else(|| anyhow!("Convoy not found"))?;

    // Award points to killer
    if let Some(ucid) = killer {
        let points = calculate_interdiction_points(&convoy.cargo);
        award_points(db, &ucid, points, "Convoy interdiction")?;
    }

    // The supplies are lost - destination doesn't receive them
    // Log for statistics
    db.stats.convoys_destroyed += 1;
    db.stats.supplies_interdicted += convoy.cargo.total_value();

    // Potentially trigger emergency resupply if destination critically low
    let dest = db.persisted.objectives.get(&convoy.to)?;
    if dest.warehouse.is_critically_low() {
        trigger_emergency_resupply(db, convoy.to)?;
    }

    Ok(())
}

fn calculate_interdiction_points(cargo: &WarehouseContents) -> i32 {
    // Points based on cargo value
    let mut points = 0;
    points += cargo.fuel as i32 / 1000;  // 1 point per 1000L fuel
    points += cargo.ammo as i32 / 100;   // 1 point per 100 rounds
    points += cargo.equipment.len() as i32 * 10;  // 10 points per vehicle
    points
}
```

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
- **Status**: Not Started
- **Description**: Random/scheduled events that affect gameplay

**Features**:
- Civilian evacuation missions
- VIP extraction/insertion
- Time-limited high-value targets
- Reinforcement waves
- Counter-offensive events

**Implementation Details**:

**Event system architecture**:
```rust
// New file: bflib/src/db/events.rs

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
- `bflib/src/db/events.rs` - New event system
- `bflib/src/db/mod.rs` - Add EventScheduler to Db
- `bflib/src/lib.rs` - Call event tick in slow_timed_events
- `bflib/src/db/cargo.rs` - VIP pickup detection

- `bfprotocols/src/cfg/mod.rs` - Event configuration

---

## Known Issues & Incomplete Features

| Feature | Status | Priority | Location | Notes |
|---------|--------|----------|----------|-------|
| Default JTAC laser codes | Missing | High | `bfprotocols/src/cfg/mod.rs` | Add `default_laser_code: u16` to JtacCfg per side |
| JTAC settings persistence | Broken | High | `bflib/src/db/cargo.rs` | Save JtacState on troop load/unload |
| Carrier aircraft spawning | Buggy | High | `bflib/src/spawnctx.rs` | Spawn position/timing issues, see Naval section |
| Troop carrier system | Incomplete | Medium | `bflib/src/menu/troop.rs` | Load/unload mechanics need completion |
| Points message spam | Cosmetic | Low | `bflib/src/db/player.rs` | Skip message when `points_change == 0` |
| C-130 internal crates blocking | Design Issue | Medium | `bflib/src/db/cargo.rs` | Internal crates block additional crate spawns |

**Fix for points message spam** (`bflib/src/db/player.rs`):
```rust
// Find the points award function and add:
if points_change != 0 {
    msg!(db, player.ucid, "Points: {} ({:+})", new_total, points_change);
}
// Remove the else branch or unconditional message
```

**Fix for default JTAC laser codes** (`bfprotocols/src/cfg/mod.rs`):
```rust
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct JtacCfg {
    // ... existing fields ...

    /// Default laser code for this side's JTACs (1111-1788)
    #[serde(default = "default_laser_code")]
    pub default_laser_code: u16,
}

fn default_laser_code() -> u16 {
    1688  // Common default
}

// Then in jtac.rs, use cfg.default_laser_code when spawning JTAC
```

---

## Performance Improvements

### Critical Path Optimizations

---

#### 1. Frontline Voronoi Calculation

- **Location**: `bflib/src/db/frontline.rs:96-124`
- **Current complexity**: O(grid_size² × num_objectives)
- **Impact**: At 100×100 grid with 50 objectives = ~500,000 distance calculations
- **Improvement**: Use spatial indexing (R-tree or KD-tree) for nearest-objective lookup
- **Expected gain**: O(grid_size² × log(num_objectives))

**Current code** (simplified):
```rust
for x in 0..grid_width {
    for y in 0..grid_height {
        let pos = grid_to_world(x, y);
        // O(n) scan through all objectives
        let nearest = objectives.iter()
            .min_by_key(|obj| distance_squared(pos, obj.pos));
        grid[x][y] = nearest.side;
    }
}
```

**Optimized implementation**:
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

**Dependencies to add** (`bflib/Cargo.toml`):
```toml
rstar = "0.11"
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

- **Current**: LandCache exists for terrain data
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
| Skip zero-point change messages | Low | Low | `bflib/src/db/player.rs` | Add `if points_change != 0` guard |
| Add default JTAC laser codes | Low | Medium | `bfprotocols/src/cfg/mod.rs` | Add field with serde default |
| Profiling instrumentation | Low | High | `bflib/src/lib.rs` | Add timing around `slow_timed_events` |
| Incremental frontline updates | Medium | High | `bflib/src/db/frontline.rs` | Track changed objectives, update locally |
| Batch message queue processing | Low | Medium | `bflib/src/msgq.rs` | Process N messages per frame instead of 1 |

**Profiling instrumentation example**:
```rust
// In lib.rs, wrap slow_timed_events:

fn run_slow_timed_events(db: &mut Db) -> Result<()> {
    let start = std::time::Instant::now();

    let t0 = std::time::Instant::now();
    tick_logistics(db)?;
    let logistics_time = t0.elapsed();

    let t1 = std::time::Instant::now();
    update_frontline(db)?;
    let frontline_time = t1.elapsed();

    let t2 = std::time::Instant::now();
    update_jtac_contacts(db)?;
    let jtac_time = t2.elapsed();

    // ... other operations ...

    let total = start.elapsed();

    // Log if any operation takes too long
    if total.as_millis() > 100 {
        log::warn!(
            "slow_timed_events took {}ms (logistics: {}ms, frontline: {}ms, jtac: {}ms)",
            total.as_millis(),
            logistics_time.as_millis(),
            frontline_time.as_millis(),
            jtac_time.as_millis(),
        );
    }

    // Publish to perf stats
    db.perf.slow_events_time = total;
    db.perf.logistics_time = logistics_time;
    db.perf.frontline_time = frontline_time;
    db.perf.jtac_time = jtac_time;

    Ok(())
}
```

---

## Technical Debt

### Error Handling (67 `unwrap()` calls)

**Risk Level**: High for production stability

**Key files requiring attention**:

| File | Count | Risk Areas |
|------|-------|------------|
| `bflib/src/db/cargo.rs` | 12+ | Crate operations, map lookups |
| `bflib/src/db/actions.rs` | 8+ | Waypoint calculations, group spawning |
| `bflib/src/admin.rs` | 10+ | Admin command parsing |
| `bflib/src/bg/logpub.rs` | 2 | UTF-8 byte conversion |

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
1. `cargo.rs` - Affects player interactions
2. `actions.rs` - Affects mission spawning
3. `admin.rs` - Affects server administration
4. `logpub.rs` - Affects logging reliability

---

### Data Consistency

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

**Large files needing refactoring**:

| File | Lines | Suggested Split |
|------|-------|-----------------|
| `logistics.rs` | 2082 | `logistics/warehouse.rs`, `logistics/convoy.rs`, `logistics/transfer.rs` |
| `objective.rs` | 1526 | `objective/capture.rs`, `objective/spawning.rs`, `objective/state.rs` |
| `jtac.rs` | 1813 | `jtac/targeting.rs`, `jtac/laser.rs`, `jtac/reports.rs` |

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

| Scenario | Target | Current Status | Test Method | Concerns |
|----------|--------|----------------|-------------|----------|
| Objectives | 100+ | Untested | Create test mission with 100 objectives | Frontline calculation, O(n²) transfers |
| Active units | 1000+ | Untested | Spawn 1000 AI units, measure frame time | JTAC scanning, position updates |
| Concurrent players | 50+ | Untested | Load test with dummy connections | Message rate limiting, slot management |
| Campaign duration | 24h+ | Unknown | Run campaign for extended period | Memory leaks, state bloat, save file growth |
| Save file size | Large campaigns | Unknown | Monitor `.state` file size over time | Load/save performance, disk usage |

**Profiling setup**:
```rust
// Add to lib.rs for basic profiling:

#[cfg(feature = "profiling")]
mod profiling {
    use std::time::{Duration, Instant};
    use std::collections::HashMap;

    thread_local! {
        static TIMINGS: RefCell<HashMap<&'static str, Vec<Duration>>> = RefCell::new(HashMap::new());
    }

    pub fn record(name: &'static str, duration: Duration) {
        TIMINGS.with(|t| {
            t.borrow_mut().entry(name).or_default().push(duration);
        });
    }

    pub fn report() -> String {
        TIMINGS.with(|t| {
            let timings = t.borrow();
            let mut report = String::new();
            for (name, durations) in timings.iter() {
                let avg = durations.iter().sum::<Duration>() / durations.len() as u32;
                let max = durations.iter().max().unwrap_or(&Duration::ZERO);
                report.push_str(&format!("{}: avg={:?}, max={:?}, samples={}\n",
                    name, avg, max, durations.len()));
            }
            report
        })
    }
}

macro_rules! timed {
    ($name:expr, $body:expr) => {{
        let start = std::time::Instant::now();
        let result = $body;
        #[cfg(feature = "profiling")]
        profiling::record($name, start.elapsed());
        result
    }};
}

// Usage:
fn slow_timed_events(db: &mut Db) -> Result<()> {
    timed!("logistics", tick_logistics(db)?);
    timed!("frontline", update_frontline(db)?);
    timed!("jtac", update_jtac_contacts(db)?);
    Ok(())
}
```

---

## Implementation Priority

### Phase 1: Stability & Polish
1. Fix carrier aircraft spawning - High impact for naval operations
2. Add default JTAC laser codes - Quick config fix
3. Fix JTAC settings persistence - Affects user experience
4. Replace critical `unwrap()` calls - Production stability
5. Skip zero-point messages - Quick cosmetic fix

### Phase 2: Performance
1. Add profiling instrumentation - Needed to measure improvements
2. Optimize frontline calculation - Add R-tree spatial index
3. Implement JTAC spatial partitioning - Add spatial grid
4. Expand caching - Position extrapolation, group caching

### Phase 3: Features
1. Complete public Lua API - Enables external tool ecosystem
2. Enhanced ground AI pathfinding - Major gameplay improvement
3. Naval warfare improvements - Expand gameplay options
4. Advanced logistics features - Deeper gameplay

### Phase 4: Polish & Expansion
1. Client-side UI plugin - Better user experience
2. Mission planning tools - Multiplayer coordination
3. Weather effects - Immersion and variety
4. Dynamic campaign events - Replayability

---

## Contributing

When working on items from this roadmap:
1. Create a branch named `feature/<feature-name>` or `fix/<issue-name>`
2. Update this document to reflect current status
3. Add tests where possible
4. Document any new configuration options in the user guide
5. Run `cargo clippy` and fix warnings before submitting

---

*Last updated: 2026-01-19*
