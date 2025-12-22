# Mission Building Guide - Fowl Engine (BFNEXT)

## Naming Convention Reference

### Objective Prefixes
- **O** = Objective (e.g., OABRKOBULETI)
- **G** = Group (e.g., GLOGI)
- **AB** = Airbase (e.g., OABRKOBULETI)
- **FOB** = Forward Operating Base (no slots, refuel/rearm/repair/get crates)
- **LO** = Logistics Objective (e.g., OLOBMukhrani)
- **NB** = Naval Base (fixed port facility)
- **CG** = Carrier Group (mobile naval objective)
- **FAC** = Factory (strategic production facility)

### Coalition Prefixes
- **B** = Blue (starting color)
- **R** = Red (starting color)
- **N** = Neutral (starting color)

### Template Type Codes
- **LR** = Long Range (SAM template)
- **SR** = SHORAD (Short Range Air Defense template)
- **DEP** = Deployable (player-dropped units via crates)

---

## Objective Compositions

### Airbase (AB)
Full military airbase with complete defensive systems:
- LOGI
- AAA x2
- SR x2
- ARMOR
- LR (long range SAM - home base only)

### Forward Operating Base (FOB)
Smaller forward position:
- LOGI
- AAA
- SR (2x for home objectives)
- ARMOR

### Logistics Objective (LO)
Supply hub:
- LOGI
- AAA
- SR
- ARMOR

### Naval Base (NB) **[NEW]**
Fixed port facility for naval operations:
- LOGI
- AAA x2
- SR x2
- ARMOR
- LR (home base)
- **Supplies Carrier Groups**
- **Repairs damaged Carrier Groups**
- **Spawns new Carrier Groups when destroyed**

### Carrier Group (CG) **[NEW]**
Mobile naval task force:
- Carrier (CVN-73 George Washington / Admiral Kuznetsov)
- Escort ships (2-4 destroyers/cruisers)
- **Supply ship (optional)** - Include a unit/group with "SUPPLY" in the name
- **Position persists and updates**
- **Can be damaged and repaired via Naval Base supplies (10 minute repair time)**
- **Can be captured when health reaches 0%**
- **Acts as mobile airbase**
- **F10 arrow drawn from parent Naval Base**
- **Warehouse disabled if supply ship is destroyed**

### Factory (FAC) **[NEW]**
Strategic production facility:
- Static factory buildings (can be bombed)
- AAA
- SR
- Small LOGI presence
- **Produces supplies/tickets over time when active**
- **Destroying reduces enemy production capacity**

---

## Standard Templates

### LOGI (Logistics)
**Blue** | 1 Outpost, 4 HEMTT, 4 M939 Heavy, 2 M249, 1 Vulcan, 1 Bofors 40mm
**Red**  | 1 Outpost, 4 ATZ-10, 4 URAL-375, 2 AK74v3, 1 S-60, 1 ZU-23 emplacement
**Neut** | 6 URAL-375

### AAA (Anti-Aircraft Artillery)
**Blue** | 3 Gepard
**Red**  | 3 Shilka
**Neut** | 3 Shilka

### SR (Short Range Air Defense)
**Blue** | 1 Linebacker Bradley M6, 2 Avengers
**Red**  | 2 SA-13, 1 SA-8
**Neut** | 1 SA-9, 2 SA-8

### LR (Long Range SAM)
**Blue** | Hawk Battery
**Red**  | SA-11 Battery
**Neut** | SA-2 Battery

### ARMOR
**Blue** | ATGM Stryker / IFV M2A2 Bradley / MBT M1A2 Abrams
**Red**  | BMD-1, BMP-3, T-80U
**Neut** | 3 BMP-1

---

## Naval Templates **[NEW]**

### CARRIER (Mission Objectives Only - NOT Deployable)
**Blue** | CVN-73 George Washington (Template: `BCARRIER1`)
**Red**  | Admiral Kuznetsov (Template: `RCARRIER1`)

**IMPORTANT**: These are late-activated mission templates for Carrier Group objectives. Do NOT create deployable carrier actions - carriers should only exist as mission objectives linked to Naval Bases.

### ESCORT
**Blue** | 2x Ticonderoga, 2x Arleigh Burke
**Red**  | 2x Neustrashimy, 2x Moskva

### NAVALAA
**Blue** | 2x Ticonderoga (SAM capability)
**Red**  | 2x Moskva (SAM capability)

### SUPPLY (Carrier Support Ship)
**Blue** | 1x Supply-class fast combat support ship
**Red**  | 1x Boris Chilikin-class replenishment oiler

**IMPORTANT**: Name the group or unit with "SUPPLY" in it (e.g., `BSUPPLY#001`). When destroyed, the carrier group's warehouse becomes disabled (no aircraft spawns, no supplies). Must be repaired to restore warehouse functionality.

### NAVALFARP (Deployable Alternatives)
For deployable naval FARPs, use:
- **Naval FARP Frigate** - Smaller naval vessel with landing pads
- **Naval FARP Destroyer** - Larger naval vessel with multiple landing pads

---

## Infantry Templates

### CRATE
**(DO NOT PUT DEP IN FRONT OF THIS)**
**Blue** | Empty
**Red**  | Empty

### STANDARDTROOP
**Blue** | 2 M249, 1 RPG, 5 M4
**Red**  | 7 AK74v3, 1 RPG

### ATTROOP (Anti-Tank)
**Blue** | 5 RPG, 3 M249
**Red**  | 5 RPG, 3 AK74v3

### STINGERTROOP
**Blue** | 2 M249, 2 Stinger, 2 Stinger C2

### RIGLATROOP
**Red**  | 2 AK74v3, 2 Igla-S, 2 Igla C2

### MORTARTROOP
**Blue** | 3 M249, 5 2B11 120mm
**Red**  | 3 AK74v3, 5 2B11 120mm

---

## Deployable Templates

*Format: DEP + [COLOR if shared] + TYPE*
*Example: DEPBAMMO (shared), DEPRoland (Blue-only)*

### Supply & Support
**DEPBAMMO** | M939 Heavy
**DEPRAMMO** | URAL-375

### Short Range SAM
**DEPRoland** | SAM Roland ADS (Blue)
**DEPSA15** | SA-15 TOR (Red)
**DEPAvenger** | SAM Avenger Stinger (Blue)
**DEPSA8** | SA-8 TEL (Red)
**DEPSA13** | SA-13 Strela TEL (Red)
**DEPLinebacker** | SAM Linebacker Bradley M6 (Blue)
**DEPTunguska** | SA-19 Tunguska (Red)

### AAA
**DEPGepard** | Gepard (Blue)
**DEPShilka** | ZSU-23-4 Shilka (Red)
**DEPVulkan** | Vulcan M163 (Blue)
**DEPZU23** | ZU-23 Emplacement (Red)

### Artillery
**DEPFirtina** | SPH T155 Firtina 155mm (Blue)
**DEPMsta** | 2S19 Msta 152mm (Red)

### Armor
**DEPBradley** | M2A2 Bradley (Blue)
**DEPBMP3** | BMP-3 (Red)
**DEPLeopard** | Leopard-2A6M (Blue)
**DEPT72** | T-72B (Red)

### EWR (Early Warning Radar)
**DEPFPS117** | AN/FPS-117 Radar + ECS (Blue)
**DEP1L13** | 55G6 (Red)

### Long Range SAM
**DEPHawk** | Standard HAWK Battery (Blue)
**DEPSA11** | SA-11 Battery (Red)
**DEPSA6** | SA-6 Battery (Red)

### FARP
**DEPBFARP** Components:
- Bofors 40mm
- M978 HEMTT
- M939 Heavy
- 2 M249 inf
- Invisible FARP: **DEPBFARPPAD**
- FARP Fuel Depot: **DEPBFARPFUEL**
- FARP Ammo Depot: **DEPBFARPAMMO**
- FARP Tent: **DEPBFARPTENT**

**DEPRFARP** Components:
- S-60
- ATZ-10
- URAL-375
- 2 AK74v3
- Invisible FARP: **DEPRFARPPAD**
- FARP Fuel Depot: **DEPRFARPFUEL**
- FARP Ammo Storage: **DEPRFARPAMMO**
- FARP Tent: **DEPRFARPTENT**

---

## Factory Templates **[NEW]**

### FACAAA (Factory Air Defense)
**Blue** | 2 Gepard
**Red**  | 2 Shilka

### FACSR (Factory SHORAD)
**Blue** | 1 Avenger
**Red**  | 1 SA-13

### FACLOGI (Factory Logistics)
**Blue** | 1 HEMTT, 1 M939 Heavy, 1 M249
**Red**  | 1 ATZ-10, 1 URAL-375, 1 AK74v3

### FACBUILDING (Factory Structures)
**Blue/Red/Neut** | Static factory buildings (workshop, chimney, warehouse cluster)

---

## F10 Map Color Coding **[NEW]**

### Objective Health Status
- 🟢 **Green** = Healthy (80-100% HP, good supply)
- 🟡 **Yellow** = Damaged (40-79% HP, or low supply)
- 🔴 **Red** = Critical (0-39% HP, or very low supply/under attack)
- ⚫ **Gray** = Destroyed/Inactive

### Coalition Ownership
- 🔵 **Blue** = Blue coalition
- 🔴 **Red** = Red coalition
- ⚪ **White/Gray** = Neutral

### Connection Arrows
- **Logistics Hub → Objectives** = Land supply routes
- **Naval Base → Carrier Group** = Naval supply routes (updates as carrier moves)
- Arrow color reflects supply status/health

---

## Mission Compilation Steps

### Prerequisites
- Rust toolchain installed
- Visual Studio Build Tools installed

### Build Process

1. **Verify Lua Environment**
   ```powershell
   echo $env:LUA_LIB
   ```
   Should point to: `*\GitHub\repo-15GI` or your repository path

2. **Setup Build Environment**
   ```powershell
   .\setup-build.ps1
   ```
   *(Takes a few seconds - sets LUA_LIB, LUA_LINK, LUA_LIB_NAME)*

3. **Compile Release Build**
   ```powershell
   cargo build --release
   ```
   *(Takes several minutes on first build, ~3 minutes on subsequent builds)*

   **Note**: This builds the entire workspace. To build only bflib:
   ```powershell
   cargo build --release --package=bflib
   ```

4. **Copy DLL to Mission Folder**
   ```powershell
   cp target/release/bflib.dll '.\miz'
   ```
   Or use your specific mission folder path

### Post-Build Fix (If Needed)

Run payload fix command:
```powershell
luae payloadfix.lua "C:\Users\Adam\Documents\GitHub\bfnext\miz\Scenarios\80s\caucasus\Caucasus 0.2.miz" "C:\Users\Adam\Documents\GitHub\bfnext\miz\Scenarios\80s\caucasus\BFConfig.json"
```

---

## Example Objectives Created

### Existing Objectives
- **OABRBeslan** - Red Airbase at Beslan
- **OFORVLADIKAVKAZ** - Red FOB at Vladikavkaz
- **OFONKAZBEGI** - Neutral FOB at Kazbegi *(REMOVE TREES)*
- **OFOBZhinvali** - Blue FOB at Zhinvali
- **OABTbilisi** - Blue Airbase at Tbilisi
- **OLOBMukhrani** - Blue Logistics Hub at Mukhrani
- **OLORDigora** - Red Logistics Hub at Digora

### Example New Objectives **[NEW]**
- **ONBBBatumi** - Blue Naval Base at Batumi
- **OCGBGeorgeWashington** - Blue Carrier Group (CVN-73)
- **ONBRNovorossiysk** - Red Naval Base at Novorossiysk
- **OCGRKuznetsov** - Red Carrier Group (Admiral Kuznetsov)
- **OFACBRustavi** - Blue Factory at Rustavi
- **OFACRGrozny** - Red Factory at Grozny

---

## Mission Settings

### Preferred Start Time
**05:30 AM** (Summer)

### Preferred Weather
**High Scattered 3**

---

## Implementation Notes

### Carrier Groups
- **Position Tracking**: Carrier positions automatically tracked and persisted to database
- **Waypoint Control**: Players can set carrier waypoints via F10 menu (CarrierWaypoint action)
- **Timed Repair** (NEW): Repair takes configurable time (default: 600s = 10 minutes)
  - Delivers carrier repair crate to Naval Base to start repair
  - Progress notifications every 5 minutes
  - Message when complete: "{Carrier} has been fully repaired and is operational"
  - Time configurable in BFConfig.json `carrier.repair_time` (in seconds)
- **Repair Cost**: Requires supplies from parent Naval Base (default: 5000 supplies)
- **Respawn Cost**: Requires supplies from parent Naval Base (default: 15000 supplies)
- **Capture Mechanics** (NEW):
  - Carrier can be captured when health and logi both reach 0%
  - Requires enemy units within 10km to trigger capture
  - Captured carrier starts at 50% health, 100% logistics
  - All carrier groups and aircraft transfer to new owner
  - Enemy can use captured carrier's warehouse
  - Messages sent to both sides when capture occurs
- **Supply Ship Dependency** (NEW):
  - Include a ship with "SUPPLY" in group/unit name (e.g., `BSUPPLY#001`)
  - When supply ship destroyed: logistics drops to 0%, warehouse disabled
  - No aircraft spawns, no supplies until carrier is repaired
  - Repairing carrier also repairs/respawns supply ship
- **Movement Speed**: Configurable in BFConfig.json (default: 5.0 m/s ≈ 10 knots)
- **Template Naming**: Carrier template group must be named with `BCARRIER`/`RCARRIER` prefix and late-activated in mission file
- **NOT Deployable**: Carrier Groups are mission objectives, NOT deployable units via actions menu
  - Do NOT use "naval-farp-carrier" deployable action (conflicts with Carrier Group objectives)
  - Use "naval-farp-frigate" or "naval-farp-destroyer" for deployable naval FARPs instead

### Naval Bases
- **Supply Connection**: Automatically connects to carrier groups with matching parent_naval_base ID
- **F10 Arrows**: Blue/Red arrows drawn to show supply routes (color indicates supply status)
- **Composition**: Same as Airbase (LOGI, AAA x2, SR x2, ARMOR, LR for home bases)

### Factories
- **Production**: Auto-generates supplies every production_interval seconds (default: 600s = 10min)
- **Production Rate**: Configurable per factory (default: 100 supplies per tick)
- **Requirements**: Must have health > 0, logi > 0, and not be neutral to produce
- **Destruction Impact**: Destroying enemy factories reduces their supply production capacity

### F10 Visual System
- **Status Boxes**: Positioned at radius × 1.3 to the right of each objective
- **Real-time Updates**: Health/Supply/Fuel percentages update automatically
- **Color Coding**:
  - Green (80-100%): Healthy
  - Yellow (40-79%): Damaged/Low supplies
  - Red (1-39%): Critical
  - Gray (0%): Destroyed
- **Arrow Updates**: Supply route arrows automatically reposition when carriers move

### Territory Zone Visualization **[NEW]**
Voronoi-based territory zones show areas of control on the F10 map:
- **Semi-transparent shading**: Red and Blue zones show controlled territory
- **Automatic updates**: Zones recalculate when objectives change ownership
- **Performance optimized**: Configurable grid resolution and update frequency
- **Neutral zones**: Neutral territory is not shaded

#### Configuration
```json
"frontline": {
  "enabled": true,
  "update_on_objective_change_only": true,
  "samples_per_boundary": 100,
  "territory_zone_alpha": 0.15
}
```

#### Parameters
- **enabled**: `true` to enable territory visualization
- **update_on_objective_change_only**: Only recalculate when objectives change owner (recommended for performance)
- **samples_per_boundary**: Grid resolution (50-200, higher = finer detail but slower)
- **territory_zone_alpha**: Transparency (0.0-1.0, 0.1-0.3 recommended for subtle shading)

### Supply Convoy System **[NEW]**

The convoy system adds physical truck-based logistics for forward/contested objectives:

#### **How It Works**
- **Secure Rear Areas** (`LOGISTICS_DETACHED = false`): Instant, automatic supply transfers from hubs
- **Forward/Contested Areas** (`LOGISTICS_DETACHED = true`): Physical truck convoys transport supplies
- **Separate Convoys**: Fuel convoys and weapons convoys spawn separately
- **Destroyable**: If enemy intercepts and destroys convoy, supplies are lost
- **Visible Gameplay**: Players can see, protect, or hunt convoys on the map

#### **Setting Up Detached Logistics**
In your mission trigger zones, add this property to forward objectives:
```lua
properties = {
  ["LOGISTICS_DETACHED"] = true,
  -- Other properties...
}
```

Objectives with `LOGISTICS_DETACHED = true`:
- ✅ Receive supplies via physical truck convoys (if enabled)
- ❌ Do NOT receive instant automatic supplies
- ⚠️ Are vulnerable to supply interdiction

Objectives with `LOGISTICS_DETACHED = false` or unset:
- ✅ Receive instant automatic supplies from nearest hub
- ✅ Safe from convoy interdiction
- 🏭 Represents secure rear logistics (railways, established supply lines)

#### **Convoy Gameplay**
- **Escort Missions**: Protect your convoys from enemy attack
- **Interdiction**: Hunt enemy supply convoys to cut off forward bases
- **Strategic Impact**: Destroying convoys denies supplies to enemy frontline positions
- **Visual Feedback**: See convoy trucks driving between hubs and objectives
- **Points**: Players earn points for destroying enemy convoys

#### **Convoy Configuration Example**
```json
"warehouse": {
  "hub_max": 10,
  "airbase_max": 2,
  "tick": 10,
  "ticks_per_delivery": 24,
  "supply_transfer_size": 25,

  "supply_transfer_fuel_crate": {
    "Red": {
      "name": "Fuel Transfer",
      "weight": 2000,
      "required": 1,
      "pos_unit": null,
      "max_drop_height_agl": 10,
      "max_drop_speed": 13
    },
    "Blue": {
      "name": "Fuel Transfer",
      "weight": 1200,
      "required": 1,
      "pos_unit": null,
      "max_drop_height_agl": 10,
      "max_drop_speed": 13
    }
  },

  "supply_transfer_weapons_crate": {
    "Red": {
      "name": "Weapons Transfer",
      "weight": 1500,
      "required": 1,
      "pos_unit": null,
      "max_drop_height_agl": 10,
      "max_drop_speed": 13
    },
    "Blue": {
      "name": "Weapons Transfer",
      "weight": 800,
      "required": 1,
      "pos_unit": null,
      "max_drop_height_agl": 10,
      "max_drop_speed": 13
    }
  },

  "convoy": {
    "enabled": true,
    "truck_template": {
      "Red": "Ural-375",
      "Blue": "M939"
    },
    "trucks_per_convoy": 5,
    "speed_kph": 60.0,
    "spawn_interval_ticks": 2,
    "max_concurrent_convoys": 10,
    "delivery_distance": 500.0,
    "check_interval_secs": 10
  }
}
```

#### **Convoy Configuration Parameters**
- **enabled**: `true` to enable convoy system, `false` for instant transfers everywhere
- **truck_template**: Vehicle type per side (must exist in DCS)
  - Red: `"Ural-375"`, `"GAZ-66"`, `"KAMAZ Truck"`
  - Blue: `"M939"`, `"M-818"`, `"HEMTT"`
- **trucks_per_convoy**: Number of trucks (1-10 recommended)
- **speed_kph**: Convoy speed in kilometers per hour (30-80 recommended)
- **spawn_interval_ticks**: How many logistics ticks between convoy spawns (1-5 recommended)
- **max_concurrent_convoys**: Maximum convoys in transit per side (5-20 recommended)
- **delivery_distance**: Distance from destination to count as "delivered" in meters (300-1000)
- **check_interval_secs**: How often to check convoy status in seconds (5-30)

#### **C-130 Physical Cargo System** **[UPDATED]**
C-130s now use a physical cargo system with visible crates:
- **Physical Crates**: Crates spawn as actual objects in C-130 cargo bay
- **Airdrop System**: Crates can be dropped at low altitude
- **Multi-Crate Support**: Multi-piece deployables (like SAM batteries) work correctly

**Available Crate Types:**
- **Fuel Transfer Crate**: Only transfers fuel/liquids to destination objective
- **Weapons Transfer Crate**: Only transfers weapons/equipment to destination objective
- **Carrier Repair Crate**: Initiates carrier group repair at Naval Base
- **Deployable Crates**: Standard deployable equipment crates

Players can use C-130s or helicopters to:
- Resupply forward bases faster than convoys
- Bypass destroyed convoy routes
- Emergency resupply during heavy combat
- Deliver repair crates to Naval Bases for carrier repairs

#### **Tactical Considerations**
- **Forward Base Placement**: Bases with `LOGISTICS_DETACHED = true` are higher risk/reward
- **Route Security**: Plan convoy routes through safe territory
- **Air Interdiction**: CAS/Attack helicopters can hunt enemy convoys
- **Timing**: Convoys spawn every N logistics ticks - plan attacks accordingly
- **Combined Arms**: Coordinate convoy escorts with air cover

### Configuration
All new features can be configured in BFConfig.json:
```json
{
  "factory": {
    "production_rate": 100,
    "production_interval": 600
  },
  "carrier": {
    "repair_cost": 5000,
    "respawn_cost": 15000,
    "movement_speed": 5.0,
    "repair_time": 600
  },
  "warehouse": {
    "supply_transfer_fuel_crate": {
      "Red": { "name": "Fuel Transfer", "weight": 2000, "required": 1 },
      "Blue": { "name": "Fuel Transfer", "weight": 1200, "required": 1 }
    },
    "supply_transfer_weapons_crate": {
      "Red": { "name": "Weapons Transfer", "weight": 1500, "required": 1 },
      "Blue": { "name": "Weapons Transfer", "weight": 800, "required": 1 }
    },
    "carrier_repair_crate": {
      "Red": { "name": "Carrier Repair", "weight": 3000, "required": 1 },
      "Blue": { "name": "Carrier Repair", "weight": 2500, "required": 1 }
    },
    "convoy": {
      "enabled": true,
      "truck_template": {
        "Red": "Ural-375",
        "Blue": "M939"
      },
      "trucks_per_convoy": 5,
      "speed_kph": 60.0
    }
  },
  "frontline": {
    "enabled": true,
    "update_on_objective_change_only": true,
    "samples_per_boundary": 100,
    "territory_zone_alpha": 0.15
  },
  "points": {
    "award_kill_points": true
  }
}
```

**Carrier Configuration Parameters:**
- `repair_cost`: Supply cost to initiate repair (default: 5000)
- `respawn_cost`: Supply cost to spawn new carrier (default: 15000)
- `movement_speed`: Carrier speed in m/s (default: 5.0 ≈ 10 knots)
- `repair_time`: Time in seconds to complete repair (default: 600 = 10 min)
  - 300 = 5 minutes
  - 600 = 10 minutes (recommended)
  - 900 = 15 minutes
  - 1200 = 20 minutes

---

## Quick Reference: Objective Type Codes

| Code | Type | Mobile | Description |
|------|------|--------|-------------|
| AB | Airbase | No | Full military airbase with slots |
| FOB | Forward Operating Base | No | Field base, no slots |
| LO | Logistics Objective | No | Supply hub |
| NB | Naval Base | No | Port facility, supports carriers |
| CG | Carrier Group | **Yes** | Mobile carrier task force |
| FAC | Factory | No | Production facility |

---

## Changelog

### Version 2.3 (2025-12-22)
**Added: C-130 Physical Cargo System & Territory Visualization**
- **C-130 Physical Cargo**: C-130s now spawn physical crate objects in their cargo bay
  - Crates can be unloaded at objectives
  - Supports deployable crates, fuel transfer, weapons transfer, and carrier repair crates
  - Enhanced multi-crate deployable support (e.g., multi-piece SAM systems)
- **Split Supply Transfer Crates**: Replaced single supply transfer crate with separate types
  - **Fuel Transfer Crate**: Only transfers fuel/liquids to destination
  - **Weapons Transfer Crate**: Only transfers equipment/weapons to destination
  - **Carrier Repair Crate**: Dedicated crate for initiating carrier repairs
  - Configuration uses `supply_transfer_fuel_crate` and `supply_transfer_weapons_crate`
- **Territory Zone Visualization** (NEW): Voronoi-based territory zones on F10 map
  - Shows areas of control for each side with semi-transparent shading
  - Configurable via `frontline` section in BFConfig.json
  - Updates automatically when objectives change ownership
  - Performance-optimized with configurable grid resolution
- **Kill Points Toggle**: New `award_kill_points` config option
  - Set to `false` to disable points for air/ground kills
  - Useful for cooperative scenarios
- **Improved Warehouse Initialization**: Better logging and error messages for warehouse setup
  - Clearer error messages when supply sources are misconfigured
  - Proper zeroing of warehouse items for carriers

**Fixed:**
- C-130 cargo priority correctly checks all crate types (fuel, weapons, carrier repair, deployable)
- Multi-crate deployables now work correctly with C-130 airdrop system
- Supply transfer warehouse initialization now properly zeros items not in objective config
- Improved error handling for missing vehicle threat distance and life type configs

### Version 2.2 (2025-12-19)
**Added: Advanced Carrier Group Mechanics**
- **Timed Carrier Repair**: Configurable repair duration (default: 10 minutes)
  - Progress notifications every 5 minutes
  - Completion message when repair finishes
  - Configurable via `carrier.repair_time` in BFConfig.json
- **Carrier Capture System**: Carriers can change ownership when destroyed
  - Requires enemy units within 10km
  - Captured carrier starts at 50% health
  - All groups and aircraft transfer to new owner
- **Supply Ship Dependency**: Optional supply ship mechanic
  - Name ship/group with "SUPPLY" (e.g., `BSUPPLY#001`)
  - Warehouse disabled when supply ship destroyed
  - Must repair carrier to restore warehouse functionality
- **Fixed**: Carrier groups now properly initialize with naval units
- **Fixed**: Removed HP/Sup/Fuel text boxes from F10 map (kept in objective label)

### Version 2.1 (2025-12-18)
**Added: Supply Convoy System**
- Physical truck-based logistics for forward objectives
- `LOGISTICS_DETACHED` trigger zone property support
- Separate fuel and weapons convoy system
- Destroyable convoys with supply loss mechanics
- Split C-130/helicopter crates into fuel and weapons variants
- Comprehensive convoy configuration options

### Version 2.0 (2025-12-18)
- Added Carrier Groups, Naval Bases, and Factories
- F10 visual system with color-coded status boxes
- Position tracking for mobile objectives

---

*Document Version: 2.3*
*Last Updated: 2025-12-22*