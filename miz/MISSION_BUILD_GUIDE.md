# Mission Building Guide - Fowl Engine (BFNEXT)

## Naming Convention Reference

### Objective Prefixes
- **O** = Objective (e.g., OABRKOBULETI)
- **G** = Group (e.g., GLOGI)
- **AB** = Airbase (e.g., OABRKOBULETI)
- **FO** = Forward Operating Base (no slots, refuel/rearm/repair/get crates)
- **LO** = Logistics Objective (e.g., OLOBMukhrani)
- **NB** = Naval Base (fixed port facility)
- **CG** = Carrier Group (mobile naval objective)
- **FAC** = Factory (strategic production facility)
- **CC** = Command Center (IADN network node — see [Command Center](#command-center-cc-new))

### Coalition Prefixes
- **B** = Blue (starting color)
- **R** = Red (starting color)
- **N** = Neutral (starting color)

### Template Type Codes
- **LR** = Long Range (SAM template)
- **SR** = SHORAD (Short Range Air Defense template)
- **DEP** = Deployable (player-dropped units via crates)

### Special SAM Site Naming **[NEW]**
Special SAM sites use a *different* convention from everything else in this
guide — see [Special SAM Sites](#special-sam-sites-new) below. Group names
are `<Location> - <Label>` (no `B`/`R`/`N` prefix); the group's coalition
placement in the editor is what determines its starting owner instead.

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
- **Can be damaged and repaired** - via Naval Base supplies, air-dropped/helo repair crates (stack to speed up, ~30 min base), or the Repair/Respawn Carrier actions
- **Can be captured** - disable it (logistics to 0 by sinking escorts/supply ship) then take its parent Naval Base; the carrier group then flips to the captor as *their* coalition's ships
- **Acts as mobile airbase**
- **F10 arrow drawn from parent Naval Base**
- **Warehouse disabled if supply ship is destroyed**

**Co-located task forces (required for capture to work):** Each CG objective must have **BOTH** coalitions' carrier task forces placed at it, all late-activated, all with **distinct group AND unit names**. bflib picks which one is live from the objective owner and swaps them on capture (no dynamic respawn — that used to collide on deck-airbase unit names). Naming convention drives the assignment:

| Group name | Belongs to | Role |
|---|---|---|
| `RCARRIER`  | the Red-home CG objective  | Red's own carrier (live while Red owns it) |
| `BCARRIER2` | the Red-home CG objective  | Blue's reserve, takes over if Blue captures it |
| `BCARRIER`  | the Blue-home CG objective | Blue's own carrier (live while Blue owns it) |
| `RCARRIER2` | the Blue-home CG objective | Red's reserve, takes over if Red captures it |

Rule: a group named `<S>CARRIER` (no trailing digit) belongs to the CG objective whose home side is `S` (from its name, e.g. "Red Strike Group"); `<S>CARRIER2` (trailing digit) belongs to the *other* side's CG objective. Group name must contain `CARRIER`. Place each side's reserve at (or near) the same spot as that objective's live carrier.

### Factory (FAC) **[NEW]**
Strategic production facility:
- Static factory buildings (can be bombed)
- AAA
- SR
- Small LOGI presence
- **Produces supplies/tickets over time when active**
- **Destroying reduces enemy production capacity**

### Special SAM Site (SAM) **[NEW]**
Hidden, map-fixed IADS installation — not built via a trigger zone like the
other objective types above. See [Special SAM Sites](#special-sam-sites-new)
below for the full workflow.
- No slots, no logistics connection, always spawned (never culled)
- **Hidden from the F10 map and dashboard** until the enemy coalition
  physically finds and destroys it
- Capturable by ground troops like any other objective
- Equipment and starting owner come entirely from a dedicated mission
  editor template, generated into campaign config via `bftools special-sam`

### Command Center (CC) **[NEW]**
IADN network node — a pure trigger-zone objective like FOB/LO, no DCS
airbase/pad needed and no aircraft slots:
- LOGI (optional — a light garrison is enough, it isn't a supply hub)
- AAA / SR for defense (recommended — losing it degrades your own IADS)
- Captured/destroyed exactly like any standard objective (health + troops,
  no special mechanic)

At mission start, **every Special SAM Site auto-links to its nearest
same-coalition Command Center** (no manual linking needed — closest wins,
same distance-based auto-link Carrier Groups use for their parent Naval
Base). A SAM site whose linked Command Center is alive and still friendly
gets full IADN network cueing (shares detections with every other sensor
on that coalition, powers its radar up only for a real threat, goes dark
automatically if an anti-radiation missile is inbound). Lose the Command
Center — destroyed, captured, or the SAM was never in range of one — and
that SAM site falls back to plain always-on DCS AI, same as if none of
this existed. Place Command Centers to actually cover your SAM sites,
not as an afterthought; a SAM site more than one Command Center's
coverage radius from any friendly CC will simply never link to one.

See [IADN](#iadn-integrated-air-defence-network-new) below for the full
mechanic (cueing, EMCON, HARM defense) and its config options.

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
**Blue** | CVN-73 George Washington - groups `BCARRIER` (at the Blue CG objective) + `RCARRIER2` (Red's reserve, at the Blue CG objective)
**Red**  | Admiral Kuznetsov - groups `RCARRIER` (at the Red CG objective) + `BCARRIER2` (Blue's reserve, at the Red CG objective)

**IMPORTANT**:
- Late-activated mission templates for Carrier Group objectives only. Do NOT create deployable carrier actions.
- All four groups need **distinct group AND unit names** — copying a group in the ME keeps its unit names, which will collide on the carrier deck airbases. Rename the units (e.g. `RCARRIER2-1`..`RCARRIER2-4`).
- See the **Carrier Group (CG)** section above for the co-located task-force layout and the `<S>CARRIER` / `<S>CARRIER2` naming rule that decides which objective a group attaches to.

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

## Special SAM Sites **[NEW]**

Unlike every other objective type in this guide, Special SAM Sites are
**not** built from an `O`-prefixed trigger zone. They're hidden, map-fixed
IADS installations meant to be found and destroyed by players, not seen on
the F10 map ahead of time — so they live entirely in the campaign config
(`special_sam_sites`), generated from a *dedicated* mission editor template
via `bftools special-sam`, not from the main mission file.

### Building the template

1. In a separate `.miz` (or a dedicated area of one), place each site's
   vehicles/statics as **one DCS group**, under whichever coalition
   (Red/Blue) the site should start owned by. All of a site's equipment
   must be in that one group — the group boundary is exactly what defines
   the site.
2. Name the **group itself** (not the individual units) `<Location> - <Label>`,
   e.g. `Hayjanah - SA-1`. No `B`/`R`/`N` prefix — the coalition you placed
   the group under is what determines the starting owner, not the name text.
3. Every group matching that two-part naming pattern under Red or Blue
   becomes its own site. The opposite coalition's mirror (same unit
   types/positions/headings) is synthesized automatically, so the site can
   flip ownership when captured — you only ever place one side by hand.

### Generating the config

```bash
bftools.exe special-sam \
  --template "path/to/special-sam-template.miz" \
  --output special_sam_sites.json \
  --merge-into "path/to/campaign-config.json"
```

`--merge-into` writes the generated sites straight into the target
config's `special_sam_sites` array in place (with a timestamped backup of
the previous file); omit it to just inspect the standalone
`special_sam_sites.json` output first.

**Capture radius** is shared by every special SAM site (they have no
mission-editor trigger zone of their own to derive one from) — set it once
via the top-level `special_sam_capture_radius_m` config field, not per
site.

### Things to double-check before merging

- **One group per site.** If a site's units end up spread across more than
  one DCS group (e.g. from copy-pasting), only the group actually named
  `<Location> - <Label>` gets picked up — the rest are silently skipped.
- **Group name, not unit names.** Unit-level names inside the group are
  ignored entirely; only the group's own name is read.
- Sites with only 1-2 units after generation are usually a sign of an
  incomplete/mis-scoped group in the editor, not an intentional
  single-launcher site — worth a second look before merging.

---

## IADN (Integrated Air Defence Network) **[NEW]**

A native (no external Lua framework) system that makes SAM search radars
behave intelligently instead of just running DCS's bare default AI. Three
parts, all driven by the same underlying multi-sensor track fusion the EWR
system already runs every tick:

### SAM cueing + EMCON
Every tick, each SAM search radar checks the fused, multi-sensor track
picture for its coalition (not just what its own radar alone could see)
for a confirmed hostile within range. No qualifying threat → radar goes
dark (`AlarmState::Green`, doesn't light up for nothing). A qualifying
threat → radar comes up (`AlarmState::Auto`) and DCS's own native SAM
engagement logic takes it from there.

**⚠ Required per SAM radar entry**: `ground_radar_ewrs` normally guesses
whether a unit is a SAM search radar or a naval radar from
`aspect_half_angle` (`Some` → SAM, `None` → naval) — but most real SAM
search radars are correctly configured omnidirectional (`aspect_half_angle:
null`, since they physically rotate/scan 360°), which means that guess
gets them wrong and silently excludes them from cueing, EMCON, HARM
defense, and layered radar entirely. Set
`"sensor_type_override": "SamSearchRadar"` explicitly on every SAM search
radar unit type's entry to fix this — see `miz/SAMPLE_CFG.json`'s
`ground_radar_ewrs` block, where every genuine SAM entry (Hawk, S-300,
Patriot, Tor, Osa, Roland, Buk, etc.) already has it set; standalone EWRs
and ship radars don't need it.

### Command Center dependency
This smart behavior is **only** available to a SAM site whose nearest
friendly [Command Center](#command-center-cc-new) is alive and still
friendly. Lose that link and the site falls straight back to plain
always-on DCS AI (`AlarmState::Auto`, no EMCON) — it isn't left stuck in
whatever state it was last forced into.

### HARM / anti-radiation missile defense
If an enemy fires a weapon whose DCS type name is listed in
`iadn.anti_radiation_weapons`, it's tracked in flight. One that comes
within `iadn.harm_defense_radius_m` of a live SAM search radar on the
threatened side forces that site dark for `iadn.harm_defense_cooldown_secs`
— overriding the normal cueing decision, survival first. This is a
proximity heuristic ("an ARM launched near an active radar might be
targeting it"), not true seeker/guidance simulation, and it applies
whether or not the site is currently networked to a Command Center.

### Engagement doctrine (hysteresis)
Raw threshold-based cueing would make sites flicker between hot/dark every
tick and snap to Auto in perfect lockstep across a network — both obvious
"it's a script" tells. Two smoothing rules fix that:
- **Reaction delay**: the first time a site sees a qualifying cue, it waits
  a random `0..=iadn.reaction_delay_max_secs` seconds (rolled per site, per
  activation) before actually going hot, so a cluster of networked sites
  reacts staggered, not simultaneously.
- **Minimum engagement dwell**: once hot, a site stays hot for at least
  `iadn.min_hot_dwell_secs` even if the cue drops in the meantime, so it
  gets a real chance to engage instead of flickering dark the instant track
  quality dips for a moment.

Going dark under a HARM threat, or falling back to Auto when the Command
Center link is lost, both bypass this doctrine entirely (survival and
"not networked" are unconditional) and reset it, so the site re-enters the
reaction-delay process fresh the next time it reacquires a normal cue.

### Layered search / tracking radar
Real systems like the SA-10 and Patriot keep a continuously-sweeping
search radar running while the higher-exposure tracking/engagement radar
only powers up close to actually firing. To get this behavior on a SAM
site, tag its **search radar unit type** with `UnitTag::SearchRadar` and
its **tracking radar unit type** with `UnitTag::TrackRadar` in
`unit_classification` (see existing Hawk/SA-3 examples in the sample
config — this tagging already existed for kill-scoring, IADN just gives it
a second use). Search radar mirrors the site's hot/dark state; tracking
radar only emits once a cue is within `iadn.track_radar_range_fraction` of
the search radar's own detection range. A site with no unit tagged either
role is unaffected — this is an optional extra control layer on top of
the group-level `AlarmState`, not a replacement for it.

### Point defense
A short-range gun/missile system (Pantsir, Shilka, Tor, etc.) co-located
in the **same DCS group** as the site's main search radar can protect it
from anti-radiation missiles. Tag its unit type with `UnitTag::EngagesWeapons`
in `unit_classification` (already applied to `ZSU-23-4 Shilka` and
`CHAP_PantsirS1` in the live config as an example) and it stays
emission-on unconditionally — including while the rest of the group is
forced dark by [HARM defense](#harm--anti-radiation-missile-defense) — so
it can keep sensing and DCS's own native point-defense AI can engage an
inbound ARM. `AlarmState` is necessarily group-wide (DCS has no per-unit
version of it), so this is what lets the main radar go dark to deny the
missile a target while the point-defense unit stays alert; this code only
keeps its sensor live, the actual intercept is native DCS AI behavior, the
same as C-RAM/Pantsir shooting down incoming ordnance in any other DCS
mission. A group with no unit tagged `EngagesWeapons` is unaffected.

### Jamming / ECM
Tag a unit type (dedicated EW aircraft, or a self-protection-jamming
airframe) with `UnitTag::Jammer` in `unit_classification`. While one is
airborne, every enemy radar donor within `iadn.jamming_range_m` gets its
detection probability degraded — full effect (scaled by
`iadn.jamming_detection_penalty` and that donor's own
`ecm_susceptibility`, configured per radar type same as other radar
physics) at 0m from the jammer, falling off linearly to none at
`jamming_range_m`. This degrades detection, it does not grant stealth —
close enough, a donor still sees through it.

### Config reference (`iadn` block)
| Field | Default | Meaning |
|-------|---------|---------|
| `track_association_radius_m` | 3000 | Radius within which two sensor detections fuse into one track |
| `detection_snr_threshold` | 0.5 | Minimum signal quality (0–1) to register a detection at all |
| `track_stale_secs` | 60 | Seconds with no detection before a track is marked stale |
| `track_drop_secs` | 120 | Seconds before a stale track is dropped entirely |
| `sam_cue_enabled` | true | Master switch for SAM cueing + EMCON |
| `sam_cue_confidence_threshold` | 0.4 | Minimum fused-track confidence before a SAM is allowed to engage it |
| `anti_radiation_weapons` | *(empty — must configure)* | DCS weapon type names treated as ARMs, e.g. `AGM_88C`, `Kh25MPU`, `Kh58Ushke`, `ALARM` |
| `harm_defense_radius_m` | 20000 | Distance from a tracked ARM within which a SAM site is considered threatened |
| `harm_defense_cooldown_secs` | 20 | How long a threatened site stays forced dark |
| `min_hot_dwell_secs` | 15 | Minimum time a site stays hot once activated |
| `reaction_delay_max_secs` | 4 | Max random delay before a site acts on a fresh cue |
| `track_radar_range_fraction` | 0.5 | Fraction of search-radar range within which tracking radar (if tagged) activates |
| `jamming_enabled` | true | Master switch for the jamming/ECM mechanic |
| `jamming_range_m` | 40000 | Distance from a Jammer-tagged unit within which detection is degraded |
| `jamming_detection_penalty` | 0.5 | Detection-probability multiplier at full jamming exposure |

`anti_radiation_weapons` ships empty — HARM defense silently does nothing
until you populate it with the actual weapon type names your theater's
loadouts use. See `miz/SAMPLE_CFG.json`'s `iadn` block for a populated
example.

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

### Generating the Final Mission (bftools) **[UPDATED]**

`bftools miz` merges your base mission with per-slot payloads/Link-16
assignments (from a weapon template) and warehouse/dynamic-spawn config —
this is the actual current build step; there is no separate payload-fix
script anymore.

```powershell
cargo build --release --package=bftools
.\target\release\bftools.exe miz `
  --output final.miz --base base.miz `
  --weapon weapons.miz --options options.miz `
  --warehouse warehouse.miz
```

If you're adding or updating Special SAM Sites, run `bftools special-sam`
too — see [Special SAM Sites](#special-sam-sites-new) above.

**Warehouses:** `bftools miz` forces **weapons and fuel to limited** on every
airbase and every ship warehouse (carriers included) regardless of what the
inventory template or editor has set — bflib is the sole authority for
weapons/fuel stock and fills each warehouse to capacity at init. Land airbase
aircraft (dynSpawn / `unlimitedAircrafts`) is left as templated; **ship
warehouses get `unlimitedAircrafts` cleared** so bflib can read the airframes
actually aboard a carrier. Per-airbase fields (`allowHotStart`,
`OperatingLevel_Air`) are now preserved from the **base mission**, not taken
from the inventory template. Objective-level `UNLIMITED_SUPPLY` /
`UNLIMITED_AIRCRAFTS` trigger-zone props still work (bflib keeps that
objective's model maxed); such objectives show a `*` above their name on the
F10 map.

**Naval aircraft roster (optional):** in `warehouse.miz`, place an actual
**carrier ship** named `BINVENTORYNAVY` and one named `RINVENTORYNAVY`
(anywhere — they're template-only, never spawned), and configure each one's
**ship warehouse** in the ME with the aircraft that side's carriers should
carry (F/A-18C, F-14, AV-8B, …) at a nonzero count. `bftools miz` copies
that `aircrafts` roster onto every carrier in the base mission — a carrier
being a **ship group whose name contains `CARRIER`** — and creates a
warehouse for any carrier the ME never saved one for. So carrier jets are
configured once, in one place, and don't leak onto land airbases. Types left
at amount 0 are ignored. Names overridable with
`--blue-navy-production-template` / `--red-navy-production-template`; omit the
two ships entirely and carriers keep whatever roster is on them in the editor.

The generated mission's campaign config (the JSON file `bflib` loads at
startup — e.g. `ODFv2_CFG`, named whatever you point bflib's state path at,
not a fixed filename) can also be edited live from the web dashboard's
**Config Editor** (`/admin/config` in bfweb, enabled by passing
`--engine-config <path>` to `bfdb`) instead of hand-editing JSON — see the
root [README.md](../README.md) for setup.

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
- **Timed Repair**: Repair takes configurable time (default: 1800s = 30 minutes)
  - Deliver a carrier repair crate to the carrier (air-drop or helo) to start / speed up repair
  - **Stacking**: each additional crate delivered while a repair is running divides the remaining time (floored at 60s) — 2 crates ≈ 15 min, 3 ≈ 10 min
  - Also startable from the naval-base actions menu (**Repair Carrier** / **Respawn Carrier**, paid from base supplies), or automatically when the carrier is near a friendly, stocked naval base
  - Progress notifications every 5 minutes
  - Time configurable in the campaign config JSON `carrier.repair_time` (in seconds)
- **Repair Cost**: Requires supplies from parent Naval Base (default: 5000 supplies)
- **Respawn Cost**: Requires supplies from parent Naval Base (default: 15000 supplies)
- **Capture Mechanics**:
  - Disable the carrier (logistics to 0 — sink escorts / supply ship), then take its parent Naval Base (either order works)
  - The enemy task force is **replaced by the captor's own co-located reserve group** in the same spot (see the CG section for the required `<S>CARRIER` / `<S>CARRIER2` layout) — no dynamic respawn
  - Captured carrier starts at 50% health, 100% logistics
  - Retained "foreign" airframes (types the captor doesn't normally produce) stay grounded until repairs finish; the captor's own aircraft work immediately
  - Messages sent to both sides when capture occurs
  - `-admin capture <CG objective name> <side>` forces it manually
- **Supply Ship Dependency** (NEW):
  - Include a ship with "SUPPLY" in group/unit name (e.g., `BSUPPLY#001`)
  - When supply ship destroyed: logistics drops to 0%, warehouse disabled
  - No aircraft spawns, no supplies until carrier is repaired
  - Repairing carrier also repairs/respawns supply ship
- **Movement Speed**: Configurable in the campaign config JSON (default: 5.0 m/s ≈ 10 knots)
- **Template Naming**: Carrier task-force groups must contain `CARRIER` in the group name and be late-activated. Use the
  `<S>CARRIER` / `<S>CARRIER2` convention (see the CG objective section) so each of the two objectives gets one group per side —
  the live one is chosen from the objective owner and swapped on capture. All groups need unique group **and** unit names.
  `carrier.groups` in the campaign config can still list templates explicitly.
- **Spawn Repositioning Speed** **[NEW]**: `carrier.spawn_repositioning_speed` (default: 100.0 m/s) — carriers always
  spawn at their mission-editor position on load; this controls how fast they navigate back to their last saved
  position afterward
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
- **Capture/Repair Progress** **[NEW]**: while an objective is actively being
  captured, or a carrier is mid-repair, the same label box also shows a
  live `Capturing: N%` or `Repairing: N%` line (mutually exclusive in
  practice, but neither is enforced against the other)
- **Color Coding**:
  - Green (80-100%): Healthy
  - Yellow (40-79%): Damaged/Low supplies
  - Red (1-39%): Critical
  - Gray (0%): Destroyed
- **Arrow Updates**: Supply route arrows automatically reposition when carriers move
- **Threat Markers** **[UPDATED]**: the first time an objective is
  threatened, a single axis-of-advance arrow + "ENEMY CONTACT" label is
  drawn (expires after 2 minutes). If it comes under active attack, an
  "UNDER ATTACK" text label is also posted on its own cooldown — this used
  to also draw two extra converging NW/NE arrows, which duplicated the
  threatened arrow and cluttered the map, so it's text-only now.

### Frontline Drawing **[NEW]**
A dashed line is drawn along the Red/Blue territory boundary on the F10 map:
- **Boundary trace**: the engine builds a Voronoi ownership grid from every
  objective and traces the edge where Red territory meets Blue territory
- **Coloured by local advantage**: each segment is blue where blue holds the
  ground around it, red where red does, and **white / dotted** where the two
  sides are roughly balanced (a genuinely contested stretch)
- **Automatic updates**: redrawn whenever an objective changes hands
- **Replaces** the older semi-transparent territory shading

#### Configuration
```json
"frontline": {
  "enabled": true,
  "update_on_objective_change_only": true,
  "samples_per_boundary": 100,
  "max_marks": 200
}
```

#### Parameters
- **enabled**: `true` to draw the frontline
- **update_on_objective_change_only**: only redraw when an objective changes owner (recommended for performance)
- **samples_per_boundary**: ownership-grid resolution (50-200, higher = finer, fewer stair-steps, slower)
- **max_marks**: cap on the number of line segments drawn (default 200). Fewer = lighter on server/clients;
  the segment list is thinned to fit.
- **territory_zone_alpha**: *legacy* — was the shading opacity; ignored by the line renderer but still parsed.

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

**Missing-crate feedback** **[NEW]**: a multi-piece deployable (e.g. a SAM
battery) auto-unpacks as soon as all its required crates have landed
nearby — auto-unpack keeps retrying every tick so a late-arriving crate
(parachute drift) still triggers it promptly. If a crate never arrives
(e.g. the delivering aircraft was shot down before dropping the rest),
each landed-but-incomplete crate shows what's still missing two ways: a
one-time panel message (sent once, not spammed every tick), and a
persistent F10 map marker at the crate's own position that stays up and
updates as long as it remains incomplete, removed automatically once the
deployable finally spawns.

#### **Tactical Considerations**
- **Forward Base Placement**: Bases with `LOGISTICS_DETACHED = true` are higher risk/reward
- **Route Security**: Plan convoy routes through safe territory
- **Air Interdiction**: CAS/Attack helicopters can hunt enemy convoys
- **Timing**: Convoys spawn every N logistics ticks - plan attacks accordingly
- **Combined Arms**: Coordinate convoy escorts with air cover

### Configuration
All new features can be configured in the campaign config JSON:
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
    "spawn_repositioning_speed": 100.0,
    "repair_time": 600,
    "groups": [
      { "template": "BCARRIER1", "display_name": "CVN-73 Washington" }
    ]
  },
  "special_sam_capture_radius_m": 300.0,
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
    "max_marks": 200,
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
- `spawn_repositioning_speed` **[NEW]**: Speed in m/s used to navigate back to the carrier's last saved position
  after a restart (default: 100.0 ≈ 194 knots)
- `groups` **[NEW]**: Optional explicit list of `{template, display_name}` pairs, if you'd rather not rely on
  `BCARRIER`/`RCARRIER` prefix auto-detection
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
| SAM | Special SAM Site | No | Hidden IADS, not built from a trigger zone — see [Special SAM Sites](#special-sam-sites-new) |
| CC | Command Center | No | IADN network node — SAM sites auto-link to the nearest friendly one, see [Command Center](#command-center-cc-new) |

---

## Changelog

### Version 2.8 (2026-09-01)
**Changed: frontline is now a line; LOGISTICS_DETACHED means fully cut off**
- **Frontline drawing**: the `frontline` overlay now draws a dashed line
  along the Red/Blue territory boundary instead of semi-transparent
  territory shading. Segments are coloured by local advantage — blue, red,
  or white/dotted where contested. `frontline.territory_zone_alpha` is now
  a legacy no-op (still parsed). See
  [Frontline Drawing](#frontline-drawing-new).
- **`LOGISTICS_DETACHED` semantics flipped**: `true` now cuts an objective
  off from *all* automatic resupply (no convoy, no air, no instant) —
  players fly supplies in by hand. `false` gets a convoy (or a cargo
  plane when very low), falling back to instant only if the convoy system
  is disabled. Front-line zones that previously relied on
  `LOGISTICS_DETACHED = true` for convoy routing should be set to `false`.
- **Carrier task forces are co-located**: each carrier objective holds
  both sides' task forces in the `.miz` (`<S>CARRIER` / `<S>CARRIER2`);
  the engine swaps live/reserve on capture. Every ship in a carrier group
  gets the naval inventory template (`BINVENTORYNAVY` / `RINVENTORYNAVY`),
  and carriers stock aircraft only from what is actually on their deck.

### Version 2.7 (2026-08-19)
**Added: IADN point defense**
- **Point defense**: a short-range system (Pantsir, Shilka, Tor, etc.)
  co-located in the same DCS group as a SAM site's main search radar can
  now be tagged `UnitTag::EngagesWeapons` to stay emission-on unconditionally,
  including while the group is forced dark by HARM defense — protecting
  the main radar from anti-radiation missiles using DCS's own native
  point-defense AI. Already applied to `ZSU-23-4 Shilka` and
  `CHAP_PantsirS1` in the live config as an example. See
  [Point defense](#point-defense).

### Version 2.6 (2026-08-19)
**Added: capture/repair progress on labels, C-130 missing-crate feedback, map cleanup**
- **Capture/repair percentage**: objective F10 labels now show a live
  `Capturing: N%` line while being captured, or `Repairing: N%` while a
  carrier is mid-repair, in the same box as health/supply/fuel/points.
- **C-130 missing-crate spam fix**: a landed crate waiting on missing
  siblings used to re-send "Crate landed, need more crates for X" to the
  panel every single tick, forever, if a sibling was permanently lost
  (e.g. destroyed along with the delivering aircraft). It's now sent at
  most once per crate. Auto-unpack retry timing is unchanged.
- **C-130 missing-crate map marker** (NEW): a landed-but-incomplete crate
  now also drops a persistent F10 marker at its own position showing
  what's still needed, updated each retry and removed once the deployable
  spawns — see [C-130 Physical Cargo System](#c-130-physical-cargo-system-updated).
- **Removed converging "under attack" arrows**: the two extra NW/NE
  attack arrows drawn alongside the "UNDER ATTACK" text label duplicated
  the single "threatened" arrow and cluttered the map — that marker is
  text-only now.
- **Internal**: F10 multi-point map symbols (naval base anchor, carrier
  silhouette) now draw as a chain of 2-point line segments instead of
  relying on DCS's nonexistent native polyline call — no visual change.

### Version 2.5 (2026-08-18)
**Added: IADN (Integrated Air Defence Network) and Command Center objective**
- **Command Center** (NEW objective type, `CC` prefix): pure trigger-zone
  IADN network node, no DCS airbase/pad, no slots. Every Special SAM Site
  auto-links to its nearest same-coalition Command Center at mission
  start — see [Command Center](#command-center-cc-new).
- **IADN SAM cueing + EMCON**: SAM search radars now use the shared,
  multi-sensor fused track picture (not just their own organic radar) to
  decide when to power up, instead of running DCS's bare default AI with
  radar always hot. Requires a live, friendly Command Center link — a
  SAM site cut off from its network falls back to plain always-on AI.
- **HARM / anti-radiation missile defense**: SAM sites now react to
  inbound anti-radiation missiles by forcing radar dark for a
  configurable cooldown, instead of having no defense against them at
  all. Requires populating the new `iadn.anti_radiation_weapons` list
  with your theater's actual ARM type names — ships empty by default.
- **Engagement doctrine (hysteresis)**: randomized per-site reaction
  delay on first detecting a cue, plus a minimum engagement dwell time
  once hot, so sites don't flicker or react in perfect network-wide
  lockstep — see `iadn.reaction_delay_max_secs` / `iadn.min_hot_dwell_secs`.
- **Layered search / tracking radar**: sites with units tagged
  `UnitTag::SearchRadar` / `UnitTag::TrackRadar` now get independent
  per-unit emission control — search radar mirrors the site's hot/dark
  state, tracking radar only powers up once a target is within
  `iadn.track_radar_range_fraction` of the search radar's range.
- **Jamming / ECM**: units tagged `UnitTag::Jammer` now degrade nearby
  enemy radar detection probability within `iadn.jamming_range_m`,
  scaled by `iadn.jamming_detection_penalty` and each radar's own
  `ecm_susceptibility`.
- See [IADN](#iadn-integrated-air-defence-network-new) for the full
  mechanic and config reference, and `miz/SAMPLE_CFG.json`'s `iadn`
  block for a populated example.

### Version 2.4 (2026-08-13)
**Added: Special SAM Sites, config editor, and doc corrections**
- **Special SAM Sites** (NEW objective type): hidden, map-fixed IADS
  installations built from a dedicated mission editor template instead of
  a trigger zone. Generated into campaign config via the new
  `bftools special-sam` command — see [Special SAM Sites](#special-sam-sites-new).
- **Carrier config additions**: `carrier.groups` (explicit template list,
  alternative to `BCARRIER`/`RCARRIER` prefix detection) and
  `carrier.spawn_repositioning_speed`.
- **Frontline config addition**: `frontline.max_marks`, capping F10 mark
  count for territory zone drawing.
- **Web-based Config Editor**: the campaign config JSON can now be edited
  from the dashboard (`/admin/config` in bfweb, behind `bfdb --engine-config`)
  instead of by hand — a form generated live from the engine's real config
  schema, validated on save. See the root `README.md`.
- **Corrected stale build step**: removed the old `luae payloadfix.lua`
  reference (script no longer exists in this repo) in favor of the actual
  current `bftools miz` mission-generation command.
- **Corrected filename**: `BFConfig.json` throughout this doc was a
  personal/stale filename that doesn't appear anywhere in the current
  codebase — replaced with generic "campaign config JSON" wording, since
  the actual filename is whatever you point `bflib`'s state path at
  (e.g. `ODFv2_CFG`).

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
  - Configurable via `frontline` section in the campaign config JSON
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
  - Configurable via `carrier.repair_time` in the campaign config JSON
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

*Document Version: 2.7*
*Last Updated: 2026-08-19*