# GCI Radar — Ground-Up Redesign Research

**Status:** Research / architecture (implementation not started)  
**Dashboard route:** `/gcimap`  
**Removed:** EuroScope + LotATC hybrid prototype (`GciMapPage.tsx` ~2.6k lines, `GciMap.css` ~780 lines)

---

## 1. Goals

| Goal | Notes |
|------|--------|
| Realistic **coalition GCI / ADOC-style** picture | Not a civilian ATC scope (EuroScope, LotATC, OpenScope patterns are wrong baseline) |
| **Campaign-truth** tracks | What blue/red GCI operators are allowed to know from EWR, AWACS, datalink rules — not DCS god-view |
| **No new Export.lua** for GCI | All dynamic data from **bflib** published on **netidx**; **bfdb** subscribes and fans out to the web UI |
| **Correct nav colors & geometry** | Sectors, airways, fixes, airfield layouts from **VATSIM sector files** (per theater), not ad-hoc map tiles |
| **Vector Strike palette** | Align with dashboard / TACMAP coalition colors (blue / red / neutral), not generic “cyber green CRT” |

---

## 2. What the old prototype did wrong

1. **God-view data** — Used `Export.lua` → UDP 42001 → `/ws/units` (every unit in the mission). Real GCI only sees **fused / reported** contacts.
2. **ATC feature creep** — Clearances, scratchpads, flight levels, conflict tools copied from EuroScope; GCI cares about **tracks, IDs, vectors, weapons, SAM rings**, not IFR clearances.
3. **14 MB embedded `sectorData.ts`** — Generated blob in-repo; hard to update, wrong licensing story vs shipping raw VATSIM `.sct2` / GeoJSON per theater.
4. **Leaflet + raster tiles** — Fine for TACMAP; GCI scopes are usually **plan-position indicator (PPI)** or **B-scope** style vector canvases with NM rings and bearing lines.
5. **No netidx contract** — Nothing subscribed to campaign engine state; duplicated paths with TACMAP instead of one authoritative feed.

---

## 3. GCI vs ATC (why the old dashboard prototype was the wrong species)

| | **GCI / air defense control** | **Civilian ATC (EuroScope, etc.)** |
|---|------------------------------|-------------------------------------|
| Purpose | Vector fighters to **hostile** contacts; cue SAMs; maintain **surveillance picture** | Separate **IFR/VFR traffic** safely in controlled airspace |
| Primary display | **PPI** or **geographic COP** with radar/link tracks | Plan view + **flight strips**, routes, alt assignments |
| Identity | IFF / link / radar **track quality** — often unknown at first | **Callsign + squawk** from flight plan |
| Key numbers | **BRAA/BRA**, track number, intercept geometry | Cleared level, heading, approach type |
| Operator output | “Vector 270, climb FL240, **tally** / **no joy**” | “Descend FL100, turn left heading 090” |

Vector Strike GCI should feel like a **CRC / EWR ops desk** or **AWACS surveillance scope**, not an approach controller scope.

---

## 4. How real GCI screens look (WWII → current generation)

### 4.1 Visual layout families

**A. Plan Position Indicator (PPI) — still the mental model**

- **Top-down** picture: **bearing** around the circle, **range** outward from center.
- Center = **radar site**, **CRC**, or **offset anchor** (bullseye, fighter, geographic fix).
- **Range rings** (10 / 25 / 50 / 100 NM typical).
- **Azimuth spokes** (every 30° or 45°).
- **Tracks** = symbol + optional **leader line** (velocity vector) + **data tag** (amplify block).
- **No** photographic map required; many military scopes are **black background + green/amber vector graphics**. Geographic coastlines may appear on **large COP displays**, not always on a tight PPI.

```
                    N
                    |
         . -  -  -  +  -  -  - .   ← 50 NM ring
       .     \  |  /     .
      .   ▲ hostile (square)  .
     .      \ | /   ○ friendly .
    W -------+------- E
     .        |        .
      .   ◇ unknown   .
       .      |      .
         ' -  -  +  -  '
                    S
        [center] = bullseye / GCI site
```

**B. Geographic Common Operating Picture (COP)**

- Used at **CRC, CAOC, ASOC-style** desks: map underlay + tracks.
- Same **track symbology** as PPI, but anchored to **lat/lon** (what VATSIM sector files help with).
- Vector Strike can offer **PPI mode** (intercept geometry) and **map mode** (theater context) sharing one track list.

**C. SAM / battery local display**

- **Smaller sector** (e.g. 120° fan, shorter range).
- Emphasis on **engagement zone**, **missile flyout**, **fire doctrine** — not full theater.
- Useful as a **layer** (WEZ rings from campaign cfg), not the whole UI.

### 4.2 Current-generation systems (what to match in *behavior*)

| Layer | Real-world role | What the operator sees |
|-------|-----------------|------------------------|
| **Long-range surveillance radar** | Initial detection | **Plot** or **track** at coarse update rate; may **coast** when scan misses |
| **EWR / 3D radar / height-finder fusion** | Track continuity | Better altitude; **stale** marking; drop after timeout |
| **AWACS / AEW** | Air picture extension | More air tracks, **look-down** coverage; still not omniscient |
| **IFF / Mode S / link (Link 16 family)** | Identification | Friendly **PPLI** (own forces) + **surveillance tracks** (J3.x air) with **TN**, identity, quality |
| **CRC / GCI controller** | Intercept control | **BRAA** to target, vectors to fighters, **tally / no joy** workflow |
| **IADN / SAM cueing** | Engagement | **Track quality**, **WEZ**, sometimes **engagement status** |

**Not** the design target: EuroScope, vATIS, LotAtc, OpenScope (civilian ATC).

### 4.3 Typical screen regions (modern desk)

| Region | Content |
|--------|---------|
| **Main scope** (largest) | PPI or map COP; tracks; rings; optional radar **coverage wedges** |
| **Track list / strip board** | Sort by range, bearing, threat; columns mirror amplify block |
| **Selected track amplify** | Full BRAA, ID, age, source, classification |
| **Own forces / PPLI** | Friendly fighters, tankers, AWACS — often brighter or link-filled |
| **Reference** | Bullseye, altimeter setting, time (Zulu), **data age** |
| **Threat / SAM** | Rings, corridors, ROZ — from doctrine data, not from “see all units” |

Colors: historically **green or amber** phosphor on black; modern systems often **dark gray + coalition color** (blue friend, red hostile, yellow unknown). Match **Vector Strike TACMAP** colors, not generic “matrix green” unless user-selectable theme.

---

## 5. What data a real GCI picture shows

### 5.1 Per-track fields (surveillance / intercept)

What Link 16-style air surveillance and EWR reporting converge on (simplified):

| Field | Operator use | Vector Strike source (target) |
|-------|--------------|-------------------------------|
| **Track ID / TN** | Correlate voice (“TN AB123”) | `FusedTrackId` or engine-assigned id |
| **Position** | Plot on scope | Fused lat/lon (from `FusedTrack.pos`) |
| **BRAA / BRA** | Intercept calls | Already in `GibBraa` (`bflib/src/ewr.rs`) |
| **Altitude** | Vertical separation, crank | Fused alt; show FL or ft |
| **Speed** | Closure, energy | From velocity |
| **Heading** | Aspect, intercept | Heading + **velocity vector** on scope |
| **Age / last update** | Stale / coast | `last_detection`, `stale` flag |
| **Track quality / confidence** | Weapons cue legality | `FusedTrack.confidence`, `pos_uncertainty_m` |
| **IFF / identity** | Hostile vs friendly | `IffState`, coalition rules |
| **Classification** | Fighter, bomber, helo | `ContactClass` |
| **Sensor source** | “Picture” trust | `DetectedBy` `[G]` ground, `[A]` airborne |
| **Callsign** (optional) | Only when ID rules allow | Player name **not** on all hostile tracks |
| **Engagement status** | Fired, spiked, etc. | Future / SAM integration |

**bflib EWR text report header (real GCI column layout):**

```text
     BRG      RNG      ALT      SPD        HDG      AGE  ASPECT    SRC
```

That table is the **right data model** for intercept control; the web UI should be a **graphical view of the same truth**, not a separate god-view.

### 5.2 What is *not* shown on a real GCI scope

- Every aircraft in the battlespace (only **detected** or **linked** tracks).
- Ground clutter as **raw radar video** (synthetic tracks only).
- Full **flight plans**, **squawks**, **approach charts** (ATC).
- Perfect **type / callsign** on first detection (ID **ripens** over time).
- Instant updates (Link 16 air tracks are often ~**12 s** or less with dead reckoning between).

### 5.3 Static / background layers

| Layer | Source for Vector Strike |
|-------|--------------------------|
| Sector / FIR boundaries | VATSIM `.sct2` → GeoJSON |
| Airways, fixes, VORs | Sector file + colors per convention |
| Airfield layout / runways | Sector + DCS `me_db` validation |
| Bullseye | Mission / engine publish |
| SAM / EWR **coverage** | `radar_donors()` + cfg ranges |
| **Terrain radar shadow** | DCS `Land::is_visible` per ground EWR donor → `GciTerrainHorizon` in `GciPicture` |
| ROZ / MEZ / borders | Mission / campaign markup (if configured) |

---

## 6. Limitations: DCS world

| Real world | DCS + campaign engine | UI implication |
|------------|----------------------|----------------|
| Physics-based radar equation | **Approximated** in `bflib` (`compute_detection_probability`, chaff, ECM, aspect) | Show **confidence / stale**, not “truth” |
| Multiple independent sensors | Modeled as donors + fusion | Show `[G]`/`[A]` and optional coverage wedges |
| Link 16 network | **Not native in DCS** | Simulate **datalink** only if bflib adds it; else EWR-only picture |
| All air movers exist in sim | Engine knows all; **players must not** | **Never** bind GCI to Export.lua god-view |
| Pilot sees GCI in cockpit | DCS has no built-in browser GCI | Dashboard = **external GCI station** (second screen) |
| Radar spin rate / scan | Tied to `scan_interval_secs` in cfg | Expect **0.5–2 Hz** track updates, not 60 FPS |
| IFF modes | Coalition + rules of engagement | Filter by **desk side** server-side |
| Classification (Bomber, etc.) | `ContactClass` from unit type heuristics | May stay **UNKNOWN** often — realistic |
| Vertical uncertainty | Height-finder fusion simplified | Altitude block with **lower confidence** styling |
| Chaff / notching | **Chaff bursts + PD notch** in `ewr.rs`; player **Deploy Chaff** (F10 EWR) | Contested **`J`** on GCI when `jam` / low `conf` |
| Stand-off ECM | **`jam` cfg** + player **Toggle ECM**; AI ECM rolls; **`jam_zones`** corridors | Purple **JAM** overlay on `/gcimap` |

**Hard rule:** If the in-game player does not get an EWR report for a contact, the web GCI must **not** show it (except admin “god” mode).

---

## 7. Limitations: web application

| Constraint | Why it matters | Mitigation |
|------------|----------------|------------|
| **Latency** (WS + HTTP) | Intercept geometry stale by 0.5–2 s | Server timestamp; **dead reckoning** leader line; “DATA AGE” |
| **No radar IQ** | Cannot render real **video** | Synthetic **symbols** only (correct for most GCI) |
| **No STANAG 5516 in browser** | Cannot ship full Link 16 stack | JSON track schema inspired by fields, not bit-exact J-messages |
| **Symbology** | MIL-STD-2525 is reference, not mandatory copy | APP-6 **simplified** frames (air hostile square, friend circle, unknown diamond) |
| **Performance** | 200+ DOM markers kills FPS | **Canvas/WebGL** single layer |
| **Large navdata** | 15 MB TS blob unusable | Per-theater JSON fetch |
| **Security** | Client-side filter cheatable | **Coalition filter on bfdb** before WS send |
| **Audio** | Threat chime optional | Phase 2 |
| **Mobile** | Pinch-zoom PPI awkward | Desktop-first; tablet read-only OK |

---

## 8. What “current generation” means for Vector Strike (realistic target)

### Terrain radar shadow (implemented)

- **Not** live ground-clutter reflections from DCS.
- **Yes:** per ground EWR donor, bflib marches bearing/range using the same LOS as detections (`landcache` + `Land::is_visible`) at ~8 000 m probe altitude.
- Published as `terrain_horizons[]` (`brg_step`, `max_nm[]`) in `GciPicture`; bfweb draws dim wedges where terrain blocks beyond the horizon.
- Recomputed every **20 s** (cached between ticks). Toggle **TERRAIN** on `/gcimap`.

### Match (v1–v2)

1. **PPI or map COP** with range rings and bullseye-centered BRAA.
2. **Track symbology** + amplify block: BRG, RNG, ALT, SPD, HDG, AGE, aspect, source — aligned with `GibBraa`.
3. **Fused tracks only** from `FusedTrack` / EWR pipeline via **netidx**.
4. **Stale / coast** (`*` prefix, dim symbol, drop at `DROP_AGE_SECS`).
5. **Coalition desk** (blue / red / admin) with server-side filtering.
6. **Radar donor** range rings and SAM WEZ from cfg.
7. **VATSIM-colored** sector + airfield underlay per theater.
8. **Short history trail** (30–120 s), not ATC flight strips.

### Defer (not required for credible GCI)

- Link 16 J-message bit-exact implementation.
- Raw radar video / RHI display.
- Full MIL-STD-2525D modifier stacks.
- Voice integration inside the scope.
- Automated intercept steering (vectors are **human**-issued in training).

### Avoid

- EuroScope menus, clearance popups, scratchpads.
- Export.lua **omniscient** unit layer on GCI route.
- Civilian **flight level** workflow as primary UI.

---

## 9. Reference systems (names only — behavior, not pixels)

| Reference | Take from it |
|-----------|----------------|
| **CRC / Control Reporting Centre** | Fused picture, intercept control, track list |
| **AWACS surveillance scope** | Air tracks, limited ID, bearing-range emphasis |
| **Link 16 COP trainers** | TN, track quality, age, coalition picture |
| **SAM battery display** | Local sector, engagement ring |
| **Historical Type 7 GCI PPI** | Simplest correct layout: rings + plots |

---

## 10. Data architecture (target)

```
┌─────────────┐     netidx publish      ┌─────────────┐     subscribe      ┌─────────────┐
│   bflib     │ ───────────────────────►│  netidx     │◄───────────────────│    bfdb     │
│  (DCS tick) │  gci/* or extend stats  │  broker     │  archive + live    │  (HTTP/WS)  │
└─────────────┘                         └─────────────┘                    └──────┬──────┘
       │                                                                          │
       │ EWR / IADN fusion (`ewr.rs`)                                             │ /ws/gci
       │ Intel (`db/intel.rs`)                                                    ▼
       │ Objectives, bullseyes, radar donors                             ┌─────────────┐
       └────────────────────────────────────────────────────────────────►│   bfweb     │
                                                                         │  GciMapPage │
                                                                         └─────────────┘
```

### Existing engine assets (use, don’t re-export)

| Source | Location | GCI use |
|--------|----------|---------|
| EWR / fused tracks | `bflib/src/ewr.rs` (`FusedTrack`, `FusedTrackId`, confidence, stale) | Primary track layer |
| Radar donors | `bflib/src/db/mod.rs` `radar_donors()` | Coverage rings, sweep sector hints |
| Intel contacts | `bflib/src/db/intel.rs` | Ground picture / ELINT (optional layer) |
| Stats stream | `bfprotocols::stats::Stat` (`Position`, `Detected`, `Unit`) | Historical / replay via netidx archive |
| Perf hooks | `ewr_tracks`, `ewr_reports` in netidx perf | Ops tuning only |
| RPC | `bflib/src/bg/rpcs.rs` | Admin/debug, not primary GCI path |

### Proposed netidx paths (draft — to implement in bflib)

Publish at `{netidx_base}/{sortie}/gci/` (exact naming TBD):

| Path | Content | Rate |
|------|---------|------|
| `tracks` | Array of fused tracks (id, side, lat/lon, alt, hdg, spd, confidence, class, iff, detected_by, stale) | Per EWR tick |
| `donors` | Active radar donors (pos, range, aspect, band, side) | On change + periodic |
| `bullseyes` | Coalition bullseye lat/lon | On change |
| `theater` | DCS theater id string | Session |
| `meta` | Schema version, sim time, server time | Each batch |

**bfdb** adds:

- Subscribe to live `gci/*` (or read from archive tail for replay)
- `GET /api/gci/snapshot` — last picture for HTTP poll
- `WS /ws/gci` — same shape as snapshot, push on update
- **Coalition gate:** session side or admin flag filters tracks server-side (never trust client filter alone)

### What stays on Export.lua (for now)

TACMAP `/ws/units` may keep Export.lua for **admin / omniscient** map until engine publishes enough for map too. **GCI must not depend on Export.**

---

## 11. Navdata & sectors (VATSIM)

### Source files

VATSIM provides sector files (`.sct2`, `.sct`, geo tools) with:

- Sector boundaries (often polylines)
- Airport definitions, runways, frequencies
- ARTCC / FIR coloring conventions

Theater mapping examples already referenced in old `sectorData.ts` header:

- Caucasus: UGGG, UBBA, …
- Syria: OAKX, OBBB, ORBB, …
- Persian Gulf: OIIX, LCCC, …

### Pipeline (recommended)

1. **Build script** (`bftools` or `scripts/`) — parse VATSIM sector file + DCS `me_db` airfields for lat/lon validation  
2. Output **`public/navdata/{theater}.json`** — airfields, fixes, airways, sectors with **standard colors**  
3. **bfweb** loads JSON when `theater` from GCI meta matches  
4. Keep **`sectorData.ts` deprecated** — delete or shrink once pipeline exists (file is ~15M chars today)

### Color conventions (typical)

| Element | Suggested color role |
|---------|---------------------|
| Sector boundary | Dim cyan/gray `#4a8fd4` at 30% opacity |
| Upper / lower airspace | Dashed vs solid |
| Airways | Amber `#c9a227` |
| Fixes VOR | Small triangle, green |
| Airfield layout | Gray runway lines; active field highlight |
| Restricted / MOA | Red hatch (if present in sector file) |

Align coalition **tracks** with TACMAP: `COL_BLUE`, `COL_RED` from `MapPage.tsx`.

---

## 12. UI architecture (recommended v1)

1. **Full-viewport canvas** — PPI centered on bullseye or draggable anchor  
2. **NM range rings** — 10 / 20 / 50 / 100 NM user selectable  
3. **Track symbology** — hostile square, friendly circle, unknown diamond (NATO simplified)  
4. **Data tag** — callsign or track #, FL, GS, age, source glyph `[G]`/`[A]`  
5. **Side panel** — track list sorted by range, filter stale  
6. **Toolbar** — theater, range, layers (donors, sectors, airfields), desk (blue/red/admin)  
7. **No** Leaflet for v1 scope view (optional mini inset map later)

Tech stack: React + **Canvas2D or regl/pixi**; consider `d3-geo` only for geo→scope projection.

---

## 13. Phased delivery

| Phase | Deliverable |
|-------|-------------|
| **0** | This doc + empty shell page (done) |
| **1** | bflib netidx `gci/tracks` publisher from `FusedTrack` state; bfdb WS |
| **2** | Canvas PPI + bullseye + track symbols consuming `/ws/gci` |
| **3** | VATSIM navdata build + sector/airfield layers |
| **4** | Radar donor wedges, SAM rings from cfg |
| **5** | Intel layer, replay from netidx archive, operator auth |

---

## 14. Open questions

1. **Desk auth** — Discord role, in-game slot, or mission password for blue vs red GCI?  
2. **God view** — Admin-only full picture or never on GCI page?  
3. **Track IDs** — Show pilot name, anonymized track number, or both by classification?  
4. **Link to SRS** — Show freq from existing SRS API or out of scope?  
5. **Replay** — Same UI scrubbing netidx archive vs live only for v1?

---

## 15. Related files

| File | Role |
|------|------|
| `bflib/src/ewr.rs` | Fusion, physics, player reports |
| `bflib/src/db/intel.rs` | ELINT contacts |
| `bflib/src/db/mod.rs` | `radar_donors()` |
| `bfdb/src/main.rs` | `/ws/units` (Export.lua) — not for GCI v1 |
| `bfweb/src/pages/MapPage.tsx` | TACMAP reference for colors/WS |
| `bfweb/src/pages/sectorData.ts` | Legacy embedded navdata — replace with build pipeline |
| `scripts/Export.lua` | Keep for TACMAP until engine replaces it |

---

*Last updated: 2026-06-02 — research baseline for implementation planning.*
