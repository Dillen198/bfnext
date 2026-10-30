# Fowl Engine — Detailed Testing Checklist

**How to use this document**
- Each item has: **Steps** (how to trigger it) and **Expected** (what correct behavior looks like)
- Mark items `[x]` as you go, add notes in the Notes column of the sign-off table at the end
- Run the [automated tests](#automated-tests) first. The tables then cover what only shows in DCS, in Discord, on the websites or on the server box
- Sections 1-25 are the campaign engine (bflib). 26 is the training range, 27 the cockpit plugin, 28 auto-update and server ops
- All testing is done in-game on a live DCS server unless otherwise noted

---

## Automated tests

These need no DCS, no netidx resolver and no network. CI (`.github/workflows/build.yml`) runs all of them.

| What | Command | Notes |
|------|---------|-------|
| Rust workspace (bflib, bfrange, bfdb, bfprotocols, dcso3, livewx, yats, cockpit net and installer) | `cargo test --workspace --locked` | The repo root must be on `PATH` for `lua.dll` (`. .\setup-build.ps1` does it). `bfweb/dist` and `bfsite/dist` must exist: build the frontends or create empty folders |
| bftools (not a workspace member) | `cargo check --locked --manifest-path bftools/Cargo.toml` | Check only |
| Fowl Engine Manager | `cargo test --manifest-path bfmanager/src-tauri/Cargo.toml` | Needs `bfmanager/dist` (a placeholder `index.html` will do) and the bundled resources: `node bfmanager/scripts/stage-bot.mjs` and `node bfmanager/scripts/stage-range.mjs` |
| Dashboard and Manager UI | `npm --prefix bfweb test` | vitest |
| Range site | `npm --prefix bfrange-web test` | vitest |
| DCSServerBot plugin | `pip install -r DCSServerBot/tests/requirements.txt`, then `python -m pytest -q` in `DCSServerBot/tests` | Fakes of the bot's core: no bot, Discord, DCS or database |
| Cockpit panel script (WIKI tab) | `"D:\DCS World\bin-mt\luae.exe" bfcockpit\tools\luatest.lua` | Runs `bfcockpit.lua` on DCS's own Lua 5.1 with stand-ins for dxgui and the DLL, every `bfdb/seed_wiki` page through the renderer. Not in CI (needs a DCS install). `LUATEST_PAGE=<slug>` prints one page as laid out |

They do not cover the DCS runtime (spawns, events, F10 menus, slots), real Discord posts, the Windows service or real GitHub releases. The tables below do.

---

## Pre-Test Setup

| # | Item | Steps | Expected | Pass |
|---|------|-------|----------|------|
| P1 | Build DLL | `cargo build --release --package=bflib` | Compiles with zero errors; `target/release/bflib.dll` updated | |
| P2 | Deploy DLL | Copy `bflib.dll` to DCS mission folder | File timestamp updated in mission folder | |
| P3 | Load mission | Start DCS server, load mission | Server reaches "Running" state, no Lua errors in `dcs.log` | |
| P4 | Admin account | Log in with known-admin UCID | Admin commands accepted in chat | |
| P5 | Regular account | Second client connects as regular player | Connects without admin privileges | |
| P6 | Logs active | Check `Logs\bfnext.txt` in the server's Saved Games folder | Log file exists and is being written to | |
| P7 | bfdb connected | Start the bot (with `bfdb.manage: true` it runs bfdb and netidx), check bfdb's log | `/api/health` answers; no netidx timeout errors | |

---

## 1. Mission Initialization

| # | Item | Steps | Expected | Pass |
|---|------|-------|----------|------|
| 1.1 | Clean start | Load fresh mission (no saved state) | No errors in log during `init_miz()` / `delayed_init_miz()` | |
| 1.2 | Objectives load | After load, open F10 map | All named objectives appear at correct map positions | |
| 1.3 | Objective sides | Check F10 map objective colors | Blue objectives show blue, red show red, neutral show neutral | |
| 1.4 | Units spawn | Watch server log after load | Ground units and objective defenders appear in DCS without errors | |
| 1.5 | Warehouse init | Run `-admin log-warehouse objective <airbase>` | Warehouse written to `bfnext.txt` with non-zero fuel and ammo for supply-capable objectives | |
| 1.6 | Config validates | Check log on startup | No `[ERROR]` lines related to cfg parsing; no unknown fields warned | |
| 1.7 | F10 markings | Open F10 map as blue/red player | Objective labels, health/supply indicators visible on map | |
| 1.8 | Netidx publish | Start bfdb, watch its output | Stats data flows; no "publisher disconnected" errors in first 60s | |
| 1.9 | State load | Restart server with existing save file | Previous objective ownership and player data restored correctly | |

---

## 2. Player Connection & Registration

| # | Item | Steps | Expected | Pass |
|---|------|-------|----------|------|
| 2.1 | New player connect | Connect fresh client (no prior UCID in DB) | Player appears in server, `on_player_try_connect` fires | |
| 2.2 | Register on first slot | New player takes a Blue slot | Player registered to Blue; chat shows "<name> has joined Blue team"; slot granted | |
| 2.3 | Switch to same side | Registered Blue player types `-switch blue` in spectators | "you are already on the requested side"; no crash | |
| 2.4 | Side switch blue | Red player in spectators types `-switch blue` | Side set to Blue; chat shows "<name> has switched to Blue" | |
| 2.5 | Side switch red | Blue player in spectators types `-switch red` | Side set to Red; chat confirms | |
| 2.6 | Side switch while in slot | Occupy a slot, then type `-switch red` | Refused: "you must be in spectators to switch sides" | |
| 2.6a | Switch limit | Use up the configured side switches, switch again | "you can't switch sides again this round" | |
| 2.7 | Whitelist rule | Set `rules.cargo` to `Whitelist` in the config, slot in with a UCID not on it | No cargo menu for that player; listed UCIDs get it | |
| 2.8 | Blacklist rule | `-admin blacklist cargo <player>` | That player loses the cargo menu; `-admin whitelist cargo <player>` gives it back | |
| 2.9 | Ban on connect | `-admin ban 1h <player>`, reconnect | Connection refused: "you are banned until ..." | |
| 2.10 | Admin privileges | Connect admin UCID (listed in the config's `admins`) | `-admin` commands accepted; a regular player's `-admin` gets no reply and does nothing | |
| 2.11 | Name filter | Set `name_filter` in the config, connect with a name that doesn't match | Connection refused: "name must match ..." | |
| 2.12 | Reconnect same UCID | Disconnect and reconnect same player | Player data persists (points, side, registration) | |

---

## 3. Slot System

| # | Item | Steps | Expected | Pass |
|---|------|-------|----------|------|
| 3.1 | Take free slot | Select any empty slot | Player spawns in cockpit; `on_player_try_change_slot` fires | |
| 3.2 | Slot type in ephemeral | After spawning, call the `query-player` RPC for that player | Shows correct vehicle type for occupied slot | |
| 3.3 | Occupied slot blocked | Player A takes slot; Player B tries same slot | Player B rejected with "slot occupied" message | |
| 3.4 | Admin takes occupied | Admin tries slot already held by another player | Admin bypasses check and occupies; original player deslotted | |
| 3.5 | Dynamic slot approved | If dynamic slots configured, request slot approval | Slot approved after server processes request | |
| 3.6 | Dynamic slot denied | Trigger deny condition (wrong side, no lives, etc.) | Player rejected; `process_slot_rejection()` message shown | |
| 3.7 | Wrong-side slot | Player set to Blue tries Red slot | Slot access denied; player stays in spectator | |
| 3.8 | Slot freed on disconnect | Player in slot disconnects | Slot freed immediately; another player can take it | |
| 3.9 | Multi-crew slot | Enter multi-crew aircraft as first crew | `units_by_slot` maps slot to multiple unit IDs | |
| 3.10 | player_in_slot map | Occupy slot, verify via the `query-player` RPC | `player_in_slot` correctly maps SlotId → Ucid | |

---

## 4. Life System

| # | Item | Steps | Expected | Pass |
|---|------|-------|----------|------|
| 4.1 | Standard life cost | Take off in a standard aircraft | Life counter decremented by 1 standard life; log confirms | |
| 4.2 | Intercept life type | Take off in configured intercept aircraft | Intercept life deducted (not standard) | |
| 4.3 | Logistics life type | Take off in a logistics aircraft (C-130 etc.) | Logistics life deducted | |
| 4.4 | Attack life type | Take off in attack aircraft | Attack life deducted | |
| 4.5 | Recon life type | Take off in recon aircraft | Recon life deducted | |
| 4.6 | No life on ground start | Spawn on ramp without taking off | No life deducted until actual takeoff event fires | |
| 4.7 | -lives command | Type `-lives` in chat | Message shows remaining lives for each type for player's side | |
| 4.8 | Life message on takeoff | Take off in any aircraft | Popup or panel message shows which life was consumed | |
| 4.9 | Lives return after capture | Capture an objective with `log_points > 0` | `return_lives()` fires; life count increases for winning side | |
| 4.10 | Periodic life return | Wait for slow timer cycle | Lives tick up passively over time (if configured) | |
| 4.11 | Zero lives blocks slot | Exhaust all lives of one type | Attempting to take slot of that type gives "no lives remaining" error | |
| 4.12 | Admin reset lives | `-admin reset-lives <player>`; `-admin lives <player> attack 1` | Lives restored to config default; then exactly 1 attack life left | |

---

## 5. Events & Death Handling

| # | Item | Steps | Expected | Pass |
|---|------|-------|----------|------|
| 5.1 | Birth event — player | Player spawns in aircraft | Birth event fires; F10 menus populate for that player's slot | |
| 5.2 | Birth event — AI group | Spawn AI group via admin | Birth event fires; group added to `groups_alive` in ephemeral | |
| 5.3 | Unit death — player aircraft | Shoot down a player aircraft | Dead event fires; unit removed from tracking; group health updated | |
| 5.4 | Unit death — AI unit | Destroy an AI ground unit | Unit removed from group; group health fraction recalculated | |
| 5.5 | Group fully destroyed | Destroy all units in a group | Group removed from `groups_alive`; group health = 0 | |
| 5.6 | Kill event credit | Player kills enemy unit | Kill attributed to correct player in ShotDb; score updated | |
| 5.7 | PilotDead + CSAR | Pilot dies in aircraft (no eject) | CSAR beacon spawns at death position (if CSAR enabled) | |
| 5.8 | Ejection + CSAR | Player ejects (bail out) | CSAR beacon spawns at ejection position | |
| 5.9 | Takeoff life deduction | Lift off from runway | Takeoff event fires; correct life type deducted | |
| 5.10 | Land event | Land aircraft at friendly airbase | Land event fires; `landed_at_objective` set; sortie state updated | |
| 5.11 | PlayerLeaveUnit | Player presses "Return to Spectators" | Slot freed; player in slot map cleared; menus removed | |
| 5.12 | Hit event recorded | Fire weapon that hits but doesn't kill | Hit recorded in ShotDb under target's entry | |
| 5.13 | Shot event recorded | Fire any weapon | Shot recorded with shooter identity | |
| 5.14 | PostponedTakeoff | Trigger postponed takeoff (AI taxiing) | Event handled gracefully; no crash or nil dereference | |
| 5.15 | MarkAdded | Player adds F10 map mark | If mark-based menu used, event fires and is processed | |
| 5.16 | MarkRemoved | Player removes F10 map mark | Event handled without crash | |

---

## 6. Kill Attribution (ShotDb)

| # | Item | Steps | Expected | Pass |
|---|------|-------|----------|------|
| 6.1 | Single shot kill | Player fires one weapon, destroys target | `bring_out_your_dead()` returns that player as killer | |
| 6.2 | Multi-hit kill | Multiple shots hit before kill | Last-hit attacker credited (or highest-hit depending on logic) | |
| 6.3 | Splash kill | Bomb splash kills multiple units | Each unit's death attributed to bomber | |
| 6.4 | AI kill (no player) | AI unit destroys another unit | Kill logged; no player score update; no crash | |
| 6.5 | Teamkill | Player kills own-side unit | Handled without crash; logged; score may be penalized | |
| 6.6 | Kill after long engagement | Unit hit, survives 5 min, then dies | Kill still attributed if within GC window | |
| 6.7 | GC runs | Wait longer than GC interval with no kills | Stale shot records cleared; `last_gc` timestamp updated | |
| 6.8 | Dead queue processed | Unit dies, check `dead` map | Entry exists briefly then processed by `bring_out_your_dead()` | |
| 6.9 | `recently_dead` window | Unit dies, check within 5-min window | `recently_dead` entry present; prevents double-attribution | |

---

## 7. Objective System

| # | Item | Steps | Expected | Pass |
|---|------|-------|----------|------|
| 7.1 | Defender health update | Destroy some objective defenders | Objective health drops; F10 label color changes | |
| 7.2 | Full defender destruction | Destroy ALL defending units at objective | Objective becomes capturable; `advise_captureable()` fires | |
| 7.3 | Capture — troops | Move infantry into capture zone with `can_capture=true` | After dwell time, objective changes side | |
| 7.4 | advise_captureable | Destroy defenders, watch chat | Friendly side gets notification that objective is capturable | |
| 7.5 | advise_captured | Complete capture | Server-wide announcement of new objective owner | |
| 7.6 | Supply affects rearming | Let supply drop to near-zero at airbase | Players at that airbase cannot rearm/refuel (or get reduced amounts) | |
| 7.7 | nearest_friendly routing | Check logistics path from isolated objective | Supply routes via nearest friendly, not direct to enemy | |
| 7.8 | Warehouse sync | After supply delivery, check DCS warehouse in-game | Ammo/fuel counts in DCS match engine's tracked values | |
| 7.9 | Map markup on capture | Capture objective, check F10 | Label updates to new side color; health/supply bar resets | |
| 7.10 | Objective re-defense | After capture, new defenders spawn | Spawned groups appear and are attributed to new owner side | |
| 7.11 | Neutral objective undefended | Load mission, check any Neutral-owned objective | Zero defenders spawn regardless of any Neut-coalition template placed in the trigger zone; objective shows as immediately capturable | |
| 7.12 | Special SAM site culling | Fly an aircraft toward a non-LR special SAM site, then away | Site spawns once inside its wake distance, culls again after `cull_after` once no threat is nearby (no longer always-spawned) | |
| 7.13 | Special SAM site LR wake distance | Approach an SA-10/S-300 special SAM site from `lr_cull_distance`/`special_sam_wake_distance` range | Site spawns/activates at that longer distance, well before the generic `unit_cull_distance` | |
| 7.14 | Special SAM site short-range wake distance | Approach a Roland/SA-15 special SAM site configured in `special_sam_wake_distance` | Site stays culled until well inside the shorter configured distance, not the generic default | |

---

## 8. Group & Unit Management

| # | Item | Steps | Expected | Pass |
|---|------|-------|----------|------|
| 8.1 | Spawn at heading | Spawn group via action with heading | Units face correct direction on spawn | |
| 8.2 | Spawn on road | Spawn ground group near road objective | Units appear on or near road surface, not floating | |
| 8.3 | Spawn offshore | Spawn naval group at offshore objective | Units appear in water, not on land | |
| 8.4 | Health tracking | Damage some units in group | Group health% decreases proportionally | |
| 8.5 | Despawn::Now | Admin deletes group | Group removed from DCS immediately | |
| 8.6 | Despawn::After | Group set to despawn after duration | Group removed after configured time elapses | |
| 8.7 | Action group lifecycle | Player triggers action (e.g. patrol) | Group spawns; executes task; despawns on task completion | |
| 8.8 | Deployed group (crates) | Assemble required crates, unpack | Group spawns at crate drop location | |
| 8.9 | Troop group | Load and unload infantry | Infantry spawns at unload position as functional DCS group | |
| 8.10 | Objective group owned | Check `DeployKind::Objective` on spawned defender | Group's origin shows `Objective { id }` | |
| 8.11 | DownedPilot beacon | CSAR scenario — pilot down | Beacon group spawns at correct Vector3 position | |
| 8.12 | Pending move flush | Issue move command to group | `pending_moves` vector processes; group receives waypoint in DCS | |
| 8.13 | Move command execution | Group given waypoint | Group actually moves in DCS toward destination | |

---

## 9. F10 Menu System

### 9A. Action Menu

| # | Item | Steps | Expected | Pass |
|---|------|-------|----------|------|
| 9A.1 | Menu appears on birth | Spawn in aircraft | F10 radio menu shows Actions submenu within a few seconds | |
| 9A.2 | Position action list | Open Actions → position-targeted list | Correct actions for your aircraft type and side shown | |
| 9A.3 | Objective action list | Open Actions → objective list | Friendly and capturable objectives listed | |
| 9A.4 | Enemy objective action | Open Actions → enemy objectives | Enemy objectives shown for attack actions | |
| 9A.5 | Execute position action | Select a position action (e.g. patrol) | Action executes; group spawns or task fires; log confirms | |
| 9A.6 | Execute objective action | Select defend/capture on friendly objective | Action group spawns at/near selected objective | |
| 9A.7 | Point cost deducted | Use action with `cost > 0` | Player points decrease by cost amount | |
| 9A.8 | Insufficient points | Try action with cost > current points | Action refused; message shown; no group spawned | |

### 9B. Cargo Menu

| # | Item | Steps | Expected | Pass |
|---|------|-------|----------|------|
| 9B.1 | List nearby crates | Land near crates, open Cargo menu | All crates within range listed by name and weight | |
| 9B.2 | Load crate | Select crate from list | Crate loaded; cargo count increases; crate removed from ground | |
| 9B.3 | Weight limit enforced | Try loading crate that exceeds weight capacity | Load refused; message shows current/max weight | |
| 9B.4 | Slot limit enforced | Fill all cargo slots, try adding more | Refused; "no cargo slots available" message | |
| 9B.5 | Drop crate | Select "Drop Crate" with crate loaded | Crate appears on ground at current position | |
| 9B.6 | Drop at speed | Try dropping crate above `max_drop_speed` | Drop refused with speed warning | |
| 9B.7 | Drop at height | Try dropping above `max_drop_height_agl` | Drop refused with height warning | |
| 9B.8 | Airdrop | Unload Crate in flight, below `max_drop_speed` and `max_drop_height_agl` | Crate falls and lands near the release point | |
| 9B.9 | List current cargo | Open "List cargo" option | All loaded crates and troops shown with counts | |
| 9B.10 | Destroy crate | Select "Destroy crate" near ground crate | Crate removed from map; no orphaned entry in DB | |
| 9B.11 | Spawn crate | At a friendly logistics objective, spawn a crate from the cargo menu | Crate appears near the aircraft; refused away from a logistics point | |
| 9B.12 | Multi-crate assembly | Drop all required crates within assembly radius | After last crate dropped, deployable assembles and spawns | |

### 9C. Troop Menu

| # | Item | Steps | Expected | Pass |
|---|------|-------|----------|------|
| 9C.1 | Load troops | Land at objective, open Troop menu, select squad | Squad loaded; weight added; troop slots consumed | |
| 9C.2 | Weight limit on troops | Load until weight capacity reached | Further loading refused | |
| 9C.3 | Unload troops | Fly to target area, select "Unload" | Infantry group spawns at helicopter position | |
| 9C.4 | Extract troops | Land near friendly infantry, "Extract" | Troops loaded back into helicopter | |
| 9C.5 | Return troops | Deliver troops back to home base | Troops removed from aircraft; returned to available pool | |
| 9C.6 | Troop capture | Unload `can_capture=true` troops at enemy objective | Troops begin capture dwell timer | |
| 9C.7 | Non-capture troops | Unload `can_capture=false` troops at enemy obj | No capture initiated; troops just fight | |

### 9D. Mission Menu

| # | Item | Steps | Expected | Pass |
|---|------|-------|----------|------|
| 9D.1 | Create mission | Open Mission menu → Create | Mission created; ID assigned; appears in active list | |
| 9D.2 | View missions | Open Mission menu → View | All active missions for player's side listed | |
| 9D.3 | Join mission | Select "Join" on an active mission | Player added to mission roster | |
| 9D.4 | Cancel mission | Mission creator selects "Cancel" | Mission removed from active list; participants notified | |
| 9D.5 | Complete mission | Fulfill mission objectives, select "Complete" | Reward points awarded; mission removed from list | |
| 9D.6 | Mission AO on map | Create mission with AO | AO quad + ingress arrow appear on F10 map (own-side only) | |

### 9E. EWR Menu

| # | Item | Steps | Expected | Pass |
|---|------|-------|----------|------|
| 9E.1 | EWR toggle off | Open EWR menu → Toggle → Off | EWR unit ceases reporting; DCS unit may change task | |
| 9E.2 | EWR toggle on | Toggle back On | Reporting resumes | |
| 9E.3 | EWR report | Select "Report" with enemies in range | BRAA contacts listed for each detected aircraft | |
| 9E.4 | No contacts | Select "Report" with no enemies in range | "No contacts" message (not crash) | |
| 9E.5 | Imperial units | Select imperial toggle | Ranges shown in nautical miles, altitude in feet | |
| 9E.6 | Metric units | Select metric toggle | Ranges shown in km, altitude in meters | |
| 9E.7 | Friendly filter | Check report | Own-side aircraft NOT listed as threats | |

### 9F. JTAC Menu

| # | Item | Steps | Expected | Pass |
|---|------|-------|----------|------|
| 9F.1 | JTAC status | Open JTAC menu → Status | Shows laser code, contacts count, current target info | |
| 9F.2 | No target status | Open status with no targets | Shows "no target" without crash | |
| 9F.3 | Auto-shift on | Toggle auto-shift → On | JTAC cycles to next contact on timer | |
| 9F.4 | Auto-shift off | Toggle auto-shift → Off | JTAC stays locked on current target | |
| 9F.5 | IR pointer toggle | Toggle IR pointer | IR pointer activates/deactivates on DCS unit | |
| 9F.6 | Smoke target | Select "Smoke target" | Smoke marker appears near current JTAC target | |
| 9F.7 | Manual shift | Select "Shift" | JTAC acquires next contact in list | |
| 9F.8 | Filter by type | Select type filter (e.g. Armor only) | Contact list shows only armor units | |
| 9F.9 | Clear filter | Select "Clear filter" | All unit types visible again | |
| 9F.10 | Set laser code | Select "Set code" → enter code | Laser code updates; status shows new code | |
| 9F.11 | Relay target | Select relay to another JTAC | Second JTAC picks up same target | |
| 9F.12 | Artillery request | Select artillery mission with arty nearby | Artillery group fires at current JTAC target | |
| 9F.13 | ALCM request | Select ALCM strike (if ALCM unit nearby) | Cruise missile launched at JTAC target | |
| 9F.14 | Pin JTAC | Select "Pin JTAC" | JTAC menu stays in player's menu even after location change | |
| 9F.15 | JTAC location list | Multiple JTACs active, open location menu | JTACs grouped by location/objective | |
| 9F.16 | Refresh locations | Move JTAC, select "Refresh" | Updated list shows new JTAC positions | |

---

## 10. JTAC System (jtac.rs)

| # | Item | Steps | Expected | Pass |
|---|------|-------|----------|------|
| 10.1 | Enemy detection in range | Spawn enemy armor within JTAC range | Contact appears in `contacts` map | |
| 10.2 | Detection outside range | Spawn enemy beyond configured `range` | Not added to contact list | |
| 10.3 | LOS check (nolos=false) | Enemy behind hill, JTAC with LOS off | Contact not detected through terrain | |
| 10.4 | nolos=true | Same scenario with `nolos=true` | Contact detected through terrain | |
| 10.5 | Laser acquisition | JTAC targets enemy unit | DCS laser spot appears on/near unit | |
| 10.6 | Target lost | Target moves out of range / destroyed | JTAC shifts to next contact or goes to no-target state | |
| 10.7 | Priority order | Multiple targets of different types | Armor targeted before infantry (per priority config) | |
| 10.8 | UnitTag filter | Set `filter = Armor` | Only armor units in `contacts` | |
| 10.9 | Artillery in range | Artillery group near JTAC | `nearby_artillery` populated; arty menu appears | |
| 10.10 | ALCM in range | ALCM unit near JTAC | `nearby_alcm` populated with ammo count | |
| 10.11 | Menu rebuild | JTAC acquires new target | `menu_dirty = true` triggers rebuild of player's JTAC submenu | |
| 10.12 | Multiple JTACs | Two JTACs active for same side | `ContactsIter` iterates across both without overlap | |
| 10.13 | JTAC unit destroyed | Kill the JTAC unit | JTAC removed from `by_id`; player menus updated | |

---

## 11. EWR System (ewr.rs)

| # | Item | Steps | Expected | Pass |
|---|------|-------|----------|------|
| 11.1 | Aircraft detection | Fly enemy aircraft within EWR range | Aircraft added to track; next report includes it | |
| 11.2 | Bearing accuracy | Enemy at known bearing | Report bearing within ±5° of actual | |
| 11.3 | Range accuracy | Enemy at known distance | Report range within ±10% of actual | |
| 11.4 | Altitude reporting | Enemy at various altitudes | Report shows correct altitude band | |
| 11.5 | Aspect reporting | Fly towards/away from EWR | Aspect shown as HOT / COLD / BEAM / DRAG | |
| 11.6 | EWR toggle off | Toggle EWR off | Unit stops updating contacts; no reports generated | |
| 11.7 | EWR toggle on | Toggle EWR on | Contacts refresh and reports resume | |
| 11.8 | Multiple EWR units | Two EWR units, overlapping ranges | Same contact not duplicated in report | |
| 11.9 | Aircraft exits range | Fly enemy out of EWR range | Track drops from report after next cycle | |
| 11.10 | Format — imperial | Select imperial | Range in NM, alt in ft, format e.g. "045/120nm/25000ft COLD" | |
| 11.11 | Format — metric | Select metric | Range in km, alt in m | |

---

## 12. Cargo & Logistics

| # | Item | Steps | Expected | Pass |
|---|------|-------|----------|------|
| 12.1 | Crate spawn at objective | Admin spawns crate | Crate unit appears at objective on map | |
| 12.2 | Crate weight in menu | Check cargo menu | Weight shown matches `Crate.weight` in config | |
| 12.3 | Single-crate deployable | Drop 1 required crate, unpack | Deployable group spawns at crate location | |
| 12.4 | Multi-crate deployable | Drop only partial crates | No spawn; counter shows "2/3 crates present" | |
| 12.5 | Multi-crate complete | Drop final required crate | Deployable spawns; all crates consumed | |
| 12.6 | C130 vehicle load | Load vehicle into C130 | Vehicle group disappears from map; `c130_cargo` state = Loaded | |
| 12.7 | C130 vehicle delivery | Land C130, lower ramp, unload | Vehicle group respawns near landing zone | |
| 12.8 | C130 troop load | Load troops into C130 | Troop group loaded; weight accounted for | |
| 12.9 | C130 ramp state | Lower ramp in C130 | `ramp_down = true` enables cargo interaction | |
| 12.10 | Downed pilot pickup | CSAR helo lands near pilot | Pilot added to helicopter cargo | |
| 12.11 | Pilot delivery | Land at friendly base with pilot | Points awarded; pilot removed from cargo | |
| 12.12 | Ground convoy spawn | Trigger convoy (if logistics configured) | Convoy group appears at source objective | |
| 12.13 | Convoy movement | Watch convoy group | Group moves along road toward destination | |
| 12.14 | Convoy arrival | Convoy reaches destination | Supply unloaded; destination objective supply increases | |
| 12.15 | Convoy interdiction | Destroy convoy group | Supply NOT delivered; source reports convoy lost | |
| 12.16 | Air logistics orbit | Air route configured and active | Aircraft orbits waypoints; route markers on F10 | |
| 12.17 | Sea route | Naval route configured (if applicable) | Ships move route; supply delivered on arrival | |
| 12.18 | Warehouse supply increase | After delivery, check warehouse | `-admin log-warehouse objective <airbase>` (or the `query-warehouse` RPC) shows increased fuel/ammo | |
| 12.19 | DCS warehouse sync | After engine delivery, check DCS F10 rearm menu | Fuel/ammo available for rearming at that airbase | |

---

## 13. Chat Commands (chatcmd.rs)

Commands start with `-`. Chat that starts with `-` but not a letter (`-_-`) is ordinary chat.

| # | Item | Steps | Expected | Pass |
|---|------|-------|----------|------|
| 13.1 | -help | Type `-help` | Command list; admins also see the `-admin` line | |
| 13.2 | -switch blue | In spectators, type `-switch blue` | Side set to Blue; confirmation in chat | |
| 13.3 | -switch red | In spectators, type `-switch red` | Side set to Red; confirmation in chat | |
| 13.4 | -lives | Type `-lives` | Life counts for all types shown for player's side | |
| 13.5 | -time | Type `-time` | Time until the server restart | |
| 13.6 | -balance | Type `-balance` | Player's current point balance shown | |
| 13.7 | -status | Type `-status` | Campaign status: objectives, convoys, streak | |
| 13.8 | -transfer valid | `-transfer 100 <player>` | 100 points moved; both players notified | |
| 13.9 | -transfer insufficient | `-transfer 999999 <player>` | Refused; balance unchanged | |
| 13.10 | -transfer to objective | `-transfer 100 objective:<objective>` | Points moved to the objective | |
| 13.11 | -transfer self | `-transfer 100 <own-name>` | Refused or no-op; no crash | |
| 13.12 | -action | `-action help`, then `-action <name> <args>` | Action list with costs; action executes if valid for the player's slot | |
| 13.13 | -bind | `-bind <uuid>` with the uuid from the dashboard's login page | Account linked; dashboard login completes | |
| 13.14 | -delete | `-delete <groupid>` on a group you deployed | Group deleted; partial refund | |
| 13.15 | -weather | Type `-weather` in a slot | Weather for your slot, including winds and temperature aloft | |
| 13.16 | -brief | Type `-brief` | Situational briefing (full report under F10 > Info > Situation) | |
| 13.17 | -jtac | `-jtac list`, then `-jtac help` | JTACs listed; JTAC chat commands listed | |
| 13.18 | -gci | `-gci`, then `-gci off` and `-gci on` | Current GCI voice settings; calls stop and resume | |
| 13.19 | -hq / -request | `-hq`, `-hq ops`, `-request <kind>` | Commander's intent and operations; support request sent to HQ | |
| 13.20 | Unknown command | Type `-asdfghjkl` | "-asdfghjkl is not a valid command. Valid commands follow." plus the help list | |
| 13.21 | Malformed args | `-transfer` (no args) | Treated as unknown: message plus the help list; no crash | |
| 13.22 | Admin prefix | `-admin help` as admin | Admin command list | |
| 13.23 | Admin prefix non-admin | `-admin help` as regular player | No reply; nothing happens | |

---

## 14. Admin Commands (admin.rs)

Chat commands are `-admin <command>` from an admin UCID; `-admin help` lists them. The query and API items are netidx RPCs under bflib's `api/` path (what bfdb and the bot call), not chat commands.

### Player Management

| # | Item | Steps | Expected | Pass |
|---|------|-------|----------|------|
| 14.1 | switch | `-admin switch red <player>` | Player moved to red side; confirmation shown | |
| 14.2 | ban timed | `-admin ban 2h <player>` | Player kicked; reconnect within 2h refused | |
| 14.3 | ban permanent | `-admin ban forever <player>` | Reconnect refused: "you are banned forever" | |
| 14.4 | unban | `-admin unban <player>` | Ban lifted; player can reconnect | |
| 14.5 | banned | `-admin banned` | All currently banned players listed | |
| 14.6 | kick | `-admin kick <player>` | Player disconnected immediately | |
| 14.7 | connected | `-admin connected` | All online players listed | |
| 14.8 | search | `-admin search <regex>` | Matching players listed (case-insensitive) | |
| 14.9 | reset-lives | `-admin reset-lives <player>`; `-admin reset-lives-all` | Lives reset to config defaults for one player / for everyone | |
| 14.10 | lives | `-admin lives <player> recon 2` | Player has exactly 2 recon lives | |
| 14.11 | add-admin | `-admin add-admin <player>` | Player gains admin; can use admin commands | |
| 14.12 | remove-admin | `-admin remove-admin <player>` | Admin removed | |
| 14.13 | balance | `-admin balance <player>` | Player's exact point total shown | |
| 14.14 | set-points | `-admin set-points 500 <player>` | Player points set to exactly 500 | |
| 14.15 | addpoints | `-admin addpoints <player> -50` | 50 points taken away | |
| 14.16 | deslot | `-admin deslot <player>` | Player forced to spectators; slot freed | |
| 14.17 | broadcast | `-admin broadcast test` | Every player gets the text as chat and a panel message | |

### Object Management

| # | Item | Steps | Expected | Pass |
|---|------|-------|----------|------|
| 14.18 | delete group | `-admin delete <groupid>` | Group despawned from DCS; removed from DB | |
| 14.19 | remark | `-admin remark <objective>` | Objective markup on the F10 map redrawn | |
| 14.20 | capture | `-admin capture <objective> red` | Objective changes hands | |
| 14.21 | spawn | F10 mark `<key> deployable blue 90 <name>`, then `-admin spawn <key>` | Deployable spawns at the mark; mark removed | |
| 14.22 | whythreat | `-admin whythreat <objective>` | Why it isn't repairing: threat units, capture timer, logistics, supply, next pulse | |
| 14.23 | ghosts | `-admin ghosts`, then `-admin purge-ghosts` | Units with no DCS object listed, then marked dead | |
| 14.24 | supply | `-admin reduce <objective> 50`, `-admin transfer <from> <to>`, `-admin tick`, `-admin deliver` | Supplies drop by half; move between objectives; a logistics tick and a delivery run now | |
| 14.25 | repair | `-admin repair <airbase>` | One repair step at that airbase | |
| 14.26 | log-desc | `-admin log-desc` in a slot | getDesc of your aircraft written to the log | |
| 14.27 | shutdown confirm | `-admin shutdown` | Asks for `confirm` within 30 s; nothing else happens (see 22.4) | |

### Query RPCs

| # | Item | Steps | Expected | Pass |
|---|------|-------|----------|------|
| 14.28 | query-objectives | Call `query-objectives` | All objectives as JSON with side, health, supply | |
| 14.29 | query-objective | Call `query-objective` with a name | Full detail for that objective | |
| 14.30 | query-players / query-player | Call each | All registered players; one player's points, side, slot | |
| 14.31 | query-groups / query-group / query-units | Call each | Groups for a side; one group's units, origin, health | |
| 14.32 | query-warehouse | Call `query-warehouse` for an objective | Fuel and ammo inventory | |
| 14.33 | query-logistics | Call `query-logistics` | Convoys, air routes, sea routes and their states | |
| 14.34 | query-campaign-state | Call `query-campaign-state` | Campaign state as JSON | |

### API RPCs

| # | Item | Steps | Expected | Pass |
|---|------|-------|----------|------|
| 14.35 | spawn-deployable | Call `spawn-deployable` (side, name, x, z, heading) | Deployable group spawns there | |
| 14.36 | spawn-troop | Call `spawn-troop` | Troop group spawns | |
| 14.37 | move-group | Call `move-group` | Group issued move waypoint | |
| 14.38 | add-points | Call `add-points` with a reason | Points added; log records reason | |

---

## 15. Campaign Events (db/events.rs)

| # | Item | Steps | Expected | Pass |
|---|------|-------|----------|------|
| 15.1 | Schedule future event | Trigger scenario that schedules event | Event appears in scheduler with future timestamp | |
| 15.2 | Event fires on time | Wait until event due time | `process()` returns effect; effect applied in-game | |
| 15.3 | ObjectiveCapture effect | Trigger objective capture event | Objective `side` field flips; map updated | |
| 15.4 | SpawnGroup effect | Event with SpawnGroup payload | Group spawns in DCS at specified position | |
| 15.5 | DespawnGroup effect | Event with DespawnGroup payload | Group removed from DCS | |
| 15.6 | PointsAward effect | Points award event fires | Correct side receives points; log confirms | |
| 15.7 | AirbaseOpened effect | Airbase open event fires | Slots at that airbase become available | |
| 15.8 | No re-fire | Already-executed event ID checked | Executed events do not fire again after restart | |
| 15.9 | nearest_friendly | Check supply routing on multi-objective map | Helper returns closest friendly objective, not enemy | |

---

## 16. Smart Commander (commander.rs)

| # | Item | Steps | Expected | Pass |
|---|------|-------|----------|------|
| 16.1 | Treasury income | Wait one commander tick cycle | Treasury value increases by configured income rate | |
| 16.2 | Objective funding | Treasury above threshold | Objectives receive funding; units replenish or repair | |
| 16.3 | Holding bonus | Hold objective for configured duration | Side receives bonus points | |
| 16.4 | Overspend guard | Treasury low, multiple objectives need funding | Commander does not go negative; funding is rationed | |
| 16.5 | No objectives crash | One side holds zero objectives | Commander tick completes without panic | |

---

## 17. Frontline System (frontline.rs)

| # | Item | Steps | Expected | Pass |
|---|------|-------|----------|------|
| 17.1 | Initial frontline | Load mission with mixed objectives | Frontline calculated from objective positions | |
| 17.2 | Frontline update on capture | Capture objective | Frontline recalculates; netidx updated | |
| 17.3 | Frontline on F10 | Check F10 map | Frontline line visible between opposing objectives | |
| 17.4 | All objectives one side | One side captures everything | Frontline gracefully handles edge case (empty or full-map line) | |

---

## 18. F10 Map Layer (db/map_layer.rs)

| # | Item | Steps | Expected | Pass |
|---|------|-------|----------|------|
| 18.1 | SAM kill ring | Deploy SAM unit | Red/orange circle showing kill radius visible on F10 (own side) | |
| 18.2 | SAM search ring | Same SAM unit | Outer search ring drawn at larger radius | |
| 18.3 | SAM ring hidden | Switch to enemy side | SAM rings NOT visible to enemy | |
| 18.4 | EWR ring | Deploy EWR unit | Detection radius circle drawn on F10 (own side only) | |
| 18.5 | Convoy route visible to all | Active ground convoy | Convoy route line + arrow visible to BOTH sides (interdiction targets) | |
| 18.6 | Air logistics orbit | Air route active | Orbit rectangle + text visible on F10 (own side only) | |
| 18.7 | Sea route markers | Naval route active | Route line + ship icons visible (own side only) | |
| 18.8 | Fire mission overlay | Artillery/strike in progress | Trajectory line + impact circle drawn on F10 | |
| 18.9 | Mission AO | Mission created with AO | Quad area + ingress arrow visible (own side) | |
| 18.10 | CSAR search area | Pilot downed | Circle around pilot with countdown timer text (own side) | |
| 18.11 | JTAC layer | JTAC has active target | Lase-range circle, target dot, bearing line, 9-line text visible | |
| 18.12 | Markings clear on despawn | Destroy SAM, convoy arrives, etc. | Associated map markings removed cleanly | |
| 18.13 | No marking leaks | Run mission for 30+ min | No orphaned marks accumulate on F10 map | |

---

## 19. CSAR System

| # | Item | Steps | Expected | Pass |
|---|------|-------|----------|------|
| 19.1 | Pilot death triggers spawn | Player aircraft shot down (PilotDead) | CSAR beacon/unit spawns at crash position | |
| 19.2 | Ejection triggers spawn | Player ejects | CSAR beacon spawns at ejection position | |
| 19.3 | Renotify broadcast | Wait `renotify_interval` minutes | All friendly helicopter pilots receive position update in chat | |
| 19.4 | Pickup radius trigger | Helo lands within `pickup_radius` of pilot | Pilot unit walks toward helicopter | |
| 19.5 | Auto-board | Helo within `board_radius` | Pilot boards automatically; added to helo cargo | |
| 19.6 | Delivery reward | Land at friendly base with pilot cargo | `rescue_reward` points awarded to pilot | |
| 19.7 | Enemy capture radius | Enemy unit enters `enemy_capture_radius` | Pilot captured/eliminated; CSAR cancelled | |
| 19.8 | Search party spawn | `search_party_size > 0` in config | Enemy infantry spawns near pilot down location | |
| 19.9 | Capture timer | Wait `capture_timer` minutes with no rescue | Pilot auto-captured; CSAR beacon removed | |
| 19.10 | Capture timer disabled | `capture_timer = 0` | Pilot remains indefinitely until rescued or enemy captured | |
| 19.11 | Smoke cooldown | Request smoke, wait, request again | Second request fails within `smoke_cooldown` seconds; works after | |
| 19.12 | CSAR disabled | `enabled = false` in config | No beacon spawns on pilot death | |

---

## 20. Background Tasks (bg/)

| # | Item | Steps | Expected | Pass |
|---|------|-------|----------|------|
| 20.1 | State save | Wait for save interval | State file updated on disk; timestamp changes | |
| 20.2 | State restore | Kill server, restart | Campaign state (objectives, players) restored from save | |
| 20.3 | State rotation | Multiple saves | Previous state backed up; rotated file exists in backup dir | |
| 20.4 | Log rotation | Wait for log rotation interval | Compressed log archive created; active log continues | |
| 20.5 | Stats publish | Check netidx/bfdb | Kill stats, sortie data flowing to bfdb | |
| 20.6 | Perf metrics | Check perf log | Frame times, event counts recorded | |
| 20.7 | RPC handler | Send RPC command via netidx client | Admin action executes; response received | |
| 20.8 | Logger output | Check log file path | Log lines appear with correct timestamps and levels | |

---

## 21. Points & Scoring

| # | Item | Steps | Expected | Pass |
|---|------|-------|----------|------|
| 21.1 | Kill points | Destroy enemy unit | Attacker's point balance increases | |
| 21.2 | Points correct value | Kill a unit with known point value | Exact configured point value added | |
| 21.3 | Sortie points | Accumulate points during sortie | `instanced_player.points` increments during flight | |
| 21.4 | Points on land | Land with positive sortie points | Points transferred to career total | |
| 21.5 | Points on death | Die with positive sortie points | Points may be lost or transferred (verify config behavior) | |
| 21.6 | Periodic award | Wait for `award_periodic_points()` cycle | All players on winning side receive periodic points | |
| 21.7 | Transfer | `-transfer 50 <player>` | Sender -50, receiver +50 | |
| 21.8 | Deploy cost | Deploy item with `cost = 100` | 100 points deducted from deploying player | |
| 21.9 | Repair cost | Repair damaged deployable | `repair_cost` deducted | |
| 21.10 | Score tracking | Accumulate kills and deaths | The pilot's stats on the dashboard show the same kills and deaths | |

---

## 22. Auto-Shutdown

| # | Item | Steps | Expected | Pass |
|---|------|-------|----------|------|
| 22.1 | Monitor player count | All players leave server | `check_auto_shutdown()` starts countdown timer | |
| 22.2 | Shutdown after timeout | Remain empty for full timeout duration | Server process exits cleanly | |
| 22.3 | Cancel on join | Player joins before timeout expires | Shutdown cancelled; server continues running | |
| 22.4 | Admin shutdown | `-admin shutdown`, then `-admin shutdown confirm` within 30 s | First asks for confirmation; second shuts the server down | |

---

## 23. Spawn Context & Templates

| # | Item | Steps | Expected | Pass |
|---|------|-------|----------|------|
| 23.1 | Airbase spawn | Spawn group at airbase objective | Units appear on airbase ramp/parking | |
| 23.2 | Pos spawn | Spawn group at arbitrary coordinates | Units appear at specified position | |
| 23.3 | Road spawn | Spawn ground group near road objective | Units placed on nearest road segment | |
| 23.4 | Offshore spawn | Spawn naval group | Units in water, correct formation | |
| 23.5 | Transport spawn | Spawn transport-role group | Units at designated transport waypoint | |
| 23.6 | Heading applied | Spawn with specific heading | Units face correct direction | |
| 23.7 | Speed applied | Spawn moving unit | Unit initialized at configured speed | |
| 23.8 | Formation | Spawn multi-unit group | Units in correct formation (line, wedge, etc.) | |
| 23.9 | Missing template | Reference non-existent template | Error logged gracefully; no panic | |
| 23.10 | Template lookup | `find_ground_template()` called | Returns correct template for requested vehicle type | |

---

## 24. Message Queue (msgq.rs)

| # | Item | Steps | Expected | Pass |
|---|------|-------|----------|------|
| 24.1 | Chat to player | Trigger event that sends chat to one player | Only that player sees message | |
| 24.2 | Popup to all | Event triggers all-player popup | Every connected player sees popup | |
| 24.3 | Popup to side | Event triggers side-specific popup | Only blue or red players see it (not both) | |
| 24.4 | Panel message | Trigger panel message | Text appears in mission briefing panel area | |
| 24.5 | Draw map mark | Add mark line via queue | Line appears on F10 map | |
| 24.6 | Clear mark by ID | Remove a mark | Line removed from F10 map | |
| 24.7 | Queue order | High-priority + low-priority queued together | High-priority messages processed first | |
| 24.8 | Queue drain | Many messages queued in one frame | All processed without frame overflow/crash | |

---

## 25. Regression / Edge Cases

| # | Item | Steps | Expected | Pass |
|---|------|-------|----------|------|
| 25.1 | Empty server frame | All players leave, wait 10+ frames | No errors in log during idle frames | |
| 25.2 | Zero-health objective | Destroy all defenders without capturing | Server handles `health = 0` state without panic | |
| 25.3 | Orphaned unit | Kill all but one unit in a group, then kill the group reference | No orphaned UnitId in maps | |
| 25.4 | Disconnect mid-flight | Player disconnects at cruise altitude | Slot freed; no stale slot data | |
| 25.5 | Disconnect with cargo | Player disconnects while carrying crates | Cargo not duplicated or lost permanently | |
| 25.6 | Mission restart | Restart DCS mission (not server) | `init_miz()` runs cleanly; no double-init | |
| 25.7 | UntilRestart persist | Deploy `PersistTyp::UntilRestart` deployable, restart | Deployable gone after restart | |
| 25.8 | Forever persist | Deploy `PersistTyp::Forever` deployable, restart | Deployable restored after restart | |
| 25.9 | WallTime persist | Deploy `PersistTyp::WallTime(300)`, wait | Deployable removed after 5 real minutes | |
| 25.10 | Restarts persist | Deploy `PersistTyp::Restarts(2)`, restart twice | Removed after second restart | |
| 25.11 | DeleteOldest limit | Deploy at limit, deploy one more | Oldest instance removed; new one spawns | |
| 25.12 | DenyCrate limit | At deployable limit, try to pick up new crate | Pickup refused; "limit reached" message | |
| 25.13 | Simultaneous player actions | Two players trigger actions within same DCS frame | Both handled; no data race (Lua is single-threaded, verify no cross-contamination) | |
| 25.14 | Large map with many objectives | Load map with 20+ objectives | No performance degradation; frame times acceptable | |
| 25.15 | Save on full map | Save state with all objectives, groups, players | State file written and re-loadable without corruption | |

---

## 26. Training Range (bfrange)

A range server set up as in `deploy/range.md`, flying the generated `VS_Range_Marianas.miz` (sortie `VSRANGE`). The "First flight" list in `deploy/range.md` is a 20-minute smoke test through most of this section. Grades and thresholds are in `bfrange/README.md`.

### 26A. Server and Engine

| # | Item | Steps | Expected | Pass |
|---|------|-------|----------|------|
| 26A.1 | Engine loads | Start the range server from the bot | `dcs.log` shows `loading bfrange.dll`; `Logs\bfrange.txt` shows `range engine running` and one line per station, tanker and carrier | |
| 26A.2 | No campaign hooks | Look in `DCS.range\Scripts\Hooks` | `bfrange_hooks.lua` is there, bflib's hook loader is not (with it every slot is refused) | |
| 26A.3 | Open slots | Take a dynamic-spawn slot at Andersen, a CVN-72 deck slot and a Combined Arms slot | Each is granted; no lives, points or side checks | |
| 26A.4 | Bad config | Break the JSON in `VSRANGE_RANGE`, restart (restore it afterwards) | Every player sees **THE RANGE CANNOT START** with the reason | |
| 26A.5 | Missing item | Point one config item at a zone that does not exist | Engine logs it and runs without that item | |
| 26A.6 | Range status | F10 > Range > Range status | The sector you are in; stations, tankers and both carriers with bearing and range | |
| 26A.7 | Sectors | F10 > Range > Sectors: what is where; open the F10 map | Sectors nearest first with bearing, range and job; the map shows the same areas | |
| 26A.8 | Sector entry | Fly into R-7201 FDM | `Entering R-7201 FDM (AIR-TO-GROUND)`; not repeated for that sector within 15 min | |
| 26A.9 | Carriers | Check CVN-72 and LHA-1 | Steaming into the wind; TACAN 72X / 71X, ICLS 11 / 12, Link-4 336.0; `bfrange.txt` logs the wind seen and course chosen | |
| 26A.10 | Netidx | `netidx resolver list /local/fowl/range` | The range publishes; `/range status` and the site's live map have data | |
| 26A.11 | Live weather (if `live_weather` is set) | Restart DCS | `[LIVE_WX]` lines in `bfrange.txt`; mission weather and start time follow the real METAR and clock; a failed fetch is logged and the mission loads as it was | |
| 26A.12 | Kept out of campaign stats | Fly a range sortie, open the campaign dashboard | Not in the leaderboard or pilot lifetime totals; the range is not in the dashboard's server selector | |
| 26A.13 | Engine update | Drop `bfrange.dll` in the bot's admin channel | Staged only in the range instance's `_staging`; `/feops stage_status` shows it; swapped in at the next DCS start, not on a mission restart | |

### 26B. Drills

| # | Item | Steps | Expected | Pass |
|---|------|-------|----------|------|
| 26B.1 | Bombing | Drop one bomb on the R-7201 bomb circle | `RANGE R-7201 FDM - Bomb Circle: ... o'clock` in the cockpit; `RESULT <id>: ...` in `bfrange.txt`; grade from miss distance (SHACK to POOR) | |
| 26B.2 | Station tools | Air-to-Ground > Stations > (station): Info / bearing, Smoke the target, Reset targets | Info shown; smoke on the target; targets respawn | |
| 26B.3 | Strafe | Strafe the FDM pit, opening fire outside the foul line | Rounds fired, hits and accuracy grade | |
| 26B.4 | Foul line | Keep firing inside 610 m | Pass voided | |
| 26B.5 | Tiered targets | Bomb an easy, a medium and a hard target at TN-2 | Scored as bombs; the card carries the tier | |
| 26B.6 | GPS jammer | Release a GPS weapon inside the EW-1 jammer radius | Warning on entering the radius; card says **GPS denied** | |
| 26B.7 | CAS drill | Air-to-Ground > CAS / JTAC > Check in | JTAC lases and passes a nine-line; first impact scored on time, miss distance and right target | |
| 26B.8 | SEAD / IADS | Fly into S-1 Rota; fire an ARM at a site; kill an emitter | SAM radars dark until the network sees you; the ARM's target goes dark for 45 s; kill graded 5 / 4 / 3 | |
| 26B.9 | BFM set-up | Air-to-Air > Set-up selection: MiG-29S, Fox 2, BFM offensive; FIGHT'S ON | Adversary spawns as selected; WIN / LOSS / DRAW at the end | |
| 26B.10 | Missile trainer | Missile trainer ON; let a missile reach you | Missile deleted inside 200 m (500 m for big warheads) with a "you'd be dead" message; result recorded | |
| 26B.11 | Missile defeated | Defeat a missile | `DEFEATED` result with reaction time and time hot / beaming / dragging | |
| 26B.12 | Duel | Two players: Duel (PvP) > Challenge nearest player, Accept challenge; later Cancel / knock it off | Engagement starts; WIN / LOSS / DRAW; cancel ends it | |
| 26B.13 | AAR | Join Arco 1 on AR MIRAGE (TACAN 52Y, 252.0), plug, leave | `contact` / `disconnect` calls; A-F grade after leaving | |
| 26B.14 | Tanker on position | Tankers / Carrier > Tanker on my position | Tanker spawns near you; shown under My spawns | |
| 26B.15 | Carrier pass | One pass to CVN-72 | Trap card with DCS's LSO grade (ours if DCS did not grade), the decoded comment and the trap sheet; Tankers / Carrier > My last pass shows it | |
| 26B.16 | Anti-ship | Anti-ship > (ship) 30 nm ahead, then attack it | Ship spawns ahead; HIT / MISS, damage and launch range | |
| 26B.17 | Helicopter pad | Land on the H-1 Andersen Precision Pad | Distance from the mark, descent, heading; PERFECT inside the radius; a hard landing caps the grade | |
| 26B.18 | Dynamic cargo | Ask the ground crew (F8) for cargo, load it, set it down at a pad or DZ/LZ | `CARGO ... delivered to ...` and a "Cargo delivery (internal)" card, with no F10 step | |
| 26B.19 | Sling and troops | Helicopter > Sling-load courses; Troops: load at ..., then Troops: unload here in the LZ | Sling load graded once on the ground and still; troop landing graded | |
| 26B.20 | CSAR | Helicopter > CSAR > (area) > Pilot down (cold); find him, land within 50 m for 8 s, fly home | Smoke (flare at night) inside 3 km; graded on time home (PERFECT inside 20 min) | |
| 26B.21 | Airdrop | C-130J: drop a package over an Airlift drop zone | Release height, speed and heading recorded; landing point scored against the zone | |
| 26B.22 | Low-level route | Fly LL-1 from gate 1 below 500 ft AGL | Gates timed; points off for missed gates, time above the ceiling and time on target | |
| 26B.23 | Field landing | Land on any runway; then a touch-and-go | Landing graded (aim point, centreline, sink rate); a take-off within 15 s makes it a touch-and-go | |
| 26B.24 | Ship deck | Land a helicopter on an FD-1 frigate | Graded in the ship's frame; card carries the ship's speed | |
| 26B.25 | CA gunnery | From a Combined Arms slot, engage the G-1 gunnery lane | Result is the fraction of the lane killed | |
| 26B.26 | Hot zone | Fly into HZ-1 Saipan | AI CAP launches; Air-to-Air > Hot zone: AWACS picture works; one card for the trip | |
| 26B.27 | My spawns | My spawns > List, then Despawn all | Your spawns listed, then removed | |

### 26C. Results, Discord and Website

| # | Item | Steps | Expected | Pass |
|---|------|-------|----------|------|
| 26C.1 | Results feed | Drop one bomb | Within ~15 s the embed with its debrief card is in `range_results_channel`, with Debrief, 3D replay, Web Tacview and Tacview clip buttons | |
| 26C.2 | Greenie channel | Trap on CVN-72 | Also posted to `greenie_channel` | |
| 26C.3 | No repost | Restart the bot | Results already posted are not posted again | |
| 26C.4 | Status embed | Watch `range_status_channel` | One embed, edited every 2 min: tankers, carriers with BRC/FB and wind over deck, stations, who is flying | |
| 26C.5 | /range commands | `/range status`; `/range me` (after `/linkme`); `/range greenie` | Live status; a link to your page on the range site; top 10 of the last 30 days | |
| 26C.6 | Campaign-only commands | `/fe_objective` for the range server | Answers that it is a range server | |
| 26C.7 | Live page | Open range.vectorstrike.org | Live map with sectors, players, stations, tankers and carriers; the top bar shows the range live | |
| 26C.8 | Debrief | Open a result from the feed | 2D plot and 3D replay, the card, card PNG, Tacview clip | |
| 26C.9 | Lists | `/results`, `/greenie`, `/leaderboards` | New results listed; `/results` filters stay in the URL | |
| 26C.10 | Spawn page | Logged in with a linked DCS account: spawn a tanker at your position, then despawn it | Tanker appears in game and goes again; a second request within 3 s is refused | |
| 26C.11 | Spawn logged out | Open `/spawn` without logging in | Spawning refused | |
| 26C.12 | Brief | Open `/brief`, print it | Frequencies, TACANs, weather, targets with coordinates, laser codes; prints as a kneeboard card | |
| 26C.13 | Server down | Stop the range server | Site shows "No live data"; results still open | |

---

## 27. Cockpit Plugin (bfcockpit)

Client side, on a player PC, flying on a campaign server. `bfcockpit/README.md` describes every item.

### 27A. Install

| # | Item | Steps | Expected | Pass |
|---|------|-------|----------|------|
| 27A.1 | Profiles found | Run `VectorStrike-Cockpit-Install.exe` with DCS closed | Every `DCS*` profile with a `Config` folder listed, all ticked | |
| 27A.2 | Add profile | **Add profile...**, point at a profile somewhere else | Added and installed into | |
| 27A.3 | Files | Check the profile after **Install** | `Scripts\Hooks\bfcockpit.lua` and `Mods\Services\BFCockpit\bin\bfcockpit_net.dll`; `Config\BFCockpit.lua` after the first run | |
| 27A.4 | Load log | Start DCS, search `dcs.log` for `BFCOCKPIT` | Version, DLL loaded, VR detected or not, the bound key | |
| 27A.5 | CLI | `bfcockpit-install-cli.exe --list`, `--yes`, `--uninstall` | Lists profiles; installs into all without asking; removes the plugin | |
| 27A.6 | Settings kept | Uninstall, then reinstall | Window position, opacity and key come back | |

### 27B. Panel and Wheel

| # | Item | Steps | Expected | Pass |
|---|------|-------|----------|------|
| 27B.1 | Open | In a slot press the Comms key; then `Ctrl+Alt+C` twice | Panel opens; `Ctrl+Alt+C` shows and hides it from anywhere | |
| 27B.2 | Key handshake | Open the panel for the first time on a server, with a second player watching chat | The `-cockpitkey` line is not seen by anyone and not in `dcs.log`; the panel starts answering | |
| 27B.3 | Reconnect | Disconnect, reconnect, open the panel | Works again with a new key | |
| 27B.4 | MENU tab | Go into a submenu, run a command, `< Back` | Same items as the F10 menu; the reply shows in the box at the top; the list scrolls instead of `More >>` | |
| 27B.5 | CARGO tab | At a logistics base: SPAWN 1, Load, Unload, Unpack | Same results and same refusals as the F10 commands; weight and room shown | |
| 27B.6 | STATUS tab | Open STATUS | Side, points, lives, crates; aircraft, position, altitude, heading, speed; nearest objective with range and bearing | |
| 27B.7 | EWR tab | Toggle EWR calls and imperial/metric | Picture shown; toggles take effect | |
| 27B.8 | LOG tab | Receive messages while on another tab | LOG tab counts them; newest first, tagged YOU / SIDE / ALL | |
| 27B.9 | Window | `-` / `+`, drag, resize, X; restart DCS | Opacity, position and size remembered | |
| 27B.10 | Wheel | `Ctrl+Alt+W`, point at a group, move onto its arc, click | Group fans out on an arc; the picked command runs; the reply shows under the wheel | |
| 27B.11 | Wheel safety | Pick the hub; pick a destroy or delete item | Hub goes back a level or closes; the destructive item needs a second pick | |
| 27B.12 | Wheel keys | With the wheel open: arrows, Enter, Backspace; then with it closed | Steps round the ring, onto arcs, fires, goes back; keys not taken while closed | |
| 27B.13 | Custom wheel | ☆ a command in MENU; edit it in the WHEEL tab; switch to a helicopter | Command on the wheel; edits kept per aircraft type; the helicopter starts with the logistics preset | |
| 27B.14 | Special page | Options > Special > Vector Strike: change the show/hide key, wheel key and panel size mid-mission | Applied within a few seconds | |
| 27B.15 | VR | Fly in VR | Panel larger and centred; the wheel opens where you look; head pointing works | |
| 27B.16 | DLL missing | Rename `bfcockpit_net.dll`, start DCS | Panel says `bfcockpit_net.dll is missing -- re-run the Vector Strike installer` | |
| 27B.17 | Server unreachable | Set `api` in `Config\BFCockpit.lua` to a host that doesn't answer | Panel says it can't reach the campaign server and gives the reason | |
| 27B.18 | Two servers | Join a second campaign server | The panel shows that server's menu | |

### 27C. Updates

| # | Item | Steps | Expected | Pass |
|---|------|-------|----------|------|
| 27C.1 | Release | Check GitHub release `cockpit-v<version>` | `VectorStrike-Cockpit-Install.exe`, `VectorStrike-Cockpit-<version>.zip` and a `.minisig` for each | |
| 27C.2 | Installer link | Open `<bfdb>/api/cockpit/plugin/installer` | Points at the installer for the version bfdb ships | |
| 27C.3 | Version check | Older plugin, join a server whose bfdb ships a newer one | `dcs.log`: `server has plugin <new>, this is <old>` | |
| 27C.4 | Download | **Update automatically** on (the default) | Status line `Downloading version <v>...`, then `Version <v> is downloaded -- it installs when you close DCS` | |
| 27C.5 | Install on close | Close DCS, start it again | `Logs\bfcockpit-update.log` written; the `BFCOCKPIT` line shows the new version | |
| 27C.6 | Auto-update off | Untick **Update automatically**, rejoin | Status line says the version is available; nothing downloads | |
| 27C.7 | Release missing | bfdb advertises a version with no GitHub release | `auto-update failed` in `dcs.log`; nothing runs; the panel keeps working | |

### 27D. WIKI tab

Needs plugin 3.5 (script and `bfcockpit_net.dll` 1.1) and a bfdb with `/api/cockpit/wiki/*`.

| # | Item | Steps | Expected | Pass |
|---|------|-------|----------|------|
| 27D.1 | Tab bar | Smallest panel (drag it as narrow as it goes, then reopen), Panel size 100% and 70% | Seven tabs, every label readable, none under the `-` `+` `X` buttons | |
| 27D.2 | Home | Open WIKI | The sections in the web wiki's sidebar order, each with its page count | |
| 27D.3 | Section and page | Pick a section, then a page; `< Back` twice | Pages in the sidebar's order; the crumb reads `WIKI > Section > Title`; Back returns to the section (at the same scroll position) and then home | |
| 27D.4 | Markdown | Open a page with headings, lists, a table and a code block (e.g. Theatre HQ, Chat Commands) | Headings bold and green, lists indented, tables as columns (or one card per row when the panel is narrow), code in the mono font; no `**`, `[`, `](` or backticks left | |
| 27D.5 | Links | On Welcome, click the button under a paragraph that links to another page | That page opens; Back comes back. An outside link shows its address in the text | |
| 27D.6 | Server numbers | Open Points and Lives on two servers with different engine configs | The numbers differ per server; `Numbers on this page are <server>'s` at the bottom; with bfdb's engine config unset, the page's fallback numbers | |
| 27D.7 | Pictures (JPEG) | Open Welcome (server browser screenshot) | The picture loads after a moment, panel-wide with its own shape; `Mods\Services\BFCockpit\cache\wiki\img\<id>-<w>-<W>x<H>.jpg` written | |
| 27D.8 | Pictures (SVG) | Open a page with an SVG diagram | Shown as a picture (rasterised PNG in the cache); readable on the panel's dark background | |
| 27D.9 | Picture cache | Close DCS, start it again, reopen the page | Pictures appear at once; no new file in the cache folder | |
| 27D.10 | Page cache | Reopen a page; then edit it in bfwiki and reopen it within 5 minutes of the edit on the panel | First: no request in `dcs.log` with `debug = true`; second: the edited text | |
| 27D.11 | Custom blocks | Open pages with the F10 menu, panel, wheel, steps and DCS message mockups | F10 menu as an indented tree with what each command answers; steps numbered; panel and wheel as their rows and labels | |
| 27D.12 | Search | Type `cargo` in the box | Titles with `cargo` at once; after a pause, pages that mention it, with a snippet; Back or `X` clears the search | |
| 27D.13 | Keyboard | Click into the search box, then switch to MENU or close the panel | Typing goes to the box while it has focus; afterwards keys reach the cockpit again | |
| 27D.14 | Offline | Open WIKI in single player | The wiki works (no key or server check needed) | |
| 27D.15 | Old DLL | Copy the 1.0 `bfcockpit_net.dll` over the new one | Pages work; pictures show `[picture -- needs the newer bfcockpit_net.dll]` with their caption; status line warns about the DLL | |
| 27D.16 | Wheel shortcut | WHEEL tab: + PANEL > WIKI; open the wheel and pick it | The panel opens on the WIKI tab | |
| 27D.17 | No sim stalls | Open several picture-heavy pages quickly in a busy mission | No frame hitch while pages and pictures load | |

---

## 28. Auto-Update and Server Ops

On the server box. `deploy/auto-update.md` is the reference; `DCSServerBot/plugins/fowlengine/README.md` lists the Discord side.

### 28A. Releases, Staging, Probation and Rollback

| # | Item | Steps | Expected | Pass |
|---|------|-------|----------|------|
| 28A.1 | Dry run | `.\deploy\publish-release.ps1 -DryRun` | Builds `bflib.dll`, `bfrange.dll`, `bfdb.exe`, `bftools.exe` and `manifest.json` with a sha256 per file | |
| 28A.2 | No key, no publish | Publish without a signing key | Refused | |
| 28A.3 | Check now | `autoupdate.enabled: false`; OPS page > **Check now** | Latest release and what would be staged; nothing staged | |
| 28A.4 | Unsigned | Remove `autoupdate.public_key`, or serve a release whose `manifest.json.sig` doesn't verify | Nothing fetched or staged | |
| 28A.5 | Staging | A signed release arrives | `bflib.dll.pending` with a `source: autoupdate` sidecar in each campaign server's staging dir; `bfdb.exe` / `bftools.exe` in the global one; announced in `ops_channel` | |
| 28A.6 | Manual upload | Drop `bflib.dll` in the admin channel | Posted to `ops_channel` with its sha256; Apply when empty / Restart now buttons; the upload message deleted once on disk | |
| 28A.7 | Manual wins | Release arrives while a manual upload is staged | The manual upload is kept | |
| 28A.8 | when_idle | `apply: when_idle`, server empty | Applied after `idle_minutes` (inside `apply_window` if set) with a DCS stop/start | |
| 28A.9 | Mission restart | Staged DLL, mission restart only | Old DLL stays loaded until a full DCS restart | |
| 28A.10 | DLL probation | After a swap | Passes once `Logs\bfnext-bflib-build.json` is newer than the swap and the server has run `probation_minutes` | |
| 28A.11 | DLL rollback | Kill DCS during probation | Backup staged back and DCS restarted on it; bad build kept as `<dll>.failed-<ts>`; release marked bad; `ops_channel` says so | |
| 28A.12 | Restart is not a crash | Scheduled restart or admin shutdown during probation | No rollback | |
| 28A.13 | bfdb probation | New `bfdb.exe` | Passes after `probation_minutes` answering `/api/health`; one that exits or never answers is rolled back together with its DB snapshot | |
| 28A.14 | Allow again | OPS page **Allow again**, or `/feops update_unmark` | The bad release is offered again | |
| 28A.15 | Campaign pack | `autoupdate.campaigns: true`; release built with `-Campaigns` | Pack staged; written at the next DCS start after a backup in `<instance home>\_fowl_campaign_backups\<ts>-<tag>\` | |
| 28A.16 | Held pack | Edit the server's `*_CFG`, publish a pack | Pack held, `ops_channel` says why; **Apply** / **Keep server's** on the OPS page | |
| 28A.17 | Held pack in Discord | `/feops campaign_apply server:<name> confirm:True`, or `/feops campaign_keep server:<name>` | Written at the next start (edited copy in the backup), or the server's cfg kept and the pack dismissed for that server | |
| 28A.18 | Pack rollback | Pack with a cfg the engine refuses | `THE MISSION CANNOT START`; backup restored at the next start; tag never retried on that server | |
| 28A.19 | Bot plugin | Release built with `-BotPlugin`, `bot_plugin: true` | Plugin updated; previous one in `<DCSServerBot>\_fowl_backups\plugin-<ts>.zip`; never unpacked over a newer plugin | |

### 28B. OPS Page and /feops

| # | Item | Steps | Expected | Pass |
|---|------|-------|----------|------|
| 28B.1 | Access | Open the dashboard as a player, then as an admin | **OPS** in the nav for admins only | |
| 28B.2 | No API key | No `ops_api.api_key` and no `bfdb.dcsserverbot_api_key` | OPS page disabled | |
| 28B.3 | Cards | Open OPS | Summary, Server box & auto-start, Processes, Engine builds, Campaign packs, Automatic updates, History & backups, Issues, Bot config, Logs | |
| 28B.4 | Restarts | Processes > **Restart bfdb**, **Restart bot** | Each restarts; DCS keeps running | |
| 28B.5 | Engine builds | Engine builds card | Per binary: running build, file on disk, staged, latest release | |
| 28B.6 | Config editor | Bot config: change a value, save | Secrets show as `__SECRET__` and are kept; file validated and backed up to `config\backup\fowlengine\`; keys naming a program, path, URL or update source can't be changed | |
| 28B.7 | Logs | Logs card | Live tails for bot, service, bfdb, bfdb boot and netidx; the log archive | |
| 28B.8 | Issues | Cause a repeated WARN; later mark it fixed and cause it again | One issue with a count; posted in `ops_channel`; **Copy report for Claude** works; shows REGRESSED when it comes back | |
| 28B.9 | Log archive | Restart a DCS server | Earlier lines still in `<bfdb.home>\_logarchive\<source>\<UTC date>.log` | |
| 28B.10 | Update commands | `/feops update_status`, `update_check`, `update_pause` | Latest release, what's installed, probation; a check now; pause and resume | |
| 28B.11 | Rollback command | `/feops update_rollback which:dll server:<name> confirm:True` | DLL rolled back to its previous build | |
| 28B.12 | Stage commands | `/feops stage_status`, `stage_cancel`, `stage_apply` | Shows, discards and applies staged binaries | |
| 28B.13 | Other commands | `/feops bfdb_restart`, `gci_show`, `versions`, `issues` | bfdb restarts; `gci.json` with secrets masked; git rev and build time of the running binaries; the issues report attached | |

### 28C. Fowl Engine Manager

| # | Item | Steps | Expected | Pass |
|---|------|-------|----------|------|
| 28C.1 | Setup | Install, then Setup: DCSServerBot folder, Install plugin, Service, Automatic sign-in | `FowlEngine` service created; the older NSSM `DCSServerBot` service disabled | |
| 28C.2 | Reboot | `Restart-Computer`, don't sign in | DCS servers and the dashboard come back | |
| 28C.3 | Bot supervision | Kill the bot process | Restarted, backing off from 5 s to 5 min | |
| 28C.4 | Plugin sync | Restart the bot with **keep the plugin synced** on | Bundled plugin synced before the start; a bundle older than the installed plugin is skipped and Overview says so | |
| 28C.5 | Self-update | Publish a newer `manager-v<version>` | Signature verified, installed; the bot restarts, DCS does not | |
| 28C.6 | Server OPS | Manager > **Server OPS** | The dashboard's OPS page, talking to the local bot | |
| 28C.7 | Backup | BACKUP tab, make a backup | `FowlEngine-backup-<pc>-<date>.zip`; OK / CHECK / BROKEN per section; log saved as `<zip>.log` | |
| 28C.8 | Check a backup | **Check a backup** on a copy of the zip | Same report | |
| 28C.9 | Restore | Restore onto a spare box | Missing prerequisites listed; existing folders renamed `*.before-restore-<time>`; service installed and started; automatic sign-in and the GitHub token have to be entered again | |

---

## Sign-off Table

| Section | Tested By | Date | Pass/Fail | Notes |
|---------|-----------|------|-----------|-------|
| Automated tests | | | | |
| Pre-Test Setup | | | | |
| 1. Mission Init | | | | |
| 2. Player Connection | | | | |
| 3. Slot System | | | | |
| 4. Life System | | | | |
| 5. Events & Death | | | | |
| 6. Kill Attribution | | | | |
| 7. Objective System | | | | |
| 8. Group & Unit Mgmt | | | | |
| 9A. Action Menu | | | | |
| 9B. Cargo Menu | | | | |
| 9C. Troop Menu | | | | |
| 9D. Mission Menu | | | | |
| 9E. EWR Menu | | | | |
| 9F. JTAC Menu | | | | |
| 10. JTAC System | | | | |
| 11. EWR System | | | | |
| 12. Cargo & Logistics | | | | |
| 13. Chat Commands | | | | |
| 14. Admin Commands | | | | |
| 15. Campaign Events | | | | |
| 16. Smart Commander | | | | |
| 17. Frontline | | | | |
| 18. F10 Map Layer | | | | |
| 19. CSAR | | | | |
| 20. Background Tasks | | | | |
| 21. Points & Scoring | | | | |
| 22. Auto-Shutdown | | | | |
| 23. Spawn Context | | | | |
| 24. Message Queue | | | | |
| 25. Regression / Edge Cases | | | | |
| 26A. Range: Server & Engine | | | | |
| 26B. Range: Drills | | | | |
| 26C. Range: Results, Discord & Website | | | | |
| 27A. Cockpit: Install | | | | |
| 27B. Cockpit: Panel & Wheel | | | | |
| 27C. Cockpit: Updates | | | | |
| 27D. Cockpit: WIKI tab | | | | |
| 28A. Releases, Staging & Rollback | | | | |
| 28B. OPS Page & /feops | | | | |
| 28C. Fowl Engine Manager | | | | |
