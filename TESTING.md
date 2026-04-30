# Fowl Engine — Detailed Testing Checklist

**How to use this document**
- Each item has: **Steps** (how to trigger it) and **Expected** (what correct behavior looks like)
- Mark items `[x]` as you go, add notes in the Notes column of the sign-off table at the end
- All testing is done in-game on a live DCS server unless otherwise noted

---

## Pre-Test Setup

| # | Item | Steps | Expected | Pass |
|---|------|-------|----------|------|
| P1 | Build DLL | `cargo build --release --package=bflib` | Compiles with zero errors; `target/release/bflib.dll` updated | |
| P2 | Deploy DLL | Copy `bflib.dll` to DCS mission folder | File timestamp updated in mission folder | |
| P3 | Load mission | Start DCS server, load mission | Server reaches "Running" state, no Lua errors in `dcs.log` | |
| P4 | Admin account | Log in with known-admin UCID | Admin commands accepted in chat | |
| P5 | Regular account | Second client connects as regular player | Connects without admin privileges | |
| P6 | Logs active | Check log file path from config | Log file exists and is being written to | |
| P7 | bfdb connected | Start bfdb, check netidx connection | bfdb shows connected, no timeout errors | |

---

## 1. Mission Initialization

| # | Item | Steps | Expected | Pass |
|---|------|-------|----------|------|
| 1.1 | Clean start | Load fresh mission (no saved state) | No errors in log during `init_miz()` / `delayed_init_miz()` | |
| 1.2 | Objectives load | After load, open F10 map | All named objectives appear at correct map positions | |
| 1.3 | Objective sides | Check F10 map objective colors | Blue objectives show blue, red show red, neutral show neutral | |
| 1.4 | Units spawn | Watch server log after load | Ground units and objective defenders appear in DCS without errors | |
| 1.5 | Warehouse init | Run `/admin query_warehouse <objective>` | Shows non-zero fuel and ammo for supply-capable objectives | |
| 1.6 | Config validates | Check log on startup | No `[ERROR]` lines related to cfg parsing; no unknown fields warned | |
| 1.7 | F10 markings | Open F10 map as blue/red player | Objective labels, health/supply indicators visible on map | |
| 1.8 | Netidx publish | Start bfdb, watch its output | Stats data flows; no "publisher disconnected" errors in first 60s | |
| 1.9 | State load | Restart server with existing save file | Previous objective ownership and player data restored correctly | |

---

## 2. Player Connection & Registration

| # | Item | Steps | Expected | Pass |
|---|------|-------|----------|------|
| 2.1 | New player connect | Connect fresh client (no prior UCID in DB) | Player appears in server, `on_player_try_connect` fires | |
| 2.2 | Register command | Type `/register` in chat | Server replies with confirmation; player marked `registered=true` in DB | |
| 2.3 | Double register | Type `/register` again after already registered | Server replies with "already registered" message, no crash | |
| 2.4 | Side switch blue | Type `/blue` in chat | Player side set to Blue; server confirms in chat | |
| 2.5 | Side switch red | Type `/red` in chat | Player side set to Red; server confirms in chat | |
| 2.6 | Side switch while in slot | Occupy a slot, then type `/blue` | Server rejects or forces deslot before switching | |
| 2.7 | Whitelist enforced | Set `Rule::Whitelist` in config, connect non-listed UCID | Connection rejected or player restricted to spectator | |
| 2.8 | Blacklist enforced | Set `Rule::Blacklist` with test UCID, attempt connect | Connection rejected with appropriate message | |
| 2.9 | Ban on connect | Ban player via `/admin ban <name> 1h`, reconnect | Server refuses connection, logs ban reason | |
| 2.10 | Admin privileges | Connect admin UCID | `/admin` commands accepted; non-admin commands blocked for regular players | |
| 2.11 | Name change cooldown | Change pilot name in DCS, reconnect | If within cooldown window, server warns or restricts; after cooldown, accepted | |
| 2.12 | Reconnect same UCID | Disconnect and reconnect same player | Player data persists (points, side, registration) | |

---

## 3. Slot System

| # | Item | Steps | Expected | Pass |
|---|------|-------|----------|------|
| 3.1 | Take free slot | Select any empty slot | Player spawns in cockpit; `on_player_try_change_slot` fires | |
| 3.2 | Slot type in ephemeral | After spawning, run `/admin query_player_details <name>` | Shows correct vehicle type for occupied slot | |
| 3.3 | Occupied slot blocked | Player A takes slot; Player B tries same slot | Player B rejected with "slot occupied" message | |
| 3.4 | Admin takes occupied | Admin tries slot already held by another player | Admin bypasses check and occupies; original player deslotted | |
| 3.5 | Dynamic slot approved | If dynamic slots configured, request slot approval | Slot approved after server processes request | |
| 3.6 | Dynamic slot denied | Trigger deny condition (wrong side, no lives, etc.) | Player rejected; `process_slot_rejection()` message shown | |
| 3.7 | Wrong-side slot | Player set to Blue tries Red slot | Slot access denied; player stays in spectator | |
| 3.8 | Slot freed on disconnect | Player in slot disconnects | Slot freed immediately; another player can take it | |
| 3.9 | Multi-crew slot | Enter multi-crew aircraft as first crew | `units_by_slot` maps slot to multiple unit IDs | |
| 3.10 | player_in_slot map | Occupy slot, verify via admin query | `player_in_slot` correctly maps SlotId → Ucid | |

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
| 4.7 | /lives command | Type `/lives` in chat | Message shows remaining lives for each type for player's side | |
| 4.8 | Life message on takeoff | Take off in any aircraft | Popup or panel message shows which life was consumed | |
| 4.9 | Lives return after capture | Capture an objective with `log_points > 0` | `return_lives()` fires; life count increases for winning side | |
| 4.10 | Periodic life return | Wait for slow timer cycle | Lives tick up passively over time (if configured) | |
| 4.11 | Zero lives blocks slot | Exhaust all lives of one type | Attempting to take slot of that type gives "no lives remaining" error | |
| 4.12 | Admin reset lives | `/admin reset_lives <player>` | Player's lives restored to config default | |

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
| 9B.8 | Parachute drop | Select unpakistan / parachute drop | Crate descends under parachute; lands at offset position | |
| 9B.9 | List current cargo | Open "List cargo" option | All loaded crates and troops shown with counts | |
| 9B.10 | Destroy crate | Select "Destroy crate" near ground crate | Crate removed from map; no orphaned entry in DB | |
| 9B.11 | Spawn crate (admin) | `/admin spawn_crate <name> <pos>` | Crate appears at specified position | |
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
| 12.18 | Warehouse supply increase | After delivery, check warehouse | `/admin query_warehouse` shows increased fuel/ammo | |
| 12.19 | DCS warehouse sync | After engine delivery, check DCS F10 rearm menu | Fuel/ammo available for rearming at that airbase | |

---

## 13. Chat Commands (chatcmd.rs)

| # | Item | Steps | Expected | Pass |
|---|------|-------|----------|------|
| 13.1 | /register | Type `/register` | Confirmation message; player data initialized | |
| 13.2 | /blue | Type `/blue` | Side set to Blue; confirmation in chat | |
| 13.3 | /red | Type `/red` | Side set to Red; confirmation in chat | |
| 13.4 | /lives | Type `/lives` | Life counts for all types shown for player's side | |
| 13.5 | /time | Type `/time` | Current mission time displayed (e.g. "12:34:56") | |
| 13.6 | /balance | Type `/balance` | Player's current point balance shown | |
| 13.7 | /status | Type `/status` | Server status (player count, objectives held, etc.) | |
| 13.8 | /transfer valid | `/transfer <player> 100` | 100 points moved; both players notified | |
| 13.9 | /transfer insufficient | `/transfer <player> 999999` | Refused; "insufficient points" message | |
| 13.10 | /transfer self | `/transfer <own-name> 100` | Refused or no-op; no crash | |
| 13.11 | /help | Type `/help` | List of available commands shown | |
| 13.12 | /action | `/action <action-name>` | Action executes if valid for player's current slot | |
| 13.13 | /bind | `/bind F1 <action>` | Key binding saved; confirmation shown | |
| 13.14 | Unknown command | Type `/asdfghjkl` | "Unknown command" message; no crash | |
| 13.15 | Malformed args | `/transfer` (no args) | Usage hint shown; no crash | |
| 13.16 | Admin prefix | `/admin <cmd>` as admin | Routes to admin handler | |
| 13.17 | Admin prefix non-admin | `/admin <cmd>` as regular player | "Permission denied" message | |

---

## 14. Admin Commands (admin.rs)

### Player Management

| # | Item | Steps | Expected | Pass |
|---|------|-------|----------|------|
| 14.1 | spawn | `/admin spawn <key>` | Named deployable spawns at default position | |
| 14.2 | sideswitch | `/admin sideswitch red <player>` | Player moved to red side; confirmation shown | |
| 14.3 | ban timed | `/admin ban <player> 2h` | Player kicked; reconnect within 2h refused | |
| 14.4 | ban permanent | `/admin ban <player> 0` | Permanent ban; no expiry | |
| 14.5 | unban | `/admin unban <player>` | Ban lifted; player can reconnect | |
| 14.6 | list_banned | `/admin list_banned` | All currently banned players listed with expiry | |
| 14.7 | kick | `/admin kick <player>` | Player disconnected immediately | |
| 14.8 | list_connected | `/admin list_connected` | All online players listed with UCID | |
| 14.9 | search | `/admin search <partial-name>` | Matching players listed (fuzzy match) | |
| 14.10 | log_desc | `/admin log_desc <ucid>` | Sortie history, kills, deaths shown | |
| 14.11 | reset_lives | `/admin reset_lives <player>` | Player lives reset to config defaults | |
| 14.12 | add_admin | `/admin add_admin <player>` | Player gains admin flag; can use admin commands | |
| 14.13 | remove_admin | `/admin remove_admin <player>` | Admin flag removed | |
| 14.14 | balance | `/admin balance <player>` | Player's exact point total shown | |
| 14.15 | set_points | `/admin set_points <player> 500` | Player points set to exactly 500 | |
| 14.16 | deslot | `/admin deslot <player>` | Player ejected to spectator; slot freed | |

### Object Management

| # | Item | Steps | Expected | Pass |
|---|------|-------|----------|------|
| 14.17 | delete group | `/admin delete <group_id>` | Group despawned from DCS; removed from DB | |
| 14.18 | remark | `/admin remark <objective> <new-name>` | Objective rename reflected on F10 map | |

### Query Commands

| # | Item | Steps | Expected | Pass |
|---|------|-------|----------|------|
| 14.19 | query_objectives | `/admin query_objectives` | All objectives listed with side, health, supply | |
| 14.20 | query_objective_details | `/admin query_objective_details <name>` | Full detail: groups, zone, warehouse link | |
| 14.21 | query_players | `/admin query_players` | All registered players listed | |
| 14.22 | query_player_details | `/admin query_player_details <name>` | Points, side, slot, ban status, registration | |
| 14.23 | query_groups | `/admin query_groups blue` | All blue-side groups listed with IDs | |
| 14.24 | query_group_details | `/admin query_group_details <id>` | Units, origin, health, destination shown | |
| 14.25 | query_units | `/admin query_units <group_id>` | All units in group with type, health, ammo | |
| 14.26 | query_warehouse | `/admin query_warehouse <objective>` | Fuel and ammo inventory shown | |
| 14.27 | query_logistics | `/admin query_logistics` | All convoys, air routes, sea routes and their states | |
| 14.28 | query_campaign_state | `/admin query_campaign_state` | Objective counts per side, treasury, event queue | |

### API Commands

| # | Item | Steps | Expected | Pass |
|---|------|-------|----------|------|
| 14.29 | api_spawn_deployable | `/admin api_spawn_deployable <obj> <deployable>` | Deployable group spawns at objective | |
| 14.30 | api_spawn_troop | `/admin api_spawn_troop <obj> <troop>` | Troop group spawns at objective | |
| 14.31 | api_move_group | `/admin api_move_group <id> <x,y>` | Group issued move waypoint | |
| 14.32 | api_add_points | `/admin api_add_points <player> 100 "test"` | 100 points added; log records reason | |

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
| 21.7 | Transfer | `/transfer <player> 50` | Sender -50, receiver +50 | |
| 21.8 | Deploy cost | Deploy item with `cost = 100` | 100 points deducted from deploying player | |
| 21.9 | Repair cost | Repair damaged deployable | `repair_cost` deducted | |
| 21.10 | Score tracking | Accumulate kills and deaths | `/admin log_desc` shows accurate K/D/cargo stats | |

---

## 22. Auto-Shutdown

| # | Item | Steps | Expected | Pass |
|---|------|-------|----------|------|
| 22.1 | Monitor player count | All players leave server | `check_auto_shutdown()` starts countdown timer | |
| 22.2 | Shutdown after timeout | Remain empty for full timeout duration | Server process exits cleanly | |
| 22.3 | Cancel on join | Player joins before timeout expires | Shutdown cancelled; server continues running | |
| 22.4 | Force spectators | `/admin force_spectators` (if exposed) | All players moved to spectator; slots freed | |

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

## Sign-off Table

| Section | Tested By | Date | Pass/Fail | Notes |
|---------|-----------|------|-----------|-------|
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
