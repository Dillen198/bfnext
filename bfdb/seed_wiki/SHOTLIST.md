# Wiki shot list

In-game (and dashboard) screenshots to capture for the player wiki. Each one
sits next to a drawn mockup (`f10menu`, `dcsmessage`, `cockpitpanel`, `wheel`,
`steps`) or an SVG diagram and shows the real thing the mockup stands in for.
This file is a work list for whoever takes the pictures. It is **not** a wiki
page and is not registered in `bfdb/src/db.rs`.

## How to capture and add one

1. **Capture.** DCS: `Print Screen` saves to `Saved Games\DCS\Screenshots`.
   Use a 16:9 window, UI scale 100%, and the default message box size. Keep
   your callsign or blank it; nobody else's name should be readable. Dashboard
   shots: a desktop browser 1600 px wide, dark theme.
2. **Prepare.** Crop to what the shot is about (a menu, a message box, a map
   area), not the whole screen, unless the line says "full". Save as **JPEG**
   (quality about 85) or **PNG**, at most 1600 px wide, under the target
   filename. **Do not use WebP**: the in-game WIKI tab cannot show it.
3. **Add.** Put the file in `bfdb/seed_wiki/images/`. Give it a fresh UUID
   (`python -c "import uuid;print(uuid.uuid4())"`), add
   `("<uuid>", include_bytes!("../seed_wiki/images/<file>"))` to
   `seed_wiki_images_if_empty` in `bfdb/src/db.rs`, and embed it in the page
   with `![<one-sentence description>](/api/wiki/images/<uuid>)` right after the
   mockup it complements. Seed images are never replaced once stored, so a new
   picture always needs a **new** UUID, including the replacements below.

Columns: **file** (target filename) · **page** (section on that page) ·
**frame** (what to show) · **complements** (the mockup or diagram next to it).

## Drawings to replace first

These pages embed drawn SVG illustrations standing in for screenshots
(`objective-buildings-*.svg`; see the comment in `seed_wiki_images_if_empty`).
Capture the real thing and swap the embed to the new UUID. Keep the drawing
if it explains something a screenshot can't (the Command Center's SAM network,
the F10 pin breakdown) and add the screenshot beside it.

| file | page | frame | complements |
| --- | --- | --- | --- |
| objective-buildings-f10-pins-real.jpeg | gameplay/objective-buildings.md (Finding the buildings: the `logi:` pins) | F10 map zoomed on an airbase, its `logi:` pins and the four status hexes | pins text and statushexes |
| objective-buildings-warehouse-real.jpeg | gameplay/objective-buildings.md (Warehouses) | external view of a warehouse at an airbase, intact | warehouse text |
| objective-buildings-industrial-real.jpeg | gameplay/objective-buildings.md (Industrial buildings) | an industrial building at a factory objective | industrial text |
| objective-buildings-fuel-real.jpeg | gameplay/objective-buildings.md (Fuel, depot and storage buildings) | fuel and storage tanks at an objective | fuel text |
| objective-buildings-destroyed-real.jpeg | gameplay/objective-buildings.md (What destroying one does) | the same warehouse destroyed (smoking wreck) | destroyed-state text |
| objective-buildings-command-center-real.jpeg | gameplay/objective-buildings.md (Command Centers) | a Command Center: its star on the F10 map and the site itself (two crops side by side) | command-centre text |
| objective-buildings-farp-real.jpeg | gameplay/objective-buildings.md (FARPs) | a crate-built FARP: tent, ammo dump and fuel depot | FARP text |
| objective-buildings-placed-logi-real.jpeg | gameplay/objective-buildings.md (Servers without map-building logistics) | placed logistics statics: command centre, ammunition depot, trucks | placed-logi text |

## Getting started and playbooks

| file | page | frame | complements |
| --- | --- | --- | --- |
| slot-welcome-chat.jpeg | getting-started/joining-team.md (Registering) | chat right after your first slot, showing the "Welcome to the Blue team..." line | steps block and welcome dcsmessage |
| slot-denied-chat.jpeg | getting-started/joining-team.md (Taking a slot) | slot screen with a refusal line in chat, e.g. "Objective does not have any ... in stock" | slot-denial table |
| takeoff-hold.jpeg | getting-started/joining-team.md (After you take a slot) | cockpit with the "Takeoff hold: NNs remaining" panel | takeoff-hold dcsmessage |
| slot-briefing-panel.jpeg | getting-started/welcome.md (First Steps) | the situation panel after spawn, ideally with the ATIS box just before it | spawn-sequence text and slot-entry-sequence.svg |
| chat-help-reply.jpeg | getting-started/hud-and-menus.md (Chat commands) | chat window after `-help`, showing the reply lines | mistyped-command dcsmessage |
| cockpit-panel-wiki-sections.jpeg | getting-started/cockpit-overlay.md (WIKI) | cockpit panel on the WIKI tab, section list, search box empty | WIKI section text |
| cockpit-panel-wiki-page.jpeg | getting-started/cockpit-overlay.md (WIKI) | WIKI tab with a page open: breadcrumb, a picture loaded, a link button and NEXT | WIKI cockpitpanel block |
| cockpit-panel-wiki-search.jpeg | getting-started/cockpit-overlay.md (WIKI) | WIKI tab with "laser" typed in the search box and the results list | search bullet |
| cockpit-panel-menu-cargo-real.jpeg | getting-started/cockpit-overlay.md (MENU) | MENU tab inside Cargo in a Huey, with a server answer in the top box | MENU cockpitpanel block (the current picture is a generated preview) |
| cockpit-wheel-combat-real.jpeg | getting-started/cockpit-overlay.md (What's on it to start with) | the wheel in a jet, GCI/EWR pointed at with its arc fanned out | combat-preset wheel block |
| f10-root-cargo-helo.jpeg | f10-menu/overview.md (The root menu) | Mi-8 or Huey with the F10 root open: GCI/EWR, Flight, Cargo, Troops, JTAC>>, Actions>>, Objectives, Info | root f10menu replica |
| f10-actions-expanded.jpeg | f10-menu/overview.md (buttons, not folders) | the Actions submenu after selecting Actions>>, an action open with `Mark 1 310/38km` entries | second f10menu replica |
| f10-map-mark-placed.jpeg | f10-menu/overview.md (Map marks) | F10 map with a named mark `CAS1` being placed near the front | mark-label table |
| landing-debrief.jpeg | playbooks/first-sortie.md (After you land) and gameplay/progression.md (After you land) | the `LANDED at ... -- landing registered` debrief panel | debrief dcsmessage |
| jtac-nine-line.jpeg | playbooks/cas-with-jtac.md (step 4) | the 9-line message box in the cockpit | steps block |
| troops-unload-hint.jpeg | playbooks/capturing-a-base.md (step 3) | helo landed inside the ring with the "troops are in X's ring -- keep them alive" panel | steps 5-6 |

## F10 menus

| file | page | frame | complements |
| --- | --- | --- | --- |
| actions-menu-tree.jpeg | f10-menu/actions.md (The menu tree) | F10 open on `Actions`: first page of entries with `(N pts)` labels and `More >>` | f10menu replica |
| actions-mark-list.jpeg | f10-menu/actions.md (Opening the menu) | an action's submenu listing marks with `045/12km` labels | f10menu replica |
| actions-move-pricing.jpeg | f10-menu/actions.md (Group lists) | Move → a group → marks with `3km (30 pts)` prices | Move branch of the replica |
| actions-orbit-marks.jpeg | f10-menu/actions.md (Orbit marks) | F10 map with an AWACS's two race-point marks and the responsible-party label | race-point dcsmessage |
| actions-points-charge.jpeg | f10-menu/actions.md (Costs, limits and refunds) | message box with `action AWACS started` and the points line | charge dcsmessage |
| jtac-location-list.jpeg | f10-menu/jtac.md (Opening the JTAC menu) | JTAC submenu with Refresh Locations and two "Place (n JTAC)>>" entries | f10menu replica |
| jtac-status-panel.jpeg | f10-menu/jtac.md (Status report) | a full JTAC status incl. "In laser range" and "Seen, out of laser range" | status dcsmessage |
| jtac-map-layer.jpeg | f10-menu/jtac.md (What JTACs draw on the F10 map) | F10 map on a drone JTAC: laser line, target pin, laser-code label | map-layer bullets and jtac-see-vs-lase.svg |
| ewr-bandits-report.jpeg | f10-menu/ewr.md (Report) | Bandits BRAA table with 3-4 contacts, one stale `*` | Report dcsmessage |
| ewr-ground-intel.jpeg | f10-menu/ewr.md (Ground Intel) | Ground Intel message box with 2-3 lines | Ground Intel dcsmessage |
| cockpit-panel-ewr-tab.jpeg | f10-menu/ewr.md (In the cockpit panel) | cockpit EWR tab in THREATS mode | cockpitpanel block |
| recon-progress.jpeg | f10-menu/recon.md (Running a pass) | "Recon 50% -- N contact(s)" while orbiting an enemy objective | progress dcsmessage and recon-pass-geometry.svg |
| recon-intel-markers.jpeg | f10-menu/recon.md (Reading the results) | F10 map after a pass: scan square, RECON pin, several [INTEL/RED] markers | marker dcsmessage |
| cargo-classic-menu.jpeg | f10-menu/cargo.md (The classic Cargo menu) | Huey at a FARP, F10 > Cargo > Crates > Radar SAMs > Hawk System(35 pts) | classic f10menu |
| cargo-dynamic-spawn.jpeg | f10-menu/cargo.md (The dynamic Cargo menu) | CH-47 with a freshly spawned crate and the "Spawned ... F8 -> Ground Crew -> Cargo" message | dynamic f10menu |
| cargo-unpack-too-close.jpeg | f10-menu/cargo.md (Unpacking: the rules) | message "can't unpack here — too close to friendly objective ..." | unpack rules table |
| cockpit-panel-cargo-onboard.jpeg | f10-menu/cargo.md (CARGO tab) | CARGO tab with ON BOARD, NEARBY CRATES and BASE SUPPLY | cockpitpanel block |
| troops-menu.jpeg | f10-menu/troops.md (The Troops menu) | F10 > Troops > Squads in a Huey | troops f10menu |
| troops-capture-hint.jpeg | f10-menu/troops.md (Unloading troops) | the "troops are NNNm outside X's ring" hint after an unload | hint dcsmessage |
| csar-alert.jpeg | f10-menu/csar.md (What happens when a pilot ejects) | "CSAR: <name> is down — bearing ..." in a helo | alert dcsmessage |
| csar-flare-pickup.jpeg | f10-menu/csar.md (Getting them out) | landed Mi-8, green flare, the pilot walking in | pickup prose |
| f10-objectives-menu.jpeg | f10-menu/objectives.md (The menu) | F10 open at Objectives showing all entries | f10menu replica |
| capture-advisor-card.jpeg | f10-menu/objectives.md (The Capture Advisor) | Capture Advisor on an enemy base with blockers and a "CANNOT capture" squad line | advisor dcsmessage |
| base-detail-card.jpeg | f10-menu/objectives.md (Nearest Base) | Base Detail on an own airbase incl. Repair and Navaids lines | detail dcsmessage |
| f10-info-menu.jpeg | f10-menu/info.md (The menu) | F10 Info submenu with all entries (HQ if present) | f10menu replica |
| my-status.jpeg | gameplay/points-and-lives.md (Checking your balance) | F10 → Info → My Status on a lives-on server | My Status dcsmessage |

## Core gameplay

| file | page | frame | complements |
| --- | --- | --- | --- |
| capturable-white-ring.jpeg | gameplay/reading-the-map.md (The rings) | an enemy base with the white inner ring and ">> UNDEFENDED - BLUE captures" label | mapsymbols block and objective-layout.svg |
| consolidating-label.jpeg | gameplay/capturing-objectives.md (Step 5) | label ">> CONSOLIDATING n% (...) - hold the zone" over a just-taken base | step 5 dcsmessages and capture-sequence.svg |
| capture-messages.jpeg | gameplay/capturing-objectives.md (Steps 3-4) | "Capturing X (90 sec)" and "BASE CAPTURE: X has been taken by Blue!" | capture dcsmessages |
| last-defenders-pins.jpeg | gameplay/capturing-objectives.md (Step 1) | F10 map with "Last defenders of X" pins around a battered base | last-defenders dcsmessage |
| dashboard-capture-report.jpeg | gameplay/capturing-objectives.md (The dashboard capture report) | dashboard drawer "Why can't this be captured?" with blockers, Still standing list, mini-map | blocker table |
| logistics-not-in-stock.jpeg | gameplay/logistics.md (Warehouses) | chat line "Objective does not have any X in stock" on a refused slot | dcsmessage |
| emergency-repair-refused.jpeg | gameplay/logistics.md (Emergency repair crates) | refusal "... is under attack -- repairs can't start ..." | refusal table and logistics-chain.svg |
| flight-cost-panel.jpeg | gameplay/points-and-lives.md (Your aircraft and weapons) | FLIGHT COST panel while taxiing on a priced server | FLIGHT COST dcsmessage |
| kill-points-message.jpeg | gameplay/points-and-lives.md (Earning points) | points message after a kill, ideally with an [xN underdog] tag | kill dcsmessage and points-flow.svg |
| chat-help.jpeg | gameplay/chat-commands.md (Getting help) | chat after `-help` | -help dcsmessage |
| jtac-list-chat.jpeg | gameplay/chat-commands.md (JTAC from chat) | chat after `-jtac list` | jtac list dcsmessage |
| your-task-mark.jpeg | gameplay/your-task.md | F10 map with the YOUR TASK mark and zone, plus the briefing panel | task dcsmessage |
| war-effort-briefing.jpeg | gameplay/war-effort.md | slot briefing panel bottom with the WAR EFFORT line | briefing dcsmessage |
| rear-base-warning.jpeg | gameplay/round-rules.md (Protected rear bases) | PROTECTED AIRSPACE warning panel | rear-base dcsmessage |
| tasking-board-pin.jpeg | gameplay/tasking-board.md (Where tasks show up) | F10 map with a task circle and its pin text open | pin dcsmessage |
| situation-slot-panel.jpeg | gameplay/briefing.md (The slot-entry panel) | SITUATION panel on spawn | panel dcsmessage |
| briefing-dashboard.jpeg | gameplay/briefing.md (intro) | dashboard BRIEFING > Situation with numbered pins | intro table |
| situation-comms-page.jpeg | gameplay/comms-plan.md (The live card in game) | Info > Situation > 6. Comms with [UP] lines | comms dcsmessage |
| navaids-directory.jpeg | gameplay/navaids.md (Finding the channel) | Info > Navaids Directory with FARP and carrier lines | Info f10menu excerpt |
| farp-tacan-hsi.jpeg | gameplay/navaids.md (Using them) | HSI/DME locked on a FARP TACAN, ident visible | channel table |
| carrier-icls-final.jpeg | gameplay/navaids.md (Carrier groups) | ICLS needles on final to a CVN | carrier navaid table |
| atis-airfield.jpeg | gameplay/weather.md (The airfield ATIS) | cockpit just after slotting at an airbase, ATIS box top right | airfield ATIS dcsmessage and slot-entry-sequence.svg |
| weather-full-report.jpeg | gameplay/weather.md (The full report) | Info > Weather with the winds-aloft table | winds-aloft dcsmessage |
| atis-carrier.jpeg | gameplay/carrier-ops.md (Finding it) | deck slot with the CARRIER ATIS box | carrier ATIS dcsmessage |
| carrier-repair-crate.jpeg | gameplay/carrier-ops.md (Carrier repair crates) | crate on deck plus the "Carrier repair crate delivered" message | crate dcsmessage |

## Advanced topics

| file | page | frame | complements |
| --- | --- | --- | --- |
| c130-cargo-menu.jpeg | advanced/c130-airdrop.md (The C-130 Cargo menu) | F10 > C-130 Cargo > Crates > Base Supply with xN entries | c130 f10menu |
| c130-crates-behind.jpeg | advanced/c130-airdrop.md (Step by step) | C-130 on the ramp with spawned crates in rows behind the tail | c130-airdrop-sequence.svg step 1 |
| c130-missing-mark.jpeg | advanced/c130-airdrop.md (Crates missing) | F10 mark "X / Missing: Y (need 1, have 0)" by a landed crate | dcsmessage and SVG |
| c130-carp-tab.jpeg | advanced/c130-airdrop.md (CARP) | web /cockpit CARP tab with a PI picked and values filled | CARP list |
| fatcow-layout.jpeg | advanced/deployables-guide.md (Fat Cow) | top-down or external view of a Fat Cow layout around a CH-47 | Fat Cow prose |
| request-fires-overlay.jpeg | advanced/artillery.md (Request Fires) | F10 map after Request Fires: gun line, impact circle, burst symbol | Request Fires f10menu |
| jtac-artillery-menu.jpeg | advanced/artillery.md (JTAC fire missions) | JTAC → Artillery with Fire All Groups Together and objective buckets | JTAC artillery f10menu |
| jtac-alcm-menu.jpeg | advanced/alcm.md (Step 2) | JTAC → ALCM → `id(missiles)` → Fire Quarter/Half/All | ALCM f10menu |
| strike-target-list.jpeg | advanced/strike-missions.md (Calling one) | a Precision Strike entry listing enemy objectives | strike f10menu |
| helo-missions-menu.jpeg | advanced/helo-missions.md (top) | Actions → AI Helo Missions with `nm [CAP]` base labels | f10menu replica |
| recon-intel-markup-ingame.jpeg | advanced/recon-intel-map.md (Drawing on the map) | F10 map with a dashboard-drawn circle/line and its "recon · name" label | markup section |
| groundwar-f10-menu.jpeg | advanced/ground-war.md (F10 → Ground Forces) | Ground Forces menu: Status report, Refresh, two formation entries | f10menu replica |
| groundwar-status-report.jpeg | advanced/ground-war.md (F10 → Ground Forces) | the GROUND FORCES status report message box | Status report says |
| groundwar-f10-pin-arrow.jpeg | advanced/ground-war.md (Reading the battlefield) | a formation pin with its attack arrow toward an enemy base | mapsymbols and ground-war-front-line.svg |
| groundwar-battle-ring.jpeg | advanced/ground-war.md (Reading the battlefield) | the "GROUND BATTLE near ..." dashed ring | mapsymbols |
| groundwar-smoke-from-air.jpeg | advanced/ground-war.md (Reading the battlefield) | cockpit view of battle smoke and burning wrecks | world-smoke text |
| frontline-f10.jpeg | advanced/ground-war.md (The front line) | F10 map with the azure and violet dashed edges and white dotted centre | ground-war-front-line.svg |
| hq-info-menu.jpeg | advanced/theatre-hq.md (Where to see it) | F10 > Info > HQ with Request Support expanded | HQ f10menu |
| hq-intent-message.jpeg | advanced/theatre-hq.md (Where to see it) | "HQ -- COMMANDER'S INTENT (...)" message box | intent says |
| hq-dashboard-take-command.jpeg | advanced/theatre-hq.md (Strategy from above) | dashboard THEATRE HQ page with the TAKE COMMAND panel | -hq table |
| command-map-buy-formation.jpeg | advanced/command.md (Buying a formation) | COMMAND page, BUY A FORMATION panel open, route drawn | buy steps |
| wingman-menu.jpeg | advanced/air-life.md (The Wingman menu) | F10 Wingman submenu: Request / Release / Status | f10menu replica |
| wingman-escort.jpeg | advanced/air-life.md (Calling a wingman) | the AI wingman off your right wing with the "starting up at ..." message | Request says |
| civil-airliner.jpeg | advanced/air-life.md (Civil traffic) | an airliner at cruise, ideally with its CIV callsign label | shoot-down dcsmessage |
| enemy-cap-circle.jpeg | advanced/ai-opposition.md (What you are told) | F10 "Enemy CAP [Red] — ACTIVE" circle plus the THREAT message | THREAT dcsmessage |
| elint-jammer-mark.jpeg | advanced/modern-war.md (Electronic warfare) | F10 mark "ELINT: GPS and comms jamming in this area" | EW text |
| jammer-truck.jpeg | advanced/modern-war.md (Killing a jammer) | the jammer truck at a command centre or naval base | destroyed-jammer dcsmessage |
| air-raid-warning.jpeg | advanced/modern-war.md (Raids on infrastructure) | the AIR RAID WARNING box, plus low drones if possible | raid-timeline.svg |
| supply-train.jpeg | advanced/modern-war.md (Supply by rail and road) | a supply train with the "Supply train departing ..." message | rail dcsmessage |
| fire-support-circle.jpeg | advanced/campaign-events.md (Barrage) | F10 dashed "Fire Support [Red] — N units firing" circle with the INTEL message | barrage dcsmessage |
| ambush-circle.jpeg | advanced/campaign-events.md (Convoy ambush) | F10 "AMBUSH [Red]" circle on a road with a convoy approaching | ambush text |

## Dashboard and replay (browser screenshots)

| file | page | frame | complements |
| --- | --- | --- | --- |
| dashboard-sitrep.jpeg | playbooks/reading-live-ops.md (SITREP) | full SITREP: stat strip, FRONT LINE with a few tags, TERRITORY CONTROL trend | steps block, step 1 |
| dashboard-base-drawer.jpeg | playbooks/reading-live-ops.md (The base drawer) | an enemy base drawer with "Why can't this be captured?", blockers and the Still standing mini-map | steps, step 2 |
| dashboard-objectives-table.jpeg | playbooks/reading-live-ops.md (OBJECTIVES) | table sorted by Health ascending, Needs Attention and Holdings by Type above | columns table |
| dashboard-tacmap-layers.jpeg | playbooks/reading-live-ops.md (TACMAP) | TACMAP with THR and COV on and a base drawer open | layers table |
| dashboard-briefing-kneeboard.jpeg | playbooks/reading-live-ops.md (BRIEFING) | Kneeboard tab with Navaids and RWR Threats / HARM Codes, KNEEBOARD PDF visible | BRIEFING section |
| dashboard-planner.jpeg | playbooks/mission-planner.md (Planning a route) | Planner with a 5-waypoint route round a SAM ring, Waypoints table, Data cartridge panel | steps block |
| dashboard-flight-log.jpeg | playbooks/flight-replay.md (Finding a sortie) | MY PROFILE Flight Log, one row expanded with FULL DEBRIEF and WATCH REPLAY | steps, step 1 |
| replay-3d-versus.jpeg | playbooks/flight-replay.md (Debrief tools) | 3D view following a jet, Versus panel on a bandit, ribbons on | steps, step 4 |
| replay-shots-tab.jpeg | playbooks/flight-replay.md (The Shots tab) | Shots tab with KILL/HIT/MISS rows, map view | steps, step 3 |
| replay-charts-layers.jpeg | playbooks/flight-replay.md (Layers / Charts) | Map view (Topo), Layers popover open, Charts with ALT + G | Layers table |
| sortie-debrief.jpeg | playbooks/flight-replay.md (The sortie debrief) | SORTIE DEBRIEF: flight profile, ground track, shot log | sortie debrief section |
