# Changelog

What changed for players, newest first. Current version: **v3.1**. Each update
gets the next number. Below v2.0 is the list of what this engine does
differently from stock Fowl Engine. Where an entry gives a campaign number, it
is read from the server selected at the top of the page; where a later entry or
a linked page says otherwise, the later one is right.

## v3.1 — October 22, 2026

**New wiki page: [Buildings at an Objective](../gameplay/objective-buildings.md)**
- What to look for at a base: the warehouses, industrial buildings and fuel
  and storage depots behind its Logi rating, the `logi:` pins that name them,
  Command Centers and FARP pieces.
- What destroying each one does, and how a base gets it back.

## v3.0 — October 21, 2026

**A new look for the dashboard**
- Military lettering throughout: stencilled headings, a condensed body face
  and terminal-style figures.
- The wireframe art is drawn from real 3D models now, the same ones the
  flight replay uses: a carrier behind the sign-in page and along the top
  bar, an F/A-18 over the stat row, and a Flanker, Hornet, Apache, Abrams or
  carrier on each page's header. Lines facing you are drawn strong, the ones
  behind faint, and the art is blended into the panels.

## v2.9 — October 21, 2026

**Fixes**
- A squadron tag can start with the numero sign: "№15" becomes the tag NO15
  instead of an error. The squadron form now says what a tag can be (2 to 5
  letters or digits) before you press Create.

## v2.8 — October 21, 2026

**Dashboard look** — wireframe art in place of the flags: a carrier along the
top bar, an Su-27 behind the sign-in page, an F/A-18 over the stat row, and a
jet, helicopter, tank or carrier on each page's header. Territory Control
keeps the server's flags.

**Fixes**
- Squadrons work again: founding, joining, leaving and removing a member all
  failed with an error (the browser called it a CORS block).
- Transmitting to GCI from the dashboard works again; it failed the same way.
- Rounds show their number even while a server is still on an older version.
- The map no longer floods the browser console with canvas warnings.

## v2.7 — October 21, 2026

**Round over, announced everywhere**
- When a side wins a round, the game says so: a panel and a chat line naming
  the winner, then a countdown to the reset (at least 30 seconds).
- Discord posts a full rundown of the round: who won and how long it ran, the
  final front line, kills and captures by side, the MVP and top five, the
  best ace, striker, capturer and logistician, and the most fought-over
  bases.
- The dashboard and the website show the result too, with a link to the
  round's full report on the Rounds page, which now also lists the round's
  leaders.

**Notifications** — round results, server notices and maintenance now go to a
bell in the top bar of the dashboard and the website, instead of banners
across every page. Only an outage still shows a banner.

**Syria: NATO vs RSII** — the modern server is now NATO against RSII, the
Russia, Syria, Iran and Iraq coalition, with their flags. Each server's
dashboard uses its own side names, so the 2008 server still reads Georgia vs
Russia.

**Fixes**
- Rounds are numbered 1, 2, 3 on each server. They used to show internal ids
  like 108000656.
- Opening a base on the dashboard while signed out, or without a side this
  round, now says why its stock is hidden instead of failing every minute.
- The flags behind the dashboard's stat row fill its full height.

## v2.6 — October 21, 2026

**Cockpit overlay** — see [Cockpit Overlay](../getting-started/cockpit-overlay.md)
- **3.1**: the in-game panel is rebuilt on DCS's own menus. Tabs for MENU,
  CARGO, STATUS, EWR, LOG and WHEEL, plus a radial wheel you drive with your
  head, mouse or keys (works in VR and on HOTAS). The installer is on GitHub.
- **3.2**: it updates itself. When a newer version is out it downloads it and
  installs it once DCS closes (Options → Special → "Update automatically", on
  by default).
- **3.3**: smaller panel sizes (down to 60%) and a separate Text size setting
  for small screens.

**Fixes**
- Picking a slot of the other side used to say "Red does not own the objective
  associated with this slot", and on dynamic slots DCS then showed "All
  spawn/parking slots are occupied", which looked like a full airfield. Chat
  now tells you which side you're on and why you can't change: sides are
  locked, or you have no side switches left.
- The 2008 status map in Discord and on the dashboard no longer shows leftover
  Syria objectives or a Syria front line, and the side totals count only the
  real campaign.
- A JTAC whose target vanished without being destroyed now moves on to the next
  contact instead of staying stuck on it.
- Units the engine picked up after a missed spawn event now show their positions
  in the stats.
- A long run of spaces in the middle of some messages is gone, including
  "Objective is still consolidating".

## v2.5 — October 20, 2026

**Dashboard** — see [Reading the Live Ops Dashboard](../playbooks/reading-live-ops.md)
- **Sortie debriefs**: each flight in your Flight Log opens a panel with that
  sortie's kills, shots, deploys and how it ended, and a full-page debrief
  with the flight profile, ground track and shot log.
- Your pilot page has career charts, your rivals, and a side-by-side compare
  with another pilot.
- **KILL FEED**: one filter row for the whole page, activity over time, what
  each side destroyed, and the top killers and weapons.
- **OBJECTIVES**: a clickable map, a list of the bases that need attention,
  holdings by type, and when each base was last taken.
- **RANKINGS** can show all time or a single round, for either side.
- The sitrep shows the front line, bases at risk, the last hour's kills and
  how territory has moved.
- The top bar shows the selected server's flags.

**Fixes**
- A logistics group whose first unit was a radar dome or a building crashed
  the server when it respawned. It no longer does.
- When an AI jet flight gets stuck on a field's ramp and is removed, jets stop
  starting on the ground there for 30 minutes: they launch from the next field
  out, and cargo flights start in the air over it.
- A cargo flight removed for being stuck returns its load to its hub, instead
  of the load being lost as if it was shot down.

## v2.4 — October 17, 2026

**Your task, and rookie mode** — see [Your Task & Rookie Mode](../gameplay/your-task.md)
- Every slot gets **one task**: the most urgent job on your side's list that
  your aircraft can do, with a zone on your own F10 map. Kills you make inside
  it pay **1.5×** points; captures, repairs, supply runs and FARPs there pay
  bonus XP.
- For your first 5 sorties on the server, the task is picked from the safer
  work (logistics, recon, CSAR, defending a base).

**The war-effort bar** — see [The War Effort Bar](../gameplay/war-effort.md)
- One tug-of-war bar per round, moved by captures. A capture after two hours
  of frozen front moves it twice as far.
- The side it favours gets a shorter takeoff hold, then cheaper actions.
- **Last defenders**: when a battered base is down to its last 8 infantry, the
  attackers get rough F10 marks on the survivors.

**XP, levels and squadrons** — see [XP, Levels & Squadrons](../gameplay/progression.md)
- **XP** that never goes down and carries across rounds, with levels, daily and
  weekly challenges, and achievements. It's on your profile on the PILOTS page.
- **SQUADRONS** on the dashboard: make or join one. A squadron's XP only
  counts what its members earned while in it.
- `/fe_card` in Discord posts your pilot card.
- The landing debrief lists what you contributed on the sortie.
- **ROUNDS** on the dashboard: every round played, who won, how long it ran,
  and its MVP and top pilots.

**New ways to fight**
- **AI Strike** actions (where configured): an AI flight attacks with its own
  weapons, no JTAC needed: precision, runway, anti-ship, interdiction or
  carpet. See [AI Strike Missions](../advanced/strike-missions.md).
- **Fat Cow** (where configured): a heavy-lift FARP you fly in by CH-47 or
  Mi-8, with fuel and weapons for helicopters, that you can pack up and move.
  The **Fat Cow (AI)** action has an AI helicopter set one up at your mark.
  See [Deployables Guide](../advanced/deployables-guide.md#fat-cow-heavy-lift-farp).

**Command** — see [Command](../advanced/command.md)
- **Buy a formation**: commanders can buy a new ground formation from the
  treasury and send it out along a route they draw.
- Units nobody owns and dismount squads show on the map, and commanders can
  **disband** them.
- What a commander puts on the map from the panel now belongs to them: they
  get the credit, and can `-delete` it (nothing is refunded).
- An AI flight given a Station order while still on the ramp now takes off and
  goes there, instead of freezing on its stand.
- **The human GCI seat**: commanders can type a call on the dashboard's radar
  scope and the GCI voice speaks it on the GCI frequency. See
  [Live GCI](../gameplay/gci.md#the-human-gci-seat).

**Server rules** (each only where the server turns it on) — see
[Rear Bases, Era Weapons & Round Length](../gameplay/round-rules.md)
- **Protected rear bases**: enemy aircraft get
  {{cfg:rear_base_protection.grace_secs|60}} s in total near one, then are
  removed.
- **Era weapons**: weapons the round's era bans must not be carried or fired.
- **Round length**: a round that runs past the server's limit ends in a draw.

**Dashboard**
- **TACMAP** is rebuilt: the command map's look, read-only, for everyone on
  your side, with zones of control, supply lines, coverage and threats. Click
  a base for its status, garrison, history and (for your own bases) its full
  warehouse; click an enemy contact for what's known about it. It now needs
  sign-in and a side. The old radar scope is under **SCOPE**. See
  [Reading the Live Ops Dashboard](../playbooks/reading-live-ops.md).
- **WAR DIARY** is a broadsheet front page for each day, with a picture for
  every day (an early edition while the day is running), and it's in the
  sidebar for everyone, signed in or not. See
  [The War Diary](../gameplay/war-diary.md).

**Bases, supply and capture**
- A base you capture starts with
  {{cfg:warehouse.capture_airframe_stock_percent|0}}% of its airframe capacity
  in your own types. See
  [Materiel & the War Economy](../gameplay/war-economy.md#captured-airframes).
- A base you've just taken on the front line is no longer cut off from road
  supply just because an enemy base is near it; only a road that runs through
  the enemy base is cut. Each base gets one convoy or airlift at a time.
- Air transports start in the air near their hub, so they no longer collide on
  the ramp, and a flight with no room to park returns its load.
- AI flights starting on the ground get the airframes and fuel they need at
  their field, fixing vanished drones and AWACS and tankers stuck on the ramp.
- Reinforcement convoys that park close to their base count as arrived.
- Troops and dismount squads that walk into a zone now count for the capture.
- Naval strikes: ships are set weapons free before launch, so they fire.

**Smaller changes**
- **CSAR** moved inside the **Cargo** menu in slots that have one, so **Info**
  no longer falls off the end of the F10 menu.
- Jammer trucks aren't spawned until enemy aircraft come near.
- F10 convoy, fire mission and task markers are see-through, so the base under
  them stays visible.
- The AI ground commander stops reissuing an order a formation got stuck on.

## v2.3 — October 9, 2026

**Flight replay** — see [Debriefing With Flight Replay](../playbooks/flight-replay.md)
- Every sortie can be replayed in the browser, in 2D or 3D, from the server's
  Tacview recordings. No Tacview needed. **REPLAY** in the sidebar; a
  **WATCH** link on each flight in your Flight Log.
- Debrief tools: telemetry, range/aspect/closure measuring, and a Shots tab
  with every missile's result, launch range and miss distance.
- Real 3D models for most aircraft, armour, SAMs, ships and common weapons.

**Stability and fixes**
- A ship firing Tomahawks no longer crashes the server.
- Formations that took over a base's garrison actually drive.
- Ground units are kept clear of runways and taxiways (150 m), and nothing
  patrols an airfield, so AI flights take off and helicopters no longer blow
  up on spawn. A fixed-wing AI flight still on the ground 15 minutes after a
  parking start is removed.
- The dashboard answers in about a second instead of ten, so no more
  "engine slow" errors.
- Fixed-wing transports with no parking spot at a pad or ship start in the air
  over the field; a road a convoy got stuck on is treated as cut for 3 hours.
- A JTAC drone whose aircraft has gone is removed, and its side is told.

**For server admins**
- Flight replay needs `tacview_dir` set per server, matching Tacview's export
  folder. Recordings are kept 30 days (`--replay-days`).
- bfdb's daily backups now really happen on a server restarted more than once
  a day.
- A damaged database says how to recover, and `/feops db_restore` in Discord
  puts back the newest daily backup (the damaged copy is kept).

## v2.2 — October 7, 2026

**Command: a real command display**
- Zones of control, the supply network (cut routes in red), your air-defence
  and radar coverage, and the enemy air defences you know of, on the map.
- Routes with waypoints, and rules of engagement and pace per formation. See
  [Command](../advanced/command.md#moving-formations).

**Command: the full order catalogue**
- Everything your side can do is on the COMMAND panel, at the place you pick:
  air operations, bombers, missile strikes, deployments by road, convoy
  ambushes, reinforcements, naval strikes, hunter groups, transfers. See
  [Command](../advanced/command.md#the-order-catalogue).
- **Naval strikes actually fire**: the ships carrying cruise missiles launch
  them in DCS (before, no ship ever fired).
- **The ground war happens in DCS**: moving and fighting formations are real
  DCS groups; nothing advances or fights off the map.
- **The combat log is kept for the whole round**: every contact, battle, loss,
  capture, order, garrison reaction and hunter group report is saved and
  survives server restarts; only a campaign reset clears it. **LOAD OLDER** on
  the COMBAT LOG pulls in the rest of the round.

**A harder, less predictable enemy**
- **Bases defend themselves**: garrisons drive out to meet enemy vehicles, call
  in a quick reaction force if they stay, and patrol inside the base. See
  [Ground war](../advanced/ground-war.md#bases-defend-themselves).
- **Mobile SAMs shoot and scoot**: after firing, and now and then anyway, they
  move somewhere else inside their base.
- **SEAD works**: SAM sites no longer all shut down perfectly for every HARM.
  Only the site a missile is flying at can react, only some sites are drilled
  to (which ones changes every round), and TALD/MALD decoys in the air make a
  drilled site likelier to miss the real missile. HARMs fired by players are
  now seen by the air-defence network too.
- **Enemy fighters are less predictable**: a reactive CAP may launch at once
  or several minutes into your attack.

**Command** — see [Command](../advanced/command.md)
- **GROUND COMMAND is now COMMAND**: every asset your side has in the field on
  one live map (AI flights, convoys, deployed units, troops, batteries,
  carriers, and the ground war), with the enemy aircraft and formations your
  side can see.
- Commanders order all of it: move formations or troops to any point,
  station or recall AI flights, fire batteries, sail carriers, send convoys and
  helicopter runs, and launch the Theatre HQ's operations. All of it is paid
  from the side's treasury and checked by the server.
- **Command is earned**: it unlocks at rank tier
  {{cfg:command.commander_rank|4}} (Major by default). Admins can grant or
  withdraw it. Commanders get the server's Commander role on Discord.

**Ground war**
- A formation carrying out a player's order is put into DCS, and so is one that
  comes within reach of any enemy ground unit already in DCS.
- Towed and emplaced guns (KS-19, ZU-23 emplacements, mortars) no longer join
  formations: DCS can't move them.
- A commander's **Move** is a road march: the column keeps going past enemies
  it only sees and returns fire, stopping to fight only within about 3 km.
- SAM launchers are no longer mistaken for artillery: fire missions stopped
  picking a Tor that would never fire.
- COMMAND keeps working when the server is busy, showing the last good picture
  marked as old instead of "assets offline".
- The F10 attack arrows point at the target again.

## v2.1 — October 4, 2026

**Theatre HQ** (where enabled) — see [Theatre HQ](../advanced/theatre-hq.md)
- Each side has an AI commander that sets a posture and main effort and runs the
  war's missions and logistics from the treasury: air packages, artillery and
  missiles, convoys and helo supply, troop insertions, reinforcements, AWACS and
  tankers.
- Strike packages: bombers always fly with a fighter escort, and SEAD goes in
  first where air defence is known. The HQ picks the right aircraft for each
  target from the server's roster.
- It steps back as more of you fly. Ask it for support with **F10 → Info → HQ**
  or `-request`; `-hq` shows its intent. The dashboard has a **THEATRE HQ** page.

**Ground war** — see [Ground War](../advanced/ground-war.md)
- Fighting is a proper model now: firepower by vehicle type, supply drawn from
  bases along supply lines that can be cut, morale that breaks, columns that
  have to deploy, positions that get dug in, garrisons that fight back,
  artillery in support.
- You only see enemy formations your forces can see: line of sight, night and
  weather all count, and they are remembered where last seen.
- **GROUND COMMAND** on the dashboard is now a live battlefield for your side,
  with every vehicle, battles, your pilots (you are marked YOU) and game-style
  controls.

**No more magic spawns**
- Every AI aircraft starts on the ground at a friendly airfield and takes off;
  heavies need a large stand. An action no field can launch is refused, free.
- Ambush parties, CSAR search parties, reinforcements and captured carrier task
  forces all set out from a base.

## v2.0 — September 28, 2026

**Bases and captures**
- Repairs now wait out an attack: the repair countdown restarts from the last
  moment an enemy was near the base (see
  [Capturing Objectives](../gameplay/capturing-objectives.md)).
- New **Emergency Repair crate** for logistics pilots (where configured).
- **Empty-server protection**: captures against a side with nobody flying take
  longer (where configured).
- The capture reward is no longer skipped when something fails mid-capture.

**Pilots**
- **Lifeline**: pilots who run out of points can keep flying their own aircraft
  with a small free loadout (where configured). See
  [Points and Lives](../gameplay/points-and-lives.md).
- `/fe_side_switch` lets admins move a pilot to the other side from Discord.

**JTAC**
- JTAC marks disappear with their target; drone JTACs report their radio
  frequency in Status.

**AI helo missions**
- Helos plan around known SAM sites, fly low near them, defend themselves and
  re-route under fire; they land at the safest spot in the zone.
- If the helo can't deliver, the squad goes **by road** from the nearest
  friendly base instead of the mission being refunded. See
  [AI Helo Missions](../advanced/helo-missions.md).
- Fixed AI helos sitting cold on the runway (empty fuel templates, parking).

**War news**
- Daily dispatches get an illustration and are posted to Discord (where
  configured).
- Each illustration now pictures that day's actual story -- a town being
  taken, a burnt-out convoy, a SAM site after a strike, jets over the
  valley -- with real, period-correct equipment (2008-era Russian and Georgian
  kit on the Caucasus campaign), in the style of a news wire photo. The
  camera, time of day and weather change from day to day and from server to
  server.

## Before v2.0: what's different from stock Fowl Engine

What changed from the original open-source Fowl Engine before numbered
releases began. It lists only what is **different from stock Fowl Engine**,
not core mechanics (F10 menus, JTAC, cargo, points and lives, capturing
objectives) that were already part of the base engine and are documented
elsewhere in this wiki. Several of these systems have changed since; the
linked pages describe them as they are now.

## Campaign Balance Pass

Applies to the campaigns run on this server rather than to the engine, so the
numbers below move with whichever server you have selected at the top of the page.

**Air defence moved out of the objectives**
Objectives used to carry their own strategic SAM cover — a Buk battery at every
airfield and an S-125 at most other objectives on the Caucasus map, a full
NASAMS battery in 32 Blue objectives on Syria. That made the classified SAM
sites largely decorative: the real threat arrived free with the ground and was
captured along with it. Garrisons now hold **point defence only** (guns,
short-range launchers). Everything with reach is a SAM site you have to find,
or a deployable you pay for. See [Objectives](../gameplay/objectives.md).

**Early warning is now a network you can dismantle**
Both campaigns gained dedicated EWR sites, placed for area coverage and
capturable on the same terms as a SAM site. Syria had none at all before — the
coalition had no radar anywhere on the map. Caucasus had two Georgian sets,
both on the west coast, leaving Tbilisi and Vaziani more than 250 km from the
nearest one.

**Lives are enforced on the 2008 campaign**
The Caucasus campaign now runs with `limited_lives` **on**, tracked per role —
scarce for air superiority, generous for transport. Syria still runs with them
off. See [Points and Lives](../gameplay/points-and-lives.md).

**Points now constrain**
Deployables, troops and premium airframes all cost points on the 2008 campaign,
and the starting grant was cut to match what a sortie actually earns. Previously
every deployable cost nothing and a new pilot arrived with the equivalent of
1,200 air kills. The live prices are on
[Deployables](./deployables.md).

**Period-correct equipment on the 2008 campaign**
HIMARS, M270 MLRS, M109, T155 Firtina, PLZ-05, TOS-1A and Iskander have been
removed from the Caucasus mission — none of them was in that theatre in August
2008. Both sides gained the guns they did have: 2S3 Akatsiya and 2S1 Gvozdika
for Georgia and Russia, plus the 2S9 Nona for the Russian airborne.

## New Campaign Systems

**IADN — Integrated Air Defence Network**
SAM sites are no longer isolated units. They share a live sensor picture across the coalition, keep search radars dark until a real threat is confirmed, and actively defend each other: fire a HARM or other anti-radiation missile at one and it goes dark to deny the lock, while short-range point-defense systems (Pantsir, Shilka) stay alert specifically to shoot the missile down. A new **Command Center** objective ties into overall network integrity.

**Advanced Radar Physics**
Detection is no longer a simple binary "in range = detected." Probability now depends on target aspect (hot/flank/beam/cold), altitude, and closure rate — flying beam-aspect with low closure rate ("the notch") makes you nearly invisible to pulse-Doppler radars, the way it does in reality. Radar tracks are smoothed instead of snapping between raw returns. AWACS and ground-based EWR are properly differentiated (AWACS gets look-down capability, ground radar degrades at low altitude).

**ELINT/SIGINT Intel Database**
Recon flights now build a persistent, decaying intel picture instead of a one-shot report: detected enemy ground units are classified, clustered, and tracked with a confidence score that fades over time (faster for older/lower-quality sources). Shown as F10 map markers and in a radio "intel picture" report.

**Live GCI — proactive AWACS radio calls**
On servers that enable it, a live GCI controller watches the coalition radar picture and transmits calls to players over SRS, addressed by flight callsign — threat, SAM launch, splash, bogey/bandit/hostile with NATO reporting names, coalition-wide group naming ("north group"), merged, faded, split/converge and periodic picture, all fog-of-war true. Players can also key up and ask (bogey dope, picture, declare, commit) where speech recognition is on, and get GCI-flown intercept vectors after a commit. A short "GCI: &lt;callsign&gt; on &lt;freq&gt;" note shows on slot entry. See [Live GCI](../gameplay/gci.md).

**Player Recon Pass**
Recon-capable airframes (server-configured) get an **F10 → Recon** menu. Start a timed pass while within range of an enemy objective, hold station for the dwell time, and every enemy unit your aircraft has line-of-sight to — SAM and AAA sites included — is revealed on the coalition F10 map through the same decaying intel picture. See [Reconnaissance](../f10-menu/recon.md).

**JTAC eyes-on also marks the map**
Any target a JTAC (ground, drone, or player) has confirmed line-of-sight on now feeds the same intel picture automatically — no menu action needed. The mark stays at full confidence while the JTAC keeps watching; once it loses the target the confidence halves every {{cfg:elint.half_life_jtac_secs|3600}} seconds, so the mark stays on the map for hours instead of vanishing the moment the JTAC looks away. See [JTAC System](../f10-menu/jtac.md#f10-map-intel).

**Mechanized Infantry**
Infantry squads can board and dismount IFVs/APCs for overland transport, not just helicopters — troops embark near a friendly vehicle and can be dropped off anywhere it can drive.

**CSAR — Combat Search & Rescue**
A lost aircraft's life can be won back. A downed pilot unit spawns at the crash site; a friendly helicopter can locate and extract them via the F10 → CSAR menu, giving the pilot the life back (where lives are enforced) and paying the rescuer {{cfg:csar.rescue_reward|0}} points. Come down inside a friendly objective's zone and the pilot recovers on their own — no helo needed. A downed pilot is held by an enemy-capture countdown ({{cfg:csar.capture_timer|15}} minutes here), but as long as a friendly rescue helicopter is near them the timer can't take them, so a pickup that's a few seconds late still works. See [Combat Search & Rescue](../f10-menu/csar.md).

**C-130 Hercules & Airdrop System**
A dedicated logistics role: the C-130 can deliver cargo, troops, and vehicles via parachute or LAPES (low-altitude extraction) runs to unprepared landing zones, with automatic detection-and-unpack on delivery — no manual trigger needed. See [C-130 Hercules & Airdrop](../advanced/c130-airdrop.md).

**Capture mechanics — health/infantry gate, troop scaling, consolidation, anti-revolving-door**
A base is capturable once its **health is 20% or less and every infantry defender is dead** — not when its logistics hit zero (a base whose garrison is wiped out to Health 0 drops to Neutral on its own, except a Command Center). The hold timer is **{{cfg:campaign_events.capture_time_secs|180}} s, divided by how many troop squads you have in the zone** (more squads = faster, never under 30 s). You can't stage troops or crates from a base that is itself capturable — bring them from elsewhere.

After the timer completes the base flips, then your troops **hold it through consolidation** ({{cfg:capture_consolidation_secs|300}} s of progress). During that hold **the enemy cannot start a fresh capture timer** — to take it back they have to wipe out your holding troops, and if they do, the base goes **Neutral**: the new garrison isn't on the map until consolidation ends, so it can't hold the base for you. For **{{cfg:campaign_events.capture_cooldown_secs|120}} s after any capture or Neutral flip** a cooldown blocks a new capture timer entirely.

On capture the previous owner's surviving units are removed and the new owner gets only a **light garrison — AAA and infantry, {{cfg:campaign_events.capture_garrison_revive_fraction|0.25}} of it, no SAMs** unless the server sets `capture_garrison_revive_include_sam`. Everything heavier rebuilds slowly through auto-repair or has to be delivered by deployable crate, so a freshly-taken base is a soft target you invest in rather than an instant fortress. A special SAM site is always open to capture (no health or infantry requirement), but the normal timer and consolidation still run. See [Capturing Objectives](../gameplay/capturing-objectives.md).

**Carrier groups change hands**
Disable an enemy carrier (sink its escorts to knock its logistics to 0) and take the naval base it's linked to, and the carrier group now **passes to the captor as their own ships** — the enemy task force is replaced by your coalition's carrier group in the same spot, with your deck slots. It comes across at 50% and can be brought back up with **carrier repair crates** (air-dropped or flown in by helo — {{cfg:carrier.repair_time|1800}} s at one crate, and stacking several divides the time) or, where the server configures them, the **Repair / Respawn Carrier** actions off the naval-base menu. A carrier whose linked naval base is still friendly and stocked also repairs automatically. An `-admin capture <objective> <blue|red|neutral>` command was also added to force any objective to change hands.

**Artillery — missile TELs, salvos, auto-turn**
The JTAC artillery menu now also commands **ballistic/cruise missile launchers** (Scud, Iskander, Silkworm), enforces each unit type's real min/max range, and adds a **"Fire All Groups Together"** salvo and an **"all ammo"** option. Batteries that spawn facing the wrong way now **reposition to bring the launcher onto the target bearing** before firing, so hull-traverse systems that used to silently no-op actually shoot. The **Move** action follows roads instead of driving cross-country. See [Artillery Missions](../advanced/artillery.md).

**ATIS & weather**
The airfield ATIS now reports the **real DCS runway designator** (it was deriving the number from runway heading and sometimes naming a runway that doesn't exist), fixes a mirrored surface-wind bearing, and adds field elevation, QFE, cloud base AGL, visibility and precipitation — all in **both metric and imperial**. Live weather sync also corrects the wind-speed units and syncs the upper (2000 m / 8000 m) wind layers, not just the surface.

**Last Stand**
A coalition reduced to {{cfg:last_stand.trigger_count|1}} primary objective(s) (airbases, naval bases, FARPs) gets a do-or-die countdown timer ({{cfg:last_stand.countdown_secs|3600}} s) instead of being able to turtle indefinitely. See [Round Rules](../gameplay/round-rules.md).

**Automated Convoys & Strategic Infrastructure**
Ground supply convoys run automatically between logistics hubs and the front — and can be interdicted. Real map buildings (warehouses, fuel depots, industrial complexes) near objectives, when destroyed, degrade that objective's logistics until a Logistics Repair Kit or a capture brings it back — a second, independent way to attack enemy supply. Each objective's logistics buildings carry `logi:` pins on the F10 map, for both sides, while the base is awake (an enemy nearby), so the map stays clean. See [Buildings at an Objective](../gameplay/objective-buildings.md). **Factories** are a new objective type that passively produces ground units for their owner. An objective flagged **fully detached** from the supply chain gets no automatic resupply at all — convoy, cargo plane, or otherwise — and has to be sustained by hand with transport crates and C-130 drops.

**Frontline on the F10 map**
When enabled, the division between blue-held and red-held ground is drawn as three smooth lines per front — blue on the blue side, white down the centre, red on the red side, with a gap between them. It's the zero line of a smooth influence field: every owned objective pulls the ground toward its side with a pull that fades over distance, so the front follows the overall shape of the theatre and ignores a lone base behind enemy lines. A theatre with an island and two land borders shows up as several distinct fronts; nothing is drawn where one side holds everything. Redraws whenever an objective changes hands.

**Live Weather Sync**
On servers that turn it on, real-world weather and time are written into the mission just before each scheduled restart, instead of being fixed at mission build time. See [Weather, ATIS & Time](../gameplay/weather.md#real-world-weather-and-time).

**Coalition tasking board**
Any player can post a task — CAP, CAS, SEAD, STRIKE, LOGISTICS, CSAR at a map
mark, or CAPTURE / SUPPLY against a base — and the whole coalition sees the area
and its pin on the F10 map, ranked into the briefing and called out on the GCI
net. Objective tasks **close themselves out** when the coalition actually does
the job. Usually free, {{cfg:actions.Blue.add-task.kind.AddTask.max_per_side|12}} slots per side. See
[The Tasking Board](../gameplay/tasking-board.md).

**AI helo missions**
Order an AI logistics helo to cold-start at a real field, fly, **land**, and
deliver — capture troops into an objective, or supply into a base running dry.
Shoot it down en route and nothing arrives. Orderable from any slot. See
[AI Helo Missions](../advanced/helo-missions.md).

**Capture Advisor**
`F10 → Objectives` now answers "why won't this base flip?" with a card listing
the specific blockers, the repair outlook, the post-capture cooldown, whether a
capture is already running, and which of your troops are in the zone — including
the ones that **cannot capture because of their troop type**. See
[Objectives Menu](../f10-menu/objectives.md).

**Materiel & the war economy**
Repairs and deployments are now paid for in a real commodity that has to
physically reach the base spending it — no materiel, no repair. Production
output scales with how much healthy territory a side holds (a factory is worth
four airbases), hubs keep an operational reserve, and **supply routing follows
the front line**: a base whose road is cut can only be resupplied by air. See
[Materiel & the War Economy](../gameplay/war-economy.md).

**Captured airframes**
Taking a base now inherits a fraction of the aircraft the losing side left on
the ramp — capped per type, and not flyable until the base is consolidated and
repaired.

**Auto-generated briefing & comms plan**
Every round writes its own per-coalition situational briefing — slot-entry
panel, six F10 pages, and the dashboard map — built from what your side has
actually detected. A server-wide frequency plan keeps the two coalitions on
non-overlapping radios. See [The Auto-Generated Briefing](../gameplay/briefing.md)
and [Comms Plan](../gameplay/comms-plan.md).

**Auto-generated navaids**
FARPs, FOBs, logistics hubs, naval bases and carrier groups — the objective
kinds DCS gives nothing — now get generated TACAN, NDB, ICLS, ACLS and Link-4,
allocated per round from per-coalition channel pools and re-lit when a base
changes hands. Listed in `F10 → Info → Navaids Directory`. See
[Navaids & Approaches](../gameplay/navaids.md).

**Objectives and Info menus**
Two new top-level F10 menus: base reports, threat lists and the Capture Advisor
under **Objectives**; your status, the situation briefing, support frequencies,
convoys in transit, navaids, weather and in-cockpit help under **Info**.

**F10 menus page instead of silently truncating**
DCS drops anything past the tenth entry in a radio menu, with no error. Every
list in the engine now spends its last slot on **`More >>`** and carries on
inside it, so the eleventh JTAC, deployable or objective is reachable instead of
invisible.

## New Tools & Interfaces

**In-DCS Cockpit UI**
An overlay panel that renders inside the DCS window itself (EWR reports, a C-130J CARP bombing-solution planner, crate spawning) for players who install it — strictly additive, the F10 menu is unchanged and always available alongside it.

**bfweb Ops Dashboard**
A full web dashboard: live tactical map with pilot names and NATO-style unit symbology (and a light/dark theme), A/A / A/G / Logistics leaderboard tabs, objective Health/Logistics/Supply/Fuel status with colour-graded bars and repair-progress percentages, sortable columns (ascending/descending) on the Objectives, Kill Feed and Admin tables, engine performance history and hardware monitoring, and Discord OAuth login. Carrier groups appear in the Objectives list with full status but no map position (their location stays hidden). Map objective icons are clean lucide symbols with no coloured glow, and the Pilots page flight/kill/deploy logs scroll cleanly on mobile. The **Briefing** page (navaids, radios, artillery, deployables, RWR/HARM threats, kneeboard PDF) is coalition-locked — you get your own side's briefing and can't pull the enemy's.

**Recon Intel Map (TARPS)**
A shared, coalition-only reconnaissance picture on the dashboard, built from F-14 TARPS photography. Upload your TARPS captures and they're geo-referenced from the filename metadata, perspective-warped onto the map, and made visible only to your own side. Includes a corner-drag align editor for lining a photo up with terrain, and it's wiped with the campaign. See [Recon Intel Map (TARPS)](../advanced/recon-intel-map.md).

**bfwiki**
This wiki — an admin-editable player reference, separate from the web dashboard, backed by the same login. Where one bfdb fronts several DCS servers, the wiki carries a **server selector**: the prose is shared, but every campaign number on a page is read live from the selected server's own engine config, so the two never disagree.

**Discord Integration**
Kill-streak achievement announcements, capture alerts with pilot attribution, and bot plugins for announcements, FAQ, rules, tickets, and server administration.

**In-game Help Menu**
A built-in F10 help menu for quick reference without leaving the game.

## See Also

- [Introduction](../introduction.md)
- [F10 Menu Overview](../f10-menu/overview.md)
- [Materiel & the War Economy](../gameplay/war-economy.md)
- [The Tasking Board](../gameplay/tasking-board.md)
- [C-130 Hercules & Airdrop](../advanced/c130-airdrop.md)
- [Capturing Objectives](../gameplay/capturing-objectives.md)
