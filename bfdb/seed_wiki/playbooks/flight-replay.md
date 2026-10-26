# Debriefing With Flight Replay

Every sortie flown on the server can be replayed in your browser, in 3D or on
the map, from the server's own Tacview recordings. The viewer works like
Tacview, so you don't need Tacview installed. The servers still record with
the real Tacview; the dashboard only plays the recordings back.
Open **REPLAY** in the dashboard sidebar (with RANKINGS, PILOTS and KILL FEED).

## Finding a sortie

- **REPLAY** lists the recordings, newest first. The page says how long they
  are kept (30 days by default).
- A recording shows up **a few minutes after the mission ends**. The server
  waits for the file to settle first.
- Your **Flight Log** on PILOTS / MY PROFILE has a **Replay** column with a
  **WATCH** link for each recorded sortie. It opens the replay focused on
  your aircraft.
- **Copy link** gives a URL that opens on the same flight at the same moment,
  so you can send someone "watch this merge".

Anyone can watch a replay, signed in or not, and **there is no fog of war**:
you see both sides. Some servers hold new recordings back from non-admins for
a while after the mission ends, so a replay can't be used to find where the
enemy is right now. Recordings from a test server are admin-only.

## Your own Tacview files

**Open a Tacview file** on the REPLAY page, the upload button in the viewer,
or dropping a `.acmi` anywhere on the viewer, plays a recording from your own
computer: a single-player sortie, another server's file, anything Tacview
recorded. The file is read in your browser and never uploaded. Plain
(`.txt.acmi`) and zipped (`.zip.acmi`) files both work, including ones cut off
by a crash.

## The viewer

The picture fills the screen. The object list is on the left, the selected
object's telemetry on the right, and the timeline with its event marks along
the bottom.

| Control | What it does |
| --- | --- |
| **3D / MAP** | A 3D theatre with satellite imagery, terrain, sky and earth curvature, or the top-down map |
| **DARK / SAT / TOPO** | The basemap on the map view. TOPO is a topographic map with hillshade in the style of the DCS F10 map |
| **RULER** | Click two points on the map for distance and bearing |
| **LAYERS** | Labels (MIN / STD / FULL), trails (15 s / 1 min / 5 min), ribbons, velocity vectors, altitude lines, lat/lon grid, airfields, weapons, ground and sea, flares/chaff/tracers, radar locks, radar sweeps, threat rings, bullseye, cockpit HUD; which coalitions to show; FT/KT or metric; object size (1× to 4×) |
| **Flights** tab | Every flight, filtered **PLAYERS / AI / ALL**, with search. Each row shows kills, shots and hits, and a mark if it was destroyed. **NOW** shows only what's airborne at the current time |
| **Objects** tab | Everything in the picture now, by coalition and category (aircraft, helicopters, weapons, air defence, ground, sea, static), as Tacview's object list. **ALL** lists every object in the recording |
| **Shots** tab | Every weapon fired (see below) |
| **Events** tab | Takeoffs, landings, kills, losses and Tacview's own messages and bookmarks, in time order |
| **Stats** tab | Every flight in one sortable table: shots, hits, kills, how it ended, time flown |
| **◂ EVT / EVT ▸** | Jump to the previous / next kill, shot or bookmark (the followed object's own, when you follow one) |
| Timeline **FLIGHT / ALL** | The timeline covers your chosen flight, or the whole mission |
| Upload / Download | Open a Tacview file; download the original `.acmi` to open it in Tacview |
| **ⓘ** | The keys and tools, below |

Playback runs from 0.25× to 128×.

| Key | Action |
| --- | --- |
| Space | Play / pause |
| ← / → | Back / forward 10 s (Shift: 60 s) |
| `,` / `.` | Step 0.1 s (Shift: 1 s) |
| `[` / `]` | Slower / faster |
| Tab | Next aircraft in the air (Shift: previous) |
| 1 2 3 4 5 0 | Cameras: external, chase, cockpit, satellite, padlock, free |
| M | Map / 3D |
| L T R G | Labels, trails, ribbons, threat rings |
| B | Measure to… |
| D | Ruler |
| V | Velocity vectors |
| Page Up / Page Down | Previous / next event |
| Esc | Stop measuring, then stop following |

**Cameras.** In 3D: EXTERNAL (orbit the object), CHASE, COCKPIT (with a HUD),
SATELLITE (straight down), PADLOCK while you're measuring to a target, and
FREE. On the map: FREE and FOLLOW.

**What's drawn.** Labels show the pilot (or callsign), then type, flight
level and speed, and stay clear of each other. Aircraft leave Tacview-style
ribbons in 3D, banked as the aircraft was. Missiles leave smoke trails, flares
and chaff show as they're dropped, and gun bursts as tracers. A dashed line
joins a shooter to whatever its radar has locked, and a radar's sweep is
drawn when the recording carries it. Threat rings (domes in 3D) show how far
each SAM and gun can reach (published ranges, rounded). Bullseyes get rings
every 20 nm.

Real 3D models are used for nearly every aircraft, helicopter, vehicle, SAM
launcher and radar, soldier and ship seen on the servers, and for missiles,
bombs and rockets in flight, including Tacview's own models (WWII aircraft
too). Anything without its own model is drawn as the nearest lookalike.
Model credits are shown in 3D.

## Debrief tools

**Charts.** Under the timeline, plot any of the followed object's values
over time -- altitude, IAS, TAS, ground speed, Mach, vertical speed, G, AOA,
pitch, bank, heading, fuel, throttle, and anything else the recording
carries -- several at once. Hover to read them off; click to jump there.
**EXPORT TELEMETRY** saves the whole track as CSV.

**Where you are.** The bottom of the map shows the cursor's position in
degrees and minutes, MGRS, and the DCS mission-editor X/Z for that map.
Airfields are drawn with their runways and their DCS names. The telemetry
panel gives the object's bearing and range from its coalition's bullseye.

**The sky.** In 3D the sun is where it was over the theatre at that moment,
so a dawn sortie is lit like a dawn sortie and a night one is dark.

**Telemetry.** The selected object shows ALT, AGL, vertical speed, IAS, TAS,
Mach, ground speed, heading, G, pitch, bank and AOA, and fuel, throttle and
radar lock when the recording has them. Values the recording doesn't carry
are worked out from the track and marked `~`. **ALL PROPERTIES** lists every
other value Tacview recorded for it.
The flight summary lists max altitude, speed, Mach, G and distance flown, with
an altitude / speed graph.

**Measuring.** Pick **MEASURE TO…** or shift-click another object to get RANGE,
bearing, altitude difference, CLOSURE, ASPECT and ATA, live as the replay runs.

**The Shots tab** is the missile debrief. Each shot lists the weapon, shooter
and target, **KILL / HIT / MISS**, miss distance, launch range, aspect, both
altitudes, speed and time of flight. Click a shot and the replay jumps to 5 s
before launch with the target already measured, so you can see exactly what
the shot looked like when it came off the rail.

## How kills are worked out

DCS doesn't write kills, takeoffs or landings into a Tacview recording, so the
replay works them out from the tracks:

- A **hit** is a weapon passing close enough to its target: within 75 m for a
  missile, 40 m for a bomb or torpedo, 25 m for anything else.
- A hit is a **kill** if the target leaves the recording within 90 s (aircraft)
  or 15 s (ground units).
- An aircraft that vanishes with nothing near it is shown as **lost in
  flight**.
- Takeoff and landing come from height above ground and ground speed.

So the replay's kill list can differ slightly from the KILL FEED, which comes
from the game's own events.

## See Also

- [Reading the Live Ops Dashboard](./reading-live-ops.md)
- [Points and Lives](../gameplay/points-and-lives.md)
