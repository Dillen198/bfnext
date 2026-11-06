# Debriefing With Flight Replay

Every sortie flown on the server can be replayed in your browser, in 3D or on
the map, from the server's own Tacview recordings. The viewer works like
Tacview, so you don't need Tacview installed. The servers still record with
the real Tacview; the dashboard only plays the recordings back.
Open **REPLAY** in the dashboard sidebar (with RANKINGS, PILOTS and KILL FEED).

## Debriefing a sortie

```steps
[
 {"title":"1. Find the flight","text":"On **PILOTS → MY PROFILE**, scroll to the **Flight Log**. Every sortie with a recording has **WATCH** in its **Replay** column. Or open **REPLAY** and pick the mission: recordings are listed newest first, with their length, flights and pilots."},
 {"title":"2. Follow your aircraft","text":"**WATCH** opens the replay following your aircraft. Otherwise click any aircraft, or pick one from the **Flights** tab. Press `Space` to play, `[` and `]` to change speed, and `?` for every key."},
 {"title":"3. Check the shots","text":"Open the **Shots** tab. Each shot shows **KILL**, **HIT** or **MISS** with the miss distance, plus launch range, aspect, both altitudes, speed and time of flight. Click one: the replay jumps to 5 s before launch with the target already measured."},
 {"title":"4. Read the merge","text":"**Versus another object** (or `B`, or Ctrl-click the other jet) shows both aircraft's speed, height and G side by side, and the range, closure, height difference, ATA, AA, HCA, pursuit and bearing between them, live."},
 {"title":"5. Chart it and share it","text":"**Charts** (`C`) plots altitude, speeds, G, AOA, fuel and more along the timeline. The link button copies a URL that opens on the same flight at the same moment, so you can send someone the exact merge."}
]
```

## Finding a sortie

- **REPLAY** (the **FLIGHT REPLAY** page) lists the recordings, newest first,
  each with its date, length, flights and pilots. The page says how long
  recordings are kept (30 days unless the server sets otherwise).
- A recording shows up **a few minutes after the mission ends**. The server
  waits for the file to settle first, and the page says how many are still
  waiting to be processed.
- Your **Flight Log** on PILOTS / MY PROFILE has a **Replay** column with a
  **WATCH** link for each recorded sortie. It opens the replay following your
  aircraft. A sortie's row also opens a panel with **WATCH REPLAY** and
  **FULL DEBRIEF →** (see [the sortie debrief](#the-sortie-debrief) below).
- The **link** button copies a URL that opens on the same flight at the same
  moment.

Anyone can watch a replay, signed in or not, and **there is no fog of war**:
you see both sides. Some servers hold new recordings back from non-admins for
a while after the mission ends, so a replay can't be used to find where the
enemy is right now. Recordings from a test server are admin-only.

## Your own Tacview files

**Open a Tacview file** on the REPLAY page, the upload button in the viewer, or
dropping a `.acmi` anywhere on the viewer plays a recording from your own
computer: a single-player sortie, another server's file, anything Tacview
recorded. The file is read in your browser and never uploaded. Plain
(`.txt.acmi`) and zipped (`.zip.acmi`) files both work, including ones cut off
by a crash. A file that isn't a recording is refused with `That is not a
Tacview .acmi file.`

## The viewer

The picture fills the screen. Along the top: the recording's name, date,
length and counts, and how many objects of each side are in the picture now.
The panel on the left lists what is in the recording; the card on the right is
the object you are following; the timeline with its event marks runs along
the bottom.

### Top bar

| Control | What it does |
| --- | --- |
| **‹** | Back to all recordings |
| **Map / 3D** | The top-down map, or a 3D theatre with satellite imagery, terrain, sky and earth curvature (`M`) |
| **Layers** | What the picture shows (below) |
| **Ruler** | On the map: click two points for distance and bearing (`D`) |
| Upload | Open a Tacview file from this computer |
| Download | The original `.acmi`, to open in Tacview |
| Link | Copy a link to this moment (`Link copied`) |
| Full screen | `F` |
| **ⓘ** | Keys and tools (`?`) |

### Layers

| Group | Options |
| --- | --- |
| **Map** (map view only) | **Dark**, **Satellite**, or **Topo** (a topographic map with hillshade in the style of the DCS F10 map) |
| **Display** | Labels (`L`), Trails (`T`, with a length slider from 5 s to 30 min), Ribbons (3D, `R`), Velocity vectors (`V`), Altitude lines (3D), Cockpit HUD (3D), Lat/lon grid (map), Airfields (map) |
| **Objects** | Weapons; Ground & sea; Flares, chaff, tracers |
| **Tactical** | Radar locks, Radar sweeps, Threat rings (`G`), Bullseye |
| **Show** | Sides (**Blue**, **Red**, **Neutral**), Labels (**Name**, **Standard**, **Full**), Units (**ft · kt** or **m · km/h**), Size (1× to 4×) |

### The left panel

| Tab | What it lists |
| --- | --- |
| **Flights** | Every flight, filtered **Players** / **AI** / **All**, searchable (**Pilot or aircraft…**). Each row shows kills, shots / hits, and ✕ if it was destroyed. **In the air** shows only what is airborne now |
| **Objects** | Everything in the picture now, by side and category (Aircraft, Helicopters, Weapons, Air defence, Ground, Sea, Static & other), as Tacview's object list. **Whole recording** lists every object in it |
| **Events** | Kills, losses, takeoffs, landings and Tacview's own messages and bookmarks, in time order. **This object only** narrows to the followed object; **Include shots** adds every weapon release. Click one to jump to just before it |
| **Shots** | Every weapon fired, **FIRED BY** or **FIRED AT** the followed aircraft, or **ALL** (below) |
| **Stats** | Every flight in one sortable table: **Pilot**, **Type**, **Sh** (shots), **Hit**, **K** (kills), **End**, **Time**; **PLAYERS ONLY** to hide the AI |
| **Range** | Only when the recording has graded passes from the training range: each one with its type (bomb, guns, trap, AAR and so on), the pilot and result. Click to jump there; **Debrief ↗** opens the full debrief on the range site |

Hide the panel with **«** to get the map back; the tabs stay as icons down the
edge.

### Playback

The bottom bar has previous / next event (kill, shot or bookmark), back /
forward 10 s, play / pause, the speed (0.25× to 128×), the clock (game time
when the recording has it, else Zulu, plus elapsed time), the timeline,
**Flight / All** (the timeline spans your chosen flight, or the whole
recording) and **Charts**.

| Key | Action |
| --- | --- |
| Space | Play / pause |
| ← / → | Back / forward 10 s (Shift: 1 min) |
| `,` / `.` | Step 0.1 s (Shift: 1 s) |
| `[` / `]` | Slower / faster |
| Page Up / Page Down | Previous / next kill, shot or bookmark |
| Tab | Next aircraft in the air (Shift: previous) |
| 1 2 3 4 5 0 | Cameras: external, chase, cockpit, top, padlock, free |
| B | Versus another object (or Ctrl-click it) |
| Esc | Stop measuring, then stop following |
| M | Map / 3D |
| L T R G V | Labels, trails, ribbons, threat rings, velocity vectors |
| D | Ruler |
| C | Telemetry charts |
| F | Full screen |
| ? | Keys and tools |

**Cameras.** In 3D: **External** (orbit the object), **Chase**, **Cockpit**
(from the pilot seat, with a HUD), **Top** (straight down), **Padlock** (while
you are measuring: behind the aircraft, looking at the target) and **Free**.
On the map: **Follow** and **Free**.

**What's drawn.** Labels show the pilot (or callsign), then type, flight
level and speed, and stay clear of each other. Aircraft leave Tacview-style
ribbons in 3D, banked as the aircraft was. Missiles leave smoke trails, flares
and chaff show as they're dropped, and gun bursts as tracers. A dashed line
joins a shooter to whatever its radar has locked, and a radar's sweep is
drawn when the recording carries it. Threat rings (domes in 3D) show how far
each SAM and gun can reach (published ranges, rounded). Bullseyes get rings
every 20 nm out to 100 nm.

Real 3D models are used for nearly every aircraft, helicopter, vehicle, SAM
launcher and radar, soldier and ship seen on the servers, and for missiles,
bombs and rockets in flight, including Tacview's own models (WWII aircraft
too). Anything without its own model is drawn as the nearest lookalike.
Model credits are shown in 3D.

## Debrief tools

**The followed object's card.** Its name and type, group and callsign, then:

- **Telemetry**: ALT, AGL, VS, IAS, TAS, MACH, GS, HDG, and for aircraft G,
  PITCH, BANK and AOA, plus FUEL, THR and the radar **LOCK** when the
  recording has them. Values the recording doesn't carry are worked out from
  the track and marked `~`.
- **BULLSEYE**: bearing and range from its own side's bullseye.
- **This flight**: **Shots**, **Hits** and **Kills**, **Lost** or **Landed**,
  and the maximum altitude, speed and Mach, G and distance flown.
- **All recorded properties**: every other value Tacview recorded for it.
- **Export telemetry (CSV)**: the whole track, one row per sample.

**Versus.** **Versus another object** (`B`, or Ctrl-click another object)
puts the two side by side (TAS, ALT, G) and gives **RANGE**, **CLOSURE**,
**ALT Δ**, **ATA** (the target's angle off your nose), **AA** (your angle off
its tail), **HCA** (the angle between the flight paths), **PURSUIT** (lead,
pure or lag) and **BRG**, live as the replay runs.

**Charts.** Under the timeline, plot any of the followed object's values over
time: ALT, AGL, VS, IAS, TAS, MACH, GS, HDG, G, AOA, PITCH, BANK, FUEL, THR,
and anything else the recording carries, several at once. Hover to read them
off; click to jump there.

**Where you are.** The bottom of the map shows the cursor's position in
degrees and minutes, MGRS, and the DCS mission-editor X/Z for that map.
Airfields are drawn with their runways and their DCS names.

**The sky.** In 3D the sun is where it was over the theatre at that moment,
so a dawn sortie is lit like a dawn sortie and a night one is dark.

**The Shots tab** is the missile debrief. A summary line counts the shots,
kills, hits and misses. Each shot lists the time, weapon, shooter → target,
**KILL / HIT / MISS** (with the miss distance), launch range, aspect, both
altitudes, speed and time of flight. Click a shot and the replay jumps to 5 s
before launch with the target already measured, so you can see exactly what
the shot looked like when it came off the rail.

## The sortie debrief

A row in your Flight Log opens that sortie's own record: its kills, weapons,
events and a timeline. **FULL DEBRIEF →** opens the **SORTIE DEBRIEF** page:
the mission, combat and logistics from the server's own records, then, when the
flight was recorded, a **Flight profile · Tacview** section with the altitude
and speed profile, the **Ground track**, and a **Shot log** giving each
weapon's time, target, **Closest** approach and result.

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
