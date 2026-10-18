# Debriefing With Flight Replay

Every sortie flown on the server can be replayed in your browser, in 2D or 3D,
from the server's own Tacview recordings. You don't need Tacview installed.
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

## The viewer

| Control | What it does |
| --- | --- |
| **2D / 3D** | Flat map, or a 3D theatre with satellite imagery, terrain and earth curvature |
| **MAP / SAT** | The basemap in 2D |
| **Flights** tab | Every flight, filtered **PLAYERS / AI / ALL**, with search. Each row shows kills, shots and hits, and a mark if it was destroyed. **NOW** jumps to what's airborne at the current time |
| **Shots** tab | Every weapon fired (see below) |
| **Events** tab | Takeoffs, landings, kills and losses in time order |
| Labels, Weapons, Ground, Trails | Toggles for what's drawn |
| Timeline **FLIGHT / ALL** | The timeline covers your chosen flight, or the whole mission |
| Download | The original `.acmi`, if you want to open it in Tacview |

Playback runs from 0.25× to 64×.

| Key | Action |
| --- | --- |
| Space | Play / pause |
| ← / → | Back / forward 10 s (Shift: 60 s) |
| `,` / `.` | Step 0.25 s |
| Esc | Stop measuring |

**Cameras.** In 3D: FREE, ORBIT, CHASE, COCKPIT, and PADLOCK while you're
measuring to a target. In 2D: FREE and FOLLOW.

Real 3D models are used for most aircraft, helicopters, armour, SAMs and ships,
and for common missiles and bombs in flight. Anything without its own model is
drawn as the nearest lookalike. Model credits are shown in 3D.

## Debrief tools

**Telemetry.** The selected flight shows ALT, AGL and vertical speed, plus G,
IAS and Mach worked out from the track (marked `~`, since they're estimates).
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
