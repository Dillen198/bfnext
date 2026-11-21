# Carrier Passes and Landings

Every pass at the boat is graded, and so is every runway landing at every
airfield on the map. A carrier pass gets an LSO grade, a decoded comment and a
trap sheet; a runway landing gets a touchdown, approach and pattern
debrief. Both reach your cockpit as messages and the range site as cards.

## The boats

Both ships are in **CV OPAREA**, south-east of Guam.

| Ship | TACAN | ICLS | Other | Tower |
| --- | --- | --- | --- | --- |
| **CVN-72 Abraham Lincoln** (Supercarrier) | 72X | 11 | Link-4 336.0, ACLS | 305.0 |
| **LHA-1 Tarawa** | 71X | 12 | | 306.0 |

CVN-72 keeps a **recovery tanker** overhead, Arco 3, an A-6E on 63Y, 261.0, at
6,000 ft and 250 KIAS, and an SH-60B plane guard.

Every 30 seconds the engine reads DCS's wind at the deck and steers each ship so
its own headwind and the true wind come straight down the landing area, aiming
for 27 kts over the deck on CVN-72 and 20 kts on LHA-1. Nothing about the wind is taken
from the mission file; it is always the wind DCS is flying.

**F10 > Range > Tankers / Carrier > Carrier status (BRC/FB/WOD, TACAN, ICLS)**
gives you the numbers now:

```dcsmessage
CVN-72 Abraham Lincoln: BRC 045 FB 036, 27 kt WOD, deck OPEN, TACAN 72X ABL, ICLS 11
LHA-1 Tarawa: BRC 052 FB 052, 21 kt WOD, deck OPEN, TACAN 71X LHA, ICLS 12
```

FB (the final bearing) is the BRC plus the angled deck, about 9° left on the
Supercarrier.

## Getting aboard

You can start on the deck: CVN-72 has F/A-18C, F-14B and F-14A slots, and
LHA-1 has AV-8B and UH-1H slots. Or launch from Andersen and fly out; a pass
is graded whoever you are and wherever you launched from.

## When a pass counts

A pass starts when a fixed-wing aircraft is airborne within 4 nm of the
landing point and below 2,500 ft above the deck. The groove starts when you
are inside three quarters of a mile, tracking within 20° of the final bearing
and roughly on the centreline. How it ends:

| Outcome | What happened |
| --- | --- |
| **Trap** | You touched down and stopped on the deck |
| **Bolter** | You touched down and were airborne again 12 s later |
| **Waveoff** | You flew past the landing area more than 15 m above the deck without touching down |
| **Own waveoff** | You went back out past about 1.2 nm from the groove |

Wander more than 6 nm away before reaching the groove and nothing is graded.
The grade arrives about 8 seconds after the pass ends, so DCS's own LSO has
time to speak first.

## Two graders

1. **DCS's own LSO.** On the Supercarrier and the Tarawa, when DCS grades the
   pass, its grade is the official one on your card.
2. **The range's groove tracker.** It samples your approach 10 times a second
   and grades every pass DCS doesn't, and draws the trap sheet for all of
   them.

The tracker splits the groove into the LSO's four parts and averages each:

| Part | Where |
| --- | --- |
| **X** (at the start) | 0.75 nm to 0.5 nm |
| **IM** (in the middle) | 0.5 nm to 0.25 nm |
| **IC** (in close) | 0.25 nm to 75 m |
| **AR** (at the ramp) | The last 75 m |

In each part it checks three things:

| Check | Against | Calls |
| --- | --- | --- |
| Glideslope | 3.5° (3.0° on the Tarawa) | `H` high: more than 0.4° above. `LO` low: more than 0.3° below |
| Lineup | The landing area's centreline | `LUL` / `LUR`: more than 0.5° off |
| Angle of attack | The on-speed band for the type (F/A-18 and F-14 only) | `F` fast / `SLO` slow |

Each call is written the LSO's way: `(H)` a little, `H`, `_H_` a lot, with
the part after it: `HIM` is high in the middle, `_LULIC_` is a lot lined up
left in close. Thresholds for the bigger glideslope calls are 0.8° and 1.5°
high, 0.6° and 0.9° low; for lineup, 1° and 3°.

It also times the groove: over 20 s is `LIG` (long in the groove), under 13 s
is `NESA` (not enough straightaway).

## Grades and points

| Grade | Means | When the tracker gives it | Points |
| --- | --- | --- | --- |
| `_OK_` | Perfect pass | No calls at all, a 15-19 s groove, and the 3 wire | 5 |
| `OK` | Good pass | Nothing worse than a little call | 4 |
| `(OK)` | Fair pass | A call of normal size | 3 |
| `B` | Bolter | | 2.5 |
| `--` | No grade | A call of the biggest size | 2 |
| `OWO` | Own waveoff | You went round, or were waved off inside the limits | 2 |
| `WO` | Waveoff | Waved off while outside the limits in close or at the ramp: more than 3° off lineup, or more than 1.2° low or 1.8° high | 1 |
| `C` | Cut pass | A crash | 0 |

DCS's LSO can also give `WOP` (pattern waveoff, 1 point), and `WOFD` (foul
deck) or `NC` (no count), which don't count towards your average at all.

The wire is DCS's when it says; otherwise it is estimated from where you
stopped, good to about one wire either way. Server positions arrive at network
rate, so the tracker's numbers are for teaching, not a court of law.

The pass in your cockpit:

```dcsmessage
LSO CVN-72 Abraham Lincoln: (OK) => Fair pass, #2 wire, groove 16.4 s
HIM (LULIC)
high in the middle (second third)
a little lined up left in close (last third)
```

In a Hornet or Tomcat the tracker also checks your hook as you enter the
groove, and adds `HOOK UP in the groove!` if it is up.

Day passes are Case I and night passes Case III.

## The greenie board

**range.vectorstrike.org/greenie** lists every pilot's passes for the last 30
days (or 7 days, 90 days, or All, which is the last year), averaged in the points above. Filter it by
carrier, airframe, and day or night. The **LSO points** leaderboard ranks the
average over at least three graded passes.

| Qualification | Needs |
| --- | --- |
| CQ Day | 6 day traps averaging 3.0 LSO points or better in the last 30 days |
| CQ Night | 4 night traps in the last 30 days |

## The pass card

The card on the range site has the grade and who gave it, the LSO comment
decoded line by line, the wire, groove time, wind over the deck and the final
bearing, and the trap sheet: your glideslope, lineup and angle of attack
through the groove. Before the groove it records the pattern: distance and altitude
abeam, and your altitude at the 90 and at the wake.

## Field landings

**Every runway landing at every airfield is graded**, from Andersen to Saipan.
**PT-1 ANDERSEN** (5 nm round Andersen, runways 06/24) is the place for
overhead breaks and circuits away from the busiest traffic, but the grading
works anywhere.

The engine works out which runway and which end you used from your touchdown
and your own track, then grades:

- **Touchdown** against an aim point 300 m past the threshold, and the
  centreline.
- **Sink rate**: the steepest descent in the last second before touchdown.
- **The approach** at 1 nm and at ½ nm: glideslope against 3°, lineup, and
  descent rate. It is **stable** if, at both, you were within 1° of the
  glideslope, within 3° of lineup and descending at under 1,200 fpm.

You start on 5 points and lose:

| For | 1 point | 2 points |
| --- | --- | --- |
| Distance from the aim point | More than 150 m | More than 300 m |
| Off the centreline | More than 5 m | More than 10 m |
| Sink rate | Over 600 fpm | Over 900 fpm |
| Unstable approach | Yes | |

Touching down short of the threshold is **POOR** whatever else you did.

| Points left | Grade |
| --- | --- |
| 5 | PERFECT |
| 4 | EXCELLENT |
| 3 | GOOD |
| 2 | FAIR |
| 1 or less | POOR |

If you take off again within 15 seconds it is graded as a touch-and-go. Either
way the message comes 15 seconds after touchdown:

```dcsmessage
LANDING Andersen AFB rwy 06R (full stop): 412 m past the threshold (+112 m to aim), 2.1 m right, 380 fpm, 142 kts, stable approach - PERFECT
PATTERN (overhead): tight downwind, 0.5 nm abeam
```

### The pattern and the taxi

The engine reads the last six minutes of your track to find the pattern you
flew (overhead, downwind entry or straight-in) and makes the calls an
instructor would. They are on the card but don't change the grade:

| Call | When |
| --- | --- |
| Wide downwind / tight downwind | More than 1.8 nm or less than 0.6 nm abeam |
| Low downwind / high downwind | Below 600 ft or above 2,500 ft |
| Extended downwind | Base turned more than 1.5 nm past the threshold |
| Short final | Rolled out less than 0.3 nm from the runway |
| Overshot the final | More than 150 m through the centreline after rolling out |

After a full stop it follows your rollout and taxi until you park, leave, take
off again or ten minutes pass, and notes a fast taxi (over 30 kts), a slow
runway exit (over 60 s), and stopping on the runway.

**F10 > Range > Low level / Landings > How landings are graded** gives the
short version in the cockpit. The **Landing grades** leaderboard ranks your
average over at least three landings.

| Qualification | Needs (rolling 90 days) |
| --- | --- |
| Pattern | 5 stable runway landings graded GOOD or better |

Helicopter landings on pads and ship decks are graded separately: see
[Helicopters, Low Level and Ground](./helicopters-and-ground.md).
