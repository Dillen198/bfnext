# Bombing and Strafing

Every bomb, rocket and air-to-ground missile you release on the range is
followed from release to impact and scored against the nearest target. Gun
passes on the strafe pit are counted round by round. The result reaches your
cockpit a moment after impact and the range site soon after.

## The air-to-ground stations

| Station | Sector | What is there | Respawn |
| --- | --- | --- | --- |
| **R-7201 FDM - Bomb Circle** | R-7201 FDM | A container in the middle of tyre rings, with scoring rings at 25, 50, 100 and 150 m on F10. Unguided and guided bombs, rockets; any run-in heading | |
| **R-7201 FDM - Strafe Pit** | R-7201 FDM | Two containers 30 m apart. Foul line 2,000 ft; run in on 209 from the NNE, rounds go into the sea | |
| **R-7201 FDM - Coordinate Targets** | R-7201 FDM | A hangar, a garage and a command post for JDAM, JSOW and other coordinate weapons | 15 min |
| **TN-1 North Field - Tactical Array** | TN-1 | Two T-72Bs, two BMP-2s and two Urals. An AI drone lases the first live target on code **1688** | 5 min |
| **TN-1 North Field - Moving Convoy** | TN-1 | Three Urals and a BTR-80 driving the old runways at 20 kts | 10 min |
| **TN-1 North Field - Coordinate Targets** | TN-1 | Hangar, garage, command post | 15 min |
| **TN-2 Tinian MLA - Easy / Medium / Hard** | TN-2 | The tiered targets (below) | 5 min |
| **EW-1 Tinian jammer - GPS-denied targets** | EW-1 | Hangar, garage, command post inside a GPS jammer's radius | 15 min |

The respawn timer starts when the first target of a station dies. You can also
respawn a station yourself: **F10 > Range > Air-to-Ground > Stations >
(station) > Reset targets**. **Smoke the target** puts red smoke on its first
live target, and **Info / bearing** gives its bearing, range, elevation, how
many targets are up, its laser code and a note on what it is for.

A station shows as **HOT** in **Range status** while someone has dropped or
fired on it in the last five minutes, and **cold** otherwise.

## How a bomb is scored

The engine reads every player weapon's position 20 times a second. The tick
after it stops existing, the impact is where its last position and velocity
meet the ground. Release altitude, true airspeed, dive angle, heading and the
wind at the release point are recorded with the shot.

The miss is the horizontal distance from the impact to the **nearest target**
of the nearest station, alive or dead. Each weapon class has a **GOOD**
radius; the other grades are fractions or multiples of it:

| Grade | Miss distance | Points |
| --- | --- | --- |
| **SHACK** | 1.53 m or less | 5 |
| **EXCELLENT** | Half the GOOD radius or less | 4 |
| **GOOD** | The GOOD radius or less | 3 |
| **INEFFECTIVE** | Twice the GOOD radius or less | 2 |
| **POOR** | Further | 1 |

| Weapon class | GOOD | EXCELLENT | INEFFECTIVE |
| --- | --- | --- | --- |
| Unguided bombs, cluster bombs | 25 m | 12.5 m | 50 m |
| Guided bombs and air-to-ground missiles | 10 m | 5 m | 20 m |
| Rockets | 30 m | 15 m | 60 m |

So a Mk-82 12 m from the target is EXCELLENT; a laser-guided bomb 12 m out is
INEFFECTIVE.

The message tells you the miss, where it landed as a clock position against
your attack heading (12 o'clock is long, 6 is short), how far long or short,
and your release:

```dcsmessage
RANGE R-7201 FDM - Bomb Circle: Mk-82 12 m @ 6 o'clock (12 m short) - EXCELLENT
released 2950 ft AGL, 455 KTAS, 30 deg dive, 1.1 nm
```

A few things to know:

- **Only the range's stations are scored.** An impact more than 1,000 m from
  every station target gets
  `RANGE: (weapon) impact not near any range target` and no card. Strafe pits
  and ship targets don't take bomb scores.
- **A rocket ripple is one result.** Rockets that keep landing within 1.5 s of
  each other are one pass: the best impact is scored, and the weapon reads
  "(best of 6)" or however many there were.
- **Tiered stations** add the tier to the message (`[MEDIUM]`), and the card
  carries it.

## The tiered targets at TN-2

Three groups, west to east across Tinian, scored exactly like bombs:

| Tier | Targets | Do they shoot back? |
| --- | --- | --- |
| **Easy** | Trucks | No. Learn the weapon |
| **Medium** | APCs and a tank under a ZSU-23-4 and a ZU-23 truck | Yes. The guns are real: stay above 10,000 ft or come in fast |
| **Hard** | T-90s, a BMPT and a BMP-3 under a Tunguska, a Strela-10 and a Pantsir | Yes, weapons free. The missile trainer protects you; the guns don't |

## GPS jamming at EW-1

A GPS jammer on the heights of southern Tinian jams GPS and GLONASS within
5 nm. Flying into its radius you are warned:

```dcsmessage
EW WARNING - EW-1 Tinian jammer: GPS jammed, GLONASS jammed within 5 nm of the jammer (172/4nm). Expect GPS/INS weapons and navigation to degrade: use laser, TV or your own eyes. Kill the jammer to clear it.
```

and `EW: clear of jamming` when you leave. Bombs are scored as usual, but if
you released inside the radius the message and the card say
**GPS DENIED at release**. Kill the jammer to clear it; it is rebuilt 15
minutes later. **F10 > Range > Air-to-Ground > EW: GPS jammers** lists the
jammers with bearing and range, and whether they are down.

## Coordinates for JDAM and JSOW

**Range status** lists every station with its position in decimal degrees.
The range site's **Brief** page has every target and SAM site with its
coordinates in DDM, DMS, MGRS and decimal, and prints as a kneeboard card.

## Strafing

A strafe pass opens when you pull the trigger inside a pit's box:

| | |
| --- | --- |
| Box length | Within 3,000 m of a pit target |
| Box width | Within 300 m either side of your line to the target |
| Height | Below 3,000 ft AGL |
| Heading | Within 60° of the target |

Rounds fired are your gun's ammunition count before and after the pass; hits
are the hits on the pit's targets. The pass closes once you have stopped firing
and are past the box, pulling away beyond 300 m, or two minutes after it
opened.

| Grade | Accuracy (hits / rounds) | Points |
| --- | --- | --- |
| **DEADEYE** | 90% or more | 5 |
| **EXCELLENT** | 75% or more | 4 |
| **GOOD** | 50% or more | 3 |
| **INEFFECTIVE** | 25% or more | 2 |
| **POOR** | Under 25% | 1 |
| **INVALID** | Fired inside the foul line, or no rounds counted | none |

**The foul line is 610 m (2,000 ft)** from the target. Firing while you are
closer than that voids the whole pass:

```dcsmessage
STRAFE R-7201 FDM - Strafe Pit: 41 hits of 120 rounds (34%) - INVALID (fired inside the 610 m foul line)
```

Invalid passes don't count toward the strafe leaderboard or the Strafe
qualification.

## CAS drill with Axeman

TN-1 North Field has an AI JTAC, **Axeman**, an MQ-9 on **133.0 AM**, lasing
on **1688**. Check in with **F10 > Range > Air-to-Ground > CAS / JTAC > Check
in with Axeman** (or the CAS drill on the range site's spawner).

Axeman picks the live target nearest the middle of the tactical array, puts
his laser on it, and passes you a nine-line. He also places a squad of
friendlies 600 m from the target:

```dcsmessage
Axeman, nine-line:
1. IP: TN-1 North Field - Tactical Array
2. Heading 012 (from your position), offset none
3. Distance 18.4 nm
4. Elevation 541 ft MSL
5. T-72B
6. N 15 04.980  E 145 38.220  MGRS ...
7. Mark: laser, code 1688
8. Friendlies: 600 m south-west of the target
9. Egress: as required
Remarks: cleared hot on the laser. Report IN.
```

Your **first impact within 3 km** of the station is the drill's answer:

- **Right target**: your impact's nearest target is the one Axeman designated,
  or your weapon hit it.
- **Danger close**: the impact was within 150 m of the friendlies.
- **Score**: the bomb grade for your miss from the designated target, or 1 point
  for a wrong target or a danger-close drop.

The card records the time from the nine-line to impact as well. The same
impact is also scored on the tactical array as an ordinary bomb. A CAS drill
doesn't count against your spawn limit.

## Your record

Every bomb and strafe pass is on your pilot record on the range site. The
**Bombing CEP** board ranks the median miss over every bomb; the **Strafe
accuracy** board ranks average accuracy on valid passes. The qualifications:

| Qualification | Needs (rolling 90 days) |
| --- | --- |
| Bombing Unguided | CEP of 15 m or better over your 6 most recent unguided bombs |
| Bombing Guided | CEP of 5 m or better over your 6 most recent guided bombs |
| Strafe | 3 valid strafe passes at 50% accuracy or better |

See [Range Results and Debriefs](./results.md).
