# Helicopters, Low Level and Ground

Helicopter crews get precision pads, sling and troop courses, ship decks and
combat search and rescue, all round Guam. Fixed-wing transports can airdrop
onto a drop zone. Fast jets have a low-level route round the coast, and
Combined Arms players have a gunnery lane. All of it is graded.

Helicopter work is on **F10 > Range > Helicopter**. Most of it is in **H-1
ANDERSEN**, which runs from the base down to Andersen South and Mt Santa Rosa.

## Precision pads

| Pad | Where | PERFECT inside |
| --- | --- | --- |
| **H-1 Andersen - Precision Pad** | On the base | 3 m |
| **H-1 Andersen - Confined Area LZ** | The Andersen South urban site | 5 m |
| **H-1 Andersen - Pinnacle** | Mt Santa Rosa | 4 m |

They are marked on the ground with tyres, a windsock and cones. **F10 > Range
> Helicopter > Landing pads** lists them.

A landing is graded when you touch down within 60 m of a pad's mark after
approaching it. The grade is your distance from the mark, against the pad's
PERFECT radius:

| Grade | Distance from the mark | Points |
| --- | --- | --- |
| **PERFECT** | Inside the radius | 5 |
| **EXCELLENT** | Inside 2× | 4 |
| **GOOD** | Inside 4× | 3 |
| **FAIR** | Inside 8× | 2 |
| **POOR** | Further | 1 |

**A hard landing caps the grade at FAIR**: more than 500 fpm, measured as the
steepest descent in the last second before touchdown. The card also records how
long you hovered over the mark.

```dcsmessage
LANDING H-1 Andersen - Pinnacle (pinnacle): 2.6 m from the mark, 210 fpm - PERFECT
```

The same PERFECT / EXCELLENT / GOOD / FAIR / POOR ladder grades the sling
loads, cargo, troops and airdrops below, each against its own radius.

## Sling-load courses

**F10 > Range > Helicopter > Sling-load courses** puts a load at the course's
pickup for you:

| Course | Load | PERFECT inside |
| --- | --- | --- |
| **H-1 Andersen - Sling Course** | 800 kg | 5 m |
| **H-1 Andersen - Heavy Lift** | 3,500 kg container | 8 m |

```dcsmessage
SLING H-1 Andersen - Sling Course: 800 kg uh1h_cargo at 260/0.6 nm. Deliver to the DZ 214/1.8 nm from it; perfect inside 5 m.
```

The clock starts when the load leaves the ground (`cargo off the ground`). It
is delivered once it is back on the ground and has stopped moving for 4
seconds; the grade is its distance from the drop zone's centre, and the time
and any damage over 5% are on the card. Starting another course removes your
last load, and a load nobody delivers is removed after an hour. A course counts
as one of your spawns.

## Dynamic cargo

You can also use DCS's own cargo. At a home field, ask the ground crew (F8)
for a load, put it in the cabin with the cargo loader or sling it, and set it
down at any pad, course drop zone or the troop LZ. There is no F10 step: the
range recognises your packages and grades the delivery. **F10 > Range >
Helicopter > Dynamic cargo: where to deliver** lists every destination and its
PERFECT radius.

```dcsmessage
CARGO <cargo> (internal, 1200 kg, intact) delivered to H-1 Andersen - Confined Area LZ: 3.8 m from the mark, 412 s - EXCELLENT
```

A load set down more than 200 m from every destination isn't a delivery; you
can pick it up again. The card says whether it went inside or on the hook.

## Troops

**H-1 Andersen - Air Assault** carries 8 troops (800 kg) from a pickup on the
base to an LZ at Andersen South:

1. Land within 100 m of the pickup and choose **F10 > Range > Helicopter >
   Troops: load at > H-1 Andersen - Air Assault**.
2. Fly to the LZ, land, and choose **Troops: unload here**. You must be on the
   ground within 180 m of the LZ's centre.

The grade is your distance from the LZ's centre: PERFECT inside 15 m.

```dcsmessage
TROOPS H-1 Andersen - Air Assault: 8 delivered, 12 m from the LZ centre (PERFECT), 318 s from pickup
```

## Ship decks

**FD-1 APRA**, off Apra Harbor, has two ships steaming a 4 nm leg back and
forth for deck landings:

| Ship | TACAN | Speed |
| --- | --- | --- |
| **FFG-7 Perry** | 41X | 10 kts |
| **DDG Arleigh Burke** | 42X | 14 kts |

A helicopter landing on either is graded in the ship's own frame. PERFECT is
within 1.5 m of the deck's centreline. A sink rate over 500 fpm relative to
the deck, or a heading more than 30° off the ship's, caps it at FAIR.

```dcsmessage
DECK LANDING FFG-7 Perry (10 kts): 0.9 m starboard of centreline, 4 m aft of the ship's centre, 240 fpm, heading off 6 deg - PERFECT
```

**F10 > Range > Helicopter > Ship decks: where are they** gives their bearing
and range. The **Deck landings** leaderboard needs two.

## CSAR

**CS-1 SOUTH GUAM** is the jungle ridges round Mt Lamlam. Ask for a mission
with **F10 > Range > Helicopter > CSAR > CS-1 South Guam**:

- **Pilot down (cold)**: just the survivor.
- **Pilot down, enemy troops near (HOT)**: two riflemen, an RPG, a MANPADS and
  a ZU-23 truck about a kilometre from him, and they really shoot.

You need to be in a helicopter, and you can only have one survivor waiting at a
time. He is somewhere on open ground in the area. The MAYDAY gives only a rough
position, circled on your F10 map:

```dcsmessage
MAYDAY MAYDAY: pilot down in CS-1 South Guam (CSAR 3). Last known position 13.33N 144.69E, 198/12nm from you, circled on your F10 map. Beacon 350 kHz AM: tune your ADF. He pops smoke (flare at night) when he hears you within 3 km. Land within 50 m of him, then bring him to any friendly airfield, FARP or ship.
```

1. **Find him.** The circle has a 2.5 km radius and is not centred on him. Tune your ADF to **350 kHz** for his beacon.
2. **Get close.** Within 3 km he pops green smoke, or a green flare at night:
   `"I hear you! Green smoke out!"`
3. **Pick him up.** Land within 50 m of him, or hover over him (within 20 m,
   below 6 m, nearly still), for 8 seconds.
4. **Bring him home** to any friendly airfield, FARP or ship.

The grade is the time from the MAYDAY to delivering him:

| Grade | |
| --- | --- |
| **PERFECT** | Home inside 20 minutes |
| **EXCELLENT** | Inside 30 minutes |
| **GOOD** | Inside 45 minutes |
| **FAIR** | Longer, or picked up but never delivered |
| **POOR** | Not reached |

A mission no one picks up within an hour ends as not rescued. **CSAR areas and
missions** on the same menu shows what is running. The **CSAR** leaderboard
ranks the fastest rescue.

## Airdrops and air-land

Fixed-wing cargo aircraft such as the C-130J use the same DCS cargo. Load at
Andersen or Won Pat with the ground crew (F8), then:

- **Airdrop** it out of the ramp over the **Orote** drop zone, Orote's disused
  airfield, running in on **070** along its old runway. The moment the load
  leaves the bay in flight is your release; where it comes to rest is scored
  against the point of impact: PERFECT inside 50 m. A load that comes down more
  than 1,500 m from the drop zone isn't scored.
- **Air-land** it: land at another field and unload it there.

```dcsmessage
AIRDROP <cargo> (2000 kg) on Orote DZ: 64 m from the point of impact (48 m long, 42 m right), released 820 ft AGL at 128 kt, 31 s under canopy - EXCELLENT
```

The airdrop card has your release height and ground speed, long/short and
left/right against the run-in, the time under canopy and the drift from the
release point.

## Low-level route

**LL-1 GUAM COAST** runs round the coast from Ritidian to Orote through 8
gates: RITIDIAN, PATI, PAGO, TALOFOFO, INARAJAN, COCOS, UMATAC and OROTE. Each
gate is a 1,000 m circle on the F10 map.

| | |
| --- | --- |
| Ceiling | 500 ft AGL |
| Floor | 100 ft AGL |
| Planned speed | 420 kts |
| Time-on-target tolerance | 15 s |

**F10 > Range > Low level / Landings > Low-level routes: gates and times**
gives each gate's heading and planned time. Fly through gate 1 low and the
clock starts:

```dcsmessage
LOW LEVEL LL-1 Guam Coast: clock running at gate 1 RITIDIAN. Ceiling 500 ft AGL, 420 kts. Next PATI 122/6.6nm, due 0:57
```

Each gate after it is timed against the plan, and your height is sampled the
whole way. Overfly a gate's circle to make it; miss one and you hear
`MISSED GATE` and the next one comes up.

You start on 5 points and lose:

| For | 1 point | 2 points |
| --- | --- | --- |
| Each missed gate | Yes | |
| Time above the 500 ft ceiling | More than 5% of the route | More than 20% |
| Time at the last gate against the plan | More than 15 s out | More than 45 s out |
| Below the 100 ft floor | More than 5 s | |

Points left map onto PERFECT (5) down to POOR (1 or less). You need at least
two gates for a card. A run ends at the last gate, or if you take more than
twice the planned time plus 10 minutes, or wander more than 40 km from every
gate.

| Qualification | Needs (rolling 90 days) |
| --- | --- |
| Low Level | 2 low-level routes with every gate, graded GOOD or better |

## Combined Arms gunnery

**G-1 NORTHWEST FIELD** is a gunnery lane for Combined Arms players, firing
east-north-east from a flagged firing line. Two T-55s and a BMP-1 stand at
800, 1,200 and 1,600 m, and come back two minutes after one is killed.

A lane session is graded when you have killed everything or have gone two
minutes without a hit. The score is the share of the lane you killed, out of
5, and the card has your shots, hits, first-round hits and time.

## Your record

| Qualification | Needs (rolling 90 days) |
| --- | --- |
| Helo Precision | 5 precision landings graded GOOD or better |
| Helo Cargo | 3 cargo deliveries, sling or internal, graded GOOD or better |
| Deck Qual | 3 helicopter deck landings on a ship under way, GOOD or better |
| CSAR | 2 downed pilots found, picked up and brought home |
