# Range Sectors and Airspace

The range is divided into **29 sectors**, each with one job. The big blocks are
the real special use airspace round Guam and the Northern Marianas (the
warning areas, the ATCAAs and R-7201), each doing the job it does in life. The
smaller filled areas inside them are the ranges themselves.

You don't have to learn the map before your first sortie: the server tells you
what a sector is for when you fly into it, and the F10 menu lists them all.

## On the F10 map

The real airspace is drawn as thin closed lines; the ranges are outlined and
lightly filled. Every sector is coloured by its job and labelled with its name
and kind (tanker tracks and the carrier op area carry their frequencies
instead). A legend sits in the empty sea north-west of the islands.

| Colour | Job |
| --- | --- |
| Amber | Air-to-ground |
| Yellow | Tactical / CAS |
| Red | Threat / SEAD |
| Tan | Combined Arms gunnery |
| Lime | Helicopter |
| Lime, dashed | CSAR |
| Cyan | Air-to-air |
| Blue | BVR |
| Pink | Duels |
| Green, dashed | Tanker tracks |
| White | Carrier ops |
| White, dashed | Landing pattern, ship decks |
| White line | Low-level route |
| Violet | Anti-ship |
| Hot red, dashed | Hot zone |
| Teal | EW / GPS jamming |

Also on the map:

- **Red dashed rings**: the threat ring of each SAM site at S-1 Rota.
- **A teal dashed circle**: the GPS jammer's radius at EW-1.
- **The low-level route**: a dashed white line with a circle round each gate.
- **Tanker tracks**: a solid green entry line from the ARIP to the ARCP, and a
  dashed exit line.
- **Scoring rings** round every target station, labelled with the station's
  name (and its laser code where it has one). Strafe pits are orange, SAM sites
  red, everything else yellow. The first ring is solid, the rest dashed.

The ranges are marked on the ground as well: tyre rings round the bomb circle
and the target arrays, a tyre foul line on the strafe pit, flags round the
coordinate targets and along the gunnery lane, and tyre pads and cones for the
helicopters. The markers are neutral, so hitting one isn't a team kill.

## Finding a sector

**F10 > Range > Sectors: what is where** lists every sector nearest first, with
bearing and range from you and what it is for:

```dcsmessage
Vector Strike Range sectors (nearest first):
H-1 ANDERSEN (HELICOPTER) you are here - Pads at Andersen, confined LZ at Andersen South, pinnacle on Mt Santa Rosa, sling and troop courses
PT-1 ANDERSEN (LANDING PATTERN) you are here - Overhead breaks and circuits, RWY 06/24; every landing on every field is graded
G-1 NORTHWEST FIELD (CA GUNNERY) 303/5nm - Combined Arms gunnery lane on the Northwest Field range, firing east-north-east
...
```

**F10 > Range > Range status** names the sector you are in, then every target
station, tanker and carrier with bearing and range.

When you fly into a sector, you get its job in the message box:

```dcsmessage
Entering R-7201 FDM (AIR-TO-GROUND)
Farallon de Medinilla: bomb circle, strafe pit (run in from the NNE along the island), coordinate targets
```

You hear each sector's announcement once, then not again for 15 minutes. Where
two sectors overlap, the smaller one is the one reported, so R-7201 inside
ATCAA 3A reads the way it does on a chart.

## Air-to-air

| Sector | Where | For |
| --- | --- | --- |
| **W-517** | Warning area south-west of Guam | BFM / ACM set-ups, live missile shots. Floor 5,000 ft |
| **ATCAA 2 (W-12)** | Round W-517 | Duels: 1v1 / 2v2 by agreement, missile trainer on, guns are real |
| **ATCAA 5** | West of Guam | BFM away from the W-517 traffic. Floor 5,000 ft |
| **ATCAA 1 (W-11)** | South-east of Guam | BVR presentations, Fox 1 / Fox 3 missile defence |
| **ATCAA 3C (W-13C)** | East of the chain | Intercepts: Fox 3 presentations at range |
| **ATCAA 3A (W-13A)** | Round Farallon de Medinilla | Large-force exercises: strike packages into R-7201 against red air |
| **ATCAA 6** | Over the islands | Intercepts and DCA CAP above FL150; the ground ranges work below it |

## Air-to-ground

| Sector | Where | For |
| --- | --- | --- |
| **R-7201A** | 12 nm ring round Farallon de Medinilla | Hold on the ring, check the range clear, run in to R-7201 |
| **R-7201 FDM** | Farallon de Medinilla | Bomb circle, strafe pit (run in on 209 from the NNE, rounds into the sea), coordinate targets |
| **TN-1 NORTH FIELD** | Tinian | Tactical array (laser 1688), a convoy on the old runways, JDAM targets, JTAC Axeman on 133.0 AM |
| **TN-2 TINIAN MLA** | Tinian | Tiered targets: EASY trucks, MEDIUM armour under AAA, HARD armour under SHORAD |
| **EW-1 TINIAN SOUTH** | Tinian | GPS jamming over coordinate targets |

How these are scored: [Bombing and Strafing](./bombing-and-strafing.md).

## Threats

| Sector | Where | For |
| --- | --- | --- |
| **R-3 AGUIJAN** | Aguijan | SA-8 threat emitter: radar on, weapons hold |
| **S-1 ROTA** | Rota | Live SEAD/IADS: EWR, SA-2, SA-3, SA-6, SA-11, SA-10, Pantsir. Weapons free, networked |
| **HZ-1 SAIPAN** | Saipan | Hot zone: red CAP, SHORAD and targets that fight back. AWACS Overlord 1 on 251.5 |

See [Missile Trainer, Threats and Air Combat](./missile-defence.md).

## Tanker tracks

| Sector | Tanker | TACAN, frequency | Altitude, speed |
| --- | --- | --- | --- |
| **AR RIO** | KC-135 boom | 51Y, 251.0 | FL250, 300 KIAS |
| **AR MIRAGE** | KC-135MPRS basket, next to FDM | 52Y, 252.0 | FL210, 270 KIAS |
| **AR LUXOR** | KC-135 boom | 54Y, 254.0 | FL240, 300 KIAS |
| **AR SHELL** | KC-130 basket, west of Guam | 53Y, 253.0 | 14,000 ft, 220 KIAS |

See [Air-to-Air Refuelling](./refuelling.md).

## Guam

| Sector | Where | For |
| --- | --- | --- |
| **H-1 ANDERSEN** | Andersen | Pads at Andersen, confined LZ at Andersen South, pinnacle on Mt Santa Rosa, sling and troop courses |
| **G-1 NORTHWEST FIELD** | Northwest Field | Combined Arms gunnery lane, firing east-north-east |
| **PT-1 ANDERSEN** | 5 nm round Andersen | Overhead breaks and circuits, RWY 06/24. Every landing on every field is graded |
| **CS-1 SOUTH GUAM** | Round Mt Lamlam | CSAR. Beacon 350 kHz |
| **LL-1 GUAM COAST** | Round the coast | Low-level route, Ritidian to Orote, 8 gates, below 500 ft AGL at 420 kts |

See [Helicopters, Low Level and Ground](./helicopters-and-ground.md) and
[Carrier Passes and Landings](./carrier-passes.md#field-landings).

## At sea

| Sector | Where | For |
| --- | --- | --- |
| **CV OPAREA** | South-east of Guam | CVN-72 (TACAN 72X, ICLS 11, Link-4 336.0) and LHA-1 (TACAN 71X); recovery tanker A-6E on 63Y, 261.0 |
| **FD-1 APRA** | Off Apra Harbor | Frigate deck landings: FFG-7 Perry (41X) and an Arleigh Burke (42X) under way |
| **AS-1 SINKEX** | In W-517 | Undefended hulks steaming west, for anti-ship weapons |
| **ATCAA 3B (W-13B)** | East of Tinian and Saipan | Maritime strike, with the AS-2 convoy in it |
| **AS-2 ESCORTED CONVOY** | In W-13B | Merchants under a Krivak, a Neustrashimy and a Tor-armed corvette. Weapons free |

See [Carrier Passes and Landings](./carrier-passes.md) and
[Missile Trainer, Threats and Air Combat](./missile-defence.md#anti-ship).
