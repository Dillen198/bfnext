# Range Results and Debriefs

Every graded event on the range, a bomb, a strafe pass, a carrier pass, a
tanker session, a missile shot, a drill, becomes a **result**. You see it as a
message in your cockpit the moment it is graded and as a card on the range
site, and results are posted to the Vector Strike Discord too, with the card
and links to the debrief.

## In the cockpit

Most results stay in the top-right message box for 20 seconds (a carrier pass
a little longer). Each page in this section shows what its messages look
like.

**F10 > Range > Results > My last results** repeats the headlines of your last
five results this session. **Results > Help** is a one-line reminder.

## Points

Most results carry a score from 1 to 5, which is what the leaderboards and
your record average:

| Discipline | 5 | 1 |
| --- | --- | --- |
| Bombs, CAS drill | SHACK | POOR, or for CAS a wrong target or danger close |
| Strafe | DEADEYE | POOR (an INVALID pass has no score) |
| Carrier pass | `_OK_` | `WO` (and `C` scores 0) |
| AAR | 5.0, an A | No contact scores 0, an F |
| Missile shot (defender) | DEFEATED | SPLASH or HIT |
| BFM, BVR, duel | WIN | LOSS (a DRAW is 3) |
| Anti-ship | HIT | MISS |
| SEAD | An emitter killed with its radar up | |
| Pads, sling, cargo, troops, airdrops, CSAR, decks, low level, field landings | PERFECT | POOR |
| Hot zone | Three kills or more, no trainer deaths | Shot down |
| CA gunnery | The whole lane killed | |

How each one is measured is on its own page:
[Bombing and Strafing](./bombing-and-strafing.md),
[Carrier Passes and Landings](./carrier-passes.md),
[Air-to-Air Refuelling](./refuelling.md),
[Missile Trainer, Threats and Air Combat](./missile-defence.md) and
[Helicopters, Low Level and Ground](./helicopters-and-ground.md).

## On the range site

Everything is at **https://range.vectorstrike.org**. To see your own record
and use the spawner, log in with Discord and link your DCS pilot (below).

| Page | What it shows |
| --- | --- |
| **Range control** (the front page) | The live map: sectors with a colour key, players, stations and their rings, tankers, carriers, spawns and threats, with the clock, wind and QNH beside it, and the latest results |
| **Results** | Every result, filterable |
| **Greenie board** | Carrier passes per pilot, averaged |
| **Leaderboards** | The boards below |
| **Pilot record** (`/me` for yours) | Scorecards, coaching notes, qualifications and trends |
| **Station** | One target's impact group: CEP and the mean point of impact |
| **Brief** | Frequencies, TACANs, weather, every target and SAM site with coordinates (DDM, DMS, MGRS, decimal), laser codes and sectors. Prints as a kneeboard card |
| **Calc** | Bomb release, wind over deck and a tanker planner |
| **Spawn** | The spawner: see [Range Menus and Commands](./menus-and-commands.md#the-spawner) |

The live picture's wind and weather are DCS's own, read from the running
mission. When you are logged in and linked, a toast pops up with a debrief
link the moment one of your results is graded, whatever page you are on.

## The debrief

Each result has its own page. It shows:

- the facts of the result: the numbers behind the grade
- an **interactive plot** from the track: the bomb's fall, the groove, your
  position behind the tanker, the missile's flight against yours
- a **3D replay** of the few minutes round it (switch between 2D and 3D)
- the **result card**: the LSO pass record, the range score sheet or the
  record form, depending on what you flew

And the buttons along the top:

| Button | What you get |
| --- | --- |
| **Card PNG** | The card as an image |
| **Tacview clip** | A small `.acmi` of just this result: a few minutes of the server's recording, cut to your aircraft, what you fired, and what was near you |
| **Web Tacview** | The whole session in the dashboard's flight replay, following you, just before this result |
| **Full session** | The server's whole Tacview recording of the session. It is large |
| **Copy link** | A link to this debrief |

The 3D replay and the Tacview downloads are cut from the range server's own
Tacview recording, so they are there only when a recording covers the result.
The clip works minutes after the pass, while the server is still recording.

## Leaderboards

**range.vectorstrike.org/leaderboards** covers the last 7, 30 (the default) or
90 days, or All (the last year):

| Board | Ranks | Minimum |
| --- | --- | --- |
| **Bombing CEP** | Median miss distance over every bomb (lower is better) | |
| **Strafe accuracy** | Average accuracy on valid passes | 3 passes |
| **LSO points** | Average greenie-board points per graded pass | 3 passes |
| **Air refuelling** | Average AAR score | 2 sessions |
| **Duel ELO** | Player-vs-player duels, rated from 1500 | |
| **Missile defence** | Share of trainer shots defeated | |
| **SEAD kills** | Radar-up kills first, then all kills | |
| **Hot zone** | Air and ground kills, then fewest deaths | |
| **Low level** | Best route score, then the smallest time-on-target error | |
| **Landing grades** | Average runway landing grade | 3 landings |
| **Deck landings** | Helicopter landings on ships under way | 2 landings |
| **CSAR** | Fastest rescue, MAYDAY to delivery | |

The **greenie board** is on its own page; see
[Carrier Passes and Landings](./carrier-passes.md#the-greenie-board).

## Your record and qualifications

Your pilot record has your recent results, your average and best score per
kind of result, weekly trends over the last six months, the airframes you fly,
coaching notes drawn from your recent records (each one links the passes it is
based on), and your qualifications.

Qualifications work like currency: each looks at a rolling window, so one you
earned and haven't flown since lapses.

| Qualification | Needs |
| --- | --- |
| CQ Day | 6 day traps averaging 3.0 LSO points or better (30 days) |
| CQ Night | 4 night traps (30 days) |
| Bombing Unguided | CEP of 15 m or better over your 6 most recent unguided bombs (90 days) |
| Bombing Guided | CEP of 5 m or better over your 6 most recent guided bombs (90 days) |
| Strafe | 3 valid strafe passes at 50% accuracy or better (90 days) |
| AAR | 3 refuelling sessions graded B or better (90 days) |
| Missile Defense | Defeat 5 missile-trainer shots (90 days) |
| Helo Precision | 5 precision landings graded GOOD or better (90 days) |
| Helo Cargo | 3 cargo deliveries, sling or internal, graded GOOD or better (90 days) |
| Deck Qual | 3 helicopter deck landings on a ship under way, GOOD or better (90 days) |
| SEAD | 3 IADS kills made while the site's radar was up (90 days) |
| Hot Zone | 3 hot-zone sorties with a kill, no trainer death and a clean egress (90 days) |
| Low Level | 2 low-level routes with every gate, graded GOOD or better (90 days) |
| Pattern | 5 stable runway landings graded GOOD or better (90 days) |
| CSAR | 2 downed pilots found, picked up and brought home (90 days) |

Each one shows a progress bar on your record.

## Linking your account

The range knows you by your DCS pilot. To see your own record and use the
spawner, link your Discord to it once:

1. In the Vector Strike Discord, run `/linkme`. It gives you a short code.
2. Join any Vector Strike server in DCS and type `-linkme <code>` in chat.
3. Reload the range site. Your record and the spawner unlock.

The login is the same Discord account you use on the Vector Strike dashboard.
