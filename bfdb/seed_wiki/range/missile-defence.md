# Missile Trainer, Threats and Air Combat

The range is live fire you survive. The **missile trainer** removes any missile
that would have killed you, SAMs included, and grades how you defended. That
is what makes the SEAD range, the hot zone and the escorted convoy safe to fly
into weapons free. This page covers the trainer, the air-to-air set-ups and
duels, and the threat ranges.

## The missile trainer

The trainer follows every air-to-air missile and SAM in the air. It checks each
one's distance to its target every 5 seconds while it is far away, more and
more often as it closes, and every frame inside a kilometre. If a missile gets
within its **kill radius** of a protected player, it is deleted before the
fuze can act, and both sides are told:

| Missile | Kill radius |
| --- | --- |
| Warhead under 50 kg | 200 m |
| Warhead of 50 kg or more | 500 m |

```dcsmessage
SPLASH! R-77 from MiG-29S (MiG-29S) would have killed you - closest 140 m
```

A missile that goes away without ever getting that close was defeated:

```dcsmessage
DEFEATED: R-77 from MiG-29S - closest 1830 m
```

When a missile is launched at you, you get a warning with the shooter's
bearing and range and the two notch headings:

```dcsmessage
MISSILE LAUNCH: R-77 from 041 for 14.2 nm (MiG-29S). Notch 131 / 311
```

**The trainer is on for everyone when they spawn.** Turn it off for live
missiles with **F10 > Range > Air-to-Air > Missile trainer OFF (live
missiles)** or in chat with `-range trainer off`; `-range trainer on` brings it
back. Once you turn it off it stays off for you when you respawn.

It protects players only, never the AI.

**Guns and AAA are always real.** DCS gives a script no way to catch a bullet,
so gun damage in a dogfight, and the AAA over the medium and hard targets, can
kill you.

### How a missile shot is graded

Every trainer shot makes a card for the defender, and for the shooter if a
player fired it:

| Outcome | Means | Defender's points | Shooter's points |
| --- | --- | --- | --- |
| **DEFEATED** | It never got inside the kill radius | 5 | 1 |
| **SPLASH (trainer kill)** | It would have killed you | 1 | 5 |
| **HIT** | It really hit (trainer off) | 1 | 5 |
| **TIMED OUT** | Lost track or self-destructed | none | none |

The card records the launch geometry (range, aspect, both altitudes and
speeds, closure) and your defence: how long you took to react (your first turn
of more than 30°), how long you spent with the missile hot on your nose (inside
45°), on the beam (70° to 110°) and dragging it (more than 135° off the nose),
how much height you gave up, and whether you went below 1,000 m AGL.

The **Missile defence** leaderboard ranks the share of trainer shots you
defeated.

| Qualification | Needs (rolling 90 days) |
| --- | --- |
| Missile Defense | Defeat 5 missile-trainer shots |

## Air-to-air set-ups

AI adversaries are spawned around you from **F10 > Range > Air-to-Air**. The
menu can't build a command for every combination, so you choose in
**Set-up selection** and then spawn it:

1. **Set-up selection > Set-up**: BFM offensive, BFM defensive, BFM neutral
   pass, BFM high perch, BFM head-on, BVR hot, BVR flanking, BVR beaming, or
   Missile defence drill.
2. **Adversary**: MiG-29S, Su-27, MiG-21bis, F-5E, F-16C, F-15C, F/A-18C,
   J-11A or JF-17.
3. **Weapons**: Guns only, Fox 2 (IR), Fox 1 (semi-active radar) or Fox 3
   (active radar).
4. **Skill**: Average, Good, High or Excellent.
5. **BVR range**: 10, 20, 30, 40 or 60 nm. **Number**: 1, 2 or 4.
6. **Show selection** reads it back. Then **FIGHT'S ON (spawn selection)**.

Every pick answers with the whole selection. Until you change it, it is BFM
offensive against one MiG-29S, guns only, High, 30 nm.

```dcsmessage
Selection: offensive vs MiG-29S x1, guns (High), BVR range 30 nm. F10 Range > Air-to-Air > FIGHT'S ON to spawn.
```

You have to be airborne. Where the adversary appears:

| Set-up | Adversary |
| --- | --- |
| BFM offensive | 1,000 m ahead of you, same heading, a little slower: you are behind it |
| BFM defensive | 1,000 m behind you, a little faster |
| BFM neutral pass | 3,000 m ahead, 300 m to the side, coming the other way |
| BFM high perch | 1,800 m ahead and 900 m below you |
| BFM head-on | 9,000 m ahead, coming the other way |
| BVR hot / flanking / beaming | At the BVR range, pointing at you, 45° off, or on the beam |

The range site's spawner offers the same, plus BVR presentations high or low
and ranges up to 80 nm. A BVR set-up with "guns only" picked on F10 gets Fox 3
instead. The adversary's missiles are whatever its pylons accept for the class
you chose.

**Missile defence drill** spawns one shooter at your BVR range that fires at
you. The trainer removes the missile if it would have killed you and grades
your defence.

### How an engagement ends

| Result | When | Points |
| --- | --- | --- |
| **WIN** | You killed it (for real or a trainer kill) | 5 |
| **LOSS** | It killed you (for real or a trainer kill) | 1 |
| **DRAW** | 15 minutes with no result | 3 |
| **ABORTED** | You left the aircraft or the adversary was despawned | none |

```dcsmessage
BFM offensive (guns) vs MiG-29S: WIN after 94 s
```

The card has the shots you fired, trainer kills and gun hits, and the tracks of
both aircraft. The adversary is removed after 30 minutes at most.

## Duels

Fly against another player with **F10 > Range > Air-to-Air > Duel (PvP)**:

1. **Challenge nearest player** picks the nearest airborne player in an
   aeroplane within 20 nm.
2. They get `DUEL: ... challenges you` and have 90 seconds to choose **Accept
   challenge**.
3. Both of you hear `FIGHT'S ON. Missile trainer protects both; 10 gun hits
   wins.`

A duel is won by a trainer kill, a real kill, or **10 gun hits** on the other
aircraft. After 15 minutes it is a draw. **Cancel / knock it off** calls it
off, and so does either of you leaving. ATCAA 2 (W-12), round W-517, is the
duel airspace. The **Duel ELO** leaderboard rates duels from 1500.

## R-3 Aguijan: the SA-8 emitter

An SA-8 on Aguijan with its radar on and weapons held: somewhere to watch your
RWR and practise the reaction without being shot at. Only an instructor can
reset it.

## S-1 Rota: the SEAD/IADS range

Rota holds a whole air defence network, weapons free:

| Site | System | Threat ring on F10 |
| --- | --- | --- |
| Sabana EWR | 55G6 early warning radar | |
| SA-10 battery | SA-10 Grumble | 75 km |
| SA-2 site | SA-2 Guideline | 43 km |
| SA-11 battery | SA-11 Buk | 35 km |
| SA-6 battery | SA-6 Kub | 25 km |
| Pantsir point defence | Pantsir-S1 | 20 km |
| SA-3 site | SA-3 Goa | 18 km |

It behaves like a network, not a row of targets:

- **The early warning radar is always up.**
- **The SAMs stay dark** until the network sees a target inside that site's
  range. Only aircraft inside S-1 (and a few miles round it) wake the network.
- **Point defence** (the Pantsir) watches for itself.
- **Kill the EWR** and every site has to look for itself.
- **Fire an anti-radiation missile** at a site and it shuts its radar down for
  about 50 seconds.
- **A site that can no longer shoot is DOWN**, and is rebuilt 15 minutes later.

The missile trainer is your safety net. **F10 > Range > Air-to-Ground > SEAD /
IADS ranges > Enemy air defence sites** lists the sites; rebuilding a network
early is for instructors.

### How a SEAD kill is graded

Every radar, launcher or command post you kill is a card:

| Points | For |
| --- | --- |
| **5** | An emitter killed while its radar was up: the job done right |
| **4** | An emitter killed while it was dark, or the kill that took the site down |
| **3** | Anything else |

Emitters are search and track radars, the EWR, the SAM vehicles and AAA.
"Radar up" is judged at the moment your weapon left the rail.

```dcsmessage
SEAD S-1 Rota IADS: AGM-88C killed the SA-6 Kub track radar (radar was up), launched 17.4 nm - SA-6 battery is DOWN
```

The card also has your weapon and its guidance, and how many SAMs the network
fired at you that sortie and how many would have killed you. The **SEAD
kills** leaderboard ranks radar-up kills first.

| Qualification | Needs (rolling 90 days) |
| --- | --- |
| SEAD | 3 IADS kills made while the site's radar was up |

## HZ-1 Saipan: the hot zone

The hot zone is somewhere to go and fight without setting anything up. It is a
7 nm circle round Saipan:

- **Red CAP**: while any of you is inside, up to two pairs of fighters (J-11A,
  Su-27 or MiG-29S, skill Good, Fox 3) launch from the far edge at 22,000 ft. A
  new flight comes 5 minutes after one is lost; when the zone has been empty for
  10 minutes they go home.
- **Ground**: armour at Aslito, a Pantsir/Tor site, an artillery battery, SHORAD
  at Marpi and a command post, each respawning 10 minutes after it is wiped out.
- **AWACS Overlord 1** on **251.5**, on your side, orbiting well clear.

Flying in:

```dcsmessage
HOT ZONE HZ-1 Saipan: weapons free, everything here fights back. Overlord 1 on 251.5 MHz; picture on F10 > Range > Hot zones. Missile trainer ON.
```

**F10 > Range > Air-to-Air > Hot zone: AWACS picture** gives you Overlord's
picture of the CAP with BRAA from you; **Hot zones: status** tells you how many
CAP flights are up and how much of the ground is left.

Each trip through the zone is one card. It ends when you have been outside for
a minute, land, or are shot down. A trip under 30 seconds with no kills isn't
recorded.

| Score | |
| --- | --- |
| Start | 3 |
| At least one kill | +1 |
| Three kills or more | +1 more |
| Each missile that would have killed you | −1 |
| Shot down | 1, whatever else |

The score stays between 1 and 5. The card lists your air and ground kills, time
inside, shots fired and missiles defeated.

| Qualification | Needs (rolling 90 days) |
| --- | --- |
| Hot Zone | 3 hot-zone sorties with a kill, no trainer death and a clean egress |

## Anti-ship

| Where | What |
| --- | --- |
| **AS-1 SINKEX**, in W-517 | A cargo ship and a tanker, undefended, steaming west. Respawn 15 minutes after the first is sunk |
| **AS-2 ESCORTED CONVOY**, in W-13B | Merchants under a Krivak, a Neustrashimy and a Tor-armed corvette. Weapons free; respawn 20 minutes after the first loss |
| **F10 > Range > Anti-ship** | An undefended ship of your choice (cargo ship, tanker, bulk carrier, supply ship) 30 nm ahead of you, under way. You have to be pointing at open water |

Defended warships (Molniya, Grisha, Krivak, Neustrashimy) can be spawned from
the range site's spawner by instructors only.

A weapon is scored against a ship if it hits one, was aimed at one, or lands
within 500 m of one (any distance for an anti-ship missile):

```dcsmessage
ANTI-SHIP AGM-84D on Dry-cargo ship-1: HIT - launched 38.6 nm, 312 s flight, 42% damage
```

A **HIT** is 5 points and a **MISS** 1. The card has the damage, whether the
ship sank, the launch range against the weapon's maximum and the time of flight. An
anti-ship missile that missed by more than 200 m is marked as intercepted.
