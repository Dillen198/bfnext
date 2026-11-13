# Air-to-Air Refuelling

Four tankers fly the range's published refuelling tracks all the time, and a
fifth orbits the carrier. Every time you join one, the range follows you from
the join to the break-away and grades the session from A to F.

## The tankers

| Track | Tanker | Callsign | TACAN | Frequency | Altitude | Speed |
| --- | --- | --- | --- | --- | --- | --- |
| **AR RIO** | KC-135 (boom) | Texaco 1 | 51Y | 251.0 | FL250 | 300 KIAS |
| **AR MIRAGE** | KC-135MPRS (basket) | Arco 1 | 52Y | 252.0 | FL210 | 270 KIAS |
| **AR LUXOR** | KC-135 (boom) | Texaco 2 | 54Y | 254.0 | FL240 | 300 KIAS |
| **AR SHELL** | KC-130 (basket) | Shell 1 | 53Y | 253.0 | 14,000 ft | 220 KIAS |
| Over CVN-72 | A-6E (basket) | Arco 3 | 63Y | 261.0 | 6,000 ft | 250 KIAS |

- **RIO** is entered on the RIO line from W-11.
- **MIRAGE** is next to Farallon de Medinilla: tank before or after R-7201.
  It is the basket for the Navy jets.
- **LUXOR** is entered on the LUXOR line past W-517.
- **SHELL**, west of Guam, is for the slower jets (Harrier, A-4).

Speeds are indicated, the way they are flown on the boom or the basket. The
F10 map shows each track with its entry line (solid, from the ARIP to the
ARCP) and its exit line (dashed). Fly the entry line from the ARIP, join at the
ARCP, and leave on the exit line.

The track tankers are kept on station for the whole mission and respawned if
lost. **F10 > Range > Tankers / Carrier > Tankers on station** lists them:

```dcsmessage
Tankers:
Texaco1 KC-135 (boom) 25000 ft, 251.000 MHz, TACAN 51Y TEX
Arco1 KC135MPRS (basket) 21000 ft, 252.000 MHz, TACAN 52Y ARC
Texaco2 KC-135 (boom) 24000 ft, 254.000 MHz, TACAN 54Y TX2
Shell1 KC130 (basket) 14000 ft, 253.000 MHz, TACAN 53Y SHL
Arco3 A6E (basket) 6000 ft, 261.000 MHz, TACAN 63Y A6E
```

A tanker that is launching, going home or respawning says so after its line.

## A tanker of your own

**F10 > Range > Tankers / Carrier > Tanker on my position** puts a tanker 3 nm
ahead of you on a race-track along your heading, at 20,000 ft with a 20 nm
leg. You pick the type: KC-135, KC135MPRS, KC130, KC130J, S-3B Tanker, A6E or
IL-78M. You have to be airborne. It comes up with its own frequency and a TACAN
channel between 40Y and 59Y:

```dcsmessage
Tanker Shell7 (KC130) ahead of you: 20000 ft, 256.5 MHz, TACAN 41Y T41. Race-track 20 nm along 087.
```

On the range site's spawner you can also choose the altitude (8,000 to
30,000 ft) and the leg (10 to 40 nm). Your tanker counts as one of your
spawns; see [Range Menus and Commands](./menus-and-commands.md#the-spawner).

## How a session is graded

A **session** starts when you are within a mile behind a tanker of your own
side (and within about 600 m either side of it and 300 m above or below). It
ends when you have been more than 2 nm from the tanker for 15 seconds, or after
30 minutes. If you never made contact and left within a minute, there is no
card.

While it runs, the range samples your position in the tanker's own frame five
times a second. You are **in contact** when DCS says you are refuelling, or
when your fuel is actually rising. Each contact and disconnect is called:

```dcsmessage
Arco1: contact
```

You start on 5 points and lose them for:

| Fault | Measured | Cost |
| --- | --- | --- |
| **No contact** | No contact in the whole session | Grade F |
| **Slow join** | More than 4 minutes from the start of the session to the first contact | Up to 1 point, reached at 6 minutes |
| **Disconnects** | Every disconnect after the first | Half a point each, up to 1.5 |
| **Unstable in contact** | The spread (standard deviation) of your position while connected, over 2 m fore-aft, 1.5 m sideways or 1.5 m up and down | Up to 0.6 fore-aft, 0.7 sideways and 0.7 vertical; the worst axis is named |
| **Hot closure** | Closing at more than 5 kts in pre-contact (20 to 60 m behind) | Half a point |
| **Overshoot** | Closing to within 5 m of the tanker's reference point, or passing it, close aboard and not connected | ¾ of a point |

| Score | Grade |
| --- | --- |
| 4.5 or more | **A** |
| 3.75 or more | **B** |
| 3.0 or more | **C** |
| 2.0 or more | **D** |
| Less | **F** |

The spread only counts the samples while you were connected, so the way you
came in and backed out doesn't count against your stability. At the end:

```dcsmessage
AAR Arco1: grade B - 2 contact(s), 1 disconnect(s), 6420 lb in 214 s
Unstable in contact, mostly lateral
```

A clean session reads `Smooth join and stable in contact`.

## The card

The card on the range site has the grade and its reasons, the join time,
contacts and disconnects, time connected, fuel taken (and the onload rate),
the closure at pre-contact, your altitude and speed, and your position in the
tanker's frame through the session.

The **Air refuelling** leaderboard ranks your average score over at least two
sessions.

| Qualification | Needs (rolling 90 days) |
| --- | --- |
| AAR | 3 refuelling sessions graded B or better |

## Worth knowing

- DCS doesn't always report refuelling for a client on a dedicated server,
  which is why rising fuel counts as contact too. Expect a little noise in the
  numbers.
- The A-6E is DCS's AI aircraft refuelling from its buddy store; that it passes
  fuel to players is still to be confirmed in game.
- The range site's **Calc** page has a tanker planner.
