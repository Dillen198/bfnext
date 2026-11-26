# Weather, ATIS & Time

The engine reads DCS's own weather at your field and hands it to you as a text
ATIS, the same way a tower would. It also keeps the campaign's clock honest and,
on servers that turn it on, brings real-world weather and time into the mission
at every restart.

## Where you get the weather

| Where | When | What you get |
| --- | --- | --- |
| **Slot-entry ATIS** | 15 seconds after you take an aircraft or helicopter slot at an airfield or a carrier | The field's ATIS (or the carrier ATIS), on screen for 30 seconds |
| **F10 → Info → Weather** | Any time | The full report for your slot, with winds aloft. Works from any slot |
| **`-weather`** in chat | Any time, while in a slot | The same full report, but only from an airfield or carrier slot (or the server weather, see below) |
| **Situation briefing** | Slot entry, `-brief`, Info → Situation | A one-line `WX:` summary for the whole map |
| **Spoken ATIS** (SRS) | On a loop, if the server runs voice ATC | Each field's ATIS read out on its own frequency |

FARP and FOB slots get no slot-entry ATIS. Use **Info → Weather** there: with
no field to report on, it gives you a general weather brief at your position.

![Timeline of what appears after you take an aircraft slot: the GCI frequency line at once if the server configures one, the airfield or carrier ATIS at plus 15 seconds for 30 seconds, and the situation briefing at plus 22 seconds for 40 seconds by default.](/api/wiki/images/e886ee43-be86-4c7d-a64b-b7a4a294b910)

## The airfield ATIS

Take a slot at an airbase and this arrives 15 seconds later. It is laid out
line for line like DCSServerBot's ATIS, so the two panels you may get on spawn
read the same:

```dcsmessage
ATIS-REPORT FOR KUTAISI
=======================
Code: UGKO
Position: N42°10'36" | E042°28'56" | 38T KM 72906 72402
Altitude: 147 ft
==============================
Tower Frequencies: 134.000 MHz | 264.000 MHz
Runways (# = active): 07 | 25#
Heading: 254° | 74°
==============================
Temperature: 17.62° C
Surface Wind: 271° (265°M) @ 9 kts
Visibility: 10 km (+) / 6 SM (+)
Clouds: Base 4,921 ft | Thickness 984 ft
QFE: 1009 hPa | 29.80 inHg
QNH: 1014 hPa | 29.96 inHg
```

How to read it:

- **Code, Position, Altitude, Tower Frequencies, Runways, Heading** come from
  the DCS terrain's own airfield table. The tower frequencies are DCS's stock
  ATC frequencies for that field.
- **Runways (# = active)**: every runway end within 90° of the wind is marked
  `#`, so you land and take off into it. On a field with parallel runways more
  than one end is marked.
- **Heading** is the main runway's true heading, both directions.
- **Surface Wind** gives the true direction first and the magnetic one in
  brackets (`°M`). Runway numbers are magnetic, so use the `°M` figure against
  them. On a mission with dynamic weather the wind is sampled live and the line
  ends in `(live)`.
- **Clouds**: a mission using a DCS cloud preset shows `Cloud Cover:` with the
  preset's METAR text instead. `Clouds: n/a` means no layer.
- **Precipitation: yes** is added when it is raining.
- **Visibility** reads `10 km (+) / 6 SM (+)` for anything 10 km or better;
  fog brings it down to metres.
- **QFE / QNH**: QFE is the pressure at the field, QNH is that reduced to sea
  level. Set QNH for altitude, QFE if your jet wants height above the field.

## The carrier ATIS

At a carrier slot you get a shorter report with the recovery details:

```dcsmessage
CARRIER ATIS - CVN-71 THEODORE ROOSEVELT
BRC: 084°M
Recovery: CASE I
Temperature: +18.0°C (+64°F)
Surface Wind: 219° (213°M) @ 12 kts / 6 m/s
Visibility: 10 km (+) / 6 SM (+)
Clouds: base 4,921 ft MSL | 984 ft thick | density 3/10
QNH: 1015 hPa | 29.97 inHg | 761 mmHg
```

**BRC** is the ship's current heading, magnetic. The **recovery case** comes
from the cloud base alone:

| Cloud | Recovery |
| --- | --- |
| No cloud layer | CASE I |
| Base 3,000 ft MSL or higher | CASE I |
| Base 1,000 to 3,000 ft MSL | CASE II |
| Base below 1,000 ft MSL | CASE III |

See [Carrier Operations](./carrier-ops.md) for the rest.

## The full report: Info → Weather and `-weather`

Both give the ATIS for your slot plus two winds-aloft blocks:

```dcsmessage
Winds aloft AGL (°T/kt): 1000ft 268/14, 2000ft 270/19, 6600ft 275/27, 26000ft 280/52
Winds/Temps Aloft (MSL):
   3000ft/914m: 268°/18kt (9m/s) +13°C (+55°F)
   6000ft/1829m: 271°/22kt (11m/s) +7°C (+45°F)
   9000ft/2743m: 274°/26kt (13m/s) +1°C (+34°F)
  12000ft/3658m: 276°/31kt (16m/s) -5°C (+23°F)
  18000ft/5486m: 279°/40kt (21m/s) -17°C (+1°F)
  24000ft/7315m: 281°/48kt (25m/s) -29°C (-20°F)
```

- **Winds aloft AGL** is one line at 1,000, 2,000, 6,600 and 26,000 ft above
  the field: the pattern, the approach, and DCS's 2,000 m and 8,000 m weather
  layers. Directions are true. `calm` means no wind at that height.
- **Winds/Temps Aloft (MSL)** is the classic table at 3,000 to 24,000 ft above
  sea level, with DCS's own temperature at each level. Levels below the field
  are left out.

Where the report comes from depends on where you are:

- **Airfield or carrier slot**: the full ATIS above, plus winds aloft.
- **FARP, FOB, ground unit, spectator** (Info → Weather only): a general brief
  at your position:

```dcsmessage
WEATHER (general)
Elevation: 492 ft / 150 m
Temperature: +15.0°C (+59°F)
Surface Wind: 271° (265°M) @ 9 kts / 5 m/s
Visibility: 10 km (+) / 6 SM (+)
Clouds: base 4,921 ft MSL | 984 ft thick | density 3/10
QFE: 996 hPa | 29.41 inHg | 747 mmHg
QNH: 1014 hPa | 29.94 inHg | 761 mmHg
Winds aloft AGL (°T/kt): 1000ft 268/14, 2000ft 270/19, 6600ft 275/27, 26000ft 280/52
```

- **When the server has weather from DCSServerBot** (it is passed on by the
  dashboard), that is used instead, so it agrees with the dashboard. Info →
  Weather then shows it with the winds-aloft line added; `-weather` prints it
  in chat:

```dcsmessage
SERVER WEATHER
Temp: 18°C / 64°F
Surface wind: 271° at 9 kt
Visibility: 10 km / 6 SM
Clouds: base 4921 ft AGL, 3/10
QNH: 1014 hPa / 29.94 inHg
```

Without server weather, `-weather` from spectators answers `You must be in a
slot to request a weather report`, and from a FARP or FOB slot it gives
nothing: use Info → Weather there.

```f10menu
{"root":"F10 Other","items":[{"label":"Info","items":[
 {"label":"My Status"},
 {"label":"Situation","items":[{"label":"1. Overview"},{"label":"2. Tasking"},{"label":"3. Hotspots"},{"label":"4. Threats & Air"},{"label":"5. Logistics"},{"label":"6. Comms"}]},
 {"label":"Support & Radios"},
 {"label":"Supply Convoys"},
 {"label":"Navaids Directory"},
 {"label":"Time & Server","says":"Server restarts in 01:42:10","note":"Or 'Server restart: not scheduled automatically.' when the server has no restart schedule."},
 {"label":"Weather","says":"ATIS-REPORT FOR KUTAISI\n=======================\nCode: UGKO\nAltitude: 147 ft\n==============================\nRunways (# = active): 07 | 25#\n==============================\nTemperature: 17.62° C\nSurface Wind: 271° (265°M) @ 9 kts\nVisibility: 10 km (+) / 6 SM (+)\nClouds: Base 4,921 ft | Thickness 984 ft\nQFE: 1009 hPa | 29.80 inHg\nQNH: 1014 hPa | 29.96 inHg\nWinds aloft AGL (°T/kt): 1000ft 268/14, 2000ft 270/19, 6600ft 275/27, 26000ft 280/52\nWinds/Temps Aloft (MSL):\n   3000ft/914m: 268°/18kt (9m/s) +13°C (+55°F)","note":"Shortened here. At a FARP or FOB slot you get the general brief instead."},
 {"label":"Help"}
]}]}
```

## The briefing's WX line

The situation briefing (slot entry, `-brief`, Info → Situation) carries a
one-line summary:

```dcsmessage
WX: MVFR -- wind 271° at 9kt, 10km vis, cloud base 2500ft
```

It is a whole-map summary, sampled at the centre of the map, not at your field.
The flight rule is a quick airman's read:

| Rule | When |
| --- | --- |
| IFR | 6/10 cover or more with the cloud base under 1,000 ft |
| MVFR | cloud base under 3,000 ft, or visibility under 5 km |
| VFR | anything better |

For your own field, use the ATIS.

## True and magnetic

What is magnetic: the ATIS surface wind's `°M` figure, the runway numbers and
the active-runway pick, the carrier ATIS BRC, and bearings the engine reads out
to you elsewhere (BRAA, 9-line headings). What is true: the leading surface
wind figure, the runway `Heading` line and all winds aloft.

The magnetic variation is one figure per map. The server can set its own;
otherwise the engine uses its built-in value:

| Map | Variation |
| --- | --- |
| Caucasus | 6.5° E |
| Syria | 5° E |
| Persian Gulf | 2° E |
| Nevada | 12° E |
| Marianas | 1° W |
| Sinai | 4.5° E |
| Kola | 15° E |
| Afghanistan | 3° E |
| Normandy, The Channel | 10° W (WWII-era) |

It is approximate: the real variation drifts a degree or two across a map.

## Real-world weather and time

A server can sync its weather with the real world. DCS cannot change the
weather of a mission that is already running, so the engine rewrites the
mission file **just before each scheduled restart**, and the new weather shows
up when the mission loads again. Nothing changes mid-session.

What it sets, from the current real-world conditions at a point the server
chooses:

- **Temperature and QNH.**
- **Surface wind**, capped at 8 m/s (about 16 kt) so helicopters can still
  work, plus a gust component derived from it.
- **Winds at 2,000 m and 8,000 m**, from the real upper-air forecast, capped
  at 12 and 20 m/s.
- **Cloud**: a DCS cloud preset picked from the real cloud cover, or a rain
  preset when it is raining.
- Optionally, the **surface layer from a real airfield's METAR**, and the
  **date and time of day** set to the real local time, so a night in the
  theatre is a night on the server.

If the weather services cannot be reached, the mission keeps the weather it
had. On a server without live weather, the weather is whatever the mission
author set.

## Time

- **Info → Time & Server** shows `Server restarts in HH:MM:SS`, or
  `Server restart: not scheduled automatically.`
- **`-time`** in chat answers `The server will shutdown in HH:MM:SS`, or
  `The server isn't configured to restart automatically`.
- The **situation briefing** header carries the mission's time of day, for
  example `=== SITUATION -- Blue 14:05 ===`.

Campaign timers run on **DCS's own clock**, not the wall clock: repairs,
production, cooldowns, capture holds and the like stop while the server is
paused and run at half speed in slow motion, so nothing comes due in a rush
when it resumes. Time the server is down between restarts does count. Things
tied to real-world time stay on the wall clock: the restart schedule, life
resets, bans and the round length.

## Spoken ATIS and tower

Some servers run voice ATC over SRS. When they do, every field your side holds
has:

- an **ATIS** loop on its own frequency, repeated every 45 seconds, and
- a **tower** you can talk to.

By default the frequencies are handed out alphabetically by field name, tower
from 340.0 MHz and ATIS from 370.0 MHz in 0.5 MHz steps, AM. A field keeps its
frequencies for the whole campaign, whoever owns it. The server can set
different ones. See [Comms Plan](./comms-plan.md).

The spoken ATIS follows the real running order and adds what only the
campaign knows:

> Kutaisi information alpha. Wind two six fife at 9. Runway in use two fife.
> Altimeter two niner niner six, Q N H 1014 hectopascals. Temperature 18,
> dewpoint 9. Pattern altitude 1500 feet. Advise on initial contact you have
> alpha.

- The **information letter** only moves on when something material changes:
  wind direction by more than 20°, wind speed by more than 3 kt, the runway,
  QNH by 2 hPa, the field opening or closing, or a threat appearing.
- It says **Field is closed** when the field's logistics or health are at
  zero, **Caution, field is under threat** when enemy units are in sight, and
  **Rearming unavailable** / **Refuelling unavailable** when the field is out
  of weapons or fuel.
- A carrier reads its **base recovery course** and the **Case** instead of a
  runway and pattern altitude.

The tower answers radio checks, start-up, taxi, takeoff, inbound, initial,
landing, go-around, altimeter and departing calls. It is advisory: it
sequences traffic and warns you about it, and always clears you in the end.

## See Also

- [Navaids & Approaches](./navaids.md): TACAN and NDB for fields without their own
- [Carrier Operations](./carrier-ops.md): BRC, recovery case, carrier navaids
- [The Auto-Generated Briefing](./briefing.md): the WX line and the slot-entry panel
- [Info Menu](../f10-menu/info.md)
- [Chat Commands](./chat-commands.md): `-weather`, `-time`, `-brief`
