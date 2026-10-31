# GCI / EWR

Your coalition's radar picture as text, on demand or pushed to you
automatically, plus the settings for the [live GCI voice net](../gameplay/gci.md).
The menu is called **`GCI/EWR`** and every aircraft slot has it.

![EWR radar operator screen tracking enemy and friendly aircraft contacts](/api/wiki/images/b68ad72b-f981-4957-986d-b301edcc4216)

## Where the picture comes from

The picture is not free. Your side sees what its sensors see:

- **Ground radars you deploy**: EWR [deployables](../reference/deployables.md)
  unpacked from crates.
- **AWACS** bought from the [Actions menu](./actions.md).
- **Radar units the server lists as EWR contributors**: typically early-warning
  radars, SAM search radars and ships in your side's garrisons.
- **Aircraft the server lists as airborne radars**, AI or flown by players. Many
  servers include fighters, so a CAP on station widens the picture just by being
  there.

Each sensor has a range; some only look in a cone ahead of them, and terrain
blocks them. Only aircraft **in the air** are tracked. When your radars are
killed, a sector goes quiet on this menu, and that silence is not empty sky. The
reverse is the cheapest SEAD there is: kill an enemy radar and you blind a sector
without touching the batteries in it.

## The menu

```f10menu
{"root":"F10 Other","items":[
 {"label":"GCI/EWR","items":[
  {"label":"Report","says":"Bandits BRAA\n   BRG°M      RNG      ALT      SPD      HDG°M      AGE  ASPECT    SRC   TYPE\n     42     38nm    A22.0    500kts     222      6s HOT       [GA]   Flanker\n     58     51nm    A24.5    400kts     265     12s FLANK R   [A]   Fulcrum\n     97     22nm     A1.2    100kts      10      9s BEAM  L   [G]   Mi-24P\n*   350     84nm    A31.0    400kts     180     71s COLD      [A]   Candid"},
  {"label":"Toggle","says":"ewr reports are disabled","note":"Automatic reports to you on or off. On by default; clicking again says: ewr reports are enabled"},
  {"label":"Friendly Report","says":"Friendlies BRAA\n   BRG°M      RNG      ALT      SPD      HDG°M      AGE  ASPECT    SRC   TYPE\n    270     12nm    A18.0    400kts      90      3s HOT       [GA]   F-16C"},
  {"label":"Units to Imperial","says":"EWR units are now Imperial"},
  {"label":"Units to Metric","says":"EWR units are now Metric"},
  {"label":"Ground Intel","says":"Ground Intel\n3 3×ADS, 047 FOR 32km, CONF HIGH\n4 4×ARMOR, 061 FOR 28km, CONF MEDIUM\n2 2×UNK, 012 FOR 45km, CONF LOW"},
  {"label":"GCI Voice","items":[
   {"label":"Toggle GCI Calls","says":"GCI voice calls are disabled","note":"Off takes you out of the voice controller's picture entirely: it neither calls nor answers you."},
   {"label":"Toggle Auto Callouts","says":"GCI will stay quiet unless you call — key up and ask for a bogey dope or picture","note":"Clicking again: GCI will call you unprompted (threats, new groups, picture)"},
   {"label":"Units: Imperial","says":"GCI calls will use imperial units"},
   {"label":"Units: Metric","says":"GCI calls will use metric units"},
   {"label":"Units: Server Default","says":"GCI calls will use the server default units"},
   {"label":"Reference: BRAA","says":"GCI calls will use BRAA from your aircraft"},
   {"label":"Reference: Bullseye","says":"GCI calls will use bullseye reference"},
   {"label":"Reference: Clock","says":"GCI calls will use clock position"}
  ]}
 ]}
]}
```

## Report (bandits)

`Report` lists the enemy aircraft your side's radars hold, as seen from **your**
aircraft:

```dcsmessage
Bandits BRAA
   BRG°M      RNG      ALT      SPD      HDG°M      AGE  ASPECT    SRC   TYPE
     42     38nm    A22.0    500kts     222      6s HOT       [GA]   Flanker
     58     51nm    A24.5    400kts     265     12s FLANK R   [A]   Fulcrum
     97     22nm     A1.2    100kts      10      9s BEAM  L   [G]   Mi-24P
*   350     84nm    A31.0    400kts     180     71s COLD      [A]   Candid
```

| Column | Meaning |
|---|---|
| `BRG°M` | Magnetic bearing from you to the contact. |
| `RNG` | Range: nautical miles (imperial) or km (metric). |
| `ALT` | Altitude. Imperial is in angels: `A22.0` = 22,000 ft, `A1.2` = 1,200 ft. Metric is metres. |
| `SPD` | Ground speed in knots or km/h, rounded to 100. |
| `HDG°M` | The contact's magnetic heading. |
| `AGE` | Seconds since a radar last updated the track. |
| `ASPECT` | `HOT` nose-on to you, `FLANK L/R` about 45° off, `BEAM L/R` about 90° off (crossing), `COLD` going away. L/R says which side of the contact you are on. |
| `SRC` | Who holds it: `[G]` ground radar, `[A]` airborne radar, `[GA]` both. |
| `TYPE` | NATO reporting name where there is one (`Flanker`, `Fulcrum`, `Candid`), otherwise the aircraft type. |

- A line starting with `*` is **stale**: no radar has updated it for 60 seconds
  or more. Tracks older than 120 seconds are dropped.
- At most 10 lines: **fighters first**, then other combat aircraft, then
  helicopters, then support aircraft (AWACS, tankers, transports, drones); nearest
  first within each.
- You never see yourself in the list.

`Friendly Report` is the same table for your own side's aircraft, headed
`Friendlies BRAA`.

## Automatic reports

The same `Bandits BRAA` table is pushed to you without asking while `Toggle` is on
(the default). How often depends on the server's EWR mode, which is
**{{cfg:ewr_mode|Original}}** here:

| Mode | Pushes a report | Track positions |
|---|---|---|
| `Original` | Every 60 s, sooner when a fresh contact is inside 20 km (or inside 40 km and 30 s have passed) | Updated on every radar sweep |
| `Delayed` | Every {{cfg:ewr_delay|60}} s | Updated only every {{cfg:ewr_delay|60}} s, so a contact is shown where it was, not where it is |

`Toggle` answers `ewr reports are disabled` or `ewr reports are enabled`. It only
stops the automatic pushes; `Report` still works.

## Units

`Units to Imperial` and `Units to Metric` set the units of your EWR text reports
(not the voice net). Until you pick, the report uses what your airframe's gauges
read: metric for Russian and Soviet types (MiG, Su, Yak, Ka, Mi, Il, Tu, An, L-39,
J-11), imperial for everything else.

## Ground Intel

Not an air report: this is what your coalition **knows** about enemy ground units,
from the shared intel picture. It is only filled on servers that run it (ELINT or
player recon enabled).

```dcsmessage
Ground Intel
3 3×ADS, 047 FOR 32km, CONF HIGH
4 4×ARMOR, 061 FOR 28km, CONF MEDIUM
2 2×UNK, 012 FOR 45km, CONF LOW
```

- Up to five contacts, weighted toward the nearest and most confident.
- Each line is a count, the cluster (`3×ADS`: three air-defence units), the
  bearing (true, not magnetic) and range in km from you, and confidence:
  `HIGH` (80 % and up), `MEDIUM` (40-79 %), `LOW` (below 40 %).
- Kinds: `ADS` air defence, `ARMOR`, `ARTY`, `INF`, `NAVAL`, `UNK`.
- With nothing known it says `No current ground intel`.

Intel decays. A SAM nobody has seen is not on it. Contacts come from two
sources: [recon passes](./recon.md) (player and AI), and every unit your
[JTACs](./jtac.md) can see. The F-14 TARPS photo map on the dashboard is separate
and does not feed this report.

## GCI Voice

Settings for the AI controller that talks to you over SRS; see
[Live GCI](../gameplay/gci.md). `Toggle Auto Callouts` is the one most people
want: it stops unprompted calls but keeps the net, so you can still key up and
ask. `Toggle GCI Calls` takes you off the controller's picture altogether. The
same settings are available in chat as `-gci`.

## In the cockpit panel

The [cockpit overlay](../getting-started/cockpit-overlay.md) has an **EWR** tab
with the same reports and switches:

```cockpitpanel
{"tab":"EWR","rows":[
 {"button":"THREATS","says":"Enemy air picture"},
 {"button":"FRIENDLIES","says":"Friendly air picture"},
 {"button":"GROUND INTEL","says":"Known enemy ground units"},
 {"button":"EWR CALLS ON/OFF","says":"EWR calls: disabled"},
 {"button":"IMPERIAL","says":"EWR units: Imperial"},
 {"button":"METRIC","says":"EWR units: Metric"},
 {"text":"Bandits BRAA"},
 {"text":"   BRG°M      RNG      ALT      SPD      HDG°M      AGE  ASPECT    SRC   TYPE"},
 {"text":"     42     38nm    A22.0    500kts     222      6s HOT       [GA]   Flanker"}
],"status":"EWR units: Imperial"}
```

## Warnings from the radar picture

The EWR system also sends these, without being asked:

```dcsmessage
SPIKE: enemy ground radar has you in cone
```

An enemy radar that looks in a cone (a SAM search radar, an enemy fighter's radar:
`airborne radar`) has you inside it while you are airborne. At most once every 30
seconds.

```dcsmessage
BUDDY SPIKE! Your radar is locked on a FRIENDLY F-16C (Viper 1-2), 312°M 8nm A18.5. Do not shoot.
```

Your own radar is locked on a friendly aircraft. If you have already fired at it:
`CHECK FIRE! Your missile is guided on a FRIENDLY ...`. The radar check can be
switched off by the server; the missile check cannot.

```dcsmessage
THREAT: Enemy CAP scrambled to the north - 2 aircraft detected by EWR!
```

The enemy's radars detected your side's aircraft and it launched fighters in
response (`THREAT: Enemy attack helicopters inbound from the ...` for its
helicopter patrols). See [AI Opposition](../advanced/ai-opposition.md).

## Limits

- **It is a snapshot.** A report is built when you ask or when it is pushed;
  nothing updates on screen.
- **Delay is real.** Check `AGE`. In `Delayed` mode positions can be up to
  {{cfg:ewr_delay|60}} s old even on a fresh line: at 480 knots that is about 8 nm.
  Treat every line as a cue to look, not a firing solution.
- **Coverage is earned.** Lose your radars and AWACS and the report empties. Low
  contacts behind terrain are not seen, and a contact beaming a pulse-Doppler
  radar can drop out.

## See Also

- [Live GCI (AWACS Calls)](../gameplay/gci.md) - the voice side of the same picture
- [Reconnaissance](./recon.md) - feeding the ground intel picture
- [JTAC System](./jtac.md) - JTACs feed it too
- [Recon Intel Map (TARPS)](../advanced/recon-intel-map.md) - the dashboard photo map
- [Actions Menu](./actions.md) - deploying an AWACS
- [Deployable Units](../reference/deployables.md) - ground EWR radars
