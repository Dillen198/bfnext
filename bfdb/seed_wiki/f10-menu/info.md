# Info Menu

`F10 → Info` is the engine's reference desk. Everything on it is a **read-only
report, free, and available from any slot**: fighter, helo or transport. If you
are ever unsure what is going on, open this menu first.

## The menu

```f10menu
{"root":"F10 Other","items":[
 {"label":"Info","items":[
  {"label":"My Status","says":"=== Viper ===\nSide: Blue\nPoints: 1250\nKill streak: 3\nCareer kills: 87\nSide switches allowed: 1/round"},
  {"label":"Situation","items":[
   {"label":"1. Overview","says":"=== SITUATION 1/6 -- Overview (Blue 14:32) ===\n(headline)\n\nTerritory: 54% of contested ground\nYours: 18 objectives (7 primary)   Enemy: 14 (6 primary)   Neutral: 2\nTreasury: 4200 pts   Pilots online: 9 vs 7"},
   {"label":"2. Tasking","says":"=== SITUATION 2/6 -- Tasking (Blue 14:32) ===\n..."},
   {"label":"3. Hotspots","says":"=== SITUATION 3/6 -- Hotspots (Blue 14:32) ===\nKuweires [Airbase] Red -- HP 15% L 100% S 66%\n  eligible -- move your capture troops into the zone"},
   {"label":"4. Threats & Air","says":"=== SITUATION 4/6 -- Threats & Air (Blue 14:32) ===\nAIR: 3 hostile track(s) (1 coasting), 6 friendly airborne\n\nKNOWN AIR DEFENCE (your intel only):\n  SA-11 near Jirah -- 19nm ring, 80% conf, ±0.8km, recon 240s old"},
   {"label":"5. Logistics","says":"=== SITUATION 5/6 -- Logistics (Blue 14:32) ===\nConvoys running: 2   Logistics stage: ...\n\nHUBS:\n  Hama -- S 88% F 92% HP 100% L 100%, feeding 4\n\nCANNOT SUSTAIN THEMSELVES:\n  none -- every objective can pay for its own repairs"},
   {"label":"6. Comms","says":"=== SITUATION 6/6 -- Comms (Blue 14:32) ===\nPRESET  FREQ       WHO\n   1   251.000 AM  AWACS  [UP]"}
  ],"note":"Six pages, one command each. Full description on The Auto-Generated Briefing page."},
  {"label":"Support & Radios","says":"=== Support & Radios ===\nAWACS [BLUE AWACS 1]  251.000 MHz\nTANKER [BLUE TANKER 1]  276.100 MHz  TACAN 51X TKR\n\nJTACs:\n  JTAC 1234 code 1688 near Abu al-Duhur (045°/22nm) [lasing]"},
  {"label":"Supply Convoys","says":"=== Supply Convoys ===\nHama -> Abu al-Duhur  (fuel) (072°/14nm)  12m en route\nTartus -> Masyaf  (mixed supplies) (198°/31nm)  4m en route"},
  {"label":"Navaids Directory","says":"=== Friendly Navaids ===\nAbu al-Duhur (067°/18nm): TACAN 37X ADH\nHama (182°/35nm): TACAN 41X HMA  NDB 375"},
  {"label":"HQ","items":[
   {"label":"Commander's Intent","says":"HQ -- COMMANDER'S INTENT (...)\n...\nMain effort: Kuweires\nHold: Abu al-Duhur, Hama\nResupply first: Masyaf"},
   {"label":"Operations","says":"HQ -- OPERATIONS\n..."},
   {"label":"Request Support (nearest objective)","items":[
    {"label":"CAS on the nearest enemy objective"},
    {"label":"CAP over the nearest friendly objective"},
    {"label":"SEAD near the nearest enemy objective"},
    {"label":"Recon of the nearest enemy objective"},
    {"label":"Fires on the nearest enemy objective"},
    {"label":"Resupply the nearest friendly objective"},
    {"label":"Troops to the nearest enemy objective"},
    {"label":"Tanker near the nearest friendly objective"},
    {"label":"AWACS near the nearest friendly objective"}
   ]}
  ],"note":"Only on servers that run the Theatre HQ for your side."},
  {"label":"Time & Server","says":"Server restarts in 02:14:37"},
  {"label":"Weather","says":"SERVER WEATHER\nTemp: 24°C / 75°F\nSurface wind: 310° at 8 kt\nVisibility: 10 km / 6 SM\nClouds: base 4500 ft AGL, 3/10\nQNH: 1013 hPa / 29.91 inHg"},
  {"label":"Help","items":[
   {"label":"Getting Started","says":"=== Getting Started ===\nSlot into any aircraft to join the fight for Blue or Red.\n..."},
   {"label":"Chat Commands","says":"=== Chat Commands ===\nType these directly in the DCS chat window (F10 default key, or your bound key).\n..."},
   {"label":"Cargo & Logistics","says":"=== Cargo & Logistics ===\nEvery base tracks four numbers, and they are NOT the same thing:\n..."},
   {"label":"Objectives & Capturing","says":"=== Objectives & Capturing ===\nUnder each base on the F10 map is a row of four hexes: Health, Logistics\n(Logi), Supply and Fuel.\n..."},
   {"label":"Carrier Groups","says":"=== Carrier Groups ===\nCarriers are NOT captured the normal way -- health/defenses alone never\nmake one capturable. It's a two-step process:\n..."},
   {"label":"Combat & JTAC","says":"=== Combat & JTAC ===\nJTAC ground controllers: the JTAC menu gives a 9-line, a status report,\nsmoke on the target, and target shifting (manual or automatic).\n..."}
  ]}
 ]}
]}
```

Without the Theatre HQ the menu has eight entries; with it, nine.

## My Status

Your own record, as the engine has it:

```dcsmessage
=== Viper ===
Side: Blue
Points: 1250
Kill streak: 3
Career kills: 87
Side switches allowed: 1/round
```

- Your name, coalition, points, current kill streak and career kills.
- `Sides are LOCKED this round` when the server locks sides, or `Side switches
  allowed: N/round` when it limits them.
- A `Lives:` block, one line per life type, **only when the server enforces
  lives** (`limited_lives`). See [Points and Lives](../gameplay/points-and-lives.md).

If you are not in a slot the engine knows about, it answers `You are not
registered in this slot.`

`-balance` and `-lives` in chat give the same numbers.

## Situation

The six-page auto-generated briefing for your coalition, one command per page:
`1. Overview`, `2. Tasking`, `3. Hotspots`, `4. Threats & Air`, `5. Logistics`
and `6. Comms`. Each page stays on screen for 60 seconds.

| Page | What's on it |
| --- | --- |
| 1. Overview | Headline, territory share, objective counts, treasury, pilots online, victory and last-stand state, weather, the last hour's events |
| 2. Tasking | Your coalition's ranked task list, with what counts as done and which roles are wanted |
| 3. Hotspots | Every objective in contact or takeable, with its status and repair outlook |
| 4. Threats & Air | Hostile air tracks and the air defence **your side has detected** |
| 5. Logistics | Convoys, hubs and what they feed, and bases that can't pay for their own repairs |
| 6. Comms | The frequency plan, with live stations marked `[UP]`, and your intra-flight channels |

It is a **coalition picture, not ground truth**: an enemy SAM nobody has seen is
not on page 4. [The Auto-Generated Briefing](../gameplay/briefing.md) explains
every page, and [Comms Plan](../gameplay/comms-plan.md) covers page 6.

## Support & Radios

Every friendly AWACS and tanker the side has called in through
[Actions](./actions.md), with frequency and TACAN, and every friendly JTAC with
its id, laser code and the objective it is near:

```dcsmessage
=== Support & Radios ===
AWACS [BLUE AWACS 1]  251.000 MHz
TANKER [BLUE TANKER 1]  276.100 MHz  TACAN 51X TKR

JTACs:
  JTAC 1234 code 1688 near Abu al-Duhur (045°/22nm) [lasing]
```

The JTAC number is the `<id>` the `-jtac` chat commands take. `[lasing]` means
it has a target. When there is nothing up: `No active AWACS, tankers, or
JTACs.` See [Calling AWACS, Tankers & CAP](../playbooks/calling-support.md) and
[JTAC System](./jtac.md).

## Supply Convoys

Friendly road convoys in transit: where from, where to, what they carry (`fuel`,
`weapons` or `mixed supplies`), bearing and range from you to the convoy, and
how many minutes it has been on the road.

```dcsmessage
=== Supply Convoys ===
Hama -> Abu al-Duhur  (fuel) (072°/14nm)  12m en route
```

When none are moving: `No friendly supply convoys are moving right now.` The
enemy has the same page for their convoys, and theirs are worth hunting. See
[Logistics & Supply](../gameplay/logistics.md).

## Navaids Directory

Every navaid on a base your side owns, nearest first: TACAN with its Morse
ident, NDB, and on carriers ICLS, Link 4 and ACLS. They are generated by the
engine and follow ownership, which is why they are not printed on the F10 map
label any more. When the feature is off: `Auto-navaids are disabled on this
server.`; before any are assigned: `No generated navaids yet.` Full
explanation in [Navaids & Approaches](../gameplay/navaids.md).

## HQ

Only on servers that run the [Theatre HQ](../advanced/theatre-hq.md) for your
side:

- **Commander's Intent**: posture, intent, main effort, the bases to hold and
  to resupply first, and where the plan came from. Before the HQ has a plan:
  `HQ is still assessing the situation.`
- **Operations**: what the HQ has under way and the support requests waiting.
  Before its first one: `HQ has run no operations yet.`
- **Request Support (nearest objective)**: nine requests, each aimed at the
  objective nearest you: `CAS on the nearest enemy objective`, `CAP over the
  nearest friendly objective`, `SEAD near the nearest enemy objective`, `Recon
  of the nearest enemy objective`, `Fires on the nearest enemy objective`,
  `Resupply the nearest friendly objective`, `Troops to the nearest enemy
  objective`, `Tanker near the nearest friendly objective` and `AWACS near the
  nearest friendly objective`.

## Time & Server

How long until the next scheduled restart, as `Server restarts in 02:14:37`.
If the server has no automatic restart it says `Server restart: not scheduled
automatically.` and the round runs until it is won (see
[Round Rules](../gameplay/round-rules.md)).

## Weather

The weather where you are. When DCSServerBot reports the server weather it is
used first, as a `SERVER WEATHER` block (temperature, surface wind, visibility,
clouds, QNH) plus winds aloft at your field. Otherwise you get your field's full
ATIS with winds aloft, or a general brief at your position if you are not at a
field. `-weather` in chat does the same. For carrier recovery cases see
[Carrier Operations](../gameplay/carrier-ops.md).

## Help

Six in-cockpit topics, each shown for 60 seconds, for when you cannot alt-tab:
`Getting Started`, `Chat Commands`, `Cargo & Logistics`, `Objectives &
Capturing`, `Carrier Groups` and `Combat & JTAC`. They are deliberately terse;
this wiki is the long version. Where the two disagree, trust the wiki pages
linked below: two lines in `Objectives & Capturing` are out of date (see
[Capturing Objectives](../gameplay/capturing-objectives.md#special-sam-sites)).

## See Also

- [Objectives Menu](./objectives.md): the base-by-base reports and the Capture Advisor
- [The Auto-Generated Briefing](../gameplay/briefing.md)
- [Comms Plan](../gameplay/comms-plan.md): the full frequency allocation
- [Navaids & Approaches](../gameplay/navaids.md)
- [Carrier Operations](../gameplay/carrier-ops.md)
- [Chat Commands](../gameplay/chat-commands.md)
