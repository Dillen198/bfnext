# The Auto-Generated Briefing

Every round writes its own briefing. Nobody types it: the engine reads the
campaign state and your coalition's sensors, decides what actually matters right
now, and hands you a ranked list of things to do.

You see the same report in several places, built from the same data, so they
can never disagree:

| Where | What you get |
| --- | --- |
| **Slot entry** | A short panel {{cfg:situation_briefing.delay_secs|22}} seconds after you take an aircraft slot, on screen for {{cfg:situation_briefing.display_secs|40}} seconds. Only on servers that switch it on (`situation_briefing`). |
| **`-brief`** in chat | The same short panel, printed in chat, any time. |
| **F10 → Info → Situation** | The full report, six pages: Overview, Tasking, Hotspots, Threats & Air, Logistics, Comms. Each stays up for 60 seconds. |
| **Dashboard → BRIEFING → Situation** | The same report on a map, with the tasking numbered against pins you can click. |

The F10 pages, `-brief` and the dashboard always work; only the push on slot
entry depends on the server.

## The slot-entry panel

```dcsmessage
=== SITUATION -- Blue 14:05 ===
Blue is ahead: 14 objectives (5 primary) against 11 (4 primary), 56% of contested ground. 1 of your objectives is takeable or in contact. Your radar net is reporting nothing: assume you are blind to air. 1 of your objectives cannot pay for their own repairs.
WX: VFR -- wind 271° at 9kt, 10km vis, cloud base 4921ft

TASKING:
[CRITICAL] DEFEND HUB Kutaisi Logistics
    logistics hub feeding 6 of your objectives -- supply 64%, fuel 71%, health 80%, enemy in contact
[HIGH] CAPTURE Senaki 291°/31nm
    eligible -- no capture troops near the zone self-repairing -- next pulse in ~7m
[HIGH] SEAD near Gudauta 318°/64nm
    SA-10 held on recon intel, 80% confidence, 40nm engagement ring, ±1.5km position error. Everything inside that ring is in a launch basket.
(+5 more -- F10 > Info > Situation)

THREAT: SA-10 near Gudauta -- 40nm ring, 80% confidence

AWACS / GCI -- MAGIC -- 251.000 AM

YOUR TASK: DEFEND HUB Kutaisi Logistics -- x1.5 points and bonus XP inside the zone marked on your map (15 km)
WAR EFFORT: 42/100 for us (perk: takeoff hold 25% shorter)
```

Line by line:

- **Header**: your side and the mission's time of day.
- **Headline**: territory, then the single most pressing fact (a capture
  timer running on one of yours, how many of yours are at risk, or how many
  enemy bases are takeable), the air picture, and how many of your bases
  cannot pay for their own repairs.
- **WX**: a whole-map weather summary. See
  [Weather, ATIS & Time](./weather.md).
- **TASKING**: the top {{cfg:situation_briefing.panel_tasks|3}} tasks, with the
  bearing and range from you where it applies. The rest are on the F10 pages.
- **THREAT**: the known air defence your side has the best fix on (highest
  confidence first), not necessarily the nearest.
- **Comms**: the first line of your comms card (normally the GCI/AWACS
  channel), or the first channel that has a live station on it.
- **YOUR TASK**: on slot entry only, the engine hands you the most urgent task
  your airframe can do. See [Your Task](./your-task.md).
- **WAR EFFORT**: on slot entry only. See [War Effort](./war-effort.md).

`-brief` prints the same panel in chat, each line indented by a space, without
the YOUR TASK and WAR EFFORT lines. Before you have taken any slot it answers
`you aren't registered yet -- take any slot on the side you want to fly`.
The server can add a line of its own
(a campaign premise, a Discord link) to the slot-entry panel.

If the server configures a GCI note, a separate one-line panel arrives the
moment you spawn, before the ATIS:

```dcsmessage
GCI: Magic on 251.0 AM (SRS)
```

## It is your coalition's picture, not the truth

This is the important part. The briefing is built **for one side**, and it only
contains what that side has earned:

- **Objective ownership, health and logi** are public. The F10 map already
  shows both sides to everyone, so the briefing does too.
- **Threat rings** come only from *your* recon flights, special forces, JTACs
  and ELINT. If nobody has looked at a SAM, it is not on your map. The briefing
  tells you so (`nothing held -- no recon, SF, JTAC or ELINT contact. Assume the picture is unmapped.`)
  rather than pretending the sky is clear.
- **The air picture** is whatever your EWR and AWACS are actually holding. When
  your radar net is down the report says
  `AIR: radar net reporting nothing -- you are blind, not clear.` That is a
  warning, not an all-clear.
- **Your artillery, hub stocks and tasking** are yours alone. Enemy carriers
  and SAM sites you have not found are never named, not even as the nearest
  landmark to a threat.

The dashboard resolves your Discord login to a coalition server-side before it
asks the engine for anything, so there is no URL you can type to read the other
side's briefing.

## The six pages

```f10menu
{"root":"F10 Other","items":[{"label":"Info","items":[
 {"label":"My Status"},
 {"label":"Situation","items":[
  {"label":"1. Overview","says":"=== SITUATION 1/6 -- Overview (Blue 14:05) ===\nBlue is ahead: 14 objectives (5 primary) against 11 (4 primary), 56% of contested ground. 1 of your objectives is takeable or in contact. Your radar net is reporting nothing: assume you are blind to air. 1 of your objectives cannot pay for their own repairs.\n\nTerritory: 56% of contested ground\nYours: 14 objectives (5 primary)   Enemy: 11 (4 primary)   Neutral: 2\nTreasury: 18450 pts   Pilots online: 9 vs 7\n\nWeather: VFR -- wind 271° at 9kt, 10km vis, cloud base 4921ft\nQNH 29.94 inHg / 1014 hPa, temp 18C\n\nLast hour:\n  Zugdidi went NEUTRAL -- garrison wiped"},
  {"label":"2. Tasking","says":"=== SITUATION 2/6 -- Tasking (Blue 14:05) ===\n[CRITICAL] DEFEND HUB Kutaisi Logistics\n    logistics hub feeding 6 of your objectives -- supply 64%, fuel 71%, health 80%, enemy in contact\n    Done when: hub secure and stocked -- everything downstream of it stops healing without it\n    Wants: CAS, Heavy lift\n[HIGH] CAPTURE Senaki 291°/31nm\n    eligible -- no capture troops near the zone self-repairing -- next pulse in ~7m\n    Done when: land capture troops inside the zone and hold them there\n    Wants: Troop lift, CAS escort"},
  {"label":"3. Hotspots","says":"=== SITUATION 3/6 -- Hotspots (Blue 14:05) ===\nKutaisi Logistics [Logistics Hub] Blue -- HP 80% L 100% S 64%\n  objective health must be <=20% (now 80%)\n  FROZEN -- enemy aircraft within threat range of the base\nSenaki [Airbase] Red -- HP 12% L 20% S 30%\n  eligible -- no capture troops near the zone\n  self-repairing -- next pulse in ~7m"},
  {"label":"4. Threats & Air","says":"=== SITUATION 4/6 -- Threats & Air (Blue 14:05) ===\nAIR: radar net reporting nothing -- you are blind, not clear.\n\nKNOWN AIR DEFENCE (your intel only):\n  SA-10 near Gudauta -- 40nm ring, 80% conf, ±1.5km, recon 340s old"},
  {"label":"5. Logistics","says":"=== SITUATION 5/6 -- Logistics (Blue 14:05) ===\nConvoys running: 2   Logistics stage: Complete { last_tick: 2026-10-30T14:04:51.250Z }\n\nHUBS:\n  Kutaisi Logistics -- S 64% F 71% HP 80% L 100%, feeding 6 [THREAT]\n\nCANNOT SUSTAIN THEMSELVES:\n  FARP Kilo -- repair stalled -- supply 3% under the 5% each pulse costs"},
  {"label":"6. Comms","note":"The comms card with live stations marked [UP]. See Comms Plan."}
 ]},
 {"label":"Support & Radios"},
 {"label":"Supply Convoys"},
 {"label":"Navaids Directory"},
 {"label":"Time & Server"},
 {"label":"Weather"},
 {"label":"Help"}
]}]}
```

1. **Overview**: the headline, territory and objective counts, the treasury,
   pilots online on each side, the victory condition if the round has one,
   the weather with QNH, and up to six things that changed in the last hour.
2. **Tasking**: every task, each with `Done when:` and `Wants:` lines.
3. **Hotspots**: your objectives under threat or takeable, and enemy
   objectives you could take, with health, logi and supply.
4. **Threats & Air**: the air picture and the known air defence, each with its
   source and age.
5. **Logistics**: convoys running, your hubs and what they feed, and the bases
   that cannot pay for their own repairs.
6. **Comms**: the frequency card. See [Comms Plan](./comms-plan.md).

## Reading the tasking

Each task is one sortie, ranked worst-first and then nearest-first, so the top
of the list is usually something you can reach from where you are sitting. The
engine keeps at most {{cfg:situation_briefing.max_tasks|12}}.

- **CRITICAL**: you lose something this sortie if nobody flies it. Enemy
  troops already on one of your bases, one of your bases wide open, a hub in
  contact, an enemy aircraft within 25 nm of your ground.
- **HIGH**: this decides the next hour. A takeable enemy base, a SAM ring of
  40 km or more blocking the push, a raid within 60 nm, a hub running dry.
- **ROUTINE**: worth doing, not on fire.

| Task title | Generated when |
| --- | --- |
| `DEFEND <base>` | Enemy troops are in the zone, or it is wide open to them |
| `CAS <base>` | One of yours has enemy ground units in sight, or is damaged |
| `CAPTURE <base>` | An enemy base is capturable now |
| `CAPTURE <base> (neutral)` | A neutral base nobody holds |
| `STRIKE <base>` | An enemy base already down to 40% health or less |
| `SEAD near <place>` | Known air defence your intel holds |
| `RESUPPLY <base>` | One of yours cannot pay for its own repairs |
| `DEFEND HUB <hub>` / `REFILL HUB <hub>` | A hub in contact, or under 30% supply |
| `INTERCEPT inbound near <place>` | The nearest hostile air track is within 60 nm of your ground |
| `RECON <base>` | Your side holds no air-defence intel at all: the enemy primary objective nearest your ground |

Every task carries **why it is on the list** (the actual state that generated
it), **what "done" looks like**, the **roles** it wants, and a position. On the
dashboard map the numbered pin matches the numbered row.

The wording for anything capture-related comes straight from the engine's own
capture rules, the same code that decides whether a base actually flips. If
the briefing says a base needs health under 20% and no infantry, that is the
literal gate, not a paraphrase.

## Hotspots

Everywhere the campaign is currently moving: your objectives under threat or
already capturable, and enemy objectives you could take. Each one shows health,
logi and supply, what is blocking a capture, and whether it will heal back out
of reach before you get there:

| Outlook | Meaning |
| --- | --- |
| `repairs FROZEN while enemy units stay in sight` | It will not heal until the zone is clear |
| `repairs FROZEN while the capture timer runs` | A capture is under way |
| `self-repairing -- next pulse in ~7m` | It heals again in that many minutes |
| `repair STARVED -- supply 3% under the 5% a pulse costs` | Not enough supply to pay for a repair |
| `logistics destroyed (logi 0%) -- cannot self-repair` | Needs a logistics repair crate first |
| `garrison wiped -- will fall to NEUTRAL, then needs troops` | Health 0, about to go neutral |
| `NEUTRAL -- never self-repairs, must be retaken with troops` | Nobody owns it |

On one of your own bases with enemies in sight, the outlook tells you how many
and roughly how close instead.

## Logistics

Hubs with how much they have left and how many objectives they are feeding, plus
the list of your objectives that **cannot sustain themselves**: either supply
below the {{cfg:repair_supply_cost|5}}% a repair pulse costs, or fuel low enough
that ground starts there will run the tanks dry. A base below the repair cost
will sit at whatever health it is on forever. That list is usually the least
glamorous and most decisive thing on the briefing.

## Comms

See [Comms Plan](./comms-plan.md) for the full frequency card. The briefing
shows the plan with the *live* stations matched onto it: a channel marked
**[UP]** has something actually on it right now, with its TACAN or laser code
underneath.

## Kneeboard PDF

The dashboard's **KNEEBOARD PDF** button (on the BRIEFING page) exports the
situation, the tasking, the comms card and the hotspots, followed by the static
reference pages from the Kneeboard tab: navaids, radios, artillery, deployables
and HARM codes. Print it before you fly.

## See Also

- [Comms Plan](./comms-plan.md)
- [Weather, ATIS & Time](./weather.md)
- [Your Task](./your-task.md)
- [Info Menu](../f10-menu/info.md)
- [Tasking Board](./tasking-board.md)
