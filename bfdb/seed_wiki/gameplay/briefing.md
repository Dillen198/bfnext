# The Auto-Generated Briefing

Every round writes its own briefing. Nobody types it — the engine reads the
campaign state and your coalition's sensors, decides what actually matters right
now, and hands you a ranked list of things to do.

You see the same report in three places, so they can never disagree:

| Where | What you get |
| --- | --- |
| **Slot entry** | A short panel about 20 seconds after you take a slot: the headline, weather, your top 3 tasks, the nearest known threat, and the GCI frequency. |
| **F10 → Info → Situation** | The full report, six pages: Overview, Tasking, Hotspots, Threats & Air, Logistics, Comms. |
| **Dashboard → Briefing → Situation** | The same report on a map, with the tasking numbered against pins you can click. |

Type `-brief` in chat for the short version any time.

## It is your coalition's picture, not the truth

This is the important part. The briefing is built **for one side**, and it only
contains what that side has earned:

- **Objective ownership, health and logi** are public — the F10 map already
  shows both sides to everyone, so the briefing does too.
- **Threat rings** come only from *your* recon flights, special forces, JTACs
  and ELINT. If nobody has looked at a SAM, it is not on your map. The briefing
  will tell you so rather than pretending the sky is clear.
- **The air picture** is whatever your EWR and AWACS are actually holding. When
  your radar net is down the report says **"blind, not clear"** — that is a
  warning, not an all-clear.
- **Your artillery, hub stocks and tasking** are yours alone.

The dashboard resolves your Discord login to a coalition server-side before it
asks the engine for anything, so there is no URL you can type to read the other
side's briefing.

## Reading the tasking

Each task is one sortie, ranked worst-first and then nearest-first, so the top
of the list is usually something you can reach from where you are sitting.

- **CRITICAL** — you lose something this sortie if nobody flies it. A running
  capture timer on one of your bases, a hub in contact.
- **HIGH** — this decides the next hour. A takeable enemy base, a SAM blocking
  the push, a raid inbound.
- **ROUTINE** — worth doing, not on fire.

Every task carries **why it is on the list** (the actual state that generated
it), **what "done" looks like**, the **roles** it wants (SEAD, heavy lift, CAS),
and a position. On the dashboard map the numbered pin matches the numbered row.

The wording for anything capture-related comes straight from the engine's own
capture rules — the same code that decides whether a base actually flips. If the
briefing says a base needs health under 20% and no infantry, that is the literal
gate, not a paraphrase.

## Hotspots

Everywhere the campaign is currently moving: your objectives under threat or
already capturable, and enemy objectives you could take. Each one shows health,
logi and supply, what is blocking a capture, and whether it will heal back out
of reach before you get there (`repairs FROZEN while enemy units stay in sight`,
`self-repairing — next pulse in ~7m`, `STARVED — supply under the repair cost`).

## Logistics

Hubs with how much they have left and how many objectives they are feeding, plus
the list of your objectives that **cannot pay for their own repairs**. A base
below the repair supply cost will sit at whatever health it is on forever. That
list is usually the least glamorous and most decisive thing on the briefing.

## Comms

See [Comms Plan](gameplay/comms-plan) for the full frequency card. The briefing
shows the plan with the *live* stations matched onto it — a channel marked
**UP** has something actually on it right now, with its TACAN or laser code.

## Kneeboard PDF

The dashboard's **Kneeboard PDF** button exports the situation, the tasking, the
comms card and the hotspots, followed by the static reference pages (navaids,
radios, artillery, deployables, HARM codes). Print it before you fly.
