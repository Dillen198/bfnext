# Frequently Asked Questions

Common questions and short answers, each with a link to the page that covers
it in full. Numbers on this page are read from the server selected at the top.

## Getting Started

### Q: How do I join the server?
**A**: Find the server in the DCS multiplayer browser (see your Discord for the exact name and IP), pick a **BLUFOR** or **REDFOR** aircraft slot, and fly. Taking your first slot registers you to that coalition automatically; there is nothing to type. See [Joining a Team](../getting-started/joining-team.md) and [Your First Sortie](../playbooks/first-sortie.md).

### Q: Can I change teams?
**A**: Only if the server allows side switches: **{{cfg:side_switches|unlimited}}** per round here. Leave your slot, then type `-switch blue` or `-switch red` in chat. `F10 → Info → My Status` says whether sides are locked and how many switches you have left. See [Joining a Team](../getting-started/joining-team.md#side-switching).

### Q: Why can't I occupy any slots?
**A**: Usually you're registered to the other coalition. The slot tells you why:

```dcsmessage
You are on Blue in this campaign and sides are locked on this server -- pick a Blue slot
```

Other causes:
- **The base has no airframes of that type left.** Warehouses are finite; see [Materiel & the War Economy](../gameplay/war-economy.md).
- **You're out of lives for that role** (lives enforced here: **{{cfg:limited_lives|yes}}**): `you have no Standard lives remaining. standard 0/3 resetting in 03:10:44`.
- **The base is capturable or still consolidating.** You can't slot in at a base of yours that is itself capturable (`Objective is capturable`), or one still being consolidated after a capture.
- **Not enough points** for that airframe; the [lifeline](../gameplay/points-and-lives.md#lifeline) may still let you fly a cheap one.

### Q: How do I link my Discord account?
**A**: Type `/linkme` in Discord to get a token, then `-linkme <token>` in DCS chat, once. That unlocks your side's dashboard pages, MY PROFILE and `/fe_card`. See [Reading the Live Ops Dashboard](../playbooks/reading-live-ops.md#signing-in-and-your-side).

## Gameplay

### Q: How do I capture an objective?
**A**:
1. Grind the objective down to **20% health or less** and kill **every** infantry defender.
2. Bring capture-capable troops of your own (by helicopter, C-130, ground vehicle or an AI helo mission).
3. Unload them inside the capture zone and hold through the timer ({{cfg:campaign_events.capture_time_secs|180}} s, divided by the squads in the zone, never under 30 s).
4. Keep them alive and in the zone through consolidation ({{cfg:capture_consolidation_secs|300}} s of progress). Lose them all and the base drops to Neutral.

See [Capturing Objectives](../gameplay/capturing-objectives.md).

### Q: Why won't this base flip? I've bombed it flat.
**A**: Ask the engine. `F10 → Objectives → Capture Advisor: Nearest` prints the exact blockers: health still above 20%, infantry defenders alive, a post-capture cooldown running ({{cfg:campaign_events.capture_cooldown_secs|120}} s), capture troops of another side in the zone, or your troops sitting outside the zone edge. It also flags **troops that cannot capture because of their troop type**, the most common wasted sortie. On the dashboard, click the base for the same report. See [Objectives Menu](../f10-menu/objectives.md) and [Capturing Objectives: Why won't it capture?](../gameplay/capturing-objectives.md#why-wont-it-capture).

Logistics at 0 is **not** the capture trigger. It stops the base repairing and supplying, but you take it by killing its garrison.

### Q: How do I earn points?
**A**: The main ones on this server:

| What | Points |
| --- | --- |
| Air kill | {{cfg:points.air_kill|25}} |
| Ground kill | {{cfg:points.ground_kill|2}} × what it was |
| Long-range SAM radar, on top | {{cfg:points.lr_sam_bonus|5}} |
| Capture, split between the capturing pilots | {{cfg:points.capture|15}} |
| Logistics repair delivered | {{cfg:points.logistics_repair|25}} and up |
| Supply transfer delivered | {{cfg:points.logistics_transfer|15}} and up |
| Rescuing someone else's downed pilot | {{cfg:csar.rescue_reward|0}} |

A new pilot starts with {{cfg:points.new_player_join|25}}. See [Points and Lives](../gameplay/points-and-lives.md#earning-points).

### Q: What happens when I run out of lives?
**A**: Lives enforced on this server: **{{cfg:limited_lives|yes}}**. Where they are off, nothing is taken and you can't run out. Where they are on, a life is taken on takeoff from a friendly objective and given back when you land at one. Running dry in one role still leaves you the other four, a role's pool refills completely once its refill period has passed since the first life you took from it (`-lives` shows the clock), and a [CSAR](../f10-menu/csar.md) rescue gives the life back. See [Points and Lives: Lives](../gameplay/points-and-lives.md#lives).

### Q: We keep bombing a base and it keeps repairing itself.
**A**: It repairs one group every {{cfg:repair_time|1800}} seconds at full logistics, but only while it is **left alone**: every time an enemy is in sight of it the countdown starts over. And each repair costs it supply; a base that can't pay doesn't repair. The `REPAIR:` line on the Capture Advisor card says whether it will heal and when the next pulse lands. Keep pressure on it, or cut its supply first. See [Logistics & Supply: Automatic repair](../gameplay/logistics.md#automatic-repair).

### Q: How do I tell my team what needs doing without being on voice?
**A**: Post it on the tasking board: `F10 → Actions>> → Add Task`. It draws on the F10 map for your whole coalition, gets ranked into the briefing, and is called out on the GCI net. Usually free; CAPTURE and SUPPLY tasks close themselves when the job is done. Each side has {{cfg:actions.Blue.add-task.kind.AddTask.max_per_side|12}} slots. See [The Tasking Board](../gameplay/tasking-board.md).

### Q: Nobody wants to fly the crate/troop run. Is there another way?
**A**: `F10 → Actions>> → Actions → AI Helo Missions`. The engine flies a real AI helo that starts up, transits, lands and delivers: troops into an objective, or supply into a base. You can order it from a fast jet. It can be shot down, so watch the route. See [AI Helo Missions](../advanced/helo-missions.md).

### Q: How do I find a FARP / carrier in the dark?
**A**: `F10 → Info → Navaids Directory`. The engine generates a TACAN and NDB (and the full carrier suite) for every objective DCS doesn't give one to, per round. Write the channel down before you take off. See [Navaids & Approaches](../gameplay/navaids.md).

### Q: The menu list is missing entries I know exist.
**A**: Look for **`More >>`** at the bottom. DCS silently drops anything past the tenth entry in a menu, so long lists continue inside a `More >>` page. See [F10 Menu Overview: Troubleshooting](../f10-menu/overview.md#troubleshooting).

### Q: I captured an enemy carrier. Why can't I fly its jets?
**A**: A captured carrier comes across at 50% health (`You have captured CVN Group with its aircraft! Carrier at 50% health`). Aircraft types your side doesn't normally fly stay grounded until it is repaired: deliver carrier repair crates, use the Repair Carrier action where the server has it, or leave it linked to a friendly, stocked naval base to auto-repair. Your own coalition's aircraft work right away. See [Carrier Operations](../gameplay/carrier-ops.md).

### Q: Where do the AI units come from? Do they just appear?
**A**: No. Every AI flight starts on the ground at a friendly airfield and
takes off, and every AI ground unit drives out from a friendly base: bought
actions, the Theatre HQ's operations, CAP scrambles, ambush parties, CSAR search
parties, reinforcements and ground formations alike. Destroy the base, or the
road, and you stop what comes out of it. See [AI Opposition](../advanced/ai-opposition.md).

### Q: My AWACS / tanker / bomber was refused. Why?
**A**: No friendly airfield in range had room to launch it:

```dcsmessage
could not start Tanker, calling tanker

Caused by:
    no friendly airfield more than 10 km from the target has room to launch this flight
```

Heavy aircraft need a large open stand, so a field with only fighter spots is
passed over. You were not charged; try again when a field is free, or capture
one closer to the front. See [Actions Menu](../f10-menu/actions.md).

### Q: Why does my ground formation keep falling back?
**A**: Either its morale broke (heavy losses, or cut off from supply), or it ran
low on fuel and ammunition and pulled back to resupply. Keep the road to a
friendly base with supply open behind it. See
[Ground War](../advanced/ground-war.md).

### Q: How do I become a commander?
**A**: Earn it. Command unlocks at rank tier {{cfg:command.commander_rank|4}}
(Major, a campaign score of 50 at the default), the same score the leaderboard
ranks you by; captures, logistics and kills all count. An admin can also make
you one. Commanders give the ground war's orders, can take over the Theatre
HQ, and get the server's Commander role on Discord. The dashboard's COMMAND
page tells you how far off you are. See [Command](../advanced/command.md#becoming-a-commander).

### Q: Who decides what the AI does?
**A**: On servers running it, each side's [Theatre HQ](../advanced/theatre-hq.md):
posture, main effort, air packages, fires, logistics. Ask it for support from
`F10 → Info → HQ → Request Support (nearest objective)` or `-request`; `-hq`
shows its intent.

## F10 Menus

### Q: I don't see the Actions menu!
**A**: Check:
- You are in an aircraft slot (not spectator or Combined Arms), and it has finished spawning.
- The server's rules let you use actions. If not, you get `you are not permitted to use actions on this server`.
- You are looking for **`Actions>>`**: it is a command that builds the **`Actions`** submenu. Select it, then open F10 again.

See [Actions Menu](../f10-menu/actions.md).

### Q: My map marks don't show up in the Actions menu
**A**: Select `Actions>>` again so the lists are rebuilt. If a mark is still missing, you placed it before you spawned, its name is over 24 characters, or another of your marks has the same name. Only **your own** marks are listed. See [F10 Menu Overview](../f10-menu/overview.md#troubleshooting).

### Q: How do I use JTAC?
**A**:
1. `F10 → JTAC>>`, then open `JTAC`, pick the area and the JTAC (it is named with its laser code, e.g. `Reaper 1234 [1688]`).
2. **Status** shows what it is lasing; **9-Line** gives the full brief.
3. Set your weapon's laser code to the JTAC's code (or change the JTAC's with **Code**).
4. Attack the lased target.

See [JTAC System](../f10-menu/jtac.md).

## Logistics & Supply

### Q: The numbers vanished from the map markers. Where did they go?
**A**: They are the four coloured hexagons beside each base: health, logi, supply, fuel, always in that order. Green is healthy, amber is thin, red is critical. Full key: [Reading the F10 Map](../gameplay/reading-the-map.md).

### Q: My map markers disappeared when I zoomed out. Is it broken?
**A**: No. The status shapes are drawn on the ground, so they shrink with the map. That is what stops the theatre view turning into a wall of markers. Zoom back in and they return.

### Q: What does a red logi hex mean?
**A**: The second hexagon in the row is logistics. Red means the base's logistics are nearly or completely gone: it repairs slowly or not at all, and at 0 it can't issue crates or troops. It does **not** by itself make the objective capturable; that needs health at 20% or less and no infantry left. See [Buildings at an Objective](../gameplay/objective-buildings.md).

### Q: How do I repair logistics?
**A**: Fly in a **{{cfg:repair_crate.Blue.name|Logistics Repair}}** crate from a *different* friendly base (`Cargo → Crates → Base Supply`) and unpack it inside the damaged base's zone. Each kit raises logistics a step and pays {{cfg:points.logistics_repair|25}} points or more. Automatic repair only rebuilds the garrison, not destroyed logistics buildings. See [Logistics & Supply: Logistics repair kits](../gameplay/logistics.md#logistics-repair-kits).

### Q: Why is my objective low on supply?
**A**: Its supply comes down the chain from a logistics hub by convoy, airlift or ship. It runs low when:
- the road to it is cut (supply routing follows the front line),
- its hub is short, lost or under attack,
- its convoys are being shot up,
- it is spending supply on repairs and on the aircraft, weapons and fuel pilots take from it.

See [Logistics & Supply](../gameplay/logistics.md).

### Q: How often does supply update?
**A**: Every **{{cfg:warehouse.tick|10}} minutes**, with a fresh outside delivery every {{cfg:warehouse.ticks_per_delivery|12}} ticks. Admins can force one with `-admin tick`.

### Q: Why is a base of mine getting nothing at all?
**A**: Its road is probably cut. Supply routing follows the front line: a road that runs through an enemy base is cut. A cut-off base can only be reached by air, by player crates, or by an AI helo run. See [Materiel & the War Economy](../gameplay/war-economy.md).

## Technical Issues

### Q: I can't load cargo/troops!
**A**: Check:
- You are inside the zone of a **friendly objective** whose logistics are above 0% and which is **not capturable**, or on a friendly carrier deck.
- That base has stock to issue.
- Your aircraft has a Cargo or Troops menu at all (it comes with the airframe).
- For crates already on the ground: you are landed and within {{cfg:crate_load_distance|50}} m of them.
- You don't already have {{cfg:max_crates|4}} crates of your own out.

The server answers every refusal with the reason. See [Cargo Operations](../f10-menu/cargo.md) and [Troop Transport](../f10-menu/troops.md).

### Q: F10 menu not responding
**A**: Commands answer in your message box; entries ending in `>>` only rebuild a menu and close F10 silently. Wait a moment and open F10 again. If a real command says nothing at all, leave and re-enter the slot, and tell an admin if it keeps happening.

### Q: My deployed unit disappeared!
**A**:
- It may have been destroyed; the KILL FEED on the dashboard shows it.
- Your side may have hit the unit's limit, and a unit set to replace the oldest removed yours.
- Check the F10 map for its marker, and tell an admin if it seems like a bug.

## Points & Economy

### Q: How much do actions cost?
**A**: Read it off the menu: every action's cost is in its own label
(`AWACS(100 pts)`), generated from the same config the engine charges you
from. This server's actions:

{{list:actions.Blue|_The action list is unavailable for this server._}}

{{list:actions.Red|_The action list is unavailable for this server._}}

Deployables and troops cost points too, charged when a deployable is unpacked
or a squad is loaded, and they also need crates and the base's materiel. See
[Deployable Units](./deployables.md) and
[Materiel & the War Economy](../gameplay/war-economy.md).

### Q: Can I get refunds?
**A**: `-delete <group id>` removes a group you deployed and gives back **half its cost**. A unit the treasury paid for refunds nothing, and action groups (AWACS, fighters and so on) and objective groups can't be deleted this way. See [Chat Commands](../gameplay/chat-commands.md).

### Q: Can I transfer points to teammates?
**A**: `-transfer <amount> <player>` gives points to a pilot on your side; `-transfer <amount> objective:<base>` pays them into a base's fund. A late joiner's head start can be spent but not given away. See [Points and Lives: Point transfers](../gameplay/points-and-lives.md#point-transfers).

### Q: What's a good starting strategy for earning points?
**A**: Take [your task](../gameplay/your-task.md): kills inside its zone pay 1.5×. Logistics pays like combat: a repair kit or supply transfer is worth about an air kill. Air and ground kills, and flying troops to a capture, do the rest.

## Troubleshooting

### Q: I'm stuck in spectator and can't slot!
**A**:
- Are you registered to the *other* coalition? (you can only slot your own side)
- Are you out of lives for that role? (check `-lives`)
- Is the base capturable or still consolidating?
- Is the slot occupied? Try a different one.

### Q: Commands don't work!
**A**:
- Check spelling and include the dash: `-lives`, not `lives`.
- `-help` lists the commands you can use.
- Some commands are admin-only.

See [Chat Commands](../gameplay/chat-commands.md).

### Q: Game crashed or disconnected. Did I lose my life?
**A**: Where lives are enforced, a life is taken on takeoff from a friendly objective and given back when you land at one. Crash on the ground before takeoff and nothing was taken. Drop out in the air and you left the slot away from a friendly objective, so that life is gone. Contact an admin if it was unfair.

## Still Have Questions?

Ask in:
- **Discord**: [https://discord.gg/XyGkb3WAXS](https://discord.gg/XyGkb3WAXS)
