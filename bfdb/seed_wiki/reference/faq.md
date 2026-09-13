# Frequently Asked Questions

Common questions and answers about Fowl Engine.

## Getting Started

### Q: How do I join the server?
**A**: Find the server in the DCS multiplayer browser (see your Discord for the exact name and IP), pick a **BLUFOR** or **REDFOR** aircraft slot, and fly. Taking your first slot registers you to that coalition automatically — you don't have to type anything. You *can* still type `blue` or `red` in chat from spectators if you prefer. See [Your First Sortie](../playbooks/first-sortie.md).

### Q: Can I change teams?
**A**: Your coalition is permanent unless the server grants side switches. If it does, go to spectators and use `-switch blue` / `-switch red`. Check your remaining switches with `F10 → Info → My Status`, which also says whether sides are locked this round.

### Q: Why can't I occupy any slots?
**A**: Usually you're registered to the other coalition — you can only take slots for your side. Registration happens on your first slot pick; if you're still in spectators you can also type `blue` or `red` in chat.

Two other causes: the **base has no airframes of that type left** (warehouses are finite — see [Materiel & the War Economy](../gameplay/war-economy.md)), or the server is enforcing **lives** and you're out for that role. Lives are switched off on the live mission, so it's almost always one of the first two.

## Gameplay

### Q: How do I capture an objective?
**A**:
1. Grind the objective down to **≤ 20% health** and kill **every** infantry defender
2. Transport capture-capable infantry of your own to the objective
3. Unload troops inside the capture zone
4. Hold through the timer, then hold the consolidation window while the garrison moves in

See [Capturing Objectives](../gameplay/capturing-objectives.md) for details.

### Q: Why can't I capture this objective?
**A**: Check these requirements:
- Health must be **≤ 20%** (logistics at 0 is *not* the capture trigger — it just means the base stopped working)
- **No** enemy infantry left alive — one surviving squad blocks it
- You must have capture-capable infantry IN the zone (unloaded, on ground)
- No enemy troops contesting the zone

### Q: How do I earn points?
**A**:
- Air kills: **25 points** (+5 bonus for LR SAMs)
- Ground kills: **2 points**
- Objective captures: **50 points** (split among participants)
- Starting bonus: **190 points**

See [Points System](../gameplay/points-and-lives.md) for full details.

### Q: What happens when I run out of lives?
**A**: Lives are **not enforced on the live mission** — no life is taken on takeoff or death, so you can't run out. Where a server does enforce them, running dry in one role still leaves you the other four (they're tracked separately), each pool refills on its own rolling timer, and a [CSAR](../f10-menu/csar.md) pickup gives a life straight back. See [Points and Lives](../gameplay/points-and-lives.md).

### Q: Why won't this base flip? I've bombed it flat.
**A**: Ask the engine. `F10 → Objectives → Capture Advisor: Nearest` prints the exact blockers — health still above 20%, infantry defenders alive, a post-capture cooldown running, or your troops sitting outside the zone edge. It also flags **capture troops that cannot capture because of their troop type**, which is the single most common wasted sortie. See [Objectives Menu](../f10-menu/objectives.md).

### Q: We keep bombing a base and it keeps repairing itself.
**A**: It has working logistics and materiel on hand. The `REPAIR:` line on the Capture Advisor card tells you whether it's repairing and when the next pulse lands. Either out-pace it, or cut its supply first — see [Logistics & Supply](../gameplay/logistics.md).

### Q: How do I tell my team what needs doing without being on voice?
**A**: Post it on the tasking board: `F10 → Actions>> → Add Task`. It draws on the F10 map for your whole coalition, gets ranked into the briefing, and is called out on the GCI net. Free, and CAPTURE / SUPPLY tasks close themselves when the job is done. See [The Tasking Board](../gameplay/tasking-board.md).

### Q: Nobody wants to fly the crate/troop run. Is there another way?
**A**: `F10 → Actions>> → AI Helo Missions`. The engine flies a real AI helo that cold-starts, transits, lands and delivers — troops into an objective, or supply into a base. You can order it from a fast jet. It can be shot down, so watch the route. See [AI Helo Missions](../advanced/helo-missions.md).

### Q: How do I find a FARP / carrier in the dark?
**A**: `F10 → Info → Navaids Directory`. The engine generates a TACAN/NDB (and full carrier suite) for every objective DCS doesn't give one to, per round. Write the channel down before you take off. See [Navaids & Approaches](../gameplay/navaids.md).

### Q: The menu list is missing entries I know exist.
**A**: Look for **`More >>`** at the bottom. DCS silently drops anything past the tenth entry in a menu, so long lists continue inside a `More >>` page. Long base lists are chunked into alphabetical submenus instead.

### Q: I captured an enemy carrier — why can't I fly its jets?
**A**: A captured carrier comes across at 50% health. Aircraft types your coalition doesn't normally produce ("foreign" airframes it had aboard) stay grounded until the carrier's repairs finish — deliver repair crates to it, use the naval base's Repair/Respawn Carrier actions where the server configures them, or leave it linked to a friendly, stocked naval base and let it auto-repair. See [Carrier Operations](../gameplay/carrier-ops.md). Your own coalition's aircraft work right away.

## F10 Menus

### Q: I don't see the Actions menu!
**A**: Check:
- Are you slotted in an aircraft?
- Does your aircraft have permission?
- Is Actions enabled on this server?
- Try re-slotting

### Q: My map markers don't show up in Actions menu
**A**:
- Marker names must be ≤24 characters
- Delete duplicate marker names
- Only one marker per name
- Must be YOUR markers (not others')

### Q: How do I use JTAC?
**A**:
1. F10 → JTAC → [Select JTAC]
2. Check Status to see current target
3. Set your weapon laser code to match JTAC's code
4. Attack the lased target

See [JTAC System](../f10-menu/jtac.md) for full guide.

## Logistics & Supply

### Q: What does "Logi: 0" mean?
**A**: The objective's infrastructure is completely destroyed — no repair, rearm or resupply. It does **not** by itself make the objective capturable; that needs health ≤ 20% and no infantry left.

### Q: How do I repair logistics?
**A**: An objective with working logistics repairs itself on a pulse — **but each repair costs it supplies and materiel, and if it doesn't have them the repair simply doesn't happen.** The player fix is a **Logistics Repair Kit** crate flown in and unpacked there, which also pays about what an air kill does. Admins can force one with `-admin repair`. See [Logistics & Supply](../gameplay/logistics.md).

### Q: Why is my objective low on supply?
**A**:
- Supply routes may be broken
- Logistics hub may be captured
- High consumption from operations
- Wait for next logistics tick

### Q: How often does supply update?
**A**: Every **{{cfg:warehouse.tick|10}} minutes**, with a fresh outside delivery every {{cfg:warehouse.ticks_per_delivery|12}} ticks. Admins can force one with `-admin tick`.

### Q: Why is a base of mine getting nothing at all?
**A**: Its road is probably cut. Supply routing follows the front line — a hub can only supply a base if the ground between them is clear of enemy-held objectives, and enemy bases interdict a belt around themselves. A cut-off base can only be reached by air, by player crates, or by an AI helo run. See [Materiel & the War Economy](../gameplay/war-economy.md).

## Technical Issues

### Q: I can't load cargo/troops!
**A**: Check:
- Are you close enough to objective? (within ~50m)
- Is it a friendly objective?
- Does your aircraft have that capability?
- Is cargo/troops available?
- Try landing closer

### Q: F10 menu not responding
**A**:
- Wait a moment (server lag)
- Try re-opening (press F10 again)
- Re-slot if persistent
- Report to admin if continues

### Q: My deployed unit disappeared!
**A**:
- It may have been destroyed
- Check F10 map for its marker
- Server restart can remove some units
- Contact admin if it seems like a bug

## Points & Economy

### Q: How much do actions cost?
**A**: Read it off the menu — every action's cost is in its own label
(`E-3A AWACS(100 pts)`), generated from the same config the engine charges you
from, so it can never be stale the way a wiki table can.

Rough shape on the live mission: AWACS and bomber 100, tanker and naval strike
50, JTAC drone 25, waypoint moves 5–10, and the tasking board free. Ground
deployables are paid for in **crates and materiel**, not points — see
[Deployable Units](./deployables.md) and
[Materiel & the War Economy](../gameplay/war-economy.md).

### Q: Can I get refunds?
**A**: Yes! Use `-delete <group-id>` to delete your deployed units and get **50% of the cost back**. Action units (AWACS, fighters, etc.) cannot be deleted by players.

### Q: Can I transfer points to teammates?
**A**: If enabled: `-transfer <amount> <player-name>`
If not enabled, this command won't work.

### Q: What's a good starting strategy for earning points?
**A**:
- Fly CAP and get air kills
- Air to Ground
- Transport troops/cargo

## Troubleshooting

### Q: I'm stuck in spectator and can't slot!
**A**:
- Are you registered to the *other* coalition? (you can only slot your own side)
- Are you out of lives for that role? (check `-lives`)
- Is the slot occupied?
- Try different slot

### Q: Commands don't work!
**A**:
- Check spelling
- Include dash `-` prefix (except `blue`/`red`)
- Verify you have permission
- Some commands require admin

### Q: Game crashed/disconnected, did I lose my life?
**A**: Depends on when:
- Crashed during loading: Usually no
- Crashed in flight: Might lose life if aircraft was destroyed
- Contact admin if unfair death

## Still Have Questions?

Ask in:
- **Discord**: [https://discord.gg/XyGkb3WAXS](https://discord.gg/XyGkb3WAXS)
