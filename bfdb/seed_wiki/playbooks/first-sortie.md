# Your First Sortie

You've connected, you're staring at the slot list, and it's clearly *not* "join
and fly." This page is the 20-minute version: what to do, in order, your first
time on the server. Every step links to the page with the full detail.

## The walkthrough

```steps
[
 {"title":"1. Look at the war","text":"Before you spawn, open the [Live Ops dashboard](./reading-live-ops.md) (or the F10 map) and see where the front is and which side is on the back foot. Once you are in a slot, `-brief` in chat gives the same picture in a few lines."},
 {"title":"2. Take a slot","text":"Choose Blue or Red and take an aircraft slot at a base your side holds. Your first slot registers you to that coalition for the round; there is nothing to type. The server answers in chat:\n\n`Welcome to the Blue team. You may only occupy slots belonging to your team. Good luck!`\n\nSwitching later is limited; see [Joining a Team](../getting-started/joining-team.md)."},
 {"title":"3. Spawn and listen","text":"Where the server has them on, the GCI frequency arrives at once, the airfield ATIS 15 seconds in, and the situation briefing {{cfg:situation_briefing.delay_secs|22}} seconds in, on screen for {{cfg:situation_briefing.display_secs|40}} seconds. Its **YOUR TASK** line is the job the engine picked for your airframe: a good first sortie if you have no better idea. See [Your Task](../gameplay/your-task.md)."},
 {"title":"4. Check your account","text":"Type `-lives` and `-balance` in chat, or open **F10 → Info → My Status**. Lives are counted per role (Standard, Intercept, Attack, Recon, Logistics) where the server limits them. See [Points and Lives](../gameplay/points-and-lives.md)."},
 {"title":"5. Wait out the takeoff hold","text":"On a server with a takeoff hold, a ground start counts down (`Takeoff hold: 25s remaining`) until `Takeoff hold cleared -- happy hunting!`. Lifting off earlier puts you back in spectators."},
 {"title":"6. Do one useful thing","text":"Pick the lane that matches your slot (below): a crate or troop run in a transport helicopter, a JTAC target in a strike jet, a CAP in a fighter, or a recon pass. Taking off from a friendly base costs a life where lives are limited."},
 {"title":"7. Land at a friendly base","text":"Land at any friendly objective. That gives your life back, banks the sortie, and the debrief starts with `LANDED at Kutaisi -- landing registered, safe to leave the slot`. Then check `-balance`; [Calling Support](./calling-support.md) shows what the points buy."}
]
```

## Your first flight: pick one

You don't need to win the war on sortie one. Pick the lane that matches your
slot and do one useful thing.

### Transport helicopter (Mi-8, Huey, CH-47F, Gazelle)

**Run a crate or a troop lift.** This is the highest-impact thing a new player
can do and it's almost impossible to mess up badly.

- **Troops**: land inside a friendly base, **F10 → Troops → Squads → Load
  Standard squad** (or another squad), fly to the target and
  **F10 → Troops → Unload** on the ground. Infantry inside an enemy base's ring
  is what captures it once the base is capturable.
- **Crates**: land inside a friendly base, **F10 → Cargo → Crates → Base
  Supply** and pick a crate, **Load Nearby Crate**, fly it to a different
  friendly base that needs it, then **Unload Crate** and **Unpack Nearby
  Crate(s)**.

```dcsmessage
KillerDog loaded Standard
3 of 10 Standard deployed, unloading will delete oldest, 5521 when the limit is exceeded
```

Full walkthroughs: [Troop Transport](../f10-menu/troops.md) and
[Running Crates & Building a Base](./running-crates.md). On a helicopter
dynamic-cargo server you load crates with **F8 → Ground Crew → Cargo**
instead; see [Cargo Operations](../f10-menu/cargo.md).

### Strike or CAS jet (A-10, JF-17, F-16, Su-25 and the like)

**Work a JTAC target.** A JTAC is a ground or drone spotter that finds targets
and lasers them for you.

1. **F10 → `JTAC>>`**. The menu closes; open it again.
2. **JTAC → `Batumi (2 JTAC)>>`** (the place nearest the fight). It closes
   again and tells you the JTACs there are loaded.
3. **JTAC → Batumi → `Reaper 1234 [1688]` → Status**. The number in brackets
   is its **laser code**.
4. Set your weapon's laser code to match, fly to the area and drop on the
   lased target.

Full walkthrough: [Flying CAS With a JTAC](./cas-with-jtac.md).

### Fighter (F-15, F/A-18, MiG-29, Su-27)

**Fly a CAP for your side.** Check the EWR picture
(**F10 → GCI/EWR → Report**, see [Early Warning Radar](../f10-menu/ewr.md)),
push toward the front, and protect the transport and CAS players who are
actually moving the front line. An air kill pays
{{cfg:points.air_kill|25}} points. Where the server holds kill points until
you land ({{cfg:points.provisional|no}} here), they only count once you are
down at a friendly base.

### A recon-capable airframe

If your aircraft has a **Recon** menu (inside **Flight** when you also get
Wingman), fly toward an enemy objective, check **Recon Status**, then
**Start Recon Pass** and hold within {{cfg:player_recon.range_m|25000}} m for
{{cfg:player_recon.dwell_secs|120}} s. The enemy units around it appear on
the F10 map for your whole coalition:

```dcsmessage
Recon complete -- 11 contact(s) on the F10 map
```

Low risk, genuinely useful, and you learn the map. Light **Recon**-role
airframes also get one of the biggest life pools
({{cfg:default_lives.Recon[0]|6}} lives). See
[Reconnaissance](../f10-menu/recon.md) and the
[Aircraft Roster](../reference/aircraft-roster.md).

## After you land

- **Land at a friendly objective** to get your life back. The debrief tells you
  what counted:

```dcsmessage
LANDED at Kutaisi -- landing registered, safe to leave the slot
Sortie: 42 min from Senaki-Kolkhi
Weapons fired: 4
Kills: 2 (+9 pts): 1x T-72B, 1x BMP-2
Balance: 1840 pts
Lives: standard 3/3 (life returned)
```

- Landing anywhere else says
  `LANDED outside a friendly objective -- no landing credit here`.
- Check `-balance`: you earn points for kills, captures, logistics and
  deliveries. Points buy support aircraft, drones and deployables:
  [Calling AWACS, Tankers & CAP](./calling-support.md).

## The rules that trip up everyone

- **You can't load from a broken base.** Troops and crates only come from a
  friendly base whose logistics are above 0% and which is **not capturable**.
  Stage from a base further back.
- **You can't unload troops in your own threatened base.** Inside one of your
  own bases with an enemy in sight, Unload answers:

```dcsmessage
you can't deploy troops here while enemies are near
```

  Wait until {{cfg:threatened_cooldown|300}} seconds after the last enemy is
  out of sight, or unload outside the base. The enemy base you are attacking
  has no such rule.
- **Menus with `>>` need selecting first**, and long lists continue under
  `More >>`. See [Understanding the Menus](../getting-started/hud-and-menus.md).

## Where to go next

- [Reading the Live Ops Dashboard](./reading-live-ops.md): the web map and
  stats pages
- [Capturing a Base](./capturing-a-base.md): the full assault, start to finish
- [Chat Commands](../gameplay/chat-commands.md): the full command list
- [FAQ](../reference/faq.md)
