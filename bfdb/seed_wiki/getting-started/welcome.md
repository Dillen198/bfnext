# Welcome to Fowl Engine

This page covers what happens when you first join a Fowl Engine server, and
what makes it different from an ordinary DCS mission.

![Finding the server in the DCS multiplayer server browser](/api/wiki/images/cf08da53-e826-49b2-8b67-7ddded3cbb74)

## First Steps

1. **Find the server** in the DCS multiplayer browser and connect.
2. **Pick a side and a slot.** Choose Blue or Red on the normal slot screen
   and take an aircraft slot at a base your side holds. There is nothing to
   type: your first slot registers you to that coalition for the round, and
   the server confirms it in chat:

```dcsmessage
Welcome to the Blue team. You may only occupy slots belonging to your team. Good luck!
```

3. **Spawn and listen.** In the first seconds after your aircraft spawns,
   where the server has them on:
   - the GCI frequency, at once;
   - a takeoff hold on ground starts ({{cfg:takeoff_delay_secs|0}} s here),
     counted down until `Takeoff hold cleared -- happy hunting!`;
   - the airfield or carrier ATIS, 15 seconds in;
   - the **situation briefing**, {{cfg:situation_briefing.delay_secs|22}}
     seconds in and on screen for
     {{cfg:situation_briefing.display_secs|40}} seconds, with the task the
     engine picked for your airframe. It only appears on servers that
     configure it (`situation_briefing`).
4. **Read the briefing any time.** `-brief` in chat gives the short version,
   and **F10 → Info → Situation** the full six pages, on every server. See
   [The Auto-Generated Briefing](../gameplay/briefing.md).
5. **Learn the basics**: the chat commands and F10 menus on
   [Understanding the Menus](./hud-and-menus.md).

Your F10 radio menu appears a few seconds after you spawn. **F10 → Info →
Help → Getting Started** sums up what each menu is for.

## What Makes Fowl Engine Different?

### Persistent Campaign

Unlike traditional DCS missions that reset after each flight:

- The battlefield state is saved and carries over server restarts
- A round runs until a side wins it (or the server's round limit or an admin
  ends it), then a new round starts everyone fresh; see
  [How a round ends](../gameplay/round-rules.md#how-a-round-ends)
- Your actions have lasting consequences: bases you take stay taken, units
  you deploy stay deployed

### Strategic Depth

- Capture objectives to expand your coalition's territory
- Supply lines and logistics decide which bases can repair and resupply
- AI ground formations fight over the front line, and players can command
  them
- Coordinate with teammates for maximum effectiveness

### Reward System

- Earn points for kills, captures **and logistics**: a logistics repair pays
  {{cfg:points.logistics_repair|25}} points and up, against
  {{cfg:points.air_kill|25}} for an air kill
- Spend points on support aircraft, drones, deployable units, AI helo
  missions and the other actions on the F10 menu
- Lives are counted per role where the server limits them (it does here:
  **{{cfg:limited_lives|yes}}**). Take off from a friendly base and a life is
  taken; land at one and you get it back
- See [Points and Lives](../gameplay/points-and-lives.md)

### Commander System

Pilots who reach the server's commander rank (tier
{{cfg:command.commander_rank|4}}, Major, by default), or whom an admin
appoints, become **commanders**: they order the side's ground formations, AI
flights, batteries, convoys and the Theatre HQ from the dashboard's COMMAND
page, and get the server's Commander role on Discord. See
[Command](../advanced/command.md).

## Next Steps

See [Joining a Team](./joining-team.md) for how coalitions work, then
[Understanding the Menus](./hud-and-menus.md) and
[Your First Sortie](../playbooks/first-sortie.md).
