# Introduction

Welcome to the **Fowl Engine Dynamic Campaign System** for DCS World. This wiki
documents the campaign from a player's point of view: what you see, what you
can do, and what the server tells you.

## What is Fowl Engine?

Fowl Engine (also known as BFNEXT) is a dynamic campaign system that turns DCS
World into a persistent, player-driven war. Unlike a scripted mission, the
campaign state moves with what players do: bases change hands, supply runs
keep them alive, and every sortie shifts the picture a little.

## Key Features

- **Persistent world**: the campaign saves and carries over server restarts,
  round after round
- **Objectives**: airbases, FARPs, FOBs, factories, logistics hubs and naval
  bases to capture and hold, each with health, logistics, supply and fuel
- **Logistics**: crates, troops, convoys and supply transfers decide which
  bases can repair, resupply and launch aircraft
- **Points and lives**: kills, captures and logistics earn points that buy
  support aircraft, drones and deployable units; lives are counted per role
  where the server limits them
- **JTACs, artillery and recon**: laser-designating spotters, fire missions
  and recon passes that put the enemy on your side's map
- **Ground war and command**: AI formations fight over the front line, and
  commanders direct them and the side's other assets
- **An auto-generated briefing**: every round the engine writes your
  coalition's situation and a ranked task list, from what your side can see
- **Team play**: choose Blue or Red and work with your side

## How one sortie moves the war

```steps
[
 {"title":"Take a slot","text":"Pick a side and an aircraft at a base your side owns. The engine builds your F10 menus and, a few seconds after you spawn, shows the airfield ATIS and your side's situation briefing with a suggested task. See [Joining a Team](./getting-started/joining-team.md) and [The Auto-Generated Briefing](./gameplay/briefing.md)."},
 {"title":"Fly a task","text":"Kill enemy units, lase targets for others, call in support from the Actions menu, or haul crates and troops. Kills, captures and deliveries earn points; your aircraft and the support you buy cost them. See [Points and Lives](./gameplay/points-and-lives.md)."},
 {"title":"Wear a base down","text":"An enemy base that has lost its defenders and most of its health becomes capturable. Troops landed inside its ring start the capture timer. See [Capturing Objectives](./gameplay/capturing-objectives.md)."},
 {"title":"Keep it supplied","text":"Captured bases need supply to repair and to launch aircraft. Convoys, airlift and player crates move it from the logistics hubs. See [Logistics & Supply](./gameplay/logistics.md)."},
 {"title":"The map moves","text":"Every capture, delivery and loss changes the map, the front line and the next briefing, for both sides. The campaign saves it all, so the next sortie starts where this one ended. See [Reading the F10 Map](./gameplay/reading-the-map.md)."}
]
```

## New here? Start with the playbooks

If it's your first time on the server and it's clearly *not* "join and fly,"
read these in order. They're task-by-task walkthroughs, not reference dumps:

1. **[Your First Sortie](./playbooks/first-sortie.md)**: your first 20 minutes
2. **[Reading the Live Ops Dashboard](./playbooks/reading-live-ops.md)**: the web map and stats
3. **[Capturing a Base](./playbooks/capturing-a-base.md)**, **[Flying CAS With a JTAC](./playbooks/cas-with-jtac.md)**, **[Running Crates](./playbooks/running-crates.md)**, **[Calling Support](./playbooks/calling-support.md)**

For the basics of the interface, see
[Welcome to Fowl Engine](./getting-started/welcome.md) and
[Understanding the Menus](./getting-started/hud-and-menus.md).

## Getting Help

- Join the [Discord community](https://discord.gg/XyGkb3WAXS)
- Check the [presentation](https://docs.google.com/presentation/d/1EAOe0iK-1s6i0UV5ObxSD86gGBj1Ixz6FOotQn5XPdc/edit#slide=id.g2b6a346170f_1_35)
- Find the server in the DCS multiplayer browser (name and IP are in Discord)
- In game: `-help` in chat, **F10 → Info → Help** for short guides, and this
  whole wiki in the **WIKI** tab of the
  [Vector Strike cockpit panel](./getting-started/cockpit-overlay.md)

## Which server am I reading about?

Campaign numbers (what a kill pays, how long a capture takes, what an AWACS
costs) are set per DCS server, and a wiki page reads them **live from the
server you have selected**. If this deployment fronts more than one server you
will see a **server selector in the top bar**, and pages that quote numbers say
which server's numbers they are showing. Switch it and the figures change with
it.

If you only see one server, there is nothing to choose and the selector is
hidden.

## About This Wiki

This wiki is organized into sections:

- **[Getting Started](./getting-started/welcome.md)**: what the campaign is, joining a side, the menus and the cockpit panel
- **[Playbooks](./playbooks/first-sortie.md)**: step-by-step walkthroughs for common tasks
- **[Core Gameplay](./gameplay/objectives.md)**: campaign mechanics: objectives, logistics, points, the briefing, the map
- **[F10 Menu Systems](./f10-menu/overview.md)**: every in-game menu, explained
- **[Advanced Topics](./advanced/c130-airdrop.md)**: C-130 airdrop, deployables, artillery, cruise missiles, recon intel, AI helo missions, the ground war, command and wingmen
- **[Reference](./reference/faq.md)**: command lists, unit tables and the FAQ
