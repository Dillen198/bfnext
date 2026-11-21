# Tips & Best Practices

Hard-earned lessons from pilots who already learned them the expensive way.
Each one links to the page with the detail.

## Before you take off

1. **Read the briefing.** `-brief`, or `F10 → Info → Situation`. It is built
   from your coalition's actual sensor picture and ranks what matters right
   now. Two minutes there saves a wasted 40-minute sortie. See
   [The Auto-Generated Briefing](../gameplay/briefing.md).

2. **Take your task.** Every slot gets one task with a zone on your F10 map.
   Kills inside it pay 1.5×. See [Your Task & Rookie Mode](../gameplay/your-task.md).

3. **Check whether support is already up.** `F10 → Info → Support & Radios`
   lists every live AWACS, tanker and JTAC with its frequency. Don't pay for a
   second AWACS.

4. **Write down the navaid.** `F10 → Info → Navaids Directory`. FARPs, hubs and
   carriers get channels generated fresh each round, and a cockpit menu is a bad
   place to be reading when you are low on fuel in weather. See
   [Navaids & Approaches](../gameplay/navaids.md).

5. **Know the weather.** `-weather` or `F10 → Info → Weather`. On servers that
   run live weather it is real-world weather at the last restart, so it changes
   between restarts. See [Weather, ATIS & Time](../gameplay/weather.md).

6. **Plan on the dashboard.** The BRIEFING page's Planner tab builds a route
   around the SAM rings your side knows about and loads it into your jet. See
   [Mission Planner](../playbooks/mission-planner.md).

## Fighting

7. **Check the EWR's age.** The EWR report has an `AGE` column. This server's
   EWR mode is **{{cfg:ewr_mode|Original}}**; in `Delayed` mode a position can be
   up to {{cfg:ewr_delay|60}} s old, about 8 nm on a fast mover. Treat a
   contact as a cue to look, not a firing solution. See
   [Early Warning Radar](../f10-menu/ewr.md).

8. **Beam and notch.** On servers that run the radar physics model, a target
   at beam aspect with low closure is hard for pulse-Doppler radars to see, so
   the enemy's EWR picture can lose you. Going beam genuinely helps.

9. **Coordinate with a JTAC for anything that matters.** A laser on a confirmed
   target beats carpet-bombing a grid square, and everything a JTAC sees also
   goes on your side's F10 intel picture. See [JTAC System](../f10-menu/jtac.md).

10. **Buy a drone JTAC early.** It is usually the cheapest sensor in the
    Actions menu (the price is in its label), and it can lase.

11. **Mind the SAMs that don't shut down.** On servers with the integrated air
    defence network only some sites are drilled to go dark for a HARM, and
    decoys make a drilled site likelier to miss it. See
    [Modern War](../advanced/modern-war.md).

## Taking ground

12. **Ask the Capture Advisor first.** `F10 → Objectives → Capture Advisor:
    Nearest` tells you exactly which precondition is failing. Most failed
    captures are a missing precondition, not a lack of firepower. See
    [Objectives Menu](../f10-menu/objectives.md).

13. **Check your troop type can actually capture.** The advisor flags the ones
    that can't. Flying the wrong squad 60 nm is the most common wasted sortie on
    the server.

14. **Keep the pressure on.** A base only repairs while it is left alone: every
    time an enemy is in sight of it the repair countdown starts over. And a base
    that can't pay for repairs doesn't repair. That is your window.

15. **The fight isn't over when the timer completes.** Consolidation follows
    ({{cfg:capture_consolidation_secs|300}} s of progress), your troops have to
    stay in the zone, and the enemy can take it back by killing them: lose them
    all and the base goes Neutral. Extra squads speed it up, and a logistics
    repair kit or supply crate landed there jumps it forward
    {{cfg:consolidation_crate_progress_secs|120}} s. See
    [Capturing Objectives](../gameplay/capturing-objectives.md#step-5-consolidation).

16. **Post a CAPTURE or SUPPLY task.** They close themselves out when the
    coalition finishes the job, so they cost you nothing to leave up. See
    [The Tasking Board](../gameplay/tasking-board.md).

## Logistics

17. **Logistics pays like combat.** A repair kit delivered pays
    {{cfg:points.logistics_repair|25}} points and up, a supply transfer
    {{cfg:points.logistics_transfer|15}} and up, against
    {{cfg:points.air_kill|25}} for an air kill. The run nobody wants to fly is
    not charity.

18. **Guard your convoys, hunt theirs.** Automated convoys are soft targets and
    the enemy knows it. `F10 → Info → Supply Convoys` shows yours in transit,
    and every enemy convoy vehicle you kill pays
    {{cfg:points.convoy_interdiction_points|10}}.

19. **Cut the road, not the base.** Supply routing follows the front line. Take
    the objectives astride an enemy supply route and everything behind it
    starves without you ever attacking it. See
    [Materiel & the War Economy](../gameplay/war-economy.md).

20. **Factories and logistics hubs are the real targets.** A factory counts
    {{cfg:warehouse.production_scaling.factory_weight|4}} and a hub
    {{cfg:warehouse.production_scaling.logistics_weight|3}} against
    {{cfg:warehouse.production_scaling.airbase_weight|1}} for an airbase in the
    enemy's war production, and damage counts, not just ownership.

21. **Order an AI helo when nobody will fly the run.** You can dispatch troop
    insertion or resupply from a fast jet. Escort it if the route is hot. See
    [AI Helo Missions](../advanced/helo-missions.md).

22. **Glide toward friendly ground before you eject.** A pilot who comes down
    inside a friendly objective's zone recovers on their own, no helicopter
    needed. See [Combat Search & Rescue](../f10-menu/csar.md).

## Housekeeping

23. **Look for `More >>`.** DCS drops anything past the tenth menu entry; the
    engine pages the rest into `More >>`. If something is missing, look there.

24. **Re-select `Actions>>` after placing a map mark.** The Actions lists are
    built when you select it, so a new mark only appears once you do.

25. **Delete stale tasks.** There are
    {{cfg:actions.Blue.add-task.kind.AddTask.max_per_side|12}} task slots per
    coalition, shared. A board full of hour-old CAS pins is worse than an empty
    one.

26. **Debrief your sortie.** The flight replay shows every shot with its miss
    distance and the geometry at launch. See
    [Debriefing With Flight Replay](../playbooks/flight-replay.md).

27. **Talk before a big push.** Three coordinated pilots beat six lone wolves,
    every time. The tasking board and the GCI net both exist to make that
    cheaper than a Discord call.

## See Also

- [FAQ](./faq.md)
- [Objectives Menu](../f10-menu/objectives.md): the Capture Advisor
- [The Tasking Board](../gameplay/tasking-board.md)
- [Materiel & the War Economy](../gameplay/war-economy.md)
- [JTAC System](../f10-menu/jtac.md)
- [Logistics & Supply](../gameplay/logistics.md)
- [Reading the Live Ops Dashboard](../playbooks/reading-live-ops.md)
