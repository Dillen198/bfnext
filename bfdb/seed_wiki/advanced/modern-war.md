# Modern War: EW, Air Defence, Raids & Tempo

The campaign borrows the way the current wars are actually fought. Between the
front lines, both sides raid each other's rear with drones and missiles. They
jam GPS and radios, and they run their air defences out of missiles. The fight
goes in offensives and pauses.

Every part of this reacts to the map as it stands: who holds what, how much
supply a base has, what is still alive and who is in the air. Each part is
switched on separately per server, so a server may run some of it and not the
rest.

## Electronic warfare

Command centres and naval bases (and any other objective kind the server
picks) host a **jammer truck**. It isn't even in DCS until enemy aircraft come
within about 1.25× its activation range, and then sits dark until they come
within {{cfg:modern_war.ew.activation_radius_m|80000}} m. "Enemy aircraft"
means anything its side's radar network paints, plus every enemy player in
the air, painted or not. Then it switches on and stays on for at least
{{cfg:modern_war.ew.min_on_secs|180}} seconds.

- **GPS and GLONASS** are jammed in the area. Naval bases **spoof** them
  instead, pushing receivers toward a false position about
  {{cfg:modern_war.ew.spoof_offset_m|25000}} m away. Expect GPS-guided weapons
  and satellite navigation to misbehave near an active jammer. Plan for laser,
  TV or dumb weapons, or kill the jammer first.
- **Radios** in the military UHF band
  ({{cfg:modern_war.ew.band_mhz[0]|225}} to
  {{cfg:modern_war.ew.band_mhz[1]|400}} MHz) are hit too. Within
  {{cfg:modern_war.ew.comms_jam_radius_m|60000}} m of an enemy jammer, the
  [GCI controller's](../gameplay/gci.md) calls break up. You hear it once on
  the way in ("Viper 1-1, Magic, you are being jammed, comms degraded"),
  after which about seven calls in ten are lost and the rest arrive with
  words missing.
- **Emitting gives it away.** When a jammer switches on, the other side gets
  an ELINT mark on the F10 map somewhere within
  {{cfg:modern_war.ew.intel_uncertainty_m|5000}} m of it, reading
  `ELINT: GPS and comms jamming in this area` (or
  `ELINT: GPS spoofing and comms jamming in this area` at a naval base). The
  mark goes when the jammer switches off.

### SAM-site jammers

SAM sites have their own jammer, which works differently:

- It jams GPS and GLONASS only. GPS-guided weapons (JDAM, JSOW, GMLRS)
  aimed at the site go astray; HARMs home on the radar and are unaffected.
- It wakes only when aircraft come within
  {{cfg:modern_war.ew.sam_activation_radius_m|40000}} m.
- It keeps the site hidden. There is no radio jamming to give it away, and
  ELINT only marks the area, roughly (within
  {{cfg:modern_war.ew.sam_reveal_uncertainty_m|15000}} m), after it has jammed
  for {{cfg:modern_war.ew.sam_reveal_after_secs|600}} seconds. That mark reads
  `ELINT: GPS jamming somewhere in this area`.
- If you find your JDAMs missing around a site, suppress it with HARMs or
  switch to laser-guided weapons.

### Killing a jammer

Kill the truck and the area clears. Both sides are told:

```dcsmessage
Enemy jammer at Senaki destroyed -- GPS and comms clear there.
```

The side that lost it reads `Our jammer at Senaki has been destroyed.` For a
SAM-site jammer the messages never name the site:
`Enemy GPS jammer destroyed -- guided weapons clear in that area.` for you,
and `A GPS jammer guarding one of our SAM sites has been destroyed.` for them.

A destroyed jammer is replaced after
{{cfg:modern_war.ew.respawn_secs|1800}} seconds, but only while its side still
holds the objective and it is not at zero health. Capture the objective and
the old owner's jammer is gone for good (the new owner gets its own after the
same delay).

## The air defence network

On a server running the integrated air defence network (IADN), a SAM site is
smarter than DCS's own AI. The basics (command centres, shared sensor
picture, radars dark until a confirmed threat) are on
[Objectives](../gameplay/objectives.md#command-centers). What matters when
you fly SEAD:

- **A networked site radiates only when it has to.** It comes up when the
  fused picture (EWR, AWACS and other sites' search radars) holds a hostile
  track with enough confidence, after a random delay of up to
  {{cfg:iadn.reaction_delay_max_secs|4}} seconds, so a cluster of sites does
  not light up on the same beat. Once up it stays up at least
  {{cfg:iadn.min_hot_dwell_secs|15}} seconds.
- **The tracking radar comes on last.** Where a site has a separate tracking
  radar, it only powers up once the target is inside
  {{cfg:iadn.track_radar_range_fraction|0.5}} of the search radar's range.
- **Some sites shut down for a HARM, some don't.** About
  {{cfg:iadn.arm_discipline|0.6}} of sites (as a fraction, re-rolled every
  mission load) are drilled to go dark when an anti-radiation missile is
  flying at them, inside {{cfg:iadn.harm_defense_radius_m|20000}} m and within
  {{cfg:iadn.arm_aim_cone_deg|25}} degrees of its nose. A drilled site notices
  with a chance of {{cfg:iadn.arm_react_chance|0.75}}, rolled once per missile,
  and stays dark {{cfg:iadn.harm_defense_cooldown_secs|20}} seconds. The rest
  keep radiating and die to it.
- **Decoys work.** Every TALD or MALD in the air near a site cuts its chance
  of noticing a real ARM by {{cfg:iadn.decoy_saturation|0.3}} (multiplied).
  Four decoys leave a drilled site under a one-in-five chance. Decoys never
  make a site go dark on their own.
- **A blinded site still fights while it is networked.** A site that has lost
  its search radar but still has a tracking radar or launcher is cued from the
  network picture out to {{cfg:iadn.sam_offboard_cue_range_m|60000}} m. Kill
  its command centre and it falls back to plain DCS behaviour.
- **Jamming degrades radar.** Radars within
  {{cfg:iadn.jamming_range_m|40000}} m of an enemy jamming aircraft (a type
  the server classifies as a jammer) have their detection chance cut, by up to
  {{cfg:iadn.jamming_detection_penalty|0.5}} at point blank for the most
  susceptible sets. The effect is strongest close to the jammer and blunts
  those radars against everything, so it covers the rest of the package too.

Player-deployed SAM sites are left out of all this: they come up hot and stay
under their owner's control.

## Air defence runs out of missiles

Every SAM site has a magazine of
{{cfg:modern_war.sam_stock.missiles_per_launcher|4}} missiles per launcher,
{{cfg:modern_war.sam_stock.reload_multiplier|2}} loads deep. Each launch spends
one. The count survives the site being culled and respawned, so a site does
not quietly refill when you fly away and come back. A site that has fired
everything goes **Winchester**: it stops shooting, and its side is told.

```dcsmessage
SAM site at Gudauta is out of missiles (Winchester). Keep it supplied to rearm.
```

Only logistics bring it back. Every
{{cfg:modern_war.sam_stock.resupply_period_secs|900}} seconds a depleted site
gets {{cfg:modern_war.sam_stock.resupply_per_period|4}} missiles back, but only
while its objective has at least
{{cfg:modern_war.sam_stock.min_supply|40}}% supply and isn't under attack.
When a Winchester site has missiles again it is weapons free and its side reads
`SAM site at Gudauta has been rearmed.`

Cut a base's supply and its air defence goes quiet. Saturate it with raids and
it runs dry.

## Raids on infrastructure

At night on the mission clock (from
{{cfg:modern_war.raids.hours[0]|20}}:00 to
{{cfg:modern_war.raids.hours[1]|5}}:00), each side raids the other's factories,
logistics hubs, airbases and naval bases about every
{{cfg:modern_war.raids.interval_secs|2700}} seconds (give or take 30%, and
faster during an offensive). Raids only run while somebody is on the server,
and a side has one raid going at a time.

The target is picked from the three most valuable enemy objectives it can
reach: factories first, then logistics hubs, airbases and naval bases, worth
more the fuller their stores. The objective your side's offensive is aimed at
counts double.

- **One-way attack drones** ({{cfg:modern_war.raids.drones_per_raid|4}} per
  raid) launch from the ground at the raiding side's objective deepest behind
  its lines that is still within range of the target (and at least 30 km
  from it). They take a couple of minutes to get airborne, then fly low, at
  {{cfg:modern_war.raids.drone_alt_agl_m|400}} m above the ground, straight at
  the target. SAMs, AAA and **you** can shoot them down. One that gets within
  about 1.2 km of the target detonates on the ground beneath it.
- **Ballistic missiles** fire from the side's own deployed launchers (Scud,
  Iskander, ATACMS), up to {{cfg:modern_war.raids.max_launchers|2}} launchers
  in range. Each missile-defence site within 40 km of the target gives every
  missile a {{cfg:modern_war.raids.intercept_per_site|0.35}} chance (as a
  fraction) of being intercepted, up to 85% in all, and each interception
  spends two of that site's missiles.

![Timeline of a raid: the raiding side is told the strike has launched and the target side gets an air raid warning with an arrival estimate, drones spend about two minutes getting airborne then fly low toward the target while defenders can shoot them down, missiles are resolved about a minute after launch with interceptions spending missile-defence stock, and each hit cuts the target's stores and halts a factory before both sides get a report.](/api/wiki/images/e1203ac6-4ad6-411c-92eb-95288ed72b2d)

The raiding side is told the raid is away, and the defending side gets an
**air raid warning** with the target and an arrival estimate, so a fighter or
helicopter on patrol has a clear job:

```dcsmessage
AIR RAID WARNING: 4 one-way attack drones and 4 ballistic missiles inbound toward Kobuleti Logistics. Drones expected in about 38 minutes.
```

The raiding side's version reads
`Strike launched on Kobuleti Logistics: 4 one-way attack drones and 4 ballistic missiles.`
A raid of only missiles has no arrival estimate.

Every drone that gets through, and every missile that isn't intercepted, cuts
the target's stores by {{cfg:modern_war.raids.supply_damage_pct|5}}% and halts
a factory's production for {{cfg:modern_war.raids.factory_pause_secs|1800}}
seconds. Both sides get a report when the raid is over:

```dcsmessage
Raid on Kobuleti Logistics is over: 2/4 drones got through, 1/4 missiles hit. stores cut 15%.
```

The raiding side reads the same result as
`Strike on Kobuleti Logistics: 2/4 drones got through, 1/4 missiles hit. stores cut 15%.`
When nothing hit, the result reads `no damage`; a factory hit adds
`production halted`.

## Sea drones

A side with a naval base sends packs of fast boats at enemy ships. They leave
friendly water near the base, run at the nearest enemy ship in reach and
detonate alongside it. Boats steer at where the ship is now, so a ship that
runs only buys time.

```dcsmessage
Fast boats closing on our CVN_73 -- engage them!
```

That warning goes to the target's side when the boats close within 15 km. It
names the ship by its DCS type. Escorts, helicopters and strafing runs can stop
them. The launching side reads
`Sea drones launched: 3 boats running at an enemy CVN_73.`, and when it is
over both sides get the result
(`Sea-drone attack on our CVN_73 is over: 1 of 3 boats hit.` for the target).

Sea drones are spawned outside the campaign's records, so sinking one is not
scored as a kill.

## Supply by rail and road

**Supply trains** run between a side's stations: any objective with a
railway within {{cfg:modern_war.rail.station_radius_m|5000}} m. A logistics
hub or factory with at least half its stock loads a train for the neediest
station below {{cfg:modern_war.rail.destination_supply_below|70}}% supply,
nearer the front first. It runs the real rail line at
{{cfg:modern_war.rail.speed_kph|60}} km/h and carries up to
{{cfg:modern_war.rail.per_item_cap|150}} of each item, far more than a truck
convoy.

```dcsmessage
Supply train departing Kutaisi for Senaki (64 km by rail).
```

- The load leaves the origin when the train departs and arrives only if the
  train does (`Supply train from Kutaisi has arrived at Senaki.`).
- **Destroy the train and its cargo is gone**, and both sides are told:
  `Our supply train to Senaki was destroyed -- its cargo is lost.` to the
  owner, `Enemy supply train bound for Senaki destroyed.` to the side that
  killed it.
- Trains never use a line that passes within
  {{cfg:modern_war.rail.enemy_clearance_m|15000}} m of an enemy objective. To
  hit one, you have to go behind the lines.
- A side sends one every {{cfg:modern_war.rail.interval_secs|1800}} seconds at
  most, with up to {{cfg:modern_war.rail.max_trains_per_side|2}} running at
  once.

**Fuel convoys** can be tractor-trailer refuelers: a KrAZ or MAZ truck towing
a fuel tank trailer. The trailers are hitched when the convoy sets off. Kill
the tractor and its trailer goes nowhere.

**Tank transporters** carry replacement armour when a player calls
**Reinforce** from the Actions menu. They drive from the nearest friendly
base, so an objective that has lost its garrison can be rebuilt. Every
transporter killed on the road is armour that never arrives. See
[Action Types](../reference/action-types.md#reinforce).

## Offensives and pauses

Each side alternates an **offensive** of
{{cfg:modern_war.tempo.offensive_hours|6}} hours with a **regroup** of
{{cfg:modern_war.tempo.regroup_hours|10}} hours. Red runs half a cycle out of
step with Blue, so one side is usually pushing while the other digs in. The
phase follows the real-world clock, not the mission clock, so an offensive
carries on across a server restart.

![Two bars over 32 hours of real time, one for Blue and one for Red: each alternates a 6-hour offensive with a 10-hour regroup at the engine defaults, with Red shifted by half a cycle (8 hours), so for most of the day one side is on the offensive while the other regroups.](/api/wiki/images/1ee94c78-dd03-4ba2-b1a2-20c1e89925fb)

- **During an offensive**, raids, sea-drone attacks and AI air packages come
  faster (their intervals are multiplied by
  {{cfg:modern_war.tempo.offensive_factor|0.5}}) and raids concentrate on one
  enemy objective, the **axis**: the one your side is best placed to hit,
  weighted toward logistics hubs, factories and airbases and never a SAM site
  or carrier. Packages head for it too when no friendly pilot is up.
- **While regrouping**, the tempo drops (intervals multiplied by
  {{cfg:modern_war.tempo.regroup_factor|1.5}}).

Orders come as an "OPERATIONAL ORDERS" message when the posture changes:

```dcsmessage
OPERATIONAL ORDERS: offensive operations toward Kobuleti begin. Raids and air packages concentrate there for the next 6 hours.
```

```dcsmessage
OPERATIONAL ORDERS: the offensive is over. Regroup and resupply for the next 10 hours; strike tempo drops.
```

If the axis changes hands mid-offensive, a new one is picked:
`OPERATIONAL ORDERS: the offensive shifts to Batumi. Strikes will concentrate there.`
The hours in these messages are what is left of the phase. No message is sent
for the phase already under way when the server starts.

## Artillery shoots and scoots

When the server sets it, a battery relocates up to
{{cfg:artillery.shoot_and_scoot_m|0}} m after every fire mission. Counter-fire
and players hunting muzzle flashes find an empty position. A value of 0 means
batteries stay put. See [Artillery](./artillery.md).

## Server setup

Everything lives under `modern_war` in the engine config, one block per
system. Leave a block out to switch that system off. The air defence network
is the separate `iadn` block, and needs `radar_physics` as well.

- **`ew`** spawns DCS's jammer trucks (`GPS_Spoofer_Blue`/`_Red`), so both must
  be in `unit_classification`. `host_kinds`, `spoof_kinds` and
  `sam_site_kinds` take objective kinds by name.
- **`sam_stock`** needs nothing.
- **`raids`** needs `drone_templates_red`/`_blue`: plane-section groups in the
  .miz flown as the drones (any slow UAV or light aircraft), classified like
  any other type. Missile raids use deployed launchers whose types are in
  `missile_types` and have `artillery.units` ranges. `run_when_empty` lets
  raids run with nobody on; `hours` set to null allows raids at any time.
- **`boat_raids`** needs a naval base on the raiding side.
- **`rail`** needs railways on the map; the train consists are set per side.
- **`tempo`** needs nothing.
- **`iadn.anti_radiation_weapons`** must list the ARM weapon types (for
  example `AGM_88C`) for the HARM defence to react at all.

## See also

- [Wingmen, AI Packages & Civil Traffic](./air-life.md)
- [AI Opposition](./ai-opposition.md)
- [Campaign Events](./campaign-events.md)
- [Live GCI](../gameplay/gci.md)
