# Modern War: EW, Air Defence, Raids & Tempo

The campaign borrows the way the current wars are actually fought. Between the
front lines, both sides raid each other's rear with drones and missiles. They
jam GPS and radios, and they run their air defences out of missiles. The fight
goes in offensives and pauses.

Every part of this reacts to the map as it stands: who holds what, how much
supply a base has, what is still alive and who is in the air. Each part is
switched on separately per server.

## Electronic warfare

Command centres and naval bases (and any other kind the server picks) host a **jammer
truck**. It sits dark until enemy aircraft come within
{{cfg:modern_war.ew.activation_radius_m|80000}} m. Then it switches on and
stays on for at least {{cfg:modern_war.ew.min_on_secs|180}} seconds.

- **GPS and GLONASS** are jammed in the area. Naval bases **spoof** them
  instead, pushing receivers toward a false position about
  {{cfg:modern_war.ew.spoof_offset_m|25000}} m away. Expect GPS-guided weapons
  and satellite navigation to misbehave near an active jammer. Plan for laser,
  TV or dumb weapons, or kill the jammer first.
- **Radios** in the jammed band are hit too. Within
  {{cfg:modern_war.ew.comms_jam_radius_m|60000}} m of an enemy jammer, the GCI
  controller's calls break up. You're told once ("you are being jammed"),
  after which most calls are lost and the rest arrive with words missing.
- **Emitting gives it away.** When a jammer switches on, the other side gets
  an ELINT mark on the F10 map somewhere within
  {{cfg:modern_war.ew.intel_uncertainty_m|5000}} m of it.
- **SAM sites have their own jammer**, which works differently:
  - It jams GPS and GLONASS only. GPS-guided weapons (JDAM, JSOW, GMLRS)
    aimed at the site go astray; HARMs home on the radar and are unaffected.
  - It wakes only when aircraft come within
    {{cfg:modern_war.ew.sam_activation_radius_m|40000}} m.
  - It keeps the site hidden. There is no radio jamming to give it away, and
    ELINT only marks the area, roughly (within
    {{cfg:modern_war.ew.sam_reveal_uncertainty_m|15000}} m), after it has jammed
    for {{cfg:modern_war.ew.sam_reveal_after_secs|600}} seconds.
  - If you find your JDAMs missing around a site, suppress it with HARMs or
    switch to laser-guided weapons.
- **Kill it and the area clears.** A destroyed jammer is replaced after
  {{cfg:modern_war.ew.respawn_secs|1800}} seconds, but only while its side
  still holds the objective. Capture the objective and it's gone for good.

## Air defence runs out of missiles

Every SAM site has a magazine of {{cfg:modern_war.sam_stock.missiles_per_launcher|4}}
missiles per launcher, {{cfg:modern_war.sam_stock.reload_multiplier|2}} loads
deep. Each launch spends one, and a site
that has fired everything goes **Winchester**: it stops shooting, and its side
is told.

Only logistics bring it back. Every
{{cfg:modern_war.sam_stock.resupply_period_secs|900}} seconds a Winchester or
depleted site gets {{cfg:modern_war.sam_stock.resupply_per_period|4}} missiles,
but only while its objective has at least
{{cfg:modern_war.sam_stock.min_supply|40}}% supply and isn't under attack. Cut a
base's supply and its air defence goes quiet. Saturate it with raids and it
runs dry.

## Raids on infrastructure

Mostly at night, each side raids the other's factories, logistics hubs,
airbases and naval bases:

- **One-way attack drones** launch from deep behind the lines and fly low to
  the target. SAMs, AAA and **you** can shoot them down. The defending side
  gets an **air raid warning** with the target and an arrival estimate, so a
  fighter or helicopter on patrol has a clear job.
- **Ballistic missiles** fire from the side's own deployed launchers in range.
  Missile-defence sites near the target intercept some of them, and each
  interception spends that site's missiles.

Every drone that gets through, and every missile that isn't intercepted, cuts
the target's stores by {{cfg:modern_war.raids.supply_damage_pct|5}}% and halts
a factory's production for {{cfg:modern_war.raids.factory_pause_secs|1800}}
seconds. Both sides get a report when the raid is over.

## Sea drones

A side with a naval base sends packs of fast boats at enemy ships. They leave
friendly water, run at the target and detonate alongside it. The target's side
is warned when they close within 15 km. Escorts, helicopters and strafing runs
can stop them.

## Supply by rail and road

**Supply trains** run between a side's stations: any objective with a
railway within {{cfg:modern_war.rail.station_radius_m|5000}} m. A logistics
hub or factory with stock loads a train for the neediest station nearer the
front, and it runs the real rail line at
{{cfg:modern_war.rail.speed_kph|60}} km/h. A train carries far more than a
truck convoy.

- The load leaves the origin when the train departs and arrives only if the
  train does. **Destroy the train and its cargo is gone**, and both sides are
  told.
- Trains never use a line that passes within
  {{cfg:modern_war.rail.enemy_clearance_m|15000}} m of an enemy objective. To
  hit one, you have to go behind the lines.

**Fuel convoys** can be tractor-trailer refuelers: a KrAZ or MAZ truck towing
a fuel tank trailer. The trailers are hitched when the convoy sets off. Kill
the tractor and its trailer goes nowhere.

## Offensives and pauses

Each side alternates an **offensive** of
{{cfg:modern_war.tempo.offensive_hours|6}} hours with a **regroup** of
{{cfg:modern_war.tempo.regroup_hours|10}} hours, half a cycle out of step with
the enemy.

- **During an offensive**, raids and AI air packages come faster and
  concentrate on one enemy objective, the **axis**. Your side is told what it
  is when the offensive starts.
- **While regrouping**, the tempo drops.

Orders come as an "OPERATIONAL ORDERS" message when the posture changes.

## Artillery shoots and scoots

When the server sets it, a battery relocates up to
{{cfg:artillery.shoot_and_scoot_m|0}} m after every fire mission. Counter-fire
and players hunting muzzle flashes find an empty position.

## Server setup

Everything lives under `modern_war` in the engine config, one block per
system. Leave a block out to switch that system off.

- **`ew`** spawns DCS's jammer trucks (`GPS_Spoofer_Blue`/`_Red`), so both must
  be in `unit_classification`. `host_kinds` and `spoof_kinds` take objective
  kinds by name.
- **`raids`** needs `drone_templates_red`/`_blue`: plane-section groups in the
  .miz flown as the drones (any slow UAV or light aircraft), classified like
  any other type. Missile raids use deployed launchers whose types have
  `artillery.units` ranges.
- **`boat_raids`** needs a naval base on the raiding side.
- **`tempo`** needs nothing. The phase follows the wall clock, so an offensive
  carries across restarts.
