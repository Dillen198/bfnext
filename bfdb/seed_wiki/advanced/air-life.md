# Wingmen, AI Packages & Civil Traffic

A quiet server should not mean an empty sky. Three things keep the air busy
when there are only a few of you on: a wingman you can call, AI flights that
fight for a side short of pilots, and neutral airliners going about their
business overhead.

Each one is switched on separately per server, so not every server runs all
three.

## Calling a wingman

**F10 > Wingman > Request Wingman** puts an AI flight in the air behind you.
It closes up off your right wing and escorts you: it goes after threats within
{{cfg:air_life.wingman.engage_dist_m|40000}} m of you and comes back to your
wing afterwards.

- **Call it once you are airborne.** It spawns in the air next to you, so on
  the ground the request is refused.
- **Jets get a fighter, helicopters get an attack helicopter.** A helicopter
  wingman stays close
  ({{cfg:air_life.wingman.rotary_engage_dist_m|8000}} m) and engages
  helicopters **and ground units**, so it will shoot at whatever is shooting at
  your LZ.
- **It is for quiet servers.** You can only call one while your side has
  {{cfg:air_life.wingman.max_side_pilots|4}} or fewer pilots in aircraft.
- **Cost:** {{cfg:air_life.wingman.cost|0}} points.

It goes home by itself when you:

- land and stay down for a minute,
- leave the slot or die,
- release it (**F10 > Wingman > Release Wingman**), or
- reach its limit of
  {{cfg:air_life.wingman.lifetime_secs|3600}} seconds.

If it is shot down, you have to wait
{{cfg:air_life.wingman.cooldown_secs|300}} seconds before calling another.
**Wingman Status** tells you how many of its aircraft are left, how far away it
is, and how much time it has.

## AI packages

When a side has {{cfg:air_life.packages.max_side_pilots|3}} or fewer pilots in
aircraft, the campaign flies for it. Every few minutes it can launch one of
that side's own **Fighters**, **Attackers** or **SEAD** actions, the same
flights you can buy from the Actions menu:

- **CAP** holds over the front line.
- **Strike** attacks the enemy objective nearest the front. It never goes
  after a SAM site or a carrier group.
- **SEAD** goes after the enemy SAM site covering that stretch of front, when
  there is one.

If one of your side's pilots is in the air, the package is sent to the part of
the front nearest them. That way the help shows up where you are flying.

A side can have up to {{cfg:air_life.packages.max_active_per_side|2}}
packages up at once, and launches at most one every
{{cfg:air_life.packages.launch_interval_secs|900}} seconds. Each package
stays on task for {{cfg:air_life.packages.lifetime_secs|1800}} seconds and
then flies home. Packages don't use up the action limits you buy against, and
they don't cost you points.

The other side gets the same help when it is short of pilots. A quiet night
means friendly AI **and** enemy AI in the air.

## Civil traffic

Neutral airliners fly across the map at cruise altitude, and some take off
from or land at airports well behind the front. They show up on your radar
like any other contact, and **nothing tells you they are civilian except
looking**. Identify before you shoot.

Airliners do not fight back or dodge, and no AI on either side engages them.
Only a player can shoot one down, and it is expensive:

- the pilot who shot it down loses
  {{cfg:air_life.civil_traffic.shootdown_penalty_points|500}} points;
- their side loses {{cfg:air_life.civil_traffic.shootdown_treasury_penalty|1000}}
  treasury;
- the whole server is told who did it.

Every airliner's name starts with `CIV` followed by its flight number (for
example `CIV THY1843`), and that is the name you will see in Tacview. A
civilian shoot-down never counts as a kill in your stats.

## Server setup

Everything lives under `air_life` in the engine config. Leave a part out to
switch it off.

**`wingman`** draws from `templates_red` / `templates_blue` (plane-section
groups) and `rotary_templates_red` / `rotary_templates_blue`
(helicopter-section groups). If a list is empty it falls back to the reactive
CAP and helo-patrol rosters under `campaign_events`, so a server that already
runs reactive CAP has jet wingmen with no extra setup.

**`packages`** uses the side's existing Fighters / Attackers / SEAD actions.
To use only some of them, name them in `actions_red` / `actions_blue`.
`treasury_cost` charges the side's treasury for each launch. `run_when_empty`
lets packages fly with nobody on the server; it is off by default, so an empty
server does not wear itself down overnight.

**`civil_traffic`** needs a **neutral country in the mission**. It uses the
first one the mission lists under the neutral coalition, unless you set
`country`. The default aircraft are the Yak-40, An-26B and IL-76MD. Each
entry in `aircraft` sets a DCS type, a cruise altitude band, a speed and,
optionally, liveries. Airports count as "rear", and so get arrivals and
departures, when they are at least
{{cfg:air_life.civil_traffic.rear_airport_min_front_m|60000}} m from the front.
