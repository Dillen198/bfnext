# Wingmen, AI Packages & Civil Traffic

A quiet server should not mean an empty sky. Three things keep the air busy
when there are only a few of you on: a wingman you can call, AI flights that
fight for a side short of pilots, and neutral airliners and merchant ships
going about their business.

Each one is switched on separately per server, so not every server runs all
three.

## The Wingman menu

When wingmen are on, every aircraft slot gets a **Wingman** menu in F10. If
your aircraft can also fly a [Recon Pass](../f10-menu/recon.md), the two share
one **Flight** menu instead (to save a slot on the F10 root, where DCS shows
only ten), so the path becomes **F10 > Flight > Wingman**.

The menu has three commands. Each reply appears in your message box a second
after you click (the request is handled on the next server tick).

```f10menu
{"root":"F10 Other","items":[
 {"label":"GCI/EWR","note":"See the GCI/EWR menu page."},
 {"label":"Wingman","items":[
  {"label":"Request Wingman","says":"Your F-16C_50 wingman is starting up at Kutaisi (48 km from you). It takes off and joins on your wing, and stays for 60 min, or until you land or release it (F10 > Wingman > Release Wingman).","note":"The minutes are this server's wingman lifetime ({{cfg:air_life.wingman.lifetime_secs|3600}} seconds). Refused on the ground: 'Call your wingman once you are airborne -- it joins you in the air.'"},
  {"label":"Release Wingman","says":"F-16C_50 wingman released, RTB.","note":"With no wingman up: 'You have no wingman.'"},
  {"label":"Wingman Status","says":"F-16C_50 wingman: 2/2 aircraft, 3.4 nm from you, 52 min left.","note":"With no wingman up: 'You have no wingman. F10 > Wingman > Request Wingman calls one.'"}
 ]},
 {"label":"JTAC","note":"See the JTAC page."},
 {"label":"Actions","note":"See the Actions page."},
 {"label":"Objectives","note":"See the Objectives menu page."},
 {"label":"Info","note":"See the Info menu page."}
]}
```

On a slot that can also fly recon:

```f10menu
{"root":"F10 Other","items":[
 {"label":"GCI/EWR","note":"See the GCI/EWR menu page."},
 {"label":"Flight","items":[
  {"label":"Recon","items":[
   {"label":"Start Recon Pass","note":"See the Recon page."},
   {"label":"Cancel Recon Pass","note":"See the Recon page."},
   {"label":"Recon Status","note":"See the Recon page."}
  ]},
  {"label":"Wingman","items":[
   {"label":"Request Wingman","says":"Your AH-64D_BLK_II wingman is starting up at FARP London (12 km from you). It takes off and joins on your wing, and stays for 60 min, or until you land or release it (F10 > Wingman > Release Wingman).","note":"A helicopter gets a helicopter wingman, launched from the nearest friendly airbase, FARP or FOB."},
   {"label":"Release Wingman","says":"AH-64D_BLK_II wingman released, RTB."},
   {"label":"Wingman Status","says":"AH-64D_BLK_II wingman: 2/2 aircraft, 0.8 nm from you, 41 min left."}
  ]}
 ]},
 {"label":"Objectives","note":"See the Objectives menu page."},
 {"label":"Info","note":"See the Info menu page."}
]}
```

The server's replies still say "F10 > Wingman > ..." when the menu sits
under Flight.

## Calling a wingman

**Request Wingman** sends an AI flight up to join you. It starts up at the
nearest friendly field that can launch it (a jet needs free parking at an
airfield, a helicopter any airbase, FARP or FOB), takes off and flies out to
where you are. You are told where it is coming from and how far away that is.
Once it reaches you it flies DCS's Escort task on your aircraft: it holds
station behind and off your right wing, goes after threats, and comes back to
your wing afterwards.

- **Call it once you are airborne.** On the ground the request is refused.
  Allow for the start-up and the flight out: call it before you need it.
- **Jets get a jet, helicopters get a helicopter.** A fixed-wing wingman
  engages aircraft within
  {{cfg:air_life.wingman.engage_dist_m|40000}} m of you. A helicopter
  wingman stays closer
  ({{cfg:air_life.wingman.rotary_engage_dist_m|8000}} m) and engages
  helicopters **and ground units**, so it will shoot at whatever is shooting
  at your LZ.
- **It never runs out of fuel** and is weapons free. It defends itself when
  shot at.
- **It is for quiet servers.** You can only call one while your side has
  {{cfg:air_life.wingman.max_side_pilots|4}} or fewer pilots in aircraft
  (a server that sets this to 0 offers wingmen whatever the count).
- **Cost:** {{cfg:air_life.wingman.cost|0}} points, taken when it launches.
- **One at a time.** A second request while yours is up is refused.

It goes home by itself when you:

- land and stay down for a minute,
- leave the slot, change aircraft or die,
- release it (**Release Wingman**), or
- reach its limit of
  {{cfg:air_life.wingman.lifetime_secs|3600}} seconds.

A released or recalled wingman flies to the nearest friendly airbase and is
removed once it lands (or after 20 minutes, wherever it is).

If it is shot down, you have to wait
{{cfg:air_life.wingman.cooldown_secs|300}} seconds before calling another.
**Wingman Status** tells you how many of its aircraft are left, how far away it
is in nautical miles, and how many minutes it has left.

### What the wingman tells you

These arrive in your message box without a click:

```dcsmessage
Your F-16C_50 wingman is down.
```

```dcsmessage
Your F-16C_50 wingman is RTB: you are on the ground.
```

```dcsmessage
Your F-16C_50 wingman is RTB: it is out of time.
```

### When a request is refused

| Reply | Why |
|---|---|
| `Wingmen are not enabled on this server.` | The server has no wingman config. |
| `You already have a F-16C_50 wingman. Release it first (F10 > Wingman > Release Wingman).` | One wingman per pilot. |
| `Your last wingman was lost. A new one is available in 4m 12s.` | The cooldown after losing one. |
| `Get in an aircraft first.` | You are not in a slot. |
| `Call your wingman once you are airborne -- it joins you in the air.` | You are on the ground. |
| `Wingmen are for quiet servers: available while your side has {{cfg:air_life.wingman.max_side_pilots|4}} or fewer pilots up (it has 6).` | Your side is busy enough. |
| `A wingman costs {{cfg:air_life.wingman.cost|0}} points and you have 150.` | Only on a server that charges for wingmen. |
| `No helicopter wingman is configured for your side.` | No template for your airframe class. |
| `No friendly airfield near you has room to launch a wingman right now.` | No field with free parking (helicopters: `airbase, FARP or FOB`). |

## AI packages

When a side has {{cfg:air_life.packages.max_side_pilots|3}} or fewer pilots in
aircraft, the campaign flies for it. Every
{{cfg:air_life.packages.check_interval_secs|120}} seconds it can launch one of
that side's own **Fighters**, **Attackers** or **SEAD** actions, the same
flights you can buy from the [Actions menu](../f10-menu/actions.md):

- **CAP** holds over the front line, halfway between the two sides' closest
  objectives.
- **Strike** attacks the enemy objective nearest that stretch of front. It
  never goes after a SAM site or a carrier group.
- **SEAD** goes after the enemy SAM site covering that stretch of front (within
  60 km), when there is one.

Strike is picked most often, then CAP, then SEAD. Targets more than
{{cfg:air_life.packages.max_target_range_m|150000}} m from every friendly
airbase are skipped.

Where on the front? If one of your side's pilots is in the air, the package is
sent to the part of the front nearest them, so the help shows up where you are
flying. With nobody up it follows the side's offensive axis (see
[Offensives and pauses](./modern-war.md#offensives-and-pauses)), or a random
one of the tightest stretches of front.

```dcsmessage
Friendly AI Strike flight launching, tasked over Gudauta.
```

Every package starts on the ground at a friendly airfield and takes off;
nothing appears in the air. On a server running the
[Theatre HQ](./theatre-hq.md), the HQ flies the air war instead and these
packages stand down.

A side can have up to {{cfg:air_life.packages.max_active_per_side|2}}
packages up at once, and launches at most one every
{{cfg:air_life.packages.launch_interval_secs|900}} seconds (faster during an
offensive, slower while regrouping). Each package stays on task for
{{cfg:air_life.packages.lifetime_secs|1800}} seconds and then flies home.
Packages don't use up the action limits you buy against, and they don't cost
you points. A server can charge the side's treasury
{{cfg:air_life.packages.treasury_cost|0}} per launch.

The other side gets the same help when it is short of pilots. A quiet night
means friendly AI **and** enemy AI in the air.

## Civil traffic

Neutral airliners fly across the map at cruise altitude, and some take off
from or land at airports well behind the front (at least
{{cfg:air_life.civil_traffic.rear_airport_min_front_m|60000}} m from it).
Overflights keep {{cfg:air_life.civil_traffic.front_standoff_m|40000}} m from
the front where they can. Up to
{{cfg:air_life.civil_traffic.max_active|4}} are in the air at once, and new
ones (airliners and ships) only appear while at least
{{cfg:air_life.civil_traffic.min_players|1}} player is in an aircraft.

Merchant ships do the same at sea: up to
{{cfg:air_life.civil_traffic.max_ships|3}} at a time, on open-water lanes
across the map.

They show up on your radar like any other contact, and **nothing tells you they
are civilian except looking**. Identify before you shoot.

They do not fight back or dodge, and no AI on either side engages them. Only a
player can shoot one down, and it is expensive:

- the pilot who shot it down loses
  {{cfg:air_life.civil_traffic.shootdown_penalty_points|500}} points;
- their side loses {{cfg:air_life.civil_traffic.shootdown_treasury_penalty|1000}}
  treasury;
- the whole server is told who did it.

```dcsmessage
CIVILIAN AIRLINER DOWN: THY1843 (IL-76MD) was shot down by Maverick (Blue). Blue pays {{cfg:air_life.civil_traffic.shootdown_treasury_penalty|1000}} from its treasury.
```

```dcsmessage
CIVILIAN SHIP SUNK: MV Black Sea Carrier 42 (Dry-cargo ship-1) was sunk by Ghost (Red). Red pays {{cfg:air_life.civil_traffic.shootdown_treasury_penalty|1000}} from its treasury.
```

The name is the shooter's player name. On a server with no treasury penalty
the second sentence is left off. You are charged
if your weapon hit it within the three minutes before it went down. An
airliner that crashes on its own costs nobody anything.

Every airliner's name starts with `CIV` followed by its flight number (for
example `CIV THY1843`), and every merchant ship's with `CIV MV` and a ship name
(`CIV MV Odessa Star 17`). That is the name you will see in Tacview. A civilian
shoot-down never counts as a kill in your stats.

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
`country`. The default aircraft are the Yak-40, An-26B and IL-76MD, and the
default ships are dry-cargo ships, a bulk carrier and two tankers. Each entry
in `aircraft` sets a DCS type, a cruise altitude band, a speed and,
optionally, liveries; `ships` takes a DCS type, a weight and a speed. An empty
`ships` list means no merchant shipping.

## See also

- [AI Opposition](./ai-opposition.md): the enemy fighters and helicopters
  that scramble against you
- [Modern War](./modern-war.md): raids, jammers and the offensive cycle that
  speeds packages up
- [Theatre HQ](./theatre-hq.md)
