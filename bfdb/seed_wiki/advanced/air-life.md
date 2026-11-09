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

Each reply appears in your message box a second after you click (the request
is handled on the next server tick).

```f10menu
{"root":"F10 Other","items":[
 {"label":"GCI/EWR","note":"See the GCI/EWR menu page."},
 {"label":"Wingman","items":[
  {"label":"Request","note":"What you can call, each with its price. Only what your aircraft can fly with: jets see the jet types, helicopters the helicopter ones.","items":[
   {"label":"Fighter escort (300 pts)","says":"Fighter escort: a F-16C bl.50 (fighter) is starting up at Ramat David (48 km from you), 300 points. It takes off and joins on your wing, and stays for 60 min, or until you land or release it. F10 > Wingman > Orders to direct it."},
   {"label":"CAS pair (400 pts)","note":"Types and prices are the server's. A server with no types offers one 'Standard wingman'."},
   {"label":"SEAD pair (500 pts)"},
   {"label":"Copy of my F-16C_50 (400 pts)","note":"A copy of your own aircraft, with its slot's loadout and paint. Only on servers that offer it, and not for every type."}
  ]},
  {"label":"Orders","items":[
   {"label":"Cover me / rejoin","says":"Wingman: rejoining, covering you."},
   {"label":"Engage air threats around me","says":"Wingman: engaging air threats around you."},
   {"label":"Attack ground targets near me","says":"Wingman: attacking targets within 10 km of you."},
   {"label":"Attack my latest map mark","says":"Wingman: on the way to your mark, 23 km out.","note":"Drop a mark on the F10 map first; it goes for your newest one."},
   {"label":"Hold here (orbit)","says":"Wingman: holding here."},
   {"label":"Weapons free","says":"Wingman: weapons free, copy."},
   {"label":"Return fire only","says":"Wingman: return fire, copy."},
   {"label":"Weapons hold","says":"Wingman: weapons hold, copy."},
   {"label":"Formation: close","says":"Wingman: close formation."},
   {"label":"Formation: spread","says":"Wingman: spread formation."}
  ]},
  {"label":"Status","says":"Fighter escort (F-16C bl.50, fighter): 1/1 aircraft, 3.4 nm from you, covering you, weapons free, close formation, 52 min left."},
  {"label":"Release (RTB)","says":"Fighter escort released, RTB.","note":"With no wingman up: 'You have no wingman.'"}
 ]},
 {"label":"JTAC","note":"See the JTAC page."},
 {"label":"Actions","note":"See the Actions page."},
 {"label":"Objectives","note":"See the Objectives menu page."},
 {"label":"Info","note":"See the Info menu page."}
]}
```

The server's replies still say "F10 > Wingman > ..." when the menu sits
under Flight.

## Calling a wingman

**Request** lists the wingmen you can call, each with its price. Pick one and
an AI flight starts up at the nearest friendly field that can launch it (a
jet needs free parking at an airfield, a helicopter any airbase, FARP or
FOB), takes off and flies out to where you are. You are told where it is
coming from and how far away that is. Once it reaches you it covers you: DCS's
Escort task, holding station behind and off your right wing, going after
threats and coming back to your wing afterwards.

What it goes after depends on what you called:

| Kind | Covers you against | "Attack" means |
|---|---|---|
| Fighter | aircraft | aircraft |
| Strike, CAS | aircraft | ground units |
| SEAD | air defences | air defences |
| Attack helicopter | helicopters and ground units | ground units and helicopters |
| Copy of your aircraft | aircraft (a helicopter: helicopters and ground units) | ground units |

- **A copy of your aircraft** is your own type, with the loadout and paint
  your slot is briefed with, not whatever you have rearmed to since.
  Transports can't be copied, and a server can price types differently.
- **Call it once you are airborne.** On the ground the request is refused.
  Allow for the start-up and the flight out: call it before you need it.
- A fixed-wing wingman covers out to
  {{cfg:air_life.wingman.engage_dist_m|40000}} m from you, a helicopter
  {{cfg:air_life.wingman.rotary_engage_dist_m|8000}} m.
- **It never runs out of fuel**, starts weapons free and defends itself when
  shot at.
- **Cost:** the price in the menu, taken only once it is actually on its
  way. A wingman that can't launch costs nothing; one you release early or
  lose is not refunded.
- **Quiet servers:** a server can offer wingmen only while your side has
  {{cfg:air_life.wingman.max_side_pilots|4}} or fewer pilots in aircraft
  (0 means whatever the count).
- **One at a time.** A second request while yours is up is refused.

## Giving orders

**Orders** are free and take effect at once:

- **Cover me / rejoin** brings it back to your wing after any other order.
- **Engage air threats around me** has it circle where you are now and fight
  any aircraft within its cover range.
- **Attack ground targets near me** has it circle where you are now and
  attack what its kind attacks (table above) within 10 km (4 km for a
  helicopter).
- **Attack my latest map mark**: drop a mark on the F10 map, then order it.
  It flies there and attacks around the mark. It goes for your newest mark.
- **Hold here** parks it in an orbit where you are now, still defending
  itself and the area.
- **Weapons free / Return fire only / Weapons hold** set its rules of
  engagement. Weapons hold keeps it from starting a fight you don't want.
- **Formation: close / spread** moves its station on your wing: spread is
  wider and higher, for when you need room to manoeuvre.

Until it is off the ground only the ROE and formation orders work; the rest
are refused with `Your wingman is still taking off.`

It goes home by itself when you:

- land and stay down for a minute,
- leave the slot, change aircraft or die,
- release it (**Release (RTB)**), or
- reach its limit of
  {{cfg:air_life.wingman.lifetime_secs|3600}} seconds.

A released or recalled wingman flies to the nearest friendly airbase and is
removed once it lands (or after 20 minutes, wherever it is).

If it is shot down, you have to wait
{{cfg:air_life.wingman.cooldown_secs|300}} seconds before calling another.
**Status** tells you what you called, how many of its aircraft are left, how
far away it is, what it is doing, its ROE and formation, and how many minutes
it has left.

### What the wingman tells you

These arrive in your message box without a click:

```dcsmessage
Your wingman (Fighter escort) is down.
```

```dcsmessage
Your wingman (Fighter escort) is RTB: you are on the ground.
```

```dcsmessage
Your wingman (Fighter escort) is RTB: it is out of time.
```

### When a request is refused

| Reply | Why |
|---|---|
| `Wingmen are not enabled on this server.` | The server has no wingman config. |
| `You already have a wingman (Fighter escort). Release it first (F10 > Wingman > Release).` | One wingman per pilot. |
| `Your last wingman was lost. A new one is available in 4m 12s.` | The cooldown after losing one. |
| `Get in an aircraft first.` | You are not in a slot. |
| `Call your wingman once you are airborne -- it joins you in the air.` | You are on the ground. |
| `Wingmen are for quiet servers: available while your side has 4 or fewer pilots up (it has 6).` | Your side is busy enough. |
| `CAS pair costs 400 points and you have 150.` | Not enough points. |
| `A C-130J-30 can't be copied on this server.` | That type is excluded from copies. |
| `No aircraft is configured for CAS pair on your side.` | No template for your side. |
| `No friendly airfield near you has room to launch a wingman right now.` | No field with free parking (helicopters: `airbase, FARP or FOB`). |
| `Drop a mark on the F10 map first: your wingman attacks around your newest one.` | Attack my latest map mark, with no mark of yours on the map. |

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
