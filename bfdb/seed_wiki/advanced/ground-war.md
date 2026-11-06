# Ground War: Formations on the March

The front line is more than bases and the lines between them. Each side
sends **ground formations** out of its bases. They drive down the roads, fight
whatever they meet, break enemy bases and take them. An AI commander runs both
sides' ground war, and commanders on each side can take any formation over.

The ground war is switched on per server. Where it is off, there is no
**Ground Forces** entry in your F10 menu and no formations on the map, but
base garrisons still [defend themselves](#bases-defend-themselves).

![A formation raised at Senaki marches by road toward Gori as a column, deploys into line when it meets the enemy inside a battle ring, then sends its infantry in; the front line, drawn as a blue dashed edge, a white dotted centre and a red dashed edge, bulges toward Red ahead of it.](/api/wiki/images/92c63a2f-7f9e-4ed5-80a6-9d141fd07b1f)

## Where formations come from

Nothing is spawned out of thin air. A formation is a base's own garrison
(its armour, infantry and a little AAA) driving out of the gate. While it is
away, **that base is weaker**.

- A formation takes at most {{cfg:ground_war.groups_per_formation|3}}
  groups: infantry first, then armour.
- Every base keeps its last infantry group, and at least
  {{cfg:ground_war.keep_home_combat_groups|1}} combat group(s) in all, at
  home. A base with no infantry left could be walked into.
- A group that has lost half its vehicles stays home to be rebuilt.
- AAA goes along only when the base has at least two AAA groups: a base is
  never stripped of its only air defence.
- Only vehicles that can drive leave. Emplaced and towed guns (a KS-19, a
  ZU-23 emplacement, a mortar) stay at their base: DCS won't move them.
- A base that is under threat, still consolidating after a capture, or a
  carrier group can't raise one.

Each side can have up to {{cfg:ground_war.max_formations_per_side|6}}
formations in the field. On most servers there is a second, tighter limit:
no more than {{cfg:ground_war.max_live_formations|6}} formations, **both sides
together**, because every formation has to be able to be in DCS (see
[In DCS, or waiting](#in-dcs-or-waiting)).

Formations are named by number, kind and the base they came from, for
example "3rd Mech Coy (Senaki)". A *Mech Coy* has armour and infantry, an
*Armd Coy* is armour only, and an *Inf Coy* is infantry only. A server can
give each side its own name for them.

## How they move

On the march a formation is a **column**, nose to tail on the road, and it
drives as fast as its slowest vehicle: tanks are slower than trucks, and
infantry with nothing to ride in walks. At normal pace it drives at up to
{{cfg:ground_war.speed_kph|35}} km/h by road; slow pace is a little over half
that and fast pace about 1.65 times it, never faster than its slowest
vehicle. Across country it goes at 0.6 of its road speed. Where the road
would be a long detour, it drives straight across country instead.

A deployed formation advancing in contact crawls, and one that is out of fuel
and ammunition moves at half speed. A formation that gets stuck on the
terrain is re-routed a few times; if that fails it halts and your side is
told:

```dcsmessage
3rd Mech Coy (Senaki) is stuck on the terrain and has halted. Give it a new order.
```

Formations defending or falling back to one of your airfields hold off its
runways and taxiways, on the side they came from, so they never park a
column where aircraft need to take off.

## In DCS, or waiting

A formation is a real DCS group only while there is a reason for it to be:

- it is carrying out **a commander's order** (from the F10 menu or the
  dashboard), for as long as that order holds;
- a player is within {{cfg:ground_war.player_bubble_m|30000}} m of it;
- it is within {{cfg:ground_war.contact_m|10000}} m of an enemy formation, of
  **any enemy ground unit that is in DCS** (a supply convoy, deployed units,
  troops, an ambush or search party, a garrison), or of the base it is
  attacking;
- it is assaulting a base;
- it is on the move (on most servers: see below).

Up to {{cfg:ground_war.max_live_formations|6}} formations can be in DCS at
once, both sides together. When every slot is taken, a formation under a
commander's order, or one that needs to fight or drive, takes the place of
one that is only there because a player is near. A formation that is
fighting keeps its place. A formation nobody needs any more leaves DCS after
{{cfg:ground_war.despawn_grace_secs|300}} seconds.

**By default, everything happens in DCS.** A formation drives and fights
only as real DCS units, and DCS deals the losses. One that can't get a slot
waits where it is, and one that reaches a fight with no slot free stops at
its edge (**HALTED** on the dashboard, "[halted at contact]" in the status
report) until a slot opens. Some servers run the older hybrid mode instead,
where formations out of DCS keep driving and fighting on the campaign map by
the rules in [How they fight](#how-they-fight).

Either way, their vehicles are where the map says they are. Zoom in on the
dashboard and you see the column on the road, or the line it has deployed
into.

## How they fight

In DCS, the fighting is DCS's. The campaign adds the parts DCS doesn't
model, and on hybrid servers it also works out the fights that happen off
DCS by the same rules:

- **Firepower depends on what is fighting.** A tank counts for far more than
  a truck; IFVs and APCs are in between; infantry is hard to kill but hits
  little at range. The bigger, better-armed force wins, but slowly. Forces
  exchange fire within {{cfg:ground_war.combat.engage_m|3000}} m.
- **Columns get caught.** A column that runs into the enemy stops and takes
  {{cfg:ground_war.combat.deploy_secs|90}} seconds to deploy into line, and
  fights at a fraction of its power until it has. Deployed, it advances at a
  crawl while the enemy is near.
- **Defenders dig in.** A formation that holds a position for
  {{cfg:ground_war.combat.dig_in_secs|600}} seconds digs in and is much
  harder to hurt. A base's garrison always fights from cover.
- **Supply matters.** Fuel and ammunition run down on the march and in a
  fight; a formation with its own trucks lasts longer. They are made good
  only within {{cfg:ground_war.combat.supply_range_m|25000}} m of a friendly
  base that has at least {{cfg:ground_war.combat.supply_base_pct|25}}% supply
  of its own, **and the refill comes out of that base's warehouse**. The
  supply line has to be open: an enemy base astride the road, or an enemy
  formation within {{cfg:ground_war.combat.supply_line_cut_m|3000}} m of it,
  cuts it. A formation cut off crawls and fights at a fraction of its power,
  and an **encircled** one loses heart fast. The AI pulls formations back to
  resupply before they run dry. Cutting the enemy's supply lines is as good
  as beating them.
- **Artillery supports the fight.** Friendly batteries within range of an
  enemy your forces can see fire on it: as real fire missions on enemies
  that are in DCS, and as part of the fight on the map on hybrid servers.
- **Morale breaks.** Losses, and being cut off, wear a formation's morale
  down. Below {{cfg:ground_war.combat.break_morale|0.25}} it **breaks**: it
  falls back to its home base (or the nearest friendly one) whatever its
  orders, drops out of any commander's control, and won't take any order but
  withdraw until it rallies at half morale.
- **You only see what your forces see.** Enemy formations show up within
  {{cfg:ground_war.combat.spot_m|7000}} m of your own formations (three
  quarters of that from your bases, and less again if the enemy is standing
  still or dug in), but only where **the terrain doesn't block the view**.
  Pilots flying low enough to see the ground
  ({{cfg:ground_war.combat.air_spot_agl_m|3000}} m or less) spot them within
  {{cfg:ground_war.combat.air_spot_m|10000}} m, as do drones, JTACs and
  recon. Everyone sees less **at night** (about
  {{cfg:ground_war.combat.night_spot|0.45}} of the daytime range) and in
  **poor visibility**. Once out of sight, enemies are remembered where they
  were last seen, as they looked then, for
  {{cfg:ground_war.combat.remember_secs|900}} seconds.

In the field they're fair game. You'll find columns on the roads and fights
around contested bases: real targets for CAS, attack helicopters and
artillery. Recon flights see them too.

```dcsmessage
3rd Mech Coy (Senaki) has broken under fire and is falling back to Senaki.
```

## Bases defend themselves

A base's garrison doesn't sit and wait to be shot. This works whether or not
the ground war is on; only the quick reaction force needs it.

- **It comes out to fight.** An armed enemy vehicle, deployed unit or troops
  within about {{cfg:garrison.react_m|5000}} m of a live base sends the base's
  armour and infantry out to meet it, spread across its line of approach, up
  to {{cfg:garrison.leash_m|2500}} m past the base's edge. They go back to
  their positions once the ground has been clear for
  {{cfg:garrison.clear_secs|120}} seconds.
- **It calls for help.** An intruder still there after
  {{cfg:garrison.qrf_delay_secs|60}} s brings a **quick reaction force**: the
  nearest idle formation within {{cfg:garrison.qrf_range_m|40000}} m, or a
  new one raised at a neighbouring base. A side has at most
  {{cfg:garrison.qrf_max|2}} out at once.
- **It moves about.** While a live base is quiet, some of its armour shifts
  to new spots inside the base every
  {{cfg:garrison.patrol_secs|480}} seconds or so. Airfields are the
  exception: nothing patrols an airfield, and no ground unit is ever moved to
  within 150 m of a runway or taxiway, so aircraft can always take off and
  land.
- **Mobile SAM sites relocate.** An SA-6, SA-8, SA-11, Tor or NASAMS site
  moves somewhere else inside its base
  {{cfg:garrison.sam_scoot_secs|90}} seconds after it stops firing, and
  about every {{cfg:garrison.sam_redeploy_secs|2700}} seconds anyway, up to
  {{cfg:garrison.sam_move_m|1500}} m at a time. Where it ends up is kept, so
  a site is rarely where you saw it last round.

The defending side hears about it:

```dcsmessage
Gori: enemy armour at the perimeter, the garrison is moving to engage.
```

```dcsmessage
GROUND COMMAND: 4th Armd Coy (Kaspi) is the quick reaction force for Gori.
```

One tank parked outside a base can't take it apart on its own: bring
friends, or soften it up from the air first.

## Taking a base

A formation attacking a base has arrived once it is inside the base's zone.
Both sides are told:

```dcsmessage
3rd Mech Coy (Senaki) has reached Gori and is going in.
```

```dcsmessage
Enemy armour is assaulting Gori!
```

It fights the garrison, and the garrison's losses are the base's. Once the
base is broken (the same point at which troops could capture it, see
[Capturing Objectives](../gameplay/capturing-objectives.md)), the formation
**sends infantry in** as a capture squad: its infantry group if it has one,
or a squad that dismounts from one of its IFVs or APCs (the vehicle stays).
From there the normal capture timer and consolidation hold apply: kill the
squad and the capture fails. After an assault goes in, the next one at the
same base waits ten minutes.

```dcsmessage
3rd Mech Coy (Senaki)'s infantry is going in to take Gori. Cover them.
```

A formation with no infantry and no troop carriers left can break a base but
**can't take it**. Its side is told once:

```dcsmessage
Gori is broken, but 4th Armd Coy (Kutaisi) has no infantry or troop carriers left to take it. Send troops in.
```

When the base is yours, the formation stays to hold it:

```dcsmessage
3rd Mech Coy (Senaki) holds Gori.
```

## Losses and refitting

A formation that falls below
{{cfg:ground_war.withdraw_strength_pct|40}}% of its starting strength is
pulled back by the AI. When it reaches a friendly base (its home, or the
base it was sent back to) it rejoins that garrison: the survivors go back
to their posts, and the base rebuilds the dead through its normal repair and
reinforcement. A formation that is wiped out returns its wrecks to its home
base to be rebuilt the same way.

```dcsmessage
3rd Mech Coy (Senaki) has been destroyed.
```

The enemy hears "Enemy formation 3rd Mech Coy (Senaki) destroyed." Every
vehicle killed in DCS burns where it died for
{{cfg:ground_war.wreck_fire_secs|900}} seconds (at most
{{cfg:ground_war.max_wreck_fires|12}} fires at once).

A formation bought by a commander (see [Command](./command.md)) never had a
garrison: when it stands down, its survivors stay where they are as that
commander's deployed units.

## The AI commander

Every {{cfg:ground_war.ai.think_secs|300}} seconds each side's AI ground
commander, in order:

1. recalls formations that have wandered far beyond its reach (more than
   twice {{cfg:ground_war.ai.max_attack_m|60000}} m from where they are
   going);
2. pulls back formations that are too badly hurt to fight, and sends the
   ones running low on supply back within reach of a supplied base;
3. counter-attacks: sends the nearest idle formation (within 60 km) to a
   friendly base that is being captured, or that is under threat with enemy
   formations within 15 km, at most two a pass;
4. raises a new formation from the garrison nearest the front, while the
   side holds at least {{cfg:ground_war.ai.min_objectives|3}} bases;
5. during an offensive, sends idle formations at the enemy bases they are
   best placed to take, within {{cfg:ground_war.ai.max_attack_m|60000}} m,
   up to {{cfg:ground_war.ai.concentration|2}} at one base, keeping
   {{cfg:ground_war.ai.reserve|1}} back for counter-attacks. Formations that
   can take a base go first;
6. moves anything still idle up to the friendly base nearest the enemy.

What counts as an offensive: with the [Theatre HQ](./theatre-hq.md)
steering the ground war, the HQ's posture (anything but defensive) and its
main effort set the axis. Otherwise, with campaign tempo on (see
[Modern War](./modern-war.md)), only during your side's offensive phases,
aimed along its axis. Without either, always.

While no one is on the server, the AI starts no new attacks, so the map isn't
rolled up overnight. Attacks already under way carry on, and threatened bases
are still defended. The AI never touches a formation a commander is
directing.

Your side hears what it does:

```dcsmessage
GROUND COMMAND: 5th Mech Coy (Kutaisi) formed up at Kutaisi.
```

```dcsmessage
GROUND COMMAND: 2 formation(s) advancing on Gori.
```

```dcsmessage
GROUND COMMAND: counter-attacking at Senaki.
```

## The front line

Where a server draws it, the front line on the F10 map is three lines that
everyone sees, both sides:

| Line | What it is |
|---|---|
| Azure, dashed | The edge of Blue's ground |
| Violet, dashed | The edge of Red's ground |
| White, dotted | The middle of no man's land |

Every base pulls the line toward the enemy around it, the way a magnet
would: the more bases a side holds close together, the further its ground
reaches. Where the two sides' bases sit close, the three lines bunch into a
sharp front; where there is a wide contested band, they spread apart. Only
stretches that really run between a Blue and a Red base are drawn, so there
are no lines along empty coastline. Carrier groups don't count.

**What moves it:**

- **A base changing hands.** The line is redrawn at once.
- **Formations in the field.** Each one adds its weight
  ({{cfg:ground_war.frontline_weight|0.6}} of a base's, times how much of it
  is still alive) where it stands, so the line bulges ahead of a formation
  pushing into enemy ground and sags back where one is beaten. This redraw is
  held to once every {{cfg:ground_war.frontline_redraw_secs|900}} seconds at
  most, and only when a formation has moved enough to matter, because every
  redraw is a few hundred map commands shared with every other label on the
  map.

The dashboard's maps draw the same line from the same calculation, so the
two never disagree. See also [Reading the Map](../gameplay/reading-the-map.md).

## Commanding a formation yourself

**Orders are for commanders.** You become one by rank: command unlocks at
rank tier {{cfg:command.commander_rank|4}} (Major, a campaign score of 50 at
the default), the same score and ranks the leaderboard shows. An admin can
also make you a commander, or take it away. Anyone on your side can watch the
ground war and read the status report; only commanders can give orders or
raise formations. Some servers let anyone command. Commanders get that
server's **Commander** role on Discord (your Blue or Red role still shows
your side, server by server).

You can command from two places:

- in game, from **F10 → Ground Forces** (below);
- on the dashboard's **COMMAND** page ([Command](./command.md)), which also
  has Move with waypoints, rules of engagement, pace, and buying formations.

Your order puts the formation under your command for
{{cfg:ground_war.player_order_lock_secs|3600}} seconds. The AI won't touch it
until then, or until you hand it back. You can give one ground order every
{{cfg:ground_war.player_order_cooldown_secs|30}} seconds. Your whole side is
told who ordered what. While your order stands the formation is put into DCS,
so you can see it on the ground and fly over it.

Raising a formation costs {{cfg:ground_war.raise_cost|0}} of your points
(free unless the server sets a price). Orders are free.

### F10 → Ground Forces

The entry reads **Ground Forces>>** until you open it: opening it builds the
menu there and then, so it always shows the formations in the field right
now. Use **Refresh this menu** if you leave it open. Each formation's entry
shows what it is doing and how much of it is left. Attack lists the eight
enemy bases nearest the formation and Defend the eight friendly ones, with
the distance; Raise lists up to eight friendly bases near you (near your
side's first formation, if you are not in an aircraft) that have groups to
spare. Carrier groups never appear: nothing drives to sea.

```f10menu
{"root":"F10 Other","items":[
 {"label":"Ground Forces","note":"Shown as Ground Forces>> until you open it. Only on servers with the ground war on.","items":[
  {"label":"Status report","says":"GROUND FORCES (2 of {{cfg:ground_war.max_formations_per_side|6}}), 1 battle(s) on the map\n3rd Mech Coy (Senaki): attacking Gori, 14 km to go, 11/12 vehicles, under Viper\n4th Armd Coy (Kutaisi): holding position, 8/8 vehicles [halted at contact]","note":"Everyone on your side can read this, commander or not. With nothing out it reads 'No formations in the field.'"},
  {"label":"Refresh this menu","note":"Rebuilds the menu with the formations and bases as they are now. Prints nothing."},
  {"label":"3rd Mech Coy (Senaki) (moving, 92%)","items":[
   {"label":"Attack","items":[
    {"label":"Gori (14 km)","says":"3rd Mech Coy (Senaki): attacking Gori, 17 km by road. Yours for 60 min, then the AI takes it back.","note":"The minutes are this server's order lock ({{cfg:ground_war.player_order_lock_secs|3600}} seconds). The km is the route; 'across country' when there is no sensible road."},
    {"label":"Kaspi (26 km)","says":"3rd Mech Coy (Senaki): attacking Kaspi, 31 km by road. Yours for 60 min, then the AI takes it back."},
    {"label":"Tskhinvali (33 km)","says":"3rd Mech Coy (Senaki): attacking Tskhinvali, 40 km across country. Yours for 60 min, then the AI takes it back."}
   ]},
   {"label":"Defend / move to","items":[
    {"label":"Senaki (9 km)","says":"3rd Mech Coy (Senaki): defending Senaki, 11 km by road. Yours for 60 min, then the AI takes it back."},
    {"label":"Kutaisi (25 km)","says":"3rd Mech Coy (Senaki): defending Kutaisi, 29 km by road. Yours for 60 min, then the AI takes it back."}
   ]},
   {"label":"Hold position","says":"3rd Mech Coy (Senaki): holding position. Yours for 60 min, then the AI takes it back."},
   {"label":"Withdraw to Senaki and refit","says":"3rd Mech Coy (Senaki): withdrawing to Senaki, 11 km by road. Yours for 60 min, then the AI takes it back.","note":"Its home base while that is still yours, else your base nearest it. On arrival it rejoins that garrison."},
   {"label":"Hand back to AI command","says":"3rd Mech Coy (Senaki) is back under AI command."}
  ]},
  {"label":"4th Armd Coy (Kutaisi) (holding, 100%)","items":[
   {"label":"Attack","items":[
    {"label":"Gori (38 km)","says":"4th Armd Coy (Kutaisi): attacking Gori, 44 km by road. Yours for 60 min, then the AI takes it back."}
   ]},
   {"label":"Defend / move to","items":[
    {"label":"Kutaisi (1 km)","says":"4th Armd Coy (Kutaisi): defending Kutaisi, 1 km by road. Yours for 60 min, then the AI takes it back."}
   ]},
   {"label":"Hold position","says":"4th Armd Coy (Kutaisi): holding position. Yours for 60 min, then the AI takes it back."},
   {"label":"Withdraw to Kutaisi and refit","says":"4th Armd Coy (Kutaisi): withdrawing to Kutaisi, 1 km by road. Yours for 60 min, then the AI takes it back."},
   {"label":"Hand back to AI command","says":"4th Armd Coy (Kutaisi) is back under AI command."}
  ]},
  {"label":"Raise a formation","note":"Only when a base near you has groups to spare.","items":[
   {"label":"From Kutaisi (2 groups, 4 km)","says":"5th Mech Coy (Kutaisi) raised and awaiting orders.","note":"Your side hears 'Viper has raised 5th Mech Coy (Kutaisi). It awaits orders.' The new formation holds at its base until someone orders it."},
   {"label":"From Zestaponi (1 groups, 31 km)","says":"can't raise a formation: Zestaponi is under threat and can't spare its garrison."}
  ]}
 ]}
]}
```

Every order is also told to your whole side, with your name:

```dcsmessage
Viper: 3rd Mech Coy (Senaki): attacking Gori, 17 km by road
```

Orders made on the dashboard say "Viper (dashboard)". When an order can't be
carried out, the reply says why. The ones you will meet most:

| Reply | Why |
|---|---|
| orders need a commander: reach Major (a campaign score of 50), or ask an admin. | You are not a commander on this server (the rank named is this server's commander rank; Major at the default) |
| wait a moment before your next ground order. | Inside the {{cfg:ground_war.player_order_cooldown_secs|30}} s between orders |
| order refused: 3rd Mech Coy (Senaki) has broken and is falling back; it can only be ordered to withdraw. | Its morale broke |
| order refused: Gori is already ours. | Attack on a friendly base (Defend on an enemy one says "is not ours") |
| that formation is not ours. | Your side changed, or it was destroyed, since the menu was built: Refresh |
| can't raise a formation: Blue already has {{cfg:ground_war.max_formations_per_side|6}} formations in the field, the most it can have. | Your side is at its limit |
| can't raise a formation: the server can run no more than {{cfg:ground_war.max_live_formations|6}} formations in DCS at once. | Both sides together are at the live limit |
| can't raise a formation: Kutaisi is still consolidating. | Just captured |
| can't raise a formation: Kutaisi has no armour or infantry it can spare. | Everything left must stay home |
| raising a formation costs {{cfg:ground_war.raise_cost|0}} points. | The server charges for raising, and you can't pay |

Formations under orders also report their progress to your side:

```dcsmessage
4th Armd Coy (Kutaisi) is in position at Kutaisi.
```

## Reading the battlefield

### On the F10 map and in the world

```mapsymbols
[
 {"icon": "f10-pin", "name": "Formation pin (your side only)", "text": "One per formation in the field, in your side's colour: its name on the first line, then its orders and roughly how strong it is, in quarters (\"attacking Gori | ~75% strength\", \"defending Senaki\", \"holding\"). It follows the formation, redrawn at most every two minutes. The enemy never sees your pins.", "side": "Blue"},
 {"icon": "f10-attack-arrow", "name": "Arrow from a pin", "text": "The formation is attacking a base, and this is the way it is heading (up to 10 km long). Drawn only when the base is more than 3 km off; formations moving or defending get no arrow.", "side": "Blue"},
 {"icon": "f10-battle", "name": "Dashed orange ring, \"GROUND BATTLE near …\"", "text": "Formations are fighting here, or a formation is fighting a base's garrison. Everyone sees it, both sides: a battle is not a secret. It moves with the fight and stays five minutes after the last shot. This is where CAS and attack helicopters are wanted."},
 {"icon": "frontline", "name": "The front line", "text": "Azure and violet dashed lines for each side's edge, a white dotted line down the middle of no man's land. Bases set it; formations in the field bend it. See The front line above."},
 {"icon": "world-smoke", "name": "Smoke and fire on the ground (in the 3D world)", "text": "Not on the map: in the world. A battle being fought in DCS sends up a column of smoke you can see from the air long before you can see the vehicles, and every vehicle killed in it burns for a while. Follow the smoke."}
]
```

### On the dashboard: COMMAND

The dashboard draws the ground war as the battlefield your coalition can see.
It uses the dashboard's red and blue, not the F10 map's violet and azure. The
pictures below are as a Blue player sees them; on Red the colours swap, but
the shapes don't: **a rectangle is always ours, a diamond is always the
enemy**.

Zoomed out, each formation is one NATO symbol. Zoom in close
and the symbol breaks up into every vehicle in it, nose pointing the way it is
driving: a column on a road, a line deployed across a field.

#### Our formations

```mapsymbols
[
 {"icon": "gc-mechanised", "name": "Rectangle: one of our formations", "text": "NATO symbol, in our side's colour. A rectangle is always ours; the bar on top means company. Its short name (\"1 MECH\") is written beside it. What is inside the frame says what it is:"},
 {"icon": "gc-armour", "name": "Oval: armour", "text": "Tanks. Armd Coy."},
 {"icon": "gc-mechanised", "name": "Cross and oval: mechanised infantry", "text": "Infantry riding in IFVs or APCs, with armour. Mech Coy: the one that can take a base."},
 {"icon": "gc-motorised", "name": "Cross and upright line: motorised infantry", "text": "Infantry in trucks."},
 {"icon": "gc-infantry", "name": "Cross: infantry", "text": "Infantry on foot. Hard to kill, slow, little punch at range. Inf Coy."},
 {"icon": "gc-moving", "name": "Staff with an arrow under it", "text": "The formation is on the move, and the arrow points the way it is heading. No staff: it has stopped."},
 {"icon": "gc-reduced", "name": "(-) at the corner", "text": "It has lost vehicles: it is not at the strength it set out with."},
 {"icon": "gc-strength", "name": "Bar on the left", "text": "How much of the formation is still alive. Green from 70%, yellow from 40%, red below that. Below {{cfg:ground_war.withdraw_strength_pct|40}}% the AI pulls it back to refit."},
 {"icon": "gc-flag-broken", "name": "Grey symbol, BROKEN", "text": "Its morale broke. It is falling back to its home base (or the nearest friendly one) whatever its orders, and will take no order but withdraw until it rallies."},
 {"icon": "gc-flag-nosupply", "name": "NO SUPPLY", "text": "Its supply line is cut: no friendly base with supply in range, or the road to one is blocked. It crawls and fights at a fraction of its power. Open the road or pull it back."},
 {"icon": "gc-flag-halted", "name": "HALTED", "text": "It has reached a fight but every DCS slot is taken, so it waits at the edge of the fight until one frees up. Bring a player near it or give it an order and it gets a slot first."},
 {"icon": "gc-flag-sim", "name": "SIM", "text": "It is not in DCS right now: on the campaign map only. On most servers that means it is parked, waiting for a reason to be in DCS; on hybrid servers it moves and fights on the map by the same rules. It turns real when someone comes within {{cfg:ground_war.player_bubble_m|30000}} m, it is given an order, or it meets the enemy. IN DCS in the detail panel means it is real."},
 {"icon": "gc-selected", "name": "Yellow brackets and a dotted ring", "text": "Selected. The dotted ring is how far it can engage ({{cfg:ground_war.combat.engage_m|3000}} m). Yellow is always \"what you are about to order\"."},
 {"icon": "gc-group", "name": "Yellow number", "text": "The control group it is in (Ctrl+1 to set, 1 to select), as in any RTS."}
]
```

#### The enemy

```mapsymbols
[
 {"icon": "gc-hostile", "name": "Diamond: an enemy formation, in sight", "text": "A diamond is always the enemy, in their colour. \"~12 VEH\" is about how many vehicles we can see; the icon inside is what it looks like it is, the same as ours."},
 {"icon": "gc-ghost", "name": "Faded, dashed diamond: last seen", "text": "We can't see it any more. It is drawn where it was last seen, as it was then, with how long ago, and fades out. It has almost certainly moved: treat it as a lead, not a target."},
 {"icon": "gc-fog", "name": "Dark shading: what we can't see", "text": "Turn on FOG in the top bar. The clear patches are what our formations, bases and pilots can see; anything could be in the dark. Enemies only ever appear inside the clear parts."}
]
```

#### Vehicles, up close

Top-down silhouettes, in their side's colour, drawn where the vehicles really
are.

```mapsymbols
[
 {"icon": "gc-v-tank", "name": "Tank", "text": "Big hull, wide tracks, turret, long gun. The most firepower and the hardest to kill."},
 {"icon": "gc-v-ifv", "name": "IFV", "text": "Tracks, small turret, short cannon. Carries infantry and fights."},
 {"icon": "gc-v-apc", "name": "APC", "text": "Wedge nose, wheels down both sides, no turret. Carries infantry; little firepower."},
 {"icon": "gc-v-recon", "name": "Recon", "text": "Small four-wheeler with a gun. Sees further than it fights."},
 {"icon": "gc-v-truck", "name": "Truck", "text": "Cab and a slatted load bed. Supply: a formation with trucks lasts longer between resupplies. Soft."},
 {"icon": "gc-v-aaa", "name": "AAA", "text": "Twin barrels and a radar dish at the back. Air cover for the formation."},
 {"icon": "gc-v-sam", "name": "SAM", "text": "Four missile tubes on the deck."},
 {"icon": "gc-v-artillery", "name": "Artillery", "text": "Long barrel over a short hull. Fires from well behind the fight."},
 {"icon": "gc-v-infantry", "name": "Infantry", "text": "Four dots: a squad on foot."},
 {"icon": "gc-v-broken", "name": "Grey vehicle", "text": "Belongs to a broken formation."}
]
```

#### Orders and movement

```mapsymbols
[
 {"icon": "gc-route-attack", "name": "Red route, chevrons at the end", "text": "Attacking: going for the base at the end. Selected, the box also shows the distance left and the ETA."},
 {"icon": "gc-route-move", "name": "Route in our colour, shield at the end", "text": "Defending or moving to a friendly base."},
 {"icon": "gc-route-withdraw", "name": "Yellow route, back arrow", "text": "Withdrawing to refit."},
 {"icon": "gc-trail", "name": "Twin fading lines behind a column", "text": "Its tracks: where it has driven, fading with age."},
 {"icon": "gc-front", "name": "Dashed lines across the map", "text": "The front: each side's line in its colour, with no man's land between, in pale. Formations in the field push it. TERR in the top bar turns it and the territory wash on and off."}
]
```

#### Bases

```mapsymbols
[
 {"icon": "gc-base", "name": "Six-sided plate: a base", "text": "Edged in its owner's colour, with an icon for the kind of base. On our bases: the bar under it is the supply in its warehouse (red under 30%, yellow under 60%), each square is a garrison group, and +2 is how many formations it could raise right now."},
 {"icon": "gc-base-threat", "name": "Red \"!\"", "text": "Enemy forces are close to this base."},
 {"icon": "gc-base-capturing", "name": "Red ring pulsing round a base", "text": "It is being captured. On an enemy base, those are our troops going in; on one of ours, theirs. Go and kill the capture squad."},
 {"icon": "gc-base-enemy", "name": "Enemy base", "text": "Theirs: no supply bar or garrison count, because we can't see their stocks."}
]
```

#### Fighting

```mapsymbols
[
 {"icon": "gc-battle", "name": "Orange glow, \"⚔ GORI\"", "text": "A battle. The flashes are guns firing, and the glow is hotter the harder the fighting is. The label gives losses on each side; it flickers while the battle is live in DCS."},
 {"icon": "gc-fireline", "name": "Tracer lines between formations", "text": "Who is shooting at whom, in each side's colour."},
 {"icon": "gc-shellfire", "name": "Bursts, smoke and scorch marks", "text": "Artillery landing and the smoke it leaves. The scorch marks stay a while after the smoke has gone."}
]
```

#### Pilots

```mapsymbols
[
 {"icon": "gc-player", "name": "Aircraft in our colour", "text": "One of our pilots, in real time, pointing the way they are flying. The tag gives their name, airframe, altitude and speed."},
 {"icon": "gc-player-helo", "name": "Helicopter (dashed rotor disc)", "text": "A helicopter pilot."},
 {"icon": "gc-you", "name": "Yellow aircraft with a ring: YOU", "text": "You, if you are in a slot on the server right now. Press F to follow yourself."}
]
```

#### The combat log

The dashboard's log is your side's running story of the ground war: who
ordered what, formations formed up, contacts ("Enemy formation spotted near
Gori"), "has made contact near Gori and is deploying", "has dug in", losses
and kills, supply cut and restored ("is encircled: its supply line is cut",
"is back in supply"), breaks and rallies, and artillery firing in support.
Click a line to fly the map to where it happened.
