# Ground War: Formations on the March

The front line is no longer just bases and the lines between them. Each side
sends **ground formations** out of its bases. They drive down the roads, fight
whatever they meet, break enemy bases and take them. An AI commander runs both
sides' ground war, and any player can take command of a formation.

## Where formations come from

Nothing is spawned out of thin air. A formation is a base's own garrison
(its armour, infantry and a little AAA) driving out of the gate. While it is
away, **that base is weaker**. Every base keeps its last infantry group and
at least {{cfg:ground_war.keep_home_combat_groups|1}} combat group(s) at home,
and a formation takes at most {{cfg:ground_war.groups_per_formation|3}} groups.

Each side can have up to {{cfg:ground_war.max_formations_per_side|6}}
formations in the field. They are named for where they came from, for example
"3rd Mech Coy (Senaki)". A *Mech Coy* has armour and infantry, an *Armd Coy*
is armour only, and an *Inf Coy* is infantry only.

## How they move

A formation drives as fast as its slowest vehicle: tanks are slower than
trucks, and infantry with nothing to ride in walks. On the road it is capped
at {{cfg:ground_war.speed_kph|30}} km/h, and across country it goes about half
that. On the march it is a **column** nose to tail on the road. It burns fuel
as it goes.

They only become real DCS units where it matters:

- while they are carrying out **a player's order** (from the F10 menu or the
  dashboard), for as long as that order holds;
- when a player is within {{cfg:ground_war.player_bubble_m|30000}} m of them;
- when they meet an enemy formation, come within
  {{cfg:ground_war.contact_m|10000}} m of **any enemy ground unit that is in
  DCS** (a supply convoy, deployed units, troops, an ambush or search party, a
  garrison), or close to within that of the base they are attacking.

Up to {{cfg:ground_war.max_live_formations|6}} formations can be in DCS at
once. A formation under a player's order takes the place of one that is only
there because a player is near; a formation that is fighting keeps its place.

Away from players they move on the campaign map only, which keeps the server
fast. Either way their vehicles are where the map says they are: zoom in on
the dashboard and you see the column on the road, or the line it has
deployed into.

## How they fight

Off the server's live budget, fighting is worked out on the map, and it
follows the same rules a real fight would:

- **Firepower depends on what is fighting.** A tank counts for far more than
  a truck; IFVs and APCs are in between; infantry is hard to kill but hits
  little at range. The bigger, better-armed force wins, but slowly.
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
  base that has supply of its own, **and the refill comes out of that base's
  warehouse**. The supply line has to be open: an enemy base astride the road,
  or an enemy formation within {{cfg:ground_war.combat.supply_line_cut_m|3000}}
  m of it, cuts it. A formation cut off crawls and fights at a fraction of
  its power, and an **encircled** one loses heart fast. The AI pulls
  formations back to resupply before they run dry. Cutting the enemy's
  supply lines is as good as beating them.
- **Artillery supports the fight.** Friendly batteries within range of an
  enemy your forces can see fire on it: as part of the fight on the map, and
  as real fire missions on enemies that are live in DCS.
- **Morale breaks.** Losses, and being cut off, wear a formation's morale
  down. Below {{cfg:ground_war.combat.break_morale|0.25}} it **breaks**: it
  falls back to the nearest friendly base whatever its orders, and won't take
  any order but withdraw until it rallies.
- **You only see what your forces see.** Enemy formations show up within
  {{cfg:ground_war.combat.spot_m|7000}} m of your own formations (less from
  your bases, and less again if the enemy is standing still or dug in), but
  only where **the terrain doesn't block the view**. Pilots flying low enough
  to see the ground ({{cfg:ground_war.combat.air_spot_agl_m|3000}} m or less)
  spot them within {{cfg:ground_war.combat.air_spot_m|10000}} m, as do drones,
  JTACs and recon. Everyone sees less **at night** and in **poor
  visibility**. Once out of sight, enemies are remembered where they were last
  seen, as they looked then.

In the field they're fair game. You'll find columns on the roads and fights
around contested bases: real targets for CAS, attack helicopters and
artillery. Recon flights see them too.

## Taking a base

A formation attacking a base fights its garrison, and the garrison's losses
are the base's. Once the base is broken
(the same point at which troops could capture it), the formation's
**infantry dismounts and goes in** as a capture squad. From there the normal
capture timer and consolidation hold apply: kill the squad and the capture
fails.

A formation with no infantry left can break a base but **can't take it**.
Its side is told to send troops in.

## Losses and refitting

A formation that falls below {{cfg:ground_war.withdraw_strength_pct|40}}% of
its starting strength is pulled back. When it reaches a friendly base it
rejoins that garrison. The survivors go back to their posts, and the base
rebuilds the dead through its normal repair and reinforcement. A formation
that is wiped out returns its wrecks to its home base to be rebuilt the same
way.

## The AI commander

Every few minutes each side's AI commander:

1. pulls back formations that are too badly hurt to fight;
2. counter-attacks friendly bases that are being captured, or that have enemy
   armour closing in;
3. raises a new formation from the garrison nearest the front;
4. during an offensive, sends formations at the enemy base it is best placed
   to take, aiming along the offensive's axis when campaign tempo is on;
5. moves anything still idle up to the friendly base nearest the enemy.

While no one is on the server, the AI starts no new attacks, so the map isn't
rolled up overnight.

## Commanding a formation yourself

**Orders are for commanders.** You become one by rank: command unlocks at
rank tier {{cfg:command.commander_rank|4}} (Major, a campaign score of 50 at
the default), the same score and ranks the leaderboard shows. An admin can
also make you a commander, or take it away. Anyone on your side can watch the
ground war and read the status report; only commanders can give orders, raise
formations or take over the HQ. Commanders get the **Blue Commander** or
**Red Commander** role on Discord.

The dashboard's **GROUND COMMAND** page is the battlefield your coalition
sees, live: your formations and every vehicle in them, enemies where you have
spotted them, battles, and your side's pilots in the air. Your own aircraft
is marked **YOU**. Select formations with a click, Shift+click or Shift+drag,
then right-click a base to attack or defend it. **A**, **D**, **H**, **W** and
**R** are attack, defend, hold, withdraw and raise; press **?** for every key.

In game, open **F10 → Ground Forces**. The menu is built when you open it, so it
always shows the formations in the field right now. Use **Refresh** if you
leave it open.

- **Status report**: every formation on your side, its orders and strength.
- **(a formation) → Attack**: the enemy bases nearest it.
- **(a formation) → Defend / move to**: the friendly bases nearest it.
- **Hold position**, **Withdraw to … and refit**, **Hand back to AI command**.
- **Raise a formation**: form a new one from a friendly base near you.

Your order puts the formation under your command for
{{cfg:ground_war.player_order_lock_secs|3600}} seconds. The AI won't touch it
until then, or until you hand it back. Your whole side is told who ordered
what. While your order stands the formation is put into DCS, so you can see it
on the ground and fly over it.

Formations are made only of vehicles that can drive. Emplaced and towed guns
(a KS-19, a ZU-23 emplacement, a mortar) stay at their base: DCS won't move
them, whatever they are told.

Each formation has a pin on your side's F10 map showing its orders and
strength, with an arrow toward the base it is attacking. The enemy doesn't
see your pins. Formations in the field also push the front line drawn on the
map.

## Reading the battlefield

### On the F10 map and in the world

```mapsymbols
[
 {"icon": "f10-pin", "name": "Formation pin (your side only)", "text": "One per formation in the field, in your side's colour: its name on the first line, then its orders and roughly how strong it is, in quarters (\"attack Gori | ~75% strength\"). Click it for the text. The enemy never sees your pins.", "side": "Blue"},
 {"icon": "f10-attack-arrow", "name": "Arrow from a pin", "text": "The formation is attacking a base, and this is the way it is heading. Drawn only when the base is more than 3 km off; formations moving or defending get no arrow.", "side": "Blue"},
 {"icon": "f10-battle", "name": "Dashed orange ring, \"GROUND BATTLE near …\"", "text": "Formations are fighting here, or a formation is fighting a base's garrison. Everyone sees it, both sides: a battle is not a secret. It moves with the fight and goes when the fighting stops. This is where CAS and attack helicopters are wanted."},
 {"icon": "world-smoke", "name": "Smoke and fire on the ground (in the 3D world)", "text": "Not on the map: in the world. A battle sends up a column of smoke you can see from the air long before you can see the vehicles, and every vehicle killed in it burns for a while. Follow the smoke."}
]
```

### On the dashboard: GROUND COMMAND

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
 {"icon": "gc-strength", "name": "Bar on the left", "text": "How much of the formation is still alive. Green from 70%, yellow from 40%, red below that. Below {{cfg:ground_war.withdraw_strength_pct|40}}% it is pulled back to refit."},
 {"icon": "gc-flag-broken", "name": "Grey symbol, BROKEN", "text": "Its morale broke. It is falling back to the nearest friendly base whatever its orders, and will take no order but withdraw until it rallies."},
 {"icon": "gc-flag-nosupply", "name": "NO SUPPLY", "text": "Its supply line is cut: no friendly base with supply in range, or the road to one is blocked. It crawls and fights at a fraction of its power. Open the road or pull it back."},
 {"icon": "gc-flag-halted", "name": "HALTED", "text": "It has stopped short of its orders: in contact, deploying, or stuck. Look at the detail panel for why."},
 {"icon": "gc-flag-sim", "name": "SIM", "text": "It is moving on the campaign map only, not spawned in DCS, because no player is near it. It fights and moves by the same rules; it turns real when someone comes within {{cfg:ground_war.player_bubble_m|30000}} m or it meets the enemy."},
 {"icon": "gc-selected", "name": "Yellow brackets and a dotted ring", "text": "Selected. The dotted ring is how far it can engage ({{cfg:ground_war.combat.engage_m|3000}} m). Yellow is always \"what you are about to order\"."},
 {"icon": "gc-group", "name": "Yellow number", "text": "The control group it is in (Ctrl+1 to set, 1 to select), as in any RTS."}
]
```

#### The enemy

```mapsymbols
[
 {"icon": "gc-hostile", "name": "Diamond: an enemy formation, in sight", "text": "A diamond is always the enemy, in their colour. \"~12 VEH\" is about how many vehicles we can see; the icon inside is what it looks like it is, the same as ours."},
 {"icon": "gc-ghost", "name": "Faded, dashed diamond: last seen", "text": "We can't see it any more. It is drawn where it was last seen, as it was then, with how long ago. It fades out over twenty minutes. It has almost certainly moved: treat it as a lead, not a target."},
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
