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

## How they fight

Formations march along the road network at about
{{cfg:ground_war.speed_kph|30}} km/h. They only become real DCS units where
it matters:

- when a player is within {{cfg:ground_war.player_bubble_m|30000}} m of them;
- when they meet an enemy formation, or close to within
  {{cfg:ground_war.contact_m|10000}} m of the base they are attacking.

Away from players they move on the campaign map only, which keeps the server
fast. When two enemy formations meet and the server is already running as
many battles as it can, both halt and wear each other down on the map until
there is room to fight it out for real.

In the field they're fair game. You'll find columns on the roads and fights
around contested bases: real targets for CAS, attack helicopters and
artillery. Recon flights see them too.

## Taking a base

A formation attacking a base fights its garrison. Once the base is broken
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

Open **F10 → Ground Forces**. The menu is built when you open it, so it
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
what.

Each formation has a pin on your side's F10 map showing its orders and
strength, with an arrow toward the base it is attacking. The enemy doesn't
see your pins. Formations in the field also push the front line drawn on the
map.
