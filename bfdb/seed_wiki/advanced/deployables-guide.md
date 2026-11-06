# Deployables Guide

A deployable is a unit you build from crates: a SAM, an AAA piece, armour,
artillery, a radar, a JTAC vehicle, a FARP. The
[Deployable Units Reference](../reference/deployables.md) lists exactly what
the server you have selected offers, with crates, weights and limits. This page
explains what each kind does on the battlefield, where you may build it, and
when to call for one over another.

![SA-22 Pantsir air defense vehicle with autocannons and missile canisters raised](/api/wiki/images/f5701fbb-6bec-4db2-b96b-7bd33d41880f)

## What each category does

The categories you see under **Cargo → Crates** are the server's own, and each
campaign fields different units (a period campaign only has period kit). The
families below are the usual ones.

### Air defence

Air defence deployables deny the sky over a piece of ground. They come in
three families with very different jobs:

**Radar SAMs** scan and engage from several kilometres out, but their radar
emissions give them away, so a HARM or SEAD flight can find and kill the radar
before the system matters. Multi-part systems (a launcher, a search radar, a
command post) usually have a **repair crate**: fly one in to bring a damaged
site back instead of rebuilding it.

**IR SAMs** are short-ranged but have no big search radar to give them away.
They punish the low-level helicopter and CAS runs that think they have dodged
the radar coverage.

**AAA** is the cheapest and quickest layer: pure guns, no missile reach, but
lethal to anything that comes close.

### Armour and IFVs

Ground vehicles are the close-in muscle and the backbone of a counter-attack.

- **IFVs and APCs** are cheap, quick to build in numbers, and good for
  screening a landing zone or contesting a capture.
- **Main battle tanks** cost more crates and more points and usually have a
  lower limit, but hit harder and survive longer.

### Artillery

Artillery sits behind the line and reaches out to suppress troops and vehicles
massing at a contested objective without exposing itself. How fire missions
are called is on [Artillery](./artillery.md).

### JTAC and recon vehicles

A recon or JTAC vehicle gives a position a standing spotter that can call CAS
without a player holding a laser on it. Some can see through terrain. Build one
to open up CAS on a position before committing other forces. See
[JTAC](../f10-menu/jtac.md).

### EWR and GCI

An **EWR** deployable feeds your side's early-warning picture far beyond any
SAM's own radar, which makes it the best way to watch an approach lane. Some
campaigns also have a **GCI station**. The reference table shows which
deployables provide an EWR or GCI, and their ranges come from the server's
config. See [EWR](../f10-menu/ewr.md) and [GCI](../gameplay/gci.md).

### Logistics

An **Ammo Truck** carries no weapon and no sensor, only resupply for nearby
units. It is cheap, so keeping one forward of a contested objective is rarely a
wasted cargo run.

### FARPs

A FARP deployable becomes a new friendly objective: a forward pad with its own
zone. When you unpack it you are told to get clear, because its units arrive a
minute later:

```dcsmessage
KillerDog unpacked Forward FARP 3, units will spawn in 60 seconds get clear
```

## Where you can build

The rules are the same whichever way the crates arrive (classic F10 menu,
helicopter dynamic cargo or C-130 drop).

| Rule | Detail |
| --- | --- |
| **Not next to your own bases** | A deployable can't be unpacked within {{cfg:logistics_exclusion|10000}} m of a friendly objective's centre, or inside its zone (plus 10%). You are told how far to move: `can't unpack here — too close to friendly objective 'Ramat David', move 3400m away`. |
| **Enemy bases are fine** | The rule only counts your own side's objectives, so you can build right next to a base you are attacking. |
| **The whole set** | Every crate the unit needs, close together (within {{cfg:crate_spread|250}} m of each other on the F10 unpack, 500 m for dropped crates). |
| **Clear of aircraft** | The unit is placed at least 120 m from any aircraft, and a second unit of the same type at the same spot is placed beside the first. |

So bases defend themselves with their own garrison, and deployables extend
coverage **between** bases and **forward** of them: the gap between two
airfields, a road junction, an approach lane, the enemy base you are about to
assault.

## What it costs

- **Points**, taken when the unit is unpacked: from you first, then from the
  fund of the base the crates came from. The price is in the menu label
  (`Hawk System(35 pts)`). With not enough between you:
  `there are 20 available points, this deployable costs 35 points to unpack`.
- **Supply** from the base the crates were spawned at:
  {{cfg:deploy_supply_cost|3}}% of its stock per crate, or
  {{cfg:warehouse.materiel.deploy_cost|100}} materiel per crate where the
  materiel economy is on. See [Logistics & Supply](../gameplay/logistics.md).

## Limits

Each unit type has a limit per side (not per player). At the limit the type
either **refuses** new ones (`the max number of "Hawk System" are already
deployed`) or **replaces your side's oldest** one, depending on how the server
set that unit up. **Load Nearby Crate** tells you which before you fly:

```dcsmessage
Hawk Launcher crate loaded
3 of 4 Hawk System deployed, unpacking will delete oldest, 4471 when the limit is exceeded
```

## Repairing a deployable

A unit with a repair crate can be brought back after it has been shot up.
Spawn its repair crate (in the unit's own crate menu), put it down within
{{cfg:crate_spread|250}} m of the damaged unit, and unpack. The repair has its
own points price, possibly zero, and the same "not next to your own bases"
rule. Success reads `KillerDog repaired a Hawk System`. Everything without a
repair crate has to be rebuilt.

`-delete <groupid>` in chat removes a group you deployed for a partial refund;
see [Chat Commands](../reference/chat-commands.md).

## Fat Cow (heavy-lift FARP)

*Only on servers that configure one.* A Fat Cow is a forward arming and
refuelling point you fly in yourself: a heavy-lift helicopter lands, and a pad
with fuel, weapons and ground crew is laid out around it. Both sides can have
one, up to the server's limit.

**Building one**

1. In a CH-47 or Mi-8 on the dynamic cargo menu, spawn the kit from
   **F10 → Cargo → Crates → Fat Cow(N pts)**: a Fat Cow Fuel Bladder
   (1,500 kg) and a Fat Cow Ammo Pallet (1,000 kg).
2. Load them through **F8 → Ground Crew → Cargo**, fly to the site, land and
   unload them the same way.
3. Stay on the ground and pick **F10 → Cargo → Unpack Nearby Crate(s)**.

The layout is built around your helicopter, facing the way you face, so land
facing the way you want it and pick a clear area about 300 × 300 m. The pad
is 100 m behind you and has ground crew straight away. A minute later the rest
appears: a fuel point 35 m aft, a hose and generator 50 m aft, a fire point
70 m aft, six guards around the helicopter and a MANPAD 50 m ahead. Land close
to the pad to rearm and refuel; if the ground crew doesn't answer, you're too
far from it.

**Placement.** The same exclusion distance from friendly objectives as any
other deployable. The Fat Cow's own zone is small: 500 m by default.

**Finite stock.** No hub supplies a Fat Cow. It holds only what it was built
with (by default 9,000 kg of jet fuel, plus Hellfires and Hydras for Blue, or
Vikhrs, Shturms and S-8s for Red). When that runs out it's dry, unless someone
flies a [supply transfer crate](../gameplay/logistics.md#supply-transfer-crates)
to it.

**Packing up and moving.** Land inside the Fat Cow's zone and pick
**F10 → Cargo → Pack Up Fat Cow**. The FARP closes and its crates are put down
next to your helicopter, carrying whatever fuel and weapons it had left:

```dcsmessage
Packed up Fat Cow 1: 6200kg jet fuel and 64 weapons left. Its 2 crates are next to you -- load them (F8 -> Ground Crew -> Cargo) and unpack them where you want it.
```

Load them, fly to the new site and unpack them. It comes back with that
leftover stock, and you aren't charged again. Packed crates don't survive a
server restart, so set it back up before one.

**Destroying one.** Destroy both of its fuel points and the Fat Cow closes.

**Fat Cow (AI).** Where the server offers it, the **Fat Cow (AI)** action
does the flying for you: drop a map mark, buy the action, and an AI heavy-lift
helicopter takes off from the nearest friendly field, lands at the mark and
sets the Fat Cow up around itself. It stays 20 minutes by default, then packs
up and flies home. If the helicopter is shot down on station, the Fat Cow
closes with it. You get your points back if it's lost or can't set up before
it starts work. See [Action Types](../reference/action-types.md#fat-cow-ai).

## Choosing what to deploy

- **Screening a landing zone against helicopters**: an IR SAM or AAA piece.
  Cheap, quick, and punishes the low-level approach.
- **Denying fixed-wing CAS over a position**: a radar SAM backed by AAA, so a
  SEAD flight alone doesn't clear the sky.
- **Reinforcing an assault on an enemy base**: IFVs first (cheap, quick), tanks
  if the fight will last. Build them next to the target, not next to your own
  base.
- **Hitting something beyond your own reach**: artillery, not a ground push.
- **Opening up CAS on a position**: a JTAC or recon vehicle first.
- **Watching an approach lane**: an EWR well forward.
- **Keeping attack helicopters in the fight far from base**: a Fat Cow, where
  the server has one.

## See Also

- [Deployable Units Reference](../reference/deployables.md) - this server's crates, limits and ranges
- [Cargo Operations](../f10-menu/cargo.md) - spawning, loading and unpacking
- [C-130 Hercules & Airdrop](./c130-airdrop.md)
- [Running Crates & Building a Base](../playbooks/running-crates.md)
- [JTAC System](../f10-menu/jtac.md)
