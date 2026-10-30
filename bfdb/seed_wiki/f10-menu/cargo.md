# Cargo Operations

Crates are how anything gets built away from a base: SAMs, armour, radars,
FARPs, repairs and supply runs all start as a crate you spawn at one friendly
base and fly somewhere else.

![Transport helicopter carrying a slung supply crate over the front line](/api/wiki/images/fe035065-f175-4f73-babe-9681513a3b93)

## Which cargo menu you get

Your airframe decides which menu appears under F10. The server lists the
airframes for each one.

| Menu | Who gets it | How crates move |
| --- | --- | --- |
| **Cargo** (classic) | Airframes the server gives crate slots, unless they are on one of the lists below | Load and unload through F10 |
| **Cargo** (dynamic) | Helicopters on the server's helo dynamic-cargo list | Spawned as real DCS cargo; load, sling and unload through **F8 → Ground Crew → Cargo** |
| **C-130 Cargo** | Aircraft on the server's C-130 cargo list | Real DCS cargo, airdropped or unloaded; unpacks itself when it lands. See [C-130 Hercules & Airdrop](../advanced/c130-airdrop.md) |

**CSAR** sits inside whichever of these you have (DCS shows only ten
top-level menus, and this keeps the rest on screen). See
[Combat Search & Rescue](./csar.md).

If the server has the [cockpit overlay](../getting-started/cockpit-overlay.md)
installed, its **CARGO** tab drives the same commands with buttons; see
[The cockpit panel's CARGO tab](#the-cockpit-panels-cargo-tab) below.

## What each airframe carries

Every carrier has crate slots, troop slots, a **total** that crates, troops and
rescued pilots share, and pilot slots for [CSAR](./csar.md). The total is the
real limit: a Huey with one crate aboard may have no room left for a squad.

| Airframe (DCS type) | Crates | Troops | Total | Downed pilots |
| --- | --- | --- | --- | --- |
| CH-47F (`CH-47Fbl1`) | {{cfg:cargo.CH-47Fbl1.crate_slots|6}} | {{cfg:cargo.CH-47Fbl1.troop_slots|10}} | {{cfg:cargo.CH-47Fbl1.total_slots|7}} | {{cfg:cargo.CH-47Fbl1.pilot_slots|4}} |
| Mi-8 (`Mi-8MT`) | {{cfg:cargo.Mi-8MT.crate_slots|1}} | {{cfg:cargo.Mi-8MT.troop_slots|2}} | {{cfg:cargo.Mi-8MT.total_slots|2}} | {{cfg:cargo.Mi-8MT.pilot_slots|3}} |
| UH-1H Huey | {{cfg:cargo.UH-1H.crate_slots|1}} | {{cfg:cargo.UH-1H.troop_slots|1}} | {{cfg:cargo.UH-1H.total_slots|2}} | {{cfg:cargo.UH-1H.pilot_slots|2}} |
| Mi-24P | {{cfg:cargo.Mi-24P.crate_slots|1}} | {{cfg:cargo.Mi-24P.troop_slots|1}} | {{cfg:cargo.Mi-24P.total_slots|1}} | {{cfg:cargo.Mi-24P.pilot_slots|1}} |
| SA342L Gazelle | {{cfg:cargo.SA342L.crate_slots|1}} | {{cfg:cargo.SA342L.troop_slots|1}} | {{cfg:cargo.SA342L.total_slots|1}} | {{cfg:cargo.SA342L.pilot_slots|1}} |
| SA342M Gazelle | {{cfg:cargo.SA342M.crate_slots|1}} | {{cfg:cargo.SA342M.troop_slots|1}} | {{cfg:cargo.SA342M.total_slots|1}} | {{cfg:cargo.SA342M.pilot_slots|1}} |

An airframe missing from the server's list has no cargo menu at all.
**List Cargo** always shows your own aircraft's real numbers. On the dynamic
and C-130 menus the crate count is not limited by these slots: what you can
lift is whatever DCS lets the ground crew load.

## Where you can spawn crates

Every crate is issued from a friendly base's stock, so you have to be at one:

- inside the zone of a **friendly objective** whose logistics are above 0% and
  which is **not capturable** (a base bombed down to capturable can't supply
  anything), or on a friendly carrier deck;
- the base must have stock to issue. With the materiel economy on, a base
  short of materiel refuses:

```dcsmessage
Ramat David is out of materiel (40 on hand, 100 needed) -- it can't issue supply until a convoy gets through
```

On the classic menu you also have to be **on the ground**, and you may only
have **{{cfg:max_crates|4}}** spawned crates of your own at once: spawning one
more deletes your oldest. The dynamic and C-130 menus don't use that limit.

A crate costs nothing to spawn. Points are taken when it is **unpacked**, from
you first and then from the fund of the base the crates came from; see
[Points and Lives](../gameplay/points-and-lives.md).

## The classic Cargo menu

For airframes that load crates through F10. The crate list is built from the
server's deployables, so the categories and units below are examples.

```f10menu
{"root":"F10 Other","items":[{"label":"Cargo","items":[
 {"label":"Unpack Nearby Crate(s)","says":"KillerDog unpacked a Hawk System","note":"Shown to your whole side. On a failure only you see the reasons, one per line, e.g. \"can't spawn Hawk System missing Hawk CC\" or \"can't unpack here — too close to friendly objective 'Ramat David', move 3400m away\"."},
 {"label":"Load Nearby Crate","says":"Hawk Launcher crate loaded\n1 of 4 Hawk System deployed, unpacking will delete oldest, 4471 when the limit is exceeded","note":"Takes the nearest friendly crate within the load distance. Fails with \"crate could not be loaded: no friendly crates within 50 meters\" or \"crate could not be loaded: you already have a full load onboard\"."},
 {"label":"Unload Crate","says":"Hawk Launcher crate unloaded","note":"Puts down the last crate you loaded. In a hover it must be slow and low enough: \"you are too high to unload your cargo, altitude agl must be at or below 10 m, and speed must be at or below 46 km/h\"."},
 {"label":"List Nearby Crates","says":"Hawk Launcher crate, bearing 271, 32 meters away\nHawk CC crate, bearing 268, 41 meters away","note":"Crates within the load distance, nearest first. \"No nearby crates\" if there are none."},
 {"label":"List Cargo","says":"Current Cargo\n----------------------------\ntroops: 0 of 1\ncrates: 1 of 1\npilots: 0 of 2\ntotal : 1 of 2\n----------------------------\nHawk Launcher crate weighing 1000 kg\n----------------------------\ntotal cargo weight: 1000 kg"},
 {"label":"Destroy Nearby Crate","note":"Deletes the nearest crate within the load distance, with no message. You must be landed: \"you must land to destroy crates\"."},
 {"label":"Crates","items":[
  {"label":"Base Supply","items":[
   {"label":"Logistics Repair","says":"2 of 4 crates spawned, 4471 will be deleted if the limit is exceeded","note":"The base repair kit. Every crate entry answers the same way; 4471 is your oldest crate's id."},
   {"label":"Emergency Repair","says":"3 of 4 crates spawned, 4471 will be deleted if the limit is exceeded","note":"Only where the server enables emergency repairs."},
   {"label":"Fuel Transfer","says":"1 of 4 crates spawned, 4471 will be deleted if the limit is exceeded"},
   {"label":"Weapons Transfer","says":"1 of 4 crates spawned, 4471 will be deleted if the limit is exceeded"}]},
  {"label":"Radar SAMs","items":[
   {"label":"Roland ADS(35 pts)","items":[{"label":"Roland(4)","says":"1 of 4 crates spawned, 4471 will be deleted if the limit is exceeded","note":"(4) means the unit needs four of this crate."}]},
   {"label":"Hawk System(35 pts)","items":[
    {"label":"Hawk Launcher","says":"1 of 4 crates spawned, 4471 will be deleted if the limit is exceeded"},
    {"label":"Hawk Search Radar","says":"2 of 4 crates spawned, 4471 will be deleted if the limit is exceeded"},
    {"label":"Hawk Track Radar","says":"3 of 4 crates spawned, 4471 will be deleted if the limit is exceeded"},
    {"label":"Hawk CC","says":"4 of 4 crates spawned, 4471 will be deleted if the limit is exceeded"},
    {"label":"Hawk Repair","says":"1 of 4 crates spawned, 4471 will be deleted if the limit is exceeded","note":"The unit's repair crate, listed after its build crates."}]}]},
  {"label":"AAA","items":[{"label":"Vulkan(15 pts)","items":[{"label":"Vulkan Crate(2)","says":"1 of 4 crates spawned, 4471 will be deleted if the limit is exceeded"}]}]},
  {"label":"Ground Units","items":[{"label":"M2A2 Bradley(25 pts)","items":[{"label":"Bradley Crate(4)","says":"1 of 4 crates spawned, 4471 will be deleted if the limit is exceeded"}]}]},
  {"label":"EWRs","items":[{"label":"AN/FPS-117(5 pts)","items":[{"label":"FPS-117 Crate(2)","says":"1 of 4 crates spawned, 4471 will be deleted if the limit is exceeded"}]}]}]},
 {"label":"CSAR","items":[{"label":"List Downed Pilots","says":"No downed pilots on your side","note":"See the CSAR page."}]}]}]}
```

If you can't afford the unit you just spawned a crate for, you are warned
straight away (the crate still spawns, and the check that counts is at unpack):

```dcsmessage
WARNING: you have 20 points, and this deployable costs 35 points
```

The common spawn failures:

| Message | Why |
| --- | --- |
| `you must land to spawn crates` | Classic menu only. |
| `not near friendly logistics (or this base is too damaged to supply from)` | You are outside a working friendly base's zone. |
| `no clear space to spawn crate, move away from other crates` | The nine spots next to you are taken. Taxi a little. |

## The dynamic Cargo menu (helicopters)

On servers that run helicopter dynamic cargo, crates are real DCS cargo
objects. You spawn them from F10, then load, sling and set them down with the
DCS ground crew (**F8 → Ground Crew → Cargo**). Each crate entry also has an
**xN** submenu that spawns 1 to 9 copies one second apart.

```f10menu
{"root":"F10 Other","items":[{"label":"Cargo","items":[
 {"label":"Unpack Nearby Crate(s)","says":"Airdropped Hawk System deployed","note":"Works on the dynamic crates on the ground within the load distance. Repeated lines are collapsed, e.g. \"Crate landed, need more crates for Hawk System (x3)\"."},
 {"label":"List Nearby Crates","says":"Hawk Launcher crate, bearing 271, 32 meters away"},
 {"label":"List Cargo","says":"Current Cargo\n----------------------------\ntroops: 1 of 10\ncrates: 0 of 6\npilots: 0 of 4\ntotal : 1 of 7\n----------------------------\nStandard troop weighing 800 kg\n----------------------------\ntotal cargo weight: 800 kg","note":"Shows troops and pilots. Dynamic crates are counted by DCS, not here."},
 {"label":"Destroy Nearby Crate","note":"Deletes the nearest crate on the ground within the load distance. No message on success."},
 {"label":"Pack Up Fat Cow","says":"Packed up Fat Cow 1: 6200kg jet fuel and 64 weapons left. Its 2 crates are next to you -- load them (F8 -> Ground Crew -> Cargo) and unpack them where you want it.","note":"Only on servers with a Fat Cow deployable. Fails with \"land at the Fat Cow to pack it up\" or \"you are not at a friendly Fat Cow\"."},
 {"label":"Crates","items":[
  {"label":"Base Supply","items":[
   {"label":"Logistics Repair","says":"Spawned Logistics Repair crate. Use DCS cargo menu (F8 -> Ground Crew -> Cargo) to load it."},
   {"label":"Logistics Repair xN","items":[{"label":"1"},{"label":"2"},{"label":"3","says":"Queued 3 crates for spawning (1 second intervals). They will appear behind your aircraft."},{"label":"4"},{"label":"5"},{"label":"6"},{"label":"7"},{"label":"8"},{"label":"9"}]},
   {"label":"Fuel Transfer","says":"Spawned Fuel Transfer crate. Use DCS cargo menu (F8 -> Ground Crew -> Cargo) to load it."},
   {"label":"Fuel Transfer xN","items":[{"label":"1"},{"label":"2","says":"Queued 2 crates for spawning (1 second intervals). They will appear behind your aircraft."}]},
   {"label":"Weapons Transfer","says":"Spawned Weapons Transfer crate. Use DCS cargo menu (F8 -> Ground Crew -> Cargo) to load it."},
   {"label":"Weapons Transfer xN","items":[{"label":"1"},{"label":"2"}]},
   {"label":"Carrier Repair","says":"Spawned Carrier Repair crate. Use DCS cargo menu (F8 -> Ground Crew -> Cargo) to load it."},
   {"label":"Carrier Repair xN","items":[{"label":"1"},{"label":"2"}]},
   {"label":"Emergency Repair","says":"Spawned Emergency Repair crate. Use DCS cargo menu (F8 -> Ground Crew -> Cargo) to load it.","note":"Single entry, no xN: a base can only take one every few minutes."}]},
  {"label":"Radar SAMs","items":[
   {"label":"Hawk System(35 pts)","items":[
    {"label":"Hawk Repair","says":"Spawned Hawk Repair crate. Use DCS cargo menu (F8 -> Ground Crew -> Cargo) to load it.","note":"The repair crate comes first here, with no xN."},
    {"label":"Hawk Launcher","says":"Spawned Hawk Launcher crate. Use DCS cargo menu (F8 -> Ground Crew -> Cargo) to load it."},
    {"label":"Hawk Launcher xN","items":[{"label":"1"},{"label":"2"}]},
    {"label":"Hawk Search Radar","says":"Spawned Hawk Search Radar crate. Use DCS cargo menu (F8 -> Ground Crew -> Cargo) to load it."},
    {"label":"Hawk Search Radar xN","items":[{"label":"1"},{"label":"2"}]},
    {"label":"Hawk Track Radar","says":"Spawned Hawk Track Radar crate. Use DCS cargo menu (F8 -> Ground Crew -> Cargo) to load it."},
    {"label":"Hawk Track Radar xN","items":[{"label":"1"},{"label":"2"}]},
    {"label":"Hawk CC","says":"Spawned Hawk CC crate. Use DCS cargo menu (F8 -> Ground Crew -> Cargo) to load it."},
    {"label":"Hawk CC xN","items":[{"label":"1"},{"label":"2"}]}]}]},
  {"label":"Fat Cow(25 pts)","items":[
   {"label":"Fat Cow Fuel Bladder","says":"Spawned Fat Cow Fuel Bladder crate. Use DCS cargo menu (F8 -> Ground Crew -> Cargo) to load it."},
   {"label":"Fat Cow Fuel Bladder xN","items":[{"label":"1"}]},
   {"label":"Fat Cow Ammo Pallet","says":"Spawned Fat Cow Ammo Pallet crate. Use DCS cargo menu (F8 -> Ground Crew -> Cargo) to load it."},
   {"label":"Fat Cow Ammo Pallet xN","items":[{"label":"1"}]}]}]},
 {"label":"CSAR","items":[{"label":"List Downed Pilots","note":"See the CSAR page."}]}]}]}
```

Spawn failures on this menu start with `Failed to spawn crate:`, for example:

```dcsmessage
Failed to spawn crate: you must be inside a friendly logistics objective to spawn crates
```

### Unpacking dynamic crates

Whether a dynamic crate unpacks itself is a server setting
(**{{cfg:helo_cargo.auto_unpack|no}}** on this server). When it doesn't, set
the crates down with the ground crew, then use **Unpack Nearby Crate(s)**.
Crates still in your cabin are left alone; if all of yours are still loaded
you are told so:

```dcsmessage
Nothing on the ground here to unpack -- your 2 crate(s) are still loaded. Set one down (F8 -> Ground Crew -> Cargo) first.
```

Dynamic crates follow the same rules as the C-130's (a set counts within
500 m, any mix of delivery methods), and a deployable built this way reports
`Airdropped <unit> deployed` even though you flew it in. The full sequence,
and what each Base Supply crate does on landing, is on
[C-130 Hercules & Airdrop](../advanced/c130-airdrop.md#what-happens-when-a-crate-lands).

## Unpacking: the rules

**Unpack Nearby Crate(s)** looks at every crate within
{{cfg:crate_load_distance|50}} m of you, then at every crate within
{{cfg:crate_spread|250}} m of those, and builds the first thing it can. On the
classic menu you must be landed (`you must land to unpack crates`).

| Rule | What the code checks |
| --- | --- |
| **The full set** | Every crate type the unit needs, in the numbers it needs. Otherwise: `can't spawn Hawk System missing Hawk CC`. |
| **Away from friendly bases** | A unit can't be unpacked within {{cfg:logistics_exclusion|10000}} m of a friendly objective's centre, or inside its zone (plus a 10% margin). Enemy and neutral objectives are fine: you can build next to a base you are attacking. |
| **Points** | You plus the crates' home base must have the unit's price: `there are 20 available points, this deployable costs 35 points to unpack`. |
| **The unit limit** | Each unit type has a per-side limit. At the limit the server either refuses (`the max number of "Hawk System" are already deployed`) or deletes your side's oldest one, depending on that unit's setting. **Load Nearby Crate** tells you which, before you fly. |
| **Supply** | Each crate used draws on the base it was spawned at: {{cfg:deploy_supply_cost|3}}% of its stock per crate, or {{cfg:warehouse.materiel.deploy_cost|100}} materiel per crate where the materiel economy is on. |

The unit spawns at the crates, kept at least 120 m from any aircraft, and a
second unit of the same type unpacked at the same spot is placed beside the
first instead of on top of it. Units with separate parts (a launcher and a
radar, say) put each part where its crate was.

Unpacking a FARP deployable answers differently:

```dcsmessage
KillerDog unpacked Forward FARP 3, units will spawn in 60 seconds get clear
```

## Base Supply crates

These do not build anything. They fix or feed a base. On the classic menu you
unpack them with **Unpack Nearby Crate(s)**; dynamic and C-130 crates act on
their own when they land (see the C-130 page).

| Crate | Classic unpack | Message |
| --- | --- | --- |
| **{{cfg:repair_crate.Blue.name|Logistics Repair}}** | Inside the zone of a friendly base **other than the one you loaded it at**. Raises that base's logistics one step. | `KillerDog repaired logistics at Ramat David to %80` |
| **{{cfg:emergency_repair.crate.name|Emergency Repair}}** | Inside the zone of a damaged, quiet friendly base. Rebuilds one destroyed group. | `KillerDog delivered an emergency repair crate. Emergency repair at Ramat David: AAA group rebuilt (health 70% -> 80%)` |
| **Fuel / Weapons Transfer** | Inside the zone of a friendly base other than its origin. Moves {{cfg:warehouse.supply_transfer_size|25}}% of the origin's stock across. | `KillerDog transfered supplies from Ramat David to Megiddo` |
| **Carrier Repair** | Dynamic and C-130 menus only. Delivered to the nearest friendly carrier group. | `Carrier repair crate delivered to CVN-71 (2 crate(s)) - ~5m remaining` |

A repair kit at a base whose logistics are already full is refused
(`objective logistics are completely repaired`), unless that base is still
consolidating after a capture: then it is spent to speed the consolidation
up instead:

```dcsmessage
KillerDog delivered a logistics repair kit to Megiddo, speeding up its consolidation
```

A repair kit pays {{cfg:points.logistics_repair|25}} points and a supply
transfer {{cfg:points.logistics_transfer|15}} points, more for a long haul or
a front-line base. If someone else unpacks the crate you flew, the pay is
split between you. Full rules for every repair are on
[Logistics & Supply](../gameplay/logistics.md#repairing-a-base).

## Repairing a deployed unit

Units with a repair crate (multi-part SAMs, usually) can be brought back
instead of rebuilt. Spawn the unit's repair crate, set it down within
{{cfg:crate_spread|250}} m of the damaged unit and unpack:

```dcsmessage
KillerDog repaired a Hawk System
```

The repair has its own points price (some units charge nothing) and the same
"away from friendly bases" rule as unpacking. Failures read
`not close enough to repair Hawk System`,
`not enough crates to repair Hawk System` or
`Repairing Hawk System costs 10, you have 4`.

## The cockpit panel's CARGO tab

With the cockpit overlay installed, the **CARGO** tab shows what is aboard,
what is lying nearby and what you can build, and every button presses the same
F10 command:

```cockpitpanel
{"tab":"CARGO","rows":[
 {"heading":"ON BOARD"},
 {"text":"Crates 1 / 1    Troops 0 / 1"},
 {"text":"  • Hawk Launcher"},
 {"heading":"NEARBY CRATES"},
 {"text":"  Hawk CC  --  41 m, bearing 268°"},
 {"button":"UNPACK NEARBY CRATE(S)","says":"can't spawn Hawk System missing Hawk Search Radar"},
 {"button":"LOAD NEARBY CRATE","says":"crate could not be loaded: you already have a full load onboard"},
 {"button":"UNLOAD CRATE","says":"Hawk Launcher crate unloaded"},
 {"button":"LIST CARGO","says":"Current Cargo"},
 {"heading":"BASE SUPPLY"},
 {"text":"Logistics Repair  --  need 1, 1500 kg each"},
 {"button":"SPAWN 1","says":"2 of 4 crates spawned, 4471 will be deleted if the limit is exceeded"},
 {"heading":"BUILD SOMETHING -- pick a category"},
 {"button":"  Radar SAMs   (2)  >"},
 {"button":"  AAA   (2)  >"}],
 "status":"Hawk Launcher crate unloaded"}
```

Inside a category each unit lists its crates with **SPAWN 1**, plus
**SPAWN ALL N** for a crate the unit needs several of, and its repair kit if
it has one. See [Cockpit overlay](../getting-started/cockpit-overlay.md).

## Tips

- **Check the limit message when you load.** It tells you how many are already
  out and whether yours will replace the oldest.
- **Fly well clear of your own bases to build.** The exclusion distance is
  measured from each friendly objective's centre.
- **Land the set together.** Crates count as one set within
  {{cfg:crate_spread|250}} m of each other.
- **Supply crates go to a different base.** A repair kit or transfer crate
  unpacked at the base it came from does nothing.

## See Also

- [Running Crates & Building a Base](../playbooks/running-crates.md) - step by step
- [Deployables Guide](../advanced/deployables-guide.md) - what to build and where
- [Deployable Units Reference](../reference/deployables.md) - this server's crate list
- [C-130 Hercules & Airdrop](../advanced/c130-airdrop.md)
- [Troop Transport](./troops.md)
- [Logistics & Supply](../gameplay/logistics.md)
