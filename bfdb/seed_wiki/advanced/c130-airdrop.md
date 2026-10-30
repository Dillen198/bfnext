# C-130 Hercules & Airdrop

![C-130 Hercules banking low over the front line](/api/wiki/images/76185372-d487-422b-a6ea-89de0da561d8)

The C-130 moves more in one sortie than any helicopter, and it does not need to
land where the cargo is going. You spawn real DCS cargo behind the aircraft,
load it with the ground crew, and drop it out of the ramp (or land and unload
it). When a crate comes to rest, the engine unpacks it on its own: a full set
of crates becomes a unit, a supply crate feeds the nearest friendly base.

The C-130 gets its own **C-130 Cargo** menu, on aircraft the server lists for
C-130 cargo.

## The C-130 Cargo menu

```f10menu
{"root":"F10 Other","items":[{"label":"C-130 Cargo","items":[
 {"label":"List Nearby Crates","says":"Hawk Launcher crate, bearing 182, 44 meters away\nHawk CC crate, bearing 176, 46 meters away","note":"Crates on the ground within the load distance. \"No nearby crates\" if there are none."},
 {"label":"Delete Nearby Crate","note":"Deletes the nearest crate on the ground, no message on success. You must be landed: \"you must land to destroy crates\"."},
 {"label":"Delete All Nearby Crates","says":"destroyed 3 nearby crates","note":"Clears every crate within the load distance."},
 {"label":"Crates","items":[
  {"label":"Base Supply","items":[
   {"label":"Logistics Repair","says":"Spawned Logistics Repair crate. Use DCS cargo menu (F8 -> Ground Crew -> Cargo) to load it."},
   {"label":"Logistics Repair xN","items":[{"label":"1"},{"label":"2"},{"label":"3","says":"Queued 3 crates for spawning (1 second intervals). They will appear behind your aircraft."},{"label":"4"},{"label":"5"},{"label":"6"},{"label":"7"},{"label":"8"},{"label":"9"}]},
   {"label":"Fuel Transfer","says":"Spawned Fuel Transfer crate. Use DCS cargo menu (F8 -> Ground Crew -> Cargo) to load it."},
   {"label":"Fuel Transfer xN","items":[{"label":"1"},{"label":"2"}]},
   {"label":"Weapons Transfer","says":"Spawned Weapons Transfer crate. Use DCS cargo menu (F8 -> Ground Crew -> Cargo) to load it."},
   {"label":"Weapons Transfer xN","items":[{"label":"1"},{"label":"2"}]},
   {"label":"Carrier Repair","says":"Spawned Carrier Repair crate. Use DCS cargo menu (F8 -> Ground Crew -> Cargo) to load it."},
   {"label":"Carrier Repair xN","items":[{"label":"1"},{"label":"2"}]},
   {"label":"Emergency Repair","says":"Spawned Emergency Repair crate. Use DCS cargo menu (F8 -> Ground Crew -> Cargo) to load it.","note":"Only where the server enables emergency repairs; no xN."}]},
  {"label":"Radar SAMs","items":[
   {"label":"SA 6 Kub(25 pts)","items":[
    {"label":"Kub Repair","says":"Spawned Kub Repair crate. Use DCS cargo menu (F8 -> Ground Crew -> Cargo) to load it.","note":"The unit's repair crate, where it has one: a single entry, listed first."},
    {"label":"Kub Launcher(2)","says":"Spawned Kub Launcher crate. Use DCS cargo menu (F8 -> Ground Crew -> Cargo) to load it.","note":"(2) means the unit needs two of this crate."},
    {"label":"Kub Launcher xN","items":[{"label":"1"},{"label":"2","says":"Queued 2 crates for spawning (1 second intervals). They will appear behind your aircraft."},{"label":"3"}]},
    {"label":"Kub Radar","says":"Spawned Kub Radar crate. Use DCS cargo menu (F8 -> Ground Crew -> Cargo) to load it."},
    {"label":"Kub Radar xN","items":[{"label":"1"}]}]}]},
  {"label":"Ground Units","items":[
   {"label":"T72(50 pts)","items":[
    {"label":"T72 Crate(4)","says":"Spawned T72 Crate crate. Use DCS cargo menu (F8 -> Ground Crew -> Cargo) to load it."},
    {"label":"T72 Crate xN","items":[{"label":"1"},{"label":"2"},{"label":"3"},{"label":"4","says":"Queued 4 crates for spawning (1 second intervals). They will appear behind your aircraft."}]}]}]}]},
 {"label":"Vehicles","items":[
  {"label":"Light Vehicles","items":[{"label":"UAZ-469 (5 pts)","says":"Spawned UAZ-469 vehicle cargo. Use DCS cargo menu (F8 -> Ground Crew -> Cargo) to load it.","note":"Only where the server configures loadable vehicles. Paid for when spawned."}]}]},
 {"label":"CSAR","items":[{"label":"List Downed Pilots","note":"See Combat Search & Rescue."}]}]}]}
```

The categories and crates are built from the server's deployables, so the ones
above are examples; the live list is on the
[Deployable Units Reference](../reference/deployables.md). There is no Unpack
and no Load entry: loading is the DCS ground crew's job, and unpacking is
automatic.

Spawn failures start with `Failed to spawn crate:` (or
`Failed to queue crates:` from an xN menu):

| Message after the prefix | Why |
| --- | --- |
| `you must be inside a friendly logistics objective to spawn crates` | Taxi into the zone of a friendly base with working logistics that is not capturable (or be on a friendly carrier deck). |
| `no clear space to spawn crate, move away from other crates` | The spots behind you are full. Move the aircraft. |
| `Ramat David is out of materiel (40 on hand, 100 needed) -- it can't issue supply until a convoy gets through` | The base has nothing left to issue. |

## Step by step

![Five steps of a C-130 crate: spawn behind the aircraft inside a friendly base, load with F8 Ground Crew Cargo, drop it or unload it, the engine sees it at rest, then auto-unpack, which either deploys the unit when the set is complete within 500 m, waits with an F10 mark when crates are missing, or is blocked with the reason on a mark; Base Supply crates go to the nearest friendly objective, carrier repair to the nearest carrier group, and emergency repair must land inside the damaged base's zone](/api/wiki/images/a7d14e58-2c39-4f86-b0e2-91c5f3d76a28)

1. **Park inside a friendly base.** Its zone, with logistics above 0%, and not
   capturable. Crates are issued from that base's stock.
2. **Spawn.** **C-130 Cargo → Crates → category → unit → crate**, or the crate's
   **xN** entry for several at once (up to 9 per click, one per second). They
   appear about 45 m behind the aircraft in rows of three.
3. **Load.** **F8 → Ground Crew → Cargo** and load each one.
4. **Fly and deliver.** Drop them out of the ramp over the drop zone, or land
   and unload them with the ground crew. The engine does not check your
   height or speed for a drop; the crate only has to arrive in one piece.
5. **Let it unpack.** Nothing to press. See below.

A crate that is still aboard is ignored entirely, so a crate riding in your
hold never unpacks itself, and **List Nearby Crates** never lists it.

## What happens when a crate lands

A crate counts as **landed** when it comes to rest after falling for at least
3 seconds, or when it comes to rest more than 100 m from where it was spawned
(landed and unloaded elsewhere). A crate unloaded within 100 m of its spawn
point never counts as delivered.

### Deployable crates

The engine looks for the rest of the unit's crates within **500 m** of the
one that landed. Any friendly crate on the ground counts, however it got there:
dropped from a C-130, flown in by a helicopter, or spawned from a classic F10
cargo menu. So two crates by C-130 and the third by Huey still make one unit.

**Set complete.** The unit spawns at the crates and your side sees:

```dcsmessage
Airdropped SA 6 Kub deployed
```

**Crates missing.** Your side is told once, and an F10 map mark at the crate
lists what is missing; it unpacks the moment the rest arrive:

```dcsmessage
Crate landed, need more crates for SA 6 Kub
```

The map mark reads `SA 6 Kub` and then `Missing: Kub Radar (need 1, have 0)`.

**Blocked.** The same rules as any unpack (see
[Cargo Operations](../f10-menu/cargo.md#unpacking-the-rules)): not within
{{cfg:logistics_exclusion|10000}} m of a friendly objective, inside the unit
limit, and the price affordable. The crate's spawner pays. A blocked crate
stays on the ground with the reason on its map mark and in one message to your
side, and tries again a minute later (or at once if it is moved):

```dcsmessage
can't unpack SA 6 Kub here — too close to friendly objective 'Ramat David', move 3400m away
```

A FARP or Fat Cow deployable answers `Forward FARP set up as Forward FARP 3`
instead.

### Base Supply crates

These do not need a set. Each one acts alone when it lands:

| Crate | Goes to | Message |
| --- | --- | --- |
| **Fuel Transfer** | The **nearest friendly objective**, which must not be the base it was loaded at. Moves up to {{cfg:warehouse.supply_transfer_size|25}}% of fuel, taken from the base it was loaded at. | `Fuel transfer crate delivered to Megiddo` |
| **Weapons Transfer** | Same, for weapons and equipment (never airframes). | `Weapons transfer crate delivered to Megiddo` |
| **{{cfg:repair_crate.Blue.name|Logistics Repair}}** | The nearest friendly objective (the base it was loaded at is allowed). Logistics up one step. | `Logistics repair crate delivered to Megiddo (logi 60 -> 80%)` |
| **Carrier Repair** | The nearest friendly carrier group. Starts its repair, or speeds one up: the repair time is divided by the number of crates delivered. | `Carrier repair crate delivered to CVN-71 (2 crate(s)) - ~5m remaining` |
| **{{cfg:emergency_repair.crate.name|Emergency Repair}}** | Must come to rest **inside the zone** of a damaged, quiet friendly base. Rebuilds one destroyed group. | `Emergency repair at Megiddo: AAA group rebuilt (health 70% -> 80%)` |

"Nearest" means nearest anywhere on the map, so drop supply crates on or right
next to the base you mean. Refusals leave the crate on the ground, with the
reason on a map mark:

- `supplies can't be delivered back to Ramat David, the base they were loaded at -- fly them to another friendly base`
- `Megiddo logistics are already fully repaired -- crate left on the ground`
  (unless Megiddo is still consolidating after a capture:
  `Megiddo logistics are already fully repaired, but the delivery sped up consolidation`)
- `Emergency repair crate not used: Megiddo is under attack -- repairs can't start while enemy units are in sight of it`.
  An emergency crate retries about once a minute, so one that lands during an
  attack still does its job when the base goes quiet. The full list of reasons
  is on [Logistics & Supply](../gameplay/logistics.md#emergency-repair-crates).

## Vehicles

Where the server configures them, **C-130 Cargo → Vehicles** spawns a vehicle
as cargo. It is paid for when you spawn it
(`Failed to spawn vehicle: Insufficient points. Need 10 points, have 4`), and a
type that refuses at its limit is refused at spawn too
(`Failed to spawn vehicle: the max number of Light Vehicles are already deployed`; the limit is counted per Vehicles submenu, not per vehicle).
Load and deliver it like a crate. When it lands:

```dcsmessage
Vehicle UAZ-469 airdropped and deployed
```

## Troops and downed pilots

The C-130 has a **Troops** menu only if the server gives it troop slots, and
it can carry rescued pilots only if it has pilot seats; **List Cargo** is not
on the C-130 menu, so check [Troop Transport](../f10-menu/troops.md) (the
**List** entry) if you have one. The **CSAR** menu sits inside C-130 Cargo
either way, so you can still find and smoke downed pilots for the helicopters.
See [Combat Search & Rescue](../f10-menu/csar.md).

## Planning the drop: CARP data

The campaign dashboard's **Cockpit** page (`/cockpit`) has a **CARP** tab laid
out like the C-130J's CNI-MU CARP pages. Pick the point of impact by clicking
its map or by typing the text of an F10 map mark, and give a drop altitude in
feet above the ground. It fills in what the aircraft can't know on its own:

- PI latitude, longitude, MGRS and elevation
- DZ elevation (highest ground within about 400 m) and obstruction elevation
  (highest ground out to about 1,200 m)
- drop altitude above sea level
- wind at drop altitude, surface wind (10 m above the PI) and the ballistic
  wind (the average of the two)
- temperature at the surface and at drop altitude

Type them into CARP INIT on the CNI-MU and let the aircraft compute the release
point. The page does not compute the release point itself.

| | |
|---|---|
| ![C-130 airdrop delivery via parachute](/api/wiki/images/61a90050-7359-4ada-b0c9-41f09dc26a34) | ![C-130 LAPES low altitude extraction run](/api/wiki/images/fab0866b-a54d-4e0a-9647-d246a700d5a6) |

## Tips

- **Unload far enough away.** Landing and unloading counts only more than
  100 m from where the crate was spawned. Airdrops don't have this problem.
- **Drop the whole set close together.** Everything has to be within 500 m of
  the crate that triggers the unpack.
- **Stay out of the exclusion ring.** Units can't be built within
  {{cfg:logistics_exclusion|10000}} m of your own bases. Pick a drop zone
  between bases, or at the enemy base you are attacking.
- **Plan your route.** A C-130 is big, slow and defenceless. Check the
  [EWR](../f10-menu/ewr.md) before you go, fly low, and ask for an escort near
  contested airspace.
- **Call your drop.** Announce the run on comms. A C-130 on a drop run is
  committed and very easy to shoot.

## See Also

- [Cargo Operations](../f10-menu/cargo.md)
- [Running Crates & Building a Base](../playbooks/running-crates.md)
- [Logistics & Supply](../gameplay/logistics.md)
- [Carrier Operations](../gameplay/carrier-ops.md)
