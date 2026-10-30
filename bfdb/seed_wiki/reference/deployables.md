# Deployable Units Reference

Every deployable ground unit and troop type configured on **the server selected in the
top bar**: crate requirements and weights, unit caps, and whether the unit brings a
JTAC, an EWR or a GCI station with it.

These tables are generated from that server's own campaign config when the page loads.
Each DCS server this wiki fronts runs its own config and they are not alike: one
campaign fields 27 Blue deployables, another 16, with different vehicles in both. If
you switch servers in the top bar, these tables change with you.

See [Deployables Guide](../advanced/deployables-guide.md) for what each category does on
the battlefield and when to call for one.

**Reading the tables**

- **Crates required** lists each crate type, how many of it you need, and the weight of
  *one* crate. Multiply to get the tonnage you have to fly in, across as many sorties
  as it takes.
- **Limit** is how many of that unit your **side** may have at once. At the limit a
  unit either refuses new ones or replaces your side's oldest, depending on its
  setting (**DenyCrate** or **DeleteOldest**).
- **Provides** flags a deployment that also acts as a JTAC, an EWR radar, or a GCI
  station.

---

## How to deploy

1. Spawn the crates inside the zone of a friendly base with working logistics. They
   are issued from that base's stock.
2. Fly them to the build site: well away from your own bases (at least
   {{cfg:logistics_exclusion|10000}} m from any friendly objective's centre), or next
   to an enemy base you are attacking.
3. Put every required crate down close together.
4. Unpack: **F10 → Cargo → Unpack Nearby Crate(s)** on the classic or helicopter
   cargo menu. Crates dropped from a C-130 (and helicopter crates, where the server
   turns on auto-unpack) unpack themselves once the whole set has landed.
5. The unit's price is taken at unpack, from you first and then from the fund of the
   base the crates came from.

**Dropping from a hover** (classic menu): each crate has a maximum height and speed
for **Unload Crate**. Go over either and the drop is refused with both limits in the
message, for example
`you are going too fast to unload your cargo, speed must be at or below 46 km/h, and altitude agl must be at or below 10 m`.
Dynamic cargo and C-130 drops have no such check.

**Threatened bases** don't stop crates. They stop **troops** being unloaded inside
your own threatened base (`you can't deploy troops here while enemies are near`), for
{{cfg:threatened_cooldown|300}} seconds after the last enemy is out of sight.

The full rules, failures and messages are on
[Cargo Operations](../f10-menu/cargo.md#unpacking-the-rules).

---

## Blue deployables

{{list:deployables.Blue|_This server publishes no deployable list. Check F10 → Cargo → Crates in game._}}

## Red deployables

{{list:deployables.Red|_This server publishes no deployable list. Check F10 → Cargo → Crates in game._}}

---

## Troop types

Troops are loaded at friendly objectives and carried by helicopter.
The **weight** column is what the squad adds to your aircraft as internal cargo: it
directly affects hover performance and useful load.

### Blue troops

{{list:troops.Blue|_This server publishes no troop list._}}

### Red troops

{{list:troops.Red|_This server publishes no troop list._}}

**Notes**:

- Only squads marked as able to capture can take an objective; MANPAD teams usually
  can't. The "Can capture" column above is the authority: it is read from the
  server's config, not from this sentence.
- A squad's price is paid when you load it, and **Troops → Return** at a friendly base
  refunds what is left of it.
- See [Troop Transport](../f10-menu/troops.md) and
  [Cargo Operations](../f10-menu/cargo.md#what-each-airframe-carries) for slot counts
  per airframe.

---

## General notes

### Repair crates

Units with a repair crate can be repaired in the field: put the repair crate down
within {{cfg:crate_spread|250}} m of the damaged unit and unpack. The repair crate is
listed in the unit's own crate menu. Everything else must be rebuilt from scratch if
destroyed.

### Unit limits

Limits count your whole side, not just your own units. With **DeleteOldest**, the
side's oldest unit of that type is removed to make room for the new one; with
**DenyCrate**, the unpack is refused until one is lost or deleted.

---

## See Also

- [Deployables Guide](../advanced/deployables-guide.md) - what each category does, and when to call for it
- [Cargo Operations](../f10-menu/cargo.md) - how to transport crates
- [C-130 Hercules & Airdrop](../advanced/c130-airdrop.md)
- [Troop Transport](../f10-menu/troops.md) - loading and moving infantry
- [Actions Menu](../f10-menu/actions.md) - air deployments
- [Points and Lives](../gameplay/points-and-lives.md) - earning points
