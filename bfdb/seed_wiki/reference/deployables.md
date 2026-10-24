# Deployable Units Reference

Every deployable ground unit and troop type configured on **the server selected in the
top bar** — crate requirements and weights, unit caps, and whether the unit brings a
JTAC, an EWR or a GCI station with it.

These tables are generated from that server's own campaign config when the page loads.
Each DCS server this wiki fronts runs its own config and they are not alike: one
campaign fields 27 Blue deployables, another 16, with different vehicles in both. If
you switch servers in the top bar, these tables change with you.

See [Deployables Guide](../advanced/deployables-guide.md) for what each category does on
the battlefield and when to call for one.

**Reading the tables**

- **Crates required** lists each crate type, how many of it you need, and the weight of
  *one* crate. Multiply to get the tonnage you have to fly in — across as many sorties
  as it takes.
- **Limit** is how many of that unit your coalition may have alive at once. At the cap,
  **DeleteOldest** removes your oldest one to make room.
- **Provides** flags a deployment that also acts as a JTAC, an EWR radar, or a GCI
  station.

---

## How to Deploy

1. Load crates onto a cargo helicopter/aircraft at a friendly objective.
2. Fly the crates to the deployment location.
3. Unload every required crate for the unit (see "Crates required").
4. The unit spawns automatically once the last required crate is delivered.

**Drop parameters**: exceeding a crate's max drop height or speed when unloading fails
the drop — the game reports the exact limits back to you when you try.

**Threatened objectives**:

- ⚠️ You **cannot** deploy troops or crates at a threatened objective.
- Wait for it to clear ({{cfg:threatened_cooldown|300}} second cooldown after enemies
  leave).
- A white circle = capturable; you can still deploy there if you own it.

---

## Blue deployables

{{list:deployables.Blue|_This server publishes no deployable list. Check the F10 → Deploy menu in game._}}

## Red deployables

{{list:deployables.Red|_This server publishes no deployable list. Check the F10 → Deploy menu in game._}}

---

## Troop types

Troops are loaded at friendly objectives and carried by helicopter or ground vehicle.
The **weight** column is what the squad adds to your aircraft as internal cargo — it
directly affects hover performance and useful load. Heavier squads mean fewer per
airframe.

### Blue troops

{{list:troops.Blue|_This server publishes no troop list._}}

### Red troops

{{list:troops.Red|_This server publishes no troop list._}}

**Notes**:

- Rifle, anti-tank and mortar squads can all **capture objectives**; MANPADS teams
  (Igla / Stinger) cannot. The "Can capture" column above is the authority — it is read
  from the server's config, not from this sentence.
- Heavier squads = fewer fit in a transport. A half-fuel SA342L Gazelle can lift one
  MANPAD team (150 kg); a rifle or mortar squad needs a Huey or larger.
- See [Troop Transport](../f10-menu/troops.md) and
  [Cargo Operations](../f10-menu/cargo.md) for aircraft troop/crate slot counts.

---

## General Notes

### Repair Crates

Multi-component systems accept a repair crate that restores a damaged deployment for
free. Everything else must be redeployed from scratch if destroyed. Which units have one
is part of the deployable's own config; the F10 menu shows a **Repair** entry for any
deployment that accepts it.

### Unit Limits

At the per-type maximum, **DeleteOldest** removes your oldest deployed unit of that type
to make room for the new one.

---

## See Also

- [Deployables Guide](../advanced/deployables-guide.md) — what each category does, and when to call for it
- [Cargo Operations](../f10-menu/cargo.md) — how to transport crates
- [Troop Transport](../f10-menu/troops.md) — loading and moving infantry
- [Actions Menu](../f10-menu/actions.md) — air deployments
- [Points and Lives](../gameplay/points-and-lives.md) — earning points
