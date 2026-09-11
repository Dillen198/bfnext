# Playbook: Running Crates & Building a Base

Logistics wins campaigns. Every SAM, tank, artillery piece, EWR, and JTAC
vehicle on the map got there because someone flew the crates out. This is also
the single easiest way for a new player to earn points and matter. Reference
pages: [Cargo Operations](../f10-menu/cargo.md),
[Deployables Guide](../advanced/deployables-guide.md),
[Deployable Units Reference](../reference/deployables.md).

## Two different jobs

| Job | What it does |
|---|---|
| **Resupply run** | Fly generic supply crates to a friendly objective that's low on supply/fuel. Keeps repairs and the garrison running. |
| **Deployable build** | Fly the specific number of crates a unit needs (e.g. 3 for an MBT, 4 for a big SAM) to a spot, then unpack them into that unit. |

## What can carry crates

| Aircraft | Crate slots |
|---|---|
| CH-47 Chinook | 6 |
| Mi-8 | 3 |
| UH-1H Huey | 2 |
| SA342L Gazelle / SA342 Minigun | 1 |
| Mi-24P | 1 |
| C-130 (airdrop) | multiple — see [C-130 & Airdrop](../advanced/c130-airdrop.md) |

The **SA342M** and **SA342 Mistral** cannot carry crates — no cabin.

## Resupply run — step by step

1. Land at a friendly airbase or FARP (within ~50 m of the objective).
2. `F10 → Cargo → Load` → pick a supply crate.
3. Fly to a forward friendly objective that's low — check
   [Live Ops → OBJECTIVES](./reading-live-ops.md) and sort by supply.
4. Land at the destination. It must **not be threatened** — no enemies nearby,
   or you'll get *"you can't deploy … while enemies are near."*
5. `F10 → Cargo → Unload` (or "Unload All"). Supply transfers to the objective.

That's it. You just raised that base's repair rate and kept its garrison fed.
You also earn logistics score for it.

## Deployable build — step by step

1. **Decide what and where.** A radar SAM to cover an objective? An EWR on an
   approach lane? A JTAC vehicle to unlock CAS on a position? See
   [Deployables Guide](../advanced/deployables-guide.md) for what each does.
2. **Check the crate count and cost.** The
   [Deployable Units Reference](../reference/deployables.md) lists crates,
   point cost, and unit limits per type on this server. Example: most MBTs need
   **3 crates**; multi-part SAMs need **3–4**.
3. **Load and fly.** `F10 → Cargo → Load`. Big builds mean multiple trips or a
   CH-47 — or drop several at once with the
   [C-130 airdrop system](../advanced/c130-airdrop.md).
4. **Drop the crates together** at the build location. Land, `F10 → Cargo →
   Unload`, repeat until the full count is on the ground in one cluster.
5. **Unpack.** With all required crates down, the Cargo menu offers an unpack /
   assemble option for that unit — select it and the vehicle spawns.
6. **Bind it if you want to move it:** `-bind <group-id>`, then
   `F10 → Actions>> → Move (Units/Troops)` to reposition (the cost is in the menu label).

## Repair crates

Multi-component SAMs (Kub, SA-3, HQ-7, Hawk, IRIS-T SLM) and captured carriers
take **repair crates** — fly one out to restore battle damage instead of
rebuilding the whole system from scratch. Much cheaper than redeploying.

## Where to build

- **Fresh captured base:** it has no SAMs and a skeleton garrison — SAM + AAA
  crates here are the highest-value delivery on the map.
- **Logistics hubs:** worth heavy SAM cover; losing one starves a whole sector.
- **Approach lanes:** an EWR (500 km detection) far forward gives your whole
  side early warning.
- **Contested objective:** IFV/APC crates to screen a landing zone, AAA to
  punish enemy helos.

## Common mistakes

- **Dropping crates too far apart** — they need to be clustered to unpack.
- **Trying to load from a threatened/capturable base** — its logistics are cut;
  stage from further back.
- **Forgetting the count** — 2 of 3 crates does nothing; you need all 3 down.
- **Building a radar SAM with no AAA backup** — one SEAD flight and it's gone.

## See also

- [Cargo Operations](../f10-menu/cargo.md)
- [C-130 Hercules & Airdrop](../advanced/c130-airdrop.md)
- [Logistics & Supply](../gameplay/logistics.md)
- [Capturing a Base](./capturing-a-base.md)
