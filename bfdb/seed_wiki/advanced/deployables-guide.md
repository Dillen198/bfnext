# Deployables Guide

The reference tables in [Deployable Units Reference](../reference/deployables.md) list every unit's crate requirements, drop limits, and EWR/JTAC ranges as configured on this server — this page explains what each category actually *does* on the battlefield and when to call for one over another.

![SA-22 Pantsir air defense vehicle with autocannons and missile canisters raised](/api/wiki/images/f5701fbb-6bec-4db2-b96b-7bd33d41880f)

## Air Defense

Air defense deployables exist to deny the sky over an objective. They fall into three families with very different jobs:

**Radar SAMs** (RED: SA-6 Kub, SA-3, SA-15 Tor, HQ-7, SA-8 Osa; BLUE: Roland, Hawk System, IRIS-T SLM) actively scan and can engage from several kilometers out — Hawk and IRIS-T SLM reach the furthest (60 km / 30 km EWR range respectively) but their radar emissions are detectable, so a HARM or SEAD strike can find and kill the radar or launch component before the system matters. Multi-component systems (Kub, SA-3, HQ-7, Hawk, IRIS-T SLM) support a **repair crate** to restore battle damage for free instead of redeploying from scratch.

**IR SAMs / short-range** (RED: SA-9 Strela; BLUE: Avenger, Linebacker) are short-range but don't rely on a big search radar, so they punish low-altitude helicopter and CAS runs that think they've dodged the radar coverage.

**AAA** (RED: ZSU-57-2, SPAA HL Zu-23, ZSU-23 Shilka, SA-19 Tunguska, SA-22 Pantsir; BLUE: C-RAM, Flakpanzergepard, Vulkan) is the cheapest, fastest-to-deploy layer — pure gun systems with no missile reach, but effective against anything that gets close, including incoming rockets and artillery shells in C-RAM's case.

## Armor and IFVs

Ground vehicles are the objective's close-in muscle and the backbone of any counter-attack:

- **IFVs/APCs** (RED: BMPT Terminator, ZBD-04A, BTR-82A; BLUE: M2A2 Bradley, M1130 Stryker, MRAP MaxxPro, MRAP M-ATV) are cheap, fast to deploy in numbers (limits of 20 on most types), and good for screening a landing zone or contesting a capture in progress.
- **MBTs** (RED: ZTZ-96B, T-84 Oplot, T-90M; BLUE: Leopard 2A6M, M1A2C Abrams, Merkava IV) cost more crates (3 per unit) and have a lower unit limit (15), but hit harder and survive longer — they're built to slug it out at the objective itself. Merkava IV is the only MBT on this server with a listed JTAC range (8 km, LOS required).
- **Artillery/MLRS** (what each side fields depends on the campaign — see
  [Deployables](../reference/deployables.md) for the live list on the server you
  have selected; the 2008 Caucasus campaign is restricted to period Soviet and
  Czech guns, the modern Syria one is not) sit behind the front line and reach out to suppress or destroy troops and vehicles massing at a contested objective without exposing themselves to direct fire. HE variants are for point targets and structures; CM (cluster munition) variants are for soft/area targets.

## JTAC and Recon Vehicles

A dedicated recon/JTAC vehicle (RED: Scout BRDM, 8 km range, no LOS required; BLUE: MRAP JTAC, 5 km range, no LOS required) is its own deployable — separate from ground troops — and gives an objective a standing spotter that can call CAS through terrain, without needing a player to hold laser designation. Deploy one to unlock CAS tasking on a position before committing other forces.

## EWR and GCI

**EWR** (RED: EWR 55G6; BLUE: AN/FPS-117) both provide a 500 km detection radius when deployed — by far the longest-ranged sensor available, useful for covering approach vectors far outside any SAM's own detection range. RED additionally has a **GCI Station** (MiG-29 GCI template) with no listed EWR range of its own — check the "GCI Stations" category in the reference table for the specific server's configured behavior before relying on it for radar coverage.

## Logistics

**Ammo Truck** (both coalitions) is the one purely support-role deployable — no weapon, no sensor, just resupply capacity for nearby units. It's cheap relative to combat units and has a high unit limit (20–30), so keeping one forward of a contested objective is rarely a bad use of a cargo run.

## Fat Cow (heavy-lift FARP)

*Only on servers that configure one.* A Fat Cow is a forward arming and
refuelling point you fly in yourself: a heavy-lift helicopter lands, and a pad
with fuel, weapons and ground crew is laid out around it. Both sides can have
one, up to the server's limit.

**Building one**

1. In a CH-47 or Mi-8, spawn the kit from **F10 → Cargo → Crates → Fat Cow**:
   a Fat Cow Fuel Bladder (1,500 kg) and a Fat Cow Ammo Pallet (1,000 kg).
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
other FARP. The Fat Cow's own zone is small: 500 m by default.

**Finite stock.** No hub supplies a Fat Cow. It holds only what it was built
with (by default 9,000 kg of jet fuel, plus Hellfires and Hydras for Blue, or
Vikhrs, Shturms and S-8s for Red). When that runs out it's dry, unless someone
flies a [supply transfer crate](../gameplay/logistics.md#supply-transfer-crates)
to it.

**Packing up and moving.** Land inside the Fat Cow's zone and pick
**F10 → Cargo → Pack Up Fat Cow**. The FARP closes and its crates are put down
next to your helicopter, carrying whatever fuel and weapons it had left. Load
them, fly to the new site and unpack them. It comes back with that leftover
stock, and you aren't charged again. Packed crates don't survive a server
restart, so set it back up before one.

**Destroying one.** Destroy both of its fuel points and the Fat Cow closes.

**Fat Cow (AI).** Where the server offers it, the **Fat Cow (AI)** action
does the flying for you: drop a map mark, buy the action, and an AI heavy-lift
helicopter takes off from the nearest friendly field, lands at the mark and
sets the Fat Cow up around itself. It stays 20 minutes by default, then packs
up and flies home. If the helicopter is shot down on station, the Fat Cow
closes with it. You get your points back if it's lost or can't set up before
it starts work. See [Action Types](../reference/action-types.md#fat-cow-ai).

## Choosing What to Deploy

- **Screening a landing zone against helicopters**: an IR SAM or AAA piece — cheap, fast, punishes the low-altitude approach.
- **Denying fixed-wing CAS over an objective**: a radar SAM (Hawk/IRIS-T SLM for BLUE, Kub/SA-3/HQ-7 for RED), backed by AAA so SEAD alone doesn't clear the sky.
- **Reinforcing a contested capture**: IFVs first (cheap, fast, high unit limit), MBTs if the fight is expected to last.
- **Hitting a target beyond your own defensive range**: artillery or MLRS, not a direct ground push.
- **Enabling CAS tasking on a position**: a JTAC/recon vehicle before committing other forces.
- **Keeping attack helicopters in the fight far from base**: a Fat Cow, where the server has one.

## See Also

- [Deployable Units Reference](../reference/deployables.md) — full crate/limit/range tables pulled from the live server config
- [Cargo Operations](../f10-menu/cargo.md) — how to transport crates
- [JTAC System](../f10-menu/jtac.md) — tasking CAS onto deployed units
