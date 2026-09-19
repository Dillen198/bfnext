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
- **Artillery/MLRS** (RED: 2S19 Msta, PLZ-05, Smerch CM/HE, TOS-1A, 9K57; BLUE: Firtina 155MM, Dana 152MM, HIMARS GMLRS HE/CM, HIMARS ATACMS HE/CM) sit behind the front line and reach out to suppress or destroy troops and vehicles massing at a contested objective without exposing themselves to direct fire. HE variants are for point targets and structures; CM (cluster munition) variants are for soft/area targets.

## JTAC and Recon Vehicles

A dedicated recon/JTAC vehicle (RED: Scout BRDM, 8 km range, no LOS required; BLUE: MRAP JTAC, 5 km range, no LOS required) is its own deployable — separate from ground troops — and gives an objective a standing spotter that can call CAS through terrain, without needing a player to hold laser designation. Deploy one to unlock CAS tasking on a position before committing other forces.

## EWR and GCI

**EWR** (RED: EWR 55G6; BLUE: AN/FPS-117) both provide a 500 km detection radius when deployed — by far the longest-ranged sensor available, useful for covering approach vectors far outside any SAM's own detection range. RED additionally has a **GCI Station** (MiG-29 GCI template) with no listed EWR range of its own — check the "GCI Stations" category in the reference table for the specific server's configured behavior before relying on it for radar coverage.

## Logistics

**Ammo Truck** (both coalitions) is the one purely support-role deployable — no weapon, no sensor, just resupply capacity for nearby units. It's cheap relative to combat units and has a high unit limit (20–30), so keeping one forward of a contested objective is rarely a bad use of a cargo run.

## Choosing What to Deploy

- **Screening a landing zone against helicopters**: an IR SAM or AAA piece — cheap, fast, punishes the low-altitude approach.
- **Denying fixed-wing CAS over an objective**: a radar SAM (Hawk/IRIS-T SLM for BLUE, Kub/SA-3/HQ-7 for RED), backed by AAA so SEAD alone doesn't clear the sky.
- **Reinforcing a contested capture**: IFVs first (cheap, fast, high unit limit), MBTs if the fight is expected to last.
- **Hitting a target beyond your own defensive range**: HIMARS/artillery, not a direct ground push.
- **Enabling CAS tasking on a position**: a JTAC/recon vehicle before committing other forces.

## See Also

- [Deployable Units Reference](../reference/deployables.md) — full crate/limit/range tables pulled from the live server config
- [Cargo Operations](../f10-menu/cargo.md) — how to transport crates
- [JTAC System](../f10-menu/jtac.md) — tasking CAS onto deployed units
