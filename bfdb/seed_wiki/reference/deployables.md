# Deployable Units Reference

Complete listing of every deployable ground unit currently configured on this server, pulled directly from the live `ODFv2_CFG` campaign config — including crate requirements, drop limits, EWR/JTAC ranges, and repair options. Each unit has its own image. See [Deployables Guide](../advanced/deployables-guide.md) for what each category does on the battlefield.

**A note on cost**: every deployable on this server is currently configured at **0 points (FREE)** — the point economy in [Points and Lives](../gameplay/points-and-lives.md) is spent elsewhere (kills, captures, logistics). If your server charges points for deployables, check your own `ODFv2_CFG`.

---

## How to Deploy

1. Load cargo helicopter/aircraft with crates at friendly objective
2. Transport crates to deployment location
3. Unload crates (must drop all required crates for unit type)
4. Unit spawns automatically when all crates are delivered

**Drop Parameters**: exceeding a crate's max drop height or speed when unloading fails the drop — the game reports the exact limits back to you at the moment you try.

**Threatened Objectives**:
- ⚠️ **CANNOT deploy** troops or crates at threatened objectives!
- Error: "you can't deploy troops here while enemies are near"
- Wait for objective to clear (5 minute cooldown after enemies leave)
- Check for white circle = capturable (can still deploy there if you own it)

---

## RED COALITION DEPLOYABLES

### SAM - Medium Range

| Image | Unit | Crates | Max | Drop Height | Drop Speed | EWR Range | JTAC Range | Repair |
|---|---|---|---|---|---|---|---|---|
| ![](/api/wiki/images/d6048837-b802-43a7-b5a8-82ae93519119) | **SA 6 Kub** | 3x "Kub Launcher" (2000kg) + 1x "Kub Radar" (2000kg) | 8 | 10m max | 47 km/h (~25 kts) max | 30 km | — | 1x "Kub Repair" (0 pts) |
| ![](/api/wiki/images/2e67633d-559c-49e5-b275-f90617da3542) | **SA 3** | 2x "SA3 Launcher" (2000kg) + 1x "SA3 Track Radar" (2000kg) + 1x "SA3 Search Radar" (1500kg) | 8 | 20m max | 1260 km/h (~680 kts) max | 30 km | — | 1x "SA3 Repair" (0 pts) |
| ![](/api/wiki/images/a8b1e563-96fa-4076-8103-e8def4a1f4fe) | **SA15 Tor** | 3x "SA15 Tor" (2000kg) | 8 | 10m max | 47 km/h (~25 kts) max | 20 km | — | — |

### SAM - Short Range

| Image | Unit | Crates | Max | Drop Height | Drop Speed | EWR Range | JTAC Range | Repair |
|---|---|---|---|---|---|---|---|---|
| ![](/api/wiki/images/4a3906e7-dabb-4cbb-b026-cf280f147a73) | **HQ-7** | 1x "HQ7 LN" (2000kg) + 1x "HQ7 Radar" (1500kg) | 12 | 20m max | 1260 km/h (~680 kts) max | 20 km | — | 1x "HQ7 Repair" (0 pts) |
| ![](/api/wiki/images/dffa56a2-278a-4365-b5b7-2cd1123f97ac) | **SA 8 Osa** | 2x "SA8 Osa" (2000kg) | 10 | 10m max | 47 km/h (~25 kts) max | 20 km | — | — |
| ![](/api/wiki/images/da4b4679-49ba-4b76-8586-ad9776510eda) | **SA 9 Strela** | 2x "SA9 Strela" (2000kg) | 10 | 10m max | 47 km/h (~25 kts) max | 20 km | — | — |

### AAA

| Image | Unit | Crates | Max | Drop Height | Drop Speed | EWR Range | JTAC Range | Repair |
|---|---|---|---|---|---|---|---|---|
| ![](/api/wiki/images/2d9d2e88-c814-4dd4-8a7d-de179534b673) | **ZSU-57-2** | 1x "ZSU-57-2" (1000kg) | 15 | 10m max | 47 km/h (~25 kts) max | — | — | — |
| ![](/api/wiki/images/ee8e0a26-d61e-46e7-8460-3e9d4b90b357) | **SPAA HL Zu-23** | 1x "SPAA HL Zu-23" (1000kg) | 15 | 10m max | 47 km/h (~25 kts) max | — | — | — |
| ![](/api/wiki/images/f8fdce84-8d81-4a3d-a430-84764715c683) | **ZSU-23 Shilka** | 1x "SPAA ZSU-23 Shilka" (2000kg) | 15 | 10m max | 47 km/h (~25 kts) max | — | — | — |
| ![](/api/wiki/images/60162e11-8a31-4baa-a7bf-caff975e7c48) | **SA-19 Tunguska** | 2x "SA-19 Tunguska" (2000kg) | 12 | 10m max | 47 km/h (~25 kts) max | — | — | — |
| ![](/api/wiki/images/f5701fbb-6bec-4db2-b96b-7bd33d41880f) | **SA-22 Pantsir** | 3x "SA-22 Pantsir" (2000kg) | 10 | 10m max | 47 km/h (~25 kts) max | — | — | — |

### Artillery

| Image | Unit | Crates | Max | Drop Height | Drop Speed | EWR Range | JTAC Range | Repair |
|---|---|---|---|---|---|---|---|---|
| ![](/api/wiki/images/23c6013f-a01a-494c-bac2-b0f26b8c7c16) | **SPH 2S19 Msta** | 2x "SPH 2S19 Msta 152MM" (2000kg) | 15 | 10m max | 47 km/h (~25 kts) max | — | — | — |
| ![](/api/wiki/images/317bf8d0-c5a5-4b2c-87cb-10687accec80) | **PLZ-05** | 2x "PLZ-05" (2000kg) | 15 | 10m max | 47 km/h (~25 kts) max | — | — | — |
| ![](/api/wiki/images/6d527976-956d-40a2-80c5-05eae20e22fd) | **MLRS Smerch CM** | 3x "MLRS Smerch CM" (2000kg) | 10 | 10m max | 47 km/h (~25 kts) max | — | — | — |
| ![](/api/wiki/images/16e38e13-092d-49bb-bc9f-9f59332e3dd4) | **MLRS Smerch HE** | 3x "MLRS Smerch HE" (2000kg) | 10 | 10m max | 47 km/h (~25 kts) max | — | — | — |
| ![](/api/wiki/images/2c85e167-d84c-4006-983c-7db60d6af208) | **MLRS TOS-1A** | 2x "MLRS TOS-1A" (2000kg) | 15 | 10m max | 47 km/h (~25 kts) max | — | — | — |
| ![](/api/wiki/images/8b17a846-92f0-41d4-81aa-f1d74c628334) | **MLRS 9K57** | 2x "MLRS 9K57" (2000kg) | 15 | 10m max | 47 km/h (~25 kts) max | — | — | — |

### MBT

| Image | Unit | Crates | Max | Drop Height | Drop Speed | EWR Range | JTAC Range | Repair |
|---|---|---|---|---|---|---|---|---|
| ![](/api/wiki/images/11039281-ca3b-4abf-bf03-0daba63cd8d4) | **ZTZ-96B** | 3x "ZTZ-96B" (2000kg) | 15 | 10m max | 47 km/h (~25 kts) max | — | — | — |
| ![](/api/wiki/images/91c70b1e-de29-4a31-bc78-908080a7142c) | **T-84 Oplot** | 3x "T-84" (2000kg) | 15 | 10m max | 47 km/h (~25 kts) max | — | — | — |
| ![](/api/wiki/images/7aeaf7eb-49c9-423f-a5ce-9b54e4253411) | **T-90M** | 3x "T-90M" (2000kg) | 15 | 10m max | 47 km/h (~25 kts) max | — | — | — |

### IFV

| Image | Unit | Crates | Max | Drop Height | Drop Speed | EWR Range | JTAC Range | Repair |
|---|---|---|---|---|---|---|---|---|
| ![](/api/wiki/images/eab7a04b-4942-4298-9683-86659465fefd) | **BMPT Terminator** | 2x "BMPT Terminator" (2000kg) | 20 | 10m max | 47 km/h (~25 kts) max | — | — | — |
| ![](/api/wiki/images/4e6c5c99-bb1b-434d-b177-b5e9994bbd9a) | **ZBD-04A** | 1x "ZBD-04A" (2000kg) | 20 | 10m max | 47 km/h (~25 kts) max | — | — | — |
| ![](/api/wiki/images/e0034850-3e94-41c0-8424-b7e0e49659e7) | **BTR-82A** | 1x "BTR-82A" (2000kg) | 20 | 10m max | 47 km/h (~25 kts) max | — | — | — |

### JTAC

| Image | Unit | Crates | Max | Drop Height | Drop Speed | EWR Range | JTAC Range | Repair |
|---|---|---|---|---|---|---|---|---|
| ![](/api/wiki/images/149efbdc-bab4-4572-91a8-72318aa8bad4) | **Scout BRDM** | 1x "Scout BRDM" (2000kg) | 20 | 10m max | 47 km/h (~25 kts) max | — | 8 km (no LOS) | — |

### Logistics

| Image | Unit | Crates | Max | Drop Height | Drop Speed | EWR Range | JTAC Range | Repair |
|---|---|---|---|---|---|---|---|---|
| ![](/api/wiki/images/44ba8543-63f5-4166-91c7-7203ee7316b3) | **Ammo Truck** | 2x "Ammo Truck" (2000kg) | 30 | 10m max | 47 km/h (~25 kts) max | — | — | — |

### EWRs

| Image | Unit | Crates | Max | Drop Height | Drop Speed | EWR Range | JTAC Range | Repair |
|---|---|---|---|---|---|---|---|---|
| ![](/api/wiki/images/89410024-5e16-4f75-b21f-a9db3f7d1152) | **EWR 55G6** | 1x "EWR 55G6" (2000kg) | 20 | 10m max | 47 km/h (~25 kts) max | 500 km | — | — |

### GCI Stations

| Image | Unit | Crates | Max | Drop Height | Drop Speed | EWR Range | JTAC Range | Repair |
|---|---|---|---|---|---|---|---|---|
| ![](/api/wiki/images/71ed5ded-b182-4421-8eef-7b672d1f6123) | **MiG-29 GCI** | 2x "GCI Station Crate" (2000kg) | 4 | 20m max | 1260 km/h (~680 kts) max | — | — | — |

---

## BLUE COALITION DEPLOYABLES

### SAM - Short Range

| Image | Unit | Crates | Max | Drop Height | Drop Speed | EWR Range | JTAC Range | Repair |
|---|---|---|---|---|---|---|---|---|
| ![](/api/wiki/images/e43d3f79-d1bc-43bb-ac4d-bfb387ea24df) | **Roland** | 2x "Roland ADS" (1000kg) | 10 | 10m max | 47 km/h (~25 kts) max | 10 km | — | — |
| ![](/api/wiki/images/6c74c2ec-9b84-4d02-b050-642f1f915fcd) | **Avenger** | 2x "Avenger" (1000kg) | 10 | 10m max | 47 km/h (~25 kts) max | — | — | — |
| ![](/api/wiki/images/2fcf4a40-7cf9-407d-920f-47ef946bbc1d) | **Linebacker** | 1x "Linebacker" (1000kg) | 10 | 10m max | 47 km/h (~25 kts) max | — | — | — |

### SAM - Medium Range

| Image | Unit | Crates | Max | Drop Height | Drop Speed | EWR Range | JTAC Range | Repair |
|---|---|---|---|---|---|---|---|---|
| ![](/api/wiki/images/4faa9027-f574-4e0b-8ee0-1e60213ad1b5) | **Hawk System** | 3x "Hawk Launcher" (1000kg) + 1x "Hawk Search Radar" (1000kg) + 1x "Hawk Track Radar" (1000kg) + 1x "Hawk CC" (1000kg) | 8 | 10m max | 47 km/h (~25 kts) max | 60 km | — | 1x "Hawk Repair" (0 pts) |
| ![](/api/wiki/images/4118d2bb-b773-4505-bf2d-c2a919251dab) | **IRIS-T SLM** | 1x "IRIST C2" (1000kg) + 1x "IRIST STR" (1001kg) + 2x "IRIST LN" (1500kg) | 8 | 20m max | 1260 km/h (~680 kts) max | 30 km | — | 1x "IRIST REPAIR" (0 pts) |

### AAA

| Image | Unit | Crates | Max | Drop Height | Drop Speed | EWR Range | JTAC Range | Repair |
|---|---|---|---|---|---|---|---|---|
| ![](/api/wiki/images/e60e2222-265d-4886-b701-294b9a9f19da) | **C-RAM** | 1x "C-RAM" (1000kg) | 15 | 10m max | 47 km/h (~25 kts) max | — | — | — |
| ![](/api/wiki/images/bf0104cd-413c-4203-9c5f-39bfcd8409f1) | **Flakpanzergepard** | 1x "Flakpanzergepard" (1000kg) | 12 | 10m max | 47 km/h (~25 kts) max | — | — | — |
| ![](/api/wiki/images/6ee596b9-f1a0-4771-bdaf-b57d9b3d83bf) | **Vulkan** | 1x "Vulkan" (1000kg) | 12 | 10m max | 47 km/h (~25 kts) max | — | — | — |

### Artillery

| Image | Unit | Crates | Max | Drop Height | Drop Speed | EWR Range | JTAC Range | Repair |
|---|---|---|---|---|---|---|---|---|
| ![](/api/wiki/images/6e70c2cb-ed58-42b1-a5bd-c46646f476d2) | **Firtina 155MM** | 2x "Firtina 155MM" (1000kg) | 15 | 10m max | 47 km/h (~25 kts) max | — | — | — |
| ![](/api/wiki/images/5427e732-a297-4f7c-bd92-98f163a091bc) | **Dana 152MM** | 2x "Dana 152MM" (1000kg) | 15 | 10m max | 47 km/h (~25 kts) max | — | — | — |
| ![](/api/wiki/images/2e9da600-e13e-4d2a-8aa8-f9b24b7f0cf5) | **Himars GMLRS HE** | 2x "Himars GMLRS HE" (1000kg) | 10 | 10m max | 47 km/h (~25 kts) max | — | — | — |
| ![](/api/wiki/images/eea9db79-5a8f-4748-8a95-e387dc8efff3) | **Himars GMLRS CM** | 2x "Himars GMLRS CM" (1000kg) | 10 | 10m max | 47 km/h (~25 kts) max | — | — | — |
| ![](/api/wiki/images/558de980-65ad-4ef3-aa59-340f18f204be) | **Himars ATACMS HE** | 2x "Himars ATACMS HE" (1000kg) | 10 | 10m max | 47 km/h (~25 kts) max | — | — | — |
| ![](/api/wiki/images/110d1c99-6d73-45a6-b83a-088323d1abe4) | **Himars ATACMS CM** | 2x "Himars ATACMS CM" (1000kg) | 10 | 10m max | 47 km/h (~25 kts) max | — | — | — |

### IFV

| Image | Unit | Crates | Max | Drop Height | Drop Speed | EWR Range | JTAC Range | Repair |
|---|---|---|---|---|---|---|---|---|
| ![](/api/wiki/images/8f83a8f8-4838-499b-853e-0b78226e0c83) | **M2A2 Bradley** | 1x "M2A2 Bradley" (1000kg) | 20 | 10m max | 47 km/h (~25 kts) max | — | — | — |
| ![](/api/wiki/images/a4c29214-9c23-4d3f-8d93-4c3e27cb8de2) | **M1130 Stryker** | 1x "M1130 Stryker" (1000kg) | 20 | 10m max | 47 km/h (~25 kts) max | — | — | — |

### APC

| Image | Unit | Crates | Max | Drop Height | Drop Speed | EWR Range | JTAC Range | Repair |
|---|---|---|---|---|---|---|---|---|
| ![](/api/wiki/images/884ca354-0f5d-4670-970b-097d1efeb757) | **MRAP MaxxPro** | 1x "MRAP MaxxPro" (1000kg) | 20 | 10m max | 47 km/h (~25 kts) max | — | — | — |
| ![](/api/wiki/images/3a8cac53-22e7-4b44-a715-3c4cb5933154) | **MRAP M-ATV** | 1x "MRAP M-ATV" (1000kg) | 20 | 10m max | 47 km/h (~25 kts) max | — | — | — |

### JTAC

| Image | Unit | Crates | Max | Drop Height | Drop Speed | EWR Range | JTAC Range | Repair |
|---|---|---|---|---|---|---|---|---|
| ![](/api/wiki/images/224ac803-00f4-42c5-ac6b-82aab5858a2a) | **MRAP JTAC** | 1x "MRAP JTAC" (1000kg) | 20 | 10m max | 47 km/h (~25 kts) max | — | 5 km (no LOS) | — |

### MBT

| Image | Unit | Crates | Max | Drop Height | Drop Speed | EWR Range | JTAC Range | Repair |
|---|---|---|---|---|---|---|---|---|
| ![](/api/wiki/images/24a37b26-36cf-4093-9fd2-ed0cb958da59) | **Leopard 2A6M** | 3x "2A6M Leopard" (1000kg) | 15 | 10m max | 47 km/h (~25 kts) max | — | — | — |
| ![](/api/wiki/images/e8c30129-b52b-4516-905e-044ddd297bfe) | **M1A2C Abrams** | 3x "M1A2C Abrams" (1000kg) | 15 | 10m max | 47 km/h (~25 kts) max | — | — | — |
| ![](/api/wiki/images/0d2814fb-186a-4f71-a0b8-5b7275c00843) | **Merkava IV** | 3x "Merkava IV" (1000kg) | 15 | 10m max | 47 km/h (~25 kts) max | — | 8 km (LOS) | — |

### Logistics

| Image | Unit | Crates | Max | Drop Height | Drop Speed | EWR Range | JTAC Range | Repair |
|---|---|---|---|---|---|---|---|---|
| ![](/api/wiki/images/44ba8543-63f5-4166-91c7-7203ee7316b3) | **Ammo Truck** | 1x "Ammo Truck" (2000kg) | 20 | 10m max | 47 km/h (~25 kts) max | — | — | — |

### EWRs

| Image | Unit | Crates | Max | Drop Height | Drop Speed | EWR Range | JTAC Range | Repair |
|---|---|---|---|---|---|---|---|---|
| ![](/api/wiki/images/fcd0f052-f904-4628-aa13-9b58a46fb2b2) | **AN/FPS-117** | 1x "AN/FPS-117" (1000kg) | 20 | 10m max | 47 km/h (~25 kts) max | 500 km | — | — |

---

## TROOP TYPES

Troops are loaded at friendly objectives and transported via helicopter or ground vehicle.

### RED COALITION TROOPS

| Image | Unit | Cost | Can Capture | Weight | Max | JTAC Range |
|---|---|---|---|---|---|---|
| ![](/api/wiki/images/cd9a77ef-9ffc-4f0e-a441-f5fdc672140b) | **Standard** | FREE | YES | 800kg/squad | 10 | 8 km (LOS) |
| ![](/api/wiki/images/ba1888f4-47d3-44f3-8170-ad7a6b41bf99) | **Anti Tank** | 1 pts | YES | 1000kg/squad | 10 | 8 km (LOS) |
| ![](/api/wiki/images/2e0dcfa4-2a2a-41b7-ae9a-157b655eeacd) | **Mortar** | 5 pts | YES | 1200kg/squad | 10 | 8 km (LOS) |
| ![](/api/wiki/images/f0751468-5d52-49cf-bc37-b7c280256624) | **Igla** | 5 pts | NO | 500kg/squad | 10 | None |

### BLUE COALITION TROOPS

| Image | Unit | Cost | Can Capture | Weight | Max | JTAC Range |
|---|---|---|---|---|---|---|
| ![](/api/wiki/images/cd9a77ef-9ffc-4f0e-a441-f5fdc672140b) | **Standard** | FREE | YES | 800kg/squad | 10 | 8 km (LOS) |
| ![](/api/wiki/images/ba1888f4-47d3-44f3-8170-ad7a6b41bf99) | **Anti Tank** | 1 pts | YES | 1000kg/squad | 10 | 8 km (LOS) |
| ![](/api/wiki/images/2e0dcfa4-2a2a-41b7-ae9a-157b655eeacd) | **Mortar** | 5 pts | YES | 1200kg/squad | 10 | 8 km (LOS) |
| ![](/api/wiki/images/eaaf82b4-0e39-444c-8925-c1d9d695e7bf) | **Stinger** | 5 pts | NO | 500kg/squad | 10 | 8 km (LOS) |

**Important Notes**:
- Standard, Anti Tank, and Mortar infantry can **ALL capture objectives**
- MANPADS (Igla/Stinger) **CANNOT capture**
- All troop JTAC tasking on this server currently requires line-of-sight (check `troops.*.jtac.nolos` in your own config if that differs)
- Heavier troops = fewer can fit in transport aircraft

---

## General Notes

### Crate Capacity
- **Mi-8**: 3 crate slots
- **CH-47**: 3 crate slots
- **UH-1H**: 1 crate slot
- **Server Max**: 6 crates total

### Unit Limits

When you reach the maximum allowed units:
- **DeleteOldest**: System automatically deletes your oldest deployed unit of that type

### Repair Crates

Systems with multiple radar/launcher components (SA-6 Kub, SA-3, HQ-7, Hawk, IRIS-T SLM) support a repair crate that restores a damaged deployment for free — everything else must be redeployed from scratch if destroyed.

---

## See Also

- [Deployables Guide](../advanced/deployables-guide.md) - What each category does on the battlefield, and when to call for it
- [Cargo Operations](../f10-menu/cargo.md) - How to transport crates
- [Actions Menu](../f10-menu/actions.md) - Air deployments
- [Points System](../gameplay/points-and-lives.md) - Earning points
