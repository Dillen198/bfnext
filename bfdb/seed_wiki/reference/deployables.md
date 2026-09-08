# Deployable Units Reference

Every deployable ground unit and troop type configured on this server, pulled from the
live `ODFv2_CFG` campaign config — crate requirements and **weights**, drop limits,
unit caps, and EWR/JTAC ranges. See [Deployables Guide](../advanced/deployables-guide.md)
for what each category does on the battlefield and when to call for one.

**Cost**: every deployable on this server is currently **0 points (FREE)**. The point
economy in [Points and Lives](../gameplay/points-and-lives.md) is spent elsewhere.

**Weights**: "Crates" lists each crate type and the weight of *one* crate. "Total to
deploy" is every required crate added up — that is the total tonnage you have to fly
in, across as many sorties as it takes. A `*` on a drop figure means the crate types in
that unit have different limits and the strictest is shown.

---

## How to Deploy

1. Load crates onto a cargo helicopter/aircraft at a friendly objective.
2. Fly the crates to the deployment location.
3. Unload every required crate for the unit (see "Crates" column).
4. The unit spawns automatically once the last required crate is delivered.

**Drop parameters**: exceeding a crate's max drop height or speed when unloading fails
the drop — the game reports the exact limits back to you when you try.

**Threatened objectives**:
- ⚠️ You **cannot** deploy troops or crates at a threatened objective.
- Wait for it to clear (5 minute cooldown after enemies leave).
- A white circle = capturable; you can still deploy there if you own it.

---

## RED COALITION DEPLOYABLES

### SAM - Medium Range

| Image | Unit | Crates (each crate's weight) | Total to deploy | Max | Drop Height | Drop Speed | EWR | JTAC | Repair |
|---|---|---|---|---|---|---|---|---|---|
| ![](/api/wiki/images/d6048837-b802-43a7-b5a8-82ae93519119) | **SA 6 Kub** | 3× "Kub Launcher" (600 kg) + 1× "Kub Radar" (600 kg) | 2400 kg | 8 | 10 m | 47 km/h (~25 kts) | 30 km | — | 1× "Kub Repair" (600 kg) |
| ![](/api/wiki/images/2e67633d-559c-49e5-b275-f90617da3542) | **SA 3** | 2× "SA3 Launcher" (600 kg) + 1× "SA3 Track Radar" (600 kg) + 1× "SA3 Search Radar" (450 kg) | 2250 kg | 8 | 10 m * | 47 km/h (~25 kts) * | 30 km | — | 1× "SA3 Repair" (600 kg) |
| ![](/api/wiki/images/a8b1e563-96fa-4076-8103-e8def4a1f4fe) | **SA15 Tor** | 3× "SA15 Tor" (600 kg) | 1800 kg | 8 | 10 m | 47 km/h (~25 kts) | 20 km | — | — |

### SAM - Short Range

| Image | Unit | Crates (each crate's weight) | Total to deploy | Max | Drop Height | Drop Speed | EWR | JTAC | Repair |
|---|---|---|---|---|---|---|---|---|---|
| ![](/api/wiki/images/4a3906e7-dabb-4cbb-b026-cf280f147a73) | **HQ-7** | 1× "HQ7 LN" (600 kg) + 1× "HQ7 Radar" (450 kg) | 1050 kg | 12 | 10 m * | 47 km/h (~25 kts) * | 20 km | — | 1× "HQ7 Repair" (600 kg) |
| ![](/api/wiki/images/dffa56a2-278a-4365-b5b7-2cd1123f97ac) | **SA 8 Osa** | 2× "SA8 Osa" (600 kg) | 1200 kg | 10 | 10 m | 47 km/h (~25 kts) | 20 km | — | — |
| ![](/api/wiki/images/da4b4679-49ba-4b76-8586-ad9776510eda) | **SA 9 Strela** | 2× "SA9 Strela" (600 kg) | 1200 kg | 10 | 10 m | 47 km/h (~25 kts) | 20 km | — | — |

### AAA

| Image | Unit | Crates (each crate's weight) | Total to deploy | Max | Drop Height | Drop Speed | EWR | JTAC | Repair |
|---|---|---|---|---|---|---|---|---|---|
| ![](/api/wiki/images/2d9d2e88-c814-4dd4-8a7d-de179534b673) | **ZSU-57-2** | 1× "ZSU-57-2" (300 kg) | 300 kg | 15 | 10 m | 47 km/h (~25 kts) | — | — | — |
| ![](/api/wiki/images/ee8e0a26-d61e-46e7-8460-3e9d4b90b357) | **SPAA HL Zu-23** | 1× "SPAA HL Zu-23" (300 kg) | 300 kg | 15 | 10 m | 47 km/h (~25 kts) | — | — | — |
| ![](/api/wiki/images/f8fdce84-8d81-4a3d-a430-84764715c683) | **ZSU-23 Shilka** | 1× "SPAA ZSU-23 Shilka" (600 kg) | 600 kg | 15 | 10 m | 47 km/h (~25 kts) | — | — | — |
| ![](/api/wiki/images/60162e11-8a31-4baa-a7bf-caff975e7c48) | **SA-19 Tunguska** | 2× "SA-19 Tunguska" (600 kg) | 1200 kg | 12 | 10 m | 47 km/h (~25 kts) | — | — | — |
| ![](/api/wiki/images/f5701fbb-6bec-4db2-b96b-7bd33d41880f) | **SA-22 Pantsir** | 3× "SA-22 Pantsir" (600 kg) | 1800 kg | 10 | 10 m | 47 km/h (~25 kts) | — | — | — |

### Artillery

| Image | Unit | Crates (each crate's weight) | Total to deploy | Max | Drop Height | Drop Speed | EWR | JTAC | Repair |
|---|---|---|---|---|---|---|---|---|---|
| ![](/api/wiki/images/23c6013f-a01a-494c-bac2-b0f26b8c7c16) | **SPH 2S19 Msta** | 2× "SPH 2S19 Msta 152MM" (600 kg) | 1200 kg | 15 | 10 m | 47 km/h (~25 kts) | — | — | — |
| ![](/api/wiki/images/317bf8d0-c5a5-4b2c-87cb-10687accec80) | **PLZ-05** | 2× "PLZ-05" (600 kg) | 1200 kg | 15 | 10 m | 47 km/h (~25 kts) | — | — | — |
| ![](/api/wiki/images/6d527976-956d-40a2-80c5-05eae20e22fd) | **MLRS Smerch CM** | 3× "MLRS Smerch CM" (600 kg) | 1800 kg | 10 | 10 m | 47 km/h (~25 kts) | — | — | — |
| ![](/api/wiki/images/16e38e13-092d-49bb-bc9f-9f59332e3dd4) | **MLRS Smerch HE** | 3× "MLRS Smerch HE" (600 kg) | 1800 kg | 10 | 10 m | 47 km/h (~25 kts) | — | — | — |
| ![](/api/wiki/images/2c85e167-d84c-4006-983c-7db60d6af208) | **MLRS TOS-1A** | 2× "MLRS TOS-1A" (600 kg) | 1200 kg | 15 | 10 m | 47 km/h (~25 kts) | — | — | — |
| _image pending_ | **MLRS GRAD** | 2× "MLRS GRAD" (600 kg) | 1200 kg | 15 | 10 m | 47 km/h (~25 kts) | — | — | — |
| ![](/api/wiki/images/8b17a846-92f0-41d4-81aa-f1d74c628334) | **MLRS 9K57** | 2× "MLRS 9K57" (600 kg) | 1200 kg | 15 | 10 m | 47 km/h (~25 kts) | — | — | — |

### MBT

| Image | Unit | Crates (each crate's weight) | Total to deploy | Max | Drop Height | Drop Speed | EWR | JTAC | Repair |
|---|---|---|---|---|---|---|---|---|---|
| ![](/api/wiki/images/11039281-ca3b-4abf-bf03-0daba63cd8d4) | **ZTZ-96B** | 3× "ZTZ-96B" (600 kg) | 1800 kg | 15 | 10 m | 47 km/h (~25 kts) | — | — | — |
| ![](/api/wiki/images/91c70b1e-de29-4a31-bc78-908080a7142c) | **T-84 Oplot** | 3× "T-84" (600 kg) | 1800 kg | 15 | 10 m | 47 km/h (~25 kts) | — | — | — |
| ![](/api/wiki/images/7aeaf7eb-49c9-423f-a5ce-9b54e4253411) | **T-90M** | 3× "T-90M" (600 kg) | 1800 kg | 15 | 10 m | 47 km/h (~25 kts) | — | — | — |

### IFV

| Image | Unit | Crates (each crate's weight) | Total to deploy | Max | Drop Height | Drop Speed | EWR | JTAC | Repair |
|---|---|---|---|---|---|---|---|---|---|
| ![](/api/wiki/images/eab7a04b-4942-4298-9683-86659465fefd) | **BMPT Terminator** | 2× "BMPT Terminator" (600 kg) | 1200 kg | 20 | 10 m | 47 km/h (~25 kts) | — | — | — |
| ![](/api/wiki/images/4e6c5c99-bb1b-434d-b177-b5e9994bbd9a) | **ZBD-04A** | 1× "ZBD-04A" (600 kg) | 600 kg | 20 | 10 m | 47 km/h (~25 kts) | — | — | — |
| ![](/api/wiki/images/e0034850-3e94-41c0-8424-b7e0e49659e7) | **BTR-82A** | 1× "BTR-82A" (600 kg) | 600 kg | 20 | 10 m | 47 km/h (~25 kts) | — | — | — |
| _image pending_ | **BMP-3** | 1× "BMP-3" (600 kg) | 600 kg | 20 | 10 m | 47 km/h (~25 kts) | — | — | — |

### JTAC

| Image | Unit | Crates (each crate's weight) | Total to deploy | Max | Drop Height | Drop Speed | EWR | JTAC | Repair |
|---|---|---|---|---|---|---|---|---|---|
| ![](/api/wiki/images/149efbdc-bab4-4572-91a8-72318aa8bad4) | **Scout BRDM** | 1× "Scout BRDM " (600 kg) | 600 kg | 20 | 10 m | 47 km/h (~25 kts) | — | 8 km (LOS) | — |

### Logistics

| Image | Unit | Crates (each crate's weight) | Total to deploy | Max | Drop Height | Drop Speed | EWR | JTAC | Repair |
|---|---|---|---|---|---|---|---|---|---|
| ![](/api/wiki/images/44ba8543-63f5-4166-91c7-7203ee7316b3) | **Ammo Truck** | 2× "Ammo Truck" (600 kg) | 1200 kg | 30 | 10 m | 47 km/h (~25 kts) | — | — | — |

### EWRs

| Image | Unit | Crates (each crate's weight) | Total to deploy | Max | Drop Height | Drop Speed | EWR | JTAC | Repair |
|---|---|---|---|---|---|---|---|---|---|
| ![](/api/wiki/images/89410024-5e16-4f75-b21f-a9db3f7d1152) | **EWR 55G6** | 1× "EWR 55G6" (600 kg) | 600 kg | 20 | 10 m | 47 km/h (~25 kts) | 500 km | — | — |

### GCI Stations

| Image | Unit | Crates (each crate's weight) | Total to deploy | Max | Drop Height | Drop Speed | EWR | JTAC | Repair |
|---|---|---|---|---|---|---|---|---|---|
| ![](/api/wiki/images/71ed5ded-b182-4421-8eef-7b672d1f6123) | **MiG-29 GCI** | 2× "GCI Station Crate" (600 kg) | 1200 kg | 4 | 20 m | 1260 km/h (~680 kts) | — | — | — |

### APC

| Image | Unit | Crates (each crate's weight) | Total to deploy | Max | Drop Height | Drop Speed | EWR | JTAC | Repair |
|---|---|---|---|---|---|---|---|---|---|
| _image pending_ | **MRAP TITAN** | 1× "MRAP TITAN" (300 kg) | 300 kg | 20 | 10 m | 47 km/h (~25 kts) | — | — | — |

### ATGM

| Image | Unit | Crates (each crate's weight) | Total to deploy | Max | Drop Height | Drop Speed | EWR | JTAC | Repair |
|---|---|---|---|---|---|---|---|---|---|
| _image pending_ | **AT-3 Sagger** | 1× "AT-3 Sagger" (500 kg) | 500 kg | 15 | 10 m | 47 km/h (~25 kts) | — | — | — |

### Missiles

| Image | Unit | Crates (each crate's weight) | Total to deploy | Max | Drop Height | Drop Speed | EWR | JTAC | Repair |
|---|---|---|---|---|---|---|---|---|---|
| _image pending_ | **SRBM 9K720 CM** | 4× "SRBM 9K720 CM" (1200 kg) | 4800 kg | 10 | 20 m | 1256 km/h (~678 kts) | — | — | — |
| _image pending_ | **SRBM 9K720 HE** | 4× "SRBM 9K720 HE" (1200 kg) | 4800 kg | 10 | 20 m | 1256 km/h (~678 kts) | — | — | — |
| _image pending_ | **SSM Scud** | 3× "SSM Scud" (1200 kg) | 3600 kg | 10 | 20 m | 1256 km/h (~678 kts) | — | — | — |

---

## BLUE COALITION DEPLOYABLES

### SAM - Short Range

| Image | Unit | Crates (each crate's weight) | Total to deploy | Max | Drop Height | Drop Speed | EWR | JTAC | Repair |
|---|---|---|---|---|---|---|---|---|---|
| ![](/api/wiki/images/e43d3f79-d1bc-43bb-ac4d-bfb387ea24df) | **Roland** | 2× "Roland" (500 kg) | 1000 kg | 15 | 10 m | 47 km/h (~25 kts) | 10 km | — | — |
| ![](/api/wiki/images/6c74c2ec-9b84-4d02-b050-642f1f915fcd) | **Avenger** | 2× "Avenger" (300 kg) | 600 kg | 10 | 10 m | 47 km/h (~25 kts) | — | — | — |
| ![](/api/wiki/images/2fcf4a40-7cf9-407d-920f-47ef946bbc1d) | **Linebacker** | 1× "Linebacker" (300 kg) | 300 kg | 10 | 10 m | 47 km/h (~25 kts) | — | — | — |

### SAM - Medium Range

| Image | Unit | Crates (each crate's weight) | Total to deploy | Max | Drop Height | Drop Speed | EWR | JTAC | Repair |
|---|---|---|---|---|---|---|---|---|---|
| ![](/api/wiki/images/4faa9027-f574-4e0b-8ee0-1e60213ad1b5) | **Hawk System** | 3× "Hawk Launcher" (300 kg) + 1× "Hawk Search Radar" (300 kg) + 1× "Hawk Track Radar" (300 kg) + 1× "Hawk CC" (300 kg) | 1800 kg | 8 | 10 m | 47 km/h (~25 kts) | 60 km | — | 1× "Hawk Repair" (300 kg) |
| ![](/api/wiki/images/4118d2bb-b773-4505-bf2d-c2a919251dab) | **IRIS-T SLM** | 1× "IRIST C2" (300 kg) + 1× "IRIST STR" (300 kg) + 2× "IRIST LN" (450 kg) | 1500 kg | 8 | 10 m * | 1260 km/h (~680 kts) * | 30 km | — | 1× "IRIST REPAIR" (450 kg) |

### AAA

| Image | Unit | Crates (each crate's weight) | Total to deploy | Max | Drop Height | Drop Speed | EWR | JTAC | Repair |
|---|---|---|---|---|---|---|---|---|---|
| ![](/api/wiki/images/e60e2222-265d-4886-b701-294b9a9f19da) | **C-RAM** | 1× "C-RAM" (300 kg) | 300 kg | 15 | 10 m | 47 km/h (~25 kts) | — | — | — |
| ![](/api/wiki/images/bf0104cd-413c-4203-9c5f-39bfcd8409f1) | **Flakpanzergepard** | 1× "Flakpanzergepard" (300 kg) | 300 kg | 12 | 10 m | 47 km/h (~25 kts) | — | — | — |
| ![](/api/wiki/images/6ee596b9-f1a0-4771-bdaf-b57d9b3d83bf) | **Vulkan** | 1× "Vulkan" (300 kg) | 300 kg | 12 | 10 m | 47 km/h (~25 kts) | — | — | — |

### Artillery

| Image | Unit | Crates (each crate's weight) | Total to deploy | Max | Drop Height | Drop Speed | EWR | JTAC | Repair |
|---|---|---|---|---|---|---|---|---|---|
| ![](/api/wiki/images/6e70c2cb-ed58-42b1-a5bd-c46646f476d2) | **Firtina 155MM** | 2× "Firtina 155MM" (300 kg) | 600 kg | 15 | 10 m | 47 km/h (~25 kts) | — | — | — |
| ![](/api/wiki/images/5427e732-a297-4f7c-bd92-98f163a091bc) | **Dana 152MM** | 2× "Dana 152MM" (300 kg) | 600 kg | 15 | 10 m | 47 km/h (~25 kts) | — | — | — |
| _image pending_ | **MLRS M270** | 2× "MLRS M270" (300 kg) | 600 kg | 15 | 10 m | 47 km/h (~25 kts) | — | — | — |
| ![](/api/wiki/images/2e9da600-e13e-4d2a-8aa8-f9b24b7f0cf5) | **Himars GMLRS HE** | 3× "Himars GMLRS HE" (300 kg) | 900 kg | 10 | 10 m | 47 km/h (~25 kts) | — | — | — |
| ![](/api/wiki/images/eea9db79-5a8f-4748-8a95-e387dc8efff3) | **Himars GMLRS CM** | 3× "Himars GMLRS CM" (300 kg) | 900 kg | 10 | 10 m | 47 km/h (~25 kts) | — | — | — |
| ![](/api/wiki/images/558de980-65ad-4ef3-aa59-340f18f204be) | **Himars ATACMS HE** | 3× "Himars ATACMS HE" (300 kg) | 900 kg | 10 | 10 m | 47 km/h (~25 kts) | — | — | — |
| ![](/api/wiki/images/110d1c99-6d73-45a6-b83a-088323d1abe4) | **Himars ATACMS CM** | 3× "Himars ATACMS CM" (300 kg) | 900 kg | 10 | 10 m | 47 km/h (~25 kts) | — | — | — |

### IFV

| Image | Unit | Crates (each crate's weight) | Total to deploy | Max | Drop Height | Drop Speed | EWR | JTAC | Repair |
|---|---|---|---|---|---|---|---|---|---|
| ![](/api/wiki/images/8f83a8f8-4838-499b-853e-0b78226e0c83) | **M2A2 Bradley** | 1× "M2A2 Bradley" (300 kg) | 300 kg | 20 | 10 m | 47 km/h (~25 kts) | — | — | — |
| ![](/api/wiki/images/a4c29214-9c23-4d3f-8d93-4c3e27cb8de2) | **M1130 Stryker** | 1× "M1130 Stryker " (300 kg) | 300 kg | 20 | 10 m | 47 km/h (~25 kts) | — | — | — |
| _image pending_ | **M1296 Dragoon** | 1× "MM1296 Dragoon" (300 kg) | 300 kg | 20 | 10 m | 47 km/h (~25 kts) | — | — | — |

### APC

| Image | Unit | Crates (each crate's weight) | Total to deploy | Max | Drop Height | Drop Speed | EWR | JTAC | Repair |
|---|---|---|---|---|---|---|---|---|---|
| ![](/api/wiki/images/884ca354-0f5d-4670-970b-097d1efeb757) | **MRAP MaxxPro** | 1× "MRAP MaxxPro" (300 kg) | 300 kg | 20 | 10 m | 47 km/h (~25 kts) | — | — | — |
| ![](/api/wiki/images/3a8cac53-22e7-4b44-a715-3c4cb5933154) | **MRAP M-ATV** | 1× "MRAP M-ATV" (300 kg) | 300 kg | 20 | 10 m | 47 km/h (~25 kts) | — | — | — |

### JTAC

| Image | Unit | Crates (each crate's weight) | Total to deploy | Max | Drop Height | Drop Speed | EWR | JTAC | Repair |
|---|---|---|---|---|---|---|---|---|---|
| ![](/api/wiki/images/224ac803-00f4-42c5-ac6b-82aab5858a2a) | **MRAP JTAC** | 1× "MRAP JTAC" (300 kg) | 300 kg | 20 | 10 m | 47 km/h (~25 kts) | — | 5 km (LOS) | — |

### MBT

| Image | Unit | Crates (each crate's weight) | Total to deploy | Max | Drop Height | Drop Speed | EWR | JTAC | Repair |
|---|---|---|---|---|---|---|---|---|---|
| ![](/api/wiki/images/24a37b26-36cf-4093-9fd2-ed0cb958da59) | **Leopard 2A6M** | 3× "2A6M Leopard" (300 kg) | 900 kg | 15 | 10 m | 47 km/h (~25 kts) | — | — | — |
| ![](/api/wiki/images/e8c30129-b52b-4516-905e-044ddd297bfe) | **M1A2C Abrams** | 3× "M1A2C Abrams" (300 kg) | 900 kg | 15 | 10 m | 47 km/h (~25 kts) | — | — | — |
| ![](/api/wiki/images/0d2814fb-186a-4f71-a0b8-5b7275c00843) | **Merkava IV** | 3× "Merkava IV" (300 kg) | 900 kg | 15 | 10 m | 47 km/h (~25 kts) | — | 8 km (LOS) | — |

### Logistics

| Image | Unit | Crates (each crate's weight) | Total to deploy | Max | Drop Height | Drop Speed | EWR | JTAC | Repair |
|---|---|---|---|---|---|---|---|---|---|
| ![](/api/wiki/images/44ba8543-63f5-4166-91c7-7203ee7316b3) | **Ammo Truck** | 1× "Ammo Truck" (600 kg) | 600 kg | 20 | 10 m | 47 km/h (~25 kts) | — | — | — |

### EWRs

| Image | Unit | Crates (each crate's weight) | Total to deploy | Max | Drop Height | Drop Speed | EWR | JTAC | Repair |
|---|---|---|---|---|---|---|---|---|---|
| ![](/api/wiki/images/fcd0f052-f904-4628-aa13-9b58a46fb2b2) | **AN/FPS-117** | 1× "AN/FPS-117" (300 kg) | 300 kg | 20 | 10 m | 47 km/h (~25 kts) | 500 km | — | — |

### ATGM

| Image | Unit | Crates (each crate's weight) | Total to deploy | Max | Drop Height | Drop Speed | EWR | JTAC | Repair |
|---|---|---|---|---|---|---|---|---|---|
| _image pending_ | **VAB Mephisto** | 1× "VAB Mephisto" (500 kg) | 500 kg | 15 | 10 m | 47 km/h (~25 kts) | — | — | — |

---

## TROOP TYPES

Troops are loaded at friendly objectives and carried by helicopter or ground vehicle. The **weight** column is what the squad adds to your aircraft as internal cargo — it directly affects hover performance and useful load. Heavier squads mean fewer per airframe.

### RED COALITION TROOPS

| Image | Squad | Cost | Can Capture | Weight | Max | JTAC |
|---|---|---|---|---|---|---|
| ![](/api/wiki/images/cd9a77ef-9ffc-4f0e-a441-f5fdc672140b) | **Standard** | FREE | YES | 700 kg | 10 | 8 km (LOS) |
| ![](/api/wiki/images/ba1888f4-47d3-44f3-8170-ad7a6b41bf99) | **Anti Tank** | 1 pts | YES | 750 kg | 10 | 8 km (LOS) |
| ![](/api/wiki/images/2e0dcfa4-2a2a-41b7-ae9a-157b655eeacd) | **Mortar** | 5 pts | YES | 900 kg | 10 | 8 km (LOS) |
| ![](/api/wiki/images/f0751468-5d52-49cf-bc37-b7c280256624) | **Igla** | 5 pts | NO | 150 kg | 10 | — |

### BLUE COALITION TROOPS

| Image | Squad | Cost | Can Capture | Weight | Max | JTAC |
|---|---|---|---|---|---|---|
| ![](/api/wiki/images/cd9a77ef-9ffc-4f0e-a441-f5fdc672140b) | **Standard** | FREE | YES | 700 kg | 10 | 8 km (LOS) |
| ![](/api/wiki/images/ba1888f4-47d3-44f3-8170-ad7a6b41bf99) | **Anti Tank** | 1 pts | YES | 750 kg | 10 | 8 km (LOS) |
| ![](/api/wiki/images/2e0dcfa4-2a2a-41b7-ae9a-157b655eeacd) | **Mortar** | 5 pts | YES | 900 kg | 10 | 8 km (LOS) |
| ![](/api/wiki/images/eaaf82b4-0e39-444c-8925-c1d9d695e7bf) | **Stinger** | 5 pts | NO | 150 kg | 10 | 8 km (LOS) |

**Notes**:
- Standard, Anti Tank and Mortar infantry can all **capture objectives**.
- MANPADS (Igla / Stinger) **cannot capture**.
- Heavier squads = fewer fit in a transport. A half-fuel SA342L Gazelle can lift one
  MANPAD team (150 kg); a rifle or mortar squad needs a Huey or larger.
- See [Troop Transport](../f10-menu/troops.md) and [Cargo Operations](../f10-menu/cargo.md)
  for aircraft troop/crate slot counts.

---

## General Notes

### Repair Crates

Multi-component systems (SA-6 Kub, SA-3, HQ-7, Hawk, IRIS-T SLM) accept a repair crate
that restores a damaged deployment for free. Everything else must be redeployed from
scratch if destroyed.

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
