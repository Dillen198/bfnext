# C-130 Hercules & Airdrop System

![C-130 Hercules banking low over the front line](/api/wiki/images/76185372-d487-422b-a6ea-89de0da561d8)

The C-130 Hercules is the backbone of coalition logistics. Unlike every other aircraft in the campaign, the Hercules can physically carry and deliver cargo, troops, and vehicles to forward positions — including unprepared landing zones deep inside enemy territory. Flying one is a force-multiplier mission that wins campaigns.

The C-130 slot features a custom airdrop and auto-unpack system built specifically for this campaign. The moment cargo touches the ground (via parachute or LAPES run), the engine automatically registers the delivery, unpacks the asset, and credits the supplying pilot — no manual input required.

## Airdrop System

Airdrop missions deliver supply crates, vehicles, and ammunition pallets to objectives that are cut off from ground convoy routes. Standard parachute drops work from any altitude above the minimum safe threshold. LAPES (Low Altitude Parachute Extraction System) runs must be performed below 15 ft AGL at low speed.

- **Min Drop Altitude**: 600 ft AGL for standard chute deployment — below this the chute will not open
- **LAPES Run**: Below 15 ft AGL, 120 kts or less — cargo extracts horizontally via drogue chute
- **Drop Zone**: Must be within the target objective's capture radius — check the F10 map before committing
- **Delivery Credit**: Credit is awarded when the cargo contacts the ground inside the DZ — automatic, no chat command needed
- **Crate Types**: Supply crates, ammo pallets, vehicle kits, and FOB construction packs (varies by mission)

You can check available cargo load options via **F10 → Menu → Cargo** before takeoff. Load type determines what gets spawned on the ground after auto-unpack.

| | |
|---|---|
| ![C-130 airdrop delivery via parachute](/api/wiki/images/61a90050-7359-4ada-b0c9-41f09dc26a34) | ![C-130 LAPES low altitude extraction run](/api/wiki/images/fab0866b-a54d-4e0a-9647-d246a700d5a6) |

## Auto-Unpack — How It Works

Unlike manual cargo systems that require a player on the ground to trigger unpacking, the auto-unpack engine handles everything the moment delivery is confirmed. This is what happens behind the scenes:

1. **Impact Detection**: The server detects the cargo crate landing within the target objective's radius. Position, velocity, and zone membership are all validated simultaneously.
2. **Crate Classification**: The system identifies crate type (supply, ammo, vehicle kit, construction pack) from the load manifest set at departure. Mixed loads are handled — each crate type is processed independently.
3. **Asset Spawn**: The correct asset is spawned at or near the landing point. Vehicles are placed on suitable terrain. Construction packs trigger FOB or FARP build sequences automatically.
4. **Supply Credit**: The objective's supply level is updated in the campaign database. The delivering pilot receives score credit proportional to the cargo value and distance flown.
5. **Pilot Notification**: An in-game message confirms delivery — crate type, objective credited, and supply delta. No manual confirmation needed.

**Note**: Multiple C-130s can deliver to the same objective simultaneously. Deliveries stack — each crate is processed individually. There is no race condition or delivery conflict.

## C-130 F10 Menu Options

The C-130 slot has an expanded F10 Radio Menu with dedicated cargo and troop transport commands not available to other aircraft.

| Menu Path | Description |
|---|---|
| Cargo → Load | Select cargo type for your current flight: supply crates, ammo pallets, vehicle kits, or FOB construction packs. Must be done at a friendly airbase or FARP before departure. |
| Cargo → Status | Check what is currently loaded in your aircraft and its total cargo weight. |
| Cargo → Drop Now | Manually trigger a cargo drop at your current position (if within a valid drop zone). Normally you can simply open the ramp and drop at the correct location. |
| Troop → Embark | Board a squad of troops at a friendly FOB or objective. Must be on the ground inside the objective radius. |
| Troop → Disembark | Deploy embarked troops at your current location. Troops auto-spawn and begin holding or advancing on the nearest objective. |
| Troop → Status | Shows how many troops are currently aboard and their unit type. |

## Tips

- **Plan your route**: Check the EWR before departure. C-130s are large, slow, and cannot defend themselves against fighters or SAMs. Fly low, use terrain masking, and request escort when operating near contested airspace.
- **Coordinate DZ timing**: Announce your drop run before committing. Other pilots may be operating in the same area. A C-130 on final approach is extremely vulnerable — you want friendlies between you and any threat.
- **High-value loads first**: FOB construction packs and vehicle kits have the highest campaign impact. Supply crates are faster to load but worth less. Prioritize construction if your coalition is building forward positions.

## See Also

- [Cargo Operations](../f10-menu/cargo.md)
- [Troop Transport](../f10-menu/troops.md)
- [Logistics & Supply](../gameplay/logistics.md)
