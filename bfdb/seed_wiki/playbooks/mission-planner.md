# Mission Planner

The dashboard's **BRIEFING → Planner** tab is a flight planner on the live
campaign map. Lay down a route, check it against the threats your side knows
about, share it with your coalition, and load it into your jet as a data
cartridge instead of typing waypoints in the cockpit. Like the rest of the
BRIEFING page it is locked to your side: sign in and link your pilot first (see
[Reading the Live Ops Dashboard](./reading-live-ops.md#signing-in-and-your-side)).

## Planning a route

```steps
[
 {"title":"1. Name it and pick the jet","text":"Open **BRIEFING → Planner**. Fill in **Name** (for example `SEAD Kutaisi`), **Callsign**, **Aircraft**, **Speed kt GS** and, if you want ETAs, **Start (mission)** as `HH:MM`. The aircraft list shows what each type can load: `DCS cartridge`, `DCS-DTC app`, `cartridge file`, or `no DTC` (those are listed but can't be picked)."},
 {"title":"2. Lay down the waypoints","text":"Click the map to add a waypoint, starting from your field. Click an objective to put a waypoint on it. Drag a waypoint to move it. With a waypoint selected, the next click adds the new one after it. The map shows your side's known SAM rings, so you can route round them."},
 {"title":"3. Fill in the table","text":"In **Waypoints** set each point's **NAME**, **ALT** (planned altitude, feet MSL), **ELEV** (the ground elevation the jet should use, important for a target) and tick **TGT** for target points. **CRS°M**, **NM** and **ETE** (or **ETA** with a start time) are worked out for you. Reorder or delete with the arrows and bin."},
 {"title":"4. Radios and notes","text":"In **Radios**, **From comms card** copies your side's numbered presets. Tick the box to write them into the cartridge, or leave it and the jet keeps the mission's presets. **Notes (route card)** goes on your kneeboard card."},
 {"title":"5. Save and share","text":"**Save** shares the plan with your whole coalition: it appears under **Blue plans** (or Red) for everyone on your side. A teammate's plan opens read-only; **Copy** it to make your own version."},
 {"title":"6. Load it into the jet","text":"**Set up & download cartridge** in the **Data cartridge (DTC)** panel walks you through it for your aircraft: download, where to put the file, and how to load it in the cockpit. **Kneeboard card** and **Route XML (CombatFlite)** work for every aircraft."}
]
```

## The panels

| Panel | What it does |
| --- | --- |
| **Plan** | Name, callsign, aircraft, ground speed, mission start time. **New**, **Copy**, **Save** (`Saved` once it is stored). Save needs a name and at least one waypoint |
| **Waypoints** | The route as a table, with the leg course, distance and timing, and the total distance and time in the header |
| **Radios** | COMM1 / COMM2 presets (COMM1 UHF / COMM2 VHF on the F-16): number, MHz, AM/FM, label |
| **Data cartridge (DTC)** | The ways your aircraft can load the plan, the recommended one first, and the setup for each |
| **Route & kneeboard** | **Kneeboard card** (a printable route card) and **Route XML (CombatFlite)** |
| **Blue plans / Red plans** | Every plan your side has saved, with its aircraft, waypoint count, author and time. Delete your own with the bin |

Cartridges only load nav points on the map they were planned on; the panel
names the map.

## Loading it into your aircraft

| Method | Aircraft | How it loads |
| --- | --- | --- |
| **DCS built-in cartridge** | F-16C, F/A-18C, F-14B(U), AH-64D, MiG-29 Fulcrum | Download the `.dtc`, put it in `Saved Games\DCS\DTC`, then press **Open DTC** on the briefing screen, or later, parked at a friendly airfield, **F8 > Ground Crew > DTC Interface**. Pick it and press **Load** |
| **DCS-DTC app** | F-16C, F/A-18C, F-15E, A-10C / A-10C II, AV-8B, AH-64D, CH-47F, OH-58D, C-130J (and the F-14, store only) | Install the free DCS-DTC app once. Download the preset (or **copy share code**), open it in DCS-DTC, tick what to replace, sit in the cockpit and press **Upload**. DCS-DTC can store F-14 presets but not upload them yet: use the DCS cartridge for the Tomcat |
| **Cartridge file** | Mirage 2000C, AJS37 Viggen, JF-17 | Mirage: a `.dtc` in `Saved Games\DCS\Datacartridges`, loaded from the PSM. Viggen: `CustomCartridge.ini` in `Saved Games\DCS_AJS37`. JF-17: the route comes from the mission; you can load the HSD route line and radio presets |

Where a jet has the built-in cartridge, the planner adds a cockpit tip, for
example on the F-16 that your route is steerpoints 1 onward and 25 is kept for
the bullseye, or on the F-14B(U) that the RIO picks flight plan 2.

**Route XML (CombatFlite)** is a route file DCS-DTC's waypoint page imports for
any aircraft (`Waypoints, Import, "From CombatFlite XML Export..."`), and other
planners read it too.

## See also

- [Reading the Live Ops Dashboard](./reading-live-ops.md)
- [The Auto-Generated Briefing](../gameplay/briefing.md): the Situation tab beside the planner
- [Comms Plan](../gameplay/comms-plan.md): where the comms card's presets come from
- [Navaids & Approaches](../gameplay/navaids.md)
