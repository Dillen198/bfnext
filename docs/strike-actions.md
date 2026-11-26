# Strike actions

A `Strike` action sends an AI flight from a friendly field to attack with its
own weapons. DCS does the damage, so kills, destroyed buildings and the
target's logistics rating all update the same way as when a player drops bombs.
Unlike the `Bomber` action, it needs no JTAC on the target.

| `mission`      | Aimed at (F10 menu)          | What the flight does                                               | Default weapons            |
|----------------|------------------------------|--------------------------------------------------------------------|----------------------------|
| `Precision`    | an enemy objective           | Attacks the objective's buildings (its logistics), nearest the centre first | guided bombs (14)          |
| `Runway`       | an enemy airbase             | Bombs the airfield's runways (`BombRunway`)                        | any bomb (2147485694)      |
| `AntiShip`     | an enemy carrier group       | Attacks the group's ships (`AttackGroup`)                          | anti-ship missiles (65536) |
| `Interdiction` | a map mark                   | Attacks the enemy convoy or ground column nearest the mark (within 15 km) | the AI picks           |
| `Carpet`       | a map mark                   | Drops a line of bombs through the mark (`CarpetBombing`)           | any bomb                   |

The route is: takeoff, an initial point 20 km short of the target, the
target, then home. Weapons are held and the flight only evades fire until the
initial point, so a stealth aircraft stays quiet on the way in. The group is
removed once it is back within 10 km of its home field. If the server
restarts while it is out, the payer gets the action's cost back.

## Config

Each action needs a late-activated group of that name in the base mission,
with the loadout the mission calls for. **A template that is missing stops the
mission from starting**, so add the group before you add the action.

```json
"F-117 Precision Strike": {
  "kind": {
    "Strike": {
      "mission": "Precision",
      "attacks": 2,
      "plane": {
        "kind": "FixedWing",
        "template": "BF117",
        "altitude": 7000,
        "altitude_typ": "BARO",
        "speed": 250,
        "duration": null,
        "freq": null,
        "tacan_channel": null,
        "tacan_band": null,
        "tacan_callsign": null,
        "callsign_id": null,
        "callsign_number": null
      }
    }
  },
  "cost": 150,
  "penalty": 100,
  "limit": 2,
  "geo_limit": "Unlimited"
}
```

The other missions use the same shape:

- **`Runway`**: `"mission": "Runway"`, for example an F-15E with Mk-84s
  (`attacks` = runways to hit).
- **`AntiShip`**: `"mission": "AntiShip"`, for example an F/A-18C with Harpoons,
  or a Tu-22M3 with Kh-22s for Red.
- **`Interdiction`**: `"mission": "Interdiction"`, for example an A-10C or a
  Su-25T.
- **`Carpet`**: `"mission": "Carpet"`, `"carpet_length_m": 2000`, for example a
  B-52H or a Tu-95MS. These get a large parking stand.

Optional fields:

- `weapon_type` sets the DCS weapon flags the flight may use.
- `attacks` is how many aim points it gets (1–8, default 2).
- `carpet_length_m` is the length of the carpet line (default 1500).

Also add the airframe to `unit_classification`, for example
`"F-117A": ["Aircraft"]`.

In game the action appears in the Actions menu. Objective missions list enemy
objectives; the others list your map marks. It also appears on the commander's
order map, and as `-action <name> <key>` in chat.
