# Artillery Missions

Your side's guns, rocket launchers and missile TELs fire for you on request.
There are three ways to call them:

| Way | Aim with | Picks the guns | Best for |
| --- | --- | --- | --- |
| `F10 → Actions → Request Fires` (or an Artillery action) | a map mark | the engine: the nearest batteries in range | quick fire on a spot you can see on the map |
| `F10 → JTAC → [JTAC] → Artillery` | the JTAC's current target | you: one battery, one objective's guns, or all | precise fire on what a JTAC is lasing |
| `-jtac <id> arty <gun id\|all> <rounds>` in chat | the JTAC's current target | you | the JTAC menu, faster once you know the ids |

**What you can actually field depends on the campaign.** The modern Syria
campaign has access to Western rocket artillery; the 2008 Caucasus one is
restricted to what was in the theatre that August: 2S3 Akatsiya, 2S1
Gvozdika, 2S19 Msta, DANA, the Grad family, BM-27 Uragan and BM-30 Smerch, plus
the 2S9 Nona on the Russian side. The live list for the server you have
selected is on the [Deployables](../reference/deployables.md) page. Missile TELs
(Scud, Iskander, Silkworm and similar) count as artillery wherever a campaign
has them; SAM launchers never do.

## Request Fires (Actions menu)

When the server has artillery fire support switched on and your side has at
least one live gun or launcher anywhere, **`Request Fires`** is added at the
end of the Actions menu. It is **free**. A server may also sell an Artillery
action under its own name; it works the same way.

```f10menu
{"root":"F10 Other","items":[
 {"label":"Actions","items":[
  {"label":"Request Fires","note":"Lists your map marks, nearest first.","items":[
   {"label":"ARTY1 080/14km","says":"fire mission requested"},
   {"label":"Mark 1 020/52km","says":"request fires failed: calling artillery fire support\n\nCaused by:\n    no friendly artillery/missiles in range of that position"},
   {"label":"Mark 2 175/3km","says":"request fires failed: calling artillery fire support\n\nCaused by:\n    target is too close for available artillery"}
  ]}
 ]}
]}
```

1. Drop an F10 map mark on the target (see
   [map marks](../f10-menu/overview.md#map-marks-are-your-targeting-system)).
2. `F10 → Actions>>`, then `Actions → Request Fires → [your mark]`.
3. The engine takes every battery on your side whose guns can reach the mark,
   nearest first, and the closest
   {{cfg:artillery.max_groups|3}} fire at it, scattered over about
   {{cfg:artillery.radius_m|200}} m. Each battery turns to face the target
   first if it has to.
4. The mark is deleted and a fire overlay goes on the F10 map for five minutes:
   a dashed line from the guns to the target (your side only) and an impact
   circle with a burst symbol (everyone).

Each gun type's reach is its own: the server's per-type ranges, or DCS's own
figures, or {{cfg:artillery.default_min_range_m|4000}}–{{cfg:artillery.default_max_range_m|30000}} m
for a type nobody has described. Guns too close to the target (inside their
minimum range) are skipped.

After firing, a battery needs {{cfg:artillery.cooldown_secs|60}} s before the
engine will task it again; the next battery in range fires instead. Where the
server enables shoot-and-scoot, batteries relocate a short distance (at most
500 m) after a fire mission, so counter-battery fire finds an empty position.

Refusals:

| Message after `Caused by:` | Means |
| --- | --- |
| `no friendly artillery/missiles in range of that position` | nothing reaches the mark |
| `target is too close for available artillery` | the only guns in reach are inside their minimum range |
| `every battery in range fired within the last 60s, try again shortly` | all of them are reloading |
| `all artillery groups are unavailable or out of range right now` | none of them could be tasked in DCS |

Firing at an enemy base also wakes up its garrison, so the shells have
something to hit.

## JTAC fire missions

Each JTAC has an **Artillery** submenu listing the friendly batteries within
{{cfg:artillery_mission_range|15000}} m of its current target. When none are in
range it says so instead:
`(no guns in range — deploy artillery nearby)`.

```f10menu
{"root":"F10 Other","items":[
 {"label":"JTAC","items":[
  {"label":"Axeman 11","note":"One JTAC; its other entries are on the JTAC page.","items":[
   {"label":"Artillery","items":[
    {"label":"Fire All Groups Together","note":"Only when two or more batteries are in range.","items":[
     {"label":"1 round each","says":"GROUP FIRE MISSION: 3 guns firing 1 rounds each\njtac 1187 near Gori\nrequested by Viper"},
     {"label":"3 rounds each","says":"GROUP FIRE MISSION: 3 guns firing 3 rounds each\njtac 1187 near Gori\nrequested by Viper"},
     {"label":"5 rounds each","says":"GROUP FIRE MISSION: 2 guns firing 5 rounds each\njtac 1187 near Gori\nrequested by Viper\nskipped 1060 (DEPBMLRS-1060) too close to target: 3100m (min 10000m)"},
     {"label":"10 rounds each","says":"GROUP FIRE MISSION: 3 guns firing 10 rounds each\njtac 1187 near Gori\nrequested by Viper"},
     {"label":"all ammo","says":"GROUP FIRE MISSION: 3 guns firing all remaining rounds each\njtac 1187 near Gori\nrequested by Viper"}
    ]},
    {"label":"Gori (2)","note":"Guns are grouped by the objective they sit at when they are at more than one.","items":[
     {"label":"Fire Battery Together","items":[
      {"label":"5 rounds each","says":"GROUP FIRE MISSION (Gori battery): 2 guns firing 5 rounds each\njtac 1187 near Gori\nrequested by Viper"}
     ]},
     {"label":"1051","items":[
      {"label":"Relay Target","says":"TARGET RELAYED to 1051\ndirected by jtac 1187 near Gori\nrequested by Viper"},
      {"label":"Get Ammo","says":"Artillery Group 1051 has 24 rounds total"},
      {"label":"Fire One","says":"ARTILLERY MISSION STARTED for 1051\ndirected by jtac 1187 near Gori\nrequested by Viper"},
      {"label":"Fire For Effect","items":[
       {"label":"5","says":"ARTILLERY MISSION STARTED for 1051\ndirected by jtac 1187 near Gori\nrequested by Viper"},
       {"label":"10","says":"ARTILLERY MISSION STARTED for 1051\ndirected by jtac 1187 near Gori\nrequested by Viper"},
       {"label":"20","says":"jtac 1187 could not start artillery mission 1051 (DEPBFIRTINA-1051) too far from target: 44210m (max 41000m)"},
       {"label":"40","says":"ARTILLERY MISSION STARTED for 1051\ndirected by jtac 1187 near Gori\nrequested by Viper"}
      ]},
      {"label":"Fire All","says":"ARTILLERY FIRE ALL MISSION STARTED for 1051\ndirected by jtac 1187 near Gori\nrequested by Viper"},
      {"label":"Target Group","items":[
       {"label":"Fire 1 per","items":[
        {"label":"Target 2 units","says":"Artillery Combo Mission: Viper firing 1 rounds per target at 2 targets"},
        {"label":"Target 6 units","says":"Artillery Combo Mission: Viper firing 1 rounds per target at 6 targets"}
       ]},
       {"label":"Fire 5 per","items":[
        {"label":"Target 3 units","says":"Artillery Combo Mission: Viper firing 5 rounds per target at 3 targets"}
       ]}
      ]}
     ]}
    ]},
    {"label":"Senaki (1)","items":[
     {"label":"1060","items":[
      {"label":"Fire One","says":"ARTILLERY MISSION STARTED for 1060\ndirected by jtac 1187 near Gori\nrequested by Viper"}
     ]}
    ]}
   ]}
  ]}
 ]}
]}
```

The battery ids are group ids; `Get Ammo` and the JTAC's status show what each
has left.

| Entry | What it does |
| --- | --- |
| **Fire All Groups Together** | Salvo from every battery in range: 1, 3, 5 or 10 rounds per gun, or all ammo. Batteries out of range or out of ammunition are skipped and listed. First in the menu so you never page for it. |
| **Fire Battery Together** | The same, for just the guns at one objective. |
| **Relay Target** | Hands the JTAC's target to that battery. |
| **Get Ammo** | That battery's remaining rounds. |
| **Fire One** | One round, to check the aim. |
| **Fire For Effect** | 5, 10, 20 or 40 rounds. |
| **Fire All** | Everything that battery has. |
| **Target Group** | 1, 2, 5 or 10 rounds at each of 2 to 6 of the JTAC's contacts. |

Every battery checks its own minimum and maximum range against the target
before firing; one that can't reach says why
(`1051 (DEPBFIRTINA-1051) too far from target: 44210m (max 41000m)`). A type
with no configured range uses DCS's own figure, or
{{cfg:artillery_min_range|1500}}–{{cfg:artillery_mission_range|15000}} m.

Common failures: `no JTAC target — designate a target first`,
`no guns in range — ...` (with each battery's reason), `Artillery Abort: 1051
is out of ammunition.`

> **Turning to fire**: fixed- or limited-traverse launchers (Grad, Smerch,
> Scud, Silkworm…) spawn facing north. When you call fire they reposition a
> few metres to bring the launcher onto the target bearing before firing.

There is no fire-adjustment (add/drop, left/right) command: re-aim by moving
the JTAC's target or by calling Request Fires on a new mark.

## Using it well

- **Register, then fire for effect.** `Fire One` from the battery you mean to
  use, watch where it lands, then `Fire For Effect`.
- **Salvo when it matters.** `Fire All Groups Together` with 3 or 5 rounds each
  breaks a SAM site or a column in one go.
- **Mind the minimum range.** Rocket and missile batteries have large minimum
  ranges; tube guns are the ones for targets close to them.
- **Save ammunition.** A battery out of rounds is just a target.
- **Expect counter-battery fire.** Your guns' position is shown to your side
  only, but the enemy can still find them.

## See Also

- [JTAC System](../f10-menu/jtac.md)
- [Actions Menu](../f10-menu/actions.md)
- [Deployable Units Reference](../reference/deployables.md)
