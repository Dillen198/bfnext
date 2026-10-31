# Air-Launched Cruise Missiles

An ALCM platform is a bomber you buy from the Actions menu that orbits behind
the lines with a load of cruise missiles. A JTAC then aims them: each missile
flies to one of the JTAC's contacts.

![Bomber launching a long-range cruise missile at a high-value target](/api/wiki/images/7fc9ec2b-79a6-46d1-b61f-27dd1a313ee9)

> **Availability**: the ALCM platform is an optional action
> (`CruiseMissileSpawn` in the [action list](../f10-menu/actions.md#what-this-server-offers)).
> If you don't see one under `Actions>>`, this server isn't running it. For
> long-range fires without it, use the **naval cruise missile strike**
> ([Carrier Operations](../gameplay/carrier-ops.md)), a **bomber** or
> **strike** action, or [artillery and missile TELs](./artillery.md).

## Step 1: put a platform up

```
F10 → Actions>> → [ALCM action] → [your map mark]
```

Drop a map mark where you want it to orbit (see
[map marks](../f10-menu/overview.md#map-marks-are-your-targeting-system)),
then pick the action and the mark. It takes off from the nearest friendly
airfield with a large stand more than 10 km from the mark and flies a circle
over it, with unlimited fuel. Cost is in the menu label, as with every action.

To move it later: `Actions → [cruise missile waypoint action] → [platform] →
[new mark]`. To send it home: `Actions → RTB`, which refunds a quarter of its
cost when it lands. Its orbit mark on the F10 map shows the group id you will
need below.

## Step 2: fire through a JTAC

Any JTAC on your side whose current target is within
{{cfg:alcm_mission_range|600000}} m of a platform lists it under **ALCM**, as
`id(missiles left)`:

```f10menu
{"root":"F10 Other","items":[
 {"label":"JTAC","items":[
  {"label":"Axeman 11","items":[
   {"label":"ALCM","items":[
    {"label":"5230(12)","items":[
     {"label":"Fire Quarter","items":[
      {"label":"Fire One per target","says":"ALCM MISSION STARTED for 5230\ndirected by jtac 1187 near Gori\nrequested by Viper"},
      {"label":"Fire Two per target","says":"ALCM MISSION STARTED for 5230\ndirected by jtac 1187 near Gori\nrequested by Viper"},
      {"label":"Fire Four per target","says":"jtac 1187 could not start ALCM mission ALCM Abort: not enough missiles to complete a minimum launch."}
     ]},
     {"label":"Fire Half","items":[
      {"label":"Fire One per target","says":"ALCM MISSION STARTED for 5230\ndirected by jtac 1187 near Gori\nrequested by Viper"},
      {"label":"Fire Two per target","says":"ALCM MISSION STARTED for 5230\ndirected by jtac 1187 near Gori\nrequested by Viper"},
      {"label":"Fire Four per target","says":"ALCM MISSION STARTED for 5230\ndirected by jtac 1187 near Gori\nrequested by Viper"}
     ]},
     {"label":"Fire All","items":[
      {"label":"Fire One per target","says":"ALCM MISSION STARTED for 5230\ndirected by jtac 1187 near Gori\nrequested by Viper"},
      {"label":"Fire Two per target","says":"ALCM MISSION STARTED for 5230\ndirected by jtac 1187 near Gori\nrequested by Viper"},
      {"label":"Fire Four per target","says":"ALCM MISSION STARTED for 5230\ndirected by jtac 1187 near Gori\nrequested by Viper"}
     ]}
    ]}
   ]}
  ]}
 ]}
]}
```

With no platform in range the ALCM submenu holds one line,
`(no ALCM shooters in range)`.

### How the missiles are shared out

1. **Magazine**: `Fire Quarter`, `Fire Half` or `Fire All` sets how much of
   the platform's load this mission may use (a quarter, half or all of it,
   rounded down).
2. **Per target**: one, two or four missiles go at each contact.
3. The platform works down the JTAC's contact list, in the JTAC's order, giving
   each contact its missiles until the allowance runs out.

So `Fire Quarter → Fire One per target` on a 12-missile platform sends one
missile at each of the first three contacts. `Fire All → Fire Four per target`
puts four into each of the first three.

Use two or four per target for hard, high-value targets (SAM radars, command
posts); one per target to spread damage over a column.

### Refusals

- `no target`: the JTAC has nothing designated.
- `ALCM Abort: 5230 is out of missiles.`
- `ALCM Abort: 5230 has only 1 missiles remaining, cannot launch 2.`
- `ALCM Abort: not enough missiles to complete a minimum launch.`: the chosen
  share of the magazine is less than one missile.

## Limitations

- The JTAC needs a target, and the platform's missiles only go at the JTAC's
  contacts.
- Missiles spent are gone; buy a new platform when it is empty.
- The platform is a big, slow aircraft. Keep it behind your fighters.
- The platform's slot counts against the action's limit until it is destroyed,
  lands or is removed.

## See Also

- [JTAC System](../f10-menu/jtac.md)
- [Actions Menu](../f10-menu/actions.md)
- [Action Types](../reference/action-types.md)
