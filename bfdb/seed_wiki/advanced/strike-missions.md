# AI Strike Missions

A **Strike** action sends an AI flight from a friendly field to attack with
its own weapons, for example an F-117 on a precision strike. Unlike the Bomber
action, it needs no JTAC on the target. DCS does the damage, so kills,
destroyed buildings and the target base's logistics update just as they would
if a player had dropped the bombs.

| Mission | You aim it at | What the flight does |
| --- | --- | --- |
| **Precision** | an enemy objective | Attacks the base's buildings (its logistics), nearest the centre first |
| **Runway** | an enemy airbase | Bombs the runways |
| **Anti-ship** | an enemy carrier group | Attacks the group's ships |
| **Interdiction** | a map mark | Attacks the enemy convoy or ground column nearest the mark (within 15 km) |
| **Carpet** | a map mark | Lays a line of bombs through the mark |

## How a strike flies

1. It takes off with weapons held and only evades fire, so a stealth aircraft
   stays quiet on the way in.
2. At the **initial point**, 20 km short of the target, it goes weapons free
   and attacks.
3. It flies home and is removed once it is back near its field.

If the server restarts while a strike is still on its way to the target,
whoever paid for it gets back what they paid. Once it has reached the target,
there is no refund.

## Calling one

```
F10 → Actions>> → <strike action> → [enemy objective or your map mark]
```

Missions aimed at a base list the enemy objectives. The others list your map
marks, so drop a mark first. Commanders can also order strikes from the
command map, and you can call one in chat with `-action <name> <key>`.

The aircraft, cost and limits come from the server's config: the
[Actions Menu](../f10-menu/actions.md) shows what your side can buy.

## See Also

- [Action Types](../reference/action-types.md)
- [Air-Launched Cruise Missiles](./alcm.md)
