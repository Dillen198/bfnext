# The Tasking Board

The tasking board is how a coalition tells itself what to do without anybody
having to be on voice. A player posts a task; **everyone on that side sees it on
the F10 map, in the briefing, and — for new tasks — hears it called on the GCI
net.**

It is free. There is no rank, no permission, no cost. If you see something that
needs doing, post it.

## Posting a task

```
F10 → Actions>> → Add Task → [type] → [where]
```

Two kinds of task, and they behave differently.

### Position tasks — posted at a map mark

`CAP`, `CAS`, `SEAD`, `STRIKE`, `LOGISTICS`, `CSAR`.

1. Open the F10 **map**, right-click, Add mark. Name it or don't.
2. Cockpit → `Actions>>` → `Add Task` → pick the type → pick your mark.

A circle and a pin appear on the F10 map for your whole coalition.

| Type | Circle | Means |
| --- | --- | --- |
| CAP | 40 km | Combat air patrol — hold this area |
| SEAD | 25 km | Suppress enemy air defences |
| CAS | 15 km | Close air support — friendlies in contact |
| STRIKE | 10 km | Strike the objective in this area |
| LOGISTICS | 10 km | Crates / troops needed here |
| CSAR | 10 km | Recover the downed pilot |

Nothing the engine can measure finishes a position task. It stays up until
somebody takes it down or it times out.

### Objective tasks — posted against a base

`CAPTURE` and `SUPPLY`. These pick a base from a list instead of a map mark, and
**they close themselves out when the coalition actually does the job.**

- **CAPTURE** — pick an enemy-held base. Completes the moment your side owns it.
- **SUPPLY** — pick one of your own bases. Completes once that base's supply
  *and* fuel are both back at 80%, however that happened: a player crate run, an
  [AI helo resupply](../advanced/helo-missions.md), or a convoy getting through.

This is the part worth understanding: an objective task is a **standing
request**, not a reminder. Post `SUPPLY` on the base that is starving and forget
about it — it comes off the board by itself when somebody fixes it.

## Removing a task

```
F10 → Actions>> → Remove Task → [task]
```

Lists everything currently on your coalition's board. On the live mission
**anyone on the posting side can remove any task** — the board is shared
property, so clean up after yourself and don't clear someone else's work out of
tidiness.

## Limits

Server configuration; these are the live mission's.

| | |
| --- | --- |
| Max open tasks per coalition | 12 |
| Auto-expiry | 2 hours after posting |
| Cost | free |
| Who can post | anyone on the coalition |
| Who can remove | anyone on the coalition |

Twelve is not many across a whole map. A board full of stale `CAS` pins from an
hour ago is worse than an empty one — it is the reason the limit exists.

## Where tasks show up

**The F10 map** — a coloured circle and a pin with the type and description.
Blue-ish for CAP, orange for CAS, magenta for SEAD, red for STRIKE, green for
LOGISTICS, yellow for CSAR, orange for CAPTURE, light blue for SUPPLY.

**The briefing** — `F10 → Info → Situation → 2. Tasking` ranks the board
alongside everything else the engine thinks matters, and the slot-entry panel
shows your top three. `-brief` in chat gives the short list.

**The GCI net** — a newly posted task is called out over SRS to your coalition.
If you are on the [GCI frequency](./comms-plan.md) you hear "new tasking"
without touching the map.

**The dashboard** — the Briefing tab draws the board on the live map, numbered
against clickable pins.

## How to use it well

- **Post before you fly, not after.** A `CAS` pin that goes up while you are
  still on the ramp gets you a wingman. One posted as you run out of fuel does not.
- **`SUPPLY` and `CAPTURE` are the high-value ones.** They are the only tasks
  that manage themselves, and they point at the two things that actually decide
  the round.
- **`CAP` marks intent, not a request.** Posting `CAP` over the corridor you
  need protected tells transport pilots where it is safe to run.
- **Be specific with position tasks.** A 40 km CAP circle centred on nothing in
  particular is noise. Put it on the thing you care about.
- **Remove what's done.** Twelve slots, shared across the whole side.

## See Also

- [Actions Menu](../f10-menu/actions.md) — the rest of what lives under Actions
- [The Auto-Generated Briefing](./briefing.md) — how tasks are ranked into the briefing
- [Live GCI (AWACS Calls)](./gci.md) — the voice net that announces new tasks
- [AI Helo Missions](../advanced/helo-missions.md) — the fastest way to close a SUPPLY task
