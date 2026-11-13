# The Tasking Board

The tasking board is how a coalition tells itself what to do without anybody
having to be on voice. A player posts a task; **everyone on that side sees it on
the F10 map, in the briefing, and (for new tasks) hears it called on the GCI
net.**

It is normally free, and anyone on the coalition can post. If you see something
that needs doing, post it.

Not to be confused with **your task**, the single task the engine hands each
pilot at slot entry with bonus points inside its zone: see
[Your Task](./your-task.md). That one is chosen by the engine; the board is
written by players and the HQ.

## Posting a task

```
F10 → Actions>> → Add Task → [type] → [where]
```

The entry is called whatever the server named its Add Task action. The task
types under it, their names, circle sizes and colours are server
configuration too. A typical set:

```f10menu
{"root":"F10 Other","items":[
 {"label":"Actions","items":[
  {"label":"Add Task","items":[
   {"label":"CAP","items":[
    {"label":"CAP1 045/12km","says":"CAP task posted","note":"Your whole side also sees: NEW TASK: CAP 12km NE of Batumi posted by Viper"},
    {"label":"Mark 1 310/38km","says":"CAP task posted"}
   ]},
   {"label":"CAS","items":[
    {"label":"CAP1 045/12km","says":"CAS task posted"},
    {"label":"Mark 1 310/38km","says":"could not post CAS task, posting task\n\nCaused by:\n    blue already has 12 tasks on the board, remove one before posting another"}
   ]},
   {"label":"SEAD","items":[{"label":"CAP1 045/12km","says":"SEAD task posted"}]},
   {"label":"STRIKE","items":[{"label":"CAP1 045/12km","says":"STRIKE task posted"}]},
   {"label":"LOGISTICS","items":[{"label":"CAP1 045/12km","says":"LOGISTICS task posted"}]},
   {"label":"CSAR","items":[{"label":"CAP1 045/12km","says":"CSAR task posted"}]},
   {"label":"CAPTURE","note":"Enemy-held bases you can see, alphabetical.","items":[
    {"label":"Gori","says":"CAPTURE task posted"},
    {"label":"Tskhinvali","says":"could not post CAPTURE task, posting objective task\n\nCaused by:\n    Tskhinvali is already on the board as task 9"}
   ]},
   {"label":"SUPPLY","note":"Your own bases, alphabetical.","items":[
    {"label":"Kobuleti","says":"SUPPLY task posted"},
    {"label":"Senaki","says":"SUPPLY task posted"}
   ]}
  ]},
  {"label":"Remove Task","items":[
   {"label":"CAP 14 (12km NE of Batumi)","says":"task 14 removed","note":"Your side sees: TASK CANCELLED: CAP 12km NE of Batumi by Viper"},
   {"label":"CAPTURE 9 (Tskhinvali)","says":"task 9 removed"}
  ]}
 ]}
]}
```

There are two kinds of task, and they behave differently.

### Position tasks: posted at a map mark

Typically `CAP`, `CAS`, `SEAD`, `STRIKE`, `LOGISTICS`, `CSAR`.

1. Open the F10 **map**, right-click, Add mark. Name it or don't (see
   [map marks](../f10-menu/overview.md#map-marks-are-your-targeting-system)).
2. Cockpit → `Actions>>` → `Add Task` → pick the type → pick your mark.

A circle and a pin appear on the F10 map for your whole coalition, and the mark
you used is deleted. The circle's size is set per type by the server (engine
default {{cfg:actions.Blue.add-task.kind.AddTask.types[0].radius_m|15000}} m);
CAP is usually drawn wide and CAS tight.

Nothing the engine can measure finishes a position task. It stays up until
somebody takes it down or it times out.

### Objective tasks: posted against a base

Typically `CAPTURE` and `SUPPLY`. These pick a base from a list instead of a map
mark (long lists are split into alphabetical ranges), and **they close
themselves out when the coalition actually does the job.**

- **CAPTURE**: pick an enemy-held base you can see. Completes the moment your
  side owns it.
- **SUPPLY**: pick one of your own bases. Completes once that base's supply
  *and* fuel are both back at the type's threshold (engine default
  {{cfg:actions.Blue.add-task.kind.AddTask.types[7].target.SupplyObjective.threshold|80}}%),
  however that happened: a player crate run, an
  [AI helo resupply](../advanced/helo-missions.md), or a convoy getting through.

The same base can't be posted twice for the same type; the second attempt is
told which task already covers it. An objective task is a **standing request**,
not a reminder. Post `SUPPLY` on the base that is starving and forget about it:
it comes off the board by itself when somebody fixes it.

## Removing a task

```
F10 → Actions>> → Remove Task → [task]
```

Lists everything currently on your coalition's board as `TYPE id (where)`. By
default **anyone on the posting side can remove any task**. A server can
restrict it to the player who posted it (and admins); then anyone else gets:

```dcsmessage
could not remove task 14, removing task

Caused by:
    task 14 was posted by Viper, only they or an admin can remove it
```

The board is shared property: clean up after yourself, and don't clear someone
else's work out of tidiness.

## What the coalition sees

Every change to the board is announced to your whole side:

```dcsmessage
NEW TASK: CAPTURE Gori posted by Viper
```

| Message | When |
| --- | --- |
| `NEW TASK: CAP 12km NE of Batumi posted by Viper` | a task is posted (`command` when the HQ posts it) |
| `TASK COMPLETE: CAPTURE Gori` | an objective task is done |
| `TASK EXPIRED: CAS 8km S of Gori` | a task timed out |
| `TASK CANCELLED: CAP 12km NE of Batumi by Viper` | somebody removed it |
| `TASK CANCELLED: SUPPLY Senaki -- objective lost` | its base was lost or destroyed, so it can never complete |

A position task's location is described from the nearest objective:
`12km NE of Batumi`, or just the name when it is within 1 km of it.

## Limits

Server configuration; these are the engine defaults.

| | |
| --- | --- |
| Max open tasks per coalition | {{cfg:actions.Blue.add-task.kind.AddTask.max_per_side|12}} |
| Auto-expiry | {{cfg:actions.Blue.add-task.kind.AddTask.ttl_secs|7200}} s after posting (0 = never) |
| Cost | the action's cost (usually free) |
| Who can post | anyone on the coalition allowed to use actions |
| Who can remove | anyone on the coalition, unless the server limits it to the poster |

A full board answers *"blue already has 12 tasks on the board, remove one before
posting another"*. A board full of stale `CAS` pins from an hour ago is worse
than an empty one; that is the reason the limit exists.

## Where tasks show up

**The F10 map**: a coloured circle and a pin. The pin reads, for example:

```dcsmessage
TASK SUPPLY #21
Resupply this objective -- AI helo supply, convoy or crates
Senaki
completes at 80% supply
posted by Viper
posted 14:05Z
expires 16:05Z
```

Colours come from the server's task types, or your coalition's colour if none
is set.

**The briefing**: `F10 → Info → Situation` ranks the board alongside everything
else the engine thinks matters, and the slot-entry panel shows your top three.
`-brief` in chat gives the short list. See
[The Auto-Generated Briefing](./briefing.md).

**The GCI net**: unless the server turns it off, a newly posted task is called
out over SRS to your coalition. If you are on the
[GCI frequency](./comms-plan.md) you hear "new tasking" without touching the
map.

**The dashboard**: the Briefing tab draws the board on the live map, numbered
against clickable pins.

## In chat

`-action <name> <type> <key>` posts a task, where `<key>` is the text of one of
your map marks for a position task, or an objective name for an objective
task. `-action <name> <task id>` with the Remove Task action removes one.
`-action help` prints both with their exact names.

## How to use it well

- **Post before you fly, not after.** A `CAS` pin that goes up while you are
  still on the ramp gets you a wingman. One posted as you run out of fuel does
  not.
- **`SUPPLY` and `CAPTURE` are the high-value ones.** They are the only tasks
  that manage themselves, and they point at the two things that actually decide
  the round.
- **`CAP` marks intent, not a request.** Posting `CAP` over the corridor you
  need protected tells transport pilots where it is safe to run.
- **Be specific with position tasks.** A big CAP circle centred on nothing in
  particular is noise. Put it on the thing you care about.
- **Remove what's done.** The slots are shared across the whole side.

## Tasks from the HQ

On servers running the [Theatre HQ](../advanced/theatre-hq.md), the HQ posts its
own asks to the board, marked **posted by command**: capture its main effort,
supply the base that needs it most, CAS on the main effort, CAP over a base
under air threat. It keeps no more than
{{cfg:smart_commander.hq.max_tasks|3}} of its own up at once.

## See Also

- [Actions Menu](../f10-menu/actions.md): the rest of what lives under Actions
- [The Auto-Generated Briefing](./briefing.md): how tasks are ranked into the briefing
- [Live GCI (AWACS Calls)](./gci.md): the voice net that announces new tasks
- [AI Helo Missions](../advanced/helo-missions.md): the fastest way to close a SUPPLY task
