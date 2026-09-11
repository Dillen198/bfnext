# Actions Menu

`Actions` is where you spend points on things that fly, shoot or think for you —
AWACS, tankers, bombers, drones, artillery, AI helo logistics — and where the
coalition's tasking board lives.

## Opening it

At the F10 root you will see **`Actions>>`** — a *command*, not a folder. Select
it once and it rebuilds itself into the full `Actions` menu, populated with the
world as it is right now: your current map marks, the JTACs currently alive, the
objectives currently capturable.

**That is also how you refresh it.** Placed a new map mark? Re-open `Actions>>`.

```
F10 → Actions>>  →  Actions
├── <action> (<cost> pts)        one entry per configured action
├── Add Task                     post a coalition task
├── Remove Task                  take one off the board
├── Request Fires                artillery, if your side has guns alive
└── AI Helo Missions             troop insertion / resupply
```

## Targeting: map marks

Most actions need a position, and you give one with an F10 **map mark**:

1. F10 map → right-click → Add mark. Name it (≤ 24 characters) or leave it.
2. Cockpit → `Actions>>` → the action → your mark is in the list.

Unnamed marks appear as `Mark 1`, `Mark 2`, … in menu order. Duplicate names
collide, so number them: `CAS1`, `CAS2`. You only ever see your own marks.

## The live action set

What is actually available is per-server and per-coalition. On the live mission:

| Action | Cost | What it does |
| --- | --- | --- |
| **E-3A AWACS** (red also has **A-50**) | 100 | Puts an AWACS on station at your mark. Frequency and TACAN appear in `Info → Support & Radios`. |
| **AWACS Waypoint** | 10 | Moves the AWACS you already have. |
| **KC-135 Boom** / **KC-135 Basket** (red: **IL-78M Basket**) | 50 | Tanker on station. Boom for receptacle aircraft, basket for probes — bring the right one. |
| **Air Refuelers Waypoint** | 10 | Moves the tanker. |
| **B-1B Attack** (red: **Tu-22M3 Attack**) | 100 | Heavy bomber strike. Expands into a list of your JTACs — the bomber hits whatever that JTAC is tracking. |
| **JTAC Drone** | 25 | Puts an orbiting drone JTAC over your mark. The cheapest way to get eyes and a laser on a target area. |
| **DRONE Waypoint** | 5 | Moves the drone. |
| **Naval Strike** | 50 | Cruise missiles from your nearest carrier in range, at an enemy objective you pick. |
| **Move (Units/Troops)** | 10 | Sends one of your deployed ground groups or squads to a mark. Carries a penalty if the group is lost. |
| **Carrier Waypoint** | free | Sails a carrier group to a mark. See [Carrier Operations](../gameplay/carrier-ops.md). |
| **Add Task** / **Remove Task** | free | The [tasking board](../gameplay/tasking-board.md). |

Costs are in the menu label. If an entry is missing, the server has not
configured it for your coalition.

### Penalties

Several actions carry a penalty on top of the cost — charged again if the asset
is lost early. An AWACS shot down soon after launch costs you twice. Put support
aircraft somewhere they will survive.

## Request Fires

Appears only when **your side has artillery groups alive**. Pick a map mark and
the guns in range fire on it. Free.

The dedicated per-battery menu — round counts, fire-for-effect, half and quarter
salvoes — lives under `F10 → JTAC`. This entry is the fast path: mark, fire,
go. See [Artillery Missions](../advanced/artillery.md).

## AI Helo Missions

Orders a real AI helo to cold-start, fly, land and deliver — capture troops into
an objective, or supply into a base that is running dry. You do not have to be
in a helo.

Full details in [AI Helo Missions](../advanced/helo-missions.md).

## Add Task / Remove Task

Posts a CAP / CAS / SEAD / STRIKE / LOGISTICS / CSAR task at a map mark, or a
CAPTURE / SUPPLY task against a base — visible on the F10 map to your entire
coalition, ranked into the briefing, and called out on the GCI net.

Full details in [The Tasking Board](../gameplay/tasking-board.md).

## Waypoint actions

`AWACS Waypoint`, `Air Refuelers Waypoint`, `DRONE Waypoint`, `Carrier Waypoint`
and `Move (Units/Troops)` all reposition something that already exists, for far
less than calling a new one. **Move the asset you have** before you buy another.

## Managing what you deployed

| | |
| --- | --- |
| Move a group | `Actions>>` → `Move (Units/Troops)` → group → mark |
| Delete a group | `-delete <group-id>` in chat |
| Bind a troop group | `-bind <troop-id>` in chat |
| See what you have | `F10 → Info → Support & Radios`, and the F10 map |

Group ids are in the menu labels and the map markers. See
[Chat Commands](../gameplay/chat-commands.md).

## Troubleshooting

**"My mark isn't in the list."** The menu was built before you placed it.
Re-open `Actions>>`.

**"The action I want isn't there."** Not configured for your coalition on this
server, or — for `Request Fires` — you have no artillery alive.

**"Nothing happened."** Every action answers with a panel message, success or
failure (`could not start …`, `not enough points`). If you saw nothing at all,
the engine did not recognise you as being in a slot; re-enter it.

**"It says I can't afford it."** `-balance`, or `Info → My Status`. Note that
the objective you took off from can contribute to some costs.

## See Also

- [F10 Menu Overview](./overview.md)
- [Action Types](../reference/action-types.md) — what each action actually spawns
- [The Tasking Board](../gameplay/tasking-board.md)
- [AI Helo Missions](../advanced/helo-missions.md)
- [Artillery Missions](../advanced/artillery.md)
- [Points and Lives](../gameplay/points-and-lives.md)
