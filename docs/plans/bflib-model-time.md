# bflib: campaign timers on DCS model time -- done

Audit finding f37. Status: **done** (branch `bflib-redesign-870271`). The
problem as it was found, then what was built.

## The problem

The engine's timers are scheduled on DCS *model* time (`timer.getTime()`:
the 1 s tick, the 5 Hz message drain, the 10 Hz admin drain, `timer.
scheduleFunction`), but every duration the campaign measures uses the *wall
clock*: about 200 `Utc::now()` calls in 38 files, and about 60 persisted
`DateTime<Utc>` fields (life reset times, capture holds, cooldowns, repair
and production timers, deadlines, `round_started`, ...).

The two only agree while the simulation runs at 1x and never stops. They
diverge when:

- **the server pauses** (DCS's "pause when empty", an admin pause, a long
  load or a hang): model time stops, the wall clock doesn't. When it resumes,
  every timer that was due during the pause fires at once on the first tick
  -- lives reset, repairs and production complete, cooldowns are over,
  capture holds have run out -- and anything with a deadline (banned-weapon
  landing, takeoff hold) may expire the moment the player is back;
- **the server is slow** (frame rate collapse): model time advances slower
  than real time; the campaign's clocks do not slow down with it;
- **the server restarts**: wall-clock deadlines saved before the restart are
  compared with the wall clock after it, so the downtime counts as campaign
  time. This one is often *wanted* (life resets, `max_round_days`) and has
  to stay that way.

## What was built

- **`crate::clock`** (`bflib/src/clock.rs`): `CampaignClock`, anchored at
  mission start and moved only by model time (`timer.getTime()`), so it
  stops while DCS is paused and runs slow in slow motion. It lives in
  `Ephemeral::clock`; `ctx.now()` / `db.ephemeral.now()` read it. Until the
  mission starts it reads the wall clock (so does anything in tests that
  never starts it). It is advanced by the 1 s tick, the 5 Hz message drain,
  the 10 Hz admin drain and on every event, so it is never more than
  ~100 ms stale.
- **Restarts.** A snapshot writes `Persisted::clock` (campaign time and the
  wall clock at the same moment); the next mission starts the clock at
  `saved.campaign + (wall_now - saved.wall)`, so the downtime still counts
  (as before) and what was paused last session stays paused. Save schema
  bumped to 2. A save without the field (schema 0/1) resumes at the wall
  clock, which is what its timestamps were measured on, so nothing in it
  needs rewriting; the conversion on load is choosing the anchor.
- **Classification.** Every `Utc::now()` in bflib (outside `bg/`, which only
  stamps logs, stats and saves) was sorted into:
  - *campaign time*: cooldowns, holds, repairs, production, CSAR, C-130
    cargo, capture timers, recon, intel decay, takeoff holds, banned-weapon
    deadlines, event and CAP expiry, HQ operations, ground war, JTAC smoke
    cooldown, the slow-pass schedule, lives (resets and penalties), the tick
    and event timestamps handed to all of the above;
  - *wall clock, on purpose*: log and perf stamps (`record_perf` gets its own
    wall-clock start everywhere), spawn and logistics time budgets, the
    loading state machine, ban expiry, the auto shutdown and its warnings
    (`check_auto_shutdown`, `-time`, Info > Time), the round length
    (`max_round_days`), stats records (stamped by the background thread),
    the cockpit message feed's `at`, the briefing's `generated`, rate limits
    on log lines.
- The pause detector (`rules::clock_gap_secs`) stays: it now says the
  campaign timers stood still with the simulation.

## Known edges

- Absolute campaign timestamps that leave the engine (a hold's or repair's
  start in an admin query, life-reset stamps in player stats) are campaign
  time, up to the session's paused time behind the wall clock. Anything that
  turns them into a countdown against the real clock (a browser) is off by
  that much until the next restart re-anchors.
- An older engine reading a schema 2 save treats its timestamps as wall
  clock: the same error, once.

## Testing

`clock::tests` (pause, slow motion, never backwards, restart anchoring, save
round trip), `db::ephemeral::clock_tests` (a 30-minute ramp block survives a
pause), `db::persisted::tests` (old save has no clock, the clock round
trips). In DCS: see the release checklist in the branch report -- pause a
running server for 5 minutes with a capture hold, a repair and a takeoff
hold in progress, unpause, and check none of them completed early; restart
and check the downtime counted.
