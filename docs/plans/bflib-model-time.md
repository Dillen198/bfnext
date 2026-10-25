# bflib: campaign timers on DCS model time

Audit finding f37. Status: plan only — no contained change fixes it.

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

## Why there is no contained fix

The gating of the slow pass itself is fine: the 1 s tick only runs while
model time runs, so `last_slow_timed_events` can't fire early. The issue is
in the ~200 places that compute "has X seconds passed", and in the persisted
timestamps. Switching some but not others would make durations inconsistent
with each other, which is worse.

## Plan

1. **A campaign clock.** Add `CampaignClock` to `Context` (session state)
   that maps model time to a campaign `DateTime<Utc>`:

   ```text
   campaign_now = anchor_wall + (model_now - anchor_model)
   ```

   anchored at mission start (`anchor_wall = Utc::now()`, `anchor_model =
   timer.getTime()`), advanced only by the tick (store the latest
   `model_now`, so it is cheap to read anywhere). While DCS is paused the
   campaign clock stops; at 0.5x it runs at half speed.
2. **One accessor.** `ctx.now()` (and `db.now()` for code that only has the
   Db: give `Ephemeral` a copy updated each tick). Pure functions keep taking
   `now: DateTime<Utc>` as a parameter, as most already do.
3. **Classify every `Utc::now()`.** Three kinds:
   - *campaign durations* (cooldowns, holds, repairs, production, deadlines,
     takeoff confirmation) -> `ctx.now()`;
   - *real-world times that must survive a restart* (life resets,
     `max_round_days`, sortie start/end for stats, ban expiry) -> keep
     `Utc::now()`, and say so in a comment;
   - *logging, perf, rate limits on log lines* -> keep `Utc::now()`.
   `grep -rn "Utc::now()" bflib/src` gives the list (202 at the time of
   writing). Do it file by file; each file is a reviewable PR.
4. **Persisted timestamps across a restart.** The campaign clock is anchored
   to the wall clock at mission start, so timestamps saved in campaign time
   stay comparable after a restart: the downtime still counts. That matches
   today's behaviour, so the save format does not change. If a timer should
   *not* run during downtime (e.g. a repair), store the remaining duration
   instead of a deadline -- a per-field decision, made in step 3.
5. **Detect pauses until then.** Cheap and independent: in the 1 s tick,
   compare the wall-clock and model-time deltas since the last tick; if the
   wall clock jumped by more than, say, 30 s more than model time, log a
   warning with the size of the gap. That at least names the cause when a
   resume sets off a burst of expired timers.

## Testing

Unit tests for `CampaignClock` (pause, slow motion, restart anchoring). In
DCS: pause a running server for 5 minutes with a capture hold, a repair and a
takeoff hold in progress, unpause, and check none of them completed early.
