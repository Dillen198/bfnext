# bflib: aliased `&mut Context` -- done

Audit finding f30. Status: **done** (branch `bflib-redesign-870271`). What
follows is the problem, what the fix is, and the two things that are only
best-effort.

## The problem

All campaign state lives in one `static mut` (`Context::get_mut()` in
`bflib/src/lib.rs`), handed out as `&'static mut Context` from about 130
places. DCS runs every callback on one thread, so there is never real
concurrency, but DCS calls back into us *synchronously from inside our own
API calls*:

- `coalition.addGroup` / `addStaticObject` fire `Birth` for every unit before
  they return.
- `Object.destroy` fires `Dead`, `UnitLost` and, for a player's aircraft,
  `PlayerLeaveUnit` before it returns.
- `net.force_player_slot` fires `PlayerLeaveUnit`.
- `trigger.action.markToAll` and friends can fire mark events.

Each of those lands in `on_event`, which takes a second `&mut Context` while
the caller still holds the first. Two live mutable references to the same
data is undefined behaviour in Rust no matter how careful each side is: the
compiler may assume the caller's view didn't change across the call (cached
loads, reordered stores). It has worked so far because the optimiser hasn't
had a reason to exploit it, not because it is sound.

## The fix

1. **One owner, one borrow flag.** The `static mut` and `Context::get_mut()`
   are gone. The state lives in a per-thread slot that is only reachable
   through a `CtxGuard` (`lib.rs`), which sets a thread-local flag while it
   is alive. `Context::try_borrow()` returns `None` when the flag is set;
   `Context::borrow(what)` panics in debug builds and tests
   (`debug_assert!`) and returns an error in release. So a second
   `&mut Context` is impossible, and a path that would have taken one is a
   test failure in debug and a logged refusal in release, not silent UB.
   Tests: `ctx_guard_tests` in `lib.rs`.
2. **Holding the state is deferring events.** `DeferEvents` / `DEFER_DEPTH`
   are gone: the borrow flag is the signal. `on_event` is now registered by
   bflib itself (`add_event_handler`, so it gets DCS's raw event table) and,
   when the state is held, queues *every* event kind:
   - Birth, Dead/UnitLost, PlayerLeaveUnit as before (re-resolved by object
     id; dying object captured into `DeathObj`);
   - every other event as `DeferredEvent::Raw`: DCS's own table, kept in the
     Lua registry and parsed again when handled. For Hit and Kill the
     target's dead/alive state is captured at event time (`target_dead`), so
     a deferred hit does not ask an object that may be gone;
   - `onPlayerDisconnect` landing inside one of our calls (a kick) is queued
     as `DeferredEvent::Disconnect`.

   The holder drains the queue (`drain_deferred_events`) between subsystems
   and before it lets go. The live `on_event` path takes the guard too and
   drains what its own handlers fire (a dismount's Birth used to re-enter).
3. **Entry points.** Every DCS-called entry takes the guard exactly once:
   the 1 s tick, the 5 Hz message drain, the 10 Hz admin drain, the mission
   init (and its error timer), every hook (`onPlayerTryConnect`,
   `TrySendChat`, `TryChangeSlot`, `MissionLoadEnd`, `Disconnect`,
   `SimulationFrame`), F10 menu handlers (`Entered::enter`, now fallible),
   scheduled one-shots (`with_ctx`: the ATIS and the slot briefing), the
   `vector_strike` API (`with_context`, which refuses re-entry without the
   debug panic: a mission script may legitimately try from its own event
   handler). The 1 s tick and the admin drain let go before calling out to
   code that takes the state itself (the API's `on_event` hook, cockpit menu
   clicks).
4. **Nested takers removed.** Code that fetched its own state while its
   caller had it now takes `&mut Context`:
   - chat `-jtac` commands ran the F10 JTAC handlers from inside the tick;
     each handler they use is split into `x` (enters) and `x_in` (takes
     `ctx`);
   - `atis::schedule_atis`, `situation::schedule_slot_briefing` (called from
     `handle_birth`), `atis::send_full_weather` / `send_weather_brief`
     (called from the tick's weather requests and the Info menu);
   - `menu::jtac::add_menu_for_jtac` (a menu builder run inside two other
     handlers) takes the pinned flag as a parameter;
   - `jtac_focus_my_mark` -> `jtac_set_focus_in`; `spawn_n_helo_crates`
     reads its config under a short borrow before `spawn_n_crates` enters;
   - `delayed_init_miz` takes the init timer's state.

## Best effort, by design

- **A deferred event that needs an object that is gone.** A Raw event is
  parsed again when drained. If what it names was destroyed in the same tick,
  handlers that call into DCS on it get an error (logged) -- the same as an
  event that arrives for a destroyed object today. Only Birth, Dead/UnitLost
  and Hit/Kill capture what they need up front, because those are the ones
  our own calls fire. The other kinds are not expected from inside our calls
  at all; each one that is deferred logs `deferring an event that fired
  inside one of our calls` at debug, so a session log answers whether they
  ever are (e.g. `trigger.action.explosion` applying damage synchronously).
- **`PlayerLeaveUnit` replayed after `force_player_slot`**: unchanged from
  before, the "landed in the last ten seconds" settlement re-reads the unit
  and is skipped if it is gone by the time the queue drains.
- **Hooks state.** A hook (the GUI Lua state) can't drain mission events:
  events its calls fire are queued and handled by the next mission-side
  holder, at most 100 ms later (the admin drain).

Not part of f30: `Perf::get_mut` (dcso3-core's per-thread singleton) is
still `unsafe fn` with the same "don't hold it across a re-entrant call"
contract. `on_event` no longer holds it across the handler.
