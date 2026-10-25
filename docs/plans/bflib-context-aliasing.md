# bflib: aliased `&mut Context` — what is fixed and what is left

Audit finding f30. Status: partly fixed in code, the rest planned here.

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

## What is fixed (branch `audit-bflib-ab6e02`)

1. **Event deferral covers what our calls fire.** While a `DeferEvents` guard
   is alive (every 1 s tick, the 10 Hz admin drain, the 5 Hz message-queue
   drain, the mission init, every `vector_strike` API call, every F10 menu
   handler), `on_event` does not touch `Context`. It queues:
   - `Birth` (re-resolved from its object id when handled);
   - `Dead` / `UnitLost`, with the object's name and position captured into
     a `DeathObj` while it still exists, handled by `handle_death` (the same
     function the live path now uses, so there is one implementation);
   - `PlayerLeaveUnit` (already just an object id);

   and ignores the no-op events (mark events without an initiator, sim
   freeze, repair start/stop, ...) without touching `Context`. The holder
   drains the queue (`drain_deferred_events`) between subsystems, with no
   borrow held.
2. **F10 menu handlers** take the state through `Entered::enter(lua)`, which
   starts deferral and drains on drop when it is the outermost holder. A
   handler that spawns crates no longer re-enters `on_event` with the state
   held.
3. **Cockpit menu clicks** (`AdminCommand::CockpitMenuInvoke`) used to run the
   item's F10 handler from inside `run_admin_commands`, i.e. a second
   `get_mut()` while the admin drain held the first. They are now queued
   (`Context::pending_menu_invokes`) and run by `admin::run_menu_invokes`
   after the drain has let go.
4. **The `vector_strike` API** goes through `with_context`, which refuses a
   re-entrant call (one made while this thread holds the state) and defers
   events like a tick. Its `on_event` hook is no longer called from inside
   the tick: events are queued and delivered after the tick returns.

## What is left

### A. Events still handled on the spot while deferring

`Hit`, `Kill`, `Shot`, `PilotDead`, `Ejection`, `Crash`, `Takeoff`, `Land`,
`PlayerEnterUnit`, mark events *with* an initiator, `MissionEnd`. These come
from the simulation, not from inside our calls, as far as we know. Two
exceptions to check in DCS:

- `trigger.action.explosion` (nukes, artillery effects): if DCS applies the
  damage synchronously, `Hit` / `Kill` fire inside the call. Deferring them
  needs the target's life captured at event time (`on_hit` reads
  `target.get_life()`), the same way `DeathObj` captures name and position.
- `PlayerLeaveUnit` replayed after `force_player_slot`: the handler re-reads
  the unit for the "landed in the last ten seconds" settlement. If the
  aircraft is gone by the time the queue drains, that settlement is skipped
  (the slot change itself is still handled by the hooks path). Rare: it needs
  a player forced to spectators within ten seconds of landing.

To find out, log the event kind when a non-deferrable event arrives with
`DEFER_DEPTH > 0` (a one-line `debug!` in `on_event`), run a session with a
nuke and an era-rules kick, and read the log.

### B. Menu builders and helpers that take their own state inside a tick

`Entered::enter` called while a tick already holds the state is a no-op
(nested), so it is still a second `&mut`. The menus are built from the tick
(`menu_init_queue`), and some handlers call each other. Same for the
`Context::get_mut()` calls left in `lib.rs` (17), `atis.rs`, `situation.rs`,
`cockpit.rs`. These need the state passed in instead of fetched.

### C. The real fix: one owner, scoped borrows

Replace `static mut SELF: Option<Context>` with a thread-local
`RefCell<Context>` (or an `UnsafeCell` with a borrow flag) and make every
access a short scoped borrow:

```rust
thread_local! { static CTX: RefCell<Option<Context>> = RefCell::new(None); }

fn with_ctx<R>(f: impl FnOnce(&mut Context) -> R) -> R {
    CTX.with(|c| f(c.borrow_mut().as_mut().expect("context")))
}
```

A re-entrant borrow then panics (in debug, or `try_borrow_mut` returns an
error) instead of being silent UB, which turns every remaining aliasing path
into a visible, fixable bug. Steps:

1. Introduce `with_ctx` next to `get_mut`, with `try_borrow_mut` in
   `on_event`: on failure, queue the event (generalising `DeferredEvent`) or
   log it. Ship that alone first: it costs nothing and reports the paths.
2. Convert the entry points one at a time (timers, hooks, menu handlers, API,
   RPC drain), each taking the borrow for exactly the span where it does not
   call into DCS in a way that can call back.
3. Where a subsystem must call DCS mid-way (spawns, despawns), split it:
   compute what to do under the borrow, release, make the DCS calls, re-borrow
   to record the result. `Ephemeral::push_despawn` / the spawn queue already
   have this shape; the despawn processing would run its `destroy()` calls
   outside the borrow.
4. Remove `get_mut`.

Size: roughly 130 call sites, most mechanical (`let ctx = unsafe {
Context::get_mut() };` becomes a `with_ctx(|ctx| { ... })` block or a
parameter). The hard part is step 3 for the handful of subsystems that spawn
or destroy in the middle of a loop over state. Estimate: several days, best
done in small PRs, each tested on a live server.
