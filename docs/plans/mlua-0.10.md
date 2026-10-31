# Plan: mlua 0.9 to 0.10

Status: done in code (audit finding f57), mlua 0.10.5. Compiles and passes
the workspace tests; not yet run in DCS. The in-game checks are at the end.

## Why

mlua was pinned at 0.9.9 (once, in the root `[workspace.dependencies]`;
bftools pins its own copy). 0.9 is no longer developed, and every fix since
lands in 0.10 and later. Staying put was safe but the gap only grew, and a
future security or soundness fix would arrive in a release we could not
take without doing this anyway.

0.10, not 0.11+: 0.10 is the step the plan was sized for, and it keeps
mlua-sys on 0.6.x, so the Lua 5.1 build and the `LUA_LIB` / `LUA_LINK=dylib`
link setup are the ones we already had.

## What changes in 0.10

The big one: Lua handles lose their lifetime. In 0.9 `Table<'lua>`,
`Value<'lua>`, `Function<'lua>`, `String<'lua>` borrow the `Lua` state; in
0.10 they are owned handles (`Table`, `Value`, ...), and the conversion
traits drop the parameter:

| 0.9 | 0.10 |
| --- | --- |
| `impl<'lua> FromLua<'lua> for T { fn from_lua(v: Value<'lua>, lua: &'lua Lua) }` | `impl FromLua for T { fn from_lua(v: Value, lua: &Lua) }` |
| `impl<'lua> IntoLua<'lua> for T { fn into_lua(self, lua: &'lua Lua) -> Result<Value<'lua>> }` | `impl IntoLua for T { fn into_lua(self, lua: &Lua) -> Result<Value> }` |
| `t.raw_get::<_, V>(k)` | `t.raw_get::<V>(k)` (key type is `impl IntoLua`) |
| `f.call::<_, R>(args)` | `f.call::<R>(args)` |

The smaller ones that touched us:

- `Error` is only `Send + Sync` with the `error-send` feature. Every crate
  turns Lua errors into `anyhow::Error` with `?`, so the workspace (and
  bftools) enable it. It is independent of `send`, which module mode still
  disables.
- `String::to_str` returns a `BorrowedStr` and `as_bytes` a
  `BorrowedBytes` (deref to `str` / `[u8]`); `to_string_lossy` returns a
  `String`.
- `Table::pairs` and `sequence_values` borrow the table instead of
  consuming it.
- `Value` has an `Other` variant (unknown Lua types); exhaustive matches
  handle it.
- `FromLuaConversionError::to` is a `String`.
- `TableExt` is `ObjectLike` (`LuaObjectLike` in the prelude);
  `get_metatable` is deprecated for `metatable`.

## What was done

1. `[workspace.dependencies]` and bftools: `mlua = "0.10.5"` with `lua51`
   and `error-send` (bftools also keeps `serialize` and `vendored`). The
   DLL crates still add `module`; nothing else about linking changed.
2. dcso3-core: the `__lua_*` macros and `lua.rs`, `net.rs` conversions.
   The perf singletons (`thread_singleton!`, used by `dcso3::perf::Perf`
   and bfprotocols' `Perf`) no longer hand out `&'static mut` from an
   `unsafe fn`: `get_mut()` returns a `SingletonGuard` with a per-thread
   held flag, like bflib's `CtxGuard`. Asking again while a guard is alive
   (a Lua callback that re-entered) gets a throw-away instance, so those
   samples are lost instead of aliasing. `reset()` is safe too and does
   nothing while a guard is alive. The `unsafe { Perf::get_mut() }`
   wrappers in bflib are gone.
3. dcso3: `wrapped_table!` types, `Sequence`, events and the miz types are
   lifetime-free. Where a wrapper kept `lua: &'lua Lua` it now keeps an
   owned `Lua` (a reference count; 0.10 handles do not expose their
   state). `Sequence` has its own owning iterator (`SequenceIter`), since
   0.10's `sequence_values` borrows. `MizLua<'lua>` / `HooksLua<'lua>` stay
   `Copy` wrappers around a borrowed `&Lua`: they are still the marker for
   which Lua state a call is in, and bflib / bfrange code that takes
   `lua: MizLua` did not change.
4. livewx, bfcockpit/net, bftools: turbofish and dropped lifetimes. bftools
   used `'static` handles from a leaked scratch `Lua`; those lose the
   lifetime too.
5. bflib and bfrange: turbofish, dropped lifetimes on wrapper types. The two
   bflib structs that hold a `MizLua` (`Entered`, `SpawnCtx`) keep their
   lifetime. `bflib/src/bg` and `bfrange/src/bg.rs` needed no change.
6. `dcso3/src/conversion_tests.rs` pins the conversions that have bitten
   before (nil where a string is expected, numeric strings, integral
   numbers arriving as `Integer`, float to integer, enums and slot ids from
   strings, serde of `None`). It was written and passed against 0.9 first,
   then passes unchanged on 0.10.

## Behaviour notes

None of the pinned conversions changed. Worth knowing anyway, since the
tests now document them:

- A fractional number read as an integer is truncated toward zero, not
  refused (`4.5` and `'4.5'` read as `i64` give `4`). Same in 0.9 and 0.10.
- Numeric strings convert to numbers (`'42'` as `i64`, `'4.5'` as `f64`),
  and numbers to strings (`42` as `String` is `"42"`, `42.0` is `"42"`).
- Lua 5.1 has one number type: an integral float stored from Rust reads
  back as `Value::Integer`.
- serde turns `None` into mlua's null (a null light userdata), not nil, so
  the key exists in the table.

New since 0.9, by design of the conversion: perf samples recorded while
another perf guard is alive on the thread are dropped (see 2 above). bflib
holds its `Perf` across some Lua calls (F10 actions, respawn after load);
a db method that takes its own `Perf::get_mut()` underneath one of those
now records into a throw-away instance. Before, both wrote through two
`&mut` to the same counters, which was undefined behaviour.

## Risks, after the fact

- Owned handles keep the Lua state alive, and a handle stashed in a Rust
  structure past the end of a DCS callback was a compile error in 0.9 and
  is legal in 0.10. Reviewed: no struct or `'static` closure in bflib,
  bfrange or livewx gained a Lua handle in the conversion. The one
  pre-existing field of that kind, `ArtilleryAdjustment::tracked:
  Option<(Weapon<'static>, ..)>` in the JTAC state, was never set and is
  removed. Keep grepping for `Table` / `Function` / `Value` / dcso3 wrapper
  fields in long-lived structs (`Context` and everything it owns) in
  review: the compiler no longer catches it.
- The DLLs must still link against DCS's `lua.dll` and carry no Lua of
  their own. `dumpbin /dependents` on the release `bflib.dll` and
  `bfrange.dll` lists `lua.dll`; dcso3's `vendored-lua` test feature still
  refuses a release build.

## In DCS

The risk is runtime, not compile: a conversion that now behaves
differently, or the module entry point. Run the full `TESTING.md` pass,
and in particular:

- Both DLLs load: mission start log shows bflib's init (hooks and mission
  environment) with no Lua error, and the same for bfrange on the range
  box.
- Events: birth, shot, hit, kill, takeoff, land, eject all arrive and are
  handled (stats, lives, kills).
- Slotting: try-change-slot accepts and refuses correctly (the auth hooks
  are `wrap_f_deny`), multicrew and CA slots too.
- Chat commands, and the F10 menu: actions, JTAC (including artillery
  adjust), cargo, troops, EWR.
- Spawning: AI groups and statics from templates, logistics deliveries,
  respawn after a restart (persisted state loads and every group is back).
- Markups on the F10 map (`markupToAll` with its variadic arguments).
- Scheduled functions keep firing for a long session; the perf log every
  10 minutes still has numbers in it.
- The cockpit module (`bfcockpit_net.dll`): `request` / `poll` round trip.
- bftools: build a mission with `bftools miz ...` from the usual templates
  and load it.
