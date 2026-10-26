# Plan: mlua 0.9 to 0.10

Status: plan only (audit finding f57). Nothing here is done.

## Why

mlua is pinned at 0.9.9 (once, in the root `[workspace.dependencies]`;
bftools pins its own copy). 0.9 is no longer developed, and every fix since
lands in 0.10 and later. Staying put is safe today but the gap only grows,
and a future security or soundness fix would arrive in a release we cannot
take without doing this anyway.

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

Smaller ones that touch us: `Lua::scope` and `UserData` signatures, error
type tweaks, and the `module` feature's entry point. Lua 5.1 stays supported
(`lua51`), and dynamic linking against DCS's `lua.dll` (`LUA_LIB`,
`LUA_LINK=dylib`) works the same way. Read the 0.10 changelog in full before
starting; this table is the shape, not the whole list.

## Size of the job

Counted on this branch:

| crate | `'lua` | `::<_, ` turbofish | FromLua/IntoLua impls |
| --- | ---: | ---: | ---: |
| dcso3 | 856 | 89 | 71 |
| dcso3-core | 56 | 0 | 6 |
| bflib | 243 | 60 | 3 |
| bfrange | 3 | 71 | 0 |
| bftools | 15 | 81 | 0 |
| bfcockpit/net | 10 | 0 | 0 |

dcso3 is most of it, and most of dcso3 is mechanical: every
`wrapped_table!` type is `Name<'lua> { t: Table<'lua>, lua: &'lua Lua }` and
becomes `Name { t: Table, lua: Lua }` (or drops the `lua` field: 0.10 tables
know their state). The macros in `dcso3/src/lib.rs` and `dcso3-core` carry
most of the impls, so changing them changes dozens of types at once.

The one non-mechanical part: dcso3's `HooksLua<'lua>` / `MizLua<'lua>`
newtypes encode *which* Lua state (hooks or mission) a value came from in
the lifetime-carrying wrapper. With owned handles that marker still works
(`MizLua(Lua)`), but every API taking `MizLua<'lua>` changes signature, so
bflib and bfrange change at every call site that names the lifetime (most
do not: they take `lua: MizLua` and let it be inferred).

## Order

1. A branch with mlua 0.10 in `[workspace.dependencies]` and bftools.
2. dcso3-core: the `__lua_*` macros and `lua.rs`. Small, and it checks the
   macro approach.
3. dcso3: macros first (`wrapped_table!`, `wrapped_prim!`, `bitflags_enum!`,
   `Sequence`), then module by module; keep `cargo test -p dcso3` (vendored
   Lua: `--features vendored-lua`) green per module.
4. livewx, bftools (bftools also uses `Lua` directly for miz templating),
   bfcockpit/net.
5. bflib and bfrange: mostly turbofish and signatures. Do it with the owners
   of those crates; it touches nearly every file that calls into Lua.
6. Release build, then the full `TESTING.md` pass in DCS: the risk is not
   compile errors but a conversion that now behaves differently at runtime
   (nil handling, integer/number coercion).

## Risks

- Behaviour changes in conversions (0.10 is stricter in places). The
  event, miz_pack and enum tests in dcso3 cover some of this; add a test per
  conversion that has bitten before (nil strings, numeric strings) first.
- Owned handles keep the Lua state alive. A handle stashed in a Rust
  structure past the end of a DCS callback was a compile error in 0.9 and
  is legal in 0.10; bflib's `Context` must not start holding Lua values
  across frames by accident. Grep for new `Table`/`Function` fields in
  long-lived structs in review.
- The DLLs must still link against DCS's `lua.dll` and carry no Lua of their
  own: `dumpbin /dependents bflib.dll` lists `lua.dll`, and dcso3's
  `vendored-lua` test feature still refuses a release build.

## Effort

dcso3 + dcso3-core: two to three days. bflib + bfrange: two days with their
owners. Testing in DCS: a session. Do it between campaign rounds.
