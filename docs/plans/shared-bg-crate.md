# Plan: one shared crate for the mission-DLL plumbing

Audit finding f124. Plan only: nothing here is implemented yet.

## The duplication

bfrange was started by copying bflib's plumbing, and the two have drifted
since:

| Piece | bflib | bfrange | Notes |
|---|---|---|---|
| Line-buffered logger feeding the background thread (`LogHandle`, `LOGBUF`) | `bflib/src/bg/mod.rs` ≈114-138 | `bfrange/src/bg.rs` ≈88-110 | bflib writes to a fallback file when the thread is gone; bfrange drops the line |
| `RUST_LOG` → `LevelFilter`, `WriteLogger` setup | `setup_logger` ≈966-985 | `init` ≈450-462 | bflib also wraps it in `QuietNetidx` (drops netidx chatter); bfrange doesn't, so its log gets the same noise |
| Log rotation on start (`<stem><UTC ts>.<ext>`) | `rotate_log` ≈374 | `rotate` | bfrange's now also rotates the JSONL feeds (`range_jsonl_rotate_mb`, f39) |
| JSONL sinks, one `write_all` per record | `Logs::Files` / `Logs::Netidx` | `Sinks` | Same record framing; bfdb tails both |
| tokio runtime on a dedicated thread, fed by an unbounded channel | `init` ≈1035 (with a supervisor that restarts a panicked runtime) | `init` (no supervisor) | bfrange loses its log, feeds and RPCs for the rest of the session if the runtime dies |
| netidx publisher from a `client.json` + bind spec | `bg/mod.rs` | `background_loop` `Task::Start` | Same config shape |
| `build.rs` (git sha + build epoch → `BFNEXT_BUILD_GIT/EPOCH`) | `bflib/build.rs` | `bfrange/build.rs` | Byte-identical, also `bftools/build.rs` |
| Hooks loader: stage `_x.dll` over `x.dll`, then `require` | `bflib/lua/hooks.lua` | `bfrange/lua/bfrange_hooks.lua` | bfrange's install is now checked and atomic (f41); bflib's still removes the staged file after an unchecked write |

Each fix lands in one copy and not the other (the supervisor, `QuietNetidx`,
the fallback log on one side; JSONL rotation and the safe DLL swap on the
other).

## Licensing comes first

bflib is AGPL v3 (Eric Stokes' copyright, plus this fork's changes), bfrange
is all rights reserved, dcso3 and livewx are MIT. A crate both link must be
under terms both can use:

- Seed it from **bfrange's** `bg.rs` (Dillen's own code), not bflib's, and
  publish it MIT like dcso3/livewx. bfrange can use an MIT crate; so can the
  AGPL bflib.
- Re-implement the bflib-only features (supervisor, fallback log,
  `QuietNetidx`) rather than copy them, or get the upstream copyright
  holder's agreement for those parts.
- This interacts with the open licence question (audit f48); settle that
  before code moves.

## Proposed crate: `fowlbg` (workspace member, MIT)

```
fowlbg/
  src/lib.rs        Bg::init(write_dir, Options) -> Bg; Bg::send(T)
  src/log.rs        LogHandle, level from RUST_LOG, netidx noise filter,
                    fallback file when the thread is gone
  src/rotate.rs     rotate(path) and rotate_if_over(path, max_bytes)
  src/jsonl.rs      JsonlSink: open/append/one-write-per-record/rotate
  src/runtime.rs    the supervised tokio thread
  src/netidx.rs     publisher from (client.json, bind) with the same errors
  build_info.rs     shared by the three build.rs (see below)
```

The background loop becomes generic over the user's task type:

```rust
pub trait Handler: Send + 'static {
    type Task: Send + 'static;
    /// One task; a panic here is caught and logged, the loop carries on.
    fn handle(&mut self, ctx: &mut BgCtx, task: Self::Task) -> impl Future<Output = ()> + Send;
}
```

`BgCtx` owns the log writer and the JSONL sinks, so each crate keeps its own
tasks (bflib: saves, stats, netidx archive; bfrange: records, RPC queue)
while the plumbing is shared.

`build.rs`: Cargo can't share a build script directly. Either a tiny
`fowl-build-info` build-dependency with `pub fn emit()` that each `build.rs`
calls in one line, or keep the three copies and add a CI step that diffs
them. The build-dependency is cleaner and has no runtime cost.

Hooks loader: one `lua/stage_dll.lua` fragment (the checked install from
`bfrange_hooks.lua`), concatenated into each hooks file by the build or
release step, or simply kept in step by hand with a test that diffs the
install function between the two files. bflib's copy should get the f41 fix
either way.

## Migration order

1. Settle licensing (above).
2. Create `fowlbg` from bfrange's `bg.rs`; port bfrange onto it with no
   behaviour change. bfrange's tests plus a live range start/stop confirm
   the log, both JSONL feeds and the RPCs.
3. Add the supervisor, fallback log and netidx filter to `fowlbg` (bfrange
   gains them).
4. Port bflib onto `fowlbg`, keeping its save/backup code in bflib (that is
   campaign logic, not plumbing). Check the save path and the stats archive
   on a test server.
5. `fowl-build-info` for the three `build.rs`.
6. Share or diff-test the hooks loader; give bflib the safe DLL install.

Each step is its own PR; bflib is the riskier half and goes last.

## Risks

- **Cargo.lock churn**: `fowlbg` needs nothing new (tokio, netidx,
  simplelog, log are already in the workspace), so the lock only gains the
  crate itself.
- **Logger is process-global**: `log::set_boxed_logger` can be called once
  per process. bflib and bfrange are never loaded in the same DCS, but the
  crate should treat a second `init` as a no-op returning the existing
  sender, as both do today.
- **On-disk formats**: the JSONL framing, file names and the rotation name
  pattern (`<stem><YYYYmmddTHHMMSSZ>.<ext>`, which bfdb's range ingest now
  looks for) must not change.
- **bftools** is outside the workspace with its own lock; it only needs the
  build-info piece.
