// Copyright 2026 Dillen Weerasinghe. MIT licensed: see fowlbg/LICENSE.
//! The background-thread plumbing the mission DLLs share.
//!
//! bfrange started as a copy of bflib's, and the two drifted: each fix
//! landed in one copy. This is the one copy (docs/plans/shared-bg-crate.md).
//! It holds only plumbing; what a task does stays in each DLL.
//!
//! * [`runtime`]: the background thread and its tokio runtime, fed by an
//!   unbounded channel of the DLL's own task type, optionally supervised.
//! * [`log`]: the logger that turns `log` records into lines for that
//!   thread, the `RUST_LOG` level, the netidx noise filter and the
//!   fallback log.
//! * [`rotate`], [`jsonl`]: the rotation name pattern and the JSONL feeds
//!   bfdb tails.
//! * [`netidx`]: the netidx client config, with opt-in TLS, and the
//!   publisher with its bind retry.
//!
//! With `default-features = false` only [`netidx`]'s config half is built,
//! for crates that carry the TLS block in their config types.

pub mod netidx;

#[cfg(feature = "runtime")]
pub mod jsonl;
#[cfg(feature = "runtime")]
pub mod log;
#[cfg(feature = "runtime")]
pub mod rotate;
#[cfg(feature = "runtime")]
pub mod runtime;

#[cfg(feature = "runtime")]
pub use crate::{
    log::{fallback_log, panic_msg, panic_payload_msg},
    runtime::{Handler, Options, Task, heartbeat_age, init, sender},
};
