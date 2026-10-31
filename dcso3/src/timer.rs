/*
Copyright 2024 Eric Stokes.

This file is part of dcso3.

dcso3 is free software: you can redistribute it and/or modify it under
the terms of the MIT License.

dcso3 is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.
*/

use crate::{as_tbl, cvt_err, record_perf, wrapped_table, LuaEnv, MizLua, Time};
use anyhow::Result;
use log::error;
use mlua::{prelude::*, Value};
use serde_derive::Serialize;
use std::{
    backtrace::Backtrace,
    ops::Deref,
    panic::{self, AssertUnwindSafe},
};

/// How long a scheduled function that failed waits before it runs again.
const RETRY_AFTER: f64 = 1.;

/// Run a scheduled callback, catching errors and panics. `None` means it
/// failed, and the failure has been logged.
fn run_scheduled<R>(name: &str, f: impl FnOnce() -> Result<R>) -> Option<R> {
    match panic::catch_unwind(AssertUnwindSafe(f)) {
        Ok(Ok(r)) => Some(r),
        Ok(Err(e)) => {
            error!("{name} failed: {e:?}");
            None
        }
        Err(e) => {
            let msg = e
                .downcast_ref::<&str>()
                .map(|s| s.to_string())
                .or_else(|| e.downcast_ref::<std::string::String>().cloned())
                .unwrap_or_else(|| format!("{e:?}"));
            error!("{name} panicked: {msg} {}", Backtrace::capture());
            None
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize)]
pub struct FunId(i64);

impl FromLua for FunId {
    fn from_lua(value: Value, _lua: &Lua) -> LuaResult<Self> {
        match value {
            Value::Integer(i) => Ok(FunId(i)),
            _ => Err(cvt_err("FunId")),
        }
    }
}

impl IntoLua for FunId {
    fn into_lua(self, _lua: &Lua) -> LuaResult<Value> {
        Ok(Value::Integer(self.0))
    }
}

wrapped_table!(Timer, None);

impl Timer {
    pub fn singleton(lua: MizLua) -> Result<Self> {
        Ok(lua.inner().globals().raw_get("timer")?)
    }

    pub fn get_time(&self) -> Result<Time> {
        Ok(record_perf!(timer_get_time, self.t.call_function("getTime", ())?))
    }

    pub fn get_abs_time(&self) -> Result<Time> {
        Ok(record_perf!(timer_get_abs_time, self.t.call_function("getAbsTime", ())?))
    }

    pub fn get_time0(&self) -> Result<Time> {
        Ok(record_perf!(timer_get_time0, self.t.call_function("getTime0", ())?))
    }

    /// Schedule `f` to run at `when`. It runs again at the time it returns,
    /// or not at all if it returns `None`.
    ///
    /// If `f` fails or panics it is rescheduled `RETRY_AFTER` seconds later.
    /// Returning nothing to DCS would unschedule it, and a repeating timer
    /// would then stop for good with only one log line to show for it. A
    /// callback that must not run twice should use `schedule_once`.
    pub fn schedule_function<T, F>(&self, when: Time, arg: T, f: F) -> Result<FunId>
    where
        F: Fn(MizLua, T, Time) -> Result<Option<Time>> + 'static,
        T: IntoLua + FromLua,
    {
        let f = self.lua.create_function(move |lua, (arg, time): (T, Time)| {
            Ok(run_scheduled("scheduled function", || f(MizLua(lua), arg, time))
                .unwrap_or_else(|| {
                    error!(
                        "scheduled function rescheduled in {RETRY_AFTER}s after failing"
                    );
                    Some(time + RETRY_AFTER)
                }))
        })?;
        Ok(record_perf!(
            timer_schedule_function,
            self.t.call_function("scheduleFunction", (f, arg, when))?
        ))
    }

    /// Schedule `f` to run once at `when`. If it fails or panics the failure
    /// is logged and it is not run again, for one shot work that may have
    /// done part of its job before it failed.
    pub fn schedule_once<T, F>(&self, when: Time, arg: T, f: F) -> Result<FunId>
    where
        F: Fn(MizLua, T, Time) -> Result<()> + 'static,
        T: IntoLua + FromLua,
    {
        let f = self.lua.create_function(move |lua, (arg, time): (T, Time)| {
            run_scheduled("scheduled one shot function", || f(MizLua(lua), arg, time));
            Ok(None::<Time>)
        })?;
        Ok(record_perf!(
            timer_schedule_function,
            self.t.call_function("scheduleFunction", (f, arg, when))?
        ))
    }

    pub fn remove_function(&self, id: FunId) -> Result<()> {
        let _: () = record_perf!(
            timer_remove_function,
            self.t.call_function("removeFunction", id)?
        );
        Ok(())
    }

    pub fn set_function_time(&self, id: FunId, when: f64) -> Result<()> {
        Ok(self.t.call_function("setFunctionTime", (id, when))?)
    }
}
