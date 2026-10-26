/*
Copyright 2024 Eric Stokes.

This file is part of dcso3.

dcso3 is free software: you can redistribute it and/or modify it under
the terms of the MIT License.

dcso3 is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.
*/

//! The perf types live in dcso3-core (bfdb reads them without Lua); this
//! module re-exports them and adds the `record_perf!` macro.

pub use dcso3_core::perf::*;

#[cfg(feature = "perf")]
#[macro_export]
macro_rules! record_perf {
    ($key:ident, $e:expr) => {{
        use crate::perf::Perf;
        use std::sync::Arc;
        // Evaluate first, borrow the global afterwards. `$e` is a Lua call
        // that can re-enter Rust (event handlers, timers) and hit another
        // record_perf!, so holding the &mut from Perf::get_mut across it
        // created two live aliases of the same static -- UB.
        let ts = chrono::Utc::now();
        let res = $e;
        {
            // SAFETY: the reference ends at the end of this block and
            // nothing in it can re-enter.
            let t = unsafe { Perf::get_mut() };
            let t = Arc::make_mut(&mut t.0); // takes about 20ns if we don't need to clone
            crate::perf::record_perf(&mut t.$key, ts);
        }
        res
    }};
}

#[cfg(not(feature = "perf"))]
#[macro_export]
macro_rules! record_perf {
    ($key:ident, $e:expr) => {
        $e
    };
}

