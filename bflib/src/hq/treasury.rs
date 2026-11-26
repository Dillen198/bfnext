/*
Copyright 2024 Eric Stokes.

This file is part of bflib.

bflib is free software: you can redistribute it and/or modify it under
the terms of the GNU Affero Public License as published by the Free
Software Foundation, either version 3 of the License, or (at your
option) any later version.

bflib is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero Public License
for more details.
*/

//! Spending a side's treasury: the one path the HQ's planner, its rules
//! mode and a human commander's orders all pay through. (Income -- treasury
//! pay, objective funding, holding bonuses -- is `crate::economy`.)

use crate::db::Db;
use compact_str::{CompactString, format_compact};
use dcso3::coalition::Side;

/// A raw config price at the HQ's `cost_scale`.
pub(crate) fn scaled(raw: i64, scale: f64) -> i64 {
    ((raw as f64) * scale.max(0.)).round() as i64
}

/// Can `side`'s treasury pay `cost`?
pub(crate) fn afford(db: &Db, side: Side, cost: i64) -> Result<(), CompactString> {
    let t = db.persisted.treasury(side);
    if cost > t {
        return Err(format_compact!("that costs {cost} and the treasury has {t}"));
    }
    Ok(())
}

/// Why `side` can't pay `cost`, or empty when it can.
pub(crate) fn shortfall(db: &Db, side: Side, cost: i64) -> String {
    let t = db.persisted.treasury(side);
    if t < cost { format!("the treasury has {t} of {cost}") } else { String::new() }
}

/// Take `cost` from `side`'s treasury. Returns the balance after.
pub(crate) fn pay(db: &mut Db, side: Side, cost: i64) -> i64 {
    let bal = db.persisted.adjust_treasury(side, -cost);
    db.ephemeral.dirty();
    bal
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn prices_and_payment() {
        assert_eq!(scaled(100, 1.5), 150);
        assert_eq!(scaled(100, -2.), 0);
        assert_eq!(scaled(5, 0.5), 3);
        let mut db = Db::default();
        db.persisted.blue_treasury = 100;
        let t = db.persisted.treasury(Side::Blue);
        assert!(afford(&db, Side::Blue, t).is_ok());
        assert!(afford(&db, Side::Blue, t + 1).is_err());
        assert!(shortfall(&db, Side::Blue, t).is_empty());
        assert_eq!(pay(&mut db, Side::Blue, 10), db.persisted.treasury(Side::Blue));
        assert_eq!(db.persisted.treasury(Side::Blue), t - 10);
        // never below zero
        assert_eq!(pay(&mut db, Side::Blue, 1000), 0);
    }
}
