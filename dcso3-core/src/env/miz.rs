/*
Copyright 2024 Eric Stokes.

This file is part of dcso3.

dcso3 is free software: you can redistribute it and/or modify it under
the terms of the MIT License.

dcso3 is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.
*/

use crate::wrapped_prim;

wrapped_prim!(UnitId, i64, Hash, Copy);

impl Default for UnitId {
    fn default() -> Self {
        UnitId(0)
    }
}

impl UnitId {
    pub fn next(&mut self) {
        self.0 += 1
    }
}
