/*
Copyright 2024 Eric Stokes.

This file is part of dcso3.

dcso3 is free software: you can redistribute it and/or modify it under
the terms of the MIT License.

dcso3 is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.
*/

use crate::simple_enum_unknown;

simple_enum_unknown!(LiquidType, u8, [
    JetFuel => 0,
    Avgas => 1,
    MW50 => 2,
    Diesel => 3
]);

impl LiquidType {
    pub const ALL: [LiquidType; 4] = [Self::Avgas, Self::Diesel, Self::JetFuel, Self::MW50];
}
