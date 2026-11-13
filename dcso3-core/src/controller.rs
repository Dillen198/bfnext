/*
Copyright 2024 Eric Stokes.

This file is part of dcso3.

dcso3 is free software: you can redistribute it and/or modify it under
the terms of the MIT License.

dcso3 is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.
*/

use crate::string_enum;

string_enum!(AltType, u8, [
    BARO => "BARO",
    RADIO => "RADIO"
]);

// TACAN channel band, as selected on the aircraft's TACAN control panel.
string_enum!(TacanBand, u8, [
    X => "X",
    Y => "Y"
]);
