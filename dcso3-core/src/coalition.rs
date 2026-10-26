/*
Copyright 2024 Eric Stokes.

This file is part of dcso3.

dcso3 is free software: you can redistribute it and/or modify it under
the terms of the MIT License.

dcso3 is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.
*/

use crate::simple_enum;
use anyhow::{bail, Result};
use std::{fmt, str::FromStr};

simple_enum!(Side, u8, [Neutral => 0, Red => 1, Blue => 2]);

impl Default for Side {
    fn default() -> Self {
        Side::Red
    }
}

impl fmt::Display for Side {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.to_str())
    }
}

impl FromStr for Side {
    type Err = anyhow::Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(match s {
            "blue" => Side::Blue,
            "red" => Side::Red,
            "neutrals" => Side::Neutral,
            s => bail!("unknown side {s}"),
        })
    }
}

impl Side {
    pub const ALL: [Side; 3] = [Side::Red, Side::Blue, Side::Neutral];

    pub fn to_str(&self) -> &'static str {
        match self {
            Side::Blue => "blue",
            Side::Red => "red",
            Side::Neutral => "neutrals",
        }
    }

    pub fn opposite(&self) -> Side {
        match self {
            Self::Blue => Self::Red,
            Self::Red => Self::Blue,
            Self::Neutral => Self::Neutral,
        }
    }
}
