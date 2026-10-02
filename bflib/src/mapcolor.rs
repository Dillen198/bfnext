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

//! The one place that decides what a coalition looks like on the F10 map.
//!
//! Every overlay in the engine used to pick its own `Color::red` /
//! `Color::blue`, which meant the palette drifted between objective labels,
//! convoy routes, the frontline and the threat marks. It also meant a
//! legibility problem had to be fixed in half a dozen files.
//!
//! Pure red (`1, 0, 0`) has poor luminance contrast against the tan terrain
//! that dominates Syria, Sinai and the Persian Gulf -- players reported red
//! objective labels as unreadable there. Pure blue (`0, 0, 1`) has the same
//! problem over water and night-dark terrain. Both are replaced by brighter,
//! more saturated stand-ins that keep the coalition reading.
//!
//! Shifting the enemy colour off red also frees red / amber / green to mean
//! "bad / marginal / good" in status glyphs without colliding with a side.

use dcso3::{Color, coalition::Side};

/// The colour of `side`'s own overlays, at alpha `a`.
pub fn side_color(side: Side, a: f32) -> Color {
    match side {
        Side::Red => Color::violet(a),
        Side::Blue => Color::azure(a),
        Side::Neutral => Color::white(a),
    }
}

/// JTAC laser overlays (bearing line, laser code). Deliberately NOT a side
/// colour: a JTAC target sits on the enemy's ground, and drawing it in the
/// lasing side's colour made players read the site underneath as that side's.
pub fn laser(a: f32) -> Color {
    Color::new(1., 0.9, 0.2, a)
}

/// Background plate for map text. Without it the glyph colour is doing all the
/// work and legibility depends entirely on what terrain happens to be behind
/// the label; with it, any side colour reads on any map.
pub fn text_plate() -> Color {
    Color::black(0.5)
}
