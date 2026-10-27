// Copyright (c) 2026 Dillen Weerasinghe. All rights reserved.
// Proprietary and confidential. No license is granted to use, copy, modify, or
// distribute this file. See NOTICE section 3 and the repository NOTICE file.
//! The range's icon pack, one glyph per discipline.
//!
//! The drawings live once, in the shared web icon set
//! (`shared/icons/index.tsx`, "Training range" group): the range site draws
//! them from there as thin outlines. Discord only takes raster emoji, so the
//! bot's copies (`DCSServerBot/plugins/fowlengine/assets/icons/vs_r_*.png`)
//! are rendered here from the very same paths, bolder, rounded and with the
//! accent colours the paths mark (`data-a="red"` ...), to sit with the rest of
//! the bot's emoji set:
//!
//! ```text
//! WRITE_RANGE_ICONS=1 cargo test -p bfdb write_range_icons
//! ```
//!
//! then `/feops icons_install` uploads the new ones. Results name their icon
//! by key (`kind_icon`); the embed carries it as `{icon:key}`, which the bot
//! turns into the emoji (or its unicode stand-in before the install).

use bfprotocols::range::RangeResult;

#[cfg(test)]
const SOURCE: &str = include_str!("../../../shared/icons/index.tsx");

/// Shared-set glyph -> the bot's icon key (emoji `vs_<key>`).
#[cfg(test)]
pub(crate) const RANGE_ICONS: [(&str, &str); 15] = [
    ("Trap", "r_trap"),
    ("Bombing", "r_bomb"),
    ("Strafe", "r_strafe"),
    ("Refuel", "r_aar"),
    ("MissileDefence", "r_missile"),
    ("Bfm", "r_bfm"),
    ("AntiShip", "r_antiship"),
    ("SlingLoad", "r_sling"),
    ("Helipad", "r_landing"),
    ("TroopDrop", "r_troops"),
    ("Gunnery", "r_gunnery"),
    ("HotZone", "r_hotzone"),
    ("LowLevel", "r_lowlevel"),
    ("FieldLanding", "r_field"),
    ("Airdrop", "r_airdrop"),
];

/// The bot icon key for a result's discipline. CAS, SEAD and CSAR use the
/// campaign's own (the same glyphs the dashboard and the briefing use).
pub(crate) fn kind_icon(r: &RangeResult) -> &'static str {
    match r {
        RangeResult::Trap(_) => "r_trap",
        RangeResult::Bomb(_) => "r_bomb",
        RangeResult::Strafe(_) => "r_strafe",
        RangeResult::Aar(_) => "r_aar",
        RangeResult::Missile(_) => "r_missile",
        RangeResult::Engagement(_) => "r_bfm",
        RangeResult::AntiShip(_) => "r_antiship",
        RangeResult::Sling(_) => "r_sling",
        RangeResult::Landing(_) => "r_landing",
        RangeResult::Troops(_) => "r_troops",
        RangeResult::Gunnery(_) => "r_gunnery",
        RangeResult::Cas(_) => "cas",
        RangeResult::Sead(_) => "sead",
        RangeResult::HotZone(_) => "r_hotzone",
        RangeResult::LowLevel(_) => "r_lowlevel",
        RangeResult::FieldLanding(_) => "r_field",
        RangeResult::Csar(_) => "csar",
        RangeResult::Airdrop(_) => "r_airdrop",
    }
}

/// The paths of one glyph in the shared set, as written there.
#[cfg(test)]
fn body(name: &str) -> Option<&'static str> {
    let head = format!("export const {name} = createIcon('{name}', <>");
    let i = SOURCE.find(&head)? + head.len();
    let j = SOURCE[i..].find("</>)")? + i;
    Some(SOURCE[i..j].trim())
}

/// The bot's palette (assets/icons/render_icons.py), for the accents.
#[cfg(test)]
fn accent(a: &str) -> &'static str {
    match a {
        "red" => "#c8102e",
        "amber" => "#e89b1e",
        "green" => "#34a868",
        "blue" => "#3884de",
        _ => "#96a2b0",
    }
}

/// A glyph as an emoji-sized SVG: the bot set's steel, its stroke weight and
/// round ends, the marked parts in their accent colour.
#[cfg(test)]
pub(crate) fn emoji_svg(name: &str, px: u32) -> Option<String> {
    let mut b = body(name)?.replace(" />", "/>");
    while let Some(i) = b.find("data-a=\"") {
        let j = b[i + 8..].find('"')? + i + 8;
        let col = accent(&b[i + 8..j]);
        b.replace_range(i..=j, &format!("color=\"{col}\""));
    }
    Some(format!(
        r##"<svg xmlns="http://www.w3.org/2000/svg" width="{px}" height="{px}" viewBox="0 0 24 24" fill="none" stroke="currentColor" color="#96a2b0" stroke-width="2" stroke-linecap="round" stroke-linejoin="round">{b}</svg>"##
    ))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn every_range_icon_is_in_the_shared_set_and_renders() {
        for (name, key) in RANGE_ICONS {
            let svg = emoji_svg(name, 128)
                .unwrap_or_else(|| panic!("{name} is not in shared/icons/index.tsx"));
            assert!(!svg.contains("data-a"), "{key}");
            let png = super::super::cards::png(&svg).unwrap();
            assert!(png.len() > 400, "{key} rendered blank");
        }
    }

    /// `WRITE_RANGE_ICONS=1 cargo test -p bfdb write_range_icons` regenerates the bot's PNGs.
    #[test]
    fn write_range_icons() {
        if std::env::var("WRITE_RANGE_ICONS").is_err() {
            return;
        }
        let dir = concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/../DCSServerBot/plugins/fowlengine/assets/icons"
        );
        for (name, key) in RANGE_ICONS {
            let png = super::super::cards::png(&emoji_svg(name, 128).unwrap()).unwrap();
            std::fs::write(format!("{dir}/vs_{key}.png"), png).unwrap();
        }
    }
}
