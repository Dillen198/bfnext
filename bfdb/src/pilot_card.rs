//! Shareable pilot cards: a small image with a pilot's name, squadron,
//! level and headline numbers, for Discord (`/card`) and the pilot page.
//! Drawn as SVG and rasterised by the range site's renderer
//! (`range::cards::png`), so it needs no new dependency.

use crate::db::Aggregates;

fn esc(s: &str) -> String {
    let mut o = String::with_capacity(s.len());
    for c in s.chars() {
        match c {
            '&' => o.push_str("&amp;"),
            '<' => o.push_str("&lt;"),
            '>' => o.push_str("&gt;"),
            '"' => o.push_str("&quot;"),
            '\'' => o.push_str("&apos;"),
            c if (c as u32) < 0x20 => (),
            c => o.push(c),
        }
    }
    o
}

const BG: &str = "#0d1009";
const BORDER_LIGHT: &str = "#262e1d";
const ACCENT: &str = "#6aab1f";
const ACCENT_BRIGHT: &str = "#8ec83f";
const TEXT: &str = "#cdd4be";
const TEXT_DIM: &str = "#6b7858";

/// 6950 -> "6,950".
fn thousands(n: u64) -> String {
    let s = n.to_string();
    let mut o = String::with_capacity(s.len() + s.len() / 3);
    for (i, ch) in s.chars().enumerate() {
        if i > 0 && (s.len() - i).is_multiple_of(3) {
            o.push(',');
        }
        o.push(ch);
    }
    o
}

pub(crate) struct CardInput<'a> {
    pub(crate) name: &'a str,
    pub(crate) squadron: Option<&'a str>,
    pub(crate) level: u64,
    pub(crate) xp: u64,
    pub(crate) total: &'a Aggregates,
    /// Achievements at tier 1 or better.
    pub(crate) badges: usize,
}

pub(crate) fn svg(c: &CardInput) -> String {
    let t = c.total;
    let kills = t.air_kills + t.ground_kills;
    let kd = if t.deaths > 0 {
        format!("{:.2}", kills as f64 / t.deaths as f64)
    } else if kills > 0 {
        "∞".to_string()
    } else {
        "0.00".to_string()
    };
    let logistics = t.repairs + t.supply_transfers + t.troops + t.farps + t.deploys;
    let name = esc(c.name);
    let squadron = c
        .squadron
        .map(|s| format!(r##"<tspan fill="{ACCENT_BRIGHT}">[{}]</tspan> "##, esc(s)))
        .unwrap_or_default();
    // Flight time the way the dashboard writes it ("2h 15m"), not bare hours.
    let mins = (t.hours as f64 * 60.0).round() as u64;
    let flight_time = format!("{}h {:02}m", mins / 60, mins % 60);
    let stat = |x: u32, label: &str, v: &str| {
        format!(
            r##"<text x="{x}" y="164" font-size="26" font-weight="700" fill="{TEXT}">{v}</text><text x="{x}" y="186" font-size="11" letter-spacing="1.5" fill="{TEXT_DIM}">{label}</text>"##
        )
    };
    let mut stats = String::new();
    for (i, (label, v)) in [
        ("AIR KILLS", t.air_kills.to_string()),
        ("GROUND KILLS", t.ground_kills.to_string()),
        ("CAPTURES", t.captures.to_string()),
        ("LOGISTICS", logistics.to_string()),
        ("K/D", kd),
        ("FLIGHT TIME", flight_time),
    ]
    .iter()
    .enumerate()
    {
        stats.push_str(&stat(32 + i as u32 * 126, label, v));
    }
    let badges = match c.badges {
        1 => "1 ACHIEVEMENT".to_string(),
        n => format!("{n} ACHIEVEMENTS"),
    };
    // The dashboard's dark theme (bfweb/src/index.css): olive accent on a
    // near-black field, cream text, khaki labels, square-cornered.
    format!(
        r##"<svg xmlns="http://www.w3.org/2000/svg" width="800" height="220" viewBox="0 0 800 220">
<rect x="0.5" y="0.5" width="799" height="219" rx="3" fill="{BG}" stroke="{BORDER_LIGHT}"/>
<rect x="0" y="0" width="4" height="220" fill="{ACCENT}"/>
<rect x="4" y="0" width="796" height="2" fill="{ACCENT}" opacity="0.35"/>
<text x="772" y="36" font-size="12" letter-spacing="3" fill="{TEXT_DIM}" text-anchor="end">VECTOR STRIKE</text>
<text x="32" y="58" font-size="34" font-weight="700" fill="{TEXT}">{squadron}{name}</text>
<text x="32" y="88" font-size="13" letter-spacing="1.5" fill="{ACCENT_BRIGHT}">LEVEL {level} · {xp} XP · {badges}</text>
<rect x="32" y="114" width="740" height="1" fill="{BORDER_LIGHT}"/>
{stats}
</svg>"##,
        level = c.level,
        xp = thousands(c.xp),
    )
}
