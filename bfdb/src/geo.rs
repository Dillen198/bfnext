//! Where on Earth an objective is.
//!
//! The war diary used to call the belligerents "Blue" and "Red", which is the
//! language of a briefing slide rather than of war reporting. Real reporting
//! names ground by the country it is in: fighting *inside Syria*, a push
//! towards the *Jordanian* border, strikes on the *Turkish* frontier. The
//! campaign itself has no idea about any of that -- DCS gives an objective a
//! coalition and a position, and the mission file's own `country` assignments
//! are usually generic placeholders (CJTF Blue, CJTF Red), so they cannot be
//! used either.
//!
//! What we do have is the position, and every DCS terrain is a real place. So
//! this module answers two questions from a latitude/longitude alone:
//!
//!   * which DCS theatre is this? (bounding boxes, decided across all of a
//!     round's objectives at once, because a few terrains overlap); and
//!   * which country is this point in?
//!
//! Nothing has to be sent from the engine and nothing new has to be stored:
//! bfdb already records every objective's lat/lon, so this works on rounds
//! that were recorded long before the module existed.
//!
//! **On the borders.** These polygons are deliberately coarse -- a few dozen
//! vertices each, enough to put a point on the correct side of an
//! international border and no more. They are for naming a sector in a news
//! bulletin, not for adjudicating anything. Where a border is disputed the
//! polygons follow the de-facto administration the DCS terrain itself draws,
//! because that is what a player looking at the F10 map sees.
//!
//! Points that fall in no polygon -- open sea, most obviously, which is where
//! carrier groups live -- come back as `None`, and the diary simply does not
//! attach a country to them. That is the honest answer, and better than
//! rounding a carrier into whichever coast happens to be nearest.

/// A country as the diary refers to it: the noun and the adjectival form.
#[derive(Debug, Clone, Copy)]
pub struct Country {
    /// "Syria". What the diary calls the territory.
    pub name: &'static str,
    /// "Syrian". What the diary calls things belonging to it.
    pub adj: &'static str,
    /// Coarse outline(s), as (latitude, longitude) rings. Several rings for a
    /// country that is not contiguous inside the theatre -- Oman's Musandam
    /// exclave, an island group.
    pub rings: &'static [&'static [(f64, f64)]],
}

/// One DCS terrain, and the countries that make it up.
#[derive(Debug, Clone, Copy)]
pub struct Theatre {
    /// The terrain's name as DCS spells it.
    pub name: &'static str,
    /// (min, max) latitude and longitude. Generous: it only has to separate
    /// this terrain from the others, and no two are anywhere near each other.
    pub lat: (f64, f64),
    pub lon: (f64, f64),
    /// Checked in order, first hit wins. Smaller territories and enclaves
    /// therefore come before the larger neighbour that surrounds them.
    pub countries: &'static [Country],
}

impl Theatre {
    fn holds(&self, lat: f64, lon: f64) -> bool {
        lat >= self.lat.0 && lat <= self.lat.1 && lon >= self.lon.0 && lon <= self.lon.1
    }

    /// The country containing this point, or `None` at sea or outside every
    /// outline the theatre knows about.
    pub fn country_at(&self, lat: f64, lon: f64) -> Option<&'static Country> {
        self.countries.iter().find(|c| c.contains(lat, lon))
    }

}

impl Country {
    fn contains(&self, lat: f64, lon: f64) -> bool {
        self.rings.iter().any(|r| point_in_ring(lat, lon, r))
    }
}

/// Standard even-odd ray cast. The rings are small and this is called
/// per-objective once a day, so there is no point indexing them.
fn point_in_ring(lat: f64, lon: f64, ring: &[(f64, f64)]) -> bool {
    let n = ring.len();
    if n < 3 {
        return false;
    }
    let mut inside = false;
    let mut j = n - 1;
    for i in 0..n {
        let (yi, xi) = ring[i];
        let (yj, xj) = ring[j];
        if (yi > lat) != (yj > lat) {
            let t = (lat - yi) / (yj - yi);
            if lon < xi + t * (xj - xi) {
                inside = !inside;
            }
        }
        j = i;
    }
    inside
}

/// Which terrain a set of positions is on.
///
/// Every position votes for every terrain whose box contains it, and the
/// terrain with the most votes wins. A few terrains genuinely overlap -- Syria
/// and Sinai share southern Israel, and it is the same ground on both -- and
/// this settles that properly: a Sinai campaign has objectives all over the
/// Sinai box and only a few in the shared slice, so Sinai wins, and the
/// reverse for a Syria campaign. It also shrugs off one objective a long way
/// out to sea. Ties go to whichever terrain is declared first.
pub fn theatre_of<I: IntoIterator<Item = (f64, f64)>>(points: I) -> Option<&'static Theatre> {
    let mut tally = vec![0u32; THEATRES.len()];
    for (lat, lon) in points {
        for (i, t) in THEATRES.iter().enumerate() {
            if t.holds(lat, lon) {
                tally[i] += 1;
            }
        }
    }
    let mut best: Option<(usize, u32)> = None;
    for (i, n) in tally.into_iter().enumerate() {
        if n > 0 && best.map(|(_, b)| n > b).unwrap_or(true) {
            best = Some((i, n));
        }
    }
    best.map(|(i, _)| &THEATRES[i])
}

/// "Israel, Turkey and Jordan" -- the form a wire report uses for a list of
/// belligerents, with the serial comma left off the way British copy does.
pub fn join_names(names: &[String]) -> String {
    match names {
        [] => String::new(),
        [a] => a.clone(),
        [a, b] => format!("{a} and {b}"),
        _ => {
            let (last, rest) = names.split_last().unwrap();
            format!("{} and {}", rest.join(", "), last)
        }
    }
}

// -- the terrains -------------------------------------------------------------
//
// Each ring only has to be right *inside* its theatre; where it runs off the
// edge of the map it is squared off, because nothing is ever asked about those
// points. What is drawn carefully is the international borders that actually
// cross playable ground.

macro_rules! country {
    ($name:literal, $adj:literal, $($ring:expr),+ $(,)?) => {
        Country { name: $name, adj: $adj, rings: &[$($ring),+] }
    };
}

// Syria ----------------------------------------------------------------------
const CYPRUS: &[(f64, f64)] = &[
    (35.05, 32.28),
    (35.40, 32.92),
    (35.35, 33.50),
    (35.69, 34.59),
    (35.42, 34.05),
    (35.05, 33.95),
    (34.88, 33.63),
    (34.56, 33.00),
    (34.70, 32.35),
];
/// North of the Syrian border: out to the Mediterranean at the Hatay coast,
/// then east along the line of the Baghdad railway to the Tigris.
const TURKEY_SYR: &[(f64, f64)] = &[
    (38.50, 30.50),
    (38.50, 43.50),
    (37.10, 42.35),
    (36.85, 40.20),
    (36.70, 39.00),
    (36.65, 38.20),
    (36.72, 37.10),
    (36.60, 36.60),
    (36.25, 36.15),
    (35.92, 35.90),
    (36.20, 35.50),
    (36.60, 34.00),
    (36.80, 30.50),
];
const LEBANON: &[(f64, f64)] = &[
    (34.63, 35.98),
    (34.70, 36.32),
    (34.45, 36.60),
    (34.20, 36.60),
    (33.80, 36.35),
    (33.45, 36.05),
    (33.28, 35.62),
    (33.09, 35.10),
    (33.60, 35.30),
    (34.10, 35.55),
    (34.45, 35.85),
];
/// Drawn on the de-facto line the DCS terrain shows, which puts the Golan on
/// the Israeli side. A map-reading convenience for naming a sector, not a
/// position on anything.
const ISRAEL: &[(f64, f64)] = &[
    (33.28, 35.62),
    (33.24, 35.78),
    (32.95, 35.92),
    (32.72, 35.88),
    (32.70, 35.57),
    (32.40, 35.55),
    (31.75, 35.55),
    (31.35, 35.47),
    (30.50, 35.15),
    (29.55, 34.97),
    (29.50, 34.90),
    (31.22, 34.27),
    (31.60, 34.50),
    (32.55, 34.88),
    (33.09, 35.10),
];
const JORDAN: &[(f64, f64)] = &[
    (32.72, 35.88),
    (32.70, 35.90),
    (32.40, 36.55),
    (32.32, 37.00),
    (32.16, 38.20),
    (33.37, 38.79),
    (32.15, 39.29),
    (31.00, 37.60),
    (29.35, 36.75),
    (29.18, 34.95),
    (30.50, 35.15),
    (31.35, 35.47),
    (31.75, 35.55),
    (32.40, 35.55),
    (32.70, 35.57),
];
const IRAQ_W: &[(f64, f64)] = &[
    (37.10, 42.35),
    (36.30, 41.30),
    (35.00, 41.20),
    (34.40, 40.90),
    (33.37, 38.79),
    (32.15, 39.29),
    (30.00, 42.00),
    (30.00, 45.50),
    (38.50, 45.50),
    (38.50, 43.50),
];
const SAUDI_NW: &[(f64, f64)] = &[
    (32.15, 39.29),
    (31.00, 37.60),
    (29.35, 36.75),
    (27.00, 36.00),
    (27.00, 45.50),
    (30.00, 45.50),
    (30.00, 42.00),
];
const EGYPT_SINAI: &[(f64, f64)] =
    &[(31.30, 32.20), (31.10, 34.20), (29.50, 34.90), (29.20, 32.55), (30.60, 32.30)];
const SYRIA: &[(f64, f64)] = &[
    (35.92, 35.90),
    (36.25, 36.15),
    (36.60, 36.60),
    (36.72, 37.10),
    (36.65, 38.20),
    (36.70, 39.00),
    (36.85, 40.20),
    (37.10, 42.35),
    (36.30, 41.30),
    (35.00, 41.20),
    (34.40, 40.90),
    (33.37, 38.79),
    (32.32, 37.00),
    (32.40, 36.55),
    (32.70, 35.90),
    (32.95, 35.92),
    (33.24, 35.78),
    (33.28, 35.62),
    (33.45, 36.05),
    (33.80, 36.35),
    (34.20, 36.60),
    (34.45, 36.60),
    (34.70, 36.32),
    (34.63, 35.98),
    (35.10, 35.90),
    (35.60, 35.80),
];

// Caucasus -------------------------------------------------------------------
const ABKHAZIA: &[(f64, f64)] = &[
    (43.40, 40.00),
    (43.60, 40.65),
    (43.30, 41.60),
    (42.50, 41.56),
    (42.60, 41.00),
    (43.00, 40.40),
];
const SOUTH_OSSETIA: &[(f64, f64)] = &[
    (42.75, 43.45),
    (42.70, 44.35),
    (42.35, 44.45),
    (42.10, 44.15),
    (42.10, 43.55),
    (42.40, 43.35),
];
/// North of the Greater Caucasus watershed, which is where the Georgian border
/// runs for its whole length.
const RUSSIA_CAU: &[(f64, f64)] = &[
    (48.00, 33.00),
    (48.00, 48.50),
    (41.60, 46.70),
    (41.90, 46.40),
    (42.55, 45.20),
    (42.70, 44.00),
    (42.95, 43.20),
    (43.40, 41.40),
    (43.55, 40.60),
    (43.38, 40.07),
    (44.90, 37.30),
    (45.30, 36.60),
    (46.00, 33.00),
];
const TURKEY_CAU: &[(f64, f64)] = &[
    (41.55, 41.53),
    (41.10, 42.55),
    (41.25, 43.45),
    (41.10, 43.65),
    (40.30, 44.40),
    (38.00, 44.80),
    (38.00, 26.00),
    (41.20, 26.00),
    (42.10, 35.00),
    (41.10, 38.40),
    (41.40, 41.10),
];
const ARMENIA: &[(f64, f64)] = &[
    (41.30, 43.45),
    (41.10, 44.85),
    (40.60, 45.45),
    (39.55, 46.60),
    (38.85, 46.50),
    (38.85, 43.60),
    (40.30, 43.40),
];
const AZERBAIJAN: &[(f64, f64)] = &[
    (41.90, 46.40),
    (41.60, 46.70),
    (41.20, 46.60),
    (40.60, 45.45),
    (39.55, 46.60),
    (38.40, 48.30),
    (38.40, 48.50),
    (42.20, 48.50),
    (41.90, 48.40),
];
const GEORGIA: &[(f64, f64)] = &[
    (43.38, 40.07),
    (43.55, 40.60),
    (43.40, 41.40),
    (42.95, 43.20),
    (42.70, 44.00),
    (42.55, 45.20),
    (41.90, 46.40),
    (41.20, 46.60),
    (41.10, 44.85),
    (41.30, 43.45),
    (41.10, 43.65),
    (41.10, 42.55),
    (41.55, 41.53),
    (42.10, 41.45),
    (42.60, 41.50),
    (43.00, 40.60),
];

// Persian Gulf ---------------------------------------------------------------
const IRAN_PG: &[(f64, f64)] = &[
    (34.00, 46.00),
    (34.00, 63.50),
    (25.00, 63.50),
    (25.30, 60.50),
    (25.60, 59.00),
    (26.30, 57.30),
    (27.15, 56.35),
    (26.55, 55.00),
    (27.20, 53.00),
    (28.80, 51.00),
    (29.90, 50.10),
    (31.00, 48.40),
    (33.00, 46.00),
];
const OMAN_MAIN: &[(f64, f64)] = &[
    (24.30, 56.40),
    (22.60, 59.85),
    (19.00, 57.80),
    (16.65, 53.10),
    (18.50, 52.00),
    (21.00, 55.00),
    (22.60, 55.25),
    (24.10, 55.80),
];
const OMAN_MUSANDAM: &[(f64, f64)] =
    &[(26.45, 56.20), (26.10, 56.55), (25.60, 56.40), (25.80, 56.10)];
const UAE: &[(f64, f64)] = &[
    (25.90, 56.05),
    (25.30, 56.40),
    (24.20, 55.80),
    (22.63, 55.20),
    (22.63, 51.60),
    (24.10, 51.60),
    (24.60, 52.60),
    (25.30, 55.00),
    (25.80, 55.90),
];
const QATAR: &[(f64, f64)] =
    &[(26.15, 51.05), (26.20, 51.60), (24.55, 51.60), (24.55, 50.80), (25.50, 50.75)];
const BAHRAIN: &[(f64, f64)] = &[(26.35, 50.42), (26.35, 50.70), (25.75, 50.70), (25.75, 50.42)];
const KUWAIT: &[(f64, f64)] =
    &[(30.10, 46.50), (30.10, 48.45), (28.50, 48.45), (28.55, 47.70), (29.10, 46.55)];
const IRAQ_SE: &[(f64, f64)] =
    &[(34.00, 44.00), (34.00, 48.40), (30.20, 48.40), (29.95, 47.70), (30.60, 44.00)];
const SAUDI_E: &[(f64, f64)] = &[
    (29.95, 47.70),
    (28.50, 48.45),
    (27.00, 49.60),
    (25.00, 50.30),
    (24.55, 50.80),
    (24.55, 51.60),
    (22.63, 51.60),
    (22.63, 55.20),
    (20.00, 55.50),
    (20.00, 44.00),
    (29.10, 46.55),
];

// Iraq -----------------------------------------------------------------------
/// The Zagros line: the Iran-Iraq border from the Turkish tripoint down to the
/// Shatt al-Arab.
const IRAN_W: &[(f64, f64)] = &[
    (38.00, 44.80),
    (38.00, 50.00),
    (27.00, 50.00),
    (30.00, 48.60),
    (31.00, 48.40),
    (32.50, 47.40),
    (33.60, 45.90),
    (35.00, 45.60),
    (36.50, 45.20),
    (37.20, 44.80),
];
const IRAQ: &[(f64, f64)] = &[
    (37.35, 42.35),
    (37.20, 44.80),
    (36.50, 45.20),
    (35.00, 45.60),
    (33.60, 45.90),
    (32.50, 47.40),
    (31.00, 48.40),
    (30.00, 48.00),
    (29.10, 46.55),
    (32.15, 39.29),
    (33.37, 38.79),
    (34.40, 40.90),
    (35.00, 41.20),
    (36.30, 41.30),
];
const TURKEY_IRQ: &[(f64, f64)] = &[
    (38.50, 38.00),
    (38.50, 45.00),
    (37.20, 44.80),
    (37.35, 42.35),
    (36.85, 40.20),
    (36.70, 39.00),
    (36.65, 38.20),
    (36.72, 38.00),
];

// Sinai ----------------------------------------------------------------------
const EGYPT: &[(f64, f64)] = &[
    (31.60, 25.00),
    (31.60, 32.30),
    (31.30, 32.35),
    (31.10, 34.20),
    (29.50, 34.90),
    (27.70, 33.60),
    (24.00, 35.50),
    (22.00, 35.50),
    (22.00, 25.00),
];
const SAUDI_HEJAZ: &[(f64, f64)] = &[
    (29.35, 36.75),
    (28.00, 36.00),
    (26.00, 36.20),
    (25.00, 38.00),
    (26.00, 40.00),
    (31.00, 40.00),
    (31.00, 37.60),
];

// Kola -----------------------------------------------------------------------
const NORWAY: &[(f64, f64)] = &[
    (71.50, 24.00),
    (71.20, 31.10),
    (69.80, 30.20),
    (69.20, 29.30),
    (68.60, 28.40),
    (69.05, 27.00),
    (68.40, 23.50),
    (67.40, 23.30),
    (65.00, 14.00),
    (63.00, 10.00),
    (62.00, 4.00),
    (71.50, 15.00),
];
const FINLAND: &[(f64, f64)] = &[
    (70.10, 27.90),
    (69.05, 27.00),
    (68.40, 23.50),
    (67.40, 23.30),
    (65.80, 24.15),
    (62.00, 21.50),
    (62.00, 31.50),
    (66.00, 30.00),
    (68.90, 28.50),
    (69.80, 29.30),
];
const SWEDEN: &[(f64, f64)] = &[
    (69.06, 20.55),
    (68.40, 23.50),
    (67.40, 23.30),
    (65.80, 24.15),
    (62.00, 21.50),
    (62.00, 12.00),
    (68.00, 18.00),
];
const RUSSIA_KOLA: &[(f64, f64)] = &[
    (73.00, 28.00),
    (73.00, 46.00),
    (62.00, 46.00),
    (62.00, 31.50),
    (66.00, 30.00),
    (68.90, 28.50),
    (69.80, 29.30),
    (69.20, 29.30),
    (69.80, 30.20),
    (71.20, 31.10),
];

// Normandy / the Channel -----------------------------------------------------
const UK_SOUTH: &[(f64, f64)] = &[
    (52.50, -7.00),
    (52.50, 2.00),
    (51.10, 1.40),
    (50.60, 0.80),
    (50.55, -1.20),
    (50.00, -5.70),
    (51.20, -5.50),
];
const FRANCE_N: &[(f64, f64)] = &[
    (51.10, 2.60),
    (50.60, 4.00),
    (46.50, 4.00),
    (46.50, -4.80),
    (48.70, -4.60),
    (49.30, -1.60),
    (49.50, 0.20),
    (50.10, 1.40),
    (50.95, 2.00),
];
const BELGIUM: &[(f64, f64)] = &[(51.50, 3.00), (51.50, 6.20), (49.50, 5.80), (49.80, 4.20), (51.00, 2.55)];

// Germany (Cold War) ---------------------------------------------------------
const EAST_GERMANY: &[(f64, f64)] = &[
    (54.70, 11.00),
    (54.40, 14.40),
    (51.00, 15.05),
    (50.20, 12.10),
    (50.90, 10.60),
    (51.65, 10.20),
    (52.90, 10.90),
    (53.90, 10.80),
];
const WEST_GERMANY: &[(f64, f64)] = &[
    (54.90, 8.40),
    (54.40, 11.00),
    (53.90, 10.80),
    (52.90, 10.90),
    (51.65, 10.20),
    (50.90, 10.60),
    (50.20, 12.10),
    (48.55, 13.85),
    (47.50, 13.00),
    (47.55, 7.60),
    (49.00, 6.00),
    (50.90, 5.90),
    (53.20, 7.20),
];
const CZECHOSLOVAKIA: &[(f64, f64)] = &[
    (51.00, 15.05),
    (50.70, 16.50),
    (49.60, 16.50),
    (47.75, 17.20),
    (48.55, 13.85),
    (50.20, 12.10),
];

// Afghanistan ----------------------------------------------------------------
const AFGHANISTAN: &[(f64, f64)] = &[
    (38.49, 70.88),
    (37.33, 74.90),
    (36.70, 74.55),
    (36.90, 71.60),
    (35.50, 71.20),
    (34.10, 71.10),
    (33.00, 69.90),
    (31.30, 69.30),
    (30.00, 66.40),
    (29.40, 64.10),
    (29.85, 61.80),
    (31.40, 60.85),
    (33.50, 60.50),
    (35.30, 61.20),
    (36.65, 64.60),
    (37.25, 66.50),
    (37.20, 68.20),
    (37.50, 70.80),
];
const PAKISTAN: &[(f64, f64)] = &[
    (35.50, 71.20),
    (34.10, 71.10),
    (33.00, 69.90),
    (31.30, 69.30),
    (30.00, 66.40),
    (29.40, 64.10),
    (27.00, 62.00),
    (27.00, 71.00),
    (29.00, 70.50),
    (32.00, 74.50),
    (35.00, 76.00),
    (36.70, 74.55),
    (36.90, 71.60),
];
const IRAN_E: &[(f64, f64)] = &[
    (38.00, 58.00),
    (37.60, 61.20),
    (35.30, 61.20),
    (33.50, 60.50),
    (31.40, 60.85),
    (29.85, 61.80),
    (29.40, 64.10),
    (27.00, 62.00),
    (27.00, 58.00),
];
const TAJIKISTAN: &[(f64, f64)] = &[
    (40.00, 68.00),
    (39.30, 73.60),
    (37.20, 74.90),
    (38.49, 70.88),
    (37.50, 70.80),
    (37.20, 68.20),
    (39.20, 67.40),
];
const UZBEKISTAN: &[(f64, f64)] = &[
    (40.00, 58.00),
    (40.00, 68.00),
    (39.20, 67.40),
    (37.20, 68.20),
    (37.25, 66.50),
    (39.00, 61.50),
];
const TURKMENISTAN: &[(f64, f64)] = &[
    (40.00, 58.00),
    (39.00, 61.50),
    (37.25, 66.50),
    (36.65, 64.60),
    (35.30, 61.20),
    (37.60, 61.20),
    (38.00, 58.00),
];

// South Atlantic -------------------------------------------------------------
const FALKLANDS: &[(f64, f64)] =
    &[(-50.90, -61.40), (-50.90, -57.70), (-52.60, -57.70), (-52.60, -61.40)];
const ARGENTINA: &[(f64, f64)] = &[
    (-45.00, -73.60),
    (-45.00, -62.00),
    (-55.00, -62.00),
    (-55.00, -66.50),
    (-54.90, -68.60),
    (-52.50, -69.50),
    (-50.00, -72.00),
    (-47.00, -71.80),
];
const CHILE: &[(f64, f64)] = &[
    (-45.00, -75.80),
    (-45.00, -73.60),
    (-47.00, -71.80),
    (-50.00, -72.00),
    (-52.50, -69.50),
    (-54.90, -68.60),
    (-57.00, -67.30),
    (-57.00, -75.80),
];

// Marianas -------------------------------------------------------------------
const GUAM: &[(f64, f64)] = &[(13.70, 144.60), (13.66, 145.02), (13.22, 144.95), (13.23, 144.61)];
const SAIPAN: &[(f64, f64)] = &[(15.32, 145.68), (15.32, 145.85), (14.94, 145.85), (14.94, 145.68)];
const TINIAN: &[(f64, f64)] = &[(15.10, 145.56), (15.10, 145.72), (14.88, 145.72), (14.88, 145.56)];
const ROTA: &[(f64, f64)] = &[(14.22, 145.08), (14.22, 145.32), (14.08, 145.32), (14.08, 145.08)];

// Nevada ---------------------------------------------------------------------
const NEVADA_US: &[(f64, f64)] =
    &[(39.50, -119.50), (39.50, -111.50), (33.50, -111.50), (33.50, -119.50)];

pub static THEATRES: &[Theatre] = &[
    Theatre {
        name: "Syria",
        lat: (31.0, 37.9),
        lon: (32.0, 43.5),
        countries: &[
            country!("Cyprus", "Cypriot", CYPRUS),
            country!("Turkey", "Turkish", TURKEY_SYR),
            country!("Lebanon", "Lebanese", LEBANON),
            country!("Israel", "Israeli", ISRAEL),
            country!("Jordan", "Jordanian", JORDAN),
            country!("Iraq", "Iraqi", IRAQ_W),
            country!("Saudi Arabia", "Saudi", SAUDI_NW),
            country!("Egypt", "Egyptian", EGYPT_SINAI),
            country!("Syria", "Syrian", SYRIA),
        ],
    },
    Theatre {
        name: "Caucasus",
        lat: (38.0, 48.0),
        lon: (26.0, 48.5),
        countries: &[
            country!("Abkhazia", "Abkhaz", ABKHAZIA),
            country!("South Ossetia", "South Ossetian", SOUTH_OSSETIA),
            country!("Russia", "Russian", RUSSIA_CAU),
            country!("Turkey", "Turkish", TURKEY_CAU),
            country!("Armenia", "Armenian", ARMENIA),
            country!("Azerbaijan", "Azerbaijani", AZERBAIJAN),
            country!("Georgia", "Georgian", GEORGIA),
        ],
    },
    Theatre {
        name: "Persian Gulf",
        lat: (16.0, 34.0),
        lon: (44.0, 63.5),
        countries: &[
            country!("Bahrain", "Bahraini", BAHRAIN),
            country!("Qatar", "Qatari", QATAR),
            country!("Kuwait", "Kuwaiti", KUWAIT),
            country!("United Arab Emirates", "Emirati", UAE),
            country!("Oman", "Omani", OMAN_MUSANDAM, OMAN_MAIN),
            country!("Iraq", "Iraqi", IRAQ_SE),
            country!("Iran", "Iranian", IRAN_PG),
            country!("Saudi Arabia", "Saudi", SAUDI_E),
        ],
    },
    Theatre {
        name: "Iraq",
        lat: (27.0, 37.9),
        lon: (38.0, 50.0),
        countries: &[
            country!("Kuwait", "Kuwaiti", KUWAIT),
            country!("Turkey", "Turkish", TURKEY_IRQ),
            country!("Syria", "Syrian", SYRIA),
            country!("Jordan", "Jordanian", JORDAN),
            country!("Iran", "Iranian", IRAN_W),
            country!("Saudi Arabia", "Saudi", SAUDI_NW, SAUDI_E),
            country!("Iraq", "Iraqi", IRAQ),
        ],
    },
    Theatre {
        name: "Sinai",
        lat: (22.0, 33.4),
        lon: (25.0, 37.2),
        countries: &[
            country!("Israel", "Israeli", ISRAEL),
            country!("Jordan", "Jordanian", JORDAN),
            country!("Saudi Arabia", "Saudi", SAUDI_HEJAZ),
            country!("Egypt", "Egyptian", EGYPT),
        ],
    },
    Theatre {
        name: "Afghanistan",
        lat: (27.0, 40.0),
        lon: (58.0, 76.0),
        countries: &[
            country!("Tajikistan", "Tajik", TAJIKISTAN),
            country!("Uzbekistan", "Uzbek", UZBEKISTAN),
            country!("Turkmenistan", "Turkmen", TURKMENISTAN),
            country!("Pakistan", "Pakistani", PAKISTAN),
            country!("Iran", "Iranian", IRAN_E),
            country!("Afghanistan", "Afghan", AFGHANISTAN),
        ],
    },
    Theatre {
        name: "Kola",
        lat: (62.0, 73.0),
        lon: (4.0, 46.0),
        countries: &[
            country!("Norway", "Norwegian", NORWAY),
            country!("Sweden", "Swedish", SWEDEN),
            country!("Finland", "Finnish", FINLAND),
            country!("Russia", "Russian", RUSSIA_KOLA),
        ],
    },
    Theatre {
        name: "Germany",
        lat: (46.0, 55.5),
        lon: (4.5, 17.5),
        countries: &[
            country!("East Germany", "East German", EAST_GERMANY),
            country!("Czechoslovakia", "Czechoslovak", CZECHOSLOVAKIA),
            country!("West Germany", "West German", WEST_GERMANY),
        ],
    },
    Theatre {
        name: "Normandy",
        lat: (46.0, 53.0),
        lon: (-8.0, 4.4),
        countries: &[
            country!("Belgium", "Belgian", BELGIUM),
            country!("United Kingdom", "British", UK_SOUTH),
            country!("France", "French", FRANCE_N),
        ],
    },
    Theatre {
        name: "South Atlantic",
        lat: (-58.0, -44.0),
        lon: (-78.0, -50.0),
        countries: &[
            country!("the Falkland Islands", "Falklands", FALKLANDS),
            country!("Chile", "Chilean", CHILE),
            country!("Argentina", "Argentine", ARGENTINA),
        ],
    },
    Theatre {
        name: "Marianas",
        lat: (11.0, 21.5),
        lon: (142.0, 148.0),
        countries: &[
            country!("Guam", "Guamanian", GUAM),
            country!("the Northern Marianas", "Marianas", SAIPAN, TINIAN, ROTA),
        ],
    },
    Theatre {
        name: "Nevada",
        lat: (33.5, 39.5),
        lon: (-119.5, -111.5),
        countries: &[country!("the United States", "American", NEVADA_US)],
    },
];

#[cfg(test)]
mod tests {
    use super::*;

    fn country(lat: f64, lon: f64) -> Option<&'static str> {
        theatre_of([(lat, lon)]).and_then(|t| t.country_at(lat, lon)).map(|c| c.name)
    }

    /// Every coordinate here is a real DCS Syria airfield or town.
    #[test]
    fn syria_airbases_land_in_the_right_countries() {
        assert_eq!(country(35.4014, 35.9486), Some("Syria")); // Bassel Al-Assad
        assert_eq!(country(33.4106, 36.5144), Some("Syria")); // Damascus
        assert_eq!(country(34.5561, 38.2500), Some("Syria")); // Palmyra
        assert_eq!(country(36.1869, 37.2242), Some("Syria")); // Aleppo
        assert_eq!(country(33.8264, 35.4884), Some("Lebanon")); // Beirut-Rafic Hariri
        assert_eq!(country(33.8506, 35.9878), Some("Lebanon")); // Rayak
        assert_eq!(country(32.6653, 35.1783), Some("Israel")); // Megiddo
        assert_eq!(country(32.6650, 35.1800), Some("Israel")); // Ramat David
        assert_eq!(country(36.1808, 37.5872), Some("Syria")); // Kuweires
        assert_eq!(country(35.4011, 35.9486), Some("Syria")); // Latakia
        assert_eq!(country(36.9500, 35.2800), Some("Turkey")); // Incirlik
        assert_eq!(country(36.9822, 35.2800), Some("Turkey")); // Adana Sakirpasa
        assert_eq!(country(36.7847, 34.5367), Some("Turkey")); // Mersin
        assert_eq!(country(36.3639, 36.2836), Some("Turkey")); // Hatay
        assert_eq!(country(32.3564, 36.2594), Some("Jordan")); // north Jordan
        assert_eq!(country(35.1461, 33.4061), Some("Cyprus")); // Nicosia
        assert_eq!(country(34.5903, 32.9879), Some("Cyprus")); // Akrotiri
    }

    #[test]
    fn caucasus_airbases_land_in_the_right_countries() {
        assert_eq!(country(41.6103, 41.5997), Some("Georgia")); // Batumi
        assert_eq!(country(41.6692, 44.9547), Some("Georgia")); // Tbilisi-Lochini
        assert_eq!(country(42.1769, 42.4825), Some("Georgia")); // Kutaisi
        assert_eq!(country(42.8864, 41.1281), Some("Abkhazia")); // Sukhumi-Babushara
        assert_eq!(country(43.1392, 40.3653), Some("Abkhazia")); // Gudauta
        assert_eq!(country(43.4494, 39.9558), Some("Russia")); // Sochi-Adler
        assert_eq!(country(45.0219, 39.1706), Some("Russia")); // Krasnodar
        assert_eq!(country(43.7869, 44.6067), Some("Russia")); // Mozdok
        assert_eq!(country(42.2167, 43.9667), Some("South Ossetia")); // Tskhinvali
    }

    #[test]
    fn open_sea_has_no_country() {
        assert_eq!(country(34.0, 34.0), None); // eastern Mediterranean
        assert_eq!(country(43.0, 38.5), None); // Black Sea
        assert_eq!(country(26.5, 53.5), None); // middle of the Gulf
    }

    /// Three pairs of terrains genuinely cover the same ground -- ED built
    /// them as neighbours, and the seam between them is real country. Any
    /// *other* overlap is a mistake in a bounding box, and would let a
    /// campaign be attributed to the wrong map.
    #[test]
    fn only_neighbouring_terrains_share_a_bounding_box() {
        const NEIGHBOURS: &[(&str, &str)] =
            &[("Syria", "Sinai"), ("Syria", "Iraq"), ("Persian Gulf", "Iraq"),
              ("Persian Gulf", "Afghanistan")];
        for (i, a) in THEATRES.iter().enumerate() {
            for b in THEATRES.iter().skip(i + 1) {
                let lat = a.lat.0.max(b.lat.0) <= a.lat.1.min(b.lat.1);
                let lon = a.lon.0.max(b.lon.0) <= a.lon.1.min(b.lon.1);
                let known = NEIGHBOURS
                    .iter()
                    .any(|(x, y)| (*x == a.name && *y == b.name) || (*x == b.name && *y == a.name));
                assert!(!(lat && lon) || known, "{} overlaps {}", a.name, b.name);
            }
        }
    }

    #[test]
    fn iraq_airbases_land_in_the_right_countries() {
        let t = THEATRES.iter().find(|t| t.name == "Iraq").unwrap();
        let c = |lat: f64, lon: f64| t.country_at(lat, lon).map(|c| c.name);
        assert_eq!(c(33.2625, 44.2347), Some("Iraq")); // Baghdad International
        assert_eq!(c(36.2376, 43.9631), Some("Iraq")); // Mosul
        assert_eq!(c(30.5081, 47.6621), Some("Iraq")); // Basrah
        assert_eq!(c(29.2266, 47.9689), Some("Kuwait")); // Kuwait International
        assert_eq!(c(34.3461, 47.1581), Some("Iran")); // Kermanshah
        assert_eq!(c(33.4106, 36.5144), Some("Syria")); // Damascus, on the west edge
    }

    #[test]
    fn a_vote_survives_one_stray_position() {
        // Damascus, Bassel Al-Assad, and something in the Gulf of Guinea.
        let pts = vec![(33.41, 36.51), (35.40, 35.95), (0.0, 0.0)];
        assert_eq!(theatre_of(pts).map(|t| t.name), Some("Syria"));
    }

    #[test]
    fn the_vote_separates_syria_from_sinai() {
        // Syria: Damascus, Aleppo, Palmyra, Incirlik -- only the last is
        // anywhere near the shared slice, and none of them are in it.
        let syria = vec![(33.41, 36.51), (36.19, 37.22), (34.56, 38.25), (36.95, 35.28)];
        assert_eq!(theatre_of(syria).map(|t| t.name), Some("Syria"));
        // Sinai: Cairo West, Sharm el-Sheikh, Ramon, Hatzerim. The last two
        // are in the slice Syria also claims; the first two are not.
        let sinai = vec![(30.12, 30.92), (27.98, 34.39), (30.78, 34.67), (31.23, 34.66)];
        assert_eq!(theatre_of(sinai).map(|t| t.name), Some("Sinai"));
    }

    #[test]
    fn lists_read_like_copy() {
        let v = |s: &[&str]| join_names(&s.iter().map(|x| x.to_string()).collect::<Vec<_>>());
        assert_eq!(v(&["Syria"]), "Syria");
        assert_eq!(v(&["Israel", "Jordan"]), "Israel and Jordan");
        assert_eq!(v(&["Israel", "Turkey", "Jordan"]), "Israel, Turkey and Jordan");
    }
}
