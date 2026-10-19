// Copyright (c) 2026 Dillen Weerasinghe. All rights reserved.
// Proprietary and confidential. No license is granted to use, copy, modify, or
// distribute this file or the `milatc` module. See the repository NOTICE file.

//! Controller phraseology: written and spoken forms.
//!
//! Every transmission is composed once, in the written form a transcript shows
//! ("Viper 1-1, runway 06R, cleared for takeoff, wind 070 at 12"). [`speak`]
//! turns that into what the voice says, by the radiotelephony rules: numbers
//! digit by digit ("zero seven zero", "one two"), altitudes in thousands
//! ("four thousand"), flight levels and frequencies with "point", runway
//! suffixes as words, and the "niner" that survives a noisy radio (US
//! controllers say "three" and "five", not ICAO's "tree" and "fife"). A
//! transcript and the audio can then never say different things.

/// ICAO digit word.
pub(crate) fn digit_word(d: u32) -> &'static str {
    match d {
        0 => "zero",
        1 => "one",
        2 => "two",
        // US military controllers say "three" and "five"; only "niner" survives
        3 => "three",
        4 => "four",
        5 => "five",
        6 => "six",
        7 => "seven",
        8 => "eight",
        _ => "niner",
    }
}

fn digits_spoken(s: &str) -> String {
    s.chars()
        .filter_map(|c| c.to_digit(10))
        .map(digit_word)
        .collect::<Vec<_>>()
        .join(" ")
}

/// Cardinal words for small counts and distances ("ten miles").
fn cardinal(n: u32) -> String {
    const ONES: [&str; 20] = [
        "zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine", "ten", "eleven",
        "twelve", "thirteen", "fourteen", "fifteen", "sixteen", "seventeen", "eighteen", "nineteen",
    ];
    const TENS: [&str; 10] = ["", "", "twenty", "thirty", "forty", "fifty", "sixty", "seventy", "eighty", "ninety"];
    match n {
        0..=19 => ONES[n as usize].to_string(),
        20..=99 => {
            let t = TENS[(n / 10) as usize];
            if n % 10 == 0 {
                t.to_string()
            } else {
                format!("{t} {}", ONES[(n % 10) as usize])
            }
        }
        _ => digits_spoken(&n.to_string()),
    }
}

/// An altitude in feet, the way a controller says it: "four thousand",
/// "two thousand one hundred", "one two thousand five hundred".
fn altitude_spoken(ft: u32) -> String {
    let thousands = ft / 1000;
    let hundreds = (ft % 1000) / 100;
    let mut parts = vec![];
    if thousands > 0 {
        let t = if thousands >= 10 { digits_spoken(&thousands.to_string()) } else { digit_word(thousands).to_string() };
        parts.push(format!("{t} thousand"));
    }
    if hundreds > 0 {
        parts.push(format!("{} hundred", digit_word(hundreds)));
    }
    if parts.is_empty() {
        "zero".into()
    } else {
        parts.join(" ")
    }
}

#[derive(Debug, PartialEq)]
enum Tok<'a> {
    Word(&'a str),
    Num(&'a str),
    Other(char),
}

fn lex(s: &str) -> Vec<Tok<'_>> {
    let mut out = vec![];
    let b = s.as_bytes();
    let mut i = 0;
    while i < b.len() {
        let c = s[i..].chars().next().unwrap();
        if c.is_ascii_digit() {
            let st = i;
            while i < b.len() && b[i].is_ascii_digit() {
                i += 1;
            }
            out.push(Tok::Num(&s[st..i]));
        } else if c.is_alphabetic() {
            let st = i;
            while i < s.len() {
                let ch = s[i..].chars().next().unwrap();
                if ch.is_alphabetic() || ch == '\'' {
                    i += ch.len_utf8();
                } else {
                    break;
                }
            }
            out.push(Tok::Word(&s[st..i]));
        } else {
            out.push(Tok::Other(c));
            i += c.len_utf8();
        }
    }
    out
}

/// The spoken form of a written transmission. See the module docs.
pub(crate) fn speak(written: &str) -> String {
    let t = lex(written);
    let mut out = String::new();
    let mut i = 0;
    let push = |out: &mut String, s: &str| {
        if !out.is_empty() && !out.ends_with(' ') {
            out.push(' ');
        }
        out.push_str(s);
    };
    while i < t.len() {
        match &t[i] {
            // FL220 → "flight level two two zero"
            Tok::Word(w) if *w == "FL" && matches!(t.get(i + 1), Some(Tok::Num(_))) => {
                if let Some(Tok::Num(n)) = t.get(i + 1) {
                    push(&mut out, &format!("flight level {}", digits_spoken(n)));
                }
                i += 2;
            }
            Tok::Num(n) => {
                let next = t.get(i + 1);
                let after = t.get(i + 2);
                // 4,000 / 12,500: an altitude
                if let (Some(Tok::Other(',')), Some(Tok::Num(m))) = (next, after) {
                    if m.len() == 3 {
                        let ft: u32 = format!("{n}{m}").parse().unwrap_or(0);
                        push(&mut out, &altitude_spoken(ft));
                        i += 3;
                        continue;
                    }
                }
                // 340.2 / 29.92: a frequency (or anything with a decimal)
                if let (Some(Tok::Other('.')), Some(Tok::Num(m))) = (next, after) {
                    push(&mut out, &format!("{} point {}", digits_spoken(n), digits_spoken(m)));
                    i += 3;
                    continue;
                }
                // 1-1: a callsign
                if let (Some(Tok::Other('-')), Some(Tok::Num(m))) = (next, after) {
                    let mut s = format!("{} {}", digits_spoken(n), digits_spoken(m));
                    let mut j = i + 3;
                    while let (Some(Tok::Other('-')), Some(Tok::Num(k))) = (t.get(j), t.get(j + 1)) {
                        s.push(' ');
                        s.push_str(&digits_spoken(k));
                        j += 2;
                    }
                    push(&mut out, &s);
                    i = j;
                    continue;
                }
                // 06R: a runway (the suffix is glued on)
                if let Some(Tok::Word(w)) = next {
                    let suffix = match *w {
                        "L" => Some("left"),
                        "R" => Some("right"),
                        "C" => Some("center"),
                        _ => None,
                    };
                    if let Some(sfx) = suffix {
                        push(&mut out, &format!("{} {sfx}", digits_spoken(n)));
                        i += 2;
                        continue;
                    }
                }
                // "10 miles", "2 aircraft": a count, said as a number
                let counted = matches!(next, Some(Tok::Other(' ')))
                    && matches!(after, Some(Tok::Word(w)) if matches!(
                        w.to_ascii_lowercase().as_str(),
                        "mile" | "miles" | "aircraft" | "minutes" | "minute" | "seconds" | "knots"
                    ));
                if counted && n.len() <= 2 {
                    push(&mut out, &cardinal(n.parse().unwrap_or(0)));
                } else {
                    push(&mut out, &digits_spoken(n));
                }
                i += 1;
            }
            Tok::Word(w) => {
                push(&mut out, w);
                i += 1;
            }
            Tok::Other(' ') => {
                i += 1;
            }
            Tok::Other(c) => {
                // Keep the punctuation TTS uses for pacing, drop the rest.
                if matches!(c, ',' | '.' | '?' | '!' | ';' | ':') {
                    out.push(*c);
                }
                i += 1;
            }
        }
    }
    out
}

/// "Viper 1-1" from what a pilot called himself: "viper one one", "Viper 11",
/// "viper 1 1", "VIPER 1-1". The first digit is the flight, the rest the
/// position in it.
pub(crate) fn format_callsign(word: &str, digits: &str) -> String {
    let mut w = word.to_lowercase();
    if let Some(f) = w.get_mut(0..1) {
        f.make_ascii_uppercase();
    }
    let d: Vec<char> = digits.chars().filter(|c| c.is_ascii_digit()).collect();
    if d.is_empty() {
        return w;
    }
    let tail: Vec<String> = d.iter().map(|c| c.to_string()).collect();
    format!("{w} {}", tail.join("-"))
}

/// A frequency for the written form: "340.2", "363.85".
pub(crate) fn freq(mhz: f64) -> String {
    let s = format!("{mhz:.3}");
    let s = s.trim_end_matches('0');
    let s = s.strip_suffix('.').map(|x| format!("{x}.0")).unwrap_or_else(|| s.to_string());
    s
}

/// A heading for the written form, always three digits: "070".
pub(crate) fn hdg(deg: f64) -> String {
    let mut d = (deg.round() as i64).rem_euclid(360);
    if d == 0 {
        d = 360;
    }
    format!("{d:03}")
}

/// An altitude for the written form: "4,000", "FL220".
pub(crate) fn alt(ft: i32) -> String {
    if ft >= 18_000 {
        return format!("FL{}", (ft + 50) / 100);
    }
    let ft = ((ft + 50) / 100 * 100).max(0);
    if ft >= 1000 {
        format!("{},{:03}", ft / 1000, ft % 1000)
    } else {
        ft.to_string()
    }
}

/// An altimeter setting, inches: "2992".
pub(crate) fn altimeter(inhg: f64) -> String {
    format!("{:04}", (inhg * 100.).round() as i64)
}

/// The surface wind for the written form: "wind 070 at 12", "wind calm".
pub(crate) fn wind(from: u16, kts: u16) -> String {
    if kts < 3 {
        "wind calm".into()
    } else {
        format!("wind {} at {kts}", hdg(from as f64))
    }
}

/// The phonetic name of a taxiway letter: "B" → "Bravo". Multi-character
/// designators ("A1") are spelled letter by letter.
pub(crate) fn taxiway(name: &str) -> String {
    name.chars()
        .map(|c| match c.to_ascii_uppercase() {
            'A' => "Alpha".to_string(),
            'B' => "Bravo".into(),
            'C' => "Charlie".into(),
            'D' => "Delta".into(),
            'E' => "Echo".into(),
            'F' => "Foxtrot".into(),
            'G' => "Golf".into(),
            'H' => "Hotel".into(),
            'I' => "India".into(),
            'J' => "Juliet".into(),
            'K' => "Kilo".into(),
            'L' => "Lima".into(),
            'M' => "Mike".into(),
            'N' => "November".into(),
            'O' => "Oscar".into(),
            'P' => "Papa".into(),
            'Q' => "Quebec".into(),
            'R' => "Romeo".into(),
            'S' => "Sierra".into(),
            'T' => "Tango".into(),
            'U' => "Uniform".into(),
            'V' => "Victor".into(),
            'W' => "Whiskey".into(),
            'X' => "X-ray".into(),
            'Y' => "Yankee".into(),
            'Z' => "Zulu".into(),
            d => d.to_string(),
        })
        .collect::<Vec<_>>()
        .join(" ")
}

/// The ATIS information letter.
pub(crate) fn letter(n: u8) -> &'static str {
    const L: [&str; 26] = [
        "Alpha", "Bravo", "Charlie", "Delta", "Echo", "Foxtrot", "Golf", "Hotel", "India", "Juliet", "Kilo",
        "Lima", "Mike", "November", "Oscar", "Papa", "Quebec", "Romeo", "Sierra", "Tango", "Uniform",
        "Victor", "Whiskey", "X-ray", "Yankee", "Zulu",
    ];
    L[(n % 26) as usize]
}

/// Clock direction / compass word for "15 miles north".
pub(crate) fn compass(brg: f64) -> &'static str {
    const C: [&str; 8] = ["north", "northeast", "east", "southeast", "south", "southwest", "west", "northwest"];
    C[(((brg.rem_euclid(360.) + 22.5) / 45.) as usize) % 8]
}


#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn speaks_numbers_by_kind() {
        assert_eq!(
            speak("Viper 1-1, runway 06R, cleared for takeoff, wind 070 at 12."),
            "Viper one one, runway zero six right, cleared for takeoff, wind zero seven zero at one two."
        );
        assert_eq!(
            speak("climb and maintain 4,000, expect FL220, departure 363.8, squawk 4215"),
            "climb and maintain four thousand, expect flight level two two zero, departure three six three point eight, squawk four two one five"
        );
        assert_eq!(speak("pattern altitude 2,100"), "pattern altitude two thousand one hundred");
        assert_eq!(speak("12,500"), "one two thousand five hundred");
        assert_eq!(speak("10 miles from touchdown"), "ten miles from touchdown");
        assert_eq!(speak("altimeter 2992"), "altimeter two niner niner two");
        assert_eq!(speak("traffic two o'clock, 3 miles"), "traffic two o'clock, three miles");
    }

    #[test]
    fn written_forms() {
        assert_eq!(freq(340.2), "340.2");
        assert_eq!(freq(363.85), "363.85");
        assert_eq!(freq(251.0), "251.0");
        assert_eq!(hdg(0.4), "360");
        assert_eq!(hdg(65.8), "066");
        assert_eq!(alt(4000), "4,000");
        assert_eq!(alt(2140), "2,100");
        assert_eq!(alt(22_000), "FL220");
        assert_eq!(altimeter(29.921), "2992");
        assert_eq!(format_callsign("viper", "11"), "Viper 1-1");
        assert_eq!(format_callsign("ENFIELD", "212"), "Enfield 2-1-2");
        assert_eq!(taxiway("B"), "Bravo");
    }
}
