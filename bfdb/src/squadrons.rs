//! Squadrons: a short tag ("[.ID]", "VS1"), a name, an owner and up to
//! `MAX_MEMBERS` pilots. A squadron's XP is what its members earned while
//! they were in it -- time served, not a pilot's whole history -- so the
//! squadron leaderboard can't be won by recruiting veterans.
//!
//! Pilots join one squadron at a time. The owner can remove members; an
//! owner who leaves hands the squadron to its longest-serving member, and
//! the last one out disbands it.

use anyhow::{bail, Result};
use chrono::{DateTime, Utc};
use dcso3::net::Ucid;
use serde::{Deserialize, Serialize};
use yats::Db;
use yats::Tree;

pub(crate) const MAX_MEMBERS: usize = 50;

#[derive(Debug, Clone, Serialize, Deserialize)]
pub(crate) struct Member {
    pub(crate) ucid: Ucid,
    pub(crate) joined: DateTime<Utc>,
    /// XP earned for the squadron while a member.
    #[serde(default)]
    pub(crate) xp: u64,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub(crate) struct Squadron {
    pub(crate) tag: String,
    pub(crate) name: String,
    pub(crate) owner: Ucid,
    pub(crate) created: DateTime<Utc>,
    pub(crate) members: Vec<Member>,
    pub(crate) xp: u64,
}

#[derive(Clone)]
pub(crate) struct Squadrons {
    by_tag: Tree<String, Squadron>,
    of_pilot: Tree<Ucid, String>,
}

/// The longest tag, symbols included: room for "[.VFA]" or "|TFG-1|".
pub(crate) const MAX_TAG: usize = 8;

/// Symbols a tag may not use: the ones that break a URL path even
/// percent-encoded (proxies refuse or rewrite `%2F` and `%5C`), `%` itself,
/// and the backtick, which opens a code span in Discord.
const TAG_BANNED: &[char] = &['/', '\\', '%', '?', '#', '`'];

/// Tags are written the way they sit in a DCS name, "[.ID]", "=VS1=",
/// "|TFG|": letters, digits and ASCII symbols, 2 to `MAX_TAG` characters,
/// at least one of them a letter or digit. They are case-insensitive and
/// stored upper case. Spaces and anything outside ASCII are refused (the
/// pilot card's font draws those as boxes), except the numero sign, which
/// is read as the "No" it stands for, so "№15" is NO15.
///
/// Membership goes by linked UCID, never by the DCS name, so a pilot's name
/// can carry the tag in any form.
pub(crate) fn norm_tag(tag: &str) -> Result<String> {
    let t = tag.trim().replace('№', "NO").to_uppercase();
    let n = t.chars().count();
    if !(2..=MAX_TAG).contains(&n) {
        bail!("a squadron tag is 2 to {MAX_TAG} characters")
    }
    if let Some(c) = t.chars().find(|c| !c.is_ascii_graphic() || TAG_BANNED.contains(c)) {
        if c == ' ' {
            bail!("a squadron tag has no spaces")
        }
        bail!("a squadron tag can't use {c:?}: letters, digits and symbols like [ ] . = - | only")
    }
    if !t.chars().any(|c| c.is_ascii_alphanumeric()) {
        bail!("a squadron tag needs at least one letter or digit")
    }
    Ok(t)
}

/// A tag as it is shown beside a name: a plain one gets brackets ("[VS1]"),
/// one that brings its own symbols ("[.ID]", "=VS1=") is shown as it is.
pub(crate) fn shown_tag(tag: &str) -> String {
    if tag.chars().all(|c| c.is_ascii_alphanumeric()) {
        format!("[{tag}]")
    } else {
        tag.to_string()
    }
}

/// The letters and digits of a tag, for telling squadrons apart: "[.ID]",
/// "=ID=" and "ID" are one squadron's tag, so only one of them can exist.
fn tag_core(tag: &str) -> String {
    tag.chars().filter(|c| c.is_ascii_alphanumeric()).collect::<String>().to_uppercase()
}

/// Words a tag or squadron name may not contain anywhere: none of these
/// turns up inside an ordinary word.
const BAD_STEMS: &[&str] = &[
    "FUCK", "SHIT", "CUNT", "NIGG", "NIGA", "FAGG", "FAGOT", "RETARD", "WHORE", "BITCH",
    "PUSSY", "PENIS", "VAGIN", "DILDO", "TWAT", "WANK", "PORN", "JIZZ", "ASSHOL", "PISS",
    "BOOB", "HITLER", "NAZI", "1488", "KKK", "PIZD", "BLYAT", "BLYAD", "EBLAN", "MUDAK",
    "PIDOR", "SIEGHEIL",
];

/// Words that are only bad on their own: "COCK" is fine in "COCKPIT", "ASS"
/// in "CLASS", "RAPE" in "GRAPE".
const BAD_WORDS: &[&str] = &[
    "ASS", "ARSE", "COCK", "DICK", "CUM", "SEX", "FAG", "TITS", "TITTY", "SLUT", "RAPE",
    "RAPIST", "KIKE", "CHINK", "SPIC", "GOOK", "COON", "WOG", "SUKA", "HUY", "HUI",
    "XYU", "SS", "HEIL",
];

/// `word` read with the usual digit and symbol stand-ins for letters, so
/// "SH1T" and "A$$" are caught. Only used when the word has a letter in it:
/// "455" is a squadron number, not "ASS".
fn unleet(word: &str) -> String {
    word.chars()
        .filter_map(|c| match c {
            '0' => Some('O'),
            '1' | '!' | '|' => Some('I'),
            '3' => Some('E'),
            '4' | '@' => Some('A'),
            '5' | '$' => Some('S'),
            '7' => Some('T'),
            '8' => Some('B'),
            c if c.is_alphanumeric() => Some(c),
            _ => None,
        })
        .collect::<String>()
        .to_uppercase()
}

/// Whether one word (letters, digits and symbols, no spaces) is offensive.
fn bad_word(word: &str) -> bool {
    let raw: String =
        word.chars().filter(|c| c.is_alphanumeric()).collect::<String>().to_uppercase();
    let mut forms = vec![raw.clone()];
    if raw.chars().any(|c| c.is_alphabetic()) {
        forms.push(unleet(word));
    }
    forms.iter().any(|f| {
        !f.is_empty()
            && (BAD_STEMS.iter().any(|s| f.contains(s))
                || BAD_WORDS.contains(&f.as_str()))
    })
}

/// Whether `text` (a tag or a squadron name) has a bad word in it. Names are
/// checked a word at a time, so "Push It" doesn't read as "PUSHIT".
pub(crate) fn offensive(text: &str) -> bool {
    text.split_whitespace().any(bad_word)
}

impl Squadrons {
    pub(crate) fn new(db: &Db) -> Result<Self> {
        Ok(Self {
            by_tag: Tree::open(db, "squadrons")?,
            of_pilot: Tree::open(db, "squadron_of_pilot")?,
        })
    }

    pub(crate) fn of(&self, ucid: &Ucid) -> Result<Option<String>> {
        self.of_pilot.get(ucid)
    }

    pub(crate) fn get(&self, tag: &str) -> Result<Option<Squadron>> {
        self.by_tag.get(&norm_tag(tag)?)
    }

    pub(crate) fn all(&self) -> Result<Vec<Squadron>> {
        let mut v = vec![];
        for r in self.by_tag.iter() {
            v.push(r?.1);
        }
        v.sort_by_key(|a| std::cmp::Reverse(a.xp));
        Ok(v)
    }

    pub(crate) fn create(&self, owner: Ucid, tag: &str, name: &str) -> Result<Squadron> {
        let tag = norm_tag(tag)?;
        let name = name.trim();
        if name.is_empty() || name.chars().count() > 40 {
            bail!("a squadron name is 1 to 40 characters")
        }
        if offensive(&tag) {
            bail!("that tag isn't allowed, pick another")
        }
        if offensive(name) {
            bail!("that squadron name isn't allowed, pick another")
        }
        if self.of_pilot.get(&owner)?.is_some() {
            bail!("leave your current squadron first")
        }
        let core = tag_core(&tag);
        for r in self.by_tag.iter() {
            let (other, _) = r?;
            if tag_core(&other) == core {
                bail!("the tag {tag} is taken (by {other})")
            }
        }
        let now = Utc::now();
        let sq = Squadron {
            tag: tag.clone(),
            name: name.to_string(),
            owner,
            created: now,
            members: vec![Member { ucid: owner, joined: now, xp: 0 }],
            xp: 0,
        };
        self.by_tag.insert(&tag, &sq)?;
        self.of_pilot.insert(&owner, &tag)?;
        Ok(sq)
    }

    pub(crate) fn join(&self, ucid: Ucid, tag: &str) -> Result<()> {
        let tag = norm_tag(tag)?;
        if self.of_pilot.get(&ucid)?.is_some() {
            bail!("leave your current squadron first")
        }
        let mut err = None;
        self.by_tag.fetch_and_update(&tag, |s| {
            let mut s = s?;
            if s.members.len() >= MAX_MEMBERS {
                err = Some(format!("{tag} is full ({MAX_MEMBERS} pilots)"));
            } else if !s.members.iter().any(|m| m.ucid == ucid) {
                s.members.push(Member { ucid, joined: Utc::now(), xp: 0 });
            }
            Some(s)
        })?;
        if let Some(e) = err {
            bail!(e)
        }
        if self.by_tag.get(&tag)?.is_none() {
            bail!("no squadron {tag}")
        }
        self.of_pilot.insert(&ucid, &tag)?;
        Ok(())
    }

    /// `ucid` leaves its squadron (or, with `by` the owner, is removed).
    pub(crate) fn leave(&self, ucid: Ucid, by: Option<Ucid>) -> Result<()> {
        let Some(tag) = self.of_pilot.get(&ucid)? else { bail!("not in a squadron") };
        let mut denied = false;
        let mut disband = false;
        self.by_tag.fetch_and_update(&tag, |s| {
            let mut s = s?;
            if by.is_some_and(|by| by != ucid && by != s.owner) {
                denied = true;
                return Some(s);
            }
            s.members.retain(|m| m.ucid != ucid);
            if s.members.is_empty() {
                disband = true;
                return Some(s);
            }
            if s.owner == ucid {
                s.members.sort_by_key(|m| m.joined);
                s.owner = s.members[0].ucid;
            }
            Some(s)
        })?;
        if denied {
            bail!("only the squadron's owner can remove a member")
        }
        if disband {
            self.by_tag.remove(&tag)?;
        }
        self.of_pilot.remove(&ucid)?;
        Ok(())
    }

    /// Credit `xp` the pilot earned at `ts` to their squadron, if they were a
    /// member by then. Checking `ts` against the join date is what lets a
    /// stats rebuild replay old events without crediting a pilot's history
    /// from before they joined.
    pub(crate) fn add_xp(&self, ucid: &Ucid, xp: u64, ts: DateTime<Utc>) -> Result<()> {
        if xp == 0 {
            return Ok(());
        }
        let Some(tag) = self.of_pilot.get(ucid)? else { return Ok(()) };
        self.by_tag.fetch_and_update(&tag, |s| {
            let mut s = s?;
            if let Some(m) =
                s.members.iter_mut().find(|m| m.ucid == *ucid && m.joined <= ts)
            {
                m.xp = m.xp.saturating_add(xp);
                s.xp = s.xp.saturating_add(xp);
            }
            Some(s)
        })?;
        Ok(())
    }

    /// Zero every squadron's XP, keeping the rosters, for a stats rebuild:
    /// the replay credits it again.
    pub(crate) fn reset_xp(&self) -> Result<()> {
        let tags: Vec<String> =
            self.by_tag.iter().map(|r| r.map(|(t, _)| t)).collect::<Result<_>>()?;
        for tag in tags {
            self.by_tag.fetch_and_update(&tag, |s| {
                let mut s = s?;
                s.xp = 0;
                for m in &mut s.members {
                    m.xp = 0;
                }
                Some(s)
            })?;
        }
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn tags_are_normalised_and_checked() {
        assert_eq!(norm_tag(" vs1 ").unwrap(), "VS1");
        assert!(norm_tag("A").is_err());
        assert!(norm_tag("TOOLONGER").is_err());
        assert_eq!(norm_tag("[.id]").unwrap(), "[.ID]");
        assert_eq!(norm_tag("=VS1=").unwrap(), "=VS1=");
        assert_eq!(norm_tag("|TFG-1|").unwrap(), "|TFG-1|");
        assert_eq!(norm_tag("A-B").unwrap(), "A-B");
        assert!(norm_tag("[.I]").is_ok());
        assert!(norm_tag("[TOOLONG]").is_err());
        assert!(norm_tag("=VS 1=").is_err());
        assert!(norm_tag("[..]").is_err());
        assert!(norm_tag("A/B").is_err());
        assert!(norm_tag("ЯК52").is_err());
        assert_eq!(norm_tag("№15").unwrap(), "NO15");
        assert!(norm_tag("№1234567").is_err());
    }

    #[test]
    fn bad_words_are_refused_and_ordinary_ones_are_not() {
        for bad in
            ["FUCK", "[SH1T]", "A$$", "=ASS=", "1488", "Fuck Off", "N1GGA", "B1tch Wing"]
        {
            assert!(offensive(bad), "{bad} should be refused");
        }
        for ok in [
            "[.ID]",
            "455",
            "VS1",
            "|TFG|",
            "Garuda virtual Air Wing",
            "Cockpit Cowboys",
            "Assault Group",
            "Push It",
            "VFA-103",
            "F-14 Tomcats",
            "B-52 Stratos",
            "Class Act",
        ] {
            assert!(!offensive(ok), "{ok} should be allowed");
        }
    }

    #[test]
    fn tags_with_the_same_letters_are_one_squadron() {
        let db = yats::Db::temporary_sled().unwrap();
        let sq = Squadrons::new(&db).unwrap();
        let a: Ucid = "0123456789abcdef0123456789abcdef".parse().unwrap();
        let b: Ucid = "fedcba9876543210fedcba9876543210".parse().unwrap();
        assert_eq!(sq.create(a, "[.ID]", "Garuda").unwrap().tag, "[.ID]");
        assert!(sq.create(b, "ID", "Copycat").is_err());
        assert!(sq.create(b, "=id=", "Copycat").is_err());
        assert!(sq.create(b, "[SH1T]", "Nope").is_err());
        sq.join(b, "[.id]").unwrap();
        assert_eq!(sq.of(&b).unwrap().as_deref(), Some("[.ID]"));
    }

    #[test]
    fn xp_counts_only_after_joining_and_resets() {
        let db = yats::Db::temporary_sled().unwrap();
        let sq = Squadrons::new(&db).unwrap();
        let owner: Ucid = "0123456789abcdef0123456789abcdef".parse().unwrap();
        let s = sq.create(owner, "VS1", "Vector").unwrap();
        let joined = s.members[0].joined;
        sq.add_xp(&owner, 500, joined - chrono::Duration::hours(1)).unwrap();
        assert_eq!(sq.get("VS1").unwrap().unwrap().xp, 0);
        sq.add_xp(&owner, 300, joined + chrono::Duration::hours(1)).unwrap();
        let s = sq.get("VS1").unwrap().unwrap();
        assert_eq!((s.xp, s.members[0].xp), (300, 300));
        sq.reset_xp().unwrap();
        let s = sq.get("VS1").unwrap().unwrap();
        assert_eq!((s.xp, s.members[0].xp, s.members.len()), (0, 0, 1));
    }
}
