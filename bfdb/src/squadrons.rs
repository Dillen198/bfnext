//! Squadrons: a 2-5 character tag, a name, an owner and up to
//! `MAX_MEMBERS` pilots. A squadron's XP is what its members earned while
//! they were in it -- time served, not a pilot's whole history -- so the
//! squadron leaderboard can't be won by recruiting veterans.
//!
//! Pilots join one squadron at a time. The owner can remove members; an
//! owner who leaves hands the squadron to its longest-serving member, and
//! the last one out disbands it.

use anyhow::{Result, bail};
use chrono::{DateTime, Utc};
use dcso3::net::Ucid;
use serde::{Deserialize, Serialize};
use sled::Db;
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

/// Tags are case-insensitive and stored upper case.
pub(crate) fn norm_tag(tag: &str) -> Result<String> {
    let t = tag.trim().to_uppercase();
    if !(2..=5).contains(&t.chars().count()) || !t.chars().all(|c| c.is_ascii_alphanumeric()) {
        bail!("a squadron tag is 2 to 5 letters or digits")
    }
    Ok(t)
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
        v.sort_by(|a, b| b.xp.cmp(&a.xp));
        Ok(v)
    }

    pub(crate) fn create(&self, owner: Ucid, tag: &str, name: &str) -> Result<Squadron> {
        let tag = norm_tag(tag)?;
        let name = name.trim();
        if name.is_empty() || name.chars().count() > 40 {
            bail!("a squadron name is 1 to 40 characters")
        }
        if self.of_pilot.get(&owner)?.is_some() {
            bail!("leave your current squadron first")
        }
        if self.by_tag.get(&tag)?.is_some() {
            bail!("the tag {tag} is taken")
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
            if by.map_or(false, |by| by != ucid && by != s.owner) {
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
            if let Some(m) = s.members.iter_mut().find(|m| m.ucid == *ucid && m.joined <= ts) {
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
        let tags: Vec<String> = self.by_tag.iter().map(|r| r.map(|(t, _)| t)).collect::<Result<_>>()?;
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
        assert!(norm_tag("TOOLONG").is_err());
        assert!(norm_tag("A-B").is_err());
    }

    #[test]
    fn xp_counts_only_after_joining_and_resets() {
        let db = sled::Config::new().temporary(true).open().unwrap();
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
