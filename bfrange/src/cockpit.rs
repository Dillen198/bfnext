// Copyright (c) 2026 Dillen Weerasinghe. All rights reserved.
// Proprietary and confidential. No license is granted to use, copy, modify, or
// distribute this file. See bfrange/LICENSE and the repository NOTICE file.
//! The cockpit plugin's key, as the campaign keeps it (bflib/src/cockpit.rs,
//! "Overlay keys").
//!
//! The plugin makes up a random key and sends it as a `-cockpitkey <key>`
//! chat line, which only that player's own DCS can do and which the range
//! swallows. bfdb then asks `resolve-player-id` with the player id and key
//! before it hands that player anything, so a guessed player id is not
//! enough. On the range the plugin only uses it for the air-refuelling
//! overlay.

use dcso3::net::{PlayerId, Ucid};
use fxhash::FxHashMap;
use std::sync::{LazyLock, Mutex};

/// The chat command the plugin registers its key with. Never logged.
pub const KEY_CMD: &str = "-cockpitkey";

static KEYS: LazyLock<Mutex<FxHashMap<PlayerId, (Ucid, String)>>> =
    LazyLock::new(|| Mutex::new(FxHashMap::default()));

fn valid_key(key: &str) -> bool {
    (16..=128).contains(&key.len()) && key.bytes().all(|b| b.is_ascii_alphanumeric())
}

/// Record the key `id` (connected as `ucid`) just sent. False if malformed.
pub fn register(id: PlayerId, ucid: Ucid, key: &str) -> bool {
    if !valid_key(key) {
        return false;
    }
    KEYS.lock().unwrap_or_else(|e| e.into_inner()).insert(id, (ucid, key.to_string()));
    true
}

/// The ucid `id` registered `key` as, compared in constant time; None when
/// it does not match. `connected` is who holds `id` now: a key dies with the
/// connection, since DCS gives a departed player's id to the next to join.
pub fn check(id: PlayerId, connected: Option<Ucid>, key: &str) -> Option<Ucid> {
    let keys = KEYS.lock().unwrap_or_else(|e| e.into_inner());
    let (owner, want) = keys.get(&id)?;
    let same = want.len() == key.len()
        && want.bytes().zip(key.bytes()).fold(0u8, |acc, (a, b)| acc | (a ^ b)) == 0;
    (same && connected == Some(*owner)).then_some(*owner)
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::str::FromStr;

    #[test]
    fn keys() {
        let u = Ucid::from_str("0123456789abcdef0123456789abcdef").unwrap();
        let other = Ucid::from_str("fedcba9876543210fedcba9876543210").unwrap();
        let id = PlayerId::from(7);
        assert!(!register(id, u, "short"));
        assert!(register(id, u, "abcdefghijklmnop0123456789"));
        assert_eq!(check(id, Some(u), "abcdefghijklmnop0123456789"), Some(u));
        assert_eq!(check(id, Some(u), "abcdefghijklmnop0123456780"), None);
        // the id now belongs to someone else
        assert_eq!(check(id, Some(other), "abcdefghijklmnop0123456789"), None);
        assert_eq!(check(id, None, "abcdefghijklmnop0123456789"), None);
        assert_eq!(check(PlayerId::from(8), Some(u), "abcdefghijklmnop0123456789"), None);
    }
}
