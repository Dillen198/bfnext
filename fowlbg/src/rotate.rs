// Copyright 2026 Dillen Weerasinghe. MIT licensed: see fowlbg/LICENSE.
//! Rotating a log or feed out of the way: `<stem><UTC timestamp>.<ext>`
//! beside it. bfdb's range ingest looks for that name when it finishes a
//! rotated `range.jsonl`, so the pattern must not change.

use chrono::{DateTime, Utc};
use std::{
    fs, io,
    path::{Path, PathBuf},
};

/// Where `p` rotates to at `now`: `<stem><YYYYmmddTHHMMSSZ>.<ext>` in the
/// same directory (`bfnext.txt` -> `bfnext20261027T101112Z.txt`).
pub fn rotated_path(p: &Path, now: DateTime<Utc>) -> PathBuf {
    let stem = p.file_stem().and_then(|s| s.to_str()).unwrap_or("log");
    let ext = p.extension().and_then(|s| s.to_str()).unwrap_or("txt");
    p.with_file_name(format!("{stem}{}.{ext}", now.format("%Y%m%dT%H%M%SZ")))
}

/// Rename `p` to [`rotated_path`] if it exists. `Ok(None)` if there was
/// nothing to rotate, or the name is taken by a rotation in the same second
/// (the first one is kept and `p` stays where it is).
pub fn rotate(p: &Path) -> io::Result<Option<PathBuf>> {
    if !p.exists() {
        return Ok(None);
    }
    let to = rotated_path(p, Utc::now());
    if to.exists() {
        return Ok(None);
    }
    fs::rename(p, &to)?;
    Ok(Some(to))
}

#[cfg(test)]
mod tests {
    use super::*;
    use chrono::TimeZone;

    #[test]
    fn name_pattern() {
        let t = Utc.with_ymd_and_hms(2026, 10, 27, 10, 11, 12).unwrap();
        let p = Path::new("Logs").join("bfnext.txt");
        assert_eq!(rotated_path(&p, t), Path::new("Logs").join("bfnext20261027T101112Z.txt"));
        let p = Path::new("Logs").join("range.jsonl");
        assert_eq!(rotated_path(&p, t), Path::new("Logs").join("range20261027T101112Z.jsonl"));
    }

    #[test]
    fn rotate_moves_and_keeps_the_first() {
        let dir = std::env::temp_dir().join(format!("fowlbg-rotate-{}", std::process::id()));
        let _ = fs::remove_dir_all(&dir);
        fs::create_dir_all(&dir).unwrap();
        let p = dir.join("x.txt");
        assert_eq!(rotate(&p).unwrap(), None, "nothing to rotate");
        fs::write(&p, b"one").unwrap();
        let to = rotate(&p).unwrap().expect("rotated");
        assert!(!p.exists());
        assert_eq!(fs::read(&to).unwrap(), b"one");
        // same second again: the first rotation is kept, the live file stays
        fs::write(&p, b"two").unwrap();
        if rotated_path(&p, Utc::now()) == to {
            assert_eq!(rotate(&p).unwrap(), None);
            assert_eq!(fs::read(&p).unwrap(), b"two");
            assert_eq!(fs::read(&to).unwrap(), b"one");
        }
        let _ = fs::remove_dir_all(&dir);
    }
}
