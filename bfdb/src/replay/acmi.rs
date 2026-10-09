// Copyright (c) 2026 Dillen Weerasinghe. All rights reserved.
// Proprietary and confidential. No license is granted to use, copy, modify, or
// distribute this file. See NOTICE section 3 and the repository NOTICE file.
//! A streaming reader for Tacview's ACMI 2.x text format.
//!
//! The format, as far as we need it:
//!
//! ```text
//! FileType=text/acmi/tacview
//! FileVersion=2.2
//! 0,ReferenceTime=2011-06-02T05:00:00Z      global properties on object 0
//! #47.13                                     time frame, seconds from ReferenceTime
//! 3000102,T=1.2|0.5|2000|0|5|90,Type=Air+FixedWing,Name=F-16C_50,Pilot=Viper
//! -3000102                                   object removed
//! 0,Event=Destroyed|3000102|                 event, on object 0
//! ```
//!
//! Object ids are hex. Property values escape a literal comma as `\,`, and a
//! line ending in an unescaped `\` continues on the next one. `.zip.acmi`
//! files are a zip holding one text entry; `.txt.acmi` is the text itself.
//! A recording is hundreds of megabytes of text, so nothing here allocates per
//! property unless the value actually contains an escape.

use anyhow::{bail, Context, Result};
use std::{
    borrow::Cow,
    fs::File,
    io::{BufRead, BufReader, Read},
    path::Path,
};

/// One logical line of a recording.
pub(crate) enum Line<'a> {
    /// `#<seconds>`: every following line happens at this time.
    Frame(f64),
    /// `-<id>`: the object left the recording.
    Remove(u64),
    /// `<id>,<props>`: properties set on an object (object 0 is global).
    Object(u64, Props<'a>),
}

/// The comma-separated `Key=Value` list of an object line.
pub(crate) struct Props<'a> {
    rest: &'a str,
}

impl<'a> Iterator for Props<'a> {
    type Item = (&'a str, Cow<'a, str>);

    fn next(&mut self) -> Option<Self::Item> {
        while !self.rest.is_empty() {
            let bytes = self.rest.as_bytes();
            let mut i = 0;
            let mut escaped = false;
            while i < bytes.len() {
                match bytes[i] {
                    b'\\' => {
                        escaped = true;
                        i += 2;
                        continue;
                    }
                    b',' => break,
                    _ => i += 1,
                }
            }
            let i = i.min(bytes.len());
            let field = &self.rest[..i];
            self.rest = if i < bytes.len() { &self.rest[i + 1..] } else { "" };
            let Some((k, v)) = field.split_once('=') else { continue };
            let v = if escaped { Cow::Owned(unescape(v)) } else { Cow::Borrowed(v) };
            return Some((k, v));
        }
        None
    }
}

fn unescape(s: &str) -> String {
    let mut out = String::with_capacity(s.len());
    let mut it = s.chars();
    while let Some(c) = it.next() {
        if c == '\\' {
            if let Some(n) = it.next() {
                out.push(n);
            }
        } else {
            out.push(c);
        }
    }
    out
}

/// A position/orientation update from a `T=` property. `None` components
/// were left out of the line and keep their previous value.
#[derive(Debug, Default, Clone, Copy, PartialEq)]
pub(crate) struct Transform {
    pub(crate) lon: Option<f64>,
    pub(crate) lat: Option<f64>,
    pub(crate) alt: Option<f64>,
    pub(crate) roll: Option<f64>,
    pub(crate) pitch: Option<f64>,
    pub(crate) yaw: Option<f64>,
    /// Whether this form carries orientation at all (6 or 9 components).
    pub(crate) has_att: bool,
}

/// Parse a `T=` value: `lon|lat|alt`, `lon|lat|alt|u|v`,
/// `lon|lat|alt|roll|pitch|yaw` or `lon|lat|alt|roll|pitch|yaw|u|v|heading`.
pub(crate) fn transform(v: &str) -> Transform {
    let mut p: [Option<f64>; 9] = [None; 9];
    let mut n = 0;
    for (i, s) in v.split('|').enumerate() {
        n = i + 1;
        if i < 9 && !s.is_empty() {
            p[i] = s.parse().ok();
        }
    }
    let has_att = n == 6 || n == 9;
    Transform {
        lon: p[0],
        lat: p[1],
        alt: p[2],
        roll: if has_att { p[3] } else { None },
        pitch: if has_att { p[4] } else { None },
        yaw: if has_att { p[5] } else { None },
        has_att,
    }
}

/// Reads logical lines from an ACMI text stream.
pub(crate) struct Reader<R> {
    inner: R,
    raw: Vec<u8>,
    line: String,
    lineno: u64,
}

impl<R: BufRead> Reader<R> {
    /// Wrap a text stream and check its two header lines.
    pub(crate) fn new(inner: R) -> Result<Self> {
        let mut r = Self { inner, raw: Vec::with_capacity(4096), line: String::new(), lineno: 0 };
        let mut saw_type = false;
        // The header is the first two non-empty lines; tolerate their order.
        for _ in 0..2 {
            if !r.read_logical()? {
                bail!("empty recording");
            }
            let l = r.line.trim_start_matches('\u{feff}').trim();
            if l.starts_with("FileType=") {
                if !l.contains("acmi") {
                    bail!("not an ACMI recording: {l}");
                }
                saw_type = true;
            } else if !l.starts_with("FileVersion=") {
                bail!("not an ACMI recording (line {}: {l:.60})", r.lineno);
            }
        }
        if !saw_type {
            bail!("ACMI header has no FileType");
        }
        Ok(r)
    }

    /// Read one logical line (joining `\`-continued physical lines) into
    /// `self.line`. False at end of stream.
    fn read_logical(&mut self) -> Result<bool> {
        self.line.clear();
        loop {
            self.raw.clear();
            let n = self.inner.read_until(b'\n', &mut self.raw)?;
            if n == 0 {
                return Ok(!self.line.is_empty());
            }
            self.lineno += 1;
            while matches!(self.raw.last(), Some(b'\n' | b'\r')) {
                self.raw.pop();
            }
            match std::str::from_utf8(&self.raw) {
                Ok(s) => self.line.push_str(s),
                Err(_) => self.line.push_str(&String::from_utf8_lossy(&self.raw)),
            }
            if continues(&self.line) {
                self.line.pop();
                self.line.push('\n');
                continue;
            }
            return Ok(true);
        }
    }

    /// The next line, or `None` at end of stream. Malformed lines are
    /// skipped rather than failing a whole recording.
    pub(crate) fn next_line(&mut self) -> Result<Option<Line<'_>>> {
        // Classify first and borrow `self.line` only once the loop is done:
        // returning a borrow from inside the loop would hold it across the
        // next `read_logical`.
        enum Got {
            Frame(f64),
            Remove(u64),
            Object(u64, usize),
        }
        let got = loop {
            if !self.read_logical()? {
                return Ok(None);
            }
            let l = self.line.trim_start_matches('\u{feff}');
            let Some(first) = l.as_bytes().first() else { continue };
            match first {
                b'#' => {
                    if let Ok(t) = l[1..].trim().parse::<f64>() {
                        break Got::Frame(t);
                    }
                }
                b'-' => {
                    if let Ok(id) = u64::from_str_radix(l[1..].trim(), 16) {
                        break Got::Remove(id);
                    }
                }
                b'/' => (), // `// comment`
                _ => {
                    let (id, rest) = l.split_once(',').unwrap_or((l, ""));
                    if let Ok(id) = u64::from_str_radix(id.trim(), 16) {
                        break Got::Object(id, self.line.len() - rest.len());
                    }
                }
            }
        };
        Ok(Some(match got {
            Got::Frame(t) => Line::Frame(t),
            Got::Remove(id) => Line::Remove(id),
            Got::Object(id, at) => Line::Object(id, Props { rest: &self.line[at..] }),
        }))
    }
}

/// A line ending in an odd number of backslashes continues on the next one.
fn continues(s: &str) -> bool {
    s.bytes().rev().take_while(|b| *b == b'\\').count() % 2 == 1
}

/// Open a recording: `.zip.acmi` (the first entry of the zip) or plain text.
/// Calls `f` with a reader over its text.
pub(crate) fn open<T>(path: &Path, f: impl FnOnce(&mut Reader<Box<dyn BufRead + '_>>) -> Result<T>) -> Result<T> {
    let mut file = File::open(path).with_context(|| format!("opening {}", path.display()))?;
    let mut magic = [0u8; 4];
    let n = file.read(&mut magic)?;
    drop(file);
    if n == 4 && &magic == b"PK\x03\x04" {
        let file = File::open(path)?;
        match zip::ZipArchive::new(BufReader::new(file)) {
            Ok(mut zip) => {
                if zip.is_empty() {
                    bail!("the zip is empty");
                }
                let entry = zip.by_index(0)?;
                let rd: Box<dyn BufRead + '_> = Box::new(BufReader::with_capacity(1 << 20, entry));
                let mut r = Reader::new(rd)?;
                f(&mut r)
            }
            Err(_) => {
                // No central directory: Tacview writes the zip as it records,
                // and a mission that ends in a hard restart never gets it
                // closed. The first entry is still all there up to the cut.
                log::info!("replay: {} has no zip directory (recording cut off); reading what is there", path.display());
                let rd = truncated_entry(path)?;
                let mut r = Reader::new(rd)?;
                f(&mut r)
            }
        }
    } else {
        let file = File::open(path)?;
        let rd: Box<dyn BufRead + '_> = Box::new(BufReader::with_capacity(1 << 20, file));
        let mut r = Reader::new(rd)?;
        f(&mut r)
    }
}

/// Read the first entry of a zip whose end is missing, straight from its
/// local header: stored or deflated, as far as the data goes.
fn truncated_entry(path: &Path) -> Result<Box<dyn BufRead>> {
    use std::io::{Seek, SeekFrom};
    let mut f = File::open(path)?;
    let mut h = [0u8; 30];
    f.read_exact(&mut h).context("zip local header")?;
    if &h[..4] != b"PK\x03\x04" {
        bail!("not a zip");
    }
    let method = u16::from_le_bytes([h[8], h[9]]);
    let name_len = u16::from_le_bytes([h[26], h[27]]) as u64;
    let extra_len = u16::from_le_bytes([h[28], h[29]]) as u64;
    f.seek(SeekFrom::Start(30 + name_len + extra_len))?;
    let raw = BufReader::with_capacity(1 << 20, f);
    let inner: Box<dyn Read> = match method {
        0 => Box::new(raw),
        8 => Box::new(flate2::bufread::DeflateDecoder::new(raw)),
        m => bail!("zip compression method {m} is not supported"),
    };
    Ok(Box::new(BufReader::with_capacity(1 << 20, UntilCut(inner, false))))
}

/// A reader that ends quietly at the first error -- the point where a
/// cut-off recording's data stops.
struct UntilCut<R>(R, bool);

impl<R: Read> Read for UntilCut<R> {
    fn read(&mut self, buf: &mut [u8]) -> std::io::Result<usize> {
        if self.1 {
            return Ok(0);
        }
        match self.0.read(buf) {
            Ok(n) => Ok(n),
            Err(_) => {
                self.1 = true;
                Ok(0)
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// A recording cut off mid-write (no central directory, the deflate
    /// stream ends early) still yields everything before the cut.
    #[test]
    fn reads_a_cut_off_zip() {
        use std::io::Write;
        let dir = std::env::temp_dir().join(format!("bfdb-acmi-cut-{}", std::process::id()));
        std::fs::create_dir_all(&dir).unwrap();
        let path = dir.join("cut.zip.acmi");
        let mut text = String::from("FileType=text/acmi/tacview\nFileVersion=2.2\n");
        for i in 0..20_000 {
            text.push_str(&format!("#{i}\n1,T={}|0|1000\n", i as f64 * 0.001));
        }
        {
            let mut z = zip::ZipWriter::new(File::create(&path).unwrap());
            z.start_file("cut.txt.acmi", zip::write::FileOptions::default()).unwrap();
            z.write_all(text.as_bytes()).unwrap();
            z.finish().unwrap();
        }
        let len = std::fs::metadata(&path).unwrap().len();
        let f = std::fs::OpenOptions::new().write(true).open(&path).unwrap();
        f.set_len(len * 2 / 3).unwrap();
        drop(f);
        assert!(zip::ZipArchive::new(File::open(&path).unwrap()).is_err(), "really cut");
        let frames = open(&path, |r| {
            let mut n = 0;
            while let Some(l) = r.next_line()? {
                if matches!(l, Line::Frame(_)) {
                    n += 1;
                }
            }
            Ok(n)
        })
        .unwrap();
        assert!(frames > 5_000 && frames < 20_000, "{frames} frames recovered");
        let _ = std::fs::remove_dir_all(&dir);
    }

    fn lines(text: &str) -> Vec<String> {
        let mut r = Reader::new(std::io::Cursor::new(text.as_bytes().to_vec())).unwrap();
        let mut out = vec![];
        while let Some(l) = r.next_line().unwrap() {
            out.push(match l {
                Line::Frame(t) => format!("#{t}"),
                Line::Remove(id) => format!("-{id:x}"),
                Line::Object(id, p) => {
                    let props: Vec<String> = p.map(|(k, v)| format!("{k}={v}")).collect();
                    format!("{id:x}:{}", props.join(";"))
                }
            });
        }
        out
    }

    #[test]
    fn parses_frames_objects_removals_and_escapes() {
        let text = "\u{feff}FileType=text/acmi/tacview\nFileVersion=2.2\n\
                    0,ReferenceTime=2011-06-02T05:00:00Z,Title=A\\, B\n\
                    #1.5\n\
                    3000102,T=1|2|3,Name=F-16C_50\n\
                    0,Comments=line one\\\nline two\n\
                    -3000102\n";
        assert_eq!(
            lines(text),
            vec![
                "0:ReferenceTime=2011-06-02T05:00:00Z;Title=A, B",
                "#1.5",
                "3000102:T=1|2|3;Name=F-16C_50",
                "0:Comments=line one\nline two",
                "-3000102",
            ]
        );
    }

    #[test]
    fn rejects_other_files() {
        assert!(Reader::new(std::io::Cursor::new(b"hello\nworld\n".to_vec())).is_err());
    }

    #[test]
    fn transform_forms() {
        let t = transform("1|2|3");
        assert_eq!((t.lon, t.lat, t.alt, t.has_att), (Some(1.), Some(2.), Some(3.), false));
        let t = transform("1|2|3|4|5");
        assert!(!t.has_att && t.roll.is_none());
        let t = transform("1||3|10|20|30");
        assert_eq!((t.lat, t.roll, t.yaw, t.has_att), (None, Some(10.), Some(30.), true));
        let t = transform("1|2|3|10|20|30|7|8|90");
        assert_eq!((t.pitch, t.has_att), (Some(20.), true));
    }
}
