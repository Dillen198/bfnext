// Copyright (c) 2026 Dillen Weerasinghe. All rights reserved.
// Proprietary and confidential. No license is granted to use, copy, modify, or
// distribute this file or the `milatc` module. See the repository NOTICE file.

//! The live feed: what is being said on every frequency, as it is said, for
//! listeners on the range site, the way LiveATC streams a real airport.
//!
//! Every transmission (controllers, AI pilots, and players when
//! `publishPilotAudio` allows) is pushed onto a per-instance broadcast in
//! real time, in 100 ms chunks of 8 kHz audio: radio quality, 16 KB/s per
//! listener. Players' voices are streamed straight from SRS as they talk
//! ([`stream`]); what the controllers and AI pilots say is the same audio
//! that goes out on SRS. A WebSocket (`/api/range/atc/live`) carries it out, bracketed by
//! "on air" / "off air" messages so the page can show who is talking and
//! play the squelch. Nothing is sent between transmissions.

use std::{
    collections::HashMap,
    sync::{
        atomic::{AtomicUsize, Ordering},
        Arc, Mutex, OnceLock,
    },
    time::Duration,
};
use tokio::sync::broadcast;

/// The feed's sample rate.
pub(crate) const RATE: u32 = 8_000;
/// One chunk: 100 ms.
const CHUNK: usize = (RATE / 10) as usize;
/// Listeners across all instances; more than this are turned away.
pub(crate) const MAX_LISTENERS: usize = 200;

#[derive(Debug)]
pub(crate) enum Frame {
    On { station: Arc<str>, who: Arc<str>, text: Arc<str> },
    Audio { station: Arc<str>, pcm: Vec<i16> },
    Off { station: Arc<str> },
}

impl Frame {
    pub(crate) fn station(&self) -> &str {
        match self {
            Frame::On { station, .. } | Frame::Audio { station, .. } | Frame::Off { station } => station,
        }
    }
}

fn hubs() -> &'static Mutex<HashMap<String, broadcast::Sender<Arc<Frame>>>> {
    static H: OnceLock<Mutex<HashMap<String, broadcast::Sender<Arc<Frame>>>>> = OnceLock::new();
    H.get_or_init(Default::default)
}

fn hub(inst: &str) -> broadcast::Sender<Arc<Frame>> {
    hubs()
        .lock()
        .unwrap()
        .entry(inst.to_string())
        // a few seconds of chunks: a slow listener skips ahead, never holds up others
        .or_insert_with(|| broadcast::channel(256).0)
        .clone()
}

pub(crate) fn subscribe(inst: &str) -> broadcast::Receiver<Arc<Frame>> {
    hub(inst).subscribe()
}

/// Listeners connected right now, for the cap.
pub(crate) static LISTENERS: AtomicUsize = AtomicUsize::new(0);

/// A held listener slot; gives it back when the socket closes.
pub(crate) struct Slot;

impl Slot {
    pub(crate) fn take() -> Option<Slot> {
        let n = LISTENERS.fetch_add(1, Ordering::Relaxed);
        if n >= MAX_LISTENERS {
            LISTENERS.fetch_sub(1, Ordering::Relaxed);
            None
        } else {
            Some(Slot)
        }
    }
}

impl Drop for Slot {
    fn drop(&mut self) {
        LISTENERS.fetch_sub(1, Ordering::Relaxed);
    }
}

/// 16 kHz → 8 kHz: average each pair, which is also the low-pass a halving
/// needs (radio audio has nothing above 4 kHz worth keeping).
pub(crate) fn to_feed_rate(pcm16k: &[i16]) -> Vec<i16> {
    pcm16k.chunks(2).map(|p| ((p[0] as i32 + *p.get(1).unwrap_or(&p[0]) as i32) / 2) as i16).collect()
}

/// One frequency's turn on the feed: a player's call (which reaches us only
/// once they unkey) and the controller's answer never play over each other.
fn floor(inst: &str, station: &str) -> Arc<tokio::sync::Mutex<()>> {
    static F: OnceLock<Mutex<HashMap<String, Arc<tokio::sync::Mutex<()>>>>> = OnceLock::new();
    F.get_or_init(Default::default).lock().unwrap().entry(format!("{inst}|{station}")).or_default().clone()
}

/// Is anyone listening to an instance's feed right now?
pub(crate) fn listening(inst: &str) -> bool {
    hub(inst).receiver_count() > 0
}

/// Put one transmission on the feed, paced in real time, after anything
/// still playing on the same frequency. Returns at once; the pacing runs on
/// its own task. Costs nothing when nobody is listening.
pub(crate) fn play(inst: &str, station: &str, who: &str, text: &str, pcm16k: &[i16]) {
    let tx = hub(inst);
    if tx.receiver_count() == 0 || pcm16k.is_empty() {
        return;
    }
    let turn = floor(inst, station);
    let station: Arc<str> = Arc::from(station);
    let pcm = to_feed_rate(pcm16k);
    let on = Frame::On { station: station.clone(), who: Arc::from(who), text: Arc::from(text) };
    tokio::spawn(async move {
        let _turn = turn.lock().await;
        tokio::time::sleep(Duration::from_millis(250)).await;
        let _ = tx.send(Arc::new(on));
        let mut tick = tokio::time::interval(Duration::from_millis(100));
        tick.set_missed_tick_behavior(tokio::time::MissedTickBehavior::Delay);
        for c in pcm.chunks(CHUNK) {
            tick.tick().await;
            let _ = tx.send(Arc::new(Frame::Audio { station: station.clone(), pcm: c.to_vec() }));
        }
        tick.tick().await;
        let _ = tx.send(Arc::new(Frame::Off { station }));
    });
}

/// A player's voice, streamed as it arrives from SRS (see
/// [`crate::voice::srs::LiveVoice`]): no pacing needed, it comes in real time.
pub(crate) fn stream(inst: &str, v: crate::voice::srs::LiveVoice) {
    use crate::voice::srs::LiveVoice;
    let tx = hub(inst);
    if tx.receiver_count() == 0 {
        return;
    }
    let f = match v {
        LiveVoice::On { label, name } => Frame::On { station: Arc::from(label.as_str()), who: Arc::from(name.as_str()), text: Arc::from("") },
        LiveVoice::Audio { label, pcm } => Frame::Audio { station: Arc::from(label.as_str()), pcm: to_feed_rate(&pcm) },
        LiveVoice::Off { label } => Frame::Off { station: Arc::from(label.as_str()) },
    };
    let _ = tx.send(Arc::new(f));
}

/// The WebSocket message for a frame: JSON for on/off, binary for audio
/// (`[station id length: u8][station id][s16le samples]`).
pub(crate) enum Wire {
    Text(String),
    Binary(Vec<u8>),
}

pub(crate) fn wire(f: &Frame) -> Wire {
    match f {
        Frame::On { station, who, text } => {
            Wire::Text(serde_json::json!({ "on": &**station, "who": &**who, "text": &**text, "rate": RATE }).to_string())
        }
        Frame::Off { station } => Wire::Text(serde_json::json!({ "off": &**station }).to_string()),
        Frame::Audio { station, pcm } => {
            let id = station.as_bytes();
            let mut b = Vec::with_capacity(1 + id.len() + pcm.len() * 2);
            b.push(id.len().min(255) as u8);
            b.extend_from_slice(&id[..id.len().min(255)]);
            for s in pcm {
                b.extend_from_slice(&s.to_le_bytes());
            }
            Wire::Binary(b)
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn downsamples_and_frames() {
        let pcm: Vec<i16> = (0..16_000).map(|i| (i % 100) as i16).collect();
        let f = to_feed_rate(&pcm);
        assert_eq!(f.len(), 8_000);
        assert_eq!(f[0], 0); // (0 + 1) / 2
        let Wire::Binary(b) = wire(&Frame::Audio { station: Arc::from("0:tower"), pcm: vec![1, -2] }) else { panic!() };
        assert_eq!(b[0] as usize, "0:tower".len());
        assert_eq!(&b[1..8], b"0:tower");
        assert_eq!(&b[8..], &[1, 0, 0xfe, 0xff]);
        let Wire::Text(t) = wire(&Frame::On { station: Arc::from("0:tower"), who: Arc::from("Andersen Tower"), text: Arc::from("hi") }) else {
            panic!()
        };
        assert!(t.contains("\"on\":\"0:tower\"") && t.contains("\"rate\":8000"));
    }

    #[tokio::test]
    async fn plays_in_real_time_to_listeners() {
        let mut rx = subscribe("live-test");
        play("live-test", "0:ground", "Andersen Ground", "taxi", &vec![0i16; 16_000 / 2]); // 0.5 s
        let t0 = std::time::Instant::now();
        let mut audio = 0;
        loop {
            let f = rx.recv().await.unwrap();
            match &*f {
                Frame::On { who, .. } => assert_eq!(&**who, "Andersen Ground"),
                Frame::Audio { pcm, .. } => audio += pcm.len(),
                Frame::Off { .. } => break,
            }
        }
        assert_eq!(audio, 4_000);
        // five chunks a tenth of a second apart
        assert!(t0.elapsed() >= Duration::from_millis(400), "{:?}", t0.elapsed());
    }
}
