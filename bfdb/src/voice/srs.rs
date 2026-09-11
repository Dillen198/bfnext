// Copyright (c) 2026 Dillen Weerasinghe. All rights reserved.
// Proprietary and confidential — no license granted. See the repository NOTICE.

//! Persistent DCS-SimpleRadio-Standalone voice client — one per coalition,
//! carrying a radio per controlled frequency. Stays connected (visible in the
//! SRS client list) and authenticates via External AWACS Mode.
//!
//! One client serves every position on a side: GCI on its UHF channel, each
//! airfield's ATC, the JTAC net. Transmits name the frequency to speak on;
//! inbound transmissions are tagged with the radio they arrived on so a router
//! can hand them to the right position.
//!
//! Protocol references (both MIT):
//!   - ciribob/DCS-SimpleRadioStandalone  Common/Models/{NetworkMessage,UDPVoicePacket}.cs
//!   - dharmab/skyeye                      pkg/simpleradio/*
//!
//! Opus encoding is done through `opus.dll` (the copy that ships with DCS-SRS),
//! loaded at runtime — there is no build-time Opus/CMake dependency.

use super::Radio;
use anyhow::{Context, Result};
use libloading::{Library, Symbol};
use serde_json::json;
use std::{
    collections::HashMap,
    io::{Read, Write},
    net::{TcpStream, UdpSocket},
    path::Path,
    sync::{
        atomic::{AtomicBool, AtomicU64, Ordering},
        Arc, Mutex,
    },
    time::{Duration, Instant},
};
use tokio::sync::mpsc::UnboundedSender;

/// A completed inbound transmission on one of our frequencies — a player
/// keying up.
pub(crate) struct Transmission {
    /// 1 = red, 2 = blue (which coalition client received it).
    pub coalition: u8,
    /// SRS client name of the transmitter (matched to a pilot downstream).
    pub name: String,
    /// SRS client GUID of the transmitter — a stable key for the session, used
    /// to stitch a paused transmission back together.
    pub guid: String,
    /// DCS unit id of the jet the caller is sitting in, when their SRS client
    /// is reporting one. The exact key back to a flight in the engine picture.
    pub unit_id: Option<u64>,
    /// The frequency it arrived on, and the owning position's label for it.
    pub freq_hz: f64,
    pub label: String,
    /// 16 kHz mono PCM.
    pub pcm: Vec<i16>,
}

/// What the TCP client stream tells us about another SRS client.
#[derive(Debug, Clone)]
struct ClientInfo {
    name: String,
    unit_id: Option<u64>,
}

const GUID_LEN: usize = 22;
const SRS_VERSION: &str = "2.1.0.2";
const UNIT_ID: u32 = 100_000_002;
const FRAME_SAMPLES: usize = 640; // 40 ms @ 16 kHz mono
const FRAME_MS: u64 = 40;
/// Radio slots an SRS client advertises (slot 0 is the intercom).
const RADIO_SLOTS: usize = 11;
/// Silence that ends a transmission. A player who pauses to think mid-call
/// ("Magic... Colt one one... bogey dope") would otherwise be split into
/// fragments, and the fragment carrying the request would arrive without the
/// callsign that addressed it.
const RX_END_GAP: Duration = Duration::from_millis(700);
/// A pause longer than `RX_END_GAP` but shorter than this, from the same
/// client on the same frequency, is stitched onto the previous transmission
/// rather than starting a new one.
const RX_STITCH_WINDOW: Duration = Duration::from_millis(2500);
/// Shortest transmission worth transcribing.
const RX_MIN_FRAMES: usize = 12; // ~0.5 s

// SRS MsgType values (NetworkMessage.NetworkMessageType).
const MSG_UPDATE: i64 = 0;
const MSG_PING: i64 = 1;
const MSG_SYNC: i64 = 2;
const MSG_RADIO_UPDATE: i64 = 3;
const MSG_VERSION_MISMATCH: i64 = 6;
const MSG_EAM_PASSWORD: i64 = 7;
const _: () = {
    // silence dead-code for the constants we keep for documentation
    let _ = (MSG_UPDATE, MSG_RADIO_UPDATE);
};

// ─── Opus (dynamically loaded) ─────────────────────────────────────────────

#[repr(C)]
struct OpusEncoder {
    _private: [u8; 0],
}
#[repr(C)]
struct OpusDecoder {
    _private: [u8; 0],
}

type OpusCreate = unsafe extern "C" fn(i32, i32, i32, *mut i32) -> *mut OpusEncoder;
type OpusEncode = unsafe extern "C" fn(*mut OpusEncoder, *const i16, i32, *mut u8, i32) -> i32;
type OpusDestroy = unsafe extern "C" fn(*mut OpusEncoder);
type OpusDecCreate = unsafe extern "C" fn(i32, i32, *mut i32) -> *mut OpusDecoder;
type OpusDecode =
    unsafe extern "C" fn(*mut OpusDecoder, *const u8, i32, *mut i16, i32, i32) -> i32;
type OpusDecDestroy = unsafe extern "C" fn(*mut OpusDecoder);

pub(crate) struct Opus {
    create: Symbol<'static, OpusCreate>,
    encode: Symbol<'static, OpusEncode>,
    destroy: Symbol<'static, OpusDestroy>,
    dec_create: Symbol<'static, OpusDecCreate>,
    decode: Symbol<'static, OpusDecode>,
    dec_destroy: Symbol<'static, OpusDecDestroy>,
}

// The symbols are plain C function pointers into a process-lifetime library.
unsafe impl Send for Opus {}
unsafe impl Sync for Opus {}

impl Opus {
    /// Load `opus.dll` (or an explicit path). The library is leaked so the
    /// symbols stay valid for the life of the process.
    pub(crate) fn load(explicit: Option<&Path>) -> Result<Self> {
        let lib: &'static Library = Box::leak(Box::new(unsafe {
            match explicit {
                Some(p) => Library::new(p).with_context(|| format!("loading {}", p.display()))?,
                None => {
                    let name = if cfg!(windows) { "opus.dll" } else { "libopus.so.0" };
                    Library::new(name).with_context(|| {
                        format!("loading {name} (put it next to bfdb.exe or set opusDllPath)")
                    })?
                }
            }
        }));
        unsafe {
            Ok(Self {
                create: lib.get(b"opus_encoder_create\0").context("opus_encoder_create")?,
                encode: lib.get(b"opus_encode\0").context("opus_encode")?,
                destroy: lib.get(b"opus_encoder_destroy\0").context("opus_encoder_destroy")?,
                dec_create: lib.get(b"opus_decoder_create\0").context("opus_decoder_create")?,
                decode: lib.get(b"opus_decode\0").context("opus_decode")?,
                dec_destroy: lib.get(b"opus_decoder_destroy\0").context("opus_decoder_destroy")?,
            })
        }
    }

    /// Decode a sequence of Opus frames (one transmission) into 16 kHz mono
    /// `i16` PCM.
    pub(crate) fn decode_transmission(&self, frames: &[Vec<u8>]) -> Result<Vec<i16>> {
        let mut err = 0i32;
        let dec = unsafe { (self.dec_create)(16_000, 1, &mut err) };
        if dec.is_null() || err != 0 {
            anyhow::bail!("opus_decoder_create failed ({err})");
        }
        let result = (|| -> Result<Vec<i16>> {
            let mut pcm: Vec<i16> = Vec::with_capacity(frames.len() * FRAME_SAMPLES);
            let mut out = [0i16; FRAME_SAMPLES * 6]; // room for up to 240 ms
            for f in frames {
                let n = unsafe {
                    (self.decode)(
                        dec,
                        f.as_ptr(),
                        f.len() as i32,
                        out.as_mut_ptr(),
                        out.len() as i32,
                        0,
                    )
                };
                if n > 0 {
                    pcm.extend_from_slice(&out[..n as usize]);
                }
            }
            Ok(pcm)
        })();
        unsafe { (self.dec_destroy)(dec) };
        result
    }

    /// Encode one full transmission of 16 kHz mono `i16` PCM into a sequence of
    /// Opus frame byte-buffers (one per 40 ms).
    fn encode_transmission(&self, pcm: &[i16]) -> Result<Vec<Vec<u8>>> {
        let mut err = 0i32;
        let enc = unsafe { (self.create)(16_000, 1, 2048 /* OPUS_APPLICATION_VOIP */, &mut err) };
        if enc.is_null() || err != 0 {
            anyhow::bail!("opus_encoder_create failed ({err})");
        }
        let result = (|| -> Result<Vec<Vec<u8>>> {
            let mut frames = Vec::with_capacity(pcm.len() / FRAME_SAMPLES + 1);
            let mut buf = [0u8; 4000];
            let mut frame = [0i16; FRAME_SAMPLES];
            let mut i = 0;
            while i < pcm.len() {
                let n = (pcm.len() - i).min(FRAME_SAMPLES);
                frame[..n].copy_from_slice(&pcm[i..i + n]);
                frame[n..].fill(0);
                let len = unsafe {
                    (self.encode)(
                        enc,
                        frame.as_ptr(),
                        FRAME_SAMPLES as i32,
                        buf.as_mut_ptr(),
                        buf.len() as i32,
                    )
                };
                if len < 0 {
                    anyhow::bail!("opus_encode failed ({len})");
                }
                frames.push(buf[..len as usize].to_vec());
                i += FRAME_SAMPLES;
            }
            Ok(frames)
        })();
        unsafe { (self.destroy)(enc) };
        result
    }
}

// ─── Client ───────────────────────────────────────────────────────────────

struct Inner {
    guid: [u8; GUID_LEN],
    name: String,
    coalition: u8, // 1 = red, 2 = blue
    /// Every frequency this client controls. Mutable at runtime: ATC gains and
    /// loses airfields as the campaign front moves, and `radios_dirty` makes the
    /// connection re-send a RadioUpdate when it changes.
    radios: Mutex<Vec<Radio>>,
    radios_dirty: AtomicBool,
    eam_password: String,
    addr: String, // host:port
    udp: UdpSocket,
    connected: AtomicBool,
    packet_id: AtomicU64,
    /// Frequency (whole Hz) → epoch-millis of the last voice packet heard on
    /// it (someone else keying up). Used for listen-before-transmit, per
    /// frequency: a busy tower channel must not gag the GCI net.
    last_voice_rx: Mutex<HashMap<u64, u64>>,
    /// SRS client GUID → what we know about that client, from the TCP
    /// Sync/Update stream.
    clients: Mutex<HashMap<String, ClientInfo>>,
    /// Where completed inbound transmissions go (Phase 3 speech recognition).
    /// `None` disables the receive path.
    rx_tx: Option<UnboundedSender<Transmission>>,
    /// `None` when opus.dll could not be loaded — the client still connects
    /// (so EAM auth can be verified in the SRS client list) but cannot transmit.
    opus: Option<Arc<Opus>>,
}

fn now_millis() -> u64 {
    std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .map(|d| d.as_millis() as u64)
        .unwrap_or(0)
}

#[derive(Clone)]
pub(crate) struct SrsClient(Arc<Inner>);

impl SrsClient {
    /// Create a client and start its background connection manager. Never
    /// blocks on the network — if SRS is down the manager keeps retrying.
    pub(crate) fn start(
        host: &str,
        port: u16,
        coalition: u8,
        name: &str,
        radios: Vec<Radio>,
        eam_password: &str,
        opus: Option<Arc<Opus>>,
        rx_tx: Option<UnboundedSender<Transmission>>,
    ) -> Result<Self> {
        if radios.is_empty() {
            anyhow::bail!("SRS client '{name}' needs at least one radio");
        }
        let addr = format!("{host}:{port}");
        let udp = UdpSocket::bind("0.0.0.0:0").context("binding voice UDP socket")?;
        udp.connect(&addr).with_context(|| format!("connecting UDP to {addr}"))?;
        let inner = Arc::new(Inner {
            guid: new_guid(),
            name: name.to_string(),
            coalition,
            radios: Mutex::new(radios),
            radios_dirty: AtomicBool::new(false),
            eam_password: eam_password.to_string(),
            addr,
            udp,
            connected: AtomicBool::new(false),
            packet_id: AtomicU64::new(1),
            last_voice_rx: Mutex::new(HashMap::new()),
            clients: Mutex::new(HashMap::new()),
            rx_tx,
            opus,
        });
        let mgr = Arc::clone(&inner);
        std::thread::Builder::new()
            .name(format!("voice-srs-{name}"))
            .spawn(move || manager_loop(mgr))
            .context("spawning SRS manager thread")?;
        let rx = Arc::clone(&inner);
        std::thread::Builder::new()
            .name(format!("voice-srs-rx-{name}"))
            .spawn(move || udp_rx_loop(rx))
            .context("spawning SRS receive thread")?;
        Ok(SrsClient(inner))
    }

    pub(crate) fn is_connected(&self) -> bool {
        self.0.connected.load(Ordering::Relaxed)
    }

    /// Retune the client. Used when the set of positions changes — an airfield
    /// captured, a JTAC net opened. The live connection re-sends a RadioUpdate
    /// on its next tick.
    pub(crate) fn set_radios(&self, radios: Vec<Radio>) {
        if radios.is_empty() {
            return;
        }
        let mut cur = self.0.radios.lock().unwrap();
        let changed = cur.len() != radios.len()
            || cur
                .iter()
                .zip(&radios)
                .any(|(a, b)| a.key() != b.key() || a.modulation != b.modulation);
        if changed {
            *cur = radios;
            self.0.radios_dirty.store(true, Ordering::Relaxed);
        }
    }

    /// Is someone else transmitting on `freq_hz` right now?
    pub(crate) fn channel_busy(&self, freq_hz: f64) -> bool {
        let key = freq_hz.max(0.0) as u64;
        let last = self
            .0
            .last_voice_rx
            .lock()
            .unwrap()
            .get(&key)
            .copied()
            .unwrap_or(0);
        last != 0 && now_millis().saturating_sub(last) < 400
    }

    /// Transmit one utterance simultaneously on every frequency in `freqs`.
    ///
    /// A single SRS voice packet carries a list of frequencies — the same
    /// mechanism a player transmitting on two radios at once uses — so a
    /// controller can be heard on UHF, VHF and FM from one transmission rather
    /// than saying everything three times. Blocking (paces packets in real
    /// time); call from `spawn_blocking`.
    pub(crate) fn transmit(&self, freqs: &[Radio], pcm_16k_mono: &[i16]) -> Result<()> {
        let c = &self.0;
        if freqs.is_empty() {
            anyhow::bail!("transmit called with no frequency");
        }
        if !c.connected.load(Ordering::Relaxed) {
            anyhow::bail!("SRS client '{}' is not connected", c.name);
        }
        let Some(opus) = c.opus.as_ref() else {
            anyhow::bail!("opus.dll not loaded — cannot transmit");
        };

        // Listen before transmit: don't step on a player who is keying up. Any
        // of our frequencies being busy holds the whole transmission, since it
        // goes out on all of them at once.
        let wait_start = Instant::now();
        while freqs.iter().any(|r| self.channel_busy(r.freq_hz))
            && wait_start.elapsed() < Duration::from_secs(10)
        {
            std::thread::sleep(Duration::from_millis(120));
        }
        if wait_start.elapsed() > Duration::from_millis(200) {
            // someone just finished — leave a short courtesy gap
            std::thread::sleep(Duration::from_millis(400));
        }

        let frames = opus.encode_transmission(pcm_16k_mono)?;
        let start = Instant::now();
        for (i, opus_frame) in frames.iter().enumerate() {
            let pid = c.packet_id.fetch_add(1, Ordering::Relaxed);
            let pkt = c.encode_voice_packet(opus_frame, pid, freqs);
            // Pace: send frame i at start + i*40ms - 20ms (half a frame early),
            // matching the SRS reference client so the server doesn't skip audio.
            let target = start + Duration::from_millis(i as u64 * FRAME_MS);
            let target = target.checked_sub(Duration::from_millis(FRAME_MS / 2)).unwrap_or(target);
            let now = Instant::now();
            if target > now {
                std::thread::sleep(target - now);
            }
            c.udp.send(&pkt).context("sending voice packet")?;
        }
        Ok(())
    }
}

impl Inner {
    /// "251.000 AM Magic, 260.000 AM Batumi Tower" — for the connection logs.
    fn describe_radios(&self) -> String {
        let radios = self.radios.lock().unwrap();
        radios
            .iter()
            .map(|r| {
                format!(
                    "{:.3} {} {}",
                    r.freq_hz / 1e6,
                    if r.modulation == 1 { "FM" } else { "AM" },
                    r.label
                )
            })
            .collect::<Vec<_>>()
            .join(", ")
    }

    fn guid_str(&self) -> &str {
        // GUID bytes are always printable ASCII (see new_guid).
        std::str::from_utf8(&self.guid).unwrap_or("")
    }

    fn client_info(&self) -> serde_json::Value {
        // SRS clients carry a fixed bank of radios; slot 0 is the intercom in a
        // real cockpit, so ours start at 1 and the bank is padded out to the
        // protocol's 11 entries. Positions beyond that still transmit and are
        // still heard (the receive path filters on our own radio list), they
        // just don't show in another client's radio overlay.
        let radios = self.radios.lock().unwrap();
        let mut bank: Vec<serde_json::Value> = Vec::with_capacity(RADIO_SLOTS);
        bank.push(json!({
            "freq": 100.0, "modulation": 3, "enc": false, "encKey": 0,
            "secFreq": 0.0, "retransmit": false
        }));
        for r in radios.iter().take(RADIO_SLOTS - 1) {
            bank.push(json!({
                "freq": r.freq_hz,
                "modulation": r.modulation,
                "name": r.label,
                "enc": false,
                "encKey": 0,
                "secFreq": 0.0,
                "retransmit": false
            }));
        }
        while bank.len() < RADIO_SLOTS {
            bank.push(json!({
                "freq": 1.0, "modulation": 3, "enc": false, "encKey": 0,
                "secFreq": 0.0, "retransmit": false
            }));
        }
        json!({
            "ClientGuid": self.guid_str(),
            "Name": self.name,
            "Seat": 0,
            "Coalition": self.coalition,
            "AllowRecord": true,
            "RadioInfo": {
                "radios": bank,
                "unit": "GCI",
                "unitId": UNIT_ID,
                "iff": { "control": 2, "status": 0, "mode1": -1, "mode2": -1, "mode3": -1, "mode4": false, "mic": -1 },
                "ambient": { "vol": 1.0, "abType": "" }
            },
            "LatLngPosition": { "lat": 0.0, "lng": 0.0, "alt": 0.0 }
        })
    }

    fn message(&self, msg_type: i64, eam_password: Option<&str>) -> String {
        let mut m = json!({
            "Version": SRS_VERSION,
            "Client": self.client_info(),
            "MsgType": msg_type,
        });
        if let Some(pw) = eam_password {
            m["ExternalAWACSModePassword"] = json!(pw);
        }
        let mut s = serde_json::to_string(&m).unwrap_or_default();
        s.push('\n');
        s
    }

    /// Serialize a UDP voice packet per the SRS UDPVoicePacket layout. The
    /// frequency segment is a repeated 10-byte record, so one packet can be
    /// heard on several radios at once.
    fn encode_voice_packet(&self, opus_frame: &[u8], packet_id: u64, freqs: &[Radio]) -> Vec<u8> {
        let audio_len = opus_frame.len();
        let freq_len = 10usize * freqs.len();
        let fixed_len = 4 + 8 + 1 + GUID_LEN + GUID_LEN; // 57
        let total = 6 + audio_len + freq_len + fixed_len;
        let mut b = vec![0u8; total];

        // Header
        b[0..2].copy_from_slice(&(total as u16).to_le_bytes());
        b[2..4].copy_from_slice(&(audio_len as u16).to_le_bytes());
        b[4..6].copy_from_slice(&(freq_len as u16).to_le_bytes());
        // Audio
        b[6..6 + audio_len].copy_from_slice(opus_frame);
        // Frequency segment — one 10-byte record per radio.
        let fo = 6 + audio_len;
        for (i, r) in freqs.iter().enumerate() {
            let o = fo + i * 10;
            b[o..o + 8].copy_from_slice(&r.freq_hz.to_le_bytes());
            b[o + 8] = r.modulation;
            b[o + 9] = 0; // encryption
        }
        // Fixed segment
        let so = fo + freq_len; // == total - 57
        b[so..so + 4].copy_from_slice(&UNIT_ID.to_le_bytes());
        b[so + 4..so + 12].copy_from_slice(&packet_id.to_le_bytes());
        b[total - 45] = 0; // retransmission count
        b[total - 44..total - 22].copy_from_slice(&self.guid); // original transmitter
        b[total - 22..total].copy_from_slice(&self.guid); // this client
        b
    }
}

struct RxBuf {
    who: [u8; GUID_LEN],
    freq_hz: f64,
    label: String,
    frames: Vec<(u64, Vec<u8>)>,
    deadline: Instant,
}

/// A transmission we just closed out, kept briefly so a follow-on burst from
/// the same client can be stitched onto it instead of arriving as an orphan.
struct RxTail {
    who: [u8; GUID_LEN],
    freq_hz: f64,
    label: String,
    frames: Vec<(u64, Vec<u8>)>,
    expires: Instant,
}

/// Receive loop: inbound UDP voice packets on our frequency drive
/// `channel_busy()` (listen-before-transmit) and, when `rx_tx` is set, are
/// buffered per-transmission, Opus-decoded, and handed off for speech
/// recognition. Runs for the life of the client.
fn udp_rx_loop(c: Arc<Inner>) {
    let _ = c.udp.set_read_timeout(Some(Duration::from_millis(120)));
    let mut buf = [0u8; 1500];
    let mut rxb: Option<RxBuf> = None;
    let mut tail: Option<RxTail> = None;
    loop {
        if let Some(b) = &rxb {
            if Instant::now() >= b.deadline {
                let b = rxb.take().unwrap();
                tail = finish_rx(&c, b);
            }
        }
        // Nothing followed the held fragment — it was the whole call after all.
        if tail.as_ref().is_some_and(|t| Instant::now() >= t.expires) {
            let t = tail.take().unwrap();
            emit_rx(&c, &t.who, t.freq_hz, &t.label, t.frames);
        }
        let n = match c.udp.recv(&mut buf) {
            Ok(n) => n,
            Err(e)
                if matches!(
                    e.kind(),
                    std::io::ErrorKind::WouldBlock | std::io::ErrorKind::TimedOut
                ) =>
            {
                continue
            }
            Err(_) => {
                std::thread::sleep(Duration::from_millis(200));
                continue;
            }
        };
        if n <= GUID_LEN + 6 {
            continue; // ping
        }
        let pkt = &buf[..n];
        let audio_len = u16::from_le_bytes([pkt[2], pkt[3]]) as usize;
        let freq_len = u16::from_le_bytes([pkt[4], pkt[5]]) as usize;
        let fo = 6 + audio_len;
        if fo + freq_len > n || freq_len % 10 != 0 || fo + freq_len + 12 > n {
            continue;
        }
        if &pkt[n - GUID_LEN..n] == c.guid {
            continue; // our own audio
        }
        // Which of our radios is this on? A packet can name several
        // frequencies (a player transmitting on two radios at once).
        let mut hit: Option<(f64, String)> = None;
        {
            let radios = c.radios.lock().unwrap();
            'outer: for f in pkt[fo..fo + freq_len].chunks_exact(10) {
                let hz = f64::from_le_bytes(f[0..8].try_into().unwrap_or([0; 8]));
                for r in radios.iter() {
                    if r.matches(hz, f[8]) {
                        hit = Some((r.freq_hz, r.label.clone()));
                        break 'outer;
                    }
                }
            }
        }
        let Some((freq_hz, label)) = hit else {
            continue;
        };
        c.last_voice_rx
            .lock()
            .unwrap()
            .insert(freq_hz.max(0.0) as u64, now_millis());

        if c.rx_tx.is_none() {
            continue;
        }
        let who: [u8; GUID_LEN] = pkt[n - GUID_LEN..n].try_into().unwrap();
        let pid = u64::from_le_bytes(
            pkt[fo + freq_len + 4..fo + freq_len + 12].try_into().unwrap(),
        );
        let opus = pkt[6..6 + audio_len].to_vec();
        let dl = Instant::now() + RX_END_GAP;
        match &mut rxb {
            Some(b) if b.who == who => {
                b.frames.push((pid, opus));
                b.deadline = dl;
            }
            Some(_) => {} // another origin already talking — one at a time
            None => {
                // Resuming after a think-pause? Carry the earlier audio over so
                // the whole call is transcribed as one utterance.
                let carried = match tail.take() {
                    Some(t) if t.who == who && (t.freq_hz - freq_hz).abs() <= 500.0 => t.frames,
                    other => {
                        tail = other;
                        Vec::new()
                    }
                };
                let mut frames = carried;
                frames.push((pid, opus));
                rxb = Some(RxBuf {
                    who,
                    freq_hz,
                    label,
                    frames,
                    deadline: dl,
                });
            }
        }
    }
}

/// A transmission at least this long is treated as a complete thought and goes
/// straight to recognition. Anything shorter is held back for
/// [`RX_STITCH_WINDOW`] first, in case it is only the front half of a call the
/// player is still making — answering "Magic..." on its own helps nobody.
const RX_SELF_CONTAINED: usize = 38; // ~1.5 s

/// Close out a buffered transmission. Returns a tail to hold when the audio was
/// too short to be sure it was the whole call.
fn finish_rx(c: &Inner, b: RxBuf) -> Option<RxTail> {
    if c.rx_tx.is_none() || c.opus.is_none() {
        return None;
    }
    if b.frames.len() >= RX_SELF_CONTAINED {
        emit_rx(c, &b.who, b.freq_hz, &b.label, b.frames);
        return None;
    }
    Some(RxTail {
        who: b.who,
        freq_hz: b.freq_hz,
        label: b.label,
        frames: b.frames,
        expires: Instant::now() + RX_STITCH_WINDOW,
    })
}

/// Decode a completed transmission and hand it to speech recognition.
fn emit_rx(
    c: &Inner,
    who: &[u8; GUID_LEN],
    freq_hz: f64,
    label: &str,
    mut frames: Vec<(u64, Vec<u8>)>,
) {
    let (Some(tx), Some(opus)) = (&c.rx_tx, c.opus.as_ref()) else {
        return;
    };
    if frames.len() < RX_MIN_FRAMES {
        log::debug!(
            "voice srs[{}]: ignoring {:.1}s transmission on {label} (too short)",
            c.name,
            frames.len() as f64 * FRAME_MS as f64 / 1000.0
        );
        return;
    }
    frames.sort_by_key(|(id, _)| *id);
    let opus_frames: Vec<Vec<u8>> = frames.into_iter().map(|(_, f)| f).collect();
    let pcm = match opus.decode_transmission(&opus_frames) {
        Ok(p) => p,
        Err(e) => {
            log::debug!("voice srs[{}]: rx decode failed: {e}", c.name);
            return;
        }
    };
    let guid = std::str::from_utf8(who).unwrap_or("").to_string();
    let info = c.clients.lock().unwrap().get(&guid).cloned();
    let (name, unit_id) = match info {
        Some(i) => (i.name, i.unit_id),
        None => (String::new(), None),
    };
    log::info!(
        "voice srs[{}]: inbound on {label} from '{}'{} ({:.1}s)",
        c.name,
        if name.is_empty() { "unknown" } else { &name },
        match unit_id {
            Some(u) => format!(" unit {u}"),
            None => String::new(),
        },
        pcm.len() as f64 / 16_000.0
    );
    let _ = tx.send(Transmission {
        coalition: c.coalition,
        name,
        guid,
        unit_id,
        freq_hz,
        label: label.to_string(),
        pcm,
    });
}

/// Learn what we can about the other SRS clients from a TCP protocol message.
///
/// `RadioInfo.unitId` is the DCS unit id of the jet the player is sitting in —
/// an exact key back to the flight in the engine's picture, and far more
/// reliable than matching the SRS display name against the DCS pilot name.
/// Players in an external (non-DCS) SRS client have no unit, hence the option.
fn learn_clients(c: &Inner, v: &serde_json::Value) {
    let mut map = c.clients.lock().unwrap();
    let mut ins = |cl: &serde_json::Value| {
        if let (Some(g), Some(nm)) = (
            cl.get("ClientGuid").and_then(|x| x.as_str()),
            cl.get("Name").and_then(|x| x.as_str()),
        ) {
            let unit_id = cl
                .get("RadioInfo")
                .and_then(|r| r.get("unitId"))
                .and_then(|u| u.as_u64())
                .filter(|u| *u > 0 && *u != UNIT_ID as u64);
            map.insert(
                g.to_string(),
                ClientInfo {
                    name: nm.to_string(),
                    unit_id,
                },
            );
        }
    };
    if let Some(cl) = v.get("Client") {
        ins(cl);
    }
    if let Some(arr) = v.get("Clients").and_then(|x| x.as_array()) {
        for cl in arr {
            ins(cl);
        }
    }
}

/// Connect + handshake + serve until the connection drops, then return so the
/// manager can retry.
fn serve_once(c: &Inner) -> Result<()> {
    let mut tcp = TcpStream::connect(&c.addr).with_context(|| format!("connecting TCP to {}", c.addr))?;
    tcp.set_nodelay(true).ok();
    tcp.set_read_timeout(Some(Duration::from_secs(1))).ok();

    tcp.write_all(c.message(MSG_SYNC, None).as_bytes())?;
    tcp.write_all(c.message(MSG_EAM_PASSWORD, Some(&c.eam_password)).as_bytes())?;
    tcp.write_all(c.message(MSG_RADIO_UPDATE, None).as_bytes())?;
    tcp.write_all(c.message(MSG_PING, None).as_bytes())?;
    c.udp.send(&c.guid).ok();

    c.connected.store(true, Ordering::Relaxed);
    c.radios_dirty.store(false, Ordering::Relaxed);
    log::info!(
        "voice srs[{}]: connected to {} ({} coalition), {}",
        c.name,
        c.addr,
        if c.coalition == 1 { "red" } else { "blue" },
        c.describe_radios(),
    );

    let mut rd = [0u8; 8192];
    let mut linebuf: Vec<u8> = Vec::with_capacity(16384);
    let mut last_ping = Instant::now();
    let mut last_rx = Instant::now();
    // A quiet DCS-SRS server sends an idle External-AWACS client nothing for
    // long stretches (no SYNC, no radio updates), so "no inbound bytes" is a
    // poor liveness signal on its own. A dead connection shows up as a failed
    // ping write or a read error/EOF instead; keep this only as a long
    // backstop for a silently half-open socket.
    // A near-empty server genuinely sends an idle EAM client nothing for
    // minutes at a time. 150s was tripping a reconnect storm every few
    // minutes on a quiet server (each reconnect briefly churns 10054/10061).
    // A dead socket surfaces as a failed PING write within tens of seconds
    // regardless, so this only needs to be a long backstop for a silently
    // half-open connection.
    const IDLE_BAIL: Duration = Duration::from_secs(600);
    loop {
        match tcp.read(&mut rd) {
            Ok(0) => anyhow::bail!("SRS server closed the connection"),
            Ok(n) => {
                last_rx = Instant::now();
                linebuf.extend_from_slice(&rd[..n]);
                while let Some(p) = linebuf.iter().position(|&b| b == b'\n') {
                    let line: Vec<u8> = linebuf.drain(..=p).collect();
                    if let Ok(v) = serde_json::from_slice::<serde_json::Value>(&line[..line.len() - 1])
                    {
                        if v.get("MsgType").and_then(|t| t.as_i64()) == Some(MSG_VERSION_MISMATCH) {
                            anyhow::bail!("SRS server reported a version mismatch");
                        }
                        learn_clients(c, &v);
                    }
                }
                if linebuf.len() > 1 << 20 {
                    linebuf.clear(); // runaway guard
                }
            }
            Err(e)
                if matches!(
                    e.kind(),
                    std::io::ErrorKind::WouldBlock | std::io::ErrorKind::TimedOut
                ) => {}
            Err(e) => return Err(e).context("reading from SRS server"),
        }

        // Retuned since we connected (an airfield changed hands, say) — tell
        // the server which frequencies we are listening on now.
        if c.radios_dirty.swap(false, Ordering::Relaxed) {
            log::info!("voice srs[{}]: retuning — {}", c.name, c.describe_radios());
            tcp.write_all(c.message(MSG_RADIO_UPDATE, None).as_bytes())
                .context("SRS radio update write failed")?;
        }

        let now = Instant::now();
        if now.duration_since(last_ping) >= Duration::from_secs(15) {
            // A failed write here is the real "connection is dead" signal.
            tcp.write_all(c.message(MSG_PING, None).as_bytes())
                .context("SRS keepalive write failed")?;
            c.udp.send(&c.guid).ok();
            last_ping = now;
        }
        if now.duration_since(last_rx) >= IDLE_BAIL {
            anyhow::bail!("no traffic from SRS server for {}s", IDLE_BAIL.as_secs());
        }
    }
}

fn manager_loop(c: Arc<Inner>) {
    loop {
        if let Err(e) = serve_once(&c) {
            log::warn!("voice srs[{}]: {e:#}", c.name);
        }
        c.connected.store(false, Ordering::Relaxed);
        std::thread::sleep(Duration::from_secs(5));
    }
}

// ─── helpers ──────────────────────────────────────────────────────────────

/// A 22-character base-57 identifier, matching SRS's shortuuid GUID shape.
fn new_guid() -> [u8; GUID_LEN] {
    // base57 alphabet (no 0/O/1/I/l) — SRS treats the GUID as an opaque ASCII
    // blob, so the exact alphabet only needs to be printable and well-spread.
    const ALPHA: &[u8] = b"23456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz";
    let mut g = [0u8; GUID_LEN];
    let mut seed = {
        let t = std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .map(|d| d.as_nanos())
            .unwrap_or(0);
        (t as u64) ^ (std::process::id() as u64).wrapping_mul(0x9E37_79B9_7F4A_7C15)
    };
    for b in &mut g {
        // xorshift64*
        seed ^= seed >> 12;
        seed ^= seed << 25;
        seed ^= seed >> 27;
        let r = seed.wrapping_mul(0x2545_F491_4F6C_DD1D);
        *b = ALPHA[(r % ALPHA.len() as u64) as usize];
    }
    g
}
