// Copyright (c) 2026 Dillen Weerasinghe. All rights reserved.
// Proprietary and confidential — no license granted. See the repository NOTICE.

//! Persistent DCS-SimpleRadio-Standalone voice client — transmit only, one
//! per coalition. Stays connected (visible in the SRS client list) and
//! authenticates via External AWACS Mode.
//!
//! Protocol references (both MIT):
//!   - ciribob/DCS-SimpleRadioStandalone  Common/Models/{NetworkMessage,UDPVoicePacket}.cs
//!   - dharmab/skyeye                      pkg/simpleradio/*
//!
//! Opus encoding is done through `opus.dll` (the copy that ships with DCS-SRS),
//! loaded at runtime — there is no build-time Opus/CMake dependency.

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

/// A completed inbound transmission on the GCI frequency — a player keying up.
pub(crate) struct Transmission {
    /// 1 = red, 2 = blue (which GCI client received it).
    pub coalition: u8,
    /// SRS client name of the transmitter (matched to a pilot downstream).
    pub name: String,
    /// 16 kHz mono PCM.
    pub pcm: Vec<i16>,
}

const GUID_LEN: usize = 22;
const SRS_VERSION: &str = "2.1.0.2";
const UNIT_ID: u32 = 100_000_002;
const FRAME_SAMPLES: usize = 640; // 40 ms @ 16 kHz mono
const FRAME_MS: u64 = 40;

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
    freq_hz: f64,
    modulation: u8, // 0 = AM, 1 = FM
    eam_password: String,
    addr: String, // host:port
    udp: UdpSocket,
    connected: AtomicBool,
    packet_id: AtomicU64,
    /// Epoch-millis of the last voice packet heard on our frequency (someone
    /// else keyed up). 0 = never. Used for listen-before-transmit.
    last_voice_rx: AtomicU64,
    /// SRS client GUID → name, from the TCP Sync/Update stream.
    clients: Mutex<HashMap<String, String>>,
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
        freq_hz: f64,
        modulation: u8,
        eam_password: &str,
        opus: Option<Arc<Opus>>,
        rx_tx: Option<UnboundedSender<Transmission>>,
    ) -> Result<Self> {
        let addr = format!("{host}:{port}");
        let udp = UdpSocket::bind("0.0.0.0:0").context("binding GCI UDP socket")?;
        udp.connect(&addr).with_context(|| format!("connecting UDP to {addr}"))?;
        let inner = Arc::new(Inner {
            guid: new_guid(),
            name: name.to_string(),
            coalition,
            freq_hz,
            modulation,
            eam_password: eam_password.to_string(),
            addr,
            udp,
            connected: AtomicBool::new(false),
            packet_id: AtomicU64::new(1),
            last_voice_rx: AtomicU64::new(0),
            clients: Mutex::new(HashMap::new()),
            rx_tx,
            opus,
        });
        let mgr = Arc::clone(&inner);
        std::thread::Builder::new()
            .name(format!("gci-srs-{name}"))
            .spawn(move || manager_loop(mgr))
            .context("spawning SRS manager thread")?;
        let rx = Arc::clone(&inner);
        std::thread::Builder::new()
            .name(format!("gci-srs-rx-{name}"))
            .spawn(move || udp_rx_loop(rx))
            .context("spawning SRS receive thread")?;
        Ok(SrsClient(inner))
    }

    pub(crate) fn is_connected(&self) -> bool {
        self.0.connected.load(Ordering::Relaxed)
    }

    /// Is someone else transmitting on our frequency right now?
    pub(crate) fn channel_busy(&self) -> bool {
        let last = self.0.last_voice_rx.load(Ordering::Relaxed);
        last != 0 && now_millis().saturating_sub(last) < 400
    }

    /// Transmit one utterance. Blocking (paces packets in real time); call from
    /// `spawn_blocking`.
    pub(crate) fn transmit(&self, pcm_16k_mono: &[i16]) -> Result<()> {
        let c = &self.0;
        if !c.connected.load(Ordering::Relaxed) {
            anyhow::bail!("SRS client '{}' is not connected", c.name);
        }
        let Some(opus) = c.opus.as_ref() else {
            anyhow::bail!("opus.dll not loaded — cannot transmit");
        };

        // Listen before transmit: don't step on a player who is keying up.
        let wait_start = Instant::now();
        while self.channel_busy() && wait_start.elapsed() < Duration::from_secs(10) {
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
            let pkt = c.encode_voice_packet(opus_frame, pid);
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
    fn guid_str(&self) -> &str {
        // GUID bytes are always printable ASCII (see new_guid).
        std::str::from_utf8(&self.guid).unwrap_or("")
    }

    fn client_info(&self) -> serde_json::Value {
        json!({
            "ClientGuid": self.guid_str(),
            "Name": self.name,
            "Seat": 0,
            "Coalition": self.coalition,
            "AllowRecord": true,
            "RadioInfo": {
                "radios": [{
                    "freq": self.freq_hz,
                    "modulation": self.modulation,
                    "enc": false,
                    "encKey": 0,
                    "secFreq": 0.0,
                    "retransmit": false
                }],
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

    /// Serialize a UDP voice packet per the SRS UDPVoicePacket layout.
    fn encode_voice_packet(&self, opus_frame: &[u8], packet_id: u64) -> Vec<u8> {
        let audio_len = opus_frame.len();
        let freq_len = 10usize; // one frequency entry
        let fixed_len = 4 + 8 + 1 + GUID_LEN + GUID_LEN; // 57
        let total = 6 + audio_len + freq_len + fixed_len;
        let mut b = vec![0u8; total];

        // Header
        b[0..2].copy_from_slice(&(total as u16).to_le_bytes());
        b[2..4].copy_from_slice(&(audio_len as u16).to_le_bytes());
        b[4..6].copy_from_slice(&(freq_len as u16).to_le_bytes());
        // Audio
        b[6..6 + audio_len].copy_from_slice(opus_frame);
        // Frequency segment
        let fo = 6 + audio_len;
        b[fo..fo + 8].copy_from_slice(&self.freq_hz.to_le_bytes());
        b[fo + 8] = self.modulation;
        b[fo + 9] = 0; // encryption
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
    frames: Vec<(u64, Vec<u8>)>,
    deadline: Instant,
}

/// Receive loop: inbound UDP voice packets on our frequency drive
/// `channel_busy()` (listen-before-transmit) and, when `rx_tx` is set, are
/// buffered per-transmission, Opus-decoded, and handed off for speech
/// recognition. Runs for the life of the client.
fn udp_rx_loop(c: Arc<Inner>) {
    let _ = c.udp.set_read_timeout(Some(Duration::from_millis(120)));
    let mut buf = [0u8; 1500];
    let mut rxb: Option<RxBuf> = None;
    loop {
        if let Some(b) = &rxb {
            if Instant::now() >= b.deadline {
                let b = rxb.take().unwrap();
                finish_rx(&c, b);
            }
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
        let mut on_freq = false;
        for f in pkt[fo..fo + freq_len].chunks_exact(10) {
            let hz = f64::from_le_bytes(f[0..8].try_into().unwrap_or([0; 8]));
            if f[8] == c.modulation && (hz - c.freq_hz).abs() <= 500.0 {
                on_freq = true;
                break;
            }
        }
        if !on_freq {
            continue;
        }
        c.last_voice_rx.store(now_millis(), Ordering::Relaxed);

        if c.rx_tx.is_none() {
            continue;
        }
        let who: [u8; GUID_LEN] = pkt[n - GUID_LEN..n].try_into().unwrap();
        let pid = u64::from_le_bytes(
            pkt[fo + freq_len + 4..fo + freq_len + 12].try_into().unwrap(),
        );
        let opus = pkt[6..6 + audio_len].to_vec();
        let dl = Instant::now() + Duration::from_millis(300);
        match &mut rxb {
            Some(b) if b.who == who => {
                b.frames.push((pid, opus));
                b.deadline = dl;
            }
            Some(_) => {} // another origin already talking — one at a time
            None => {
                rxb = Some(RxBuf {
                    who,
                    frames: vec![(pid, opus)],
                    deadline: dl,
                });
            }
        }
    }
}

fn finish_rx(c: &Inner, mut b: RxBuf) {
    let (Some(tx), Some(opus)) = (&c.rx_tx, c.opus.as_ref()) else {
        return;
    };
    // < ~1.2 s: too short for reliable recognition.
    if b.frames.len() < 30 {
        return;
    }
    b.frames.sort_by_key(|(id, _)| *id);
    let frames: Vec<Vec<u8>> = b.frames.into_iter().map(|(_, f)| f).collect();
    let pcm = match opus.decode_transmission(&frames) {
        Ok(p) => p,
        Err(e) => {
            log::debug!("gci srs[{}]: rx decode failed: {e}", c.name);
            return;
        }
    };
    let name = std::str::from_utf8(&b.who)
        .ok()
        .and_then(|g| c.clients.lock().unwrap().get(g).cloned())
        .unwrap_or_default();
    log::info!(
        "gci srs[{}]: inbound transmission from '{}' ({:.1}s)",
        c.name,
        if name.is_empty() { "unknown" } else { &name },
        pcm.len() as f64 / 16_000.0
    );
    let _ = tx.send(Transmission {
        coalition: c.coalition,
        name,
        pcm,
    });
}

/// Learn SRS client GUID→name from a TCP protocol message.
fn learn_clients(c: &Inner, v: &serde_json::Value) {
    let mut map = c.clients.lock().unwrap();
    let mut ins = |cl: &serde_json::Value| {
        if let (Some(g), Some(nm)) = (
            cl.get("ClientGuid").and_then(|x| x.as_str()),
            cl.get("Name").and_then(|x| x.as_str()),
        ) {
            map.insert(g.to_string(), nm.to_string());
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
    log::info!(
        "gci srs[{}]: connected to {} ({} coalition) on {:.3} MHz {}",
        c.name,
        c.addr,
        if c.coalition == 1 { "red" } else { "blue" },
        c.freq_hz / 1e6,
        if c.modulation == 1 { "FM" } else { "AM" },
    );

    let mut rd = [0u8; 8192];
    let mut linebuf: Vec<u8> = Vec::with_capacity(16384);
    let mut last_ping = Instant::now();
    let mut last_rx = Instant::now();
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

        let now = Instant::now();
        if now.duration_since(last_ping) >= Duration::from_secs(15) {
            tcp.write_all(c.message(MSG_PING, None).as_bytes())
                .context("sending TCP keepalive")?;
            c.udp.send(&c.guid).ok();
            last_ping = now;
        }
        if now.duration_since(last_rx) >= Duration::from_secs(45) {
            anyhow::bail!("no traffic from SRS server for 45s");
        }
    }
}

fn manager_loop(c: Arc<Inner>) {
    loop {
        if let Err(e) = serve_once(&c) {
            log::warn!("gci srs[{}]: {e:#}", c.name);
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
