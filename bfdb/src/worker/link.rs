//! The worker link's wire format and the request/reply plumbing both ends
//! share.
//!
//! One WebSocket, opened by the worker to bfdb's loopback worker listener.
//! Text frames are JSON [`Msg`]s; binary frames are live range-ATC audio
//! ([`audio_frame`]). Either side may send a request ([`Msg::Rpc`] from the
//! worker, [`Msg::Req`] from bfdb) and gets a [`Msg::Reply`] with the same id.

use crate::{news::NewsDigest, news_llm::Voice};
use serde_derive::{Deserialize, Serialize};
use std::{
    collections::HashMap,
    sync::{
        atomic::{AtomicU64, Ordering},
        Arc, Mutex,
    },
    time::Duration,
};
use tokio::sync::{mpsc, oneshot};

/// Bumped when a message changes shape; both ends refuse a peer that differs.
pub(crate) const PROTOCOL: u32 = 1;

/// Frames queued for the socket. Live audio is dropped rather than queued
/// past this; everything else is small.
const OUT_QUEUE: usize = 4096;

#[derive(Debug, Serialize, Deserialize)]
#[serde(tag = "t", rename_all = "snake_case")]
pub(crate) enum Msg {
    /// worker -> bfdb, first: who it is and what it runs.
    Hello {
        protocol: u32,
        version: String,
        instances: Vec<String>,
        /// The war diary's model, when the worker has a writer configured.
        news_writer: Option<String>,
        /// The worker can draw the diary's pictures.
        news_draw: bool,
    },
    /// bfdb -> worker: accepted. Nothing else is sent before it.
    Welcome {
        protocol: u32,
        version: String,
    },
    /// worker -> bfdb: call an engine procedure for a voice service.
    /// `optional` is `StatsDb::call_engine_rpc_optional`, else the counted
    /// `call_engine_rpc_str`. Every argument the services pass is a string.
    Rpc {
        id: u64,
        inst: String,
        proc_name: String,
        args: Vec<(String, String)>,
        optional: bool,
    },
    /// worker -> bfdb: one GCI transcript line (the JSON /ws/gci carries).
    GciLine {
        inst: String,
        line: String,
    },
    /// worker -> bfdb, after Welcome: the transcript so far, oldest first.
    GciBacklog {
        inst: String,
        lines: Vec<String>,
    },
    /// worker -> bfdb: a range ATC transmission starts / ends on the live
    /// feed. Its audio comes as binary frames.
    LiveOn {
        inst: String,
        station: String,
        who: String,
        text: String,
    },
    LiveOff {
        inst: String,
        station: String,
    },
    /// bfdb -> worker: a request.
    Req {
        id: u64,
        op: Op,
    },
    /// Either way: the answer to `Rpc` or `Req` `id`.
    Reply {
        id: u64,
        ok: Option<serde_json::Value>,
        err: Option<RemoteErr>,
    },
}

/// What bfdb asks of the worker.
#[derive(Debug, Serialize, Deserialize)]
#[serde(tag = "op", rename_all = "snake_case")]
pub(crate) enum Op {
    /// `gci::human_call`: a dashboard controller's transmission.
    GciTransmit { inst: String, side: String, text: String },
    /// `milatc::transcript_json`.
    AtcTranscript { inst: String, since: i64 },
    /// `milatc::audio_wav`, base64 in the reply.
    AtcAudio { inst: String, id: u64 },
    /// `news_llm::write_dispatch` with the worker's writer, in `voice`. The
    /// voice defaults, so a request from an older bfdb (which sends none) is
    /// written in the default voice; an older worker ignores it and answers
    /// in the default voice, with no `voice` in its reply, which is how bfdb
    /// knows (`hub::news_write`).
    NewsWrite {
        digest: Box<NewsDigest>,
        history: Vec<NewsDigest>,
        #[serde(default)]
        voice: Voice,
    },
    /// One picture from the worker's image provider (and its fallback).
    NewsDraw { prompt: String },
}

/// A failed request, as much of it as the caller needs to act the same way it
/// would on the local error.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub(crate) struct RemoteErr {
    /// The HTTP status of a typed `websec::ApiError`; 0 for anything else.
    pub(crate) status: u16,
    pub(crate) msg: String,
    /// A news picture call that was probably billed (`GenError::billed`).
    #[serde(default)]
    pub(crate) billed: bool,
    /// A provider's 429 (`news_llm::RateLimited`, `GenError::rate_limited`).
    #[serde(default)]
    pub(crate) rate_limited: bool,
    /// How long the provider asked to wait, for the writer's gate.
    #[serde(default)]
    pub(crate) retry_secs: Option<f64>,
}

impl RemoteErr {
    pub(crate) fn new(status: u16, msg: impl Into<String>) -> Self {
        Self {
            status,
            msg: msg.into(),
            billed: false,
            rate_limited: false,
            retry_secs: None,
        }
    }

    /// The link itself failed: no worker, gone mid-call, or no answer.
    pub(crate) fn link(msg: impl Into<String>) -> Self {
        Self::new(503, msg)
    }

    /// From a handler error, keeping a typed status.
    pub(crate) fn from_anyhow(e: &anyhow::Error) -> Self {
        match e.downcast_ref::<crate::websec::ApiError>() {
            Some(a) => Self::new(a.status.as_u16(), a.msg.clone()),
            None => Self::new(0, format!("{e:#}")),
        }
    }

    /// Back into the error the local call would have returned.
    pub(crate) fn into_anyhow(self) -> anyhow::Error {
        match warp::http::StatusCode::from_u16(self.status) {
            Ok(status) if self.status != 0 => {
                anyhow::Error::new(crate::websec::ApiError { status, msg: self.msg })
            }
            _ => anyhow::anyhow!(self.msg),
        }
    }
}

impl std::fmt::Display for RemoteErr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.msg)
    }
}

/// One socket frame, whichever WebSocket library carries it.
#[derive(Debug)]
pub(crate) enum Frame {
    Text(String),
    Binary(Vec<u8>),
}

/// A live-feed audio chunk: `[1][inst len][inst][station len][station][s16le]`.
pub(crate) fn audio_frame(inst: &str, station: &str, pcm: &[i16]) -> Vec<u8> {
    let (i, s) = (
        &inst.as_bytes()[..inst.len().min(255)],
        &station.as_bytes()[..station.len().min(255)],
    );
    let mut b = Vec::with_capacity(3 + i.len() + s.len() + pcm.len() * 2);
    b.push(1);
    b.push(i.len() as u8);
    b.extend_from_slice(i);
    b.push(s.len() as u8);
    b.extend_from_slice(s);
    for x in pcm {
        b.extend_from_slice(&x.to_le_bytes());
    }
    b
}

/// [`audio_frame`] read back: (instance, station, samples).
pub(crate) fn parse_audio_frame(b: &[u8]) -> Option<(String, String, Vec<i16>)> {
    let (&kind, rest) = b.split_first()?;
    if kind != 1 {
        return None;
    }
    let (&il, rest) = rest.split_first()?;
    let (inst, rest) = rest.split_at_checked(il as usize)?;
    let (&sl, rest) = rest.split_first()?;
    let (station, pcm) = rest.split_at_checked(sl as usize)?;
    if pcm.len() % 2 != 0 {
        return None;
    }
    let pcm =
        pcm.as_chunks::<2>().0.iter().map(|c| i16::from_le_bytes([c[0], c[1]])).collect();
    Some((
        String::from_utf8(inst.to_vec()).ok()?,
        String::from_utf8(station.to_vec()).ok()?,
        pcm,
    ))
}

type Pending = HashMap<u64, oneshot::Sender<Result<serde_json::Value, RemoteErr>>>;

/// One end of a live link: the outgoing queue and the requests waiting on
/// a reply.
pub(crate) struct Peer {
    out: mpsc::Sender<Frame>,
    pending: Mutex<Pending>,
    next: AtomicU64,
}

impl Peer {
    /// A peer and the receiving end of its outgoing queue, which the socket's
    /// writer task drains.
    pub(crate) fn new() -> (Arc<Self>, mpsc::Receiver<Frame>) {
        let (out, rx) = mpsc::channel(OUT_QUEUE);
        (
            Arc::new(Self {
                out,
                pending: Mutex::new(HashMap::new()),
                next: AtomicU64::new(1),
            }),
            rx,
        )
    }

    /// Queue a message. False when the link is gone or backed up.
    pub(crate) fn send(&self, m: &Msg) -> bool {
        match serde_json::to_string(m) {
            Ok(s) => self.out.try_send(Frame::Text(s)).is_ok(),
            Err(e) => {
                log::warn!("worker link: cannot encode a message: {e}");
                false
            }
        }
    }

    /// Queue a binary frame; dropped when the link is backed up.
    pub(crate) fn send_binary(&self, b: Vec<u8>) -> bool {
        self.out.try_send(Frame::Binary(b)).is_ok()
    }

    /// Send a request built around a fresh id and wait for its reply.
    pub(crate) async fn call(
        &self,
        make: impl FnOnce(u64) -> Msg,
        timeout: Duration,
    ) -> Result<serde_json::Value, RemoteErr> {
        let id = self.next.fetch_add(1, Ordering::Relaxed);
        let (tx, rx) = oneshot::channel();
        self.pending.lock().unwrap().insert(id, tx);
        if !self.send(&make(id)) {
            self.pending.lock().unwrap().remove(&id);
            return Err(RemoteErr::link("the worker link is down"));
        }
        match tokio::time::timeout(timeout, rx).await {
            Ok(Ok(r)) => r,
            Ok(Err(_)) => Err(RemoteErr::link("the worker link closed before answering")),
            Err(_) => {
                self.pending.lock().unwrap().remove(&id);
                Err(RemoteErr::link(format!(
                    "no answer over the worker link in {}s",
                    timeout.as_secs()
                )))
            }
        }
    }

    /// Hand a reply to whoever is waiting on it.
    pub(crate) fn resolve(
        &self,
        id: u64,
        ok: Option<serde_json::Value>,
        err: Option<RemoteErr>,
    ) {
        if let Some(tx) = self.pending.lock().unwrap().remove(&id) {
            let _ = tx.send(match (ok, err) {
                (_, Some(e)) => Err(e),
                (Some(v), None) => Ok(v),
                (None, None) => Ok(serde_json::Value::Null),
            });
        }
    }

    /// Answer request `id`.
    pub(crate) fn reply(&self, id: u64, r: Result<serde_json::Value, RemoteErr>) {
        let (ok, err) = match r {
            Ok(v) => (Some(v), None),
            Err(e) => (None, Some(e)),
        };
        self.send(&Msg::Reply { id, ok, err });
    }

    /// The link is gone: every waiting request fails now, not at its timeout.
    pub(crate) fn fail_all(&self) {
        for (_, tx) in self.pending.lock().unwrap().drain() {
            let _ = tx.send(Err(RemoteErr::link("the worker link closed")));
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn audio_frames_round_trip() {
        let b = audio_frame("range", "tower-1", &[0, 1, -1, i16::MAX, i16::MIN]);
        let (i, s, pcm) = parse_audio_frame(&b).unwrap();
        assert_eq!((i.as_str(), s.as_str()), ("range", "tower-1"));
        assert_eq!(pcm, [0, 1, -1, i16::MAX, i16::MIN]);
        assert!(parse_audio_frame(&b[..b.len() - 1]).is_none(), "half a sample");
        assert!(parse_audio_frame(&[2, 0, 0]).is_none());
        assert!(parse_audio_frame(&[1, 9, b'x']).is_none(), "truncated name");
    }

    #[test]
    fn messages_keep_their_shape() {
        let m = Msg::Rpc {
            id: 3,
            inst: "a".into(),
            proc_name: "query-gci".into(),
            args: vec![("side".into(), "blue".into())],
            optional: false,
        };
        let s = serde_json::to_string(&m).unwrap();
        assert_eq!(
            s,
            r#"{"t":"rpc","id":3,"inst":"a","proc_name":"query-gci","args":[["side","blue"]],"optional":false}"#
        );
        let r: Msg = serde_json::from_str(
            r#"{"t":"reply","id":3,"ok":null,"err":{"status":422,"msg":"no slot"}}"#,
        )
        .unwrap();
        match r {
            Msg::Reply { id: 3, ok: None, err: Some(e) } => {
                assert_eq!((e.status, e.billed), (422, false))
            }
            other => panic!("{other:?}"),
        }
        let q = Msg::Req { id: 9, op: Op::AtcTranscript { inst: "r".into(), since: 5 } };
        assert_eq!(
            serde_json::to_string(&q).unwrap(),
            r#"{"t":"req","id":9,"op":{"op":"atc_transcript","inst":"r","since":5}}"#
        );
    }

    #[test]
    fn a_news_write_without_a_voice_is_the_default_voice() {
        let digest = r#"{"day":"2026-09-27","generated":"2026-09-27T12:00:00Z","round":1,
            "headline":"H","items":[],"facts":{"blue_captures":0,"red_captures":0,
            "objectives_traded":0,"air_kills":0,"ground_kills":0,"blue_held":0,
            "red_held":0,"neutral_held":0,"changed_hands":[]},"final_":false}"#;
        // What a bfdb from before voices sends.
        let old = format!(
            r#"{{"t":"req","id":4,"op":{{"op":"news_write","digest":{digest},"history":[]}}}}"#
        );
        match serde_json::from_str::<Msg>(&old).unwrap() {
            Msg::Req { id: 4, op: Op::NewsWrite { voice, .. } } => {
                assert_eq!(voice, Voice::Default)
            }
            other => panic!("{other:?}"),
        }
        // And this one names it.
        let new = Msg::Req {
            id: 5,
            op: Op::NewsWrite {
                digest: Box::new(serde_json::from_str(digest).unwrap()),
                history: vec![],
                voice: Voice::Soviet,
            },
        };
        let s = serde_json::to_string(&new).unwrap();
        assert!(s.contains(r#""voice":"soviet""#), "{s}");
        match serde_json::from_str::<Msg>(&s).unwrap() {
            Msg::Req { op: Op::NewsWrite { voice, .. }, .. } => {
                assert_eq!(voice, Voice::Soviet)
            }
            other => panic!("{other:?}"),
        }
        // An older worker's reply carries no voice (`hub::checked_voice`).
        let reply: crate::news_llm::Written =
            serde_json::from_str(r#"{"headline":"H","body":["b"]}"#).unwrap();
        assert!(reply.voice.is_none());
    }

    #[test]
    fn typed_errors_survive_the_link() {
        let e = crate::websec::engine_refused("you must be in a slot");
        let back = RemoteErr::from_anyhow(&e).into_anyhow();
        let a = back.downcast_ref::<crate::websec::ApiError>().unwrap();
        assert_eq!((a.status.as_u16(), a.msg.as_str()), (422, "you must be in a slot"));
        let plain =
            RemoteErr::from_anyhow(&anyhow::anyhow!("disk on fire")).into_anyhow();
        assert!(plain.downcast_ref::<crate::websec::ApiError>().is_none());
    }

    #[tokio::test]
    async fn calls_get_their_replies_and_fail_when_the_link_goes() {
        let (peer, mut rx) = Peer::new();
        let p = peer.clone();
        let echo = tokio::spawn(async move {
            // answer the first request, then drop the link
            if let Some(Frame::Text(t)) = rx.recv().await {
                if let Ok(Msg::Req { id, .. }) = serde_json::from_str::<Msg>(&t) {
                    p.resolve(id, Some(serde_json::json!("ok")), None);
                }
            }
            let _ = rx.recv().await;
            p.fail_all();
        });
        let op = |inst: &str| Op::AtcTranscript { inst: inst.into(), since: 0 };
        let v = peer
            .call(|id| Msg::Req { id, op: op("a") }, Duration::from_secs(5))
            .await
            .unwrap();
        assert_eq!(v, serde_json::json!("ok"));
        let e = peer
            .call(|id| Msg::Req { id, op: op("b") }, Duration::from_secs(5))
            .await
            .unwrap_err();
        assert_eq!(e.status, 503);
        echo.await.unwrap();
    }
}
