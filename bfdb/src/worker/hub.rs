//! bfdb's end of the worker link (`--worker-mode external`).
//!
//! A listener of its own on loopback (`--worker-listen`), never the public
//! one: the link is not reachable through Caddy at all. One worker at a time;
//! a new connection replaces the old one (a worker restarting before its old
//! socket timed out).
//!
//! The functions at the bottom are what routes and tasks call. Each does the
//! local thing in in-process mode and asks the worker in external mode.

use super::link::{self, Msg, Op, Peer, RemoteErr, PROTOCOL};
use crate::{db::StatsDb, websec, GciMap};
use futures::{SinkExt, StreamExt};
use netidx::publisher::Value;
use std::{
    net::SocketAddr,
    sync::{
        atomic::{AtomicBool, AtomicU64, Ordering},
        Arc, Mutex, OnceLock,
    },
    time::Duration,
};
use warp::{ws::Message, Filter, Reply};

/// The engine procedures the voice services and the HQ strategist call --
/// all a worker may ask bfdb to call for it.
const WORKER_PROCS: &[&str] =
    &["query-gci", "query-atc", "query-range", "range-atc", "query-hq", "hq-directive"];

pub(crate) struct Hub {
    secret: String,
    db: StatsDb,
    gci: GciMap,
    /// The connected worker: its connection number and its end of the link.
    current: Mutex<Option<(u64, Arc<Peer>)>>,
    conns: AtomicU64,
    /// The diary model the worker last announced (see `DiarySource::Worker`).
    writer_model: Mutex<Option<String>>,
    /// The worker last said it can draw the diary's pictures.
    draws: AtomicBool,
    /// A worker has said hello since bfdb started.
    seen: AtomicBool,
}

static HUB: OnceLock<Arc<Hub>> = OnceLock::new();

/// The hub, in external mode.
pub(crate) fn hub() -> Option<&'static Arc<Hub>> {
    HUB.get()
}

/// Start the worker listener on `listen`.
pub(crate) fn start(
    listen: SocketAddr,
    secret: String,
    db: StatsDb,
    gci: GciMap,
) -> anyhow::Result<()> {
    let hub = Hub::new(secret, db, gci);
    HUB.set(hub.clone())
        .map_err(|_| anyhow::anyhow!("the worker hub was started twice"))?;
    let route = routes(hub);
    log::info!("worker link listening on ws://{listen}/internal/worker (loopback callers with the secret only)");
    tokio::spawn(async move {
        if let Err(e) = crate::serve::serve(route, listen, None).await {
            log::error!("worker link: {e:#} -- the voice worker cannot connect");
        }
    });
    Ok(())
}

/// `GET /internal/worker`: a loopback caller with the secret upgrades.
fn routes(hub: Arc<Hub>) -> warp::filters::BoxedFilter<(warp::reply::Response,)> {
    warp::path!("internal" / "worker")
        .and(crate::serve::peer_addr())
        .and(warp::header::optional::<String>("authorization"))
        .and(warp::ws())
        .map(move |peer: Option<SocketAddr>, auth: Option<String>, ws: warp::ws::Ws| {
            if !peer.is_some_and(|p| p.ip().is_loopback()) {
                return websec::ws_refused(
                    warp::http::StatusCode::FORBIDDEN,
                    "loopback only",
                );
            }
            let token =
                auth.as_deref().and_then(|a| a.strip_prefix("Bearer ")).unwrap_or("");
            if !websec::ct_eq(token, &hub.secret) {
                log::warn!(
                    "worker link: refused a connection with a wrong or missing secret"
                );
                return websec::ws_refused(
                    warp::http::StatusCode::UNAUTHORIZED,
                    "bad worker secret",
                );
            }
            let hub = hub.clone();
            ws.max_message_size(32 << 20)
                .max_frame_size(32 << 20)
                .on_upgrade(move |socket| hub.serve(socket))
                .into_response()
        })
        .boxed()
}

impl Hub {
    fn new(secret: String, db: StatsDb, gci: GciMap) -> Arc<Self> {
        Arc::new(Hub {
            secret,
            db,
            gci,
            current: Mutex::new(None),
            conns: AtomicU64::new(0),
            writer_model: Mutex::new(None),
            draws: AtomicBool::new(false),
            seen: AtomicBool::new(false),
        })
    }

    /// A worker has connected at least once, so `writer_model` is its
    /// answer and not just "not heard from yet".
    pub(crate) fn seen(&self) -> bool {
        self.seen.load(Ordering::Relaxed)
    }

    pub(crate) fn writer_model(&self) -> Option<String> {
        self.writer_model.lock().unwrap().clone()
    }

    fn peer(&self) -> Option<Arc<Peer>> {
        self.current.lock().unwrap().as_ref().map(|(_, p)| p.clone())
    }

    /// Ask the worker.
    async fn request(
        &self,
        op: Op,
        timeout: Duration,
    ) -> Result<serde_json::Value, RemoteErr> {
        match self.peer() {
            Some(p) => p.call(|id| Msg::Req { id, op }, timeout).await,
            None => Err(RemoteErr::link("the voice worker is not connected")),
        }
    }

    async fn serve(self: Arc<Self>, ws: warp::ws::WebSocket) {
        let (mut sink, mut stream) = ws.split();
        let hello =
            match tokio::time::timeout(Duration::from_secs(10), stream.next()).await {
                Ok(Some(Ok(m))) if m.is_text() => {
                    serde_json::from_str::<Msg>(m.to_str().unwrap_or_default()).ok()
                }
                _ => None,
            };
        let Some(Msg::Hello { protocol, version, instances, news_writer, news_draw }) =
            hello
        else {
            log::warn!("worker link: the peer sent no hello -- closing");
            return;
        };
        if protocol != PROTOCOL {
            log::error!(
                "worker link: the worker (bfdb {version}) speaks protocol {protocol}, this bfdb {PROTOCOL} -- \
                 run the same bfdb.exe build for both"
            );
            let _ = sink.send(Message::close_with(1008u16, "protocol mismatch")).await;
            return;
        }
        let known: Vec<String> =
            self.db.instances().all().iter().map(|c| c.id.clone()).collect();
        let strangers: Vec<&String> =
            instances.iter().filter(|i| !known.contains(i)).collect();
        if !strangers.is_empty() {
            log::warn!("worker link: the worker runs instances this bfdb does not know: {strangers:?} -- check both read the same --instances");
        }
        log::info!(
            "worker link: worker connected (bfdb {version}; instances {instances:?}; diary writer {}; pictures {})",
            news_writer.as_deref().unwrap_or("none"),
            if news_draw { "yes" } else { "no" }
        );
        *self.writer_model.lock().unwrap() = news_writer;
        self.draws.store(news_draw, Ordering::Relaxed);
        self.seen.store(true, Ordering::Relaxed);

        let (peer, mut out) = Peer::new();
        let me = self.conns.fetch_add(1, Ordering::Relaxed) + 1;
        if let Some((_, old)) = self.current.lock().unwrap().replace((me, peer.clone())) {
            log::info!("worker link: replacing the previous worker connection");
            old.fail_all();
        }
        peer.send(&Msg::Welcome {
            protocol: PROTOCOL,
            version: crate::BUILD_VERSION.to_string(),
        });
        let writer = tokio::spawn(async move {
            while let Some(f) = out.recv().await {
                let m = match f {
                    link::Frame::Text(t) => Message::text(t),
                    link::Frame::Binary(b) => Message::binary(b),
                };
                if sink.send(m).await.is_err() {
                    break;
                }
            }
            let _ = sink.close().await;
        });
        let still_current = |hub: &Hub| {
            hub.current.lock().unwrap().as_ref().is_some_and(|(n, _)| *n == me)
        };
        loop {
            // A silent link is a dead one: the worker pings every 20 s.
            let m = match tokio::time::timeout(Duration::from_secs(90), stream.next())
                .await
            {
                Ok(Some(Ok(m))) => m,
                Ok(_) => break,
                Err(_) => {
                    log::warn!(
                        "worker link: nothing from the worker for 90s -- dropping it"
                    );
                    break;
                }
            };
            if !still_current(&self) {
                break;
            }
            if m.is_close() {
                break;
            }
            if m.is_binary() {
                if let Some((inst, station, pcm)) = link::parse_audio_frame(m.as_bytes())
                {
                    crate::milatc::live::inject(
                        &inst,
                        crate::milatc::live::Frame::Audio {
                            station: station.into(),
                            pcm,
                        },
                    );
                }
                continue;
            }
            let Ok(text) = m.to_str() else { continue };
            match serde_json::from_str::<Msg>(text) {
                Ok(msg) => self.handle(&peer, msg),
                Err(e) => log::warn!("worker link: unreadable message ({e})"),
            }
        }
        {
            let mut g = self.current.lock().unwrap();
            if g.as_ref().is_some_and(|(n, _)| *n == me) {
                *g = None;
            }
        }
        peer.fail_all();
        writer.abort();
        log::warn!("worker link: worker disconnected");
    }

    fn handle(self: &Arc<Self>, peer: &Arc<Peer>, msg: Msg) {
        match msg {
            Msg::Rpc { id, inst, proc_name, args, optional } => {
                let (hub, peer) = (self.clone(), peer.clone());
                tokio::spawn(async move {
                    let r = hub.engine_rpc(&inst, &proc_name, args, optional).await;
                    peer.reply(id, r);
                });
            }
            Msg::GciLine { inst, line } => {
                if let Some((tx, hist)) = self.gci.get(inst.as_str()) {
                    {
                        let mut h = hist.lock().unwrap();
                        h.push_back(line.clone());
                        while h.len() > crate::gci::TRANSCRIPT_CAP {
                            h.pop_front();
                        }
                    }
                    let _ = tx.send(line);
                }
            }
            Msg::GciBacklog { inst, lines } => {
                if let Some((_, hist)) = self.gci.get(inst.as_str()) {
                    let mut h = hist.lock().unwrap();
                    h.clear();
                    let skip = lines.len().saturating_sub(crate::gci::TRANSCRIPT_CAP);
                    h.extend(lines.into_iter().skip(skip));
                }
            }
            Msg::LiveOn { inst, station, who, text } => crate::milatc::live::inject(
                &inst,
                crate::milatc::live::Frame::On {
                    station: station.into(),
                    who: who.into(),
                    text: text.into(),
                },
            ),
            Msg::LiveOff { inst, station } => crate::milatc::live::inject(
                &inst,
                crate::milatc::live::Frame::Off { station: station.into() },
            ),
            Msg::Reply { id, ok, err } => peer.resolve(id, ok, err),
            Msg::Hello { .. } | Msg::Welcome { .. } | Msg::Req { .. } => {
                log::debug!("worker link: ignoring a message the worker should not send")
            }
        }
    }

    /// An engine call on the worker's behalf, made exactly as the services
    /// make it in-process.
    async fn engine_rpc(
        &self,
        inst: &str,
        proc_name: &str,
        args: Vec<(String, String)>,
        optional: bool,
    ) -> Result<serde_json::Value, RemoteErr> {
        if !WORKER_PROCS.contains(&proc_name) {
            return Err(RemoteErr::new(
                403,
                format!("the worker may not call {proc_name}"),
            ));
        }
        if !self.db.instances().all().iter().any(|c| c.id == inst) {
            return Err(RemoteErr::new(400, format!("unknown instance {inst:?}")));
        }
        let st = self.db.state(&Arc::from(inst));
        let args: Vec<(&str, Value)> =
            args.iter().map(|(k, v)| (k.as_str(), Value::from(v.clone()))).collect();
        if optional {
            match tokio::time::timeout(
                Duration::from_secs(15),
                self.db.call_engine_rpc_optional(&st, proc_name, args),
            )
            .await
            {
                Ok(Ok(Value::String(s))) => Ok(serde_json::Value::String(s.to_string())),
                // the engine's own refusal, kept apart (EngineHost::rpc_optional)
                Ok(Ok(Value::Error(e))) => Err(RemoteErr::new(422, e.to_string())),
                Ok(Ok(other)) => {
                    Err(RemoteErr::new(0, format!("unexpected RPC reply: {other:?}")))
                }
                Ok(Err(e)) => Err(RemoteErr::new(0, format!("{e:#}"))),
                Err(_) => Err(RemoteErr::new(0, format!("{proc_name} timed out"))),
            }
        } else {
            crate::call_engine_rpc_str(&self.db, &st, proc_name, args)
                .await
                .map(serde_json::Value::String)
                .map_err(|e| RemoteErr::from_anyhow(&e.0))
        }
    }
}

// ── What routes and tasks call ──────────────────────────────────────────────

/// A dashboard controller's GCI transmission (`gci::human_call`).
pub(crate) async fn gci_transmit(
    inst: &str,
    side: dcso3::coalition::Side,
    text: &str,
) -> Result<(), String> {
    match hub() {
        None => crate::gci::human_call(inst, side, text).map_err(str::to_string),
        Some(h) => {
            let side = if side == dcso3::coalition::Side::Red { "red" } else { "blue" };
            h.request(
                Op::GciTransmit {
                    inst: inst.into(),
                    side: side.into(),
                    text: text.into(),
                },
                Duration::from_secs(5),
            )
            .await
            .map(|_| ())
            .map_err(|e| e.msg)
        }
    }
}

/// The range ATC transcript (`milatc::transcript_json`). Empty while the
/// worker is away.
pub(crate) async fn atc_transcript(inst: &str, since: i64) -> serde_json::Value {
    match hub() {
        None => crate::milatc::transcript_json(inst, since),
        Some(h) => match h
            .request(
                Op::AtcTranscript { inst: inst.into(), since },
                Duration::from_secs(5),
            )
            .await
        {
            Ok(v) => v,
            Err(e) => {
                log::debug!("range ATC transcript from the worker: {e}");
                serde_json::json!({ "stations": [], "lines": [] })
            }
        },
    }
}

/// One kept range ATC transmission, as WAV (`milatc::audio_wav`).
pub(crate) async fn atc_audio(inst: &str, id: u64) -> Option<Arc<Vec<u8>>> {
    use base64::Engine as _;
    match hub() {
        None => crate::milatc::audio_wav(inst, id),
        Some(h) => {
            let v = h
                .request(Op::AtcAudio { inst: inst.into(), id }, Duration::from_secs(10))
                .await
                .ok()?;
            let b = base64::engine::general_purpose::STANDARD
                .decode(v.get("wav")?.as_str()?)
                .ok()?;
            Some(Arc::new(b))
        }
    }
}

/// Run a request from blocking code (the diary's pass runs under
/// `block_in_place`, the picture route under `spawn_blocking`).
fn blocking_request(op: Op, timeout: Duration) -> Result<serde_json::Value, RemoteErr> {
    let Some(h) = hub() else {
        return Err(RemoteErr::link("not in external worker mode"));
    };
    tokio::runtime::Handle::current().block_on(h.request(op, timeout))
}

/// The diary's prose from the worker's writer. A 429 there pushes this
/// process's writer gate back just as a local one would.
pub(crate) fn news_write(
    d: &crate::news::NewsDigest,
    history: &[crate::news::NewsDigest],
) -> anyhow::Result<crate::news_llm::Written> {
    let op = Op::NewsWrite { digest: Box::new(d.clone()), history: history.to_vec() };
    match blocking_request(op, Duration::from_secs(180)) {
        Ok(v) => Ok(serde_json::from_value(v)?),
        Err(e) if e.rate_limited => {
            let backoff = crate::news_llm::note_rate_limited(
                e.retry_secs.map(Duration::from_secs_f64),
            );
            Err(anyhow::Error::new(crate::news_llm::RateLimited { backoff, body: e.msg }))
        }
        Err(e) => Err(anyhow::anyhow!("worker: {}", e.msg)),
    }
}

/// One diary picture from the worker's provider: (bytes, who drew it).
pub(crate) fn news_draw(
    prompt: &str,
) -> Result<(Vec<u8>, String), crate::news_image::GenError> {
    use base64::Engine as _;
    let fail = |msg: String, billed: bool, rate_limited: bool| {
        crate::news_image::GenError { msg, billed, rate_limited }
    };
    if !hub().is_some_and(|h| h.draws.load(Ordering::Relaxed)) {
        return Err(fail(
            "the worker has no picture provider (or is not connected)".into(),
            false,
            false,
        ));
    }
    match blocking_request(
        Op::NewsDraw { prompt: prompt.into() },
        Duration::from_secs(240),
    ) {
        Ok(v) => {
            let bytes = v
                .get("b64")
                .and_then(|b| b.as_str())
                .and_then(|b| base64::engine::general_purpose::STANDARD.decode(b).ok())
                .ok_or_else(|| fail("the worker sent no picture".into(), true, false))?;
            Ok((
                bytes,
                v.get("by").and_then(|b| b.as_str()).unwrap_or_default().to_string(),
            ))
        }
        Err(e) => Err(fail(format!("worker: {}", e.msg), e.billed, e.rate_limited)),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::worker::{
        client::{self, Link},
        host::EngineHost,
    };

    async fn until(mut f: impl FnMut() -> bool) {
        for _ in 0..200 {
            if f() {
                return;
            }
            tokio::time::sleep(Duration::from_millis(25)).await;
        }
        panic!("timed out waiting");
    }

    /// bfdb's hub and a worker over a real loopback socket: the handshake,
    /// engine calls both allowed and refused, GCI lines coming back, bfdb's
    /// requests answered, and a wrong secret turned away.
    #[tokio::test(flavor = "multi_thread")]
    async fn hub_and_worker_talk() {
        let h = crate::route_tests::harness();
        let id: crate::instance::InstanceId = Arc::from("default");
        let fresh = || {
            let (tx, _) = tokio::sync::broadcast::channel::<String>(8);
            let hist: crate::LogHistory = Default::default();
            (tx, hist)
        };
        let gci: GciMap = Arc::new([(id.clone(), fresh())].into_iter().collect());
        let hub = Hub::new("s3cret".into(), h.db.clone(), gci.clone());
        let l = tokio::net::TcpListener::bind(("127.0.0.1", 0)).await.unwrap();
        let url =
            format!("ws://127.0.0.1:{}/internal/worker", l.local_addr().unwrap().port());
        tokio::spawn(crate::serve::serve_on(l, routes(hub.clone()), None));

        // the wrong secret never gets a link
        let link = Arc::new(Link::default());
        let cfg = h.db.instances().all()[0].clone();
        let host = EngineHost::remote(cfg.clone(), false, link.clone());
        let worker_gci: std::collections::HashMap<_, _> =
            [(id.clone(), fresh())].into_iter().collect();
        let svc = client::test_services(vec![(host.clone(), None)], worker_gci.clone());
        assert!(client::test_session(
            url.clone(),
            "wrong".into(),
            svc.clone(),
            link.clone()
        )
        .await
        .is_err());
        assert!(hub.peer().is_none());

        let session = tokio::spawn(client::test_session(
            url.clone(),
            "s3cret".into(),
            svc,
            link.clone(),
        ));
        until(|| hub.peer().is_some()).await;

        // an engine call goes through bfdb: this instance is offline, so it
        // fails there with the typed error the local call gives
        let e = host
            .rpc_str("query-gci", vec![("side", netidx::publisher::Value::from("blue"))])
            .await
            .unwrap_err();
        assert_eq!(
            e.0.downcast_ref::<crate::websec::ApiError>().map(|a| a.status.as_u16()),
            Some(503),
            "{:#}",
            e.0
        );
        // a procedure the services never call is refused outright
        let r = link
            .call(
                |id| Msg::Rpc {
                    id,
                    inst: "default".into(),
                    proc_name: "admin-reset".into(),
                    args: vec![],
                    optional: false,
                },
                Duration::from_secs(5),
            )
            .await;
        assert_eq!(r.unwrap_err().status, 403);

        // a GCI line from the worker lands in bfdb's transcript
        worker_gci[&id].0.send(r#"{"text":"picture clean"}"#.into()).unwrap();
        until(|| gci[&id].1.lock().unwrap().len() == 1).await;

        // bfdb's requests
        let t = hub
            .request(
                Op::AtcTranscript { inst: "default".into(), since: 0 },
                Duration::from_secs(5),
            )
            .await
            .unwrap();
        assert_eq!(t, serde_json::json!({ "stations": [], "lines": [] }));
        let e = hub
            .request(
                Op::GciTransmit {
                    inst: "default".into(),
                    side: "blue".into(),
                    text: "hi".into(),
                },
                Duration::from_secs(5),
            )
            .await
            .unwrap_err();
        assert_eq!(
            (e.status, e.msg.as_str()),
            (400, "GCI voice is not running on this server")
        );
        let e = hub
            .request(Op::NewsDraw { prompt: "x".into() }, Duration::from_secs(5))
            .await
            .unwrap_err();
        assert!(e.msg.contains("no picture provider"), "{}", e.msg);
        session.abort();
    }
}
