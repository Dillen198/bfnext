//! The `bfdb --worker` process.
//!
//! Connects to bfdb's worker link, and once bfdb has welcomed it starts the
//! voice services and the HQ strategist exactly as bfdb would in-process
//! (`super::spawn_voice`), their engine calls going over the link. It also
//! answers bfdb's requests: dashboard GCI transmissions, the range ATC
//! transcript and audio, the diary's prose and pictures.
//!
//! Voice must never run twice against one SRS server. So nothing starts
//! before the first welcome -- only a bfdb in external mode has a worker
//! listener to welcome it -- and a worker that has lost bfdb for
//! [`LOST_FOR`] exits (code 3) rather than keep talking on its own. Its
//! supervisor starts it again, and it waits for a welcome again.

use super::{
    host::EngineHost,
    link::{self, Msg, Op, Peer, RemoteErr, PROTOCOL},
};
use crate::{instance::InstanceId, news_image::ImageCfg, news_llm::WriterCfg, LogHistory};
use futures::{SinkExt, StreamExt};
use std::{
    collections::{HashMap, VecDeque},
    sync::{Arc, Mutex},
    time::{Duration, Instant},
};
use tokio::sync::broadcast;
use tokio_tungstenite::tungstenite::{client::IntoClientRequest, Message};

/// How long the worker keeps its services up with no bfdb to talk to.
const LOST_FOR: Duration = Duration::from_secs(60);

/// The worker's end of the link, across reconnects.
#[derive(Default)]
pub(crate) struct Link {
    current: Mutex<Option<Arc<Peer>>>,
}

impl Link {
    fn peer(&self) -> Option<Arc<Peer>> {
        self.current.lock().unwrap().clone()
    }

    pub(crate) async fn call(&self, make: impl FnOnce(u64) -> Msg, timeout: Duration) -> Result<serde_json::Value, RemoteErr> {
        match self.peer() {
            Some(p) => p.call(make, timeout).await,
            None => Err(RemoteErr::link("not connected to bfdb")),
        }
    }

    fn send(&self, m: &Msg) {
        if let Some(p) = self.peer() {
            p.send(m);
        }
    }

    fn send_binary(&self, b: Vec<u8>) {
        if let Some(p) = self.peer() {
            p.send_binary(b);
        }
    }
}

/// What the worker runs, settled from the command line.
pub(crate) struct Services {
    hosts: Vec<(EngineHost, Option<crate::gci::GciConfig>)>,
    gci: HashMap<InstanceId, (broadcast::Sender<String>, LogHistory)>,
    news_writer: Option<WriterCfg>,
    hq_writer: Option<WriterCfg>,
    hq_every: Duration,
    images: Option<Arc<ImageCfg>>,
}

pub(crate) async fn run(args: crate::Args) -> anyhow::Result<()> {
    log::info!("bfdb worker v{} git:{} -- voice and model services, no database", crate::BUILD_VERSION, crate::BUILD_GIT);
    let registry = crate::registry_from_args(&args)?;
    let link = Arc::new(Link::default());
    let multi = registry.all().len() > 1;
    let default_id = registry.default_id().to_string();
    let global_gci = crate::campaign_gci(&args);
    let mut hosts = Vec::new();
    let mut gci = HashMap::new();
    for cfg in registry.all() {
        let id: InstanceId = Arc::from(cfg.id.as_str());
        let c = super::gci_config_for(cfg, *cfg.id == *default_id, &global_gci);
        let (tx, _) = broadcast::channel::<String>(128);
        gci.insert(id, (tx, Arc::new(Mutex::new(VecDeque::new())) as LogHistory));
        hosts.push((EngineHost::remote(cfg.clone(), multi, link.clone()), c));
    }
    let news_writer = WriterCfg::resolve(args.news_llm_url.clone(), args.news_llm_key.clone(), args.news_llm_model.clone());
    let hq_writer = crate::hq_writer_cfg(&crate::HqFlags::of(&args), &news_writer);
    let images = match ImageCfg::resolve(crate::news_image_args(&args, false)) {
        Ok(c) => c.map(Arc::new),
        Err(e) => {
            log::error!("worker: dispatch pictures are OFF -- {e}");
            None
        }
    };
    let svc = Arc::new(Services {
        hosts,
        gci,
        news_writer,
        hq_writer,
        hq_every: Duration::from_secs(args.hq_strategist_minutes.max(1) * 60),
        images,
    });

    let mut started = false;
    let mut lost_since: Option<Instant> = None;
    let mut backoff = Duration::from_secs(1);
    loop {
        let secret = match super::secret(&args.worker, &args.db, false) {
            Ok(Some(s)) => s,
            Ok(None) => {
                log::info!("worker: waiting for bfdb to write the shared secret");
                tokio::time::sleep(Duration::from_secs(2)).await;
                continue;
            }
            Err(e) => return Err(e),
        };
        match session(&args.worker.url, &secret, &svc, &link, &mut started).await {
            Ok(()) => {
                lost_since = None;
                backoff = Duration::from_secs(1);
            }
            Err(e) => log::warn!("worker: {e:#}"),
        }
        if started {
            let since = *lost_since.get_or_insert_with(Instant::now);
            if since.elapsed() >= LOST_FOR {
                log::error!(
                    "worker: no bfdb at {} for {}s -- stopping, so voice never runs twice",
                    args.worker.url,
                    LOST_FOR.as_secs()
                );
                log::logger().flush();
                std::process::exit(3);
            }
        }
        tokio::time::sleep(backoff).await;
        backoff = (backoff * 2).min(Duration::from_secs(5));
    }
}

/// One connection, from handshake to close. `Ok` once it was welcomed.
async fn session(
    url: &str,
    secret: &str,
    svc: &Arc<Services>,
    link: &Arc<Link>,
    started: &mut bool,
) -> anyhow::Result<()> {
    let mut req = url.into_client_request()?;
    req.headers_mut().insert("authorization", format!("Bearer {secret}").parse()?);
    let (ws, _) = tokio::time::timeout(Duration::from_secs(10), tokio_tungstenite::connect_async(req))
        .await
        .map_err(|_| anyhow::anyhow!("connecting to {url} timed out"))?
        .map_err(|e| anyhow::anyhow!("connecting to {url}: {e}"))?;
    let (mut sink, mut stream) = ws.split();
    let hello = Msg::Hello {
        protocol: PROTOCOL,
        version: crate::BUILD_VERSION.to_string(),
        instances: svc.hosts.iter().map(|(h, _)| h.id.to_string()).collect(),
        news_writer: svc.news_writer.as_ref().map(|w| w.model.clone()),
        news_draw: svc.images.is_some(),
    };
    sink.send(Message::text(serde_json::to_string(&hello)?)).await?;
    let welcome = match tokio::time::timeout(Duration::from_secs(10), stream.next()).await {
        Ok(Some(Ok(Message::Text(t)))) => serde_json::from_str::<Msg>(t.as_str()).ok(),
        Ok(Some(Ok(Message::Close(c)))) => anyhow::bail!("bfdb closed the link: {c:?}"),
        _ => None,
    };
    match welcome {
        Some(Msg::Welcome { protocol, .. }) if protocol == PROTOCOL => {}
        Some(Msg::Welcome { protocol, version }) => {
            anyhow::bail!("bfdb {version} speaks worker protocol {protocol}, this worker {PROTOCOL}")
        }
        _ => anyhow::bail!("bfdb did not welcome the worker"),
    }
    log::info!("worker: connected to bfdb at {url}");

    let (peer, mut out) = Peer::new();
    *link.current.lock().unwrap() = Some(peer.clone());
    // The transcript so far, so a restarted bfdb's dashboard is not blank.
    for (id, (_, hist)) in &svc.gci {
        let lines: Vec<String> = hist.lock().unwrap().iter().cloned().collect();
        if !lines.is_empty() {
            peer.send(&Msg::GciBacklog { inst: id.to_string(), lines });
        }
    }
    if !*started {
        *started = true;
        start_services(svc, link);
    }

    let writer = tokio::spawn(async move {
        let mut ping = tokio::time::interval(Duration::from_secs(20));
        loop {
            tokio::select! {
                f = out.recv() => {
                    let Some(f) = f else { break };
                    let m = match f {
                        link::Frame::Text(t) => Message::text(t),
                        link::Frame::Binary(b) => Message::binary(b),
                    };
                    if sink.send(m).await.is_err() {
                        break;
                    }
                }
                _ = ping.tick() => {
                    if sink.send(Message::Ping(Vec::new().into())).await.is_err() {
                        break;
                    }
                }
            }
        }
    });
    while let Some(m) = stream.next().await {
        match m {
            Ok(Message::Text(t)) => match serde_json::from_str::<Msg>(t.as_str()) {
                Ok(Msg::Reply { id, ok, err }) => peer.resolve(id, ok, err),
                Ok(Msg::Req { id, op }) => {
                    let (svc, peer) = (svc.clone(), peer.clone());
                    tokio::spawn(async move {
                        let r = answer(&svc, op).await;
                        peer.reply(id, r);
                    });
                }
                Ok(_) => log::debug!("worker: ignoring a message bfdb should not send"),
                Err(e) => log::warn!("worker: unreadable message from bfdb ({e})"),
            },
            Ok(Message::Close(_)) | Err(_) => break,
            Ok(_) => {}
        }
    }
    {
        let mut g = link.current.lock().unwrap();
        if g.as_ref().map_or(false, |p| Arc::ptr_eq(p, &peer)) {
            *g = None;
        }
    }
    peer.fail_all();
    writer.abort();
    log::warn!("worker: lost bfdb");
    Ok(())
}

/// The services, once: voice per instance, the HQ strategist per campaign,
/// and the forwarders that carry their output to bfdb.
fn start_services(svc: &Arc<Services>, link: &Arc<Link>) {
    for (host, gci_cfg) in &svc.hosts {
        let (tx, hist) = svc.gci.get(&host.id).cloned().expect("a transcript per instance");
        // GCI transcript lines, as /ws/gci sends them.
        {
            let (mut rx, link, inst) = (tx.subscribe(), link.clone(), host.id.to_string());
            tokio::spawn(async move {
                loop {
                    match rx.recv().await {
                        Ok(line) => link.send(&Msg::GciLine { inst: inst.clone(), line }),
                        Err(broadcast::error::RecvError::Lagged(_)) => continue,
                        Err(_) => break,
                    }
                }
            });
        }
        if host.cfg.is_range() {
            // The range ATC's live feed. Subscribing also makes the feed
            // play (it is silent with no listener): bfdb's listeners are not
            // visible from here.
            let (mut rx, link, inst) = (crate::milatc::live::subscribe(&host.id), link.clone(), host.id.to_string());
            tokio::spawn(async move {
                use crate::milatc::live::Frame;
                loop {
                    match rx.recv().await {
                        Ok(f) => match &*f {
                            Frame::On { station, who, text } => link.send(&Msg::LiveOn {
                                inst: inst.clone(),
                                station: station.to_string(),
                                who: who.to_string(),
                                text: text.to_string(),
                            }),
                            Frame::Off { station } => {
                                link.send(&Msg::LiveOff { inst: inst.clone(), station: station.to_string() })
                            }
                            Frame::Audio { station, pcm } => link.send_binary(link::audio_frame(&inst, station, pcm)),
                        },
                        Err(broadcast::error::RecvError::Lagged(_)) => continue,
                        Err(_) => break,
                    }
                }
            });
        }
        super::spawn_voice(host.clone(), gci_cfg.clone(), tx, hist);
        if let Some(w) = svc.hq_writer.clone() {
            if host.cfg.base.is_some() && !host.cfg.is_range() {
                tokio::spawn(crate::hq_strategist::run(host.clone(), w, svc.hq_every));
            }
        }
    }
    if svc.hq_writer.is_none() {
        log::info!("worker: hq strategist off");
    }
}

/// One of bfdb's requests.
async fn answer(svc: &Services, op: Op) -> Result<serde_json::Value, RemoteErr> {
    use base64::Engine as _;
    let b64 = base64::engine::general_purpose::STANDARD;
    match op {
        Op::GciTransmit { inst, side, text } => {
            let side = if side == "red" { dcso3::coalition::Side::Red } else { dcso3::coalition::Side::Blue };
            crate::gci::human_call(&inst, side, &text)
                .map(|_| serde_json::Value::Null)
                .map_err(|e| RemoteErr::new(400, e))
        }
        Op::AtcTranscript { inst, since } => Ok(crate::milatc::transcript_json(&inst, since)),
        Op::AtcAudio { inst, id } => match crate::milatc::audio_wav(&inst, id) {
            Some(w) => Ok(serde_json::json!({ "wav": b64.encode(&**w) })),
            None => Err(RemoteErr::new(404, "that transmission's audio is no longer kept")),
        },
        Op::NewsWrite { digest, history } => {
            let Some(w) = svc.news_writer.clone() else {
                return Err(RemoteErr::new(0, "this worker has no diary writer configured"));
            };
            let r = tokio::task::spawn_blocking(move || crate::news_llm::write_dispatch(&w, &digest, &history))
                .await
                .map_err(|e| RemoteErr::new(0, format!("writer task died: {e}")))?;
            match r {
                Ok(out) => serde_json::to_value(out).map_err(|e| RemoteErr::new(0, e.to_string())),
                Err(e) => Err(match e.downcast_ref::<crate::news_llm::RateLimited>() {
                    Some(rl) => RemoteErr {
                        rate_limited: true,
                        retry_secs: Some(rl.backoff.as_secs_f64()),
                        ..RemoteErr::new(429, rl.body.clone())
                    },
                    None => RemoteErr::new(0, format!("{e:#}")),
                }),
            }
        }
        Op::NewsDraw { prompt } => {
            let Some(cfg) = svc.images.clone() else {
                return Err(RemoteErr::new(0, "this worker has no picture provider configured"));
            };
            let r = tokio::task::spawn_blocking(move || crate::news_image::draw(&cfg, &prompt))
                .await
                .map_err(|e| RemoteErr::new(0, format!("picture task died: {e}")))?;
            match r {
                Ok((bytes, by)) => Ok(serde_json::json!({ "b64": b64.encode(bytes), "by": by })),
                Err(e) => Err(RemoteErr { billed: e.billed, rate_limited: e.rate_limited, ..RemoteErr::new(0, e.msg) }),
            }
        }
    }
}

#[cfg(test)]
pub(crate) fn test_services(
    hosts: Vec<(EngineHost, Option<crate::gci::GciConfig>)>,
    gci: HashMap<InstanceId, (broadcast::Sender<String>, LogHistory)>,
) -> Arc<Services> {
    Arc::new(Services { hosts, gci, news_writer: None, hq_writer: None, hq_every: Duration::from_secs(60), images: None })
}

#[cfg(test)]
pub(crate) async fn test_session(url: String, secret: String, svc: Arc<Services>, link: Arc<Link>) -> anyhow::Result<()> {
    let mut started = false;
    session(&url, &secret, &svc, &link, &mut started).await
}
