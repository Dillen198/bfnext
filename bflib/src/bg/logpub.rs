use anyhow::Result;
use bytes::BytesMut;
use futures::{
    StreamExt,
    channel::{
        mpsc::{self, UnboundedReceiver, UnboundedSender},
        oneshot,
    },
    select_biased,
};
use fxhash::FxHashSet;
use log::error;
use netidx::{
    chars::Chars,
    path::Path,
    publisher::{Event, Publisher, Value},
};
use std::{io::SeekFrom, path::PathBuf, time::Duration};
use tokio::{
    fs::OpenOptions,
    io::{AsyncBufReadExt, AsyncSeekExt, AsyncWriteExt, BufReader},
    task,
};

enum ToLogger {
    Log(Chars),
    Close(oneshot::Sender<()>),
}

/// How much of the log a new subscriber is sent. It used to be the whole
/// file -- days of engine log on a long-running server -- replayed inside this
/// loop, so every dashboard that opened the log view stalled live logging for
/// as long as that took. The tail is what anyone subscribing actually reads.
const REPLAY_TAIL_BYTES: u64 = 256 * 1024;

/// Lines per batch commit during the replay.
const REPLAY_BATCH_LINES: usize = 250;

/// Returns Ok when the logger was closed on purpose (or its publisher went
/// away), Err on an I/O failure worth restarting over.
async fn logger_loop(
    publisher: &Publisher,
    file_path: &PathBuf,
    netidx_path: &Path,
    input: &mut UnboundedReceiver<ToLogger>,
) -> Result<()> {
    let mut file = OpenOptions::new()
        .append(true)
        .create(true)
        .read(true)
        .write(true)
        .open(&file_path)
        .await?;
    let (tx, mut events) = mpsc::unbounded();
    let contents = publisher.publish(netidx_path.clone(), Value::Null)?;
    publisher.events_for_id(contents.id(), tx);
    let mut subs = FxHashSet::default();
    let mut batch = publisher.start_batch();
    let mut line: Vec<u8> = Vec::new();
    let mut bytes = BytesMut::new();
    loop {
        select_biased! {
            e = events.select_next_some() => match e {
                Event::Destroyed(_) => return Ok(()),
                Event::Unsubscribe(_, cl) => {
                    subs.remove(&cl);
                }
                Event::Subscribe(_, cl) => {
                    subs.insert(cl);
                    let len = file.seek(SeekFrom::End(0)).await?;
                    let start = len.saturating_sub(REPLAY_TAIL_BYTES);
                    file.seek(SeekFrom::Start(start)).await?;
                    let mut bufreader = BufReader::new(file);
                    if start > 0 {
                        // landed mid-line, skip to the next whole one
                        line.clear();
                        bufreader.read_until(b'\n', &mut line).await?;
                    }
                    let mut n = 0;
                    loop {
                        line.clear();
                        if bufreader.read_until(b'\n', &mut line).await? == 0 {
                            break
                        }
                        // Lossy: one invalid byte anywhere in the file used to
                        // fail read_line, and that ended the logger for good.
                        let s = std::string::String::from_utf8_lossy(&line);
                        bytes.extend_from_slice(s.trim().as_bytes());
                        let Ok(chars) = Chars::from_bytes(bytes.split().freeze()) else {
                            continue
                        };
                        contents.update_subscriber(&mut batch, cl, Value::String(chars));
                        n += 1;
                        if n >= REPLAY_BATCH_LINES {
                            n = 0;
                            batch.commit(Some(Duration::from_secs(10))).await;
                            batch = publisher.start_batch();
                        }
                    }
                    file = bufreader.into_inner();
                    batch.commit(Some(Duration::from_secs(10))).await;
                    batch = publisher.start_batch();
                },
            },
            e = input.select_next_some() => match e {
                ToLogger::Log(b) => {
                    file.write_all_buf(&mut b.as_bytes()).await?;
                    bytes.extend_from_slice(b.trim().as_bytes());
                    if let Ok(c) = Chars::from_bytes(bytes.split().freeze()) {
                        for cl in &subs {
                            contents.update_subscriber(&mut batch, *cl, Value::String(c.clone()))
                        }
                    }
                    batch.commit(Some(Duration::from_secs(10))).await;
                    batch = publisher.start_batch();
                }
                ToLogger::Close(ch) => {
                    drop(contents);
                    drop(file);
                    let _ = ch.send(());
                    return Ok(())
                }
            },
            complete => return Ok(())
        }
    }
}

#[derive(Debug, Clone)]
pub struct LogPublisher(UnboundedSender<ToLogger>);

impl LogPublisher {
    pub fn new(publisher: Publisher, file_path: &PathBuf, netidx_path: Path) -> Result<Self> {
        let (tx, mut rx) = mpsc::unbounded();
        let file_path = file_path.clone();
        task::spawn(async move {
            // Any I/O error in here used to end the logger permanently: every
            // log line after it failed to send and the engine log just
            // stopped. Start it again instead; lines that arrive meanwhile
            // wait in the channel.
            loop {
                match logger_loop(&publisher, &file_path, &netidx_path, &mut rx).await {
                    Ok(()) => break,
                    Err(e) => {
                        error!("{file_path:?} logger failed, restarting in 5s {e:?}");
                        tokio::time::sleep(Duration::from_secs(5)).await;
                    }
                }
            }
        });
        Ok(Self(tx))
    }

    pub fn append(&self, m: Chars) -> Result<()> {
        Ok(self.0.unbounded_send(ToLogger::Log(m))?)
    }

    pub async fn close(&self) -> Result<()> {
        let (tx, rx) = oneshot::channel();
        self.0.unbounded_send(ToLogger::Close(tx))?;
        Ok(rx.await?)
    }
}
