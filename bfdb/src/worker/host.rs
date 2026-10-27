//! How the voice services and the HQ strategist reach the engine, wherever
//! they run.
//!
//! In bfdb itself ([`EngineHost::local`]) a call goes straight through
//! `StatsDb`, as it always has. In a `bfdb --worker` process
//! ([`EngineHost::remote`]) it goes over the worker link to bfdb, which makes
//! the same call -- so the instance's breaker, its sortie discovery and its
//! netidx subscriptions stay in one place, and the worker needs no netidx
//! and no database at all.

use super::link::Msg;
use crate::{db::StatsDb, instance::InstanceCfg, Error, Inst};
use netidx::publisher::Value;
use std::{sync::Arc, time::Duration};

#[derive(Clone)]
enum Via {
    Local { db: StatsDb, inst: Inst },
    Remote(Arc<super::client::Link>),
}

/// One instance's engine, for a service that talks to it.
#[derive(Clone)]
pub(crate) struct EngineHost {
    /// `cfg.id`.
    pub(crate) id: crate::instance::InstanceId,
    pub(crate) cfg: Arc<InstanceCfg>,
    /// This bfdb fronts more than one DCS server (GCI's Discord prefix).
    pub(crate) multi_instance: bool,
    via: Via,
}

/// An optional call's raw answer (`StatsDb::call_engine_rpc_optional`).
pub(crate) enum Answer {
    Str(String),
    /// The engine replied with its own error.
    Refused(String),
}

/// Engine replies the services use are strings; anything else is a bug.
fn string_args(args: Vec<(&str, Value)>) -> Result<Vec<(String, String)>, Error> {
    args.into_iter()
        .map(|(k, v)| match v {
            Value::String(s) => Ok((k.to_string(), s.to_string())),
            other => Err(Error(anyhow::anyhow!(
                "engine RPC argument {k} is not a string: {other:?}"
            ))),
        })
        .collect()
}

impl EngineHost {
    pub(crate) fn local(db: &StatsDb, inst: Inst) -> Self {
        Self {
            id: inst.id.clone(),
            cfg: inst.cfg.clone(),
            multi_instance: db.instances().all().len() > 1,
            via: Via::Local { db: db.clone(), inst },
        }
    }

    pub(crate) fn remote(
        cfg: Arc<InstanceCfg>,
        multi_instance: bool,
        link: Arc<super::client::Link>,
    ) -> Self {
        Self {
            id: Arc::from(cfg.id.as_str()),
            cfg,
            multi_instance,
            via: Via::Remote(link),
        }
    }

    /// `call_engine_rpc_str`: a counted call, bounded to 10 s.
    pub(crate) async fn rpc_str(
        &self,
        proc_name: &str,
        args: Vec<(&str, Value)>,
    ) -> Result<String, Error> {
        match &self.via {
            Via::Local { db, inst } => {
                crate::call_engine_rpc_str(db, inst, proc_name, args).await
            }
            Via::Remote(link) => {
                let args = string_args(args)?;
                // bfdb bounds the call itself at 10 s; a little more for the hop
                let v = link
                    .call(
                        |id| Msg::Rpc {
                            id,
                            inst: self.id.to_string(),
                            proc_name: proc_name.into(),
                            args,
                            optional: false,
                        },
                        Duration::from_secs(12),
                    )
                    .await
                    .map_err(|e| Error(e.into_anyhow()))?;
                v.as_str()
                    .map(str::to_string)
                    .ok_or_else(|| Error(anyhow::anyhow!("unexpected RPC reply: {v}")))
            }
        }
    }

    /// `StatsDb::call_engine_rpc_optional`: uncounted, with the engine's own
    /// refusal kept apart from a failed call. Not bounded here, like the
    /// local call; callers put their own timeout on it.
    pub(crate) async fn rpc_optional(
        &self,
        proc_name: &str,
        args: Vec<(&str, Value)>,
    ) -> anyhow::Result<Answer> {
        match &self.via {
            Via::Local { db, inst } => {
                match db.call_engine_rpc_optional(inst, proc_name, args).await? {
                    Value::String(s) => Ok(Answer::Str(s.to_string())),
                    Value::Error(e) => Ok(Answer::Refused(e.to_string())),
                    other => anyhow::bail!("unexpected RPC reply: {other:?}"),
                }
            }
            Via::Remote(link) => {
                let args = string_args(args).map_err(|e| e.0)?;
                let r = link
                    .call(
                        |id| Msg::Rpc {
                            id,
                            inst: self.id.to_string(),
                            proc_name: proc_name.into(),
                            args,
                            optional: true,
                        },
                        Duration::from_secs(20),
                    )
                    .await;
                match r {
                    Ok(v) => Ok(Answer::Str(v.as_str().unwrap_or_default().to_string())),
                    // bfdb sends the engine's own refusal back as a 422
                    Err(e) if e.status == 422 => Ok(Answer::Refused(e.msg)),
                    Err(e) => Err(e.into_anyhow()),
                }
            }
        }
    }
}
