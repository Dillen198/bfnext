//! The netidx client configs bfdb subscribes (and calls engine RPCs) through.
//!
//! One Subscriber per distinct client config: the default one, plus one per
//! instance that reaches its engine through a resolver of its own
//! (`netidx_config`, a DCS server on another PC). Each can be switched to
//! mutual TLS (deploy/netidx-tls.md): `--netidx-tls <file>` for the default
//! config, an instance's `netidx_tls` for its own. Neither set, every config
//! loads exactly as before (normally Anonymous).

use crate::instance::InstanceCfg;
use anyhow::{bail, Context, Result};
use fowlbg::netidx::{load_config, Tls};
use netidx::subscriber::{Subscriber, SubscriberBuilder};
use std::{
    collections::{BTreeMap, HashMap},
    path::{Path, PathBuf},
    sync::Arc,
};

/// `--netidx-tls`: a JSON file holding one TLS block (the shape of
/// `fowlbg::netidx::Tls`, the same as `netidx_tls` in the engine configs).
pub(crate) fn load_tls_file(path: Option<&Path>) -> Result<Option<Tls>> {
    let Some(path) = path else { return Ok(None) };
    let txt = std::fs::read_to_string(path)
        .with_context(|| format!("reading --netidx-tls {}", path.display()))?;
    let tls: Tls = serde_json::from_str(&txt)
        .with_context(|| format!("parsing --netidx-tls {}", path.display()))?;
    tls.validate().with_context(|| format!("--netidx-tls {}", path.display()))?;
    Ok(Some(tls))
}

/// The TLS block each distinct client config gets, for the instances with a
/// live engine (`base`). An instance's own `netidx_tls` wins; the default
/// config (no `netidx_config`) otherwise takes `default_tls`. Two instances
/// on one client config must agree, since they share its Subscriber.
/// Each config also carries the id of the first instance on it.
pub(crate) fn plan(
    instances: &[Arc<InstanceCfg>],
    default_tls: Option<&Tls>,
) -> Result<BTreeMap<Option<PathBuf>, (Option<Tls>, String)>> {
    let mut out: BTreeMap<Option<PathBuf>, (Option<Tls>, String)> = BTreeMap::new();
    for i in instances.iter().filter(|i| i.base.is_some()) {
        if let Some(t) = &i.netidx_tls {
            t.validate().with_context(|| format!("instance {:?}: netidx_tls", i.id))?;
        }
        let tls = match (&i.netidx_tls, &i.netidx_config) {
            (Some(t), _) => Some(t.clone()),
            (None, None) => default_tls.cloned(),
            (None, Some(_)) => None,
        };
        match out.get(&i.netidx_config) {
            None => {
                out.insert(i.netidx_config.clone(), (tls, i.id.clone()));
            }
            Some((prev, first)) if *prev != tls => {
                let which = match &i.netidx_config {
                    Some(p) => format!("netidx_config {}", p.display()),
                    None => "the default netidx client config".to_string(),
                };
                bail!(
                    "instances {first:?} and {:?} both use {which} but with different netidx_tls; \
                     they share one connection, so give them the same block (or none)",
                    i.id
                )
            }
            Some(_) => (),
        }
    }
    Ok(out)
}

/// One Subscriber per client config in [`plan`].
pub(crate) fn subscribers(
    instances: &[Arc<InstanceCfg>],
    default_tls: Option<&Tls>,
) -> Result<HashMap<Option<PathBuf>, Subscriber>> {
    let mut subscribers = HashMap::new();
    for (path, (tls, id)) in plan(instances, default_tls)? {
        let what = match &path {
            Some(p) => format!("netidx config {}", p.display()),
            None => "the default netidx config".to_string(),
        };
        let ncfg = load_config(path.as_deref(), tls.as_ref())
            .with_context(|| format!("instance {id:?}: loading {what}"))?;
        if let Some(p) = &path {
            log::info!("instance {id:?}: netidx via {}", p.display());
        }
        if let Some(t) = &tls {
            log::info!(
                "netidx: {what} uses TLS to resolver {} as {}",
                t.resolver_name,
                t.certificate
            );
        }
        subscribers.insert(path, SubscriberBuilder::new().config(ncfg).build()?);
    }
    Ok(subscribers)
}

#[cfg(test)]
mod tests {
    use super::*;

    fn tls(resolver: &str, cert: &str) -> Tls {
        Tls {
            trusted: "C:/fowl/tls/ca.pem".into(),
            certificate: format!("C:/fowl/tls/{cert}.crt"),
            private_key: format!("C:/fowl/tls/{cert}.key"),
            resolver_name: resolver.into(),
            domain: None,
            askpass: None,
        }
    }

    fn inst(v: serde_json::Value) -> Arc<InstanceCfg> {
        Arc::new(serde_json::from_value(v).unwrap())
    }

    fn fleet() -> Vec<Arc<InstanceCfg>> {
        vec![
            inst(serde_json::json!({"id": "vs1", "base": "/local/fowl/vs1"})),
            inst(serde_json::json!({"id": "vs2", "base": "/local/fowl/vs2"})),
            inst(serde_json::json!({"id": "range", "base": "/local/fowl/range",
                                    "netidx_config": "C:/bfdb/netidx.range.json"})),
            // no live engine: never connects
            inst(serde_json::json!({"id": "offline"})),
        ]
    }

    /// Nothing configured: the same client configs as before, none of them TLS.
    #[test]
    fn off_is_anonymous() {
        let p = plan(&fleet(), None).unwrap();
        let keys: Vec<_> = p.keys().cloned().collect();
        assert_eq!(keys, [None, Some(PathBuf::from("C:/bfdb/netidx.range.json"))]);
        assert!(p.values().all(|(t, _)| t.is_none()));
        assert_eq!(p[&None].1, "vs1");
        // an instances file from before this field still parses
        assert!(fleet()[0].netidx_tls.is_none());
    }

    #[test]
    fn default_tls_covers_only_the_default_config() {
        let t = tls("resolver.campaign.fowl", "bfdb.fowl");
        let p = plan(&fleet(), Some(&t)).unwrap();
        assert_eq!(p[&None].0, Some(t));
        assert_eq!(p[&Some(PathBuf::from("C:/bfdb/netidx.range.json"))].0, None);
    }

    #[test]
    fn instance_tls_for_its_own_resolver() {
        let mut f = fleet();
        f[2] = inst(serde_json::json!({"id": "range", "base": "/local/fowl/range",
            "netidx_config": "C:/bfdb/netidx.range.json",
            "netidx_tls": {"trusted": "C:/fowl/tls/ca.pem", "certificate": "C:/fowl/tls/bfdb.fowl.crt",
                           "private_key": "C:/fowl/tls/bfdb.fowl.key", "resolver_name": "resolver.range.fowl"}}));
        let p = plan(&f, None).unwrap();
        assert_eq!(p[&None].0, None, "the campaign resolver stays Anonymous");
        let r = p[&Some(PathBuf::from("C:/bfdb/netidx.range.json"))].0.as_ref().unwrap();
        assert_eq!(r.resolver_name, "resolver.range.fowl");
    }

    #[test]
    fn instances_sharing_a_config_must_agree() {
        let mut f = fleet();
        let mut vs2 = (*f[1]).clone();
        vs2.netidx_tls = Some(tls("resolver.campaign.fowl", "other.fowl"));
        f[1] = Arc::new(vs2);
        let e = plan(&f, None).unwrap_err().to_string();
        assert!(
            e.contains("\"vs1\"")
                && e.contains("\"vs2\"")
                && e.contains("default netidx client config"),
            "{e}"
        );
        // agreeing with the default is fine
        let t = tls("resolver.campaign.fowl", "other.fowl");
        assert!(plan(&f, Some(&t)).is_ok());
    }

    #[test]
    fn a_bad_instance_block_is_reported() {
        let mut f = fleet();
        let mut vs1 = (*f[0]).clone();
        vs1.netidx_tls = Some(Tls {
            domain: Some("example.com".into()),
            ..tls("resolver.campaign.fowl", "bfdb.fowl")
        });
        f[0] = Arc::new(vs1);
        let e = format!("{:?}", plan(&f, None).unwrap_err());
        assert!(e.contains("vs1") && e.contains("not under"), "{e}");
    }

    #[test]
    fn tls_file() {
        assert_eq!(load_tls_file(None).unwrap(), None);
        let dir =
            std::env::temp_dir().join(format!("bfdb-netidx-tls-{}", std::process::id()));
        std::fs::create_dir_all(&dir).unwrap();
        let p = dir.join("tls.json");
        std::fs::write(
            &p,
            serde_json::to_string(&tls("resolver.campaign.fowl", "bfdb.fowl")).unwrap(),
        )
        .unwrap();
        assert_eq!(
            load_tls_file(Some(&p)).unwrap(),
            Some(tls("resolver.campaign.fowl", "bfdb.fowl"))
        );
        std::fs::write(&p, r#"{"trusted": "a"}"#).unwrap();
        assert!(load_tls_file(Some(&p)).is_err());
        let _ = std::fs::remove_dir_all(&dir);
    }
}
