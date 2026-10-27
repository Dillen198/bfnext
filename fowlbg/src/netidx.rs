// Copyright 2026 Dillen Weerasinghe. MIT licensed: see fowlbg/LICENSE.
//! The netidx client config the engines (and bfdb) connect with, and the
//! engine's publisher.
//!
//! Without a [`Tls`] block a config loads exactly as netidx loads it:
//! `client.json` as written, normally Anonymous. With one, the same
//! `client.json` (it still says where the resolvers are) is switched to
//! mutual TLS: every resolver address authenticates as
//! [`Tls::resolver_name`], and this process presents the certificate the
//! block names. See deploy/netidx-tls.md.

use anyhow::{Context, Result, bail};
use netidx::config::{Config, DefaultAuthMech, file};
use serde_derive::{Deserialize, Serialize};
use std::{collections::BTreeMap, path::Path};

/// Mutual TLS for one process's netidx connections. Off when absent.
///
/// The names follow netidx's own `tls` block: `trusted` is the CA bundle,
/// `certificate` and `private_key` this process's identity.
#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord, Serialize, Deserialize)]
#[cfg_attr(feature = "schemars", derive(schemars::JsonSchema))]
#[serde(deny_unknown_fields)]
pub struct Tls {
    /// PEM file with the CA certificate(s) this process trusts: the private
    /// CA every resolver, engine and bfdb certificate is issued by.
    pub trusted: String,
    /// This process's certificate (PEM, exactly one certificate). Its CN and
    /// its single DNS subjectAltName are its netidx identity, the name the
    /// resolver's `perms` grant to, e.g. `engine.range.fowl`.
    pub certificate: String,
    /// The certificate's private key (PEM). An encrypted key needs `askpass`.
    pub private_key: String,
    /// The resolver's identity (its certificate's name), e.g.
    /// `resolver.range.fowl`. Every resolver address in the client config
    /// is switched to TLS under this name.
    pub resolver_name: String,
    /// The peers this identity is presented to: netidx picks an identity by
    /// the name of the server it connects to, and this one is used for every
    /// server named under `domain` (resolvers and publishers alike). Unset,
    /// the last label of `resolver_name` (`fowl` for `resolver.range.fowl`).
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub domain: Option<String>,
    /// A program netidx runs to get the password of an encrypted private key
    /// (it is passed the key's path and prints the password).
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub askpass: Option<String>,
}

fn under(name: &str, domain: &str) -> bool {
    name == domain || name.strip_suffix(domain).is_some_and(|rest| rest.ends_with('.'))
}

impl Tls {
    /// The domain the identity is filed under (see [`Tls::domain`]).
    pub fn domain(&self) -> &str {
        match &self.domain {
            Some(d) => d.trim_matches('.'),
            None => self.resolver_name.rsplit('.').next().unwrap_or(&self.resolver_name),
        }
    }

    /// The checks that need no files: everything is named, and the resolver
    /// is under `domain` (netidx would only say "required identity ... not
    /// found"). Whether the files exist and hold one certificate is checked
    /// by netidx when the config is built ([`load_config`]).
    pub fn validate(&self) -> Result<()> {
        for (key, v) in [
            ("trusted", &self.trusted),
            ("certificate", &self.certificate),
            ("private_key", &self.private_key),
            ("resolver_name", &self.resolver_name),
        ] {
            if v.trim().is_empty() {
                bail!("netidx_tls.{key} is empty")
            }
        }
        if self.resolver_name.contains(char::is_whitespace) || self.resolver_name.starts_with('.') {
            bail!("netidx_tls.resolver_name {:?} is not a DNS name", self.resolver_name)
        }
        let domain = self.domain();
        if domain.is_empty() {
            bail!("netidx_tls.domain is empty")
        }
        if !under(&self.resolver_name, domain) {
            bail!(
                "netidx_tls.resolver_name {:?} is not under netidx_tls.domain {domain:?}: \
                 this identity would never be offered to the resolver",
                self.resolver_name
            )
        }
        Ok(())
    }

    /// Switch a client config (as read from `client.json`) to TLS: the one
    /// identity in `tls`, TLS as the default auth, and TLS to every resolver
    /// address under `resolver_name`. The addresses and base are kept.
    pub fn apply(&self, cfg: &mut file::Config) {
        let domain = self.domain().to_string();
        let identity = file::TlsIdentity {
            trusted: self.trusted.clone(),
            certificate: self.certificate.clone(),
            private_key: self.private_key.clone(),
        };
        cfg.tls = Some(file::Tls {
            default_identity: Some(domain.clone()),
            identities: BTreeMap::from([(domain, identity)]),
            askpass: self.askpass.clone(),
        });
        cfg.default_auth = DefaultAuthMech::Tls;
        for (_, auth) in cfg.addrs.iter_mut() {
            *auth = file::Auth::Tls(self.resolver_name.clone());
        }
    }
}

/// The netidx client config: `path`, else netidx's default search
/// (`NETIDX_CFG`, then the account's `%APPDATA%\netidx\client.json`, ...).
///
/// `tls: None` is exactly `Config::load` / `Config::load_default`, today's
/// behaviour. With TLS the file is switched over by [`Tls::apply`] before
/// netidx validates it, which is when the certificate files are read.
pub fn load_config(path: Option<&Path>, tls: Option<&Tls>) -> Result<Config> {
    let Some(tls) = tls else {
        return match path {
            Some(p) => Config::load(p),
            None => Config::load_default(),
        };
    };
    tls.validate()?;
    let mut f = match path {
        Some(p) => file::Config::load(p)?,
        None => file::Config::load_default()?,
    };
    tls.apply(&mut f);
    Config::from_file(f).context("netidx TLS (netidx_tls)")
}

#[cfg(feature = "runtime")]
pub use publisher::*;

#[cfg(feature = "runtime")]
mod publisher {
    use anyhow::Result;
    use log::info;
    use netidx::{
        config::Config,
        publisher::{BindCfg, Publisher, PublisherBuilder},
    };
    use std::time::Duration;

    /// The waits between attempts to start the publisher. A mission restart
    /// drops the old publisher just before binding the new one, and its
    /// listener lets go of the port a moment later; with an exact bind port
    /// the first bind can lose that race. About 15 s in all before giving up.
    pub const BIND_BACKOFF: &[Duration] = &[
        Duration::from_millis(250),
        Duration::from_millis(500),
        Duration::from_secs(1),
        Duration::from_secs(2),
        Duration::from_secs(4),
        Duration::from_secs(8),
    ];

    /// `attempt` until it succeeds, sleeping `backoff[i]` after failure `i`;
    /// the last failure once `backoff` is used up. An empty `backoff` is one
    /// try.
    pub async fn retry<T, E: std::fmt::Display, Fut>(
        what: &str,
        backoff: &[Duration],
        mut attempt: impl FnMut() -> Fut,
    ) -> std::result::Result<T, E>
    where
        Fut: std::future::Future<Output = std::result::Result<T, E>>,
    {
        let mut waits = backoff.iter();
        loop {
            match attempt().await {
                Ok(v) => return Ok(v),
                Err(e) => match waits.next() {
                    Some(wait) => {
                        info!("{what} failed ({e}), trying again in {wait:?}");
                        tokio::time::sleep(*wait).await;
                    }
                    None => return Err(e),
                },
            }
        }
    }

    /// A publisher on `cfg`, bound per `bind` (the config's
    /// `default_bind_config` when `None`), retried per `backoff`.
    pub async fn start_publisher(
        cfg: &Config,
        bind: Option<BindCfg>,
        backoff: &[Duration],
    ) -> Result<Publisher> {
        retry("starting the netidx publisher", backoff, || {
            let cfg = cfg.clone();
            let bind = bind.clone();
            async move { PublisherBuilder::new(cfg).bind_cfg(bind).build().await }
        })
        .await
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use netidx::{protocol::resolver::Auth, subscriber::DesiredAuth};
    use std::{fs, path::PathBuf};

    /// procman's loopback client config, as it writes it today.
    const ANON: &str = r#"{"base": "/", "addrs": [["127.0.0.1:4564", "Anonymous"]], "default_auth": "Anonymous"}"#;

    fn tls() -> Tls {
        serde_json::from_str(
            r#"{"trusted": "C:/fowl/tls/ca.pem",
                "certificate": "C:/fowl/tls/engine.range.fowl.crt",
                "private_key": "C:/fowl/tls/engine.range.fowl.key",
                "resolver_name": "resolver.range.fowl"}"#,
        )
        .unwrap()
    }

    fn scratch(name: &str, body: &str) -> PathBuf {
        let dir = std::env::temp_dir().join(format!("fowlbg-netidx-{name}-{}", std::process::id()));
        let _ = fs::remove_dir_all(&dir);
        fs::create_dir_all(&dir).unwrap();
        let p = dir.join("client.json");
        fs::write(&p, body).unwrap();
        p
    }

    #[test]
    fn tls_parses_with_defaults() {
        let t = tls();
        assert_eq!(t.domain(), "fowl");
        assert_eq!(t.askpass, None);
        t.validate().unwrap();
        // the defaults are left out when written back
        let s = serde_json::to_string(&t).unwrap();
        assert!(!s.contains("domain") && !s.contains("askpass"), "{s}");
        assert_eq!(serde_json::from_str::<Tls>(&s).unwrap(), t);
    }

    #[test]
    fn tls_rejects_unknown_and_missing_keys() {
        let typo = r#"{"trusted": "a", "certificate": "b", "private_key": "c",
                       "resolver_name": "r.fowl", "ca": "x"}"#;
        assert!(serde_json::from_str::<Tls>(typo).is_err());
        let missing = r#"{"trusted": "a", "certificate": "b", "private_key": "c"}"#;
        assert!(serde_json::from_str::<Tls>(missing).is_err());
    }

    #[test]
    fn tls_validate() {
        let mut t = tls();
        t.domain = Some(".range.fowl.".into());
        assert_eq!(t.domain(), "range.fowl");
        t.validate().unwrap();
        t.domain = Some("campaign.fowl".into());
        let e = t.validate().unwrap_err().to_string();
        assert!(e.contains("not under"), "{e}");
        // a suffix that is not a whole label does not count
        t.domain = Some("owl".into());
        assert!(t.validate().is_err());
        let mut t = tls();
        t.private_key = " ".into();
        assert!(t.validate().unwrap_err().to_string().contains("private_key"));
        let mut t = tls();
        t.resolver_name = "resolver range".into();
        assert!(t.validate().is_err());
    }

    #[test]
    fn apply_switches_every_address_to_tls() {
        let mut f: file::Config = serde_json::from_str(
            r#"{"base": "/", "addrs": [["192.168.1.60:4564", "Anonymous"], ["192.168.1.61:4564", "Anonymous"]],
                "default_auth": "Anonymous", "default_bind_config": "192.168.1.0/24"}"#,
        )
        .unwrap();
        tls().apply(&mut f);
        assert!(matches!(f.default_auth, DefaultAuthMech::Tls));
        assert_eq!(f.addrs.len(), 2);
        for (_, a) in &f.addrs {
            assert!(matches!(a, file::Auth::Tls(n) if n == "resolver.range.fowl"), "{a:?}");
        }
        assert_eq!(f.default_bind_config.as_deref(), Some("192.168.1.0/24"));
        let t = f.tls.unwrap();
        assert_eq!(t.default_identity.as_deref(), Some("fowl"));
        let id = &t.identities["fowl"];
        assert_eq!(id.trusted, "C:/fowl/tls/ca.pem");
        assert_eq!(id.certificate, "C:/fowl/tls/engine.range.fowl.crt");
        assert_eq!(id.private_key, "C:/fowl/tls/engine.range.fowl.key");
    }

    /// Off is today's config, byte for byte: Anonymous everywhere.
    #[test]
    fn off_is_anonymous() {
        let p = scratch("off", ANON);
        let c = load_config(Some(&p), None).unwrap();
        assert!(matches!(c.default_auth(), DesiredAuth::Anonymous));
        assert!(c.tls.is_none());
        assert_eq!(c.addrs.len(), 1);
        assert!(matches!(c.addrs[0].1, Auth::Anonymous));
        assert_eq!(c.addrs[0].0, "127.0.0.1:4564".parse().unwrap());
        let _ = fs::remove_dir_all(p.parent().unwrap());
    }

    /// With TLS on, netidx itself reads the certificate files; a missing one
    /// is an error at load, not a silent fall back to Anonymous.
    #[test]
    fn on_with_missing_files_fails() {
        let p = scratch("missing", ANON);
        let mut t = tls();
        let dir = p.parent().unwrap().to_string_lossy().to_string();
        t.trusted = format!("{dir}/nope-ca.pem");
        t.certificate = format!("{dir}/nope.crt");
        t.private_key = format!("{dir}/nope.key");
        let e = format!("{:?}", load_config(Some(&p), Some(&t)).unwrap_err());
        assert!(e.contains("netidx_tls") && e.contains("trusted"), "{e}");
        let _ = fs::remove_dir_all(p.parent().unwrap());
    }

    #[test]
    fn on_with_a_bad_block_fails_before_reading_files() {
        let mut t = tls();
        t.domain = Some("example.com".into());
        // the client config path does not exist either: validate runs first
        let e = load_config(Some(Path::new("Z:/no/such/client.json")), Some(&t)).unwrap_err();
        assert!(e.to_string().contains("not under"), "{e}");
    }

    #[cfg(feature = "runtime")]
    mod retry {
        use super::super::*;
        use std::{
            sync::atomic::{AtomicUsize, Ordering},
            time::Duration,
        };

        fn rt() -> tokio::runtime::Runtime {
            tokio::runtime::Builder::new_current_thread().enable_all().build().unwrap()
        }

        const FAST: &[Duration] = &[Duration::from_millis(1); 3];

        #[test]
        fn stops_at_the_first_success() {
            let calls = AtomicUsize::new(0);
            let r: std::result::Result<usize, String> = rt().block_on(retry("t", FAST, || {
                let n = calls.fetch_add(1, Ordering::SeqCst);
                async move { if n < 2 { Err(format!("fail {n}")) } else { Ok(n) } }
            }));
            assert_eq!(r, Ok(2));
            assert_eq!(calls.load(Ordering::SeqCst), 3);
        }

        #[test]
        fn gives_up_with_the_last_error() {
            let calls = AtomicUsize::new(0);
            let r: std::result::Result<(), String> = rt().block_on(retry("t", FAST, || {
                let n = calls.fetch_add(1, Ordering::SeqCst);
                async move { Err(format!("fail {n}")) }
            }));
            assert_eq!(r, Err("fail 3".to_string()), "one try plus one per backoff step");
            assert_eq!(calls.load(Ordering::SeqCst), 4);
        }

        #[test]
        fn no_backoff_is_one_try() {
            let calls = AtomicUsize::new(0);
            let r: std::result::Result<(), String> = rt().block_on(retry("t", &[], || {
                calls.fetch_add(1, Ordering::SeqCst);
                async move { Err("no".to_string()) }
            }));
            assert!(r.is_err());
            assert_eq!(calls.load(Ordering::SeqCst), 1);
        }

        /// What a pinned port looks like across a mission restart: the old
        /// listener lets go of the port only after the first bind was tried.
        #[test]
        fn rides_out_a_port_still_held() {
            rt().block_on(async {
                let old = tokio::net::TcpListener::bind("127.0.0.1:0").await.unwrap();
                let addr = old.local_addr().unwrap();
                let release = tokio::spawn(async move {
                    tokio::time::sleep(Duration::from_millis(100)).await;
                    drop(old);
                });
                let tries = AtomicUsize::new(0);
                let backoff = [Duration::from_millis(50); 10];
                let new = retry("binding", &backoff, || {
                    tries.fetch_add(1, Ordering::SeqCst);
                    tokio::net::TcpListener::bind(addr)
                })
                .await
                .expect("bound once the old listener let go");
                assert_eq!(new.local_addr().unwrap(), addr);
                assert!(tries.load(Ordering::SeqCst) > 1, "the first bind should have hit the held port");
                release.await.unwrap();
            });
        }

        #[test]
        fn bind_backoff_is_bounded() {
            let total: Duration = BIND_BACKOFF.iter().sum();
            assert!(total >= Duration::from_secs(5) && total <= Duration::from_secs(30), "{total:?}");
        }
    }
}
