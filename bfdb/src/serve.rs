//! The listeners: plain HTTP, or TLS with `--cert`/`--key`, on hyper 1 in
//! front of the warp filter chain.
//!
//! warp 0.4 dropped its own TLS server, and the remote address it records
//! for `warp::addr::remote()` can only be set by its own accept loop. So
//! bfdb accepts connections itself (rustls 0.23 through tokio-rustls, the
//! TLS stack reqwest already brings), puts the peer's address on each
//! request as a [`PeerAddr`] extension, and hands the request to
//! `warp::service`. [`peer_addr`] reads either, so `warp::test` requests
//! (which set warp's own) and served ones look the same to the routes.

use anyhow::{Context as _, Result};
use std::{convert::Infallible, net::SocketAddr, path::Path, sync::Arc, time::Duration};
use tokio::net::TcpListener;
use tokio_rustls::rustls;
use warp::Filter;

/// The address a served request came from.
#[derive(Clone, Copy, Debug)]
pub(crate) struct PeerAddr(pub(crate) SocketAddr);

/// The peer's address: warp's own when it set one (`warp::test`), else ours.
pub(crate) fn peer_addr(
) -> impl Filter<Extract = (Option<SocketAddr>,), Error = Infallible> + Clone {
    warp::addr::remote().and(warp::ext::optional::<PeerAddr>()).map(
        |warp: Option<SocketAddr>, ours: Option<PeerAddr>| warp.or(ours.map(|p| p.0)),
    )
}

/// A TLS client gets this long to finish its handshake. Without a bound, a
/// connection that opens and says nothing holds a task forever.
const TLS_HANDSHAKE: Duration = Duration::from_secs(15);

/// The server certificate chain and key from PEM files (`--cert`, `--key`).
/// The key may be PKCS#8, PKCS#1 (RSA) or SEC1 (EC), as warp 0.3 took.
pub(crate) fn load_tls(cert: &Path, key: &Path) -> Result<Arc<rustls::ServerConfig>> {
    let cert_pem = std::fs::read(cert)
        .with_context(|| format!("reading --cert {}", cert.display()))?;
    let key_pem =
        std::fs::read(key).with_context(|| format!("reading --key {}", key.display()))?;
    tls_config(&cert_pem, &key_pem)
}

/// [`load_tls`] from PEM bytes.
pub(crate) fn tls_config(
    cert_pem: &[u8],
    key_pem: &[u8],
) -> Result<Arc<rustls::ServerConfig>> {
    use rustls::pki_types::{pem::PemObject, CertificateDer, PrivateKeyDer};
    let chain = CertificateDer::pem_slice_iter(cert_pem)
        .collect::<std::result::Result<Vec<_>, _>>()
        .map_err(|e| anyhow::anyhow!("--cert is not a PEM certificate chain: {e:?}"))?;
    if chain.is_empty() {
        anyhow::bail!("--cert holds no certificate");
    }
    let key = PrivateKeyDer::from_pem_slice(key_pem).map_err(|e| {
        anyhow::anyhow!(
            "--key holds no usable private key (PKCS#8, RSA or EC PEM): {e:?}"
        )
    })?;
    let provider = Arc::new(rustls::crypto::ring::default_provider());
    let mut cfg = rustls::ServerConfig::builder_with_provider(provider)
        .with_safe_default_protocol_versions()
        .context("TLS protocol versions")?
        .with_no_client_auth()
        .with_single_cert(chain, key)
        .context("--cert and --key do not make a usable pair")?;
    // as warp 0.3 offered: HTTP/2 when the client wants it, else HTTP/1.1
    cfg.alpn_protocols = vec![b"h2".to_vec(), b"http/1.1".to_vec()];
    Ok(Arc::new(cfg))
}

/// Serve `filter` on `addr` until the future is dropped.
pub(crate) async fn serve<F>(
    filter: F,
    addr: SocketAddr,
    tls: Option<Arc<rustls::ServerConfig>>,
) -> Result<()>
where
    F: Filter + Clone + Send + Sync + 'static,
    F::Extract: warp::Reply,
{
    let listener =
        TcpListener::bind(addr).await.with_context(|| format!("binding {addr}"))?;
    serve_on(listener, filter, tls).await;
    Ok(())
}

/// [`serve`] on a listener that is already bound (tests bind port 0).
pub(crate) async fn serve_on<F>(
    listener: TcpListener,
    filter: F,
    tls: Option<Arc<rustls::ServerConfig>>,
) where
    F: Filter + Clone + Send + Sync + 'static,
    F::Extract: warp::Reply,
{
    let acceptor = tls.map(tokio_rustls::TlsAcceptor::from);
    loop {
        let (tcp, peer) = match listener.accept().await {
            Ok(c) => c,
            Err(e) => {
                // EMFILE and friends: back off a moment instead of spinning
                log::warn!("accept failed: {e}");
                tokio::time::sleep(Duration::from_millis(100)).await;
                continue;
            }
        };
        let svc = warp::service(filter.clone());
        let acceptor = acceptor.clone();
        tokio::spawn(async move {
            let _ = tcp.set_nodelay(true);
            match acceptor {
                None => connection(hyper_util::rt::TokioIo::new(tcp), peer, svc).await,
                Some(acceptor) => {
                    match tokio::time::timeout(TLS_HANDSHAKE, acceptor.accept(tcp)).await
                    {
                        Ok(Ok(tls)) => {
                            connection(hyper_util::rt::TokioIo::new(tls), peer, svc).await
                        }
                        Ok(Err(e)) => {
                            log::debug!("TLS handshake with {peer} failed: {e}")
                        }
                        Err(_) => log::debug!("TLS handshake with {peer} timed out"),
                    }
                }
            }
        });
    }
}

/// One connection, HTTP/1.1 or HTTP/2, with WebSocket upgrades.
async fn connection<I, S>(io: I, peer: SocketAddr, svc: S)
where
    I: hyper::rt::Read + hyper::rt::Write + Unpin + Send + 'static,
    S: tower_service::Service<
            http::Request<hyper::body::Incoming>,
            Response = warp::reply::Response,
            Error = Infallible,
        > + Clone
        + Send
        + 'static,
    S::Future: Send + 'static,
{
    let svc = hyper::service::service_fn(
        move |mut req: http::Request<hyper::body::Incoming>| {
            req.extensions_mut().insert(PeerAddr(peer));
            let mut svc = svc.clone();
            svc.call(req)
        },
    );
    if let Err(e) =
        hyper_util::server::conn::auto::Builder::new(hyper_util::rt::TokioExecutor::new())
            .serve_connection_with_upgrades(io, svc)
            .await
    {
        log::debug!("connection from {peer}: {e}");
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    // Throwaway self-signed certificate for `localhost` / 127.0.0.1, made for
    // these tests only (openssl, EC P-256, valid to 2126). Not used anywhere
    // else and protects nothing.
    const CERT: &str = "\
-----BEGIN CERTIFICATE-----
MIIBvjCCAWWgAwIBAgIUdHIkUq7rKvjv9/0y/4mDQQA7clgwCgYIKoZIzj0EAwIw
FDESMBAGA1UEAwwJbG9jYWxob3N0MCAXDTI2MTAyNzA5MDUyNFoYDzIxMjYxMDAz
MDkwNTI0WjAUMRIwEAYDVQQDDAlsb2NhbGhvc3QwWTATBgcqhkjOPQIBBggqhkjO
PQMBBwNCAASYRkNH69jDakcm4dkgAj0itfBzytmgKosH0AuiRvUWrbaUg3xpKirQ
6Y3Wq2BpxL5nl3EDeY6zZKvQA/U5IaqLo4GSMIGPMB0GA1UdDgQWBBTKg85hxDEb
IO84yy1ecYyaCoViYTAfBgNVHSMEGDAWgBTKg85hxDEbIO84yy1ecYyaCoViYTAa
BgNVHREEEzARgglsb2NhbGhvc3SHBH8AAAEwDAYDVR0TAQH/BAIwADAOBgNVHQ8B
Af8EBAMCB4AwEwYDVR0lBAwwCgYIKwYBBQUHAwEwCgYIKoZIzj0EAwIDRwAwRAIg
LH7KRpGuzQ97qGIGnVJf+JOkfGDzNqv3DNeKwztBGhYCIF8Z4fe3rCIce5WVt7g2
O1Gi89xGQBECtEK2hcR2nT8t
-----END CERTIFICATE-----
    ";
    const KEY_PKCS8: &str = "\
-----BEGIN PRIVATE KEY-----
MIGHAgEAMBMGByqGSM49AgEGCCqGSM49AwEHBG0wawIBAQQgy8icm9zixuKXA40S
4TsDpJaHfCAjXHFl+24LfywDvlOhRANCAASYRkNH69jDakcm4dkgAj0itfBzytmg
KosH0AuiRvUWrbaUg3xpKirQ6Y3Wq2BpxL5nl3EDeY6zZKvQA/U5IaqL
-----END PRIVATE KEY-----
    ";
    const KEY_SEC1: &str = "\
-----BEGIN EC PRIVATE KEY-----
MHcCAQEEIMvInJvc4sbilwONEuE7A6SWh3wgI1xxZftuC38sA75ToAoGCCqGSM49
AwEHoUQDQgAEmEZDR+vYw2pHJuHZIAI9IrXwc8rZoCqLB9ALokb1Fq22lIN8aSoq
0OmN1qtgacS+Z5dxA3mOs2Sr0AP1OSGqiw==
-----END EC PRIVATE KEY-----
    ";

    /// What a served request saw of its peer: the client address and
    /// whether it counts as a direct loopback caller.
    fn whoami() -> impl Filter<Extract = (String,), Error = warp::Rejection> + Clone {
        crate::websec::with_client_ip()
            .and(crate::websec::with_direct_loopback())
            .map(|ip: std::net::IpAddr, direct: bool| format!("{ip} {direct}"))
    }

    async fn listen<F>(f: F, tls: Option<Arc<rustls::ServerConfig>>) -> u16
    where
        F: Filter + Clone + Send + Sync + 'static,
        F::Extract: warp::Reply,
    {
        let l = TcpListener::bind(("127.0.0.1", 0)).await.unwrap();
        let port = l.local_addr().unwrap().port();
        tokio::spawn(serve_on(l, f, tls));
        port
    }

    #[test]
    fn tls_takes_pkcs8_and_ec_keys() {
        assert!(tls_config(CERT.as_bytes(), KEY_PKCS8.as_bytes()).is_ok());
        assert!(tls_config(CERT.as_bytes(), KEY_SEC1.as_bytes()).is_ok());
        let cfg = tls_config(CERT.as_bytes(), KEY_PKCS8.as_bytes()).unwrap();
        assert_eq!(cfg.alpn_protocols, vec![b"h2".to_vec(), b"http/1.1".to_vec()]);
    }

    #[test]
    fn tls_refuses_what_is_not_a_cert_and_key() {
        let e = tls_config(b"not a pem", KEY_PKCS8.as_bytes()).unwrap_err().to_string();
        assert!(e.contains("--cert"), "{e}");
        // the key file swapped in for the cert
        assert!(tls_config(KEY_PKCS8.as_bytes(), KEY_PKCS8.as_bytes()).is_err());
        let e = tls_config(CERT.as_bytes(), CERT.as_bytes()).unwrap_err().to_string();
        assert!(e.contains("--key"), "{e}");
        let e = load_tls(Path::new("no/such/cert.pem"), Path::new("no/such/key.pem"))
            .unwrap_err()
            .to_string();
        assert!(e.contains("reading --cert"), "{e}");
    }

    #[tokio::test(flavor = "multi_thread")]
    async fn plain_http_sees_the_peer_address() {
        let port = listen(whoami(), None).await;
        let body = reqwest::get(format!("http://127.0.0.1:{port}/"))
            .await
            .unwrap()
            .text()
            .await
            .unwrap();
        assert_eq!(body, "127.0.0.1 true");
    }

    #[tokio::test(flavor = "multi_thread")]
    async fn tls_serves_and_sees_the_peer_address() {
        let tls = tls_config(CERT.as_bytes(), KEY_PKCS8.as_bytes()).unwrap();
        let port = listen(whoami(), Some(tls)).await;
        let client = reqwest::Client::builder()
            .add_root_certificate(
                reqwest::Certificate::from_pem(CERT.as_bytes()).unwrap(),
            )
            .build()
            .unwrap();
        let r = client.get(format!("https://localhost:{port}/")).send().await.unwrap();
        assert_eq!(r.status(), 200);
        assert_eq!(r.text().await.unwrap(), "127.0.0.1 true");
        // plain HTTP to the TLS port is not answered
        assert!(reqwest::get(format!("http://127.0.0.1:{port}/")).await.is_err());
    }

    #[tokio::test(flavor = "multi_thread")]
    async fn a_forwarded_request_is_not_direct_loopback() {
        let port = listen(whoami(), None).await;
        let body = reqwest::Client::new()
            .get(format!("http://127.0.0.1:{port}/"))
            .header("x-forwarded-for", "203.0.113.9")
            .send()
            .await
            .unwrap()
            .text()
            .await
            .unwrap();
        // loopback is a trusted proxy by default (Caddy on the same box), so
        // the forwarded address is the client -- but it is no longer a direct
        // loopback caller
        assert_eq!(body, "203.0.113.9 false");
    }

    /// The upgrade goes through hyper's connection upgrade, which
    /// `warp::test::ws` never touches: a raw handshake on a real socket.
    #[tokio::test(flavor = "multi_thread")]
    async fn websocket_upgrade_over_a_real_connection() {
        use tokio::io::{AsyncReadExt, AsyncWriteExt};
        let echo = warp::path("ws").and(warp::ws()).map(|ws: warp::ws::Ws| {
            ws.on_upgrade(|socket| async move {
                use futures::{SinkExt, StreamExt};
                let (mut tx, mut rx) = socket.split();
                if let Some(Ok(m)) = rx.next().await {
                    let _ = tx.send(m).await;
                }
            })
        });
        let port = listen(echo, None).await;
        let mut s = tokio::net::TcpStream::connect(("127.0.0.1", port)).await.unwrap();
        s.write_all(
            concat!(
                "GET /ws HTTP/1.1\r\nHost: localhost\r\nUpgrade: websocket\r\nConnection: Upgrade\r\n",
                "Sec-WebSocket-Key: dGhlIHNhbXBsZSBub25jZQ==\r\nSec-WebSocket-Version: 13\r\n\r\n",
            )
            .as_bytes(),
        )
        .await
        .unwrap();
        let mut buf = vec![0u8; 1024];
        let n = s.read(&mut buf).await.unwrap();
        let head = String::from_utf8_lossy(&buf[..n]).to_ascii_lowercase();
        assert!(head.starts_with("http/1.1 101"), "{head}");
        // RFC 6455's own example key and answer
        assert!(
            head.contains("sec-websocket-accept: s3pplmbitxaq9kygzzhzrbk+xoo="),
            "{head}"
        );
        // a masked text frame "hi", echoed back unmasked
        s.write_all(&[0x81, 0x82, 1, 2, 3, 4, b'h' ^ 1, b'i' ^ 2]).await.unwrap();
        let mut f = [0u8; 4];
        s.read_exact(&mut f).await.unwrap();
        assert_eq!(f, [0x81, 2, b'h', b'i']);
    }
}
