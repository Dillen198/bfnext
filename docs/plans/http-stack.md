# One HTTP stack (warp 0.3 and its hyper 0.14 tree)

Status: **done** (audit finding f129). bfdb is on warp 0.4.3 (hyper 1,
http 1). This page records what changed and why, for whoever touches the
listener next.

## Why

`cargo tree -d` showed two of most of the HTTP stack:

| crate | via warp 0.3 (bfdb) | via reqwest 0.12 |
| --- | --- | --- |
| hyper | 0.14 | 1 |
| http | 0.2 | 1 |
| rustls | 0.22 (tokio-rustls 0.25) | 0.23 (tokio-rustls 0.26) |

Two TLS stacks in one binary means two sets of security updates to follow,
and hyper 0.14 / rustls 0.22 get fewer of them.

After the upgrade `cargo tree -p bfdb -d` lists none of hyper, http,
http-body, rustls, rustls-webpki, tokio-rustls, tungstenite or
tokio-tungstenite twice; hyper 0.14, http 0.2, rustls 0.22, tokio-rustls 0.25,
tokio-tungstenite 0.21, multer and their leaves left Cargo.lock.

## What changed

- **warp 0.4.3** with features `server`, `websocket`, `test` (`test` is used
  outside tests too: the public snapshot renders wiki pages through the
  route chain). The filter API carried over unchanged; no route code moved.
- **TLS and the listener are bfdb's own** (`bfdb/src/serve.rs`). warp 0.4
  has no TLS server, and its accept trait is private, so bfdb accepts
  connections itself, terminates TLS with tokio-rustls 0.26 / rustls 0.23
  (ring provider, the stack reqwest already uses) and serves each connection
  with hyper-util's auto (HTTP/1.1 + HTTP/2) builder with upgrades, exactly
  as warp's own loop does. ALPN offers `h2` and `http/1.1`, as warp 0.3 did.
  `--cert`/`--key` still take PEM files; the key may be PKCS#8, PKCS#1 or
  SEC1. A bad pair now stops startup with a message instead of warp's panic.
  A TLS handshake must finish in 15 s.
- **Peer address.** warp's `warp::addr::remote()` is only filled by warp's
  own accept loop, so the listener puts a `PeerAddr` extension on every
  request and `serve::peer_addr()` reads either (warp's is what
  `warp::test` sets). `websec::with_client_ip` and `with_direct_loopback`
  use it, so rate limits, loopback-only routes and trusted-proxy handling are
  unchanged.
- **Streaming downloads** (range and replay Tacview files) use
  `warp::reply::stream`; warp 0.4 no longer exposes hyper's `Body`.
- **Static files.** rust-embed's `warp-ex` feature was never used (bfdb serves
  the embedded files through its own filter); it is dropped.
- The `--site-address` server goes through the same listener.

## Tests

`serve.rs` tests: PEM parsing (PKCS#8 and SEC1 keys, bad cert, bad key,
missing files), a real TLS round trip with reqwest trusting a throwaway
localhost certificate, the peer address seen over plain HTTP and TLS, a
forwarded request not counting as direct loopback, and a WebSocket upgrade
plus one echoed frame over a real socket (warp's in-memory `warp::test::ws`
does not go through hyper's upgrade). `route_tests.rs` adds a CORS preflight
(allowed and refused origin, allowed headers, exposed `x-total-count`), the
`/ws/logs` admin gate and history replay, and the 413 on an oversized JSON
body.

## Still to check by hand

Web apps against a local bfdb (bfweb, bfsite, bfwiki, bfrange-web), the
websocket feeds, Discord login, uploads (news images, replays, intel), and
TLS with the production certificate if a box serves TLS itself (the usual
setup has Caddy in front, which is unaffected).
