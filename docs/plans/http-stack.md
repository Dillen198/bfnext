# Plan: one HTTP stack (warp 0.3 and its hyper 0.14 tree)

Status: plan only (audit finding f129, the warp part; the zip/zstd part is
done).

## Why

`cargo tree -d` shows two of most of the HTTP stack:

| crate | via warp 0.3 (bfdb, rust-embed's `warp-ex`) | via reqwest 0.12 |
| --- | --- | --- |
| hyper | 0.14 | 1 |
| http | 0.2 | 1 |
| rustls | 0.22 (tokio-rustls 0.25) | 0.23 (tokio-rustls 0.26) |

Two TLS stacks in one binary means two sets of security updates to follow,
and hyper 0.14 / rustls 0.22 get fewer of them. It also costs build time and
binary size in bfdb.

## Scope in bfdb

warp is used in five files, overwhelmingly through its filter API:
`warp::path` (~195 uses), `reply`, `body`, `query`, `http`, `post`/`get`,
`ws` (5 websocket endpoints: groundfeed and four in `main.rs`), `header`,
`reject`, `cors`, `redirect`, and `serve` (3) with the `tls` feature. Static
assets come from rust-embed's `warp-ex` integration (bfweb and bfsite
builds).

## Options

1. **warp's hyper 1 line (0.4; check crates.io for its current state).**
   Same filter API, so most of bfdb is untouched. Check its release notes
   before starting for removed pieces, above all whether `serve(..).tls()`
   is still there (bfdb serves TLS itself), and whether rust-embed's
   `warp-ex` supports it. If `warp-ex` lags, serving the embedded files
   through a small filter of our own (`rust_embed::Embed::get` plus a
   content type from `mime_guess`, both already dependencies) replaces it.
2. **axum.** The mainstream hyper 1 server; a rewrite of every route
   (filters become handlers and extractors) and of the websocket code. More
   work, and only worth it if warp's hyper 1 line turns out to be thin.

Recommendation: option 1. Try it on a branch: bump warp, fix compile errors,
and if TLS serving or `warp-ex` blocks it, put TLS in front with
`tokio-rustls` 0.26 (the version reqwest already brings) and serve with
hyper-util.

## Steps

1. Branch; bump `warp` in `[workspace.dependencies]`; replace `warp-ex` if
   needed.
2. Fix the TLS listener (`serve` with `tls()`), the websocket upgrades and
   any `warp::hyper` / `warp::http` uses (these now name hyper 1 / http 1
   types).
3. `cargo tree -d` should no longer list hyper, http, rustls or
   tokio-rustls twice.
4. Test: bfdb's test suite, then the web apps against a local bfdb (bfweb,
   bfsite, bfwiki, bfrange-web), the websocket feeds, login (OAuth
   redirect), file uploads (news images, replays), and TLS with the
   production certificate setup (Caddy in front, or bfdb's own TLS).

## Risk

Mostly in the edges: TLS configuration, websocket close handling, request
body limits and CORS defaults that changed between hyper versions. The
filter logic itself carries over.
