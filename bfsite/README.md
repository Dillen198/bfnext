# bfsite

The public Vector Strike website (React + Vite). It is deployed on its own to
Vercel (`vectorstrike.org`) and is also embedded in `bfdb`, which serves it
under `/site/`.

## Development

```bash
npm ci
npm run dev                 # http://localhost:5174, /api proxied to a local bfdb on :8880
npm run dev -- --mode live  # same, but proxied to the production API (see .env.live)
```

Checks, the same ones CI runs:

```bash
npm run typecheck
npm run lint
npm test
npm run build               # -> dist/
```

## Environment

| Variable | Default | What it does |
|----------|---------|--------------|
| `VITE_API_BASE` | `''` (same origin) | bfdb's origin, no trailing slash. Set on Vercel to `https://api.vectorstrike.org`. |
| `VITE_STATUS_URL` | `/status.json` | Where the server status file is read from. |
| `VITE_BFDB_URL` | `http://localhost:8880` | Dev server only: where `/api` is proxied. |

## Building before bfdb

`dist/` is not checked in. `bfdb` embeds it at compile time with
`rust-embed` (`#[folder = "../bfsite/dist/"]` in `bfdb/src/main.rs`) and does
not compile without it, so build this app first:

```bash
cd bfsite && npm ci && npm run build
cargo build --release --package=bfdb
```

If you only need bfdb to compile and do not care about the embedded site, an
empty `bfsite/dist/` directory is enough; that is what CI does.
