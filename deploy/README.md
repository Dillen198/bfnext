# Hosting bfweb/bfsite separately from bfdb

By default `bfdb.exe` embeds and serves `bfweb` (the dashboard) and `bfsite`
(the public site) itself, all on one origin — that's how `bfsystem.ps1` runs
today, and nothing about that changes unless you opt in below.

This setup instead splits things in two:

- **`bfdb` stays exactly where it is today** — on the Windows machine next
  to the DCS server. It can't be moved into a generic Linux container: it
  pulls in `mlua` transitively (via `dcso3`/`bfprotocols`) and, per this
  project's build setup, links dynamically against DCS's own Lua DLL. It
  just gets an HTTPS front door and a CORS allow-list.
- **`bfweb` and `bfsite`** become plain static builds — no Rust, no Lua —
  deployable to any Linux host (DigitalOcean droplet, App Platform, etc.).

```
 players' browsers
        │
        ├──► https://dashboard.example.com  (bfweb, static, any host)
        ├──► https://example.com            (bfsite, static, any host)
        │            │
        │            │  fetch/WebSocket, cross-origin, credentials: include
        │            ▼
        └──► https://api.example.com  ──►  Caddy (TLS) ──►  bfdb.exe:8765 (localhost)
                                                                   on the game server machine
```

## 1. Backend (bfdb, on the game server machine)

1. Bind `bfdb` to localhost only — Caddy will be the public-facing TLS
   endpoint. In `bfsystem.ps1`, change:
   ```powershell
   $listenAddress = "127.0.0.1:8765"
   ```
2. Add the new `--cors-origin` flag (repeat it for each origin that needs to
   call the API with credentials — your dashboard and your site):
   ```powershell
   $argList += "--cors-origin", "https://dashboard.example.com"
   $argList += "--cors-origin", "https://example.com"
   ```
   This does two things: it switches CORS from same-origin-only to an
   explicit allow-list, and it switches the session cookie from
   `SameSite=Lax` to `SameSite=None; Secure` (required for a cross-site
   fetch to carry the cookie at all). `Secure` means the cookie is only ever
   sent over HTTPS — which is exactly what Caddy provides in front of bfdb.
3. Point DNS for `api.example.com` at this machine's public IP, and forward
   ports 80 + 443 to it if you're behind a router/firewall.
4. Install [Caddy](https://caddyserver.com/docs/install#windows) and run it
   with the config in `deploy/backend/Caddyfile` (edit the hostname first):
   ```
   caddy run --config deploy/backend/Caddyfile
   ```
   Caddy handles Let's Encrypt automatically — no manual cert management,
   and no need to pass `--cert`/`--key` to `bfdb` itself.
5. Restart `bfdb` (re-run `bfsystem.ps1`) with the updated args.

Discord OAuth, if you use it: update `--discord-redirect-uri` to
`https://api.example.com/api/auth/callback` (bfdb handles the OAuth
callback, not the dashboard) and update the redirect URI in your Discord
app settings to match.

## 2. Frontend (bfweb + bfsite, on the separate host)

Both are ordinary Vite static builds now. `VITE_API_BASE` is the only new
piece — set it to bfdb's public HTTPS origin, no trailing slash.

**Using Docker (recommended, artifacts already in this repo):**

```bash
# on the target host, with this repo cloned to /opt/bfnext-vector
cd /opt/bfnext-vector
echo "VITE_API_BASE=https://api.example.com" > deploy/frontend/.env
docker compose -f deploy/frontend/docker-compose.yml up -d --build
```

This builds and runs two nginx containers: `dashboard` on port 8081
(bfweb) and `site` on port 8082 (bfsite). Put your own reverse proxy /
load balancer in front of them for TLS + the real hostnames
(`dashboard.example.com` → :8081, `example.com` → :8082) — e.g. Caddy or
nginx with Let's Encrypt, or your host's managed load balancer.

To have this survive reboots, install the systemd unit:
```bash
sudo cp deploy/frontend/vector-dashboard.service /etc/systemd/system/
sudo systemctl daemon-reload
sudo systemctl enable --now vector-dashboard
```
(edit `WorkingDirectory` in that file first if you didn't clone to
`/opt/bfnext-vector`)

**Without Docker:** `npm ci && VITE_API_BASE=https://api.example.com npm run build`
in `bfweb/` and `bfsite/` (bfsite additionally needs `--base=/` passed to
`vite build` instead of its default `/site/`), then serve each `dist/`
folder with any static file server.

## Recon intel photo storage

Recon Intel (TARPS) uploads default to living as blobs inside the stats
DB. TARPS PNGs are large and a busy round can hold hundreds, so for a real
deployment pass `--intel-dir <path>` and bfdb stores them as files there
instead, keeping only a small index row in the DB. bfdb creates the
directory if missing and clears it on a campaign reset. Point it at a disk
with room to spare (a few GB per active round); back it up alongside the
`--db` directory if you want the imagery to survive a rebuild.

## Reverting to embedded mode

Just don't pass `--cors-origin`, keep `--listen-address 0.0.0.0:<port>`,
and keep using the embedded `bfweb`/`bfsite` bundles that ship inside
`bfdb.exe` — nothing above is required for that to keep working.
