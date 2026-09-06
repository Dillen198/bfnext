# Server Setup (DCS on a Dedicated Server)

This is the production setup where DCS runs on a dedicated game server and `bfdb` + the resolver run either on the same server or a separate stats machine.

## Option A — Everything on the DCS Server

```
DCS Server (e.g. 192.168.1.10)
┌──────────────────────────────────────────────────────────┐
│  DCS (bflib.dll)  ──►  resolver :4564  ◄──  bfdb :8080  │
└──────────────────────────────────────────────────────────┘
                                                │
                                         players browse
                                      http://192.168.1.10:8080
```

Simplest. One machine to manage. Good if the DCS server has spare CPU/RAM.

## Option B — Separate Stats Machine

```
DCS Server (192.168.1.10)          Stats Server (192.168.1.20)
┌─────────────────────┐            ┌─────────────────────────┐
│   DCS (bflib.dll)   │──────────► │  resolver :4564         │
└─────────────────────┘            │  bfdb.exe :8080         │
                                   └─────────────────────────┘
                                              │
                                       players browse
                                   http://192.168.1.20:8080
```

Better if you want to keep DCS server load clean. The resolver and bfdb can run on any Linux or Windows machine on the same network.

---

## Step 1 — Create Config Files

### On the machine running the resolver (stats server or DCS server)

**Resolver config** (`netidx-resolver.json`) — use the server's LAN IP or `0.0.0.0` to accept connections from other machines:

```json
{
    "parent": null,
    "children": [],
    "member_servers": [
        {
            "pid_file": "",
            "addr": "0.0.0.0:4564",
            "max_connections": 768,
            "hello_timeout": 10,
            "reader_ttl": 60,
            "writer_ttl": 120,
            "id_map_type": "DoNotMap",
            "auth": "Anonymous"
        }
    ],
    "perms": {
        "/": {"Anonymous": "swlpd"}
    }
}
```

**Client config** (`%APPDATA%\netidx\client.json` on Windows, `~/.config/netidx/client.json` on Linux) — use the resolver machine's actual IP:

```json
{
    "addrs": [["192.168.1.20:4564", "Anonymous"]],
    "base": "/",
    "default_auth": "Anonymous"
}
```

### On the DCS server (if separate from the resolver machine)

Copy the same `client.json` to `%APPDATA%\netidx\client.json` on the DCS server, pointing at the resolver machine's IP. bflib reads this config to know where to publish.

> **Firewall**: Open port `4564` (TCP) on the resolver machine so other machines can reach it.

---

## Step 2 — Set `netidx_base` in the Campaign Config

On the DCS server, find `<sortie>_CFG` in the mission state folder and set:

```json
{
    "netidx_base": "/fowl-engine",
    ...
}
```

---

## Step 3 — Start the Resolver

On the **stats/resolver machine**:

**Windows (PowerShell)**:
```powershell
netidx resolver-server -f -c C:\fowl\netidx-resolver.json
```

**Linux (systemd)**:
```bash
# /etc/systemd/system/netidx-resolver.service
[Unit]
Description=Netidx Resolver

[Service]
ExecStart=/usr/local/bin/netidx resolver-server -f -c /opt/fowl/netidx-resolver.json
Restart=always

[Install]
WantedBy=multi-user.target
```
```bash
systemctl enable --now netidx-resolver
```

---

## Step 4 — Start bfdb

On the **stats machine** (or same server):

```powershell
.\bfdb.exe `
    --db C:\fowl\campaign.db `
    --listen-address 0.0.0.0:8080 `
    --base "/fowl-engine/Vector Strike"
```

Replace `Vector Strike` with your actual sortie name from the `_CFG` file.

**Linux**:
```bash
./bfdb \
    --db /opt/fowl/campaign.db \
    --listen-address 0.0.0.0:8080 \
    --base "/fowl-engine/Vector Strike"
```

**As a systemd service** (Linux):
```bash
# /etc/systemd/system/bfdb.service
[Unit]
Description=Fowl Engine Stats Server
After=netidx-resolver.service

[Service]
ExecStart=/opt/fowl/bfdb --db /opt/fowl/campaign.db --listen-address 0.0.0.0:8080 --base "/fowl-engine/Vector Strike"
Restart=always

[Install]
WantedBy=multi-user.target
```
```bash
systemctl enable --now bfdb
```

---

## Step 5 — Deploy bflib.dll to DCS

Copy `target\release\bflib.dll` to the DCS mission folder on the DCS server. The mission's Lua script loads it with `require("bflib")`.

Restart DCS / reload the mission. Once initialized, bflib connects to the resolver and starts publishing. Stats will appear in real time at `http://<stats-server>:8080`.

---

## Netidx Path Reference

| Setting | Value |
|---------|-------|
| `netidx_base` in `_CFG` | `/fowl-engine` |
| bflib publishes to | `/fowl-engine/<sortie_name>` |
| bfdb `--base` | `/fowl-engine/<sortie_name>` |
| `client.json` resolver address | `<resolver-machine-ip>:4564` |

---

## Troubleshooting

**bfdb says "no default config file was found"**
The `client.json` is missing or in the wrong location.
- Windows: `%APPDATA%\netidx\client.json`
- Linux: `~/.config/netidx/client.json`

**bfdb starts but web UI shows no live data**
- Check that `netidx_base` is set (not `null`) in your `_CFG` file
- Check the sortie name in `--base` exactly matches the sortie name in `_CFG`
- Check port 4564 is open between DCS server and resolver machine

**DCS connects but stats look stale**
The `reader_ttl` (60s) and `writer_ttl` (120s) in the resolver config control how long values are cached. If bflib crashes or disconnects, values go stale after those timeouts.

**Running without TLS (Anonymous auth) on the public internet**
Anonymous auth means anyone who can reach port 4564 can publish or subscribe to any path. This is fine for a LAN or a VPN. For a public server, consider putting the resolver behind a firewall or VPN, or setting up TLS auth (see the production `resolver.json` in `bftools/scripts/netidx/` for an example).
