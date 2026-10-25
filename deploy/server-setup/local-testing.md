# Local Testing

This setup runs everything on **one machine** — DCS, the resolver, and bfdb all on your PC. Good for developing and testing campaigns before deploying to a real server.

```
Your PC
┌──────────────────────────────────────────────────────┐
│                                                      │
│  DCS (bflib.dll)  ──►  resolver :4564  ◄──  bfdb    │
│                                              │       │
│                                         :8080 (web)  │
└──────────────────────────────────────────────────────┘
```

## Prerequisites

- Rust toolchain + the project built (`cargo build --release --package=bfdb`)
- `netidx-tools` installed: `cargo install netidx-tools`
- `bfweb` built: `cd bfweb && npm run build`
- DCS World installed with the Fowl Engine mission loaded

---

## Step 1 — Create the Netidx Config Files

These files only need to be created once.

**Resolver config** — save as `netidx-resolver.json` in the repo root:
```json
{
    "parent": null,
    "children": [],
    "member_servers": [
        {
            "pid_file": "",
            "addr": "127.0.0.1:4564",
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

**Client config** — save as `%APPDATA%\netidx\client.json`
(i.e. `C:\Users\<you>\AppData\Roaming\netidx\client.json`):
```json
{
    "addrs": [["127.0.0.1:4564", "Anonymous"]],
    "base": "/",
    "default_auth": "Anonymous"
}
```

---

## Step 2 — Set `netidx_base` in Your Campaign Config

Find your campaign's `<sortie>_CFG` file (in the mission's state folder) and make sure `netidx_base` is set:

```json
{
    "netidx_base": "/fowl-engine",
    ...
}
```

The full publish path bflib will use is `{netidx_base}/{sortie_name}`, e.g. `/fowl-engine/Vector Strike`.

---

## Step 3 — Start the Resolver

Open a PowerShell window and keep it running:

```powershell
netidx resolver-server -f -c E:\Github\bfnext-vector\netidx-resolver.json
```

You should see no errors. The resolver listens on `127.0.0.1:4564`.

---

## Step 4 — Start bfdb

In a second PowerShell window, run bfdb with `--base` matching your sortie path:

```powershell
.\target\release\bfdb.exe `
    --db ./campaign.db `
    --listen-address 0.0.0.0:8080 `
    --base "/fowl-engine/Vector Strike"
```

Replace `Vector Strike` with your actual sortie name. You should see:
```
API server listening on http://0.0.0.0:8080
```

---

## Step 5 — Load DCS

Start DCS and load the Fowl Engine mission. Once the mission initializes, bflib will connect to the resolver and start publishing. You'll see live data appear at **http://localhost:8080**.

---

## Offline Mode (No DCS Running)

If you just want to browse historical stats without DCS:

```powershell
.\target\release\bfdb.exe --db ./campaign.db --listen-address 0.0.0.0:8080
```

No `--base` needed — bfdb reads the existing database and serves the web UI in read-only mode.

---

## Rebuild Workflow

When you make code changes:

```powershell
# If bfweb changed:
cd bfweb
npm run build

# Rebuild bfdb (embeds bfweb dist):
cd ..
. .\setup-build.ps1
cargo build --release --package=bfdb

# Rebuild bflib (the DCS DLL):
cargo build --release --package=bflib
# Then copy target\release\bflib.dll to your DCS mission folder
```
