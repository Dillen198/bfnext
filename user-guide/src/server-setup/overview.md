# Server Setup Overview

Fowl Engine consists of three components that work together:

| Component | What it does |
|-----------|-------------|
| `bflib.dll` | Campaign logic — loaded into DCS, runs the war |
| `bfdb.exe` | Stats server + web UI — runs outside DCS |
| Netidx resolver | Message broker — connects bflib to bfdb in real time |

```
DCS Process                    Netidx Resolver         Stats Machine
┌──────────────┐               ┌─────────────┐         ┌────────────────┐
│  bflib.dll   │──publishes──► │   resolver  │ ◄───────│  bfdb.exe      │
│  (campaign)  │               │  (broker)   │         │  + web UI      │
└──────────────┘               └─────────────┘         └────────────────┘
                                                              │
                                                         browser ◄─── players
```

**Netidx is what makes the live web UI possible.** Without it, `bfdb` still works but only shows historical data from its database — it won't update in real time while DCS is running.

## Two Setup Scenarios

- **[Local Testing](./local-testing.md)** — Everything on one machine. Good for development and testing campaigns before going live.
- **[Server Setup](./server-setup.md)** — DCS runs on a dedicated server, `bfdb` and the resolver run elsewhere (same server or a separate stats machine).
