# data/

## unitdb-baseline.json

A snapshot of DCS's unit range database — engagement (`ThreatRange` /
`ThreatRangeMin`) and detection (`DetectionRange`) ranges per unit type, keyed by
the DCS type name that `Unit.getTypeName()` returns.

**This is a fallback, not the source of truth.** The engine harvests the *running*
DCS install at startup (`bflib/src/unitdb.rs`, reading `_G.db.Units` from the hooks
Lua state), so anything running against a live server should use that — it matches
the installed build and includes mods, which a committed file never can.

This file exists for the consumers that have no DCS to ask: `bftools`, the wiki
range tables, and anything building without a server.

### Refreshing it

From a live server, whose snapshot is authoritative for that install:

```bash
curl -s http://<bfdb-host>/api/unitdb > data/unitdb-baseline.json
```

Or copy `<DCS saved games>/Logs/unitdb.json`, which the engine writes on every
harvest.

### Provenance of the current file

Parsed from [Quaggles/dcs-lua-datamine](https://github.com/Quaggles/dcs-lua-datamine)
at DCS **2.9.29.27278** (its 2026-08-27 export), which includes the Currenthill and
China asset packs but not every mod we run. 881 unit types.

`max_target_detection_range_m` is `null` throughout: it lives nested under each
unit's `WS` table and isn't recoverable from that text dump. The live harvest
fills it in.
