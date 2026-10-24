# Carriers

- investigate issues with spawning aircraft on carriers

# Interface

- Client side plugin for richer ui (in progress: Vector Strike cockpit 3.1 is native dxgui + bfcockpit_net.dll with MENU/CARGO/STATUS/EWR/LOG/WHEEL tabs, a radial wheel (head, mouse or keys) and an Options -> Special page; installer on GitHub releases as cockpit-v<version>; F10 answers reach it through the msgq feed (/api/cockpit/messages); needs the new bflib/bfdb deployed and a final in-game pass)

# Lua API

- ai orders/missions beyond `move_group`
- actions (tanker, AWACS, CAP, ...) callable from the API
- replicate the rest of the netidx rpc interface (queries, spawn_deployable/spawn_troop, move_group, add_points already exist in `bflib/src/api.rs`)

See ROADMAP.md for feature status, and its "Audit Sept 2026" section for new ideas.
