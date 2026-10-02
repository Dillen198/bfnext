# Carriers

- investigate issues with spawning aircraft on carriers

# Interface

- Client side plugin for richer ui (in progress: bfcockpit, CockpitPage/KneeboardTab; F10 menu not yet replaced)

# Lua API

- ai orders/missions beyond `move_group`
- actions (tanker, AWACS, CAP, ...) callable from the API
- replicate the rest of the netidx rpc interface (queries, spawn_deployable/spawn_troop, move_group, add_points already exist in `bflib/src/api.rs`)

See ROADMAP.md for feature status, and its "Audit Sept 2026" section for new ideas.
