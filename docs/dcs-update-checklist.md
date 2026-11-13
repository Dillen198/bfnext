# DCS update checklist

What to check when DCS World updates, before building and deploying an engine
for the new version. Most DCS updates change none of this; the ones that do
fail quietly (a shifted event id decodes as the wrong event, a new enum value
fails a whole read), so the checks are worth the twenty minutes.

## 1. Lua runtime (`lua.dll`, `lua.lib`)

Every DLL DCS loads (bflib, bfrange, bfcockpit-net) links dynamically against
DCS's own Lua 5.1. `lua.lib` is generated from the exact `lua.dll` it
describes, and `lua.sha256` records that pair.

1. Point the check at the updated install and run it:
   ```powershell
   $env:DCS_PATH = "D:\DCS World"   # the install folder
   . .\setup-build.ps1
   ```
   It warns if `<DCS_PATH>\bin-mt\lua.dll` differs from the repo's. No
   warning means ED did not touch Lua: skip to section 2.
2. If it differs, copy the new `bin-mt\lua.dll` to the repo root and, from a
   Visual Studio developer prompt (needs `dumpbin` and `lib`), run
   `dll2lib.bat lua.dll`. It rebuilds `lua.lib` and rewrites `lua.sha256`.
3. Confirm it is still Lua 5.1: the dump in section 3 prints `_VERSION`. A
   different Lua version is not a rebuild, it is a port (mlua's `lua51`
   feature, see [plans/mlua-0.10.md](plans/mlua-0.10.md)).
4. Commit `lua.dll`, `lua.lib` and `lua.sha256` together. `setup-build`
   warns on every build when the three disagree.
5. Rebuild the DLLs in release mode and run `TESTING.md` in game.

## 2. Event ids

`dcso3/src/event.rs` (`translate`) maps `world.event` ids to `Event`
variants by number. DCS appends ids, and has at least once inserted one in
the middle, shifting everything after it (2.9.29: runway touch and takeoff
had been decoding as other events).

- bflib logs the live table at every mission start:
  `[EVENT_IDS] DCS world.event: 0=S_EVENT_INVALID, 1=S_EVENT_SHOT, ...`.
  Diff that line against `translate`.
- An id `translate` does not know is ignored (`Event::Invalid`, logged at
  debug), so a new id is harmless until something needs it. A shifted id is
  not harmless: fix the table, and update the tests in `event.rs`.

## 3. Numeric enums

dcso3 turns DCS's numeric ids into Rust enums with `simple_enum!` (unknown id
= decode error) and `simple_enum_unknown!` (unknown id = `Unknown(n)`). Dump
the live tables and compare. From the mission scripting environment (a
trigger's DO SCRIPT, or `net.dostring_in("mission", ...)` from the hooks
state):

```lua
local function dump(name, t)
    local kv = {}
    for k, v in pairs(t or {}) do kv[#kv + 1] = tostring(k) .. "=" .. tostring(v) end
    table.sort(kv)
    env.info("ENUMDUMP " .. name .. ": " .. table.concat(kv, ", "))
end
env.info("ENUMDUMP _VERSION: " .. tostring(_VERSION))
dump("world.event", world.event)
dump("country.id", country.id)
dump("coalition.side", coalition.side)
dump("coalition.service", coalition.service)
dump("Object.Category", Object.Category)
dump("Unit.Category", Unit.Category)
dump("Group.Category", Group.Category)
dump("Airbase.Category", Airbase.Category)
dump("Weapon.Category", Weapon.Category)
dump("Weapon.GuidanceType", Weapon.GuidanceType)
dump("Weapon.MissileCategory", Weapon.MissileCategory)
dump("land.SurfaceType", land.SurfaceType)
```

The lines land in `dcs.log`. Compare with:

| Lua table | Rust type | File |
| --- | --- | --- |
| `country.id` | `Country` (unknown-tolerant) | `dcso3-core/src/country.rs` |
| `coalition.side` | `Side` (strict, on purpose) | `dcso3-core/src/coalition.rs` |
| `coalition.service` | `Service` | `dcso3/src/coalition.rs` |
| `Object.Category` | `ObjectCategory` | `dcso3/src/object.rs` |
| `Group.Category` | `GroupCategory` (strict) | `dcso3/src/group.rs` |
| `Weapon.Category` | `WeaponCategory` (strict) | `dcso3/src/weapon.rs` |
| `Weapon.GuidanceType` | `GuidanceType` (strict, **numbering unverified**) | `dcso3/src/weapon.rs` |
| `Weapon.MissileCategory` | `MissileCategory` | `dcso3/src/weapon.rs` |
| `land.SurfaceType` | `SurfaceType` (strict) | `dcso3/src/land.rs` |
| `net.PS_*` (hooks state) | `PlayerStat` | `dcso3/src/net.rs` |

`GuidanceType` first: its table puts a millimetre-wave radar at 3, which the
usual public references do not have. Settle it against this dump (and a few
known weapons' `getDesc().guidance`), then fix the enum and
`WeaponDesc::from_table` together.

Rules when an enum changes:

- Adding a value: add the case after the existing ones. Stored rows are
  positional bincode by variant *index*, so the order of existing cases must
  never change. For a persisted enum (bfdb stores `LiquidType` and `Side`;
  `bfdb/tests/golden_rows.rs` lists what it pins) there is a catch:
  `Unknown` must stay the last variant, so a new case takes the index
  `Unknown` had, and any stored `Unknown(n)` row would then read as the new
  case. Check bfdb for such rows (there should be none: an unknown liquid
  type is not something the engine records) before adding the case, and
  expect `golden_rows` to fail on the `Unknown` line; that one line may be
  updated, with the check noted in the commit.
- A value renumbered by DCS: change the `=> n` only; the variant order (and
  so the stored index) stays.
- `golden_rows` failing means a stored encoding moved. Do not update the
  expected bytes; see that file's header.

## 4. Hard-coded DCS names

Names copied from DCS that an update can rename or remove. They fail softly
(a spawn or preset that silently does nothing), so look for them in the logs
after the update and in game.

- `bfprotocols/src/cfg/example.rs`: the example campaign config (about 2,600
  lines) names unit, weapon, airframe and template types. Operators' configs
  carry the same names; the engine warns about types it cannot find at
  startup.
- `livewx/src/lib.rs`, `select_cloud_preset`: the DCS 2.9 dynamic-weather
  cloud presets `Preset2`, `Preset6`, `Preset14`, `Preset22`,
  `RainyPreset1`-`3`, and their base altitudes, taken from the preset
  templates the 2.9 mission editor ships. A renamed preset shows as clear sky
  under live weather.
- The unit database: `bftools unit-db` against a fresh harvest
  (`Logs/unitdb.json`) shows ranges DCS changed; regenerate the wiki range
  tables from it.

## 5. Build and test

```powershell
. .\setup-build.ps1
$env:PATH = "$(Get-Location);$env:PATH"
cargo test --workspace --locked
cargo build --release --locked -p bflib -p bfrange -p bfdb
```

Then the in-game checklist in `TESTING.md` on a test server before the update
reaches the live one.
