## Unreleased (breaking: next release is 0.2.0)

- BREAKING: `Time` is now `f64` (was `f32`, which lost sub-second
  resolution after a few days of uptime); `Add`/`AddAssign`/`Sub` follow
- BREAKING: `String::from_lua` errors on nil/table/function instead of
  stringifying them ("nil", "table: 0x..."). Values that may be absent
  must be read as `Option<String>`; `PlayerInfo::name` and
  `Net::dostring_in` now return `Option<String>`, and `MarkPanel` text
  reads nil as ""
- BREAKING: `Country`, `ObjectCategory`, `UnitCategory` and `LiquidType`
  gain an `Unknown(n)` catch-all (appended last, so bincode compatible)
  instead of failing the decode on ids DCS adds in updates. They no
  longer have repr discriminants, use the new `id()` instead of `as`.
  The generating macro is exported as `simple_enum_unknown!`
- BREAKING: event initiators are optional so a dead object no longer
  drops the whole event: `Shot`, `Birth`, `AtPlace`, `WeaponAdd`,
  `EjectionEvent` (and its `target`). Bare tables without an object
  metatable decode as `None` for `WeaponUse`/`UnitEvent` initiator and
  target and for `MarkPanel` initiator
- BREAKING: `weapon_name` is `Option<String>` on `Shot`, `ShootingEnd`,
  `WeaponUse` and `WeaponAdd`. DCS leaves it nil for e.g. cluster
  submunitions, which used to be stored as the literal text "nil"
- BREAKING: fix the `world.event` id table from 49 up to match DCS
  2.9.29 (it was shifted by one from 51). Remove `UnitTaskTimeout`,
  `MacSubtaskScore`, `PostponedTakeoff` and `PostponedLand`; add
  `UnitTaskComplete`, `RunwayTakeoff`, `RunwayTouch`, `MacLmsRestart`,
  `SimulationFreeze`, `SimulationUnfreeze`, `HumanAircraftRepairStart`,
  `HumanAircraftRepairFinish` and `GroupChangeOption`
- BREAKING: `Event::Refueling`/`RefuelingStop`, `LandingQualityMark` and
  `WeaponDrop` now carry payloads (`Refueling`, `LandingQualityMark` with
  the LSO grade comment, `WeaponDrop`). `WeaponUse` gains `weapon`, the
  live weapon object for Hit/Kill
- BREAKING: `Unit::get_life`/`get_life0` return `f64` (was `i32`, which
  truncated damaged units). The dead-unit workaround now tests `< 1`
- BREAKING: `Dcs::get_unit_property` takes `(mission_id, property)` as
  DCS expects. `Dcs::get_version` is new; it falls back to
  `__DCS_VERSION__` on builds where `DCS.getVersion` is nil
- BREAKING: rename `Action::out_line_to_all` to `freeform_to_all`. It now
  calls `markupToAll` with shape id 7 and varargs points (the old
  `outLineToAll` doesn't exist in DCS)
- BREAKING: `Command::ActivateBeacon` gains TACAN `channel`,
  `mode_channel` (`TacanBand`), `aa` and `bearing`, and `Task::FireAtPoint`
  gains `counter_battery_radius` (it also sends `zoneRadius`)
- BREAKING: new variants on the (exhaustive) `Task`, `Command`,
  `AirOption`, `GroundOption`, `TurnMethod`, `AirReactionToThreat` and
  `GroundRoe` enums, listed below
- `onPlayerTryConnect`/`onPlayerTryChangeSlot` hooks fail closed: a
  handler error, panic or undecodable argument now denies (was: nil =
  allow). Add `wrap_f_deny`
- `Task` gains `Raw`: any task, command or option the binding can't
  decode is kept as the raw table and written back verbatim instead of
  failing the whole waypoint/route. Decode `Land`
- new tasks `AttachTrailer`/`DetachTrailer`. `EngageTargets` sends
  `maxDistEnabled` with `maxDist` (the AI otherwise engaged at any
  range). A `Mission` task's route is now read from `params.route.points`
  (it always came back empty)
- new commands `ActivateGci`/`DeactivateGci`,
  `ActivateJammer`/`DeactivateJammer` (with `GnssJamming`/`RadioJamming`),
  `ActivateRsbn`/`DeactivateRsbn` and `NoAction`
- command decode fixes: `DeactivateICLS` decoded as `DeactivateACLS`;
  `ActivateICLS` and `SMOKE_ON_OFF` now decode. `StopTransmission` is
  written in the ME spelling, and both spellings are read
- new AI options. Air: `PreferVertical`, `LandingOptions`
  (`AirLandingOption`), `AllowLineUpRunway`, `DisengageAndRtb`, and
  `AirReactionToThreat::HorAaaEvadeFire`. Ground: `AltRestrictionMin`/
  `AltRestrictionMax`, `RestrictTarget` (`GroundRestrictTarget`),
  `ColumnInterval`, `EvasionOfArm`, and `GroundRoe::WeaponFree`
- the radio usage contact/engage/kill options decode the ';' joined
  attribute string DCS writes (e.g. "Air Defence;") instead of refusing
  to load the mission
- `TurnMethod` gains `FromParkingArea`, `FromGroundArea`,
  `FromGroundAreaHot`, `FromRunway` and `Landing`. `MissionPoint` reads
  and writes `airdromeId` (was misspelled `airdromId`)
- airbase: `Airbase::get_category` (`AirbaseCategory`),
  `get_parking_spots` returning typed `ParkingSpot`s (`term_type`
  consts, `usable_by`, `occupied`, `preference`), and `Runway::name`.
  Fix `Airbase::get_id`, which called the nonexistent `getId`
- miz `Unit`: `set_typ`, `set_mass`, `set_alt_type`, `set_parking` and
  `clear_parking`. `GroupKind` derives `PartialEq`/`Eq`/`Hash`
- new `atmosphere` module: `get_wind`, `get_wind_with_turbulence`,
  `get_temperature_and_pressure`
- weapon: `get_point`, `get_position`, `get_velocity`, `get_coalition`,
  `destroy`, and `get_weapon_desc` (typed `WeaponDesc` with
  `WeaponCategory`/`GuidanceType`/`MissileCategory`)
- unit: `get_draw_argument_value`, `get_coalition`, `get_country`,
  `has_attribute`, `get_cargos_on_board`, and `Ammo::missile_category`.
  `Object::get_coalition`. `StaticObject::get_cargo_weight` and
  `get_cargo_display_name`. `Land::get_ip_opt`. `WSType::levels`.
  `Color::violet` and `Color::azure`
- F10 menu mirror: group menus added through `MissionCommands` are
  recorded and can be read back with `mirrored_menu` and fired with
  `invoke_mirrored_menu`
- new `env::miz_pack`: `read_table_from_miz`, `rewrite_entry_in_miz` and
  `serialize_to_lua` for reading and rewriting .miz entries.
  `scratch_lua` gives a no-stdlib state, and entries load text-only (no
  bytecode). The Lua writer is byte exact: it escapes quotes, control
  characters and newlines, writes non-UTF-8 bytes as `\ddd`, writes
  NaN/inf as `(0/0)`/`(1/0)`/`(-1/0)`, and keeps zero/negative integer
  keys. Rewrites go via an fsynced temp file that is removed on any
  error
- fix `Timer::set_function_time`, which called `removeFunction`
- fix `Coalition::get_country_coalition`, which called `getCountrySide`
- warehouse counts (`get_item_count`, `get_liquid_amount`, and the
  inventory `item`/`for_each`) are decoded as f64 and clamped to `u32`.
  Unlimited/negative values read as `u32::MAX` instead of failing the read
- `Ucid::from_str` rejects non-hex/multibyte input instead of panicking.
  `DeepClone` has a cycle guard and depth limit. `atomic_id!` saturates
  instead of wrapping
- `record_perf!` no longer holds the global perf `&mut` across the Lua
  call (re-entrancy UB)

## 0.1.10

- derive `schemars::JsonSchema` across the `dcso3`-defined types used in
  campaign config (`String`, `simple_enum!`/`string_enum!`-generated enums
  like `Side`/`Country`/`AltType`) so a JSON Schema can be generated
  directly from the real config types
- gracefully ignore unrecognized DCS event ids instead of erroring — DCS
  periodically adds new `world.event` ids (observed: simulation
  freeze/unfreeze, human aircraft repair start/finish, and others past the
  previous table's max of 57) that aren't relevant to campaign logic

## 0.1.9

- update the PlayerLeaveUnit event to ED's new format
- fix warnings for rust 1.89

## 0.1.8

- add new controller options used by the mission editor

## 0.1.7

- add stats for Land.isVisible and Land.getHeight

## 0.1.6

- add optional DCS api performance stats tracking. Enable the feature
  "perf" and the perf::Perf struct will contain hdrhistograms of the
  time various important DCS api calls take

## 0.1.5

- work around a DCS bug that causes isExist to return true on dead
  groups
- make ucid copy
- flesh out mark events
- set psi when setting heading in mission files

## 0.1.4

- implement DcsObject for StaticObject
- fix ComboTask, thank you Hoggit

## 0.1.3

- weapon dcsoid implementation
- some common object methods on weapon
- fix a bunch of sequence lifetimes to 'lua
- map route points to MissionPoint
- route

## 0.1.2

- alter the return type of on_player_try_connect to allow returning a
  ban message
- add Display implementation for PlayerId and Ucid

## 0.1.1

- very minor bug fixes and changes

## 0.1.0

- warehouse system
- StaticObject::get_by_name can return an airbase
- ai tasking system converted to enum
- ai command system converted to enum

## 0.0.10

- fix cargo category and keywords
- implement laser spot
- implement object ids for object, unit, and group
- implement a few additional vec3 and vec2 functions

## 0.0.9

- fix get_velocity was calling getPosition
- catch panics in all callbacks
- fix order of args in on_player_try_connect

## 0.0.8

- fix lifetimes on some returned objects were not as long as they
  should have been

## 0.0.7

- wherever possible transition error handing to anyhow
- some callback type changes
- a lot of bugs fixed
- bind the land module
- bind world.searchVolume and world.removeJunk

## 0.0.6

- add bindings for most of the triggers singleton, including almost
all of the f10 map shape options
- add bindings for the missionCommands singleton

## 0.0.5

- separate the two DCS lua environments in a statically type safe way
- add new events
- convert u32 returns to i64, which is the native lua type internally

## 0.0.4

- bug fixes

## 0.0.3

- add env.mission
- add timer
- harden returns to dcs because it crashes on lua errors
- fix a lot of bugs

## 0.0.2

- add missing UserHooks callbacks

## 0.0.1

initial release
