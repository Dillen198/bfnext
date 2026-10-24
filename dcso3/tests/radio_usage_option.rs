//! Regression tests for the AI "radio usage" options (tags 21/22/23).
//!
//! A mission with Radio Usage Contact actually set refused to load:
//!
//!     THE MISSION CANNOT START: initalizing the mission
//!     runtime error: unknown option,
//!       air: RuntimeError("expected a table, got String(\"Air Defence;\")")
//!       ground: RuntimeError("unknown GroundOption 21")
//!       naval: RuntimeError("unkown NavalOption 21")
//!
//! DCS stores the selected attributes in the mission three ways at once -- a
//! `targetTypes` array, a `noTargetTypes` array, and `value` as the names
//! joined with ';'. The decoder is handed `value`, so a string is the ordinary
//! case; only the empty selection arrives as "none", and that was the one case
//! handled. These parse through the real public entry point, `Task::from_lua`,
//! rather than the private tag decoder, so they cover the path the mission
//! loader actually takes.

use dcso3::{
    attribute::Attribute,
    controller::{AiOption, AirOption, Task},
};
use mlua::{prelude::*, Lua, Value};

/// Build the table DCS writes for a WrappedAction/Option waypoint task.
fn option_task<'lua>(lua: &'lua Lua, tag: u8, value: Value<'lua>) -> Value<'lua> {
    let params = lua.create_table().unwrap();
    params.set("name", tag).unwrap();
    params.set("value", value).unwrap();
    let action = lua.create_table().unwrap();
    action.set("id", "Option").unwrap();
    action.set("params", params).unwrap();
    let outer_params = lua.create_table().unwrap();
    outer_params.set("action", action).unwrap();
    let task = lua.create_table().unwrap();
    task.set("id", "WrappedAction").unwrap();
    task.set("params", outer_params).unwrap();
    Value::Table(task)
}

fn parse_contact<'lua>(lua: &'lua Lua, value: &str) -> Vec<(Attribute, bool)> {
    let v = Value::String(lua.create_string(value).unwrap());
    let task: Task = FromLua::from_lua(option_task(lua, 21, v), lua)
        .unwrap_or_else(|e| panic!("parsing radio-usage-contact {value:?}: {e}"));
    match task {
        Task::WrappedOption(AiOption::Air(AirOption::OptionRadioUsageContact(attrs))) => {
            let mut out = vec![];
            for pair in (*attrs).clone().pairs::<Attribute, bool>() {
                let (k, v) = pair.unwrap();
                out.push((k, v));
            }
            out
        }
        other => panic!("expected OptionRadioUsageContact, got {other:?}"),
    }
}

#[test]
fn semicolon_joined_attributes_parse() {
    let lua = Lua::new();
    // The exact value that broke the campaign, off a SEAD template.
    let got = parse_contact(&lua, "Air Defence;");
    assert_eq!(got, vec![(Attribute::AirDefence, true)]);
}

#[test]
fn several_attributes_parse() {
    let lua = Lua::new();
    let mut got = parse_contact(&lua, "Air Defence;Tanks;");
    got.sort_by_key(|(a, _)| format!("{a:?}"));
    assert_eq!(
        got,
        vec![(Attribute::AirDefence, true), (Attribute::Tanks, true)]
    );
}

#[test]
fn none_is_an_empty_set() {
    let lua = Lua::new();
    // The one case that already worked -- make sure it still does.
    assert!(parse_contact(&lua, "none").is_empty());
}

#[test]
fn unknown_attribute_names_are_not_fatal() {
    let lua = Lua::new();
    // `Attribute` has a `Custom` catch-all, so a name DCS adds later must
    // round-trip rather than refuse to load the mission.
    let got = parse_contact(&lua, "Space Lasers;");
    assert_eq!(got.len(), 1);
    assert!(matches!(got[0].0, Attribute::Custom(_)));
}

#[test]
fn a_table_value_still_parses() {
    // Older/other writers hand over a table rather than a string.
    let lua = Lua::new();
    let t = lua.create_table().unwrap();
    t.set("Air Defence", true).unwrap();
    let task: Task = FromLua::from_lua(option_task(&lua, 21, Value::Table(t)), &lua)
        .expect("table-valued radio usage contact should still parse");
    assert!(matches!(
        task,
        Task::WrappedOption(AiOption::Air(AirOption::OptionRadioUsageContact(_)))
    ));
}
