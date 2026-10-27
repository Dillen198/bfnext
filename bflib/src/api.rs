//! The `vector_strike` table: a Lua API for scripts running in the *mission*
//! scripting environment (DO SCRIPT triggers, mission-side Lua), registered
//! when the campaign loads. It is not visible from the server hooks
//! environment, where DCSServerBot's plugins run; the bot reads the same
//! data from bfdb instead.
//!
//! Every function returns `nil` (queries, spawns) or `false` (move_group,
//! add_points) on failure and logs why. The mutating calls are refused until
//! a campaign is loaded. None of them may be called from inside an engine
//! tick -- that includes a `vector_strike.on_event` hook, which is therefore
//! only ever called between ticks (see `dispatch_event`).
//!
//! Mission scripts can define `vector_strike.on_event(ev)` to receive
//! `{ type = "achievement" | "alert", message = "..." }`.

use crate::{admin, with_context, Context};
use anyhow::{bail, Result};
use bfprotocols::db::group::GroupId;
use dcso3::{coalition::Side, wrap_f, LuaEnv, MizLua, Vector2};
use std::cell::RefCell;

fn loaded(ctx: &mut Context) -> Result<&mut Context> {
    if !ctx.db_loaded {
        bail!("the campaign is not loaded yet")
    }
    Ok(ctx)
}

/// Serialize a query result for Lua.
fn json<T: serde::Serialize>(v: &T) -> Result<Option<String>> {
    Ok(Some(serde_json::to_string(v)?))
}

pub fn register(lua: MizLua) -> Result<()> {
    let globals = lua.inner().globals();
    let api = lua.inner().create_table()?;

    api.set(
        "get_objectives",
        lua.inner().create_function(|lua, ()| {
            wrap_f("vector_strike.get_objectives", MizLua(lua), |lua| {
                with_context(lua, "get_objectives", |ctx| json(&admin::query_objectives(ctx)))
            })
        })?,
    )?;

    api.set(
        "get_objective",
        lua.inner().create_function(|lua, name: String| {
            wrap_f("vector_strike.get_objective", MizLua(lua), |lua| {
                with_context(lua, "get_objective", |ctx| {
                    json(&admin::query_objective_details(ctx, &name)?)
                })
            })
        })?,
    )?;

    api.set(
        "get_players",
        lua.inner().create_function(|lua, ()| {
            wrap_f("vector_strike.get_players", MizLua(lua), |lua| {
                with_context(lua, "get_players", |ctx| json(&admin::query_players(ctx)))
            })
        })?,
    )?;

    api.set(
        "get_player",
        lua.inner().create_function(|lua, name: String| {
            wrap_f("vector_strike.get_player", MizLua(lua), |lua| {
                with_context(lua, "get_player", |ctx| {
                    json(&admin::query_player_details(ctx, &name)?)
                })
            })
        })?,
    )?;

    api.set(
        "get_groups",
        lua.inner().create_function(|lua, side: Option<String>| {
            wrap_f("vector_strike.get_groups", MizLua(lua), |lua| {
                with_context(lua, "get_groups", |ctx| {
                    let side = side.and_then(|s| s.parse::<Side>().ok());
                    json(&admin::query_groups(ctx, side))
                })
            })
        })?,
    )?;

    api.set(
        "get_campaign_state",
        lua.inner().create_function(|lua, ()| {
            wrap_f("vector_strike.get_campaign_state", MizLua(lua), |lua| {
                with_context(lua, "get_campaign_state", |ctx| {
                    json(&admin::query_campaign_state(ctx))
                })
            })
        })?,
    )?;

    // Action APIs
    api.set(
        "spawn_deployable",
        lua.inner().create_function(
            |lua, (side, name, x, y, heading): (String, String, f64, f64, f64)| {
                wrap_f("vector_strike.spawn_deployable", MizLua(lua), |lua| {
                    with_context(lua, "spawn_deployable", |ctx| {
                        let side = side.parse::<Side>()?;
                        let pos = Vector2::new(x, y);
                        let gid = admin::api_spawn_deployable(loaded(ctx)?, lua, side, &name, pos, heading)?;
                        Ok(Some(gid.to_string()))
                    })
                })
            },
        )?,
    )?;

    api.set(
        "spawn_troop",
        lua.inner().create_function(
            |lua, (side, name, x, y, heading): (String, String, f64, f64, f64)| {
                wrap_f("vector_strike.spawn_troop", MizLua(lua), |lua| {
                    with_context(lua, "spawn_troop", |ctx| {
                        let side = side.parse::<Side>()?;
                        let pos = Vector2::new(x, y);
                        let gid = admin::api_spawn_troop(loaded(ctx)?, lua, side, &name, pos, heading)?;
                        Ok(Some(gid.to_string()))
                    })
                })
            },
        )?,
    )?;

    api.set(
        "move_group",
        lua.inner().create_function(|lua, (id, x, y): (i64, f64, f64)| {
            wrap_f("vector_strike.move_group", MizLua(lua), |lua| {
                with_context(lua, "move_group", |ctx| {
                    let gid = GroupId::from(id);
                    admin::api_move_group(loaded(ctx)?, lua, &gid, Vector2::new(x, y))?;
                    Ok(true)
                })
            })
        })?,
    )?;

    api.set(
        "add_points",
        lua.inner().create_function(|lua, (player, amount, reason): (String, i32, String)| {
            wrap_f("vector_strike.add_points", MizLua(lua), |lua| {
                with_context(lua, "add_points", |ctx| {
                    admin::api_add_points(loaded(ctx)?, &player, amount, &reason)?;
                    Ok(true)
                })
            })
        })?,
    )?;

    globals.set("vector_strike", api)?;
    Ok(())
}

thread_local! {
    /// Events for `vector_strike.on_event`, queued by `dispatch_event`.
    static PENDING: RefCell<Vec<(&'static str, String)>> = const { RefCell::new(Vec::new()) };
}

/// The most hook events kept waiting; older ones are dropped beyond this.
const MAX_PENDING: usize = 256;

/// Queue an event for the mission's `vector_strike.on_event` hook.
///
/// The hook used to be called right here, from inside the tick, while the
/// tick held the campaign state -- and a hook calling back into
/// `vector_strike.*` took a second mutable reference to it. Now it is queued
/// and `flush_events` calls the hook once the tick has let go of the state.
pub(crate) fn dispatch_event(event_type: &'static str, message: &str) {
    PENDING.with(|q| {
        let mut q = q.borrow_mut();
        if q.len() >= MAX_PENDING {
            q.remove(0);
        }
        q.push((event_type, message.to_owned()));
    })
}

/// Call `vector_strike.on_event` for every queued event. Only call this
/// when nothing holds the campaign state (between ticks).
pub(crate) fn flush_events(lua: MizLua) {
    let pending = PENDING.with(|q| std::mem::take(&mut *q.borrow_mut()));
    if pending.is_empty() {
        return;
    }
    let globals = lua.inner().globals();
    let Ok(api) = globals.get::<mlua::Table>("vector_strike") else { return };
    let Ok(func) = api.get::<mlua::Function>("on_event") else { return };
    for (event_type, message) in pending {
        if let Ok(tbl) = lua.inner().create_table() {
            let _ = tbl.set("type", event_type);
            let _ = tbl.set("message", message);
            if let Err(e) = func.call::<()>(tbl) {
                log_hook_error(event_type, &e);
            }
        }
    }
}

/// Report a failing `vector_strike.on_event` hook -- the error used to be
/// discarded, so a broken mission-side script failed silently forever. At
/// most one line a minute, with a count of what was suppressed in between, so
/// a hook that throws on every event can't flood the log.
fn log_hook_error(event_type: &str, e: &mlua::Error) {
    use std::sync::atomic::{AtomicI64, AtomicU64, Ordering};
    static LAST: AtomicI64 = AtomicI64::new(0);
    static SUPPRESSED: AtomicU64 = AtomicU64::new(0);
    let now = chrono::Utc::now().timestamp();
    if now.saturating_sub(LAST.load(Ordering::Relaxed)) >= 60 {
        LAST.store(now, Ordering::Relaxed);
        let n = SUPPRESSED.swap(0, Ordering::Relaxed);
        log::error!(
            "vector_strike.on_event failed for a {event_type:?} event ({n} more failures \
             suppressed since the last report): {e}"
        );
    } else {
        SUPPRESSED.fetch_add(1, Ordering::Relaxed);
    }
}
