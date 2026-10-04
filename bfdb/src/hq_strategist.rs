//! The HQ strategist: a language model as each coalition's chief of staff.
//!
//! The engine's theatre HQ (`bflib::hq`) runs the war on its own rules every
//! couple of minutes. This sits above it, on a much slower clock: every
//! `--hq-strategist-minutes` it takes each side's HQ view -- built by the
//! engine for that side alone, so fog of war is the engine's to enforce, not
//! the model's to respect -- and asks the model what the strategy should be:
//! posture, main effort, what to hold, what to resupply first, where to stay
//! out of, how hard to lean on each line of effort, and the commander's
//! intent in a sentence or two that players will read.
//!
//! The answer goes back as a `Directive` (`hq-directive`). The engine checks
//! every objective id in it against the side it is for and drops anything
//! that doesn't fit, clamps the weights, and lets the directive expire if no
//! fresh one comes. So a model that is down, slow, wrong or rate limited
//! costs nothing but its own advice: the HQ's rules keep running the war.
//!
//! It never calls while a human commander's orders stand (they outrank it),
//! nor while nobody is on the server.

use crate::{db::StatsDb, news_llm, Inst};
use anyhow::{anyhow, bail, Result};
use bfprotocols::hq::{Directive, HqView, Line};
use netidx::publisher::Value;
use std::{fmt::Write, time::Duration};

const SYSTEM: &str = "\
You are the chief of staff to one coalition's theatre commander in a persistent \
multiplayer DCS World campaign. Two coalitions fight over a fixed set of \
objectives (airbases, FOBs, FARPs, logistics hubs, factories, SAM sites). An \
objective falls when its logistics are destroyed and enemy troops reach it. \
Supply flows from logistics hubs by convoy and helicopter; bases without supply \
cannot repair or rearm.

Below the commander, an automated HQ already runs the war every two minutes: it \
buys AI air packages (CAP, strike, SEAD, recon), artillery and missile fires, \
supply convoys and helo supply runs, helo troop insertions and reinforcement \
convoys out of the coalition treasury, steers the ground formations, and posts \
tasks for the human pilots. It does not need you to pick individual missions. \
It needs a strategy: where to concentrate, what must be held, what to resupply \
first, where not to go, and how much weight to put on each line of effort.

You are shown only what your coalition knows. Do not assume anything about the \
enemy beyond it.

Guidance:
- Concentrate. One main effort, usually the enemy objective closest to falling \
  that is worth taking (capturable ones -- no logistics left -- fall to troops). \
  Keep the current main effort unless there is a clear reason to change it; \
  switching every cycle wastes everything already sent.
- A base of ours being captured is an emergency: posture defensive or balanced, \
  that base first in `defend`, logistics and troops weighted up.
- Respect the record: an operation kind that keeps failing (e.g. strike packages \
  lost to air defence) should get less weight, or SEAD more.
- Mind the treasury: a near-empty treasury means fewer, cheaper operations -- \
  weight logistics, not air.
- Humans already flying a job means the HQ needs to do less of it; you do not \
  need to compensate for that, the HQ already does.
- `avoid` is for objectives sending anything at would be wasted (a dense SAM \
  belt, a fortress not worth the losses). Use it sparingly.

Reply with JSON only, no code fence, exactly these keys:
{\"posture\": \"offensive\" | \"balanced\" | \"defensive\",
 \"main_effort\": <enemy objective id or null>,
 \"defend\": [<own objective ids, most important first, at most 4>],
 \"supply_priority\": [<own objective ids, at most 5>],
 \"avoid\": [<objective ids, usually empty>],
 \"weights\": {\"air\": w, \"fires\": w, \"logistics\": w, \"troops\": w, \"ground\": w},
 \"intent\": \"<one or two plain sentences for the coalition's pilots: what we are doing and why. Names, never ids.>\",
 \"rationale\": \"<two or three sentences for the commander's staff>\"}
Weights are 0.0-2.5, 1.0 normal. Use only objective ids listed in the brief.";

fn yes(b: bool) -> &'static str {
    if b { "Y" } else { "-" }
}

/// The brief: the HQ view as compact text. Tables, not JSON -- a third of
/// the tokens, and models read a table at least as well.
fn brief(v: &HqView) -> String {
    let p = &v.picture;
    let mut s = String::new();
    let _ = writeln!(s, "YOUR COALITION: {}", v.side);
    let _ = writeln!(
        s,
        "Holding {} of {} objectives ({:.0}%). Treasury {} (reserve {}). {} human players on your side \
         ({} fixed-wing and {} helicopters airborne).",
        p.own_objectives,
        p.own_objectives + p.enemy_objectives,
        p.territory_pct,
        v.treasury,
        v.reserve,
        p.humans,
        p.humans_fixed_wing_airborne,
        p.humans_helo_airborne
    );
    if let Some(post) = v.posture {
        let _ = writeln!(
            s,
            "Current posture {:?}, main effort {}.",
            post,
            v.main_effort.as_ref().map(|o| format!("{} (#{})", o.name, o.id)).unwrap_or_else(|| "none".into())
        );
    }
    if !v.reasons.is_empty() {
        let _ = writeln!(s, "HQ's own assessment: {}.", v.reasons.join("; "));
    }

    let _ = writeln!(s, "\nOUR OBJECTIVES (id name kind health% logi% supply% fuel% threatened being-captured no-logi front-km inbound-supply):");
    let mut own: Vec<_> = p.objectives.iter().filter(|o| o.owner == "own").collect();
    own.sort_by(|a, b| a.front_km.total_cmp(&b.front_km));
    for o in own.iter().take(30) {
        let _ = writeln!(
            s,
            "{} {} {} {} {} {} {} {} {} {} {:.0} {}",
            o.id,
            o.name,
            o.kind,
            o.health,
            o.logi,
            o.supply,
            o.fuel,
            yes(o.threatened),
            yes(o.being_captured),
            yes(o.capturable),
            o.front_km,
            yes(o.inbound)
        );
    }
    if own.len() > 30 {
        let _ = writeln!(s, "(+{} more in the rear)", own.len() - 30);
    }

    let _ = writeln!(s, "\nENEMY OBJECTIVES NEAREST US (id name kind health% logi% supply% no-logi front-km):");
    let mut enemy: Vec<_> = p.objectives.iter().filter(|o| o.owner == "enemy").collect();
    enemy.sort_by(|a, b| a.front_km.total_cmp(&b.front_km));
    for o in enemy.iter().take(25) {
        let _ = writeln!(
            s,
            "{} {} {} {} {} {} {} {:.0}",
            o.id,
            o.name,
            o.kind,
            o.health,
            o.logi,
            o.supply,
            yes(o.capturable),
            o.front_km
        );
    }

    let _ = writeln!(
        s,
        "\nINTEL: {} enemy aircraft on our radars. {} enemy formations in contact with ours.",
        p.enemy_air_detected, p.enemy_formations_in_contact
    );
    if !p.enemy_sams.is_empty() {
        let v: Vec<String> = p.enemy_sams.iter().take(12).map(|c| format!("{} km from {}", c.near_km, c.near)).collect();
        let _ = writeln!(s, "Known enemy air defence: {}.", v.join("; "));
    }
    if !p.enemy_ground.is_empty() {
        let v: Vec<String> = p
            .enemy_ground
            .iter()
            .take(12)
            .map(|c| format!("{} x{} {} km from {} ({}m old)", c.class, c.count, c.near_km, c.near, c.age_mins))
            .collect();
        let _ = writeln!(s, "Enemy ground seen: {}.", v.join("; "));
    }
    let _ = writeln!(
        s,
        "Our ground formations: {} ({} idle). AI air up: {}. Supply runs out: {}. Troop insertions out: {}.",
        p.formations, p.formations_idle, p.ai_air_up, p.logistics_out, p.troops_out
    );

    let _ = writeln!(s, "\nOPERATIONS AVAILABLE (cost): {}", {
        let v: Vec<String> = v.available.iter().map(|(k, c)| format!("{} {c}", k.label())).collect();
        v.join(", ")
    });
    if !v.record.is_empty() {
        let r: Vec<String> = v
            .record
            .iter()
            .map(|r| format!("{} {}/{} succeeded", r.kind.label(), r.succeeded, r.succeeded + r.failed))
            .collect();
        let _ = writeln!(s, "RECORD SO FAR: {}.", r.join(", "));
    }
    let active: Vec<String> = v
        .ops
        .iter()
        .filter(|o| o.status == "active")
        .take(10)
        .map(|o| format!("{} on {}", o.kind.label(), o.target_name))
        .collect();
    if !active.is_empty() {
        let _ = writeln!(s, "UNDER WAY: {}.", active.join("; "));
    }
    let asks: Vec<String> = v
        .requests
        .iter()
        .filter(|r| r.status == "open")
        .map(|r| format!("{} asks {} at {}", r.by, r.kind.label(), r.target_name))
        .collect();
    if !asks.is_empty() {
        let _ = writeln!(s, "PILOTS ASKING: {}.", asks.join("; "));
    }
    if let Some(d) = v.directive.as_ref() {
        if let Some(r) = d.directive.rationale.as_ref() {
            let _ = writeln!(s, "\nYOUR LAST DIRECTIVE'S RATIONALE: {r}");
        }
    }
    s
}

/// Pull the directive out of a reply that may be fenced or padded.
fn parse(raw: &str) -> Result<Directive> {
    let start = raw.find('{').ok_or_else(|| anyhow!("no JSON object in reply"))?;
    let end = raw.rfind('}').ok_or_else(|| anyhow!("no JSON object in reply"))?;
    if end <= start {
        bail!("malformed JSON in reply");
    }
    // Through Value, not straight into the struct, so a model that writes
    // `"main_effort": "12"` still lands.
    let mut v: serde_json::Value = serde_json::from_str(&raw[start..=end])?;
    if let Some(me) = v.get_mut("main_effort") {
        if let Some(n) = me.as_str().and_then(|s| s.trim_start_matches('#').parse::<u64>().ok()) {
            *me = serde_json::json!(n);
        }
    }
    for key in ["defend", "supply_priority", "avoid"] {
        if let Some(serde_json::Value::Array(a)) = v.get_mut(key) {
            for x in a.iter_mut() {
                if let Some(n) = x.as_str().and_then(|s| s.trim_start_matches('#').parse::<u64>().ok()) {
                    *x = serde_json::json!(n);
                }
            }
            a.retain(|x| x.is_u64());
        }
    }
    let mut d: Directive = serde_json::from_value(v)?;
    // The weights the model invented a line for are dropped; the engine
    // clamps the rest.
    d.weights.retain(|l, w| Line::ALL.contains(l) && w.is_finite());
    Ok(d)
}

async fn query(db: &StatsDb, inst: &Inst, side: &str) -> Result<Option<HqView>> {
    let args = vec![("side", Value::from(side.to_string())), ("ucid", Value::from(String::new()))];
    let reply = tokio::time::timeout(Duration::from_secs(15), db.call_engine_rpc_optional(inst, "query-hq", args))
        .await
        .map_err(|_| anyhow!("query-hq timed out"))??;
    match reply {
        Value::String(s) => Ok(Some(serde_json::from_str(&s)?)),
        Value::Error(e) => {
            log::debug!("[{}] hq strategist: query-hq refused: {e}", inst.id);
            Ok(None)
        }
        other => bail!("unexpected query-hq reply {other:?}"),
    }
}

async fn send(db: &StatsDb, inst: &Inst, side: &str, d: &Directive) -> Result<String> {
    let args = vec![
        ("side", Value::from(side.to_string())),
        ("directive", Value::from(serde_json::to_string(d)?)),
    ];
    let reply = tokio::time::timeout(Duration::from_secs(15), db.call_engine_rpc_optional(inst, "hq-directive", args))
        .await
        .map_err(|_| anyhow!("hq-directive timed out"))??;
    match reply {
        Value::String(s) => Ok(s.to_string()),
        Value::Error(e) => bail!("engine refused the directive: {e}"),
        other => bail!("unexpected hq-directive reply {other:?}"),
    }
}

/// The per-instance loop. `writer` is the model; `every` the cycle.
pub(crate) async fn run(db: StatsDb, inst: Inst, writer: news_llm::WriterCfg, every: Duration) {
    // Let the engine come up and the HQ form its own first plan.
    tokio::time::sleep(Duration::from_secs(180)).await;
    let mut tick = tokio::time::interval(every);
    tick.set_missed_tick_behavior(tokio::time::MissedTickBehavior::Delay);
    let mut fails: u32 = 0;
    loop {
        tick.tick().await;
        let mut views = vec![];
        for side in ["blue", "red"] {
            match query(&db, &inst, side).await {
                Ok(Some(v)) if v.enabled => views.push((side, v)),
                Ok(_) => (),
                Err(e) => log::debug!("[{}] hq strategist: {side}: {e}", inst.id),
            }
        }
        // Nobody on the server: the HQ isn't planning, so neither are we.
        if views.iter().all(|(_, v)| v.picture.humans == 0) {
            continue;
        }
        for (side, v) in views {
            if v.override_.is_some() {
                log::debug!("[{}] hq strategist: {side} has human orders, standing by", inst.id);
                continue;
            }
            let prompt = brief(&v);
            let w = writer.clone();
            let res = tokio::task::block_in_place(|| news_llm::chat(&w, SYSTEM, &prompt, 900, 0.3));
            let raw = match res {
                Ok((raw, _)) => raw,
                Err(e) => {
                    fails += 1;
                    log::warn!("[{}] hq strategist: {side}: model call failed ({fails} in a row): {e}", inst.id);
                    if fails >= 3 {
                        // Back off: skip the next cycles in proportion.
                        tokio::time::sleep(every * fails.min(6)).await;
                    }
                    continue;
                }
            };
            fails = 0;
            let d = match parse(&raw) {
                Ok(d) => d,
                Err(e) => {
                    log::warn!(
                        "[{}] hq strategist: {side}: unusable reply ({e}): {:?}",
                        inst.id,
                        raw.chars().take(200).collect::<String>()
                    );
                    continue;
                }
            };
            let summary = format!(
                "posture {:?}, main effort {:?}, defend {:?}",
                d.posture, d.main_effort, d.defend
            );
            match send(&db, &inst, side, &d).await {
                Ok(_) => log::info!("[{}] hq strategist: {side}: {summary}", inst.id),
                Err(e) => log::warn!("[{}] hq strategist: {side}: {e}", inst.id),
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn a_fenced_reply_with_string_ids_parses() {
        let raw = "Here:\n```json\n{\"posture\":\"offensive\",\"main_effort\":\"#12\",\"defend\":[\"3\",4,\"x\"],\
                   \"weights\":{\"air\":1.4,\"troops\":2.0},\"intent\":\"Take Gori.\",\"rationale\":\"weak\"}\n```";
        let d = parse(raw).unwrap();
        assert_eq!(d.main_effort, Some(12));
        assert_eq!(d.defend, vec![3, 4]);
        assert_eq!(d.weights.get(&Line::Troops), Some(&2.0));
        assert_eq!(d.intent.as_deref(), Some("Take Gori."));
    }

    #[test]
    fn prose_is_refused() {
        assert!(parse("I recommend attacking Gori.").is_err());
    }

    #[test]
    fn the_brief_names_both_sides_of_the_line() {
        let mut v = HqView { side: "Blue".into(), enabled: true, treasury: 900, ..Default::default() };
        v.picture.objectives.push(bfprotocols::hq::ObjInfo {
            id: 1,
            name: "Senaki".into(),
            kind: "airbase".into(),
            owner: "own".into(),
            pos: [0., 0.],
            health: 80,
            logi: 100,
            supply: 40,
            fuel: 90,
            threatened: true,
            being_captured: false,
            capturable: false,
            front_km: 22.,
            inbound: false,
        });
        v.picture.objectives.push(bfprotocols::hq::ObjInfo {
            id: 2,
            name: "Gori".into(),
            kind: "fob".into(),
            owner: "enemy".into(),
            pos: [0., 0.],
            health: 30,
            logi: 0,
            supply: 50,
            fuel: 50,
            threatened: false,
            being_captured: false,
            capturable: true,
            front_km: 22.,
            inbound: false,
        });
        let b = brief(&v);
        assert!(b.contains("1 Senaki airbase 80"));
        assert!(b.contains("2 Gori fob 30 0 50 Y 22"));
        assert!(b.contains("Treasury 900"));
    }
}
