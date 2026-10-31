# bflib: three overlapping AI command layers

Audit finding f126. Status: **merged as far as it goes without changing AI
behaviour** (branch `bflib-redesign-870271`); see "What was done" at the
end for the state now and what is left.

## What exists

Three layers decide what the AI does with a side's money and forces, plus a
fourth for ground formations. They grew one after another, each taking over
part of the previous one's job without removing it.

| Layer | Where | Decides | Runs |
|---|---|---|---|
| Smart Commander | `bflib/src/commander.rs` (~860 lines), called from `tick_smart_commander` in `lib.rs` | Treasury income, objective funding, holding bonuses (`commander::tick`); and, when the HQ is off, one strategic campaign event per check (`commander::tick_events`: CAP, strikes, ALCM, ambushes picked by `score_actions`) | slow pass, every `smart_commander.tick_period_secs` |
| Theatre HQ | `bflib/src/hq/` (~5,000 lines: picture, strategy, planner, dispatch, requests, support, chat, view) | Per side: a picture of the war, a posture and main effort (human override > LLM directive > rules), every operation it could run valued and bought from the treasury, dispatched through the systems that already exist (`spawn_auto_package`, actions, logistics, campaign events), tracked to the end | slow pass (`hq::tick`), when `smart_commander.hq` is set |
| Command / orders | `bflib/src/command.rs` (who may command, ranks from bfdb) and `bflib/src/command/orders.rs` (~970 lines: the command map's order catalogue, hunter groups) | What a *human* commander can order from the dashboard's command map, paid from the same treasury; keeps hunter groups alive (`tick_hunters`) | on RPC, plus `tick_hunters` in the slow pass |
| Ground AI | `bflib/src/groundwar/ai.rs` | Formations: withdraw, counter-attack, raise, attack (following the tempo's axis or the HQ's main effort), screen | `groundwar::tick` |

### Where they overlap

- **Strategic events.** `commander::tick_events` and the HQ planner both buy
  CAP, strikes and ambushes. Today `tick_smart_commander` returns early when
  `hq::active`, so they do not both run -- but the two pickers are separate
  code with separate scoring (`commander::score_actions` vs
  `hq::planner`), separate assessments of the battlefield
  (`commander::build_side_assessment` vs `hq::picture`), and separate
  config. A fix to one does not reach the other.
- **The picture of the war.** At least three: `commander::ObjAssessment`,
  `hq::picture`, `groundwar::picture`. Each walks the objectives, works out
  threat and supply, and finds the front.
- **Spending.** The treasury is debited by `commander` (events), the HQ
  (operations) and `command::orders` (human orders), each with its own
  checks.
- **Dispatch.** `hq::dispatch` and `command::orders` both turn "do X at Y"
  into engine calls (`spawn_auto_package`, actions, events), with their own
  copies of the argument plumbing.

## Plan

Goal: one picture, one valuation, one dispatcher; the layers that remain
differ only in *who* chooses (rules, LLM directive, human).

1. **Pin behaviour first.** Add tests around the pure parts each layer
   already has: `commander::score_actions` / `select_best_action`,
   `hq::planner` valuation, `groundwar::ai` choices. They take `&Db` plus
   config; a `Db::default()` with a few objectives inserted is enough. Without
   these, the merge below can't be checked.
2. **One picture.** Make `hq::picture` the only battlefield model. Port
   `commander::build_side_assessment`'s fields that `hq::picture` lacks, then
   have `commander` and `groundwar::picture` read from it (built once per
   slow pass, stored in `Context`, instead of three walks of every
   objective).
3. **Retire `commander::tick_events`.** When the HQ is off, run the HQ planner
   in a "one operation per check" mode that reproduces the old picker's
   cadence (`side_should_check`), instead of a second picker. Keep the config
   keys working: map `smart_commander` event settings onto planner limits, and
   log once when old keys are in use. `commander.rs` keeps only the economy
   (`tick_treasury_income`, `tick_objective_funding`, `tick_holding_bonuses`)
   and is renamed to say so (`economy.rs`).
4. **One dispatcher.** Move the "operation -> engine call" code into
   `hq::dispatch` and make `command::orders` build an HQ operation and hand it
   over, so a human order and an AI one go down the same path (same spending
   checks, same tracking, same success record).
5. **Ground AI reads the plan.** `groundwar::ai` already follows the HQ's main
   effort when there is one; make that the only input (posture, main effort,
   bases to hold) and drop its private objective scoring.

Each step is its own PR and leaves the game playable. Steps 3 and 4 change AI
behaviour on servers running without the HQ, so they need a live session on
such a server before release.

Rough size: step 1 two days, step 2 two to three, step 3 three, step 4 two,
step 5 one to two.

## What was done

The merge stops where the next step would change what the AI does on an
existing config. On every config the AI makes the same decisions as before.

- **One planner, two modes.** `hq::tick` is the AI's planning pass for both
  sides on every server with a `smart_commander`. With the theatre HQ on it
  runs the full planner as before. With it off it runs the **rules mode**
  (`hq/rules.rs`): the Smart Commander's old event picker
  (`commander::tick_events`), moved into the HQ unchanged. One event per
  pass, best value per treasury point, same cadence
  (`tick_period_secs`, `check_interval_secs`, the 30 s emergency interval,
  `max_concurrent_events` with the capture bypass), same keys. The second
  picker is gone as a separate layer, and `hq::active` decides the mode in
  one place.
- **Pinned.** The scorer is now a pure function (`rules::score` over
  `rules::Inputs`, which `Inputs::gather` reads from the db first), with
  tests for every buy (barrage range and weakness, missile range, cost and
  one-at-a-time, ambush, CAP balance, cooldown and cover), the
  value-per-point choice and the check cadence.
- **One picture module.** The rules mode's objective summary
  (`build_side_assessment`, ALCM groups, fixed-wing pilot count) is now
  `hq::picture::{side_objectives, missile_groups, fixedwing_pilots}`, next to
  the HQ's own picture.
- **One spending path.** `hq/treasury.rs` (`scaled`, `afford`, `shortfall`,
  `pay`) is what the HQ's planner and dispatcher, the rules mode and the
  command map's human orders (`command.rs`, `command/orders.rs`) all price
  and pay through.
- **The economy on its own.** What was left of `commander.rs` (treasury
  income, objective funding, holding bonuses) is `economy.rs`, run by
  `tick_economy` in the slow pass just before `hq::tick`, as before.

## What is left, and why

- **Rules mode -> HQ valuation** (plan step 3 in full): running the HQ
  planner in a one-operation-per-check mode *instead of* the rules scorer.
  The two value things differently (the HQ's operation valuation vs the
  rules mode's weakness/strategic formula), so this changes what a no-HQ
  server's AI buys. It needs a design decision and a live session on such a
  server, not a refactor.
- **One dispatcher for human orders** (step 4): command-map orders start
  actions with `start_action` at cost 0 and pay up front, while the HQ uses
  `start_treasury_action` and records the operation. Routing human orders
  through `hq::dispatch` would make them HQ operations (tracked, counted in
  the HQ's record, shown in its ops list): visible behaviour, so not done
  here. They share the spending path now.
- **Ground AI from the plan only** (step 5): `groundwar::ai` already
  follows the HQ's main effort when there is one; dropping its own objective
  scoring changes formations' choices on servers without the HQ.
- **One picture for everything** (step 2 in full): `hq::picture::build`,
  the rules mode's summary and `groundwar::picture` still each walk the
  objectives. Folding them into one cached picture per pass is a
  performance change; the three compute different things today and merging
  them is only safe together with the steps above.
