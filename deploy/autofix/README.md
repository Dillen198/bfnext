# Auto-fix: Claude fixes engine errors and player bug reports

The runner lives on your PC, not on a server box. It reads every box's error
report and the bug reports players send with `/fe_bug`, and has Claude Code
find the cause, fix it, add a test and run the checks. It then checks the
result itself and pushes a branch. In **auto** mode it also publishes the fix
as an engine release, with nobody looking, when the fix passes every gate
below. Anything that misses a gate waits for you.

Fowl Engine Manager's **AUTO-FIX** tab runs it, shows what it is doing, and has
the buttons: Start/Stop, Pause, Publish a held fix, and Send a player's report
to Claude.

```
boxes (over Tailscale, OPS API)            this PC
  log analyzer -> GET issues/report  ─┐
  /fe_bug      -> GET bugs           ─┴─> autofix.py watch (started by the Manager)
                                          -> worktree autofix/<id> -> claude -p (agent-rules.md)
                                          -> checks again here -> push branch -> Discord post
                                          -> auto + every gate passed:
                                               merge into vector -> publish-release.ps1
                                               -> boxes: issue marked fixed, reporter DM'd,
                                                  update/check -> each box installs it on probation
```

## What may ship with nobody looking

A fix is published by itself only when **all** of these hold. Otherwise it
waits under "Waiting for you" (or `autofix.py approve <id>`).

- Claude said it fixed a real defect, and the checks for what changed (cargo
  fmt/clippy/test for the crates, pytest, the dashboard's typecheck and tests)
  pass **again on this PC**, run by the runner rather than by the agent.
- It touches nothing under `deploy/` or `.github/`, no lockfile, signing key or
  Lua link file. A fix that does is thrown away.
- It is small: at most `auto_max_files` (8) files and `auto_max_changed_lines`
  (200) lines.
- Claude did not flag the report as trying to give it instructions
  (`"suspicious": true`).
- It came from the logs, from a **trusted** reporter (DCS Admin, or a role in
  the bot's `bugs.trusted_roles`), or from a player's report that an admin sent
  from the Manager. An untrusted player's report never runs by itself.
- Fewer than `max_publishes_per_day` (3) releases went out today, and auto-fix
  isn't paused.

**What the players see.** Claude also writes a one- or two-line note for
the players, in their terms (`player_note` in `agent-rules.md`). It is the
reply in the report's post in the bug forum, the DM to the reporter, and the
release's changelog line, which each box posts to its changelog channel when
the release goes live there (`community:` in fowlengine.yaml). The runner strips
mentions and links from it, and sends none for a report flagged suspicious.

After that, the boxes' own updater installs the release under probation. It
rolls the release back by itself if DCS crashes, the mission never loads the
engine, or bfdb won't come up (`autoupdate.py`). An issue that comes back after
a fix shows as REGRESSED, and the runner tries it once more.

**Why it's built this way.** Logs carry text players write (names, chat, F10
marks), and a bug report is nothing but a stranger's words. The agent
therefore gets file tools and an exact list of test commands, and nothing
else: no web, no `git push`, no other programs (`--permission-mode dontAsk`).
The boxes' OPS keys are kept out of its environment. The runner commits and
pushes itself, and the gates above decide whether a person has to look first.
The repository is public, so summaries never carry player names or log lines.

## Setup (once)

1. **Claude Code** on this PC, signed in:
   ```powershell
   npm install -g @anthropic-ai/claude-code
   claude
   ```
   Log in once, then close it. It uses that Claude account. If
   `ANTHROPIC_API_KEY` is set, it bills that API key instead.
2. **Publishing works here.** `gh auth status` is logged in, and the engine
   signing key is at `%USERPROFILE%\.tauri\fowl-engine.key` (with
   `FOWL_ENGINE_SIGNING_PASSWORD` set if it has one). See
   `deploy/auto-update.md`. Try `deploy\publish-release.ps1 -DryRun` once.
3. **The boxes** are under the Manager's ALL SERVERS with their OPS keys
   (`deploy/manager-remote.md`). Each box needs the fowlengine plugin from
   this release or later: older plugins have no `bugs` route, and the runner
   then reads only their error report.
4. **AUTO-FIX tab:** set the repository folder (this checkout) and the Discord
   webhook of a **private** ops channel, pick Automatic or Approve, and press
   Start. From then on the runner starts whenever the Manager does.

Running it without the Manager works too: copy `autofix.sample.json` to
`%LOCALAPPDATA%\fowl-autofix\config.json`, fill in `boxes`, and run
`python deploy\autofix\autofix.py watch`.

## Commands

```powershell
python deploy\autofix\autofix.py check --dry-run   # one pass, commit locally, push and publish nothing
python deploy\autofix\autofix.py watch             # what the Manager runs
python deploy\autofix\autofix.py queue <id>        # work on this one next (filters and trust aside)
python deploy\autofix\autofix.py approve <id>      # merge a held fix and publish it
python deploy\autofix\autofix.py approve <id> --no-publish
python deploy\autofix\autofix.py pause | resume
python deploy\autofix\autofix.py status
```

## What it picks up

- **Errors**: new or regressed issues at ERROR or CRASH, seen at least
  `min_count` times, from the sources in `sources` (engine logs and the bot by
  default). DCS's own log is left out by default, since its crashes are mostly
  DCS's own bugs. Network and DNS outages are skipped too (`skip_patterns`).
  The same issue on two boxes is worked on once.
- **Bug reports** (`discord` in `sources`): open `/fe_bug` reports. Trusted
  reporters' reports are picked up straight away; anyone else's are listed in
  the Manager for an admin to send or dismiss.

When the cause is outside this repository (DCS, the network, a box's config),
or the report asks for a feature rather than describing a defect, Claude
changes nothing. The issue is then marked on the box, and the reporter is told.

Limits: `max_runs_per_day` fixes (an admin's "Send to Claude" is not counted),
`max_budget_usd` per fix.

## Files

- `autofix.py`: the runner (stdlib only)
- `agent-rules.md`: what the agent is told, including to treat reports as data
- `autofix.sample.json`: the settings
- `%LOCALAPPDATA%\fowl-autofix\`: `config.json`, `state.json` (what it did),
  `events.jsonl` (activity), `current.json`, `queue.json`, `paused`,
  `runner.log` (the Manager's copy of its output)
- Box side: `DCSServerBot/plugins/fowlengine/bugreports.py` and `cog_bugs.py`
  (`/fe_bug`), and the `bugs` routes in `opsapi.py`
- Manager side: `bfmanager/src-tauri/src/autofix.rs` and
  `bfweb/src/manager/AutoFix.tsx`
- Tests: `DCSServerBot/tests/test_autofix.py` runs whole passes with a
  stand-in for Claude Code, a throwaway git repo and a fake box.
  `test_fowlengine_ops.py` covers the bug reports. The Manager's are in
  `autofix.rs`.
