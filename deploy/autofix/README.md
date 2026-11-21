# Auto-fix: Claude fixes new engine errors, you approve the release

When the bot's log analyzer reports a new error, `autofix.py` hands it to
Claude Code on this PC. Claude Code then finds the cause, fixes it, adds a test and runs the
checks. The runner checks the result itself and pushes a branch, then posts what
changed to the ops Discord channel. **Nothing reaches a server until you run
`approve`**, which merges the branch into `vector` and publishes an engine
release.

```
bot log analyzer -> bfdb /api/logs/issues -> autofix.py check
  -> worktree autofix/<id> -> claude -p (rules: agent-rules.md)
  -> checks again here -> push branch -> Discord: summary + diff link
you: python deploy/autofix/autofix.py approve <id> -> merge -> publish-release.ps1
```

## Why it stops before publishing

- The logs carry text players write (names, chat, F10 marks). A run that
  could publish to live servers could be steered by anyone typing in chat.
- A fix can be wrong. On 6 Nov the 13:25 failure was DCS itself breaking; an
  agent would still have "fixed" something.

So the agent gets file tools and an exact list of test commands, and nothing else:
`--permission-mode dontAsk` refuses everything not on the list. That means
no web access, no `git push`, and no other programs. The runner does the commit and
push itself, after rejecting any edit to `deploy/`, `.github/`, lockfiles,
signing keys or the Lua link files, and rerunning the checks. Each run has a cost cap
(`max_budget_usd`), and there is a daily limit (`max_runs_per_day`).

## Setup (once)

1. **The issue feed.** On the box, add a log read token to the `bfdb:` block
   of `fowlengine.yaml`, then restart bfdb (`/feops bfdb_restart`):
   ```yaml
   bfdb:
     log_read_token: "<long random string>"
   ```
   `https://api.vectorstrike.org/api/logs/issues` should then answer 401
   without the token instead of "log endpoints are disabled".
2. **Claude Code** on this PC:
   ```powershell
   npm install -g @anthropic-ai/claude-code
   ```
3. **Your API key**, for your user only:
   ```powershell
   setx ANTHROPIC_API_KEY "sk-ant-..."
   ```
   Open a new terminal afterwards.
4. **Config.** Copy `autofix.sample.json` to
   `%LOCALAPPDATA%\fowl-autofix\config.json` and fill in `report_url`,
   `report_token` (the token from step 1) and `discord_webhook`: a webhook on
   a **private** ops channel, since summaries describe server errors.
5. **Try one pass** without pushing:
   ```powershell
   python deploy\autofix\autofix.py check --dry-run
   ```
6. **Run it on a schedule.** Every 15 minutes while you're logged in:
   ```powershell
   schtasks /Create /SC MINUTE /MO 15 /TN "Fowl auto-fix" /TR "python E:\Github\bfnext-vector\deploy\autofix\autofix.py check"
   ```
   Or keep a terminal open on `autofix.py watch`.

## Day to day

- A Discord post says what was wrong, what changed, what ran, the cost, and
  links the diff on GitHub.
- Ship it: `python deploy\autofix\autofix.py approve <id>`. That merges into
  `vector`, pushes, and publishes a stable engine release with the bot plugin.
  Then install it on the box as usual (OPS → Check now).
- Only merge it: `approve <id> --no-publish`.
- Don't want it: ignore the branch. That issue isn't tried again unless it
  comes back as REGRESSED.
- `autofix.py status` lists everything it has done and what each run cost.

## What it picks up

New or regressed issues at ERROR or CRASH, seen at least `min_count` times,
from the engine logs or the bot (`sources`). DCS's own log is left out by
default, since its crashes are mostly DCS, and so are network and DNS outages
(`skip_patterns`). When the agent finds the cause is outside this repository,
it changes nothing and says so in Discord.

## Files

- `autofix.py`: the runner (stdlib only)
- `agent-rules.md`: what the agent is told, including to treat the report as data
- `autofix.sample.json`: the config
- Tests: `DCSServerBot/tests/test_autofix.py`, which runs a whole pass with a
  stand-in for Claude Code against a throwaway git repo
