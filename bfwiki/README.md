# Fowl Engine Wiki

Player/gameplay wiki for the Fowl Engine (BFNEXT) dynamic campaign system. A React + Vite SPA, styled to match `bfweb`'s military ops-dashboard identity — reading is public, editing requires an admin login (same Discord OAuth / local admin login as the `bfweb` dashboard, since both talk to the same `bfdb` backend).

Content lives in `bfdb`'s database (a `wiki_pages` Sled tree), not as files in this repo — it's seeded once from `bfdb/seed_wiki/*.md` on first run, then editable live through the site by anyone with admin access. `bfdb/seed_wiki/` is the only copy of the player docs in the repo (the old mdBook `user-guide/` was folded into it); server setup docs live in `deploy/server-setup/`.

## Development

Requires a local `bfdb` instance running (see repo root `bfsystem.ps1` / `TESTING.md`) — the dev server proxies `/api` to `http://localhost:8880` (see `vite.config.ts`).

```bash
npm install
npm run dev
```

## Build

```bash
npm run build
npm run preview
```

Static output goes to `dist/`.

## Deploying to Vercel

1. Import this repo as a new Vercel project with the project root set to `bfwiki/` (Vercel auto-detects the Vite framework preset — build command `npm run build`, output `dist`).
2. In the project's **Settings → Environment Variables**, add `VITE_API_BASE = https://api.vectorstrike.org` (no trailing slash) — same bfdb instance bfweb/bfsite already talk to. This is a build-time var; redeploy after adding/changing it. Optionally add `VITE_SNAPSHOT_URL = https://snapshot.vectorstrike.org` too, so the wiki stays readable (read-only) while bfdb is down; see `deploy/snapshot-fallback.md`.
3. Set the project's domain to `wiki.vectorstrike.org` (Settings → Domains), matching `vectorstrike.org` / `dashboard.vectorstrike.org`.
4. `https://wiki.vectorstrike.org` must be in bfdb's `--cors-origin` allow-list (`$corsOrigins` in the repo root `bfsystem.ps1`) for cross-origin admin login/edit to work — already added there, takes effect the next time bfdb is (re)started.

See `deploy/README.md` in the repo root for the general cross-origin setup this follows (CORS allow-list, cookie `SameSite` mode).

## Per-server numbers: `{{cfg:...}}`

One bfdb can front several DCS servers (`--instances`, see
`deploy/multi-instance.md`), and **each runs its own engine config** — different
point values, capture timings, action costs. The wiki's prose is shared between
them; the numbers are not.

So a page never hard-codes a campaign number. It writes the path to it, with a
fallback:

```markdown
An air kill is worth {{cfg:points.air_kill|350}} points.
Consolidation takes {{cfg:capture_consolidation_secs|300}} seconds.
Standard squads get {{cfg:default_lives.Standard[0]|3}} lives.
```

At render time `WikiMarkdown` substitutes each token from
`GET /api/wiki/facts`, which serves an allow-listed subset of the **selected
instance's** engine config (`WIKI_FACT_KEYS` in `bfdb/src/main.rs` — it
deliberately excludes the admin table, the ban list, the netidx base and the
CheckWX key). Paths index into that object with `.` for keys and `[n]` for
array indices.

- **Always write a fallback** (`|350`). It is what a reader sees if the server
  has no engine config wired up, the key is absent, or the request fails — and
  it keeps the raw markdown readable in the editor and in git.
- Fallbacks should be the **engine's own default** for that key, not a made-up
  number, so a server that doesn't set it still reads correctly.
- A page that contains any token shows a note naming which server's numbers it
  is displaying, with a selector inline.
- The topbar selector (`InstanceSelect`) renders **only** when this bfdb fronts
  more than one server, so single-server deployments see nothing.
- `?instance=<id>` in the URL pins a page to one server's numbers, and the
  choice is remembered in `localStorage`.

Implementation: `src/lib/cfgTokens.ts` (parser), `src/context/InstanceContext.tsx`
(selection + fact fetch), `src/components/InstanceSelect.tsx` (the control).

## Mockup blocks: live replicas of the game

A fenced code block whose language is one of these draws as a working copy
of what the player sees in DCS, not as code. The body is JSON except for
`dcsmessage`.

| Block | Draws | Body |
|---|---|---|
| ` ```f10menu ` | The F10 radio menu, top left: `F1.`..`F10.`, `F11. Back`, `F12. Exit`, submenus ending in `...`, more than ten entries split into `Next Page...`. Click an entry or press its number (`1`-`9`, `0` for F10, `Backspace`, `Esc`, `\` to reopen). A command shows its `says` in the message box and its `note` under the screen | `{"root":"F10 Other","items":[{"label":"Cargo","items":[{"label":"Unpack","says":"...","note":"..."}]}]}` |
| ` ```dcsmessage ` | The top-right message box | plain text, exactly as printed |
| ` ```cockpitpanel ` | The Vector Strike panel (colours, fonts, tab bar, rows, status line and emblem from `bfcockpit.lua`). A button row's `says` goes in the status line | `{"tab":"CARGO","rows":[{"heading":"..."},{"text":"..."},{"button":"label","says":"..."}],"status":"..."}` |
| ` ```wheel ` | The radial wheel (geometry from `Theme/wheel/layout.lua`, themes green/blue/amber/red/white, the plugin's icons). Point at a group to open its arc, click to fire or go in, the hub goes back; destructive entries ask twice; arrow keys work as in the plugin | `{"theme":"green","items":[{"label":"Cargo","items":[{"label":"Unload","says":"..."}]}]}` |
| ` ```steps ` | A walkthrough with Back/Next and progress | `[{"title":"...","text":"markdown","image":"/api/wiki/images/<uuid>"}]` |

Optional extras beyond that contract: `cockpitpanel` takes `crumb`, a text
row's `style` (`dim`, `bad`, `warn`, `good`, `blue`, `mono`, `answer`), and a
button's `sub`, `go` and `enabled`; `wheel` takes `title`/`subtitle` for the
hub and per item `icon`, `hub` and `danger`; a step takes `alt`. Unknown
fields are ignored.

A body that does not parse draws as a red box saying what is wrong and
where, with the source under it, never as a blank. The editor has a
**Blocks** panel above the text area with a copy-paste (or insert) example of
each, and its live preview draws them as you type.

Code: `src/components/blocks/` (parsers in `parse.ts`, the menu and wheel
behaviour in `f10menuModel.ts` / `wheelModel.ts`, examples in `examples.ts`).
The wheel icons and emblem are copies of the plugin's, in `public/cockpit/`.
Under `npm run dev`, `/__blocks` draws every block without a bfdb, and
`node scripts/shoot-blocks.mjs` (dev server on port 5274, headless Edge or
Chrome) refreshes the pictures in `docs/blocks/`.

## Admin editing

Log in (top-right "ADMIN LOGIN") with the same Discord account / local admin credentials used for the `bfweb` dashboard — `is_admin` is a property of the session, not something configured per-app. Once logged in as admin:

- **Edit** button on any page opens it in the Markdown editor with a live preview.
- **New Page** (topbar) creates a page at a slug you choose (e.g. `gameplay/new-topic` — this becomes the URL path and its position in the sidebar comes from the `section`/`order` fields).
- **Delete** removes a page permanently.
- The editor's live preview resolves `{{cfg:...}}` tokens the same way the
  published page does, so you can see the real numbers while writing.

Note that an admin edit **detaches that page from the seed**: `seed_wiki_if_empty`
only rewrites pages still marked `updated_by == "seed"`, so a page edited through
the site stops receiving updates from `bfdb/seed_wiki/`. Prefer editing the
markdown in the repo for anything that should keep tracking the engine.
