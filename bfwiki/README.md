# Fowl Engine Wiki

Player/gameplay wiki for the Fowl Engine (BFNEXT) dynamic campaign system. A React + Vite SPA, styled to match `bfweb`'s military ops-dashboard identity — reading is public, editing requires an admin login (same Discord OAuth / local admin login as the `bfweb` dashboard, since both talk to the same `bfdb` backend).

Content lives in `bfdb`'s database (a `wiki_pages` Sled tree), not as files in this repo — it's seeded once from `bfdb/seed_wiki/*.md` on first run, then editable live through the site by anyone with admin access. This is a separate, standalone site from `user-guide` (the mdBook guide, one level up in the repo).

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
2. In the project's **Settings → Environment Variables**, add `VITE_API_BASE = https://api.vectorstrike.org` (no trailing slash) — same bfdb instance bfweb/bfsite already talk to. This is a build-time var; redeploy after adding/changing it.
3. Set the project's domain to `wiki.vectorstrike.org` (Settings → Domains), matching `vectorstrike.org` / `dashboard.vectorstrike.org`.
4. `https://wiki.vectorstrike.org` must be in bfdb's `--cors-origin` allow-list (`$corsOrigins` in the repo root `bfsystem.ps1`) for cross-origin admin login/edit to work — already added there, takes effect the next time bfdb is (re)started.

See `deploy/README.md` in the repo root for the general cross-origin setup this follows (CORS allow-list, cookie `SameSite` mode).

## Admin editing

Log in (top-right "ADMIN LOGIN") with the same Discord account / local admin credentials used for the `bfweb` dashboard — `is_admin` is a property of the session, not something configured per-app. Once logged in as admin:

- **Edit** button on any page opens it in the Markdown editor with a live preview.
- **New Page** (topbar) creates a page at a slug you choose (e.g. `gameplay/new-topic` — this becomes the URL path and its position in the sidebar comes from the `section`/`order` fields).
- **Delete** removes a page permanently.
