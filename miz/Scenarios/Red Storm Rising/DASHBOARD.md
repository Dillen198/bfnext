# Red Storm Rising: the dashboard

The Southern Flank campaign has **its own dashboard, `bfrsr-web/`**. It
started as a copy of the campaign dashboard (`bfweb`) and is redesigned
throughout for the 1985 war. The existing dashboard is not changed.

File references are to this repository as of October 2026.

## 1. Approach

- **A separate app.** `bfrsr-web/` is its own React + Vite app, built and
  deployed on its own, like the range dashboard (`bfrange-web/`). Changes to
  it never touch `bfweb`, and `bfweb` keeps its look.
- **Same data.** It reads the same bfdb API as `bfweb`, for the Red Storm
  instance only (`VITE_INSTANCE`).
- **Shared assets, not copied.** The map imagery and 3D models (about
  120 MB) stay in `bfweb/public`; `bfrsr-web/vite.config.ts` serves them in
  dev and copies them into its build.
- **The cost of a copy:** a fix or feature in `bfweb` doesn't reach
  `bfrsr-web` by itself. Port what matters by hand.

## 2. The design: NATO Southern Command's situation room, 1985

- **Shell:** a 1980s command console instead of the sidebar: function-key
  tabs (SITUATION, PLOT, WIRE, FRONT HQ, TARGETS, HONOURS, LOSSES,
  BRIEFING), an LED mission clock, a classification banner.
- **Look:** CRT phosphor on gunmetal consoles, typewriter paper for reports,
  acetate map overlays, rubber stamps. NATO in steel blue and map green, the
  Soviet side in red and brass. Light mode is paper documents.
- **SITUATION (home):** a situation board: the plot map in the centre, a
  teletype roll of engagements, the holdings board, the latest dispatch as a
  newspaper clipping, and panels for will to fight and story events (shown
  once the engine provides them).
- **WIRE (news):** *The Allied Dispatch*, with the Soviet *Front Bulletin*
  beside it when the day has one.
- **Everything else** restyled to match: objectives as target folders, the
  leaderboard as an honours board, the kill feed as loss reports, pilots as
  personnel files.

## 3. What supports it outside the app

These were built for the Red Storm campaign and stay:

| Part | What | Where |
|---|---|---|
| Instance theme and name | `theme: "redstorm"` and `brand_name` per instance, sent with `/api/instances` | `bfdb/src/instance.rs`, `bfdb/src/routes/campaign.rs` |
| Two-voice news | A Western 1985 wire dispatch and a Soviet front communiqué (`alt`) for each day | `bfdb/src/news_llm.rs`, `bfdb/src/news.rs`, `bfdb/src/tasks.rs`, `bfdb/src/worker/` |
| 1985 news pictures | `ColdWar1985` image era | `bfdb/src/news_image.rs` |
| Discord | Name, emblem, colour and newspaper title per instance | `DCSServerBot/plugins/fowlengine/branding.py` |

## 4. Work list

| # | Work | Status |
|---|---|---|
| D1 | `bfrsr-web` created from `bfweb`, shared assets, CI | Done |
| D2 | The redesign (section 2) | In progress |
| D3 | Deploy: a Vercel project from `bfrsr-web/` with `VITE_API_BASE` and `VITE_INSTANCE`, its own domain | Not started |
| D4 | Front HQ banner: how much of each side the AI runs | After E4 |
| D5 | Will to fight and the story timeline | After E5 and E5b |
| D6 | Fuel and stocks board | |
| D7 | Order of battle page | After E6 |
