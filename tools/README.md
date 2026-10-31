# tools

Standalone helpers that run straight from the file system: open them in a
browser, no build and no server.

| File | What |
|------|------|
| `action-cfg-editor.html` | Edits the engine config's `actions` (what each F10 action costs and spawns) |
| `deployable-cfg-editor.html` | Edits the engine config's `deployables` |

## The offline fallback to the dashboard's config editor

The normal way to change the engine config is the dashboard's
**Admin → Config** page (`/admin/config`, `bfweb/src/pages/ConfigEditorPage.tsx`).
It renders a form for the whole config from the JSON Schema bfdb serves,
validates as you type and saves straight to the server.

These two pages cover the same two sections offline. Use them when the
dashboard is not available to you: bfdb was started without
`--engine-config`, it is down, you are preparing a config for a server that
is not running yet, or you have a config file on disk and nothing else. Load
a whole `cfg.json` (or just the section), edit, and **Download JSON**; the
file then goes onto the server by hand.

They know the config's shape from their own code, not from bfdb's schema, so
a field added to the engine later shows up in the dashboard's editor first.
Check the dashboard's form when something looks missing here. The action
editor remembers its unit list in the browser's localStorage.
