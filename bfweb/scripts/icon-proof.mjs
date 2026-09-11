/**
 * Regenerates the icon contact sheet from src/icons/index.tsx so the proof can
 * never drift from the source. Writes public/__icon-proof.html; that file is a
 * scratch artefact and is not committed.
 *
 *   node scripts/icon-proof.mjs
 */
import { readFileSync, writeFileSync } from 'node:fs'

const src = readFileSync(new URL('../src/icons/index.tsx', import.meta.url), 'utf8')

const GROUPS = []
for (const chunk of src.split(/\/\/ ── /).slice(1)) {
  const group = chunk.slice(0, chunk.indexOf(' ')).trim()
  const icons = [...chunk.matchAll(/export const (\w+) = createIcon\('\w+', <>(.*?)<\/>\)/gs)]
    .map(([, name, body]) => [name, body.replace(/\s+/g, ' ').replace(/ \/>/g, '/>').trim()])
  if (icons.length) GROUPS.push([group, icons])
}

const svg = (body, s) =>
  `<svg width="${s}" height="${s}" viewBox="0 0 24 24" fill="none" stroke="currentColor"` +
  ` stroke-width="1.5" stroke-linecap="square" stroke-linejoin="miter">${body}</svg>`

const rows = GROUPS.map(([group, icons]) => `
  <tr><td colspan="3" class="grp">${group.toUpperCase()} · ${icons.length}</td></tr>
  ${icons.map(([name, body]) => `<tr>
    <td class="name">${name.toUpperCase()}</td>
    <td><div class="cell">${svg(body, 16)}${svg(body, 24)}${svg(body, 48)}</div></td>
    <td class="w">${svg(body, 24)}</td></tr>`).join('')}`).join('')

const total = GROUPS.reduce((n, [, i]) => n + i.length, 0)

writeFileSync(new URL('../public/__icon-proof.html', import.meta.url), `<!doctype html><meta charset="utf-8">
<title>VS icon set</title><style>
body{background:#060908;color:#d7dbd6;font-family:ui-monospace,Menlo,Consolas,monospace;margin:0;padding:24px}
h1{font-size:12px;letter-spacing:.28em;color:#8ec83f;font-weight:700;margin:0 0 20px}
table{border-collapse:collapse;width:100%;max-width:640px}
td{padding:10px 0;border-top:1px solid #141c14;vertical-align:middle}
.grp{font-size:9px;letter-spacing:.22em;color:#8ec83f;padding:22px 0 6px;border-top:none}
.name{font-size:10px;letter-spacing:.16em;color:#98a396;width:190px}
svg{color:#8ec83f;display:block}.w svg{color:#d7dbd6}
.cell{display:flex;align-items:center;gap:26px}
</style><h1>VECTOR STRIKE ICON SET — ${total} GLYPHS · 16 / 24 / 48 px</h1>
<table>${rows}</table>`)

console.log(`wrote ${total} icons in ${GROUPS.length} groups`)
