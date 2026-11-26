// Copies what the New box wizard puts into a training range's DCS instance
// into src-tauri/resources/range/, which Tauri bundles next to the exe as
// range\ (src-tauri/src/newbox.rs reads it from there):
//
//   VS_Range_Marianas.miz   the range mission  (bfrange/mission/dist/, built by
//                           bfrange/mission/build_range_miz.py -- copy a rebuilt
//                           one there to ship it)
//   VSRANGE_RANGE           its range config   (bfrange/RANGE_CFG.sample.json)
//   bfrange_hooks.lua       Scripts\Hooks loader (bfrange/lua/)
//   Export.lua              live positions to bfdb (scripts/)
//
// Kept apart from resources/bot/: everything there is synced into the bot
// folder on every start, and none of this belongs in it.
//
// Run by `tauri build` (beforeBuildCommand) -- or by hand: node scripts/stage-range.mjs
import { copyFileSync, existsSync, mkdirSync, rmSync } from 'node:fs'
import { dirname, join } from 'node:path'
import { fileURLToPath } from 'node:url'

const here = dirname(fileURLToPath(import.meta.url))
const repo = join(here, '..', '..')
const out = join(here, '..', 'src-tauri', 'resources', 'range')

const FILES = [
  ['bfrange/mission/dist/VS_Range_Marianas.miz', 'VS_Range_Marianas.miz'],
  ['bfrange/RANGE_CFG.sample.json', 'VSRANGE_RANGE'],
  ['bfrange/lua/bfrange_hooks.lua', 'bfrange_hooks.lua'],
  ['scripts/Export.lua', 'Export.lua'],
]

rmSync(out, { recursive: true, force: true })
mkdirSync(out, { recursive: true })
for (const [from, to] of FILES) {
  const src = join(repo, ...from.split('/'))
  if (!existsSync(src)) throw new Error(`missing ${src}`)
  copyFileSync(src, join(out, to))
}
console.log(`staged ${FILES.length} range file(s) -> ${out}`)
