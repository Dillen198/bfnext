// Copies the DCSServerBot pieces Fowl Engine Manager installs into the bot
// (plugins/fowlengine + extensions/bf*) into src-tauri/resources/bot/, which
// Tauri bundles next to the exe as bot\, and writes the manifest.json the
// service syncs by (path -> sha256).
//
// Left out on purpose: fowlengine.yaml (a live config can hold secrets and
// belongs in DCSServerBot's config\ anyway), __pycache__, *.pyc.
//
// Run by `tauri build` (beforeBuildCommand) -- or by hand: node scripts/stage-bot.mjs
import { createHash } from 'node:crypto'
import { cpSync, existsSync, mkdirSync, readdirSync, readFileSync, rmSync, statSync, writeFileSync } from 'node:fs'
import { dirname, join, relative, sep } from 'node:path'
import { execSync } from 'node:child_process'
import { fileURLToPath } from 'node:url'

const here = dirname(fileURLToPath(import.meta.url))
const repo = join(here, '..', '..')
const botSrc = join(repo, 'DCSServerBot')
const out = join(here, '..', 'src-tauri', 'resources', 'bot')

const SKIP_NAMES = new Set(['__pycache__', 'fowlengine.yaml', '.pytest_cache'])
const skip = (name) => SKIP_NAMES.has(name) || name.endsWith('.pyc')

const dirs = ['plugins/fowlengine']
for (const e of readdirSync(join(botSrc, 'extensions'), { withFileTypes: true })) {
  if (e.isDirectory() && e.name.startsWith('bf')) dirs.push(`extensions/${e.name}`)
}

rmSync(out, { recursive: true, force: true })
mkdirSync(out, { recursive: true })

const files = {}
function walk(abs) {
  for (const e of readdirSync(abs, { withFileTypes: true })) {
    if (skip(e.name)) continue
    const p = join(abs, e.name)
    if (e.isDirectory()) walk(p)
    else if (e.isFile()) {
      const rel = relative(botSrc, p).split(sep).join('/')
      const dest = join(out, ...rel.split('/'))
      mkdirSync(dirname(dest), { recursive: true })
      cpSync(p, dest)
      files[rel] = createHash('sha256').update(readFileSync(p)).digest('hex')
    }
  }
}
for (const d of dirs) {
  const abs = join(botSrc, ...d.split('/'))
  if (!existsSync(abs) || !statSync(abs).isDirectory()) throw new Error(`missing ${abs}`)
  walk(abs)
}

const cargo = readFileSync(join(here, '..', 'src-tauri', 'Cargo.toml'), 'utf8')
const version = /^version\s*=\s*"([^"]+)"/m.exec(cargo)?.[1] ?? '0.0.0'
let git = null
try {
  git = execSync('git rev-parse --short=12 HEAD', { cwd: repo }).toString().trim()
  if (execSync('git status --porcelain -- DCSServerBot', { cwd: repo }).toString().trim()) git += '-dirty'
} catch { /* not a git checkout */ }

const manifest = { version: git ? `${version}+${git}` : version, git, files }
writeFileSync(join(out, 'manifest.json'), JSON.stringify(manifest, null, 2))
console.log(`staged ${Object.keys(files).length} bot file(s) from ${dirs.join(', ')} -> ${out} (bundle ${manifest.version})`)
