// Screenshots of the wiki's mockup blocks, for bfwiki/docs/blocks/.
//
//   npm --prefix bfwiki run dev -- --port 5274      (in another terminal)
//   node bfwiki/scripts/shoot-blocks.mjs [http://localhost:5274]
//
// Drives a headless Edge or Chrome over the DevTools protocol (Node 22+ has
// the WebSocket it needs, so there is nothing to install) through the dev-only
// /__blocks page: each block as it first draws, then after the clicks that show
// what it does, at desktop and phone widths and in both themes.
import { spawn } from 'node:child_process'
import { existsSync, mkdirSync, writeFileSync, rmSync } from 'node:fs'
import { dirname, join } from 'node:path'
import { fileURLToPath } from 'node:url'

const BASE = process.argv[2] ?? 'http://localhost:5274'
const HERE = dirname(fileURLToPath(import.meta.url))
const OUT = join(HERE, '..', 'docs', 'blocks')
const PROFILE = join(HERE, '..', 'node_modules', '.tmp', 'shoot-profile')
const PORT = 9333

const BROWSERS = [
  process.env.BROWSER,
  'C:/Program Files (x86)/Microsoft/Edge/Application/msedge.exe',
  'C:/Program Files/Google/Chrome/Application/chrome.exe',
  '/usr/bin/google-chrome',
  '/usr/bin/chromium',
  '/Applications/Google Chrome.app/Contents/MacOS/Google Chrome',
].filter(Boolean)

const exe = BROWSERS.find(p => existsSync(p))
if (!exe) throw new Error('no Edge or Chrome found; set BROWSER to one')

mkdirSync(OUT, { recursive: true })
rmSync(PROFILE, { recursive: true, force: true })
const browser = spawn(exe, [
  '--headless=new', `--remote-debugging-port=${PORT}`, `--user-data-dir=${PROFILE}`,
  '--hide-scrollbars', '--no-first-run', '--no-default-browser-check', 'about:blank',
], { stdio: 'ignore' })

const sleep = ms => new Promise(r => setTimeout(r, ms))

async function pageSocket() {
  for (let i = 0; i < 50; i++) {
    try {
      const list = await (await fetch(`http://127.0.0.1:${PORT}/json/list`)).json()
      const page = list.find(t => t.type === 'page')
      if (page) return page.webSocketDebuggerUrl
    } catch { /* not up yet */ }
    await sleep(200)
  }
  throw new Error('the browser did not start')
}

const ws = new WebSocket(await pageSocket())
await new Promise(r => ws.addEventListener('open', r, { once: true }))
let seq = 0
const waiting = new Map()
ws.addEventListener('message', ev => {
  const msg = JSON.parse(ev.data)
  if (msg.id && waiting.has(msg.id)) {
    const { resolve, reject } = waiting.get(msg.id)
    waiting.delete(msg.id)
    if (msg.error) reject(new Error(msg.error.message))
    else resolve(msg.result)
  }
})
const send = (method, params = {}) =>
  new Promise((resolve, reject) => {
    const id = ++seq
    waiting.set(id, { resolve, reject })
    ws.send(JSON.stringify({ id, method, params }))
  })

async function evaluate(expr) {
  const r = await send('Runtime.evaluate', { expression: `(async () => { ${expr} })()`, awaitPromise: true, returnByValue: true })
  if (r.exceptionDetails) throw new Error(r.exceptionDetails.exception?.description ?? r.exceptionDetails.text)
  return r.result.value
}

async function open(width, theme) {
  await send('Emulation.setDeviceMetricsOverride', { width, height: 900, deviceScaleFactor: width < 600 ? 2 : 1, mobile: width < 600 })
  await send('Page.navigate', { url: `${BASE}/__blocks?theme=${theme}` })
  await sleep(600)
  await evaluate(`
    localStorage.setItem('bfwiki-theme', '${theme}');
    document.documentElement.dataset.theme = '${theme}';
    for (let i = 0; i < 100 && !document.querySelector('.mk-block'); i++) await new Promise(r => setTimeout(r, 100));
    await document.fonts.ready;
    await Promise.all([...document.images].map(i => i.complete ? 0 : new Promise(r => { i.onload = i.onerror = r })));
    await new Promise(r => setTimeout(r, 300));
  `)
}

// Blocks on the page, in fixture order.
const B = { f10menu: 0, dcsmessage: 1, cockpitpanel: 2, wheel: 3, steps: 4, amber: 5, error: 6 }
const block = n => `document.querySelectorAll('.wiki-prose > .mk-block, .wiki-prose > .mk-error')[${n}]`

async function shoot(name, selector, setup = '') {
  if (setup) await evaluate(setup)
  await sleep(250)
  const rect = await evaluate(`
    const el = ${selector};
    el.scrollIntoView({ block: 'center' });
    await new Promise(r => setTimeout(r, 80));
    const r = el.getBoundingClientRect();
    return { x: r.left + scrollX, y: r.top + scrollY, width: r.width, height: r.height };
  `)
  const pad = 8
  const clip = { x: Math.max(0, rect.x - pad), y: Math.max(0, rect.y - pad), width: rect.width + pad * 2, height: rect.height + pad * 2, scale: 1 }
  const { data } = await send('Page.captureScreenshot', { format: 'png', clip, captureBeyondViewport: true })
  writeFileSync(join(OUT, `${name}.png`), Buffer.from(data, 'base64'))
  console.log(`docs/blocks/${name}.png`)
}

// In-page helpers: click by text, point the mouse at a wheel slot.
const HELPERS = `
  window.__clickText = (root, text) => {
    const el = [...root.querySelectorAll('button, [role="button"], [role="menuitem"]')].find(b => b.textContent.trim().startsWith(text));
    if (!el) throw new Error('no button ' + text);
    el.click();
  };
  window.__point = (root, cx, cy) => {
    const svg = root.querySelector('svg.wh');
    const r = svg.getBoundingClientRect();
    const k = r.width / 760;
    const opts = { bubbles: true, pointerType: 'mouse', clientX: r.left + (380 + cx) * k, clientY: r.top + (380 + cy) * k };
    svg.dispatchEvent(new PointerEvent('pointermove', opts));
  };
  window.__polar = (r, deg) => [r * Math.cos(deg * Math.PI / 180), r * Math.sin(deg * Math.PI / 180)];
`

try {
  await send('Page.enable')
  await send('Runtime.enable')

  for (const [width, suffix] of [[1100, ''], [390, '-phone']]) {
    await open(width, 'dark')
    await evaluate(HELPERS)

    await shoot(`f10menu${suffix}`, block(B.f10menu))
    await shoot(`f10menu-command${suffix}`, block(B.f10menu), `
      const b = ${block(B.f10menu)};
      __clickText(b, 'F1. Cargo');
      await new Promise(r => setTimeout(r, 50));
      __clickText(b, 'F1. Unpack');
    `)
    await shoot(`dcsmessage${suffix}`, block(B.dcsmessage))
    await shoot(`cockpitpanel${suffix}`, block(B.cockpitpanel), `__clickText(${block(B.cockpitpanel)}, 'UNPACK NEARBY')`)
    await shoot(`wheel${suffix}`, block(B.wheel))
    await shoot(`wheel-arc${suffix}`, block(B.wheel), `
      const b = ${block(B.wheel)};
      __point(b, ...__polar(168, -90));      // Cargo, at the top: its arc opens
      await new Promise(r => setTimeout(r, 50));
      __point(b, ...__polar(294, -100));     // out onto the arc
      await new Promise(r => setTimeout(r, 50));
      b.querySelector('[data-seg="a2"]').dispatchEvent(new MouseEvent('click', { bubbles: true }));
      __point(b, ...__polar(294, -100));
    `)
    if (!suffix) {
      await shoot('wheel-confirm', block(B.wheel), `
        const b = ${block(B.wheel)};
        __point(b, ...__polar(294, -40));     // DESTROY, at the end of the arc
        await new Promise(r => setTimeout(r, 50));
        b.querySelector('[data-seg="a5"]').dispatchEvent(new MouseEvent('click', { bubbles: true }));
      `)
      await shoot('wheel-amber-more', block(B.amber), `__point(${block(B.amber)}, ...__polar(168, -90 + 9 * 36))`)
    }
    await shoot(`steps${suffix}`, block(B.steps), `__clickText(${block(B.steps)}, 'Next')`)
    await shoot(`error${suffix}`, block(B.error))
  }

  await open(1100, 'light')
  await evaluate(HELPERS)
  await shoot('steps-light', block(B.steps))
  await shoot('cockpitpanel-light', block(B.cockpitpanel))
  await shoot('error-light', block(B.error))
  await shoot('editor-reference', `document.querySelector('.mk-ref')`, `document.querySelector('.mk-ref').open = true`)
} finally {
  ws.close()
  browser.kill()
}
