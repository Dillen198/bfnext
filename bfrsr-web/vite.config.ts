import { defineConfig, loadEnv, type Plugin } from 'vite'
import { fileURLToPath, URL } from 'node:url'
import { cpSync, createReadStream, existsSync, statSync } from 'node:fs'
import { extname, join, normalize } from 'node:path'
import react from '@vitejs/plugin-react'
import tailwindcss from '@tailwindcss/vite'

/**
 * The Red Storm Rising dashboard: started as a copy of bfweb and redesigned
 * for the 1985 campaign. It talks to the same bfdb API as bfweb.
 *
 * The dev server proxies /api and /ws to a bfdb: the local one, or with
 * `--mode live` (.env.live) the production API, read-only.
 */

/**
 * bfweb's map imagery and 3D models (`public/images`, `public/models`, about
 * 120 MB) are served from bfweb rather than copied into this app, so the
 * repository carries one copy. Dev serves them from there; a build copies
 * them into `dist`.
 */
const SHARED_PUBLIC = fileURLToPath(new URL('../bfweb/public', import.meta.url))
const SHARED_DIRS = ['images', 'models']
const TYPES: Record<string, string> = {
  '.png': 'image/png',
  '.jpg': 'image/jpeg',
  '.jpeg': 'image/jpeg',
  '.webp': 'image/webp',
  '.svg': 'image/svg+xml',
  '.json': 'application/json',
  '.glb': 'model/gltf-binary',
  '.gltf': 'model/gltf+json',
  '.bin': 'application/octet-stream',
}

function sharedPublic(): Plugin {
  let outDir = 'dist'
  let building = false
  return {
    name: 'bfrsr-shared-public',
    configResolved(c) {
      outDir = c.build.outDir
      // Vitest runs the bundle hooks too, with a dummy outDir: copying the
      // assets there put 120 MB on disk per test run.
      building = c.command === 'build' && !process.env.VITEST
    },
    configureServer(server) {
      server.middlewares.use((req, res, next) => {
        const path = decodeURIComponent((req.url ?? '').split('?')[0])
        const top = path.split('/')[1]
        if (!SHARED_DIRS.includes(top)) return next()
        const file = normalize(join(SHARED_PUBLIC, path))
        if (!file.startsWith(SHARED_PUBLIC) || !existsSync(file) || !statSync(file).isFile()) {
          return next()
        }
        res.setHeader('Content-Type', TYPES[extname(file).toLowerCase()] ?? 'application/octet-stream')
        createReadStream(file).pipe(res)
      })
    },
    closeBundle() {
      if (!building) return
      for (const d of SHARED_DIRS) {
        const from = join(SHARED_PUBLIC, d)
        if (existsSync(from)) cpSync(from, join(outDir, d), { recursive: true })
      }
    },
  }
}

export default defineConfig(({ mode }) => {
  const env = loadEnv(mode, process.cwd(), '')
  const target = env.VITE_BFDB_URL || 'http://localhost:8880'
  return {
    plugins: [react(), tailwindcss(), sharedPublic()],
    // A local checkout may link node_modules to bfweb's; keep the caches apart.
    cacheDir: '.vite',
    // DCS's embedded browser is Chromium 106; see bfweb/vite.config.ts.
    build: { target: 'chrome106' },
    resolve: {
      alias: [
        { find: '@icons', replacement: fileURLToPath(new URL('../shared/icons', import.meta.url)) },
        { find: '@shared', replacement: fileURLToPath(new URL('../shared', import.meta.url)) },
        { find: /^react$/, replacement: fileURLToPath(new URL('./node_modules/react', import.meta.url)) },
        { find: /^react\/jsx-runtime$/, replacement: fileURLToPath(new URL('./node_modules/react/jsx-runtime', import.meta.url)) },
        { find: /^react\/jsx-dev-runtime$/, replacement: fileURLToPath(new URL('./node_modules/react/jsx-dev-runtime', import.meta.url)) },
        { find: /^@tanstack\/react-query$/, replacement: fileURLToPath(new URL('./node_modules/@tanstack/react-query', import.meta.url)) },
      ],
    },
    server: {
      fs: { allow: ['..'] },
      proxy: {
        '/api': { target, changeOrigin: true, cookieDomainRewrite: 'localhost' },
        '/ws': { target: target.replace(/^http/, 'ws'), ws: true, changeOrigin: true },
      },
    },
  }
})
