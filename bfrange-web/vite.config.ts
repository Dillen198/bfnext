import { defineConfig, loadEnv, type Plugin } from 'vite'
import fs from 'node:fs'
import path from 'node:path'
import { fileURLToPath, URL } from 'node:url'
import react from '@vitejs/plugin-react'
import tailwindcss from '@tailwindcss/vite'

/**
 * The range site talks to bfdb on another origin (https://api.vectorstrike.org
 * by default, see src/api.ts), so the dev server needs no proxy for normal
 * work. The /api proxy below is for pointing a local build at a local bfdb
 * with a same-origin cookie: set `VITE_API_BASE=` (empty) and
 * `VITE_BFDB_URL=http://localhost:8880` in `.env.local`.
 *
 * `npm run dev:mock` (`--mode mock`, see .env.mock) serves everything from
 * the fixtures in src/mock/ -- no backend at all.
 */
/**
 * The 3D debrief draws the dashboard's real models: one copy of the files,
 * in bfweb/public/models (credits and licences in CREDITS.txt there). This
 * serves them at /models/ in dev and copies them into the build.
 */
function sharedModels(): Plugin {
  const dir = fileURLToPath(new URL('../bfweb/public/models', import.meta.url))
  const types: Record<string, string> = { '.glb': 'model/gltf-binary', '.txt': 'text/plain; charset=utf-8' }
  return {
    name: 'shared-models',
    configureServer(server) {
      server.middlewares.use('/models', (req, res, next) => {
        const name = decodeURIComponent((req.url ?? '').split('?')[0]).replace(/^\/+/, '')
        const file = path.join(dir, name)
        if (!name || name.includes('..') || !fs.existsSync(file) || !types[path.extname(file)]) return next()
        res.setHeader('Content-Type', types[path.extname(file)])
        res.setHeader('Cache-Control', 'public, max-age=3600')
        fs.createReadStream(file).pipe(res)
      })
    },
    writeBundle(opts) {
      const out = path.join(opts.dir ?? 'dist', 'models')
      fs.mkdirSync(out, { recursive: true })
      for (const f of fs.readdirSync(dir)) if (types[path.extname(f)]) fs.copyFileSync(path.join(dir, f), path.join(out, f))
    },
  }
}

export default defineConfig(({ mode }) => {
  const env = loadEnv(mode, process.cwd(), '')
  const target = env.VITE_BFDB_URL || 'http://localhost:8880'
  return {
    plugins: [react(), tailwindcss(), sharedModels()],
    // A literal true/false, so a normal build drops the mock client and its
    // fixtures entirely instead of shipping them as an unused chunk.
    define: { __RANGE_MOCK__: JSON.stringify(env.VITE_MOCK === '1') },
    resolve: {
      // One icon set for every front end in the repo -- see shared/icons/README.md.
      // It sits outside this app, so it has no node_modules to resolve React
      // from; the exact-match aliases below point it at this app's copy.
      // They must be regexes -- a plain 'react' alias is a prefix match and
      // would swallow 'react-dom/client' with it.
      alias: [
        { find: '@icons', replacement: fileURLToPath(new URL('../shared/icons', import.meta.url)) },
        // shared/web: code the four front ends share (see shared/web/README.md).
        { find: '@shared', replacement: fileURLToPath(new URL('../shared', import.meta.url)) },
        { find: /^react$/, replacement: fileURLToPath(new URL('./node_modules/react', import.meta.url)) },
        { find: /^react\/jsx-runtime$/, replacement: fileURLToPath(new URL('./node_modules/react/jsx-runtime', import.meta.url)) },
        { find: /^react\/jsx-dev-runtime$/, replacement: fileURLToPath(new URL('./node_modules/react/jsx-dev-runtime', import.meta.url)) },
        { find: /^@tanstack\/react-query$/, replacement: fileURLToPath(new URL('./node_modules/@tanstack/react-query', import.meta.url)) },
      ],
    },
    server: {
      fs: { allow: ['..'] },
      port: 5177,
      proxy: {
        '/api': {
          target,
          changeOrigin: true,
          cookieDomainRewrite: 'localhost',
        },
      },
    },
  }
})
