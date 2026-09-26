import { defineConfig, loadEnv } from 'vite'
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
export default defineConfig(({ mode }) => {
  const env = loadEnv(mode, process.cwd(), '')
  const target = env.VITE_BFDB_URL || 'http://localhost:8880'
  return {
    plugins: [react(), tailwindcss()],
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
        { find: /^react$/, replacement: fileURLToPath(new URL('./node_modules/react', import.meta.url)) },
        { find: /^react\/jsx-runtime$/, replacement: fileURLToPath(new URL('./node_modules/react/jsx-runtime', import.meta.url)) },
        { find: /^react\/jsx-dev-runtime$/, replacement: fileURLToPath(new URL('./node_modules/react/jsx-dev-runtime', import.meta.url)) },
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
