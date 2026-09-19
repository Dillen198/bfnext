import { defineConfig, loadEnv } from 'vite'
import { fileURLToPath, URL } from 'node:url'
import react from '@vitejs/plugin-react'
import tailwindcss from '@tailwindcss/vite'

/**
 * The dev server proxies /api to a bfdb. That is normally the local one, but
 * `--mode live` (see .env.live) points it at the production API instead, which
 * is the only way to see this app with a real campaign behind it without
 * running a DCS server locally. Read-only: nothing here writes without an admin
 * session, and there is no admin session over this proxy.
 */
export default defineConfig(({ mode }) => {
  const env = loadEnv(mode, process.cwd(), '')
  const target = env.VITE_BFDB_URL || 'http://localhost:8880'
  return {
    plugins: [react(), tailwindcss()],
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
      port: 5174,
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
