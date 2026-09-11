import { defineConfig, loadEnv } from 'vite'
import react from '@vitejs/plugin-react'
import tailwindcss from '@tailwindcss/vite'

/**
 * The dev server proxies /api and /ws to a bfdb. That is normally the local one,
 * but `--mode live` (see .env.live) points it at the production API instead,
 * which is the only way to exercise pages that need a populated campaign --
 * the kill feed, the objective map, the briefing -- without running a DCS
 * server locally. Read-only: nothing in the dashboard writes without an admin
 * session, and there is no admin session over this proxy.
 */
export default defineConfig(({ mode }) => {
  const env = loadEnv(mode, process.cwd(), '')
  const target = env.VITE_BFDB_URL || 'http://localhost:8880'
  return {
    plugins: [react(), tailwindcss()],
    server: {
      proxy: {
        '/api': {
          target,
          changeOrigin: true,
          cookieDomainRewrite: 'localhost',
        },
        '/ws': {
          target: target.replace(/^http/, 'ws'),
          ws: true,
          changeOrigin: true,
        },
      },
    },
  }
})
