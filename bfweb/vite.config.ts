import { defineConfig, loadEnv } from 'vite'
import { fileURLToPath, URL } from 'node:url'
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
    // This app is not only viewed in a current browser: the in-DCS cockpit
    // overlay renders it in DCS World's embedded CEF, which is Chromium 106
    // (checked against bin/libcef.dll). Vite's default baseline is newer than
    // that, so without this the bundle can ship syntax the sim's browser
    // cannot parse -- and a parse error there is an unexplained blank panel,
    // with no console to read.
    //
    // Note this covers JavaScript only. Tailwind v4's generated CSS uses
    // oklch() and color-mix(), which need Chromium 111 and degrade in the
    // overlay; CockpitPage.tsx therefore styles itself from the plain
    // hex/rgba tokens in index.css rather than Tailwind utilities.
    build: { target: 'chrome106' },
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
    server: { fs: { allow: ['..'] },
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
