import { defineConfig, type Plugin } from 'vite'
import { fileURLToPath, URL } from 'node:url'
import { renameSync, existsSync, rmSync } from 'node:fs'
import { join } from 'node:path'
import react from '@vitejs/plugin-react'
import tailwindcss from '@tailwindcss/vite'

/**
 * Fowl Engine Manager's UI (bfmanager/, a Tauri desktop app) is built from
 * THIS project so it shares React, Tailwind, the design tokens, the icon set
 * and whole pages -- the OPS page runs in it unchanged, over Tauri IPC
 * instead of HTTP (see src/manager/tauri.ts and setTransport in src/api.ts).
 *
 *   npm run dev:manager     dev server on :5190 (bfmanager's `tauri dev` starts it)
 *   npm run build:manager   -> ../bfmanager/dist (bfmanager's `tauri build` runs it)
 *
 * The entry is manager.html; Tauri wants index.html, so it is renamed on the
 * way out and served as / in dev.
 */
const outDir = fileURLToPath(new URL('../bfmanager/dist', import.meta.url))

function managerIndex(): Plugin {
  return {
    name: 'manager-index',
    configureServer(server) {
      server.middlewares.use((req, _res, next) => {
        const [path, query] = (req.url ?? '/').split('?', 2)
        if (path === '/' || path === '/index.html') req.url = '/manager.html' + (query ? `?${query}` : '')
        next()
      })
    },
    closeBundle() {
      const from = join(outDir, 'manager.html')
      const to = join(outDir, 'index.html')
      if (existsSync(from)) {
        if (existsSync(to)) rmSync(to)
        renameSync(from, to)
      }
    },
  }
}

export default defineConfig({
  plugins: [react(), tailwindcss(), managerIndex()],
  // Tauri serves the bundle from its own origin; relative asset URLs keep it portable.
  base: './',
  // only what the manager itself uses is imported (src/manager/logo.png); the
  // dashboard's public/ folder would just bloat the installer
  publicDir: false,
  clearScreen: false,
  resolve: {
    alias: [
      { find: '@icons', replacement: fileURLToPath(new URL('../shared/icons', import.meta.url)) },
      { find: /^react$/, replacement: fileURLToPath(new URL('./node_modules/react', import.meta.url)) },
      { find: /^react\/jsx-runtime$/, replacement: fileURLToPath(new URL('./node_modules/react/jsx-runtime', import.meta.url)) },
      { find: /^react\/jsx-dev-runtime$/, replacement: fileURLToPath(new URL('./node_modules/react/jsx-dev-runtime', import.meta.url)) },
    ],
  },
  server: { port: 5190, strictPort: true, fs: { allow: ['..'] } },
  build: {
    // WebView2 is current Chromium
    target: 'es2022',
    outDir,
    emptyOutDir: true,
    rollupOptions: { input: fileURLToPath(new URL('./manager.html', import.meta.url)) },
  },
})
