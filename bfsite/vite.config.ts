import { defineConfig } from 'vite'
import react from '@vitejs/plugin-react'
import tailwindcss from '@tailwindcss/vite'

export default defineConfig({
  plugins: [react(), tailwindcss()],
  // When embedded in bfdb, the site is served at /site/
  // For local dev (npm run dev) this base is overridden by the dev server
  base: '/site/',
  server: {
    port: 5174,
    proxy: {
      '/api': 'http://localhost:8080',
    },
  },
})
