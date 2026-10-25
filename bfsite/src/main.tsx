import { StrictMode } from 'react'
import { createRoot } from 'react-dom/client'
import { QueryClient, QueryClientProvider } from '@tanstack/react-query'
import { BrowserRouter } from 'react-router-dom'
import './index.css'
import App from './App'
import { ErrorBoundary } from '@shared/web/ErrorBoundary'
import { installChunkReload } from '@shared/web/chunkReload'
import { ThemeProvider, type ThemeOptions } from '@shared/web/theme'

// Only an explicit choice is remembered; until then the site follows the OS.
// index.html's pre-paint script reads the same key; keep the two in step.
const THEME: ThemeOptions = { storageKey: 'bfsite-theme', rememberChoiceOnly: true }

const queryClient = new QueryClient({
  defaultOptions: {
    queries: {
      staleTime: 10_000,
      retry: 1,
    },
  },
})

// A tab left open across a deploy asks for route chunks that are gone.
installChunkReload()

createRoot(document.getElementById('root')!).render(
  <StrictMode>
    <ErrorBoundary>
    <QueryClientProvider client={queryClient}>
      <ThemeProvider options={THEME}>
        {/* BASE_URL keeps routes working when bfdb serves the build under /site/. */}
        <BrowserRouter basename={import.meta.env.BASE_URL.replace(/\/$/, '')}>
          <App />
        </BrowserRouter>
      </ThemeProvider>
    </QueryClientProvider>
    </ErrorBoundary>
  </StrictMode>,
)
