import { StrictMode } from 'react'
import { createRoot } from 'react-dom/client'
import { BrowserRouter } from 'react-router-dom'
import { QueryClient, QueryClientProvider } from '@tanstack/react-query'
import './index.css'
import App from './App'
import { ThemeProvider, type ThemeOptions } from '@shared/web/theme'
import { AuthProvider } from './context/AuthContext'
import { ToastProvider } from './context/ToastContext'

// Dark by default; the choice is remembered per browser. The range's DaisyUI
// themes are called range / range-light, and the browser chrome follows.
const THEME: ThemeOptions = {
  storageKey: 'range.theme',
  fallback: 'dark',
  apply: theme => {
    document.documentElement.dataset.theme = theme === 'light' ? 'range-light' : 'range'
    document.querySelector('meta[name="theme-color"]')?.setAttribute('content', theme === 'light' ? '#e9edf1' : '#0b1016')
  },
}

const queryClient = new QueryClient({
  defaultOptions: {
    queries: {
      staleTime: 30_000,
      refetchOnWindowFocus: false,
      retry: (n, err) => n < 2 && !(err instanceof Error && 'status' in err && (err as { status: number }).status === 404),
    },
  },
})

createRoot(document.getElementById('root')!).render(
  <StrictMode>
    <QueryClientProvider client={queryClient}>
      <ThemeProvider options={THEME}>
        <AuthProvider>
          <BrowserRouter>
            <ToastProvider>
              <App />
            </ToastProvider>
          </BrowserRouter>
        </AuthProvider>
      </ThemeProvider>
    </QueryClientProvider>
  </StrictMode>,
)
