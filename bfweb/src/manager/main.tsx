import { StrictMode } from 'react'
import { createRoot } from 'react-dom/client'
import { QueryClient, QueryClientProvider } from '@tanstack/react-query'
import { MemoryRouter } from 'react-router-dom'
import '../index.css'
import { setTransport } from '../api'
import { AuthProvider } from '../context/AuthContext'
import { managerTransport } from './tauri'
import ManagerApp from './ManagerApp'
import { ErrorBoundary } from '@shared/web/ErrorBoundary'
import { installChunkReload } from '@shared/web/chunkReload'

// Every page borrowed from the dashboard talks to the local bot, not bfdb.
setTransport(managerTransport)

const queryClient = new QueryClient({
  defaultOptions: { queries: { refetchOnWindowFocus: false } },
})

// A tab left open across a deploy asks for route chunks that are gone.
installChunkReload()

createRoot(document.getElementById('root')!).render(
  <StrictMode>
    <ErrorBoundary>
    <QueryClientProvider client={queryClient}>
      <MemoryRouter initialEntries={['/admin/ops']}>
        <AuthProvider>
          <ManagerApp />
        </AuthProvider>
      </MemoryRouter>
    </QueryClientProvider>
    </ErrorBoundary>
  </StrictMode>,
)
