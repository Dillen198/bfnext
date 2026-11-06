import { StrictMode } from 'react'
import { createRoot } from 'react-dom/client'
import './index.css'
import App from './App.tsx'
import { ErrorBoundary } from '@shared/web/ErrorBoundary'
import { installChunkReload } from '@shared/web/chunkReload'

// A tab left open across a deploy asks for route chunks that are gone.
installChunkReload()

createRoot(document.getElementById('root')!).render(
  <StrictMode>
    <ErrorBoundary>
    <App />
    </ErrorBoundary>
  </StrictMode>,
)
