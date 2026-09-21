import { lazy, Suspense, type ReactNode } from 'react'
import NewsPage from './pages/NewsPage'
import { BrowserRouter, Routes, Route, Navigate } from 'react-router-dom'
import { QueryClient, QueryClientProvider } from '@tanstack/react-query'
import { RoundProvider } from './context/RoundContext'
import { InstanceProvider } from './context/InstanceContext'
import { AuthProvider } from './context/AuthContext'
import Layout from './components/Layout'
import Dashboard from './pages/Dashboard'
import LoginPage from './pages/LoginPage'
import { PilotPage } from './pages/PilotPage'
import { useAuth } from './context/AuthContext'

// Route-level code splitting. Dashboard and LoginPage stay eager -- they are
// the two entry points, and deferring them would just add a round trip where
// it hurts most. Everything else pulls its own heavy dependencies (MapLibre,
// Leaflet, milsymbol, jsPDF, recharts) only when someone navigates there.
const Objectives       = lazy(() => import('./pages/Objectives'))
const BriefingPage     = lazy(() => import('./pages/BriefingPage'))
const Leaderboard      = lazy(() => import('./pages/Leaderboard'))
const Pilots           = lazy(() => import('./pages/Pilots'))
const KillFeed         = lazy(() => import('./pages/KillFeed'))
const AdminPage        = lazy(() => import('./pages/AdminPage'))
const ConfigEditorPage = lazy(() => import('./pages/ConfigEditorPage'))
const AboutPage        = lazy(() => import('./pages/AboutPage'))
const CockpitPage      = lazy(() => import('./pages/CockpitPage'))
const IntelPage        = lazy(() => import('./pages/IntelPage'))
const IntelTestPage    = lazy(() => import('./pages/IntelTestPage'))
const ScopePage        = lazy(() => import('./scope/ScopePage'))

/** Shown while a route chunk is in flight. Deliberately quiet -- on a fast
 *  connection the chunk arrives before this is perceptible, and a spinner
 *  that flashes for 80ms reads as jank. */
function RouteFallback() {
  return (
    <div style={{
      display: 'flex', alignItems: 'center', justifyContent: 'center',
      flex: 1, minHeight: 200, color: 'var(--text-dim)',
      fontFamily: 'var(--font-mono)', fontSize: '0.68rem', letterSpacing: '0.14em',
    }}>
      LOADING&#8230;
    </div>
  )
}

const queryClient = new QueryClient({
  defaultOptions: { queries: { staleTime: 10_000, retry: 1 } },
})

/** Login-gated and coalition-locked pages (recon intel, per-side briefing):
 *  you must be signed in and resolvable to a Blue/Red side this campaign
 *  (admins exempt). */
function RequireCoalition({ what, children }: { what: string; children: ReactNode }) {
  const { user, loading } = useAuth()
  // BriefingPage's ?mock flag (dev builds only) renders the page from
  // fixtures and never calls the API, so gating it on a real coalition just
  // made mock mode unreachable. import.meta.env.DEV is false in any
  // production bundle, so this cannot loosen the live gate.
  if (import.meta.env.DEV && new URLSearchParams(location.search).has('mock')) {
    return <>{children}</>
  }
  if (loading) return null
  if (!user) return <Navigate to="/login" replace />
  if (!user.side && !user.is_admin) {
    return (
      <div style={{
        display: 'flex', flexDirection: 'column', gap: 8, alignItems: 'center',
        justifyContent: 'center', height: '100%', padding: 24, textAlign: 'center',
        color: 'var(--text-muted)', fontSize: '0.85rem',
      }}>
        <div style={{ fontFamily: "'Bebas Neue',sans-serif", fontSize: '1.4rem', letterSpacing: '0.12em', color: 'var(--text)' }}>
          NO COALITION
        </div>
        <div style={{ maxWidth: 400, lineHeight: 1.6 }}>
          The {what} is locked to your coalition, and the dashboard can't tell
          which side you're on. Make sure your Discord is linked
          (<code>-linkme</code> in DCS chat), and that you've taken a slot on the
          server this campaign, then reload.
        </div>
      </div>
    )
  }
  return <>{children}</>
}

export default function App() {
  return (
    <QueryClientProvider client={queryClient}>
      <AuthProvider>
        <InstanceProvider>
          <BrowserRouter>
            <RoundProvider>
            <Suspense fallback={<RouteFallback />}>
            <Routes>
              <Route path="/login" element={<LoginPage />} />
              <Route path="/cockpit" element={<CockpitPage />} />
              <Route path="/inteltest" element={<IntelTestPage />} />
              <Route path="/" element={<Layout />}>
                <Route index element={<Dashboard />} />
                <Route path="map" element={<ScopePage />} />
                <Route path="scope" element={<Navigate to="/map" replace />} />
                <Route path="news" element={<NewsPage />} />
                <Route path="objectives" element={<Objectives />} />
                <Route path="briefing" element={<RequireCoalition what="briefing"><BriefingPage /></RequireCoalition>} />
                <Route path="leaderboard" element={<Leaderboard />} />
                <Route path="pilots" element={<Pilots />} />
                <Route path="kills" element={<KillFeed />} />
              <Route path="intel" element={<RequireCoalition what="recon intel"><IntelPage /></RequireCoalition>} />
              <Route path="admin" element={<AdminPage />} />
              <Route path="admin/config" element={<ConfigEditorPage />} />
              <Route path="about" element={<AboutPage />} />
              <Route path="pilot/:ucid" element={<PilotPage />} />
              <Route path="*" element={<Navigate to="/" replace />} />
              </Route>
            </Routes>
            </Suspense>
            </RoundProvider>
          </BrowserRouter>
        </InstanceProvider>
      </AuthProvider>
    </QueryClientProvider>
  )
}
