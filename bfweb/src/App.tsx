import type { ReactNode } from 'react'
import { BrowserRouter, Routes, Route, Navigate } from 'react-router-dom'
import { QueryClient, QueryClientProvider } from '@tanstack/react-query'
import { RoundProvider } from './context/RoundContext'
import { AuthProvider } from './context/AuthContext'
import Layout from './components/Layout'
import Dashboard from './pages/Dashboard'
import MapPage from './pages/MapPage'
import Objectives from './pages/Objectives'
import BriefingPage from './pages/BriefingPage'
import Leaderboard from './pages/Leaderboard'
import Pilots from './pages/Pilots'
import KillFeed from './pages/KillFeed'
import LoginPage from './pages/LoginPage'
import AdminPage from './pages/AdminPage'
import ConfigEditorPage from './pages/ConfigEditorPage'
import AboutPage from './pages/AboutPage'
import { PilotPage } from './pages/PilotPage'
import CockpitPage from './pages/CockpitPage'
import IntelPage from './pages/IntelPage'
import { useAuth } from './context/AuthContext'

const queryClient = new QueryClient({
  defaultOptions: { queries: { staleTime: 10_000, retry: 1 } },
})

/** Recon intel is login-gated and coalition-locked: you must be signed in and
 *  resolvable to a Blue/Red side in the active round (admins exempt). */
function RequireIntelAccess({ children }: { children: ReactNode }) {
  const { user, loading } = useAuth()
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
        <div style={{ maxWidth: 380, lineHeight: 1.6 }}>
          Recon intel is locked to your coalition. Slot in on the server this
          round so the campaign knows which side you're on, then reload.
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
        <RoundProvider>
          <BrowserRouter>
            <Routes>
              <Route path="/login" element={<LoginPage />} />
              <Route path="/cockpit" element={<CockpitPage />} />
              <Route path="/" element={<Layout />}>
                <Route index element={<Dashboard />} />
                <Route path="map" element={<MapPage />} />
                <Route path="objectives" element={<Objectives />} />
                <Route path="briefing" element={<BriefingPage />} />
                <Route path="leaderboard" element={<Leaderboard />} />
                <Route path="pilots" element={<Pilots />} />
                <Route path="kills" element={<KillFeed />} />
              <Route path="intel" element={<RequireIntelAccess><IntelPage /></RequireIntelAccess>} />
              <Route path="admin" element={<AdminPage />} />
              <Route path="admin/config" element={<ConfigEditorPage />} />
              <Route path="about" element={<AboutPage />} />
              <Route path="pilot/:ucid" element={<PilotPage />} />
              <Route path="*" element={<Navigate to="/" replace />} />
              </Route>
            </Routes>
          </BrowserRouter>
        </RoundProvider>
      </AuthProvider>
    </QueryClientProvider>
  )
}
