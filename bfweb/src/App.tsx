import { BrowserRouter, Routes, Route, Navigate } from 'react-router-dom'
import { QueryClient, QueryClientProvider } from '@tanstack/react-query'
import { RoundProvider } from './context/RoundContext'
import { AuthProvider } from './context/AuthContext'
import Layout from './components/Layout'
import Dashboard from './pages/Dashboard'
import MapPage from './pages/MapPage'
import Objectives from './pages/Objectives'
import Leaderboard from './pages/Leaderboard'
import Pilots from './pages/Pilots'
import KillFeed from './pages/KillFeed'
import LoginPage from './pages/LoginPage'
import AdminPage from './pages/AdminPage'
import AboutPage from './pages/AboutPage'

const queryClient = new QueryClient({
  defaultOptions: { queries: { staleTime: 10_000, retry: 1 } },
})

export default function App() {
  return (
    <QueryClientProvider client={queryClient}>
      <AuthProvider>
        <RoundProvider>
          <BrowserRouter>
            <Routes>
              <Route path="/login" element={<LoginPage />} />
              <Route path="/" element={<Layout />}>
                <Route index element={<Dashboard />} />
                <Route path="map" element={<MapPage />} />
                <Route path="objectives" element={<Objectives />} />
                <Route path="leaderboard" element={<Leaderboard />} />
                <Route path="pilots" element={<Pilots />} />
                <Route path="kills" element={<KillFeed />} />
              <Route path="admin" element={<AdminPage />} />
              <Route path="about" element={<AboutPage />} />
              <Route path="*" element={<Navigate to="/" replace />} />
              </Route>
            </Routes>
          </BrowserRouter>
        </RoundProvider>
      </AuthProvider>
    </QueryClientProvider>
  )
}
