import { BrowserRouter, Routes, Route } from 'react-router-dom'
import { QueryClient, QueryClientProvider } from '@tanstack/react-query'
import Layout from './components/Layout'
import Dashboard from './pages/Dashboard'
import MapPage from './pages/MapPage'
import Objectives from './pages/Objectives'
import Leaderboard from './pages/Leaderboard'
import Pilots from './pages/Pilots'
import KillFeed from './pages/KillFeed'

const queryClient = new QueryClient({
  defaultOptions: { queries: { staleTime: 10_000, retry: 1 } },
})

export default function App() {
  return (
    <QueryClientProvider client={queryClient}>
      <BrowserRouter>
        <Routes>
          <Route path="/" element={<Layout />}>
            <Route index element={<Dashboard />} />
            <Route path="map" element={<MapPage />} />
            <Route path="objectives" element={<Objectives />} />
            <Route path="leaderboard" element={<Leaderboard />} />
            <Route path="pilots" element={<Pilots />} />
            <Route path="kills" element={<KillFeed />} />
          </Route>
        </Routes>
      </BrowserRouter>
    </QueryClientProvider>
  )
}
