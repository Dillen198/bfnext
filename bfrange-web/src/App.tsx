import { lazy, Suspense, useEffect } from 'react'
import { Link, Route, Routes, useLocation, useNavigate } from 'react-router-dom'
import { Shell } from './components/Shell'
import { Loading } from './components/States'
import LivePage from './pages/LivePage'
import { AFTER_LOGIN_KEY, useAuth } from './context/AuthContext'

// the heavier pages load on demand
const GreeniePage = lazy(() => import('./pages/GreeniePage'))
const ResultsPage = lazy(() => import('./pages/ResultsPage'))
const StationPage = lazy(() => import('./pages/StationPage'))
const PilotPage = lazy(() => import('./pages/PilotPage'))
const MePage = lazy(() => import('./pages/MePage'))
const ResultPage = lazy(() => import('./pages/ResultPage'))
const SpawnPage = lazy(() => import('./pages/SpawnPage'))
const BriefPage = lazy(() => import('./pages/BriefPage'))
const CalcPage = lazy(() => import('./pages/CalcPage'))
const LeaderboardsPage = lazy(() => import('./pages/LeaderboardsPage'))

function NotFound() {
  return (
    <div className="wrap page">
      <div className="page-head">
        <h1 className="display">Off the range</h1>
      </div>
      <p className="muted">
        There is nothing at this address. <Link to="/" className="underline">Back to range control</Link>.
      </p>
    </div>
  )
}

/** After the Discord round trip (which always lands on /), go back to where the login started. */
function ReturnAfterLogin() {
  const { me } = useAuth()
  const nav = useNavigate()
  useEffect(() => {
    if (!me?.logged_in) return
    try {
      const to = sessionStorage.getItem(AFTER_LOGIN_KEY)
      sessionStorage.removeItem(AFTER_LOGIN_KEY)
      if (to && to !== '/' && to.startsWith('/')) nav(to, { replace: true })
    } catch { /* storage blocked */ }
  }, [me?.logged_in, nav])
  return null
}

export default function App() {
  const loc = useLocation()
  return (
    <>
      <ReturnAfterLogin />
      {/* range control is the one full-bleed page: the map fills the frame */}
      <Shell bleed={loc.pathname === '/'}>
        <Suspense fallback={<Loading />}>
          <Routes>
            <Route path="/" element={<LivePage />} />
            <Route path="/greenie" element={<GreeniePage />} />
            <Route path="/results" element={<ResultsPage />} />
            <Route path="/stations/:id" element={<StationPage />} />
            <Route path="/pilot/:ucid" element={<PilotPage />} />
            <Route path="/me" element={<MePage />} />
            <Route path="/result/:id" element={<ResultPage />} />
            <Route path="/spawn" element={<SpawnPage />} />
            <Route path="/brief" element={<BriefPage />} />
            <Route path="/calc" element={<CalcPage />} />
            <Route path="/leaderboards" element={<LeaderboardsPage />} />
            <Route path="*" element={<NotFound />} />
          </Routes>
        </Suspense>
      </Shell>
    </>
  )
}
