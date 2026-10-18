import { useEffect } from 'react'
import { Route, Routes, useLocation } from 'react-router-dom'
import Nav from './components/Nav'
import Footer from './components/Footer'
import Execution from './components/Execution'
import Community from './components/Community'
import JoinBand from './components/JoinBand'
import HomePage from './pages/HomePage'
import ServersPage from './pages/ServersPage'
import StatusPage from './pages/StatusPage'
import NotFoundPage from './pages/NotFoundPage'

// Routes manage their own scroll position (see PageChrome); stop the browser
// restoring a stale one over it on reload.
if ('scrollRestoration' in history) history.scrollRestoration = 'manual'

const TITLES: Record<string, string> = {
  '/': 'Vector Strike — Persistent PvP Campaign for DCS World',
  '/servers': 'Servers — Vector Strike',
  '/join': 'How to join — Vector Strike',
  '/status': 'Server status — Vector Strike',
  '/community': 'Community — Vector Strike',
}

/** A new page starts at its top, and says which page it is in the tab. */
function PageChrome() {
  const { pathname } = useLocation()
  useEffect(() => {
    window.scrollTo(0, 0)
    document.title = TITLES[pathname] ?? 'Vector Strike'
  }, [pathname])
  return null
}

export default function App() {
  return (
    <>
      <PageChrome />
      <Nav />
      <main>
        <Routes>
          <Route path="/" element={<HomePage />} />
          <Route path="/servers" element={<ServersPage />} />
          <Route path="/join" element={<div className="page-first-wrap"><Execution /></div>} />
          <Route path="/status" element={<StatusPage />} />
          <Route path="/community" element={<div className="page-first-wrap"><Community /><JoinBand /></div>} />
          <Route path="*" element={<NotFoundPage />} />
        </Routes>
      </main>
      <Footer />
    </>
  )
}
