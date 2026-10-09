/**
 * `/me` — your own record. Sends you to /pilot/<your ucid>, or explains what
 * is missing: a login, or the Discord-to-DCS link the bot makes.
 */
import { Navigate } from 'react-router-dom'
import { useAuth } from '../context/AuthContext'
import { Loading } from '../components/States'
import { LinkGuidance, LoginPrompt } from '../components/AuthGate'

export default function MePage() {
  const { me, loading } = useAuth()
  if (loading) return <Loading />
  if (!me?.logged_in) {
    return (
      <div className="wrap page">
        <LoginPrompt why="to see your own range record" />
      </div>
    )
  }
  if (!me.ucid) {
    return (
      <div className="wrap page">
        <LinkGuidance />
      </div>
    )
  }
  return <Navigate to={`/pilot/${encodeURIComponent(me.ucid)}`} replace />
}
