import { Navigate, useParams } from 'react-router-dom'

// The pilot detail view lives on Pilots.tsx (search page, selectable via
// ?ucid=), so /pilot/:ucid is kept only as a redirect for old bookmarks/links
// rather than as a second, independently-styled implementation of the same
// feature.
export const PilotPage: React.FC = () => {
  const { ucid } = useParams<{ ucid: string }>()
  return <Navigate to={ucid ? `/pilots?ucid=${ucid}` : '/pilots'} replace />
}
