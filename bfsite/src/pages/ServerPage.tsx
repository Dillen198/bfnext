import { lazy, Suspense, useEffect, type ComponentType, type LazyExoticComponent } from 'react'
import { useParams } from 'react-router-dom'
import { serverBySlug, type ServerProfile, type ServerTheme } from '../config/servers'
import NotFoundPage from './NotFoundPage'

// Each server page brings its own stylesheet and fonts, so it loads only
// when someone opens it.
const PAGES: Record<ServerTheme, LazyExoticComponent<ComponentType<{ profile: ServerProfile }>>> = {
  desert: lazy(() => import('../servers/SyriaPage')),
  staff: lazy(() => import('../servers/RedStormPage')),
  chart: lazy(() => import('../servers/RangePage')),
}

export default function ServerPage() {
  const { slug } = useParams()
  const profile = serverBySlug(slug)
  useEffect(() => {
    if (profile) document.title = `${profile.title} — Vector Strike`
  }, [profile])
  if (!profile) return <NotFoundPage />
  const Page = PAGES[profile.theme]
  return (
    <Suspense fallback={<div className="server-loading" aria-busy="true" />}>
      <Page profile={profile} />
    </Suspense>
  )
}
