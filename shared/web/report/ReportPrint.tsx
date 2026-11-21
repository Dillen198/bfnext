// `/report/print?instance=<id>&days=7`: the weekly report poster alone, with
// the automatic dispatch, for the FowlEngine bot's weekly schedule. It opens
// this page in a headless Edge/Chrome with no login (mapsnap.capture_page),
// waits for `data-snapshot-ready` on <html>, clips `#weekly-poster` and reads
// the Discord post text from `#weekly-post-text`. Public: everything on it
// comes from the public /api/report/weekly.

import { useEffect, useMemo } from 'react'
import { useQuery } from '@tanstack/react-query'
import { draftDispatch, posterModel, postText, type WeeklyReportResponse } from './weekly'
import { WeeklyPoster } from './WeeklyPoster'
import { usePosterFonts } from './fonts'
import { defaultLogo, siteFor } from './site'

export function ReportPrint({ fetch, fallback, fixture }: {
  fetch: (days: number) => Promise<WeeklyReportResponse>
  fallback: { blue: string; red: string; brand: string }
  /** Draw this instead of asking bfdb (the dev-only `?fixture=` preview). */
  fixture?: WeeklyReportResponse | null
}) {
  const days = Math.min(31, Math.max(1, Number(new URLSearchParams(window.location.search).get('days')) || 7))
  const q = useQuery({
    queryKey: ['weekly-report-print', days, !!fixture],
    queryFn: () => (fixture ? Promise.resolve(fixture) : fetch(days)),
    retry: 2,
  })
  const model = useMemo(() => (q.data ? posterModel(q.data, fallback) : null), [q.data, fallback])
  const dispatch = useMemo(() => (model ? draftDispatch(model) : null), [model])
  const fonts = usePosterFonts(model?.theme ?? 'levant')
  const site = model ? siteFor(model) : undefined

  useEffect(() => {
    const root = document.documentElement
    if (!model || !fonts) return
    let live = true
    // two frames: the poster is laid out and painted in its fonts
    requestAnimationFrame(() => requestAnimationFrame(() => { if (live) root.setAttribute('data-snapshot-ready', '1') }))
    return () => { live = false; root.removeAttribute('data-snapshot-ready') }
  }, [model, fonts])

  if (q.isError) return <p style={{ color: '#e0574a', padding: 24, fontFamily: 'monospace' }}>The report did not load.</p>
  if (!model || !dispatch) return null
  return (
    <div style={{ background: '#000', minHeight: '100vh', margin: 0 }}>
      <WeeklyPoster id="weekly-poster" model={model} dispatch={dispatch} site={site} logo={defaultLogo(model)} />
      <pre id="weekly-post-text" hidden>{postText(model, dispatch, site?.dashboard)}</pre>
    </div>
  )
}
