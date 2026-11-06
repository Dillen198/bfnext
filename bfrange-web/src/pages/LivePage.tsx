/**
 * `/` — range control: the range as it is right now. The map fills the
 * frame; the dock beside it carries the situation and one tab per job
 * (results, traffic, the boat, threats, stations). A thing picked on either
 * side is picked on both, and `?focus=<key>` (the search palette) opens with
 * the camera on it. Live state polls every 2 s while the tab is visible.
 */
import { useCallback, useEffect, useState } from 'react'
import { useSearchParams } from 'react-router-dom'
import { ErrorState, Loading } from '../components/States'
import { locate, tabFor, type DockTab } from '../lib/select'
import { useLive } from '../lib/useLive'
import { usePref } from '../lib/usePref'
import { LiveDock } from './live/LiveDock'
import { LiveMap, type Focus } from './live/LiveMap'

const TABS: DockTab[] = ['feed', 'traffic', 'boat', 'threats', 'stations']
const isTab = (v: unknown): v is DockTab => TABS.includes(v as DockTab)

export default function LivePage() {
  const q = useLive(2000)
  const live = q.data?.live ?? null
  const [tab, setTab] = usePref<DockTab>('range.live.tab', 'feed', isTab)
  const [selected, setSelected] = useState<string | null>(null)
  const [focus, setFocus] = useState<Focus | null>(null)
  const [params, setParams] = useSearchParams()

  /** From the dock: select, open nothing new, fly the camera there. */
  const pickFromDock = useCallback((key: string) => {
    setSelected(key)
    setFocus(f => ({ key, n: (f?.n ?? 0) + 1 }))
  }, [])
  /** From the map: select and open the dock tab that lists it. */
  const pickFromMap = useCallback((key: string | null) => {
    setSelected(key)
    if (key) setTab(tabFor(key))
  }, [setTab])

  // ?focus= from the search palette: take it once the picture is in, then
  // clear it from the address so a reload does not fly there again
  const want = params.get('focus')
  const [taken, setTaken] = useState<string | null>(null)
  if (want !== taken && (!want || live)) {
    setTaken(want)
    if (want && live && locate(live, want)) {
      setSelected(want)
      setTab(tabFor(want))
      setFocus(f => ({ key: want, n: (f?.n ?? 0) + 1 }))
    }
  }
  useEffect(() => {
    if (want && live) setParams(p => { p.delete('focus'); return p }, { replace: true })
  }, [want, live, setParams])

  // a selection whose thing has gone (despawned, logged off) shows as none
  const current = selected && live && locate(live, selected) ? selected : null

  return (
    <div className="control">
      <div className="control-map">
        {q.isLoading ? (
          <div className="h-full grid place-items-center"><Loading label="Contacting the range" /></div>
        ) : q.error ? (
          <div className="p-4"><ErrorState error={q.error} retry={() => q.refetch()} /></div>
        ) : !live ? (
          <div className="h-full grid place-items-center p-6">
            <div className="panel panel-b max-w-md text-center">
              <span className="chip bad">RANGE OFFLINE</span>
              <p className="mt-3 mb-1">{q.data?.reason ?? 'The range server is not publishing a live picture right now.'}</p>
              <p className="muted text-[12.5px] m-0">Results already flown are in the feed, your record and the boards.</p>
            </div>
          </div>
        ) : (
          <LiveMap live={live} height="100%" selected={current} onSelect={pickFromMap} focus={focus} />
        )}
      </div>
      <LiveDock live={live} tab={live ? tab : 'feed'} setTab={setTab} selected={current} onSelect={pickFromDock} />
    </div>
  )
}
