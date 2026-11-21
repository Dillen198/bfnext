import { useEffect, useState } from 'react'
import { useQuery } from '@tanstack/react-query'
import { api, type Frontlines } from '../api'
import { campaign } from '../config/campaign'
import TacMap from '../components/TacMap'
import { fmtTimeZ } from '../lib/format'
import { useInstance } from '../context/InstanceContext'

/** The Discord status snapshot: the SITREP map full-frame, no dashboard
 *  chrome. The bot opens `/snapshot?server=<name>&theme=dark` in a headless
 *  browser at a fixed viewport and attaches the screenshot to the Campaign
 *  Status embed.
 *
 *  Public data only -- the same objectives/frontline routes the SITREP map
 *  uses, never the fog-of-war tacmap feed -- because the picture lands in a
 *  channel both sides can read.
 *
 *  `data-snapshot-ready` goes on <html> once the data is in and the basemap
 *  has loaded; the bot waits for it before it captures. */
export default function SnapshotPage() {
  const objectivesQ = useQuery({ queryKey: ['objectives', 'snapshot'], queryFn: () => api.objectives(), retry: 2 })
  const frontsQ = useQuery<Frontlines>({ queryKey: ['frontline', 'snapshot'], queryFn: () => api.frontline(), retry: 2 })
  const { data: stats } = useQuery({ queryKey: ['stats'], queryFn: api.stats, retry: 2 })
  // The bot names the server with ?server=<DCS server name>, which bfdb
  // resolves for the data routes but the instance context doesn't select, so
  // match it here for the label ("Modern - Syria" beats a scenario id).
  const { instances, current } = useInstance()
  const serverParam = new URLSearchParams(window.location.search).get('server')
  const inst = (serverParam && instances.find(i => i.dcs_server_name === serverParam)) || current
  const campaignLabel = inst?.label || stats?.active_round?.scenario

  const objectives = objectivesQ.data ?? []
  const fronts = frontsQ.data ?? { mid: [], blue: [], red: [] }
  const settled = !objectivesQ.isLoading && !frontsQ.isLoading

  // Ready = data in and every visible basemap tile loaded (or no map to draw).
  // The tile load can stall on a slow basemap; the capture tool has its own
  // timeout and shoots whatever is there.
  const [tilesLoaded, setTilesLoaded] = useState(false)
  const ready = settled && (tilesLoaded || objectives.length === 0)
  useEffect(() => {
    if (ready) document.documentElement.setAttribute('data-snapshot-ready', '1')
  }, [ready])

  const chip: React.CSSProperties = {
    background: 'rgba(8,11,6,0.90)', border: '1px solid var(--border)',
    padding: '10px 18px', fontFamily: 'var(--font-mono)', fontSize: '1.6rem',
    letterSpacing: '0.1em', color: 'var(--text-dim)', textTransform: 'uppercase',
  }

  return (
    <div style={{ position: 'fixed', inset: 0, background: '#050806' }}>
      {/* Mounted once the data is in, so the map is created at its final
          bounds (see TacMap). */}
      {settled && <TacMap objectives={objectives} fronts={fronts} snapshot onTilesLoaded={() => setTilesLoaded(true)} />}
      <div style={{ position: 'absolute', top: 8, right: 8, zIndex: 1000, ...chip }}>
        <span style={{ color: 'var(--text)', fontWeight: 700 }}>{campaign.shortName || campaign.name}</span>
        {campaignLabel && <> · {campaignLabel}</>}
        {' · '}{fmtTimeZ(new Date().toISOString())}
      </div>
      {settled && objectives.length === 0 && (
        <div style={{
          position: 'absolute', top: '50%', left: '50%', transform: 'translate(-50%, -50%)',
          zIndex: 1000, ...chip, fontSize: '2rem',
        }}>
          No active campaign
        </div>
      )}
    </div>
  )
}
