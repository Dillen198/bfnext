// Altitude and speed of the followed aircraft over the timeline's span, with
// the playhead. Click to seek. Drawn from the stitched whole track, so it is
// complete as soon as that one request lands.

import { useMemo, useRef } from 'react'
import { type TrackStore, distM, M_TO_FT, MS_TO_KT } from './data'

const W = 1000
const H = 64

export default function FlightGraph({ store, idx, range, t, onSeek, version }: {
  store: TrackStore
  idx: number
  range: [number, number]
  t: number
  onSeek: (t: number) => void
  /** bumps when the track arrives */
  version: number
}) {
  const el = useRef<SVGSVGElement>(null)
  const [a, b] = range
  const span = Math.max(1, b - a)

  const g = useMemo(() => {
    const s = store.wholeTrack(idx)
    if (!s || s.t.length < 2) return null
    let maxAlt = 1, maxSpd = 1
    const alt: string[] = [], spd: string[] = []
    const n = s.t.length
    // ~1 point per pixel column is plenty
    const stride = Math.max(1, Math.floor(n / 1200))
    for (let i = 0; i < n; i += stride) maxAlt = Math.max(maxAlt, s.alt[i] * M_TO_FT)
    const speeds: number[] = []
    for (let i = 0; i < n; i += stride) {
      let v: number
      if (s.ias) v = s.ias[i] * MS_TO_KT
      else {
        const j = Math.max(0, i - stride)
        const dt = (s.t[i] - s.t[j]) / 1000
        v = dt > 0 ? (distM(s.lon[j], s.lat[j], s.lon[i], s.lat[i]) / dt) * MS_TO_KT : 0
      }
      speeds.push(v)
      maxSpd = Math.max(maxSpd, v)
    }
    let k = 0
    for (let i = 0; i < n; i += stride, k++) {
      const x = ((s.t[i] - a) / span) * W
      if (x < -5 || x > W + 5) continue
      alt.push(`${x.toFixed(1)},${(H - 4 - (s.alt[i] * M_TO_FT / maxAlt) * (H - 10)).toFixed(1)}`)
      spd.push(`${x.toFixed(1)},${(H - 4 - (speeds[k] / maxSpd) * (H - 10)).toFixed(1)}`)
    }
    return { alt: alt.join(' '), spd: spd.join(' '), maxAlt, maxSpd, ias: !!s.ias }
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [store, idx, a, span, version])

  const seek = (clientX: number) => {
    const r = el.current?.getBoundingClientRect()
    if (!r) return
    onSeek(a + ((clientX - r.left) / r.width) * span)
  }

  const px = ((t - a) / span) * W
  return (
    <div className="rp-graph">
      <svg
        ref={el} viewBox={`0 0 ${W} ${H}`} preserveAspectRatio="none"
        onClick={ev => seek(ev.clientX)}
        role="img" aria-label="Altitude and speed of this flight"
      >
        {g && <polyline points={g.alt} className="rp-graph-alt" />}
        {g && <polyline points={g.spd} className="rp-graph-spd" />}
        <line x1={px} x2={px} y1={0} y2={H} className="rp-graph-head" />
      </svg>
      <div className="rp-graph-legend">
        {g ? (
          <>
            <span className="alt">ALT max {Math.round(g.maxAlt).toLocaleString()} ft</span>
            <span className="spd">{g.ias ? 'IAS' : 'GS'} max {Math.round(g.maxSpd)} kt</span>
          </>
        ) : <span>loading flight profile…</span>}
      </div>
    </div>
  )
}
