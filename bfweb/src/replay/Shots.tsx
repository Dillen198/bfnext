// The engagement table: every shot in the recording (or by / at the followed
// aircraft), who it was aimed at, and what happened -- the core of a BVR
// debrief. Launch geometry comes from both aircraft's tracks at the moment of
// launch, fetched only for the rows on screen.

import { useMemo, useState } from 'react'
import { type RecMeta, type Shot, type TrackStore, shotsOf, objLabel, sampleSeg, M_TO_FT, MS_TO_KT, fmtClock, fmtDuration } from './data'
import { braa, aspectWord, M_TO_NM, tasOf } from './analysis'

type Scope = 'by' | 'at' | 'all'

export default function Shots({ meta, store, focus, version, onPick }: {
  meta: RecMeta
  store: TrackStore
  focus: number | null
  /** bumps when tracks arrive, to fill in launch geometry */
  version: number
  onPick: (shot: Shot) => void
}) {
  const [scopePick, setScope] = useState<Scope | null>(null)
  const scope: Scope = scopePick ?? (focus != null ? 'by' : 'all')
  const all = useMemo(() => shotsOf(meta), [meta])
  const list = all.filter(s =>
    scope === 'all' || focus == null ? true : scope === 'by' ? s.ev.o === focus : s.ev.tg === focus,
  )
  const name = (i?: number | null) => (i != null && meta.objects[i] ? objLabel(meta.objects[i]) : '—')
  const wname = (i?: number | null) => (i != null && meta.objects[i] ? (meta.objects[i].n ?? '').replace(/_/g, ' ') : '?')
  const counts = { kill: 0, hit: 0, miss: 0, unknown: 0 }
  for (const s of list) counts[s.result]++

  return (
    <>
      <div className="rp-search">
        {focus != null && (
          <>
            <button className={`rp-chip ${scope === 'by' ? 'on' : ''}`} onClick={() => setScope('by')}>FIRED BY</button>
            <button className={`rp-chip ${scope === 'at' ? 'on' : ''}`} onClick={() => setScope('at')}>FIRED AT</button>
          </>
        )}
        <button className={`rp-chip ${scope === 'all' ? 'on' : ''}`} onClick={() => setScope('all')}>ALL</button>
      </div>
      <div className="rp-shot-sum">
        {list.length} shots · <b className="k">{counts.kill} kills</b> · <b className="h">{counts.hit} hits</b> · {counts.miss} missed
      </div>
      <div className="rp-list">
        {list.length === 0 && <div className="rp-empty">No shots</div>}
        {list.slice(0, 400).map((s, i) => (
          <ShotRow
            key={i}
            shot={s}
            store={store}
            version={version}
            withGeometry={scope !== 'all' || list.length <= 40}
            shooter={name(s.ev.o)}
            target={name(s.ev.tg)}
            weapon={wname(s.ev.w)}
            clock={fmtClock(meta.start_ms + s.ev.t)}
            onPick={() => onPick(s)}
          />
        ))}
      </div>
    </>
  )
}

function ShotRow({ shot, store, version, withGeometry, shooter, target, weapon, clock, onPick }: {
  shot: Shot; store: TrackStore; version: number; withGeometry: boolean
  shooter: string; target: string; weapon: string; clock: string; onPick: () => void
}) {
  const { ev, result } = shot
  const geo = useMemo(() => {
    if (!withGeometry || ev.o == null || ev.tg == null) return null
    const a = store.wholeTrack(ev.o), b = store.wholeTrack(ev.tg)
    if (!a || !b) return null
    const sa = sampleSeg(a, ev.t), sb = sampleSeg(b, ev.t)
    if (!sa || !sb) return null
    return { r: braa(sa, sb), sa, sb }
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [withGeometry, ev, store, version])
  const tof = ev.te != null ? fmtDuration(ev.te - ev.t) : null
  return (
    <button className={`rp-shot rp-shot-${result}`} onClick={onPick}>
      <div className="rp-shot-head">
        <span className="rp-ev-t">{clock}</span>
        <span className="rp-shot-w">{weapon}</span>
        <span className={`rp-shot-res ${result}`}>
          {result === 'kill' ? 'KILL' : result === 'hit' ? 'HIT' : result === 'miss' ? `MISS ${fmtMiss(ev.md)}` : '—'}
        </span>
      </div>
      <div className="rp-shot-who">{shooter} → {target}</div>
      {geo && (
        <div className="rp-shot-geo">
          <span>{(geo.r.range * M_TO_NM).toFixed(1)} nm</span>
          <span>{aspectWord(geo.r.aspect)} {Math.round(geo.r.aspect)}°</span>
          <span>{Math.round(geo.sa.alt * M_TO_FT / 1000)}k → {Math.round(geo.sb.alt * M_TO_FT / 1000)}k ft</span>
          <span>{Math.round(tasOf(geo.sa) * MS_TO_KT)} kt</span>
          {tof && <span>TOF {tof}</span>}
        </div>
      )}
    </button>
  )
}

function fmtMiss(md?: number | null): string {
  if (md == null) return ''
  return md >= 1852 ? `${(md / 1852).toFixed(1)} nm` : `${Math.round(md)} m`
}
