import { useMemo, useState, useCallback, useEffect } from 'react'
import { api, type Objective } from '../api'
import GciScope from '../gci/GciScope'
import { useGciFeed } from '../gci/useGciFeed'
import {
  donorLabel,
  resolveAnchor,
  suggestedRangeNm,
  tracksRelativeToAnchor,
  type GciFocusMode,
} from '../gci/focus'
import type { GciDesk, GciTrack } from '../gci/types'
import { clsLabel, iffLabel, srcLabel } from '../gci/types'
import './GciMap.css'

const RANGES = [50, 100, 150, 200] as const

function TrackRow({
  t,
  selected,
  onSelect,
}: {
  t: GciTrack
  selected: boolean
  onSelect: () => void
}) {
  const col =
    iffLabel(t.iff) === 'friendly'
      ? 'var(--gci-friendly)'
      : iffLabel(t.iff) === 'hostile'
        ? 'var(--gci-hostile)'
        : 'var(--gci-unknown)'
  return (
    <tr
      className={`gci-tr${selected ? ' selected' : ''}${t.stale ? ' stale' : ''}`}
      onClick={onSelect}
    >
      <td style={{ color: col }}>{t.tn}</td>
      <td>{t.brg.toString().padStart(3, '0')}</td>
      <td>{t.rng_nm}</td>
      <td>{Math.round(t.alt_ft / 100)}</td>
      <td>{t.spd_kts}</td>
      <td>{t.age}</td>
      <td>
        {srcLabel(t.src)}
        {(t.contested || (t.jam ?? 0) > 25) && <span className="gci-j-flag">J</span>}
      </td>
    </tr>
  )
}

export default function GciMapPage() {
  const [desk, setDesk] = useState<GciDesk>('blue')
  const [rangeNm, setRangeNm] = useState<number>(100)
  const [showDonors, setShowDonors] = useState(true)
  const [showTerrain, setShowTerrain] = useState(true)
  const [showJamZones, setShowJamZones] = useState(true)
  const [showTags, setShowTags] = useState(true)
  const [showFogOfWar, setShowFogOfWar] = useState(false)
  const [minAlt, setMinAlt] = useState<number>(0)
  const [maxAlt, setMaxAlt] = useState<number>(99999)
  const [selectedId, setSelectedId] = useState<number | null>(null)
  const [focusMode, setFocusMode] = useState<GciFocusMode>('bullseye')
  const [focusDonorIndex, setFocusDonorIndex] = useState(0)
  const [objectives, setObjectives] = useState<Objective[]>([])

  useEffect(() => {
    api.objectives().then(setObjectives).catch(console.error)
  }, [])

  const { msg, status } = useGciFeed(desk)

  const rawSelected = useMemo(
    () => (msg?.tracks ?? []).find((t) => t.id === selectedId) ?? null,
    [msg, selectedId],
  )

  const anchor = useMemo(
    () => resolveAnchor(msg, focusMode, focusDonorIndex, rawSelected),
    [msg, focusMode, focusDonorIndex, rawSelected],
  )

  const displayTracks = useMemo(() => {
    if (!msg || !anchor) return []
    const sorted = [...msg.tracks].sort((a, b) => a.rng_nm - b.rng_nm)
    return tracksRelativeToAnchor(sorted, anchor)
  }, [msg, anchor])

  const selected = useMemo(
    () => displayTracks.find((t) => t.id === selectedId) ?? null,
    [displayTracks, selectedId],
  )

  const focusDonorHighlight = focusMode === 'donor' ? focusDonorIndex : null

  const contestedCount = useMemo(
    () => displayTracks.filter((t) => t.contested || (t.jam ?? 0) > 25).length,
    [displayTracks],
  )

  const handleFocusDonor = useCallback(
    (index: number) => {
      if (!msg?.donors[index]) return
      setFocusMode('donor')
      setFocusDonorIndex(index)
      setRangeNm(suggestedRangeNm(msg.donors[index]!))
      setShowTerrain(true)
    },
    [msg],
  )

  return (
    <div className="gci-root theme-locked-dark">
      <header className="gci-bar">
        <div className="gci-bar-title">
          <span className="gci-bar-eyebrow">VECTOR STRIKE · GCI</span>
          <h1>GCI RADAR</h1>
        </div>

        <div className="gci-bar-group">
          <span className="gci-bar-label">DESK</span>
          {(['blue', 'red', 'god'] as const).map((d) => (
            <button
              key={d}
              type="button"
              className={`gci-btn${desk === d ? ' active' : ''}`}
              onClick={() => {
                setDesk(d)
                setSelectedId(null)
              }}
            >
              {d.toUpperCase()}
            </button>
          ))}
        </div>

        <div className="gci-bar-group gci-focus-group">
          <span className="gci-bar-label">PPI CENTER</span>
          <select
            className="gci-select"
            value={focusMode === 'donor' ? `donor:${focusDonorIndex}` : focusMode}
            onChange={(e) => {
              const v = e.target.value
              if (v === 'bullseye') {
                setFocusMode('bullseye')
                return
              }
              if (v === 'track') {
                if (selected) setFocusMode('track')
                return
              }
              if (v.startsWith('donor:')) {
                const i = Number(v.slice(6))
                handleFocusDonor(i)
              }
            }}
          >
            <option value="bullseye">BULLSEYE</option>
            {msg?.donors.map((d, i) => (
              <option key={`d-${i}`} value={`donor:${i}`}>
                {donorLabel(d, i)}
              </option>
            ))}
            <option value="track" disabled={!selected}>
              SELECTED TRACK{selected ? ` (${selected.tn})` : ''}
            </option>
          </select>
        </div>

        <div className="gci-bar-group">
          <span className="gci-bar-label">RNG NM</span>
          {RANGES.map((r) => (
            <button
              key={r}
              type="button"
              className={`gci-btn${rangeNm === r ? ' active' : ''}`}
              onClick={() => setRangeNm(r)}
            >
              {r}
            </button>
          ))}
        </div>

        <div className="gci-bar-group">
          <button
            type="button"
            className={`gci-btn${showDonors ? ' active' : ''}`}
            onClick={() => setShowDonors((v) => !v)}
          >
            DONORS
          </button>
          <button
            type="button"
            className={`gci-btn${showTerrain ? ' active' : ''}`}
            onClick={() => setShowTerrain((v) => !v)}
          >
            TERRAIN
          </button>
          <button
            type="button"
            className={`gci-btn${showJamZones ? ' active' : ''}`}
            onClick={() => setShowJamZones((v) => !v)}
          >
            JAM
          </button>
          <button
            type="button"
            className={`gci-btn${showTags ? ' active' : ''}`}
            onClick={() => setShowTags((v) => !v)}
          >
            TAGS
          </button>
          <button
            type="button"
            className={`gci-btn${showFogOfWar ? ' active' : ''}`}
            onClick={() => setShowFogOfWar((v) => !v)}
            title="Fog of War Overlay"
          >
            FOG
          </button>
        </div>

        <div className="gci-bar-group">
          <span className="gci-bar-label">ALT FLTR</span>
          <input
            type="number"
            className="gci-input"
            value={minAlt}
            onChange={(e) => setMinAlt(Number(e.target.value) || 0)}
            placeholder="Min"
          />
          <span style={{ color: 'rgba(142, 200, 63, 0.5)', margin: '0 2px' }}>-</span>
          <input
            type="number"
            className="gci-input"
            value={maxAlt}
            onChange={(e) => setMaxAlt(Number(e.target.value) || 0)}
            placeholder="Max"
          />
        </div>

        <div className={`gci-status gci-status-${status}`}>
          {status === 'open' ? 'LIVE' : status === 'error' ? 'ERR' : 'OFF'}
          {msg?.time && (
            <span className="gci-status-time">
              {new Date(msg.time).toISOString().slice(11, 19)}Z
            </span>
          )}
        </div>
      </header>

      <div className="gci-main">
        <div className="gci-scope-panel">
          <GciScope
            msg={msg}
            anchor={anchor}
            tracks={displayTracks}
            rangeNm={rangeNm}
            showDonors={showDonors}
            showTerrain={showTerrain}
            showJamZones={showJamZones}
            showTags={showTags}
            showFogOfWar={showFogOfWar}
            minAlt={minAlt}
            maxAlt={maxAlt}
            selectedId={selectedId}
            focusDonorIndex={focusDonorHighlight}
            objectives={objectives}
            onSelect={setSelectedId}
            onFocusDonor={handleFocusDonor}
          />
        </div>

        <aside className="gci-side">
          <div className="gci-side-head">TRACK LIST</div>
          {contestedCount > 0 && (
            <p className="gci-ew-warn">
              {contestedCount} contested — ECM/chaff/jam corridor degrading track quality
            </p>
          )}
          <p className="gci-hint">BRG/RNG from PPI center. Click a donor ring on scope to focus it.</p>
          <div className="gci-table-wrap">
            <table className="gci-table">
              <thead>
                <tr>
                  <th>ID</th>
                  <th>BRG</th>
                  <th>NM</th>
                  <th>FL</th>
                  <th>SPD</th>
                  <th>AGE</th>
                  <th>SC</th>
                </tr>
              </thead>
              <tbody>
                {displayTracks.length === 0 && (
                  <tr>
                    <td colSpan={7} className="gci-empty">
                      {status === 'open'
                        ? 'No tracks in picture'
                        : 'Waiting for campaign EWR feed…'}
                    </td>
                  </tr>
                )}
                {displayTracks.map((t) => (
                  <TrackRow
                    key={`${t.id}-${t.tn}`}
                    t={t}
                    selected={t.id === selectedId}
                    onSelect={() => setSelectedId(t.id)}
                  />
                ))}
              </tbody>
            </table>
          </div>

          {selected && (
            <div className="gci-amplify">
              <div className="gci-amplify-title">AMPLIFY — {selected.tn}</div>
              <div className="gci-amplify-grid">
                <span>BRG</span><span>{selected.brg}°</span>
                <span>RNG</span><span>{selected.rng_nm} NM</span>
                <span>ALT</span><span>{selected.alt_ft.toLocaleString()} ft</span>
                <span>SPD</span><span>{selected.spd_kts} kts</span>
                <span>HDG</span><span>{selected.hdg}°</span>
                <span>AGE</span><span>{selected.age}s{selected.stale ? ' *' : ''}</span>
                <span>CLS</span><span>{clsLabel(selected.cls)}</span>
                <span>SRC</span><span>{srcLabel(selected.src)}</span>
                <span>CONF</span><span>{(selected.conf * 100).toFixed(0)}%</span>
                {(selected.contested || (selected.jam ?? 0) > 0) && (
                  <>
                    <span>EW</span>
                    <span className="gci-j-flag">J {selected.jam ?? 0}%</span>
                  </>
                )}
              </div>
              <button
                type="button"
                className="gci-btn gci-amplify-btn"
                onClick={() => setFocusMode('track')}
              >
                CENTER PPI ON TRACK
              </button>
            </div>
          )}

          <p className="gci-footnote">
            IADN fused picture. Players: F10 → EWR → Toggle ECM / Deploy Chaff. Mission jam corridors
            in cfg <code>jam.jam_zones</code>. Contested tracks (J) have reduced detection confidence.
          </p>
        </aside>
      </div>
    </div>
  )
}
