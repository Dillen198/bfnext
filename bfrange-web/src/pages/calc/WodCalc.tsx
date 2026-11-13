/**
 * Wind over deck: what the deck feels for a given ship course and speed, and
 * the BRC and speed that put 25–30 kt straight down the landing area.
 *
 * By default everything comes from the running mission for the chosen ship
 * (LIVE · DCS): its heading and speed, the true wind DCS has at the ship and
 * its deck geometry, with the engine's own measured WOD alongside as a
 * cross-check. Typing over any of it (or taking the recommended course)
 * makes it a what-if (EDITED).
 */
import { useMemo, useState } from 'react'
import { useSearchParams } from 'react-router-dom'
import { NumberField, Panel, Select } from '../../components/Controls'
import { fmt, fmtSigned, pad3 } from '../../lib/format'
import { norm180, rad } from '../../lib/geo'
import { NIMITZ_DECK_ANGLE_DEG, isStraightDeck, recommendBrc, windOverDeck, type Wod } from '../../lib/wod'
import { useLivePicture } from '../../lib/useLivePicture'
import type { LiveCarrier } from '../../types'
import { SourceChip, SourceNote, type Source } from './LiveSource'

/** What the fields hold when there is nothing live to start from. */
const DEFAULTS = { hdg: 40, speed: 20, windFrom: 30, windKts: 10 }

const DECKS = [
  { value: String(NIMITZ_DECK_ANGLE_DEG), label: 'Angled deck, 9.1° to port (Nimitz, Forrestal)' },
  { value: '0', label: 'Straight deck (Tarawa, LHA / LHD)' },
]

const r1 = (x: number) => Math.round(x * 10) / 10

/** The deck geometry in words. `angle` is FB − BRC, negative = to port. */
function deckText(angle: number): string {
  if (isStraightDeck(angle)) return 'Straight deck: the landing area runs straight along the bow.'
  const side = angle < 0 ? 'port' : 'starboard'
  return `Angled deck: the landing area points ${fmt(Math.abs(angle), 1)}° to ${side} of the bow, so final bearing = BRC ${angle < 0 ? '−' : '+'} ${fmt(Math.abs(angle), 1)}°.`
}

/** A ship's live deck angle: DCS's own, or worked out from FB and BRC on an older engine. */
function liveDeckAngle(c: LiveCarrier): number {
  return c.deck_angle_deg ?? norm180(c.fb_deg - c.brc_deg)
}

/** Which side the relative wind comes from, off the landing area. */
const sideOf = (deg: number) => (Math.abs(deg) < 0.05 ? 'straight down' : deg > 0 ? 'from starboard' : 'from port')

function DeckDiagram({ w, heading, deckAngle }: { w: Wod; heading: number; deckAngle: number }) {
  // plan view, ship's bow up
  const S = 300, c = S / 2
  const rel = rad(w.wod_from_deg - heading)
  const L = Math.min(120, 40 + w.wod_kts * 2.4)
  const fx = c + Math.sin(rel) * L, fy = c - Math.cos(rel) * L
  // negative deck angle = to port = to the left with the bow up
  const a = rad(deckAngle)
  const straight = isStraightDeck(deckAngle)
  const x0 = straight ? c : c + 10
  const ok = w.axial_kts >= 25 && w.axial_kts <= 30 && Math.abs(w.cross_kts) <= 7
  return (
    <svg viewBox={`0 0 ${S} ${S}`} className="plot" style={{ maxWidth: 320 }} role="img" aria-label={`Relative wind ${fmt(w.wod_kts, 1)} knots from ${fmt(w.rel_bow_deg)} degrees off the bow`}>
      <defs>
        <marker id="wod-ah" viewBox="0 0 10 10" refX="8" refY="5" markerWidth="7" markerHeight="7" orient="auto">
          <path d="M0 0 10 5 0 10z" fill={ok ? 'var(--datum)' : 'var(--ball)'} />
        </marker>
      </defs>
      {[40, 80, 120].map(r => <circle key={r} cx={c} cy={c} r={r} className="grid" fill="none" />)}
      {/* hull */}
      <path d={`M ${c} ${c - 110} L ${c + 22} ${c - 70} L ${c + 26} ${c + 100} L ${c - 22} ${c + 100} L ${c - 34} ${c - 20} L ${c - 18} ${c - 80} Z`} fill="var(--panel-3)" stroke="var(--haze)" />
      {/* landing-area centreline */}
      <line x1={x0} y1={c + 98} x2={x0 + Math.sin(a) * 200} y2={c + 98 - Math.cos(a) * 200} stroke="var(--band-0)" strokeDasharray="5 4" />
      <text x={c - 34} y={c + 118} textAnchor="end" style={{ fill: 'var(--band-0)' }}>
        {straight ? 'straight deck' : `angled deck ${fmt(Math.abs(deckAngle), 1)}°`}
      </text>
      {/* relative wind arrow: from the "from" point towards the ship */}
      <line x1={fx} y1={fy} x2={c + Math.sin(rel) * 18} y2={c - Math.cos(rel) * 18} stroke={ok ? 'var(--datum)' : 'var(--ball)'} strokeWidth={3} markerEnd="url(#wod-ah)" />
      <text x={fx} y={fy - 8} textAnchor="middle" style={{ fill: 'var(--chalk)', fontSize: 12, fontWeight: 600 }}>{fmt(w.wod_kts, 1)} kt</text>
      <text x={c} y={S - 6} textAnchor="middle" className="axis-label">bow up · relative wind</text>
    </svg>
  )
}

export function WodCalc() {
  const [sp] = useSearchParams()
  const live = useLivePicture()
  const [carrierId, setCarrierId] = useState(sp.get('carrier') ?? '')
  const [edited, setEdited] = useState(false)
  // the user's own numbers, used once they have edited (or nothing is live)
  const [hdg, setHdg] = useState(DEFAULTS.hdg)
  const [speed, setSpeed] = useState(DEFAULTS.speed)
  const [windFrom, setWindFrom] = useState(DEFAULTS.windFrom)
  const [windKts, setWindKts] = useState(DEFAULTS.windKts)
  const [manualDeck, setManualDeck] = useState(NIMITZ_DECK_ANGLE_DEG)

  const picture = live.data?.live ?? null
  const carriers = picture?.carriers ?? []
  const cv = carriers.find(c => c.id === carrierId) ?? carriers[0]
  const source: Source = edited ? 'edited' : live.isLoading ? 'loading' : cv ? 'live' : 'none'
  const useLive = source === 'live'

  // the true wind DCS has at the ship; an older engine only sends the range's surface wind
  const liveWind = cv
    ? cv.true_wind_kts !== undefined && cv.true_wind_from_deg !== undefined
      ? { from: cv.true_wind_from_deg, kts: cv.true_wind_kts }
      : { from: picture!.wind.surface_from_deg, kts: picture!.wind.surface_kts }
    : null
  // the ship's geometry is not a condition: it stays the live ship's while editing
  const deck = cv ? liveDeckAngle(cv) : manualDeck

  const shipHdg = useLive ? cv!.brc_deg : hdg
  const shipKts = useLive ? cv!.speed_kts : speed
  const twFrom = useLive ? liveWind!.from : windFrom
  const twKts = useLive ? liveWind!.kts : windKts

  /** Any edit turns the live values into a what-if, starting from them. */
  function editConditions(apply: () => void) {
    if (useLive) {
      setHdg(Math.round(shipHdg))
      setSpeed(r1(shipKts))
      setWindFrom(Math.round(twFrom))
      setWindKts(r1(twKts))
    }
    apply()
    setEdited(true)
  }

  const w = useMemo(
    () => windOverDeck({ ship_heading_deg: shipHdg, ship_speed_kts: shipKts, wind_from_deg: twFrom, wind_kts: twKts, deck_angle_deg: deck }),
    [shipHdg, shipKts, twFrom, twKts, deck],
  )
  const rec = useMemo(() => recommendBrc(twFrom, twKts, { deck_angle_deg: deck }), [twFrom, twKts, deck])
  const ok = w.axial_kts >= 25 && w.axial_kts <= 30
  const straight = isStraightDeck(deck)
  const along = straight ? 'Down the deck' : 'Down the angled deck'
  const hint = (liveText: string, text: string) => (useLive ? <span style={{ color: 'var(--datum)' }}>{liveText}</span> : text)

  return (
    <div className="grid gap-4 lg:grid-cols-[340px_minmax(0,1fr)]">
      <Panel title="Ship and wind" right={<SourceChip source={source} />}>
        <div className="flex flex-col gap-3">
          {carriers.length > 0 && (
            <Select label="Ship" value={cv?.id ?? ''} onChange={id => { setCarrierId(id); setEdited(false) }}
              options={carriers.map(c => ({ value: c.id, label: c.name }))} />
          )}
          <div className="grid grid-cols-2 gap-3">
            <NumberField label="Ship course (BRC)" unit="° true" value={Math.round(shipHdg)} min={0} max={360}
              onChange={v => editConditions(() => setHdg(v))} hint={hint('its heading now, from DCS', 'the heading it steams')} />
            <NumberField label="Ship speed" unit="kt" value={r1(shipKts)} min={0} max={35}
              onChange={v => editConditions(() => setSpeed(v))} hint={hint('from DCS', 'through the water')} />
            <NumberField label="True wind from" unit="° true" value={Math.round(twFrom)} min={0} max={360} step={5}
              onChange={v => editConditions(() => setWindFrom(v))} hint={hint('at the ship, from DCS', 'the direction it blows from')} />
            <NumberField label="True wind speed" unit="kt" value={r1(twKts)} min={0} step={1}
              onChange={v => editConditions(() => setWindKts(v))} hint={hint('at the ship, from DCS', 'the real wind, not the deck’s')} />
          </div>
          {cv ? (
            <p className="text-[12px] dim m-0">{deckText(deck)}</p>
          ) : (
            <Select label="Deck" value={String(manualDeck)} onChange={v => setManualDeck(Number(v))} options={DECKS} />
          )}
          <SourceNote
            source={source}
            updatedAt={live.dataUpdatedAt}
            canUseLive={!!cv}
            onUseLive={() => setEdited(false)}
            live={<>{cv?.name}’s heading, speed and deck, and the true wind at the ship, from the running mission.</>}
            edited={<>Your numbers: a what-if, not what the ship is doing.{cv ? '' : ' The range server is not sending live ships right now.'}</>}
            none={<>{picture ? 'No ship is on the range right now' : 'The range server is not running'}, so these are defaults: enter the ship and wind yourself.</>}
          />
        </div>
      </Panel>
      <div className="flex flex-col gap-4 min-w-0">
        <div className="panel panel-b grid gap-4 md:grid-cols-[320px_minmax(0,1fr)] items-center">
          <DeckDiagram w={w} heading={shipHdg} deckAngle={deck} />
          <div className="grid grid-cols-2 gap-4">
            <div className="kpi"><span className="caps">Wind over deck</span><span className="v" style={{ color: ok ? 'var(--datum)' : 'var(--ball)', fontSize: 30 }}>{fmt(w.wod_kts, 1)}<small>kt</small></span></div>
            <div className="kpi"><span className="caps">Final bearing</span><span className="v">{pad3(w.fb_deg)}°</span></div>
            <div className="kpi"><span className="caps">{along}</span><span className="v">{fmt(w.axial_kts, 1)}<small>kt</small></span><span className="mono text-[11px] dim">target 25–30 kt</span></div>
            <div className="kpi"><span className="caps">Across it</span><span className="v">{fmt(Math.abs(w.cross_kts), 1)}<small>kt {w.cross_kts >= 0 ? 'from stbd' : 'from port'}</small></span></div>
            <div className="kpi"><span className="caps">Off the bow</span><span className="v">{fmtSigned(w.rel_bow_deg, 1)}°</span><span className="mono text-[11px] dim">+ = from starboard</span></div>
            <div className="kpi"><span className="caps">{straight ? 'Off the deck' : 'Off the angled deck'}</span><span className="v">{fmtSigned(w.rel_deck_deg, 1)}°</span></div>
          </div>
        </div>
        {cv && (
          <Panel title={`On deck now · ${cv.name}`}
            right={<span className={`chip ${cv.recovery_open ? 'live' : 'outline'}`}>{cv.recovery_open ? <><span className="dot live" />RECOVERY OPEN</> : 'DECK CLOSED'}</span>}>
            <div className="flex flex-wrap items-end gap-6">
              <div className="kpi">
                <span className="caps">WOD measured by the range</span>
                <span className="v">{fmt(cv.wind_over_deck_kts, 1)}<small>kt</small></span>
                <span className="mono text-[11px] dim">{fmt(Math.abs(cv.wind_over_deck_angle_deg), 1)}° {sideOf(cv.wind_over_deck_angle_deg)}</span>
              </div>
              <div className="kpi">
                <span className="caps">{useLive ? 'Calculated here' : 'Your what-if'}</span>
                <span className="v">{fmt(w.wod_kts, 1)}<small>kt</small></span>
                <span className="mono text-[11px] dim">{fmt(Math.abs(w.rel_deck_deg), 1)}° {sideOf(w.rel_deck_deg)}</span>
              </div>
              <div className="kpi">
                <span className="caps">Recovery</span>
                <span className="v text-[16px]">{cv.case ? `Case ${['I', 'II', 'III'][cv.case - 1] ?? cv.case}` : '—'}</span>
                <span className="mono text-[11px] dim">{cv.next_window ? `window ${cv.next_window}` : cv.recovery_open ? 'open' : 'no window scheduled'}</span>
              </div>
            </div>
            <p className="text-[12px] dim mt-3 mb-0">
              {useLive
                ? Math.abs(cv.wind_over_deck_kts - w.wod_kts) <= 1.5
                  ? 'The range’s own reading agrees with this calculation.'
                  : 'The range measures the wind at deck height as the ship moves, so it can differ a little from this calculation.'
                : 'The range’s reading is what the deck feels right now; the numbers above are your what-if.'}
            </p>
          </Panel>
        )}
        <Panel title="Recommended recovery course">
          <div className="flex flex-wrap items-end gap-6">
            <div className="kpi"><span className="caps">BRC</span><span className="v" style={{ color: 'var(--ball)', fontSize: 30 }}>{pad3(rec.brc_deg)}°</span></div>
            <div className="kpi"><span className="caps">Ship speed</span><span className="v">{fmt(rec.ship_kts, 1)}<small>kt</small></span></div>
            <div className="kpi"><span className="caps">{straight ? 'Down the deck' : 'Axial WOD'}</span><span className="v">{fmt(rec.wod.axial_kts, 1)}<small>kt</small></span></div>
            <div className="kpi"><span className="caps">Crosswind</span><span className="v">{fmt(Math.abs(rec.wod.cross_kts), 1)}<small>kt</small></span></div>
            <button className="btn-range ml-auto" onClick={() => editConditions(() => { setHdg(Math.round(rec.brc_deg)); setSpeed(r1(rec.ship_kts)) })}>Use this course</button>
          </div>
          <p className="text-[13px] mt-3 mb-0">
            {rec.in_window ? <span className="chip live mr-2">IN LIMITS</span> : <span className="chip warn mr-2">BEST AVAILABLE</span>}
            {rec.note}
          </p>
        </Panel>
      </div>
    </div>
  )
}
