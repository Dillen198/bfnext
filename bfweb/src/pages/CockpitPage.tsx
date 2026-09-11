import { useState, useRef } from 'react'
import { useMutation, useQuery } from '@tanstack/react-query'
import { MapContainer, TileLayer, Marker, Polyline, useMapEvents } from 'react-leaflet'
import L from 'leaflet'
import { api, type CarpSolution } from '../api'
import { useAuth } from '../context/AuthContext'
import { campaign } from '../config/campaign'

// Standalone page (no dashboard chrome). Normally loaded inside DCS itself
// by bflib/lua/cockpit.lua, a Hooks script each player installs, via a
// dxgui WebViewWidget -- see that file for why this has to be a per-player
// local script and not something bflib triggers remotely. It passes
// ?playerid=<net.get_my_player_id()>, which bfdb resolves to a ucid using
// the live connected-player table -- no login, no manual linking, it just
// works the moment a player joins. If loaded standalone in a real browser
// (no playerid param), it falls back to the Discord-linked session flow
// below so this page is still testable outside DCS.
//
// Visual language: the CARP tab stands in for the C-130J's own CNI-MU
// (Control-Display Unit), so it's built to actually look like one instead
// of a generic dashboard -- phosphor-green monospace "glass" panels with a
// scanline/vignette overlay, and fields laid out as L1-L6/R1-R6 line-select
// rows matching the DCS C-130J User Manual's CARP INIT page numbering
// exactly (see bflib/src/carp.rs for the page-by-page source). Colors and
// fonts are all drawn from the app's existing --accent/--font-mono tokens
// (index.css) so this stays part of the same visual family, just executed
// as an instrument panel rather than a form.
export default function CockpitPage() {
  const playerId = new URLSearchParams(window.location.search).get('playerid') ?? undefined
  const { user, loading } = useAuth()

  if (playerId) return <Tabs playerId={playerId} />
  if (loading) return <Centered>Loading…</Centered>
  if (!user) return <LoginPrompt />
  if (!user.ucid) return <LinkPrompt />
  return <Tabs />
}

function Tabs({ playerId }: { playerId?: string }) {
  const [tab, setTab] = useState<'ewr' | 'carp' | 'cargo'>('ewr')
  return (
    <Shell
      tabs={
        <div style={{ display: 'flex', gap: '0.35rem' }}>
          <FnKey active={tab === 'ewr'} onClick={() => setTab('ewr')}>EWR</FnKey>
          <FnKey active={tab === 'carp'} onClick={() => setTab('carp')}>CARP</FnKey>
          <FnKey active={tab === 'cargo'} onClick={() => setTab('cargo')}>CARGO</FnKey>
        </div>
      }
    >
      {tab === 'ewr' && <EwrBody playerId={playerId} />}
      {tab === 'carp' && <CarpBody playerId={playerId} />}
      {tab === 'cargo' && <CargoBody playerId={playerId} />}
    </Shell>
  )
}

// A physical bezel button: rectangular, small LED dot, glows when active.
function FnKey({ active, onClick, children }: { active: boolean; onClick: () => void; children: React.ReactNode }) {
  return (
    <button
      onClick={onClick}
      style={{
        display: 'flex', alignItems: 'center', gap: '0.35rem',
        padding: '0.3rem 0.7rem', background: active ? 'var(--bg)' : 'var(--bg-card)',
        color: active ? 'var(--accent-bright)' : 'var(--text-dim)',
        border: `1px solid ${active ? 'var(--accent-border)' : 'var(--border)'}`,
        borderRadius: '2px', cursor: 'pointer', fontFamily: 'var(--font-mono)',
        fontSize: '0.68rem', letterSpacing: '0.1em',
        boxShadow: active ? '0 0 8px var(--accent-glow), inset 0 0 6px var(--accent-glow)' : 'none',
        transition: 'all 0.12s',
      }}
    >
      <span style={{
        width: '5px', height: '5px', borderRadius: '50%', flexShrink: 0,
        background: active ? 'var(--accent-bright)' : 'var(--border-light)',
        boxShadow: active ? '0 0 4px var(--accent-bright)' : 'none',
      }} />
      {children}
    </button>
  )
}

function Shell({ children, tabs }: { children: React.ReactNode; tabs?: React.ReactNode }) {
  return (
    <div className="theme-locked-dark" style={{
      height: '100%', minHeight: '100vh', background: 'var(--bg)', color: 'var(--text)',
      display: 'flex', flexDirection: 'column', fontFamily: 'var(--font-mono)',
    }}>
      <div style={{
        padding: '0.55rem 0.9rem', background: 'var(--bg-card)', borderBottom: '1px solid var(--border)',
        display: 'flex', alignItems: 'center', justifyContent: 'space-between', gap: '0.6rem',
      }}>
        <span style={{
          fontFamily: 'var(--font-display)', fontSize: '1.05rem', letterSpacing: '0.14em',
          color: 'var(--accent-bright)', textShadow: '0 0 8px var(--accent-glow)',
        }}>
          {campaign.name} <span style={{ color: 'var(--text-dim)' }}>CNI-MU</span>
        </span>
        {tabs}
      </div>
      <div style={{ flex: 1, overflow: 'auto', padding: '0.9rem' }}>{children}</div>
    </div>
  )
}

function Centered({ children }: { children: React.ReactNode }) {
  return (
    <div className="theme-locked-dark" style={{
      height: '100vh', display: 'flex', alignItems: 'center', justifyContent: 'center',
      background: 'var(--bg)', color: 'var(--text-dim)', fontSize: '0.8rem', fontFamily: 'var(--font-mono)',
    }}>
      {children}
    </div>
  )
}

function LoginPrompt() {
  return (
    <Shell>
      <p style={{ fontSize: '0.78rem', color: 'var(--text-muted)', marginBottom: '1rem' }}>
        Sign in with Discord to use the cockpit UI.
      </p>
      <a
        href={api.auth.loginUrl()}
        style={{
          display: 'inline-block', background: '#5865F2', color: '#fff',
          padding: '0.6rem 1.2rem', borderRadius: '2px', textDecoration: 'none',
          fontSize: '0.78rem', letterSpacing: '0.08em', fontFamily: 'var(--font-display)',
        }}
      >
        LOGIN WITH DISCORD
      </a>
    </Shell>
  )
}

function LinkPrompt() {
  const { refresh } = useAuth()
  const [checking, setChecking] = useState(false)

  async function handleRecheck() {
    setChecking(true)
    try {
      await refresh()
    } finally {
      setChecking(false)
    }
  }

  return (
    <Shell>
      <p style={{ fontSize: '0.78rem', color: 'var(--text-muted)', marginBottom: '0.75rem' }}>
        Your Discord account isn't linked to a DCS pilot yet. In Discord, run <code style={{ fontFamily: 'var(--font-mono)' }}>/linkme</code> to
        get a token, then type <code style={{ fontFamily: 'var(--font-mono)' }}>-linkme &lt;token&gt;</code> in DCS chat.
      </p>
      <Button onClick={handleRecheck} disabled={checking}>{checking ? '…' : "I'VE LINKED — CHECK AGAIN"}</Button>
    </Shell>
  )
}

// Bezel action button: bracket corners, amber glow on hover/active.
function Button({ onClick, disabled, children, tone = 'accent' }: {
  onClick: () => void
  disabled?: boolean
  children: React.ReactNode
  tone?: 'accent' | 'amber'
}) {
  const color = tone === 'amber' ? 'var(--yellow)' : 'var(--accent-bright)'
  return (
    <button
      onClick={onClick}
      disabled={disabled}
      style={{
        padding: '0.42rem 0.8rem', background: 'var(--bg-card)', color: disabled ? 'var(--text-dim)' : color,
        border: `1px solid ${disabled ? 'var(--border)' : color}`, borderRadius: '2px',
        cursor: disabled ? 'not-allowed' : 'pointer', fontFamily: 'var(--font-mono)',
        fontSize: '0.7rem', letterSpacing: '0.08em', opacity: disabled ? 0.45 : 1,
        boxShadow: disabled ? 'none' : `0 0 6px ${tone === 'amber' ? 'rgba(201,162,39,0.18)' : 'var(--accent-glow)'}`,
      }}
    >
      ‹{children}›
    </button>
  )
}

// ─── CRT glass panel ──────────────────────────────────────────────────────
// Shared "screen" chrome: scanlines + vignette + glow border. Used for both
// the EWR report readout and every CARP page, so the whole cockpit reads as
// one instrument, not a page full of mismatched widgets.
function Glass({ corner, children }: { corner?: string; children: React.ReactNode }) {
  return (
    <div style={{
      position: 'relative', background: 'var(--bg-card)', border: '1px solid var(--accent-border)',
      borderRadius: '3px', boxShadow: '0 0 14px var(--accent-glow), inset 0 0 24px rgba(0,0,0,0.5)',
      overflow: 'hidden', padding: '0.7rem 0.8rem',
    }}>
      <div style={{
        position: 'absolute', inset: 0, pointerEvents: 'none',
        background: 'repeating-linear-gradient(0deg, rgba(0,0,0,0) 0px, rgba(0,0,0,0.12) 1px, rgba(0,0,0,0) 3px)',
      }} />
      <div style={{
        position: 'absolute', inset: 0, pointerEvents: 'none',
        background: 'radial-gradient(ellipse at 50% 40%, transparent 55%, rgba(0,0,0,0.35) 100%)',
      }} />
      {corner && (
        <span style={{
          position: 'absolute', top: '0.4rem', right: '0.6rem', fontSize: '0.58rem',
          color: 'var(--text-dim)', letterSpacing: '0.08em',
        }}>
          {corner}
        </span>
      )}
      <div style={{ position: 'relative' }}>{children}</div>
    </div>
  )
}

function EwrBody({ playerId }: { playerId?: string }) {
  const [report, setReport] = useState<string>('SELECT A REPORT ABOVE')

  const reportMut = useMutation({
    mutationFn: (friendly: boolean) => api.cockpit.ewrReport(friendly, playerId),
    onSuccess: r => setReport(r.report),
  })
  const intelMut = useMutation({
    mutationFn: () => api.cockpit.ewrIntel(playerId),
    onSuccess: r => setReport(r.report),
  })
  const toggleMut = useMutation({
    mutationFn: () => api.cockpit.ewrToggle(playerId),
    onSuccess: r => setReport(`EWR reports are now ${r.state}`),
  })
  const unitsMut = useMutation({
    mutationFn: (imperial: boolean) => api.cockpit.ewrUnits(imperial, playerId),
    onSuccess: r => setReport(`EWR units are now ${r.units}`),
  })

  const busy = reportMut.isPending || intelMut.isPending || toggleMut.isPending || unitsMut.isPending
  const err = reportMut.error ?? intelMut.error ?? toggleMut.error ?? unitsMut.error

  return (
    <>
      <PageEyebrow>EWR</PageEyebrow>
      <div style={{ display: 'flex', flexWrap: 'wrap', gap: '0.4rem', marginBottom: '0.75rem' }}>
        <Button disabled={busy} onClick={() => reportMut.mutate(false)}>BANDITS</Button>
        <Button disabled={busy} onClick={() => reportMut.mutate(true)}>FRIENDLIES</Button>
        <Button disabled={busy} onClick={() => intelMut.mutate()}>GND INTEL</Button>
        <Button disabled={busy} onClick={() => toggleMut.mutate()}>TOGGLE</Button>
        <Button disabled={busy} onClick={() => unitsMut.mutate(true)}>IMPERIAL</Button>
        <Button disabled={busy} onClick={() => unitsMut.mutate(false)}>METRIC</Button>
      </div>
      {err && (
        <p style={{ fontSize: '0.7rem', color: 'var(--red)', marginBottom: '0.5rem' }}>
          {err instanceof Error ? err.message : 'Request failed'}
        </p>
      )}
      <Glass corner="EWR">
        <pre style={{
          fontSize: '0.7rem', lineHeight: 1.6, whiteSpace: 'pre-wrap',
          color: 'var(--accent-bright)', textShadow: '0 0 6px var(--accent-glow)', margin: 0,
          fontFamily: 'var(--font-mono)',
        }}>
          {report}
        </pre>
      </Glass>
    </>
  )
}

// ─── Cargo ────────────────────────────────────────────────────────────────
// Same backend logic the F10 "Cargo" menu's "Spawn N Crates" items call
// (bflib/src/menu/cargo.rs's spawn_crates_for_ucid) -- the only real
// difference from F10 is a free quantity field here instead of a fixed
// menu list of preset amounts (F10 radio menus can't take free-form
// numeric input at all).
const CARGO_KIND_OPTIONS = ['C-130', 'HELO'] as const
type CargoKind = (typeof CARGO_KIND_OPTIONS)[number]

function CargoBody({ playerId }: { playerId?: string }) {
  const [crateName, setCrateName] = useState('')
  const [qty, setQty] = useState('1')
  const [kind, setKind] = useState<CargoKind>('C-130')
  const [message, setMessage] = useState<string>('ENTER A CRATE NAME AND QUANTITY, THEN SPAWN')

  const spawnMut = useMutation({
    mutationFn: () => api.cockpit.cargoSpawn(crateName.trim(), Number(qty) || 1, kind === 'C-130', playerId),
    onSuccess: r => setMessage(r.message),
  })

  const qtyNum = Number(qty)
  const validQty = Number.isFinite(qtyNum) && qtyNum >= 1
  const canSpawn = crateName.trim().length > 0 && validQty && !spawnMut.isPending

  return (
    <>
      <PageEyebrow>CARGO — CRATE SPAWN</PageEyebrow>
      <Glass corner="CARGO REQUEST">
        <LskPage>
          <LskCol>
            <Lsk tag="L1" label="CRATE NAME">
              <LskInput value={crateName} onChange={setCrateName} placeholder="as configured in cfg.deployables" />
            </Lsk>
            <Lsk tag="L2" label="QUANTITY">
              <LskInput value={qty} onChange={setQty} numeric />
            </Lsk>
            <Lsk tag="L3" label="AIRCRAFT">
              <LskSelect value={kind} onChange={setKind} options={CARGO_KIND_OPTIONS} />
            </Lsk>
          </LskCol>
          <LskCol>
            <Lsk tag="R3" label="SPAWN">
              <Button disabled={!canSpawn} onClick={() => spawnMut.mutate()}>
                {spawnMut.isPending ? 'QUEUING…' : `SPAWN ${validQty ? qtyNum : ''}`}
              </Button>
            </Lsk>
          </LskCol>
        </LskPage>
        {spawnMut.error && (
          <p style={{ fontSize: '0.68rem', color: 'var(--red)', margin: '0.5rem 0 0' }}>
            {spawnMut.error instanceof Error ? spawnMut.error.message : 'Request failed'}
          </p>
        )}
      </Glass>
      <div style={{ height: '0.6rem' }} />
      <Glass corner="STATUS">
        <p style={{
          fontSize: '0.7rem', lineHeight: 1.6, margin: 0, fontFamily: 'var(--font-mono)',
          color: 'var(--accent-bright)', textShadow: '0 0 6px var(--accent-glow)',
        }}>
          {message}
        </p>
      </Glass>
    </>
  )
}

function PageEyebrow({ children }: { children: React.ReactNode }) {
  return (
    <div style={{ fontSize: '0.62rem', color: 'var(--text-dim)', letterSpacing: '0.16em', marginBottom: '0.5rem' }}>
      {children}
    </div>
  )
}

// ─── CARP ───────────────────────────────────────────────────────────────
// Mirrors the C-130J CNI-MU's CARP INIT 1/5-4/5, CHUTE LIST, and CARP PROG
// 2/2 field layout, using the manual's own L1-L6/R1-R6 line-select-key
// numbering (see bflib/src/carp.rs for the page-by-page breakdown this is
// built from). Fields the mission can supply (PI position, elevations,
// wind, temp) are fetched from the engine and shown read-only; everything
// else is a pilot/mission-planning choice, entered by hand. The CNI-MU
// itself supports "up to ten separate CARP procedures ... differentiated
// via identifier numbers 1-10" (CARP INIT, R2) -- this mirrors that with
// up to 10 local entries. Nothing here is persisted server-side: it's a
// reference readout/log to copy into the CNI-MU and back, not a
// replacement for it. CARP PROG's drift/drop-result fields aren't driven
// by live telemetry (this page has no feed of the aircraft's actual
// position/track) -- they're logging fields the crew fills in from what
// they see in the cockpit, plus simple weight-tracking arithmetic for the
// drop-confirmation fields.

// Two side-by-side columns (L1-L6 on the left, R1-R6 on the right) --
// a real two-column grid, not just alternating rows, so the two sets of
// tags actually line up as two columns like a real CDU page instead of
// stacking on top of each other.
function LskPage({ children }: { children: React.ReactNode }) {
  return (
    <div style={{ display: 'flex', gap: '1.2rem', flexWrap: 'wrap' }}>
      {children}
    </div>
  )
}
function LskCol({ children }: { children: React.ReactNode }) {
  return <div style={{ flex: '1 1 260px', minWidth: '240px' }}>{children}</div>
}

// One line-select-key row: a bracketed tag (L1.../R1...) plus label plus
// value/control, matching a real CDU's line-select layout.
function Lsk({ tag, label, children }: {
  tag: string
  label: string
  children: React.ReactNode
}) {
  return (
    <div style={{
      display: 'flex', alignItems: 'center', gap: '0.45rem', padding: '0.32rem 0',
      borderBottom: '1px solid var(--border)',
    }}>
      <span style={{
        display: 'inline-flex', alignItems: 'center', justifyContent: 'center', flexShrink: 0,
        minWidth: '1.7rem', height: '1.15rem', padding: '0 0.15rem', fontSize: '0.56rem', fontWeight: 700,
        color: 'var(--accent-dim)', border: '1px solid var(--border-light)', borderRadius: '2px',
      }}>
        {tag}
      </span>
      <span style={{ fontSize: '0.6rem', color: 'var(--text-dim)', letterSpacing: '0.03em', flexShrink: 0 }}>
        {label}
      </span>
      <div style={{ flex: 1, display: 'flex', justifyContent: 'flex-end', minWidth: 0 }}>
        {children}
      </div>
    </div>
  )
}

function LskValue({ children, dim }: { children: React.ReactNode; dim?: boolean }) {
  return (
    <span style={{
      fontFamily: 'var(--font-mono)', fontSize: '0.72rem',
      color: dim ? 'var(--text-muted)' : 'var(--accent-bright)',
      textShadow: dim ? 'none' : '0 0 6px var(--accent-glow)',
      whiteSpace: 'nowrap', overflow: 'hidden', textOverflow: 'ellipsis',
    }}>
      {children}
    </span>
  )
}

const scratchInputStyle: React.CSSProperties = {
  width: '100%', maxWidth: '9rem', padding: '0.2rem 0.4rem', background: 'var(--bg-input)',
  border: '1px solid var(--border)', borderRadius: '2px', color: 'var(--accent-bright)',
  fontSize: '0.72rem', outline: 'none', textAlign: 'right', fontFamily: 'var(--font-mono)',
}

function LskInput({ value, onChange, placeholder, numeric }: {
  value: string
  onChange: (v: string) => void
  placeholder?: string
  numeric?: boolean
}) {
  return (
    <input
      value={value}
      onChange={e => onChange(e.target.value)}
      placeholder={placeholder}
      inputMode={numeric ? 'decimal' : 'text'}
      style={scratchInputStyle}
    />
  )
}

function LskSelect<T extends string>({ value, onChange, options }: {
  value: T
  onChange: (v: T) => void
  options: readonly T[]
}) {
  return (
    <select
      value={value}
      onChange={e => onChange(e.target.value as T)}
      style={{ ...scratchInputStyle, textAlign: 'left' }}
    >
      {options.map(o => <option key={o} value={o}>{o}</option>)}
    </select>
  )
}

function fmtWind(dirDeg: number, spdKt: number): string {
  return `${Math.round(dirDeg).toString().padStart(3, '0')}° / ${Math.round(spdKt)} kt`
}

const LOAD_TYPES = ['PER', 'CDS', 'HE', 'BDL-OTH'] as const
type LoadType = (typeof LOAD_TYPES)[number]
const CHUTE_TYPES = ['G-12D', 'G-12E'] as const
type ChuteType = (typeof CHUTE_TYPES)[number]
const DZ_ESC_OPTIONS = ['ESC', 'L', 'R'] as const
type DzEsc = (typeof DZ_ESC_OPTIONS)[number]
const RELEASE_SYS_OPTIONS = ['TOW', 'EXTR'] as const
const WIND_SOURCE_OPTIONS = ['SEN', 'ENT'] as const
type WindSource = (typeof WIND_SOURCE_OPTIONS)[number]
const DRIFT_SIDE_OPTIONS = ['L', 'R'] as const

interface CarpEntry {
  id: number
  // CARP INIT 1/5
  markKey: string
  dropAltFt: string
  leTe: string
  lePi: string
  sdDist: string
  tpDist: string
  nextWpTurnDist: string
  runInCourse: string
  tot: string
  solution: CarpSolution | null
  solveError: string | null
  // CARP INIT 2/5
  loadType: LoadType
  stages: string
  chuteType: ChuteType
  chuteQty: string
  cas: string
  dzEsc: DzEsc
  fusSta: string
  releaseSys: string
  weightPer: string
  qty: string
  // CARP INIT 3/5 (entered-wind override, used when windSource === 'ENT')
  windSource: WindSource
  enteredAltDir: string
  enteredAltSpd: string
  enteredSfcDir: string
  enteredSfcSpd: string
  // CARP INIT 4/5
  rqdClncHt: string
  minDropHt: string
  // CARP PROG 2/2 (pilot-logged, not telemetry-driven)
  driftDeg: string
  driftSide: (typeof DRIFT_SIDE_OPTIONS)[number]
  dropResultDir: string
  dropResultYd: string
  droppedLbs: number
}

function newEntry(id: number): CarpEntry {
  return {
    id,
    markKey: '', dropAltFt: '1200', leTe: '', lePi: '', sdDist: '', tpDist: '', nextWpTurnDist: '', runInCourse: '',
    tot: '', solution: null, solveError: null,
    loadType: 'CDS', stages: '1', chuteType: 'G-12E', chuteQty: '1', cas: '140', dzEsc: 'ESC',
    fusSta: '', releaseSys: 'EXTR', weightPer: '', qty: '1',
    windSource: 'SEN', enteredAltDir: '', enteredAltSpd: '', enteredSfcDir: '', enteredSfcSpd: '',
    rqdClncHt: '', minDropHt: '',
    driftDeg: '', driftSide: 'L', dropResultDir: '', dropResultYd: '', droppedLbs: 0,
  }
}

const MAX_ENTRIES = 10

// ─── CARP map ───────────────────────────────────────────────────────────
// Click-to-set PI, no F10 mark needed: picks a lat/long directly off the
// map and solves from that (api.cockpit.carpSolveLatLon). Also plots TP
// and SD as reference points, projected back along the run-in course from
// the PI, when those fields are filled in.

function projectLatLon(lat: number, lon: number, bearingDeg: number, distNm: number): [number, number] {
  const R = 3440.065, d = distNm / R, brg = bearingDeg * Math.PI / 180
  const phi1 = lat * Math.PI / 180, lambda1 = lon * Math.PI / 180
  const phi2 = Math.asin(Math.sin(phi1) * Math.cos(d) + Math.cos(phi1) * Math.sin(d) * Math.cos(brg))
  const lambda2 = lambda1 + Math.atan2(Math.sin(brg) * Math.sin(d) * Math.cos(phi1), Math.cos(d) - Math.sin(phi1) * Math.sin(phi2))
  return [phi2 * 180 / Math.PI, lambda2 * 180 / Math.PI]
}

function carpDivIcon(label: string, color: string, active: boolean): L.DivIcon {
  const size = active ? 24 : 18
  return L.divIcon({
    html: `<div style="width:${size}px;height:${size}px;border-radius:50%;background:${color}26;
             border:2px solid ${color};color:${color};font-size:${active ? 10 : 8}px;font-weight:700;
             display:flex;align-items:center;justify-content:center;font-family:'Share Tech Mono',monospace;
             box-shadow:0 0 8px ${color}aa;">${label}</div>`,
    className: '', iconSize: [size, size], iconAnchor: [size / 2, size / 2],
  })
}

function MapClickHandler({ onClick }: { onClick: (lat: number, lon: number) => void }) {
  useMapEvents({ click: e => onClick(e.latlng.lat, e.latlng.lng) })
  return null
}

function CarpMap({ entries, activeId, onPick }: {
  entries: CarpEntry[]
  activeId: number
  onPick: (id: number, lat: number, lon: number) => void
}) {
  const { data: objectives = [] } = useQuery({
    queryKey: ['objectives-for-carp'],
    queryFn: () => api.objectives(),
    staleTime: Infinity,
  })
  const centerRef = useRef<[number, number] | null>(null)
  if (!centerRef.current) {
    const solved = entries.find(e => e.solution)?.solution
    if (solved) {
      centerRef.current = [solved.pi_lat, solved.pi_lon]
    } else if (objectives.length > 0) {
      const lats = objectives.map(o => o.lat), lons = objectives.map(o => o.lon)
      centerRef.current = [
        lats.reduce((a, b) => a + b, 0) / lats.length,
        lons.reduce((a, b) => a + b, 0) / lons.length,
      ]
    }
  }
  const center = centerRef.current ?? [42.35, 43.5]
  const active = entries.find(e => e.id === activeId)
  const activeColor = 'var(--yellow)'
  const inactiveColor = '#6aab1f'

  return (
    <div style={{
      width: '340px', minWidth: '280px', height: '454px', flexShrink: 0,
      display: 'flex', flexDirection: 'column', position: 'relative',
      border: '1px solid var(--accent-border)', borderRadius: '3px', overflow: 'hidden',
      boxShadow: '0 0 14px var(--accent-glow)',
    }}>
      <MapContainer center={center} zoom={objectives.length ? 8 : 6} style={{ width: '100%', flex: 1, background: '#040603' }}>
        <TileLayer
          url="https://server.arcgisonline.com/ArcGIS/rest/services/Canvas/World_Dark_Gray_Base/MapServer/tile/{z}/{y}/{x}"
          attribution="Esri"
        />
        <MapClickHandler onClick={(lat, lon) => onPick(activeId, lat, lon)} />
        {entries.map(e => {
          if (!e.solution) return null
          const isActive = e.id === activeId
          const color = isActive ? '#c9a227' : '#6aab1f'
          const piPos: [number, number] = [e.solution.pi_lat, e.solution.pi_lon]
          const runIn = Number(e.runInCourse)
          const tpDist = Number(e.tpDist)
          const sdDist = Number(e.sdDist)
          const hasRunIn = e.runInCourse.trim() !== '' && !Number.isNaN(runIn)
          const tp = hasRunIn && e.tpDist.trim() !== '' && !Number.isNaN(tpDist)
            ? projectLatLon(piPos[0], piPos[1], runIn + 180, tpDist) : null
          const sd = hasRunIn && e.sdDist.trim() !== '' && !Number.isNaN(sdDist)
            ? projectLatLon(piPos[0], piPos[1], runIn + 180, sdDist) : null
          const linePts: [number, number][] = [tp, sd, piPos].filter((p): p is [number, number] => p !== null)
          return (
            <div key={e.id}>
              <Marker position={piPos} icon={carpDivIcon(`${e.id}`, color, isActive)} />
              {isActive && tp && <Marker position={tp} icon={carpDivIcon('TP', color, false)} />}
              {isActive && sd && <Marker position={sd} icon={carpDivIcon('SD', color, false)} />}
              {isActive && linePts.length > 1 && <Polyline positions={linePts} pathOptions={{ color, weight: 1.5, dashArray: '4 4' }} />}
            </div>
          )
        })}
      </MapContainer>
      <div style={{
        fontSize: '0.6rem', color: 'var(--text-dim)', padding: '0.3rem 0.5rem',
        background: 'var(--bg-card)', borderTop: '1px solid var(--border)',
        fontFamily: 'var(--font-mono)', display: 'flex', alignItems: 'center', gap: '0.4rem',
      }}>
        <span style={{ width: '6px', height: '6px', borderRadius: '50%', background: activeColor, boxShadow: `0 0 4px ${activeColor}`, flexShrink: 0 }} />
        CLICK TO SET PI — CARP {active?.id ?? activeId} — NO F10 MARK NEEDED
        <span style={{ width: '6px', height: '6px', borderRadius: '50%', background: inactiveColor, marginLeft: 'auto', flexShrink: 0 }} />
        <span>OTHER ENTRIES</span>
      </div>
    </div>
  )
}

function EntrySelect({ entries, activeId, setActiveId, addEntry, removeEntry }: {
  entries: CarpEntry[]
  activeId: number
  setActiveId: (id: number) => void
  addEntry: () => void
  removeEntry: (id: number) => void
}) {
  return (
    <div style={{ display: 'flex', flexWrap: 'wrap', alignItems: 'center', gap: '0.3rem', marginBottom: '0.6rem' }}>
      <span style={{ fontSize: '0.58rem', color: 'var(--text-dim)', letterSpacing: '0.1em', marginRight: '0.2rem' }}>
        CARP ID
      </span>
      {entries.map(e => {
        const active = e.id === activeId
        return (
          <div key={e.id} style={{ display: 'flex', alignItems: 'stretch' }}>
            <button
              onClick={() => setActiveId(e.id)}
              style={{
                width: '1.9rem', height: '1.6rem', background: active ? 'var(--bg)' : 'var(--bg-card)',
                color: active ? 'var(--yellow)' : 'var(--text-dim)',
                border: `1px solid ${active ? 'var(--yellow)' : 'var(--border)'}`,
                borderRadius: entries.length > 1 ? '2px 0 0 2px' : '2px', borderRight: entries.length > 1 ? 'none' : undefined,
                cursor: 'pointer', fontSize: '0.68rem', fontFamily: 'var(--font-mono)', fontWeight: 700,
                boxShadow: active ? '0 0 6px rgba(201,162,39,0.35)' : 'none',
              }}
            >
              {e.id}
            </button>
            {entries.length > 1 && (
              <button
                onClick={() => removeEntry(e.id)}
                aria-label={`Remove CARP ${e.id}`}
                style={{
                  padding: '0 0.35rem', background: 'var(--bg-card)', color: 'var(--text-dim)',
                  border: '1px solid var(--border)', borderLeft: 'none', borderRadius: '0 2px 2px 0',
                  cursor: 'pointer', fontSize: '0.62rem',
                }}
              >
                ×
              </button>
            )}
          </div>
        )
      })}
      {entries.length < MAX_ENTRIES && (
        <button
          onClick={addEntry}
          style={{
            height: '1.6rem', padding: '0 0.5rem', background: 'transparent', color: 'var(--text-dim)',
            border: '1px dashed var(--border-light)', borderRadius: '2px', cursor: 'pointer', fontSize: '0.62rem',
            fontFamily: 'var(--font-mono)',
          }}
        >
          +
        </button>
      )}
    </div>
  )
}

const CARP_PAGES = [
  { id: 'p1', label: '1/5' },
  { id: 'p2', label: '2/5' },
  { id: 'chute', label: 'CHUTE' },
  { id: 'p3', label: '3/5' },
  { id: 'p4', label: '4/5' },
  { id: 'prog', label: 'PROG' },
] as const
type CarpPageId = (typeof CARP_PAGES)[number]['id']

function PageNav({ page, setPage }: { page: CarpPageId; setPage: (p: CarpPageId) => void }) {
  return (
    <div style={{ display: 'flex', gap: '0.25rem', marginBottom: '0.5rem' }}>
      {CARP_PAGES.map(p => {
        const active = p.id === page
        return (
          <button
            key={p.id}
            onClick={() => setPage(p.id)}
            style={{
              flex: 1, padding: '0.3rem 0', background: active ? 'var(--bg)' : 'var(--bg-card)',
              color: active ? 'var(--yellow)' : 'var(--text-dim)',
              border: `1px solid ${active ? 'var(--yellow)' : 'var(--border)'}`,
              borderBottom: active ? '1px solid var(--bg)' : `1px solid ${'var(--border)'}`,
              borderRadius: '2px 2px 0 0', cursor: 'pointer', fontSize: '0.64rem', fontWeight: 700,
              fontFamily: 'var(--font-mono)', letterSpacing: '0.04em',
              boxShadow: active ? '0 -2px 6px rgba(201,162,39,0.25)' : 'none',
            }}
          >
            {p.label}
          </button>
        )
      })}
    </div>
  )
}

function CarpBody({ playerId }: { playerId?: string }) {
  const [entries, setEntries] = useState<CarpEntry[]>([newEntry(1)])
  const [activeId, setActiveId] = useState(1)
  const [page, setPage] = useState<CarpPageId>('p1')

  const entry = entries.find(e => e.id === activeId) ?? entries[0]
  const update = (patch: Partial<CarpEntry>) =>
    setEntries(es => es.map(e => (e.id === entry.id ? { ...e, ...patch } : e)))

  function addEntry() {
    if (entries.length >= MAX_ENTRIES) return
    const used = new Set(entries.map(e => e.id))
    let id = 1
    while (used.has(id) && id <= MAX_ENTRIES) id++
    setEntries(es => [...es, newEntry(id)])
    setActiveId(id)
  }
  function removeEntry(id: number) {
    setEntries(es => {
      const next = es.filter(e => e.id !== id)
      return next.length ? next : [newEntry(1)]
    })
    if (activeId === id) {
      const remaining = entries.filter(e => e.id !== id)
      setActiveId(remaining[0]?.id ?? 1)
    }
  }

  const solveMut = useMutation({
    mutationFn: async (targetId: number) => {
      const target = entries.find(e => e.id === targetId)
      if (!target) throw new Error('entry not found')
      const solution = await api.cockpit.carpSolve(target.markKey.trim(), Number(target.dropAltFt) || 0, playerId)
      return { targetId, solution }
    },
    onSuccess: ({ targetId, solution }) => {
      setEntries(es => es.map(e => (e.id === targetId ? { ...e, solution, solveError: null } : e)))
    },
    onError: (err, targetId) => {
      const message = err instanceof Error ? err.message : 'Request failed'
      setEntries(es => es.map(e => (e.id === targetId ? { ...e, solveError: message, solution: null } : e)))
    },
  })

  const solveLatLonMut = useMutation({
    mutationFn: async ({ id, lat, lon }: { id: number; lat: number; lon: number }) => {
      const target = entries.find(e => e.id === id)
      const dropAlt = target ? Number(target.dropAltFt) || 0 : 0
      const solution = await api.cockpit.carpSolveLatLon(lat, lon, dropAlt, playerId)
      return { id, solution }
    },
    onSuccess: ({ id, solution }) => {
      setEntries(es => es.map(e => (e.id === id ? { ...e, solution, solveError: null } : e)))
      setPage('p1')
    },
    onError: (err, vars) => {
      const message = err instanceof Error ? err.message : 'Request failed'
      setEntries(es => es.map(e => (e.id === vars.id ? { ...e, solveError: message } : e)))
    },
  })

  const s = entry.solution
  const totalWeight = (Number(entry.weightPer) || 0) * (Number(entry.qty) || 0)
  const remaining = Math.max(0, totalWeight - entry.droppedLbs)
  const usesExit = entry.loadType === 'PER' || entry.loadType === 'BDL-OTH'
  const showFusSta = !usesExit

  return (
    <div style={{ display: 'flex', flexWrap: 'wrap', gap: '0.75rem' }}>
      <CarpMap
        entries={entries}
        activeId={entry.id}
        onPick={(id, lat, lon) => solveLatLonMut.mutate({ id, lat, lon })}
      />

      <div style={{ flex: '1 1 480px', minWidth: '280px', maxWidth: '760px' }}>
        <PageEyebrow>CARP — COMPUTED AIR RELEASE POINT</PageEyebrow>
        <EntrySelect entries={entries} activeId={entry.id} setActiveId={setActiveId} addEntry={addEntry} removeEntry={removeEntry} />
        <PageNav page={page} setPage={setPage} />

        {page === 'p1' && (
          <Glass corner={`CARP INIT 1/5 · CARP ${entry.id}`}>
            <LskPage>
              <LskCol>
                <Lsk tag="L1" label="PI IDENT">
                  <LskInput value={entry.markKey} onChange={v => update({ markKey: v })} placeholder="F10 mark, or click map" />
                </Lsk>
                <Lsk tag="L2" label="PI LAT/LONG">
                  <LskValue dim={!s}>{s ? `${s.pi_lat.toFixed(4)}, ${s.pi_lon.toFixed(4)}` : '— CLICK MAP —'}</LskValue>
                </Lsk>
                <Lsk tag="L3" label="LE-TE (YD)">
                  <LskInput value={entry.leTe} onChange={v => update({ leTe: v })} numeric />
                </Lsk>
                <Lsk tag="L4" label="SD DIST (NM)">
                  <LskInput value={entry.sdDist} onChange={v => update({ sdDist: v })} numeric />
                </Lsk>
                <Lsk tag="L5" label="RUN-IN CRS">
                  <LskInput value={entry.runInCourse} onChange={v => update({ runInCourse: v })} placeholder="274" numeric />
                </Lsk>
                <Lsk tag="L6" label="ALT (FT AGL)">
                  <LskInput value={entry.dropAltFt} onChange={v => update({ dropAltFt: v })} numeric />
                </Lsk>
              </LskCol>
              <LskCol>
                <Lsk tag="R1" label="TOT">
                  <LskInput value={entry.tot} onChange={v => update({ tot: v })} placeholder="HH:MM:SS" />
                </Lsk>
                <Lsk tag="R2" label="PI MGRS">
                  <LskValue dim={!s}>{s ? s.pi_mgrs : '—'}</LskValue>
                </Lsk>
                <Lsk tag="R3" label="LE-PI (YD)">
                  <LskInput value={entry.lePi} onChange={v => update({ lePi: v })} numeric />
                </Lsk>
                <Lsk tag="R4" label="TP DIST (NM)">
                  <LskInput value={entry.tpDist} onChange={v => update({ tpDist: v })} numeric />
                </Lsk>
                <Lsk tag="R5" label="NEXT WP DIST">
                  <LskInput value={entry.nextWpTurnDist} onChange={v => update({ nextWpTurnDist: v })} numeric />
                </Lsk>
                <Lsk tag="R6" label="SOLVE">
                  <Button disabled={solveMut.isPending || !entry.markKey.trim()} onClick={() => solveMut.mutate(entry.id)}>SOLVE</Button>
                </Lsk>
              </LskCol>
            </LskPage>
            {entry.solveError && (
              <p style={{ fontSize: '0.68rem', color: 'var(--red)', margin: '0.5rem 0 0' }}>{entry.solveError}</p>
            )}
          </Glass>
        )}

        {page === 'p2' && (
          <Glass corner={`CARP INIT 2/5 · CARP ${entry.id}`}>
            <LskPage>
              <LskCol>
                <Lsk tag="L1" label="LOAD TYPE">
                  <LskSelect value={entry.loadType} onChange={v => update({ loadType: v })} options={LOAD_TYPES} />
                </Lsk>
                <Lsk tag="L2" label="STAGES">
                  <LskInput value={entry.stages} onChange={v => update({ stages: v })} numeric />
                </Lsk>
                <Lsk tag="L3" label="CHUTE / QTY">
                  <div style={{ display: 'flex', gap: '0.3rem', justifyContent: 'flex-end' }}>
                    <LskSelect value={entry.chuteType} onChange={v => update({ chuteType: v })} options={CHUTE_TYPES} />
                    <input value={entry.chuteQty} onChange={e => update({ chuteQty: e.target.value })} inputMode="decimal"
                      style={{ ...scratchInputStyle, width: '3rem', maxWidth: '3rem' }} />
                  </div>
                </Lsk>
                <Lsk tag="L4" label="CAS (KT)">
                  <LskInput value={entry.cas} onChange={v => update({ cas: v })} numeric />
                </Lsk>
                <Lsk tag="L5" label="DZ ESC">
                  <LskSelect value={entry.dzEsc} onChange={v => update({ dzEsc: v })} options={DZ_ESC_OPTIONS} />
                </Lsk>
              </LskCol>
              <LskCol>
                {showFusSta && (
                  <Lsk tag="R1" label="FUS STA">
                    <LskInput value={entry.fusSta} onChange={v => update({ fusSta: v })} numeric />
                  </Lsk>
                )}
                <Lsk tag="R2" label={usesExit ? 'EXIT' : 'RELEASE SYS'}>
                  {usesExit ? (
                    <LskInput value={entry.releaseSys} onChange={v => update({ releaseSys: v })} placeholder="paratroop door" />
                  ) : (
                    <LskSelect value={entry.releaseSys as 'TOW' | 'EXTR'} onChange={v => update({ releaseSys: v })} options={RELEASE_SYS_OPTIONS} />
                  )}
                </Lsk>
                <Lsk tag="R3" label="WT / QTY">
                  <div style={{ display: 'flex', gap: '0.3rem', justifyContent: 'flex-end' }}>
                    <input value={entry.weightPer} onChange={e => update({ weightPer: e.target.value })} inputMode="decimal" placeholder="lb"
                      style={{ ...scratchInputStyle, width: '4.2rem', maxWidth: '4.2rem' }} />
                    <input value={entry.qty} onChange={e => update({ qty: e.target.value })} inputMode="decimal"
                      style={{ ...scratchInputStyle, width: '3rem', maxWidth: '3rem' }} />
                  </div>
                </Lsk>
                <Lsk tag="R4" label="TOTAL WT">
                  <LskValue>{Math.round(totalWeight)} LB</LskValue>
                </Lsk>
              </LskCol>
            </LskPage>
          </Glass>
        )}

        {page === 'chute' && (
          <Glass corner="CHUTE LIST">
            <p style={{ fontSize: '0.68rem', color: 'var(--text-muted)', margin: '0 0 0.5rem' }}>
              PARACHUTE IDENTIFIERS WITH BALLISTICS DATA IN THE CNI-MU DATABASE:
            </p>
            <LskPage>
              <LskCol>
                <Lsk tag="L1" label="G-12D"><LskValue>AVAILABLE</LskValue></Lsk>
                <Lsk tag="L2" label="G-12E"><LskValue>AVAILABLE</LskValue></Lsk>
              </LskCol>
            </LskPage>
            <p style={{ fontSize: '0.64rem', color: 'var(--text-dim)', margin: '0.6rem 0 0', lineHeight: 1.5 }}>
              The CNI-MU computes the actual ballistics solution (time of fall, rate of fall,
              forward travel time) onboard from this database once load/chute is entered on
              CARP INIT 5/5 — that isn't reproduced here.
            </p>
          </Glass>
        )}

        {page === 'p3' && (
          <Glass corner={`CARP INIT 3/5 · CARP ${entry.id}`}>
            <LskPage>
              <LskCol>
                <Lsk tag="L1" label="ALT W/V"><LskValue dim={!s}>{s ? fmtWind(s.alt_wind_dir_deg, s.alt_wind_speed_kt) : '—'}</LskValue></Lsk>
                <Lsk tag="L3" label="SFC W/V"><LskValue dim={!s}>{s ? fmtWind(s.sfc_wind_dir_deg, s.sfc_wind_speed_kt) : '—'}</LskValue></Lsk>
                <Lsk tag="L4" label="BAL W/V"><LskValue dim={!s}>{s ? fmtWind(s.bal_wind_dir_deg, s.bal_wind_speed_kt) : '—'}</LskValue></Lsk>
                <Lsk tag="L5" label="WINDS">
                  <LskSelect value={entry.windSource} onChange={v => update({ windSource: v })} options={WIND_SOURCE_OPTIONS} />
                </Lsk>
              </LskCol>
              <LskCol>
                <Lsk tag="R1" label="ALT TEMP"><LskValue dim={!s}>{s ? `${Math.round(s.alt_temp_c)}°C` : '—'}</LskValue></Lsk>
                <Lsk tag="R3" label="SFC TEMP"><LskValue dim={!s}>{s ? `${Math.round(s.sfc_temp_c)}°C` : '—'}</LskValue></Lsk>
                {entry.windSource === 'ENT' && (
                  <>
                    <Lsk tag="R4" label="ENT ALT W/V">
                      <div style={{ display: 'flex', gap: '0.3rem', justifyContent: 'flex-end' }}>
                        <input value={entry.enteredAltDir} onChange={e => update({ enteredAltDir: e.target.value })} inputMode="decimal" placeholder="dir"
                          style={{ ...scratchInputStyle, width: '3.4rem', maxWidth: '3.4rem' }} />
                        <input value={entry.enteredAltSpd} onChange={e => update({ enteredAltSpd: e.target.value })} inputMode="decimal" placeholder="kt"
                          style={{ ...scratchInputStyle, width: '3rem', maxWidth: '3rem' }} />
                      </div>
                    </Lsk>
                    <Lsk tag="R5" label="ENT SFC W/V">
                      <div style={{ display: 'flex', gap: '0.3rem', justifyContent: 'flex-end' }}>
                        <input value={entry.enteredSfcDir} onChange={e => update({ enteredSfcDir: e.target.value })} inputMode="decimal" placeholder="dir"
                          style={{ ...scratchInputStyle, width: '3.4rem', maxWidth: '3.4rem' }} />
                        <input value={entry.enteredSfcSpd} onChange={e => update({ enteredSfcSpd: e.target.value })} inputMode="decimal" placeholder="kt"
                          style={{ ...scratchInputStyle, width: '3rem', maxWidth: '3rem' }} />
                      </div>
                    </Lsk>
                  </>
                )}
              </LskCol>
            </LskPage>
            {!s && (
              <p style={{ fontSize: '0.64rem', color: 'var(--text-dim)', margin: '0.5rem 0 0' }}>
                Solve CARP INIT 1/5 to pull sensed wind and temperature.
              </p>
            )}
          </Glass>
        )}

        {page === 'p4' && (
          <Glass corner={`CARP INIT 4/5 · CARP ${entry.id}`}>
            <LskPage>
              <LskCol>
                <Lsk tag="L4" label="RQD CLNC HT">
                  <LskInput value={entry.rqdClncHt} onChange={v => update({ rqdClncHt: v })} numeric />
                </Lsk>
                <Lsk tag="L5" label="MIN DROP HT">
                  <LskInput value={entry.minDropHt} onChange={v => update({ minDropHt: v })} numeric />
                </Lsk>
              </LskCol>
              <LskCol>
                <Lsk tag="R1" label="DROP ALT MSL"><LskValue dim={!s}>{s ? `${Math.round(s.drop_altitude_ft)} FT` : '—'}</LskValue></Lsk>
                <Lsk tag="R3" label="PI ELEV"><LskValue dim={!s}>{s ? `${Math.round(s.pi_elevation_ft)} FT` : '—'}</LskValue></Lsk>
                <Lsk tag="R4" label="OBSTR ELEV"><LskValue dim={!s}>{s ? `${Math.round(s.obstr_elevation_ft)} FT` : '—'}</LskValue></Lsk>
                <Lsk tag="R5" label="DZ ELEV"><LskValue dim={!s}>{s ? `${Math.round(s.dz_elevation_ft)} FT` : '—'}</LskValue></Lsk>
              </LskCol>
            </LskPage>
            {!s && (
              <p style={{ fontSize: '0.64rem', color: 'var(--text-dim)', margin: '0.5rem 0 0' }}>
                Solve CARP INIT 1/5 to pull PI/DZ/obstruction elevation.
              </p>
            )}
          </Glass>
        )}

        {page === 'prog' && (
          <Glass corner={`CARP PROG 2/2 · CARP ${entry.id}`}>
            <LskPage>
              <LskCol>
                <Lsk tag="L1" label="DRIFT">
                  <div style={{ display: 'flex', gap: '0.3rem', justifyContent: 'flex-end' }}>
                    <input value={entry.driftDeg} onChange={e => update({ driftDeg: e.target.value })} inputMode="decimal"
                      style={{ ...scratchInputStyle, width: '3.4rem', maxWidth: '3.4rem' }} />
                    <LskSelect value={entry.driftSide} onChange={v => update({ driftSide: v })} options={DRIFT_SIDE_OPTIONS} />
                  </div>
                </Lsk>
                <Lsk tag="L3" label="DROP RESULT DIR">
                  <LskInput value={entry.dropResultDir} onChange={v => update({ dropResultDir: v })} placeholder="rel run-in" />
                </Lsk>
                <Lsk tag="L4" label="DROP RESULT YD">
                  <LskInput value={entry.dropResultYd} onChange={v => update({ dropResultYd: v })} numeric />
                </Lsk>
                <Lsk tag="L5" label="PAYLOAD"><LskValue>{Math.round(totalWeight)} LB</LskValue></Lsk>
              </LskCol>
              <LskCol>
                <Lsk tag="R1" label="TOTAL DROPPED"><LskValue>{Math.round(entry.droppedLbs)} LB</LskValue></Lsk>
                <Lsk tag="R2" label="REMAINING"><LskValue>{Math.round(remaining)} LB</LskValue></Lsk>
                <Lsk tag="R5" label="CONFIRM">
                  <Button
                    tone="amber"
                    disabled={totalWeight <= 0 || remaining <= 0}
                    onClick={() => update({ droppedLbs: Math.min(totalWeight, entry.droppedLbs + (Number(entry.weightPer) || 0)) })}
                  >
                    CONFIRM DROP
                  </Button>
                </Lsk>
              </LskCol>
            </LskPage>
          </Glass>
        )}
      </div>
    </div>
  )
}
