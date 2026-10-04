/**
 * The ground war's symbols, drawn again for the wiki: the dashboard's GROUND
 * COMMAND and THEATRE HQ maps (`gc-*`, `hq-*`) and what the ground war puts on
 * the F10 map and in the world (`f10-*`, `world-*`).
 *
 * Copied by eye from `bfweb/src/pages/groundwar/{theme,sprites,markers,engine}.ts`
 * and `bfweb/src/pages/HqPage.tsx`, so the colours are the dashboard's (its
 * ground screen keeps its own red/blue, not the F10 map's violet/azure). If
 * those change, change these.
 */
import type { ReactElement, ReactNode } from 'react'

type Side = 'Blue' | 'Red' | 'Neutral'

const GC_SIDE = { Blue: '#4a8fd4', Red: '#cc4444', Neutral: '#8a8f80' } as const
const GC_BRIGHT = { Blue: '#7db8ff', Red: '#ff6b5e', Neutral: '#b9bca9' } as const
const PENCIL = '#ffd23f'
const FIRE = '#ff8c1a'
const ATTACK = '#ff5b45'
const WITHDRAW = '#f2c14e'
const GOOD = '#8ec83f'
const AMBER = '#e8c547'
const BONE = '#e6e1cf'
const BROKEN = '#8a8a80'
const GROUND = '#1f251d'
const MONO = 'ui-monospace, SFMono-Regular, Menlo, Consolas, monospace'
// The engine's F10 palette (bflib/src/mapcolor.rs), for the in-game marks.
const F10_SIDE = { Blue: '#40a6ff', Red: '#b84dff', Neutral: '#ffffff' } as const
const BATTLE = '#ff8c1a'

const other = (s: Side): Side => (s === 'Red' ? 'Blue' : 'Red')

function shade(hex: string, f: number): string {
  const n = parseInt(hex.slice(1), 16)
  const ch = (v: number) => Math.max(0, Math.min(255, Math.round(f < 0 ? v * (1 + f) : v + (255 - v) * f)))
  return `rgb(${ch((n >> 16) & 255)},${ch((n >> 8) & 255)},${ch(n & 255)})`
}

/** The dashboard is dark imagery; its symbols are drawn on a dark tile so they read as they do there. */
function Tile({ w = 48, children }: { w?: number; children: ReactNode }) {
  return (
    <svg width={w} height={48} viewBox={`0 0 ${w} 48`} aria-hidden>
      <rect width={w} height={48} rx="4" fill={GROUND} />
      {children}
    </svg>
  )
}

// ── NATO frames (what milsymbol draws on the dashboard) ──────────────────

type Kind = 'armour' | 'mechanised' | 'motorised' | 'infantry'

/** A friendly land unit: rectangle frame, the kind's icon in black, company bar on top. */
function Friend({ kind, fill, x = 24, y = 25, s = 1, dashed = false, company = true }: {
  kind: Kind; fill: string; x?: number; y?: number; s?: number; dashed?: boolean; company?: boolean
}) {
  const w = 30 * s
  const h = 20 * s
  const l = x - w / 2
  const t = y - h / 2
  const ink = { stroke: '#000', strokeWidth: 1.4 * s, fill: 'none' }
  return (
    <g>
      <rect x={l} y={t} width={w} height={h} fill={fill} fillOpacity={dashed ? 0.35 : 1} stroke="#000"
        strokeWidth={1.6 * s} strokeDasharray={dashed ? `${3 * s} ${2 * s}` : undefined} />
      {(kind === 'infantry' || kind === 'mechanised' || kind === 'motorised') && (
        <g {...ink}>
          <line x1={l} y1={t} x2={l + w} y2={t + h} />
          <line x1={l + w} y1={t} x2={l} y2={t + h} />
        </g>
      )}
      {(kind === 'armour' || kind === 'mechanised') && (
        <ellipse cx={x} cy={y} rx={w * 0.32} ry={h * 0.26} {...ink} />
      )}
      {kind === 'motorised' && <line x1={x} y1={t} x2={x} y2={t + h} {...ink} />}
      {company && <line x1={x} y1={t - 6 * s} x2={x} y2={t - 1.5 * s} stroke="#000" strokeWidth={2 * s} />}
    </g>
  )
}

/** A hostile land unit: diamond frame. */
function Hostile({ kind = 'armour', fill, x = 24, y = 24, r = 15, ghost = false }: {
  kind?: Kind; fill: string; x?: number; y?: number; r?: number; ghost?: boolean
}) {
  const ink = { stroke: '#000', strokeWidth: 1.4, fill: 'none' }
  return (
    <g>
      <polygon points={`${x},${y - r} ${x + r},${y} ${x},${y + r} ${x - r},${y}`} fill={fill} fillOpacity={ghost ? 0.35 : 1}
        stroke={ghost ? BONE : '#000'} strokeWidth="1.6" strokeDasharray={ghost ? '3 2' : undefined} />
      {(kind === 'armour' || kind === 'mechanised') && <ellipse cx={x} cy={y} rx={r * 0.5} ry={r * 0.27} {...ink} />}
      {(kind === 'infantry' || kind === 'mechanised') && (
        <g {...ink}>
          <line x1={x - r / 2} y1={y - r / 2} x2={x + r / 2} y2={y + r / 2} />
          <line x1={x + r / 2} y1={y - r / 2} x2={x - r / 2} y2={y + r / 2} />
        </g>
      )}
    </g>
  )
}

function Tag({ x, y, text, color, bg = 'rgba(0,0,0,0.75)', ink, dashed = false, size = 6.5 }: {
  x: number; y: number; text: string; color: string; bg?: string; ink?: string; dashed?: boolean; size?: number
}) {
  const w = text.length * size * 0.62 + 5
  return (
    <g>
      <rect x={x - w / 2} y={y - size * 0.8} width={w} height={size * 1.6} fill={bg} stroke={color}
        strokeWidth="0.8" strokeDasharray={dashed ? '2 1.5' : undefined} />
      <text x={x} y={y + size * 0.36} textAnchor="middle" fontFamily={MONO} fontSize={size} fontWeight="700"
        letterSpacing="0.04em" fill={ink ?? color}>{text}</text>
    </g>
  )
}

// ── Vehicle silhouettes (sprites.ts drawVehicle, top-down, nose up) ──────

type Role = 'tank' | 'ifv' | 'apc' | 'recon' | 'truck' | 'aaa' | 'sam' | 'artillery' | 'infantry'

function Vehicle({ role, body }: { role: Role; body: string }) {
  const dark = shade(body, -0.5)
  const deck = shade(body, 0.18)
  const R = (x0: number, y0: number, x1: number, y1: number, fill: string, k?: string) => (
    <rect key={k} x={x0} y={y0} width={x1 - x0} height={y1 - y0} fill={fill} />
  )
  const D = (x: number, y: number, r: number, fill: string, k?: string) => <circle key={k} cx={x} cy={y} r={r} fill={fill} />
  let parts: ReactElement[] = []
  switch (role) {
    case 'tank':
      parts = [R(8.5, 7, 11, 27, dark, 'a'), R(21, 7, 23.5, 27, dark, 'b'), R(11, 7.5, 21, 26.5, body, 'c'), D(16, 18, 4.4, deck, 'd'), R(15.25, 1, 16.75, 14, dark, 'e')]
      break
    case 'ifv':
      parts = [R(9.5, 8, 11.5, 26, dark, 'a'), R(20.5, 8, 22.5, 26, dark, 'b'), R(11.5, 8, 20.5, 26, body, 'c'), R(13.5, 13, 18.5, 18.5, deck, 'd'), R(15.4, 4.5, 16.6, 13, dark, 'e')]
      break
    case 'apc':
      parts = [
        <polygon key="h" points="12,6.5 20,6.5 21.5,10 21.5,26.5 10.5,26.5 10.5,10" fill={body} />,
        R(13.5, 16, 18.5, 21, deck, 'd'),
        ...[10, 15.5, 21].flatMap((y) => [R(9, y, 10.5, y + 3, dark, `l${y}`), R(21.5, y, 23, y + 3, dark, `r${y}`)]),
      ]
      break
    case 'recon':
      parts = [
        R(11.5, 9, 20.5, 25, body, 'b'),
        ...[10.5, 20.5].flatMap((y) => [R(10, y, 11.5, y + 3, dark, `l${y}`), R(20.5, y, 22, y + 3, dark, `r${y}`)]),
        D(16, 16, 2.8, deck, 't'), R(15.5, 6, 16.5, 14, dark, 'g'),
      ]
      break
    case 'truck':
      parts = [
        R(11.5, 5.5, 20.5, 11, deck, 'c'), R(11, 12.5, 21, 27.5, body, 'b'),
        <path key="s" d="M11.5 16.5H20.5M11.5 20.5H20.5M11.5 24.5H20.5" stroke="rgba(0,0,0,0.4)" strokeWidth="0.8" />,
      ]
      break
    case 'aaa':
      parts = [
        R(9.5, 8, 11.5, 26, dark, 'a'), R(20.5, 8, 22.5, 26, dark, 'b'), R(11.5, 8, 20.5, 26, body, 'c'),
        R(12.5, 12.5, 19.5, 19.5, deck, 'd'), R(13.2, 2, 14.4, 13, dark, 'g1'), R(17.6, 2, 18.8, 13, dark, 'g2'),
        <path key="r" d="M12.9 22.9 A3.4 3.4 0 0 1 19.1 22.9" fill="none" stroke={deck} strokeWidth="1.4" />,
      ]
      break
    case 'sam':
      parts = [
        R(9.5, 6.5, 11.5, 27, dark, 'a'), R(20.5, 6.5, 22.5, 27, dark, 'b'), R(11.5, 6.5, 20.5, 27, body, 'c'), R(12.5, 11, 19.5, 24, deck, 'd'),
        ...([[14.2, 14], [17.8, 14], [14.2, 20.5], [17.8, 20.5]] as const).map(([x, y]) => D(x, y, 1.5, dark, `m${x}${y}`)),
      ]
      break
    case 'artillery':
      parts = [R(9.5, 10, 11.5, 28, dark, 'a'), R(20.5, 10, 22.5, 28, dark, 'b'), R(11.5, 10, 20.5, 28, body, 'c'), R(12.5, 14, 19.5, 23, deck, 'd'), R(15.2, 0.5, 16.8, 15, dark, 'e')]
      break
    case 'infantry':
      parts = ([[16, 10], [11.5, 16], [20.5, 16], [16, 22]] as const).map(([x, y]) => D(x, y, 2.4, body, `p${x}${y}`))
      break
  }
  return (
    <g transform="translate(1.6 2.4) scale(1.4)" stroke="rgba(0,0,0,0.85)" strokeWidth="0.6"
      style={{ filter: 'drop-shadow(0 0 1px rgba(0,0,0,0.8))' }}>
      {parts}
    </g>
  )
}

const PLANE = 'M12 1.2 13.3 6.8 21.8 12.6v1.8l-8.5-2.4-.4 6.2 3.1 2.4v1.5L12 21.2 8 22.1v-1.5l3.1-2.4-.4-6.2-8.5 2.4v-1.8l8.5-5.8z'
const HELO = 'M12 4.6c1.9 0 3.1 1.7 3.1 4.3 0 2.2-1 3.6-2.3 4.1v6.6h2.4v1.4H8.8v-1.4h2.4V13C9.9 12.5 8.9 11.1 8.9 8.9c0-2.6 1.2-4.3 3.1-4.3z'

/** A base on the Ground Command map: the six-sided plate, coloured by owner. */
function BasePlate({ col, x = 24, y = 17 }: { col: string; x?: number; y?: number }) {
  const hex = (r: number) => {
    const w = r
    return `${x - w / 2},${y - r} ${x + w / 2},${y - r} ${x + r},${y} ${x + w / 2},${y + r} ${x - w / 2},${y + r} ${x - r},${y}`
  }
  return (
    <g>
      <polygon points={hex(12)} fill={col} />
      <polygon points={hex(10)} fill="rgba(10,12,10,0.92)" />
      {/* a runway: the airbase glyph, standing in for every kind of base */}
      <g stroke={shade(col, 0.45)} strokeWidth="1.6" strokeLinecap="round">
        <line x1={x - 5} y1={y + 4} x2={x + 5} y2={y - 4} />
        <line x1={x - 2} y1={y - 3} x2={x + 2} y2={y + 1} strokeWidth="1" />
      </g>
    </g>
  )
}

function HqDiamond({ col, x = 24, y = 22, halo }: { col: string; x?: number; y?: number; halo?: string }) {
  return (
    <g>
      {halo && <rect x={x - 6.5} y={y - 6.5} width="13" height="13" transform={`rotate(45 ${x} ${y})`} fill="none" stroke={halo} strokeWidth="2" />}
      <rect x={x - 4.5} y={y - 4.5} width="9" height="9" transform={`rotate(45 ${x} ${y})`} fill={col} stroke="#000" strokeWidth="1" />
    </g>
  )
}

/** The ground war's glyphs; anything else is a plain dot, as `MapSymbol` draws for an unknown icon. */
export default function GroundGlyph({ icon, side = 'Blue' }: { icon: string; side?: Side }): ReactElement {
  const own = GC_SIDE[side] ?? GC_SIDE.Blue
  const ownBright = GC_BRIGHT[side] ?? GC_BRIGHT.Blue
  const foe = GC_SIDE[other(side)]

  if (icon === 'gc-v-broken') return <Tile><Vehicle role="tank" body="#9a9a90" /></Tile>
  if (icon.startsWith('gc-v-')) {
    const role = icon.slice(5) as Role
    return <Tile><Vehicle role={role} body={side === 'Neutral' ? '#9a9a90' : ownBright} /></Tile>
  }

  switch (icon) {
    // ---- our formations ------------------------------------------------
    case 'gc-armour':
    case 'gc-mechanised':
    case 'gc-motorised':
    case 'gc-infantry':
      return <Tile><Friend kind={icon.slice(3) as Kind} fill={own} /></Tile>
    case 'gc-moving':
      return (
        <Tile>
          <Friend kind="mechanised" fill={own} y={20} />
          <g stroke="#000" strokeWidth="1.6" fill="none">
            <line x1="24" y1="30" x2="24" y2="38" />
            <line x1="24" y1="38" x2="38" y2="38" />
            <polyline points="34,35 38,38 34,41" />
          </g>
        </Tile>
      )
    case 'gc-reduced':
      return (
        <Tile>
          <Friend kind="armour" fill={own} x={22} />
          <text x="42" y="14" textAnchor="middle" fontFamily={MONO} fontSize="7" fontWeight="700" fill={BONE}
            stroke="#000" strokeWidth="2" paintOrder="stroke">(-)</text>
        </Tile>
      )
    case 'gc-strength':
      return (
        <Tile>
          {[[6, GOOD, 0.9], [11, AMBER, 0.55], [16, ATTACK, 0.25]].map(([x, c, p]) => (
            <g key={x as number}>
              <rect x={x as number} y="15" width="3.5" height="20" fill="rgba(0,0,0,0.65)" stroke="#000" strokeWidth="0.6" />
              <rect x={x as number} y={15 + 20 * (1 - (p as number))} width="3.5" height={20 * (p as number)} fill={c as string} />
            </g>
          ))}
          <Friend kind="mechanised" fill={own} x={33} s={0.8} />
        </Tile>
      )
    case 'gc-selected':
      return (
        <Tile>
          <circle cx="24" cy="25" r="21" fill="none" stroke={PENCIL} strokeOpacity="0.45" strokeWidth="1" strokeDasharray="3 5" />
          <Friend kind="armour" fill={own} />
          <g stroke={PENCIL} strokeWidth="2" fill="none">
            <polyline points="6,19 6,12 13,12" />
            <polyline points="35,12 42,12 42,19" />
            <polyline points="6,32 6,39 13,39" />
            <polyline points="35,39 42,39 42,32" />
          </g>
        </Tile>
      )
    case 'gc-group':
      return (
        <Tile>
          <Friend kind="infantry" fill={own} x={20} />
          <rect x="36" y="27" width="9" height="11" fill={PENCIL} />
          <text x="40.5" y="35.5" textAnchor="middle" fontFamily={MONO} fontSize="8.5" fontWeight="700" fill="#0b0d0a">1</text>
        </Tile>
      )
    case 'gc-flag-broken':
      return (
        <Tile w={60}>
          <Friend kind="armour" fill={BROKEN} x={22} y={19} />
          <Tag x={30} y={39} text="BROKEN" color={ATTACK} />
        </Tile>
      )
    case 'gc-flag-nosupply':
      return <Tile w={60}><Friend kind="mechanised" fill={own} x={30} y={19} /><Tag x={30} y={39} text="NO SUPPLY" color={AMBER} /></Tile>
    case 'gc-flag-halted':
      return <Tile w={60}><Friend kind="mechanised" fill={own} x={30} y={19} /><Tag x={30} y={39} text="HALTED" color={AMBER} /></Tile>
    case 'gc-flag-sim':
      return <Tile w={60}><Friend kind="motorised" fill={own} x={30} y={19} /><Tag x={30} y={39} text="SIM" color="#9a9a90" /></Tile>

    // ---- the enemy -----------------------------------------------------
    case 'gc-hostile':
      return (
        <Tile w={60}>
          <Hostile fill={foe} x={30} y={19} r={13} />
          <Tag x={30} y={40} text="~12 VEH" color="#000" bg="rgba(0,0,0,0.66)" ink="#ffc1b8" />
        </Tile>
      )
    case 'gc-ghost':
      return (
        <Tile w={60}>
          <g opacity="0.6"><Hostile fill={foe} x={30} y={19} r={13} ghost /></g>
          <Tag x={30} y={40} text="LAST SEEN 6M" color="rgba(230,225,207,0.4)" ink="#cfcab8" dashed size={5.6} />
        </Tile>
      )
    case 'gc-fog':
      return (
        <svg width="48" height="48" viewBox="0 0 48 48" aria-hidden>
          <defs>
            <radialGradient id="gcFogHole">
              <stop offset="0.55" stopColor="#000" />
              <stop offset="1" stopColor="#fff" />
            </radialGradient>
            <mask id="gcFogMask">
              <rect width="48" height="48" fill="#fff" />
              <circle cx="20" cy="26" r="16" fill="url(#gcFogHole)" />
            </mask>
          </defs>
          <rect width="48" height="48" rx="4" fill="#4b5a3e" />
          <path d="M0 34 Q14 28 24 31 T48 22" stroke="#6b6650" strokeWidth="2" fill="none" />
          <rect width="48" height="48" rx="4" fill="#0a0c0a" fillOpacity="0.72" mask="url(#gcFogMask)" />
          <Friend kind="armour" fill={own} x={20} y={27} s={0.55} company={false} />
        </svg>
      )

    // ---- movement --------------------------------------------------------
    case 'gc-route-attack':
    case 'gc-route-move':
    case 'gc-route-withdraw': {
      const kind = icon.slice(9)
      const col = kind === 'attack' ? ATTACK : kind === 'withdraw' ? WITHDRAW : own
      return (
        <Tile>
          <line x1="8" y1="38" x2="32" y2="16" stroke={col} strokeOpacity="0.2" strokeWidth="4" strokeLinecap="round" />
          <line x1="8" y1="38" x2="32" y2="16" stroke={col} strokeOpacity="0.8" strokeWidth="1.6" strokeDasharray="0 4 3" />
          <Friend kind="armour" fill={own} x={9} y={38} s={0.4} company={false} />
          <rect x="28" y="7" width="16" height="13" fill="rgba(0,0,0,0.62)" stroke={col} strokeWidth="1" />
          <g stroke={col} strokeWidth="1.8" fill="none" strokeLinecap="square">
            {kind === 'attack' && <path d="M31 10 34.5 13.5 31 17M36 10l3.5 3.5L36 17" />}
            {kind === 'withdraw' && <path d="M41 13.5H32M35 10 31.5 13.5 35 17" />}
            {kind === 'move' && <path d="M36 9.5 40.5 11v3c0 2.5-2 4-4.5 5-2.5-1-4.5-2.5-4.5-5v-3z" strokeWidth="1.4" />}
          </g>
        </Tile>
      )
    }
    case 'gc-trail':
      return (
        <Tile>
          <defs>
            <linearGradient id="gcTrail" x1="0" y1="1" x2="1" y2="0">
              <stop offset="0" stopColor={ownBright} stopOpacity="0" />
              <stop offset="0.7" stopColor={ownBright} stopOpacity="0.35" />
              <stop offset="1" stopColor={ownBright} stopOpacity="0.7" />
            </linearGradient>
          </defs>
          <path d="M4 44 Q16 30 26 28 T38 12" stroke="url(#gcTrail)" strokeWidth="1.2" fill="none" transform="translate(-1.5 0)" />
          <path d="M4 44 Q16 30 26 28 T38 12" stroke="url(#gcTrail)" strokeWidth="1.2" fill="none" transform="translate(1.5 0)" />
          <g transform="translate(38 11) rotate(25) translate(-8 -8) scale(0.5)"><Vehicle role="tank" body={ownBright} /></g>
        </Tile>
      )
    case 'gc-front':
      return (
        <Tile>
          <path d="M6 10 Q14 22 10 40" stroke={GC_SIDE.Blue} strokeOpacity="0.7" strokeWidth="1.2" strokeDasharray="3 2" fill="none" />
          <path d="M24 6 Q30 24 22 44" stroke={BONE} strokeOpacity="0.7" strokeWidth="1.9" strokeDasharray="3 2" fill="none" />
          <path d="M40 8 Q46 24 36 42" stroke={GC_SIDE.Red} strokeOpacity="0.7" strokeWidth="1.2" strokeDasharray="3 2" fill="none" />
        </Tile>
      )

    // ---- bases -----------------------------------------------------------
    case 'gc-base':
      return (
        <Tile>
          <BasePlate col={own} />
          <rect x="35" y="22" width="9" height="8" fill={GOOD} />
          <text x="39.5" y="28.5" textAnchor="middle" fontFamily={MONO} fontSize="6.5" fontWeight="700" fill="#071003">+2</text>
          <rect x="9" y="34" width="30" height="3" fill="rgba(0,0,0,0.75)" />
          <rect x="9" y="34" width="20" height="3" fill={GOOD} />
          {[0, 1, 2, 3, 4].map((i) => <rect key={i} x={15.5 + i * 4} y="40" width="3" height="3" fill={BONE} stroke="rgba(0,0,0,0.7)" strokeWidth="0.6" />)}
        </Tile>
      )
    case 'gc-base-threat':
      return (
        <Tile>
          <BasePlate col={own} y={22} />
          <circle cx="36" cy="11" r="5.5" fill={ATTACK} />
          <text x="36" y="14" textAnchor="middle" fontFamily={MONO} fontSize="8" fontWeight="700" fill="#000">!</text>
        </Tile>
      )
    case 'gc-base-capturing':
      return (
        <Tile>
          <circle cx="24" cy="23" r="19" fill="none" stroke={ATTACK} strokeWidth="1" strokeOpacity="0.35" />
          <circle cx="24" cy="23" r="15.5" fill="none" stroke={ATTACK} strokeWidth="2" />
          <BasePlate col={foe} y={23} />
        </Tile>
      )
    case 'gc-base-enemy':
      return <Tile><BasePlate col={foe} y={22} /></Tile>

    // ---- fighting --------------------------------------------------------
    case 'gc-battle':
      return (
        <Tile>
          <defs>
            <radialGradient id="gcBattle">
              <stop offset="0" stopColor={FIRE} stopOpacity="0.3" />
              <stop offset="0.6" stopColor={FIRE} stopOpacity="0.1" />
              <stop offset="1" stopColor={FIRE} stopOpacity="0" />
            </radialGradient>
          </defs>
          <circle cx="24" cy="28" r="18" fill="url(#gcBattle)" />
          <circle cx="24" cy="28" r="14" fill="none" stroke={FIRE} strokeOpacity="0.55" strokeWidth="1.2" />
          <circle cx="24" cy="28" r="18" fill="none" stroke={FIRE} strokeOpacity="0.25" strokeWidth="1" />
          {[[19, 25], [28, 31], [25, 22]].map(([x, y]) => (
            <g key={x}><circle cx={x} cy={y} r="2.6" fill="#ffd59a" opacity="0.8" /><circle cx={x} cy={y} r="1.2" fill="#fff" /></g>
          ))}
          <rect x="7" y="3" width="34" height="9" fill="rgba(0,0,0,0.6)" />
          <text x="24" y="10" textAnchor="middle" fontFamily={MONO} fontSize="6.5" letterSpacing="0.1em" fill="#ffb766">⚔ GORI</text>
        </Tile>
      )
    case 'gc-fireline':
      return (
        <Tile>
          <Friend kind="armour" fill={own} x={11} y={34} s={0.42} company={false} />
          <Hostile fill={foe} x={38} y={14} r={6} />
          <line x1="15" y1="31" x2="33" y2="17" stroke={GC_BRIGHT[side]} strokeWidth="1.3" strokeDasharray="4 5" />
          <line x1="34" y1="18" x2="16" y2="32" stroke={GC_BRIGHT[other(side)]} strokeWidth="1.3" strokeDasharray="3 6" strokeDashoffset="3" />
        </Tile>
      )
    case 'gc-shellfire':
      return (
        <Tile>
          {[[16, 30, 9], [30, 20, 11], [32, 34, 7]].map(([x, y, r]) => (
            <circle key={x + y} cx={x} cy={y} r={r} fill="#8b8a80" fillOpacity="0.28" />
          ))}
          <circle cx="22" cy="34" r="3" fill="#000" fillOpacity="0.55" />
          <circle cx="28" cy="25" r="5" fill={FIRE} fillOpacity="0.4" />
          <circle cx="28" cy="25" r="2.4" fill="#ffe2b0" />
          <circle cx="28" cy="25" r="8.5" fill="none" stroke={FIRE} strokeOpacity="0.5" strokeWidth="1" />
        </Tile>
      )

    // ---- players ---------------------------------------------------------
    case 'gc-player':
      return (
        <Tile>
          <g transform="translate(8 8) scale(1.35)"><path d={PLANE} fill={ownBright} stroke="rgba(0,0,0,0.85)" strokeWidth="1.1" strokeLinejoin="round" /></g>
        </Tile>
      )
    case 'gc-player-helo':
      return (
        <Tile>
          <g transform="translate(8 8) scale(1.35)">
            <circle cx="12" cy="9" r="8.4" fill="none" stroke="rgba(255,255,255,0.45)" strokeWidth="0.8" strokeDasharray="2 1.6" />
            <path d={HELO} fill={ownBright} stroke="rgba(0,0,0,0.85)" strokeWidth="1.1" strokeLinejoin="round" />
          </g>
        </Tile>
      )
    case 'gc-you':
      return (
        <Tile>
          <circle cx="24" cy="24" r="20" fill="none" stroke={PENCIL} strokeWidth="1.2" opacity="0.35" />
          <circle cx="24" cy="24" r="15" fill="none" stroke={PENCIL} strokeWidth="1.5" />
          <g transform="translate(13 13) scale(0.92)"><path d={PLANE} fill={PENCIL} stroke="rgba(0,0,0,0.85)" strokeWidth="1.1" strokeLinejoin="round" /></g>
        </Tile>
      )

    // ---- the THEATRE HQ map ---------------------------------------------
    case 'hq-own':
      return <Tile><HqDiamond col={own} /></Tile>
    case 'hq-enemy':
      return <Tile><HqDiamond col={foe} /></Tile>
    case 'hq-neutral':
      return <Tile><HqDiamond col={GC_SIDE.Neutral} /></Tile>
    case 'hq-effort':
      return (
        <Tile w={60}>
          <circle cx="30" cy="18" r="12" fill="none" stroke={FIRE} strokeWidth="2" />
          <HqDiamond col={foe} x={30} y={18} />
          <text x="30" y="40" textAnchor="middle" fontFamily={MONO} fontSize="6.5" fontWeight="700" fill={FIRE}>MAIN EFFORT</text>
        </Tile>
      )
    case 'hq-hold':
      return (
        <Tile>
          <HqDiamond col={own} y={18} halo={own} />
          <text x="24" y="40" textAnchor="middle" fontFamily={MONO} fontSize="6.5" fontWeight="700" fill={own}>HOLD</text>
        </Tile>
      )
    case 'hq-capturable':
      return <Tile><HqDiamond col={foe} halo={FIRE} /></Tile>
    case 'hq-sam':
      return (
        <Tile>
          <rect x="13" y="18" width="22" height="12" rx="2" fill="rgba(0,0,0,0.55)" stroke={foe} strokeWidth="1" />
          <text x="24" y="27" textAnchor="middle" fontFamily={MONO} fontSize="7.5" fill={foe}>SAM</text>
        </Tile>
      )
    case 'hq-op':
      return (
        <Tile w={60}>
          <rect x="16" y="6" width="28" height="10" rx="2" fill={own} />
          <text x="30" y="13.5" textAnchor="middle" fontFamily={MONO} fontSize="6.5" fontWeight="700" fill="#000">BOMBER</text>
          <rect x="16" y="17" width="28" height="10" rx="2" fill={own} />
          <text x="30" y="24.5" textAnchor="middle" fontFamily={MONO} fontSize="6.5" fontWeight="700" fill="#000">CAP</text>
          <HqDiamond col={foe} x={30} y={38} />
        </Tile>
      )

    // ---- the F10 map and the world ------------------------------------------
    case 'f10-battle':
      return (
        <svg width="60" height="48" viewBox="0 0 60 48" aria-hidden>
          <circle cx="24" cy="19" r="15" fill={BATTLE} fillOpacity="0.12" stroke={BATTLE} strokeOpacity="0.9" strokeWidth="2" strokeDasharray="5 4" />
          <rect x="7" y="36" width="52" height="10" fill="rgba(20,22,26,0.85)" />
          <text x="33" y="43.3" textAnchor="middle" fontFamily={MONO} fontSize="5.6" fontWeight="700" fill={BATTLE}>GROUND BATTLE</text>
        </svg>
      )
    case 'f10-pin': {
      const c = F10_SIDE[side] ?? F10_SIDE.Blue
      return (
        <svg width="60" height="48" viewBox="0 0 60 48" aria-hidden>
          <path d="M10 40 L10 10" stroke="#d8d8d8" strokeWidth="1.5" />
          <path d="M10 10 h10 l-3 4 l3 4 h-10z" fill={c} stroke="#000" strokeWidth="0.8" />
          <rect x="22" y="22" width="37" height="18" fill="rgba(255,255,255,0.9)" stroke="#555" strokeWidth="0.6" />
          <text x="24" y="29.5" fontFamily={MONO} fontSize="5" fontWeight="700" fill="#111">1st Mech Coy</text>
          <text x="24" y="36.5" fontFamily={MONO} fontSize="4.5" fill="#333">attack · ~75%</text>
        </svg>
      )
    }
    case 'f10-attack-arrow': {
      const c = F10_SIDE[side] ?? F10_SIDE.Blue
      return (
        <svg width="48" height="48" viewBox="0 0 48 48" aria-hidden>
          <polygon points="6,30 30,30 30,22 44,34 30,46 30,38 6,38" transform="translate(0 -10)" fill={c} fillOpacity="0.35" stroke={c} strokeOpacity="0.85" strokeWidth="2" strokeLinejoin="round" />
        </svg>
      )
    }
    case 'world-smoke':
      return (
        <svg width="48" height="48" viewBox="0 0 48 48" aria-hidden>
          {[[24, 34, 6, 0.5], [21, 25, 7, 0.4], [26, 16, 8, 0.32], [21, 8, 8, 0.22]].map(([x, y, r, o]) => (
            <circle key={y} cx={x} cy={y} r={r} fill="#6b6b66" fillOpacity={o} />
          ))}
          <path d="M18 44 Q20 36 24 38 Q26 33 30 44z" fill={FIRE} />
          <path d="M21 44 Q23 40 24 41 Q26 39 27 44z" fill="#ffd36b" />
        </svg>
      )
  }
  return <svg width="48" height="48" viewBox="0 0 48 48" aria-hidden><circle cx="24" cy="24" r="4" fill="rgba(255,255,255,0.55)" /></svg>
}
