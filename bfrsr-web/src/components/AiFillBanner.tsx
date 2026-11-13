// How much the AI is fighting for each side (DASHBOARD.md D4), from the HQ
// view's `ai_fill` (PvP with the AI filling in). The view is the viewer's own
// side, so it gives that side in full and only the other side's pilot count;
// an admin's two views give both in full. No `ai_fill` (the feature is off,
// the HQ is off, or an older engine): nothing is drawn.
import type { ReactElement } from 'react'
import { useQueries } from '@tanstack/react-query'
import { api, type HqView } from '../api'
import { useAuth } from '../context/AuthContext'
import { campaign } from '../config/campaign'

export type AiSide = 'Blue' | 'Red'

export interface AiFillLine {
  side: AiSide
  /** The side's name, upper case. */
  label: string
  /** The side's pilots in a slot. */
  pilots: number
  /** Only known for a side whose own view is in hand. */
  aiLed: boolean | null
  /** The AI's pressure (`ai_fill.pressure`) as a percentage, or null when
   *  this side's view is not in hand. */
  aiPct: number | null
  /** The whole line as text: "SOVIET — AI-LED · 0 PILOTS · AI AT 43%". */
  text: string
}

type ViewLike = Pick<HqView, 'side' | 'ai_fill'> | null | undefined

const pct = (x: number) => Math.round(Math.max(0, Math.min(1, Number.isFinite(x) ? x : 0)) * 100)
const pilotsText = (n: number) => `${n} PILOT${n === 1 ? '' : 'S'}`
const sideLabel = (s: AiSide) => (s === 'Blue' ? campaign.blueLabel : campaign.redLabel).toUpperCase()

/** One line per side the views say anything about, Blue first. */
// eslint-disable-next-line react-refresh/only-export-components
export function aiFillLines(views: ViewLike[]): AiFillLine[] {
  const own = new Map<AiSide, NonNullable<HqView['ai_fill']>>()
  for (const v of views) {
    if (v?.ai_fill && (v.side === 'Blue' || v.side === 'Red')) own.set(v.side, v.ai_fill)
  }
  if (own.size === 0) return []
  const lines: AiFillLine[] = []
  for (const side of ['Blue', 'Red'] as const) {
    const label = sideLabel(side)
    const f = own.get(side)
    if (f) {
      const aiPct = pct(f.pressure)
      const parts = [...(f.ai_led ? ['AI-LED'] : []), pilotsText(f.pilots), `AI AT ${aiPct}%`]
      lines.push({ side, label, pilots: f.pilots, aiLed: f.ai_led, aiPct, text: `${label} — ${parts.join(' · ')}` })
      continue
    }
    // Only the other side's view: all it tells about this side is its pilots.
    const other = own.get(side === 'Blue' ? 'Red' : 'Blue')
    if (other) {
      lines.push({ side, label, pilots: other.enemy_pilots, aiLed: null, aiPct: null, text: `${label} — ${pilotsText(other.enemy_pilots)}` })
    }
  }
  return lines
}

/** The lines as a status-board strip. `compact` is the home page's one-liner. */
export default function AiFillBanner({ lines, compact = false }: { lines: AiFillLine[]; compact?: boolean }): ReactElement | null {
  if (lines.length === 0) return null
  return (
    <div className={`ai-fill${compact ? ' ai-fill--compact' : ''}`} role="group" aria-label="AI share by side">
      {lines.map((l) => (
        <div key={l.side} className="ai-fill-line" aria-label={l.text}>
          <span className={`rsr-side-tag rsr-side-tag--${l.side.toLowerCase()} ai-fill-tag`}>{l.label}</span>
          {l.aiLed && <span className="ai-fill-led">AI-LED</span>}
          <span className="ai-fill-item">
            <span className="rsr-fig ai-fill-num">{l.pilots}</span> PILOT{l.pilots === 1 ? '' : 'S'}
          </span>
          {l.aiPct != null && (
            <span className="ai-fill-item">
              AI AT <span className="rsr-fig ai-fill-num">{l.aiPct}%</span>
            </span>
          )}
        </div>
      ))}
    </div>
  )
}

/** The HQ views the viewer may see, for the banner: their own side's, or both
 *  for an admin with no side. Signed out (or no side): none, and no request. */
// eslint-disable-next-line react-refresh/only-export-components
export function useAiFillLines(): AiFillLine[] {
  const { user } = useAuth()
  // Dev builds only: `?mock` uses the HQ page's fixture (keep the guard literal).
  const mock = import.meta.env.DEV && new URLSearchParams(location.search).has('mock')
  const sides: (AiSide | undefined)[] = mock
    ? [undefined]
    : !user
      ? []
      : user.is_admin && !user.side
        ? ['Blue', 'Red']
        : user.side ? [undefined] : []
  const views = useQueries({
    queries: sides.map((s) => ({
      // The HQ page's cache key, so the two share a fetch.
      queryKey: ['hq', s],
      queryFn: (): Promise<HqView> =>
        import.meta.env.DEV && mock ? import('../pages/hqMock').then((m) => m.hqMock) : api.hq.view(s),
      retry: false,
      refetchInterval: 30_000,
    })),
  })
  return aiFillLines(views.map((q) => q.data))
}
