import { useDashLook } from '../lib/look'
import '../pages/levant/objectives.css'

interface Props { value: number; showLabel?: boolean; height?: number }

// Green / yellow / red by value so the bars actually read at a glance.
function barColor(v: number) {
  if (v > 66) return { solid: '#22c55e', glow: '#22c55e40' } // green
  if (v > 33) return { solid: '#eab308', glow: '#eab30840' } // yellow
  return { solid: '#ef4444', glow: '#ef444440' }             // red
}

/** The same three bands in Levant Ablaze's words (objectives.css). */
const band = (v: number) => (v > 66 ? 'ok' : v > 33 ? 'mid' : 'low')

export default function HealthBar({ value, showLabel = true, height = 6 }: Props) {
  const look = useDashLook()
  const pct = Math.min(Math.max(value, 0), 100)
  // Levant Ablaze: a flat strip on charred ground, sand when healthy, ochre
  // then red as it drains, the figure in Oswald beside it. No glow.
  if (look.profile) {
    return (
      <div className={`ob-hb ${band(value)}`}>
        <div className="ob-hb-track" style={{ height: Math.max(4, height - 1) }}><i style={{ width: `${pct}%` }} /></div>
        {showLabel && <b>{value}%</b>}
      </div>
    )
  }
  const { solid, glow } = barColor(value)
  return (
    <div className="flex items-center gap-2">
      <div
        className="flex-1 rounded-full overflow-hidden min-w-[48px]"
        style={{ height, background: 'var(--bg-hover)' }}
      >
        <div
          className="h-full rounded-full health-fill"
          style={{
            width: `${pct}%`,
            background: `linear-gradient(90deg, ${solid}cc, ${solid})`,
            boxShadow: pct <= 33 ? `0 0 6px ${glow}` : 'none',
          }}
        />
      </div>
      {showLabel && (
        <span className="text-[10px] font-mono tabular-nums w-8 text-right font-semibold" style={{ color: solid }}>
          {value}%
        </span>
      )}
    </div>
  )
}
