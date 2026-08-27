interface Props { value: number; showLabel?: boolean; height?: number }

function barColor(v: number) {
  if (v >= 75) return { solid: '#22c55e', glow: '#22c55e40' }
  if (v >= 40) return { solid: '#eab308', glow: '#eab30840' }
  return { solid: '#ef4444', glow: '#ef444440' }
}

export default function HealthBar({ value, showLabel = true, height = 6 }: Props) {
  const { solid, glow } = barColor(value)
  const pct = Math.min(Math.max(value, 0), 100)
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
            boxShadow: pct < 40 ? `0 0 6px ${glow}` : 'none',
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
