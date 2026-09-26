import type { ReactNode } from 'react'
import { DAY_OPTIONS } from '../lib/days'


/** Days filter; 0 = all time. */
export function DaysSeg({ value, onChange, options = DAY_OPTIONS }: { value: number; onChange: (d: number) => void; options?: readonly number[] }) {
  return (
    <div className="seg" role="group" aria-label="Time window">
      {options.map(d => (
        <button key={d} aria-pressed={value === d} onClick={() => onChange(d)}>
          {d === 0 ? 'All' : `${d}d`}
        </button>
      ))}
    </div>
  )
}

export function Select<T extends string>({
  label,
  value,
  onChange,
  options,
  className,
}: {
  label: string
  value: T
  onChange: (v: T) => void
  options: { value: T; label: string }[]
  className?: string
}) {
  return (
    <label className={`field ${className ?? ''}`}>
      <span>{label}</span>
      <select className="select-range" value={value} onChange={e => onChange(e.target.value as T)}>
        {options.map(o => (
          <option key={o.value} value={o.value}>
            {o.label}
          </option>
        ))}
      </select>
    </label>
  )
}

export function NumberField({
  label,
  value,
  onChange,
  unit,
  step = 1,
  min,
  max,
  hint,
}: {
  label: string
  value: number
  onChange: (v: number) => void
  unit?: string
  step?: number
  min?: number
  max?: number
  hint?: ReactNode
}) {
  return (
    <label className="field">
      <span>
        {label}
        {unit ? <span className="dim"> · {unit}</span> : null}
      </span>
      <input
        className="input-range"
        type="number"
        inputMode="decimal"
        value={Number.isFinite(value) ? value : ''}
        step={step}
        min={min}
        max={max}
        onChange={e => onChange(e.target.value === '' ? NaN : Number(e.target.value))}
      />
      {hint ? <span className="dim text-[11px]">{hint}</span> : null}
    </label>
  )
}

export function Panel({
  title,
  right,
  children,
  className,
  bodyClass = 'panel-b',
}: {
  title: ReactNode
  right?: ReactNode
  children: ReactNode
  className?: string
  bodyClass?: string
}) {
  return (
    <section className={`panel ${className ?? ''}`}>
      <header className="panel-h">
        <h2 className="caps m-0">{title}</h2>
        {right && <div className="right">{right}</div>}
      </header>
      <div className={bodyClass}>{children}</div>
    </section>
  )
}
