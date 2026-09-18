/**
 * The war news panel — the campaign's own daily dispatch.
 *
 * Generated server-side by `bfdb/src/news.rs` from the event stream, and NOT
 * fog-of-war scoped: both coalitions read the same wire report, because in a
 * real war the enemy reads the paper too.
 *
 * This is the compact form for the SITREP column. `NewsPage` shows the whole
 * archive.
 */
import { Link } from 'react-router-dom'
import type { NewsDay } from '../api'

/** Ordering is the server's; this only decides what gets emphasis. */
function itemTone(weight: number): string {
  if (weight >= 80) return 'var(--accent)'
  if (weight >= 55) return 'var(--text)'
  return 'var(--text-muted)'
}

export function NewsDayBlock({ day, compact }: { day: NewsDay; compact?: boolean }) {
  const items = compact ? day.items.slice(0, 4) : day.items
  return (
    <article style={{ padding: compact ? '9px 12px' : '16px 18px', borderBottom: '1px solid var(--border)' }}>
      <header style={{ display: 'flex', alignItems: 'baseline', gap: 8, marginBottom: 6, flexWrap: 'wrap' }}>
        <h3
          style={{
            margin: 0,
            fontFamily: 'var(--font-mono)',
            fontSize: compact ? '0.72rem' : '0.95rem',
            letterSpacing: '0.08em',
            color: 'var(--text)',
            flex: 1,
          }}
        >
          {day.headline}
        </h3>
        <time
          className="font-mono-vs"
          style={{ fontSize: '0.6rem', color: 'var(--text-dim)', whiteSpace: 'nowrap' }}
        >
          {day.day}
          {!day.final_ && ' · developing'}
        </time>
      </header>

      {items.map((it, i) => (
        <p
          key={`${it.angle}-${it.subject}-${i}`}
          style={{
            margin: '0 0 5px',
            fontSize: compact ? '0.74rem' : '0.86rem',
            lineHeight: 1.5,
            color: itemTone(it.weight),
          }}
        >
          {it.text}
        </p>
      ))}

      {!compact && (
        <div
          className="font-mono-vs"
          style={{
            marginTop: 10,
            display: 'flex',
            gap: 14,
            flexWrap: 'wrap',
            fontSize: '0.62rem',
            color: 'var(--text-dim)',
          }}
        >
          <span>BLUE HOLDS {day.facts.blue_held}</span>
          <span>RED HOLDS {day.facts.red_held}</span>
          {day.facts.changed_hands.length > 0 && (
            <span>{day.facts.changed_hands.length} CHANGED HANDS</span>
          )}
          {day.facts.air_kills > 0 && <span>{day.facts.air_kills} AIR KILLS</span>}
          {day.facts.ground_kills > 0 && <span>{day.facts.ground_kills} GROUND KILLS</span>}
        </div>
      )}
    </article>
  )
}

export default function NewsPanel({ days }: { days: NewsDay[] | undefined }) {
  if (!days) {
    return (
      <div className="font-mono-vs" style={{ padding: '10px 12px', fontSize: '0.6rem', color: 'var(--text-dim)', letterSpacing: '0.12em', textAlign: 'center' }}>
        LOADING…
      </div>
    )
  }
  if (days.length === 0) {
    return (
      <div className="font-mono-vs" style={{ padding: '10px 12px', fontSize: '0.6rem', color: 'var(--text-dim)', letterSpacing: '0.12em', textAlign: 'center' }}>
        NO DISPATCHES YET — THE FIRST IS WRITTEN AFTER THE CAMPAIGN'S FIRST DAY
      </div>
    )
  }
  return (
    <div>
      {days.slice(0, 3).map(d => (
        <NewsDayBlock key={d.day} day={d} compact />
      ))}
      <div style={{ padding: '8px 12px', textAlign: 'center' }}>
        <Link
          to="/news"
          className="font-mono-vs"
          style={{ fontSize: '0.62rem', letterSpacing: '0.14em', color: 'var(--accent)', textDecoration: 'none' }}
        >
          FULL ARCHIVE →
        </Link>
      </div>
    </div>
  )
}
