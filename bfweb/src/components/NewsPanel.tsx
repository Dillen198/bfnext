/**
 * The war news panel — the campaign's own daily dispatch.
 *
 * Generated server-side from the event stream: `bfdb/src/news.rs` decides what
 * was worth reporting and `news_llm.rs` writes it up. NOT fog-of-war scoped —
 * both coalitions read the same wire report, because in a real war the enemy
 * reads the paper too.
 *
 * A day arrives either as a written dispatch (`body`, paragraphs) or, when bfdb
 * has no writer configured, as the analysis's own sentences (`items`). Render
 * whichever is there; the structured items are always present underneath, which
 * is what the footer figures come from.
 *
 * This is the compact form for the SITREP column. `NewsPage` shows the archive.
 */
import { Link } from 'react-router-dom'
import type { NewsDay, NewsItem } from '../api'

/** Ordering is the server's; this only decides what gets emphasis. */
function itemTone(weight: number): string {
  if (weight >= 80) return 'var(--accent)'
  if (weight >= 55) return 'var(--text)'
  return 'var(--text-muted)'
}

/** Totals across every category, for the footer chips. */
function lossTotal(day: NewsDay, side: 'blue' | 'red'): number {
  const l = day.facts.losses
  if (!l) return 0
  return Object.values(l).reduce((n, t) => n + (t?.[side] ?? 0), 0)
}

function Paragraphs({ day, compact }: { day: NewsDay; compact?: boolean }) {
  const size = compact ? '0.74rem' : '0.88rem'
  if (day.body && day.body.length > 0) {
    const paras = compact ? day.body.slice(0, 2) : day.body
    return (
      <>
        {paras.map((p, i) => (
          <p key={i} style={{ margin: '0 0 8px', fontSize: size, lineHeight: 1.62, color: 'var(--text)' }}>
            {p}
          </p>
        ))}
        {compact && day.body.length > paras.length && (
          <p className="font-mono-vs" style={{ margin: 0, fontSize: '0.6rem', color: 'var(--text-dim)' }}>
            …
          </p>
        )}
      </>
    )
  }
  // No writer configured server-side: the analysis's own sentences, one per
  // angle, ordered by weight.
  const items: NewsItem[] = compact ? day.items.slice(0, 4) : day.items
  return (
    <>
      {items.map((it, i) => (
        <p
          key={`${it.angle}-${it.subject}-${i}`}
          style={{ margin: '0 0 5px', fontSize: size, lineHeight: 1.5, color: itemTone(it.weight) }}
        >
          {it.text}
        </p>
      ))}
    </>
  )
}

export function NewsDayBlock({ day, compact }: { day: NewsDay; compact?: boolean }) {
  const blueLost = lossTotal(day, 'blue')
  const redLost = lossTotal(day, 'red')
  // Older digests predate per-campaign faction names; fall back to what they
  // were written with rather than mislabelling them.
  const blueName = (day.factions?.blue ?? 'Blue').toUpperCase()
  const redName = (day.factions?.red ?? 'Red').toUpperCase()
  return (
    <article style={{ padding: compact ? '9px 12px' : '18px', borderBottom: '1px solid var(--border)' }}>
      <header style={{ display: 'flex', alignItems: 'baseline', gap: 8, marginBottom: 6, flexWrap: 'wrap' }}>
        <h3
          style={{
            margin: 0,
            fontFamily: 'var(--font-mono)',
            fontSize: compact ? '0.72rem' : '1rem',
            letterSpacing: '0.08em',
            color: 'var(--text)',
            flex: 1,
          }}
        >
          {day.headline}
        </h3>
        <time className="font-mono-vs" style={{ fontSize: '0.6rem', color: 'var(--text-dim)', whiteSpace: 'nowrap' }}>
          {day.facts.campaign_day ? `DAY ${day.facts.campaign_day} · ` : ''}
          {day.day}
          {!day.final_ && ' · developing'}
        </time>
      </header>

      <Paragraphs day={day} compact={compact} />

      {!compact && (
        <div
          className="font-mono-vs"
          style={{
            marginTop: 12,
            display: 'flex',
            gap: 14,
            flexWrap: 'wrap',
            fontSize: '0.62rem',
            color: 'var(--text-dim)',
          }}
        >
          <span>{blueName} HOLDS {day.facts.blue_held}</span>
          <span>{redName} HOLDS {day.facts.red_held}</span>
          {day.facts.changed_hands.length > 0 && <span>{day.facts.changed_hands.length} CHANGED HANDS</span>}
          {day.facts.air_kills > 0 && <span>{day.facts.air_kills} AIR KILLS</span>}
          {(blueLost > 0 || redLost > 0) && (
            <span>
              LOSSES {blueLost}/{redLost}
            </span>
          )}
          {day.facts.theatre && <span style={{ opacity: 0.6 }}>{day.facts.theatre.toUpperCase()}</span>}
          {day.written_by && day.written_by !== 'templates' && (
            <span style={{ opacity: 0.6 }}>FILED BY {day.written_by.toUpperCase()}</span>
          )}
        </div>
      )}

      {!compact && <TerritorySplit day={day} />}
    </article>
  )
}

/**
 * Who holds whose ground. Only worth a line when the war spans more than one
 * country -- on a single-country map it would just restate the holdings above.
 */
function TerritorySplit({ day }: { day: NewsDay }) {
  const by = day.facts.held_by_country
  if (!by || Object.keys(by).length < 2) return null
  const rows = Object.entries(by)
    .map(([country, t]) => ({ country, blue: t?.blue ?? 0, red: t?.red ?? 0 }))
    .sort((a, b) => b.blue + b.red - (a.blue + a.red))
  return (
    <div
      className="font-mono-vs"
      style={{
        marginTop: 6,
        display: 'flex',
        gap: 14,
        flexWrap: 'wrap',
        fontSize: '0.58rem',
        color: 'var(--text-dim)',
      }}
    >
      {rows.map((r) => (
        <span key={r.country}>
          {r.country.toUpperCase()}{' '}
          <span style={{ color: 'var(--blue)' }}>{r.blue}</span>
          {'/'}
          <span style={{ color: 'var(--red)' }}>{r.red}</span>
        </span>
      ))}
    </div>
  )
}

export default function NewsPanel({ days, error }: { days: NewsDay[] | undefined; error?: unknown }) {
  // A failed request is not a pending one. Before bfdb carries the diary at
  // all, /api/news 404s -- without this the panel sits on LOADING for ever,
  // which reads as "the server is slow" rather than "this build predates it".
  if (error != null && !days) {
    return (
      <div
        className="font-mono-vs"
        style={{ padding: '10px 12px', fontSize: '0.6rem', color: 'var(--text-dim)', letterSpacing: '0.12em', textAlign: 'center', lineHeight: 1.7 }}
      >
        WAR DIARY UNAVAILABLE
      </div>
    )
  }
  if (!days) {
    return (
      <div
        className="font-mono-vs"
        style={{ padding: '10px 12px', fontSize: '0.6rem', color: 'var(--text-dim)', letterSpacing: '0.12em', textAlign: 'center' }}
      >
        LOADING…
      </div>
    )
  }
  if (days.length === 0) {
    return (
      <div
        className="font-mono-vs"
        style={{ padding: '10px 12px', fontSize: '0.6rem', color: 'var(--text-dim)', letterSpacing: '0.12em', textAlign: 'center' }}
      >
        NO DISPATCHES YET — THE FIRST IS FILED ONCE THE CAMPAIGN HAS A DAY BEHIND IT
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
