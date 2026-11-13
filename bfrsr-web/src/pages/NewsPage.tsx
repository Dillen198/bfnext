/**
 * F3 WIRE: the campaign's daily dispatch as the papers of 1985 print it.
 *
 * *The Allied Dispatch* is the Western wire's paper. When bfdb's two-voice
 * writer has filed the same day as the Soviet side tells it (`day.alt`), the
 * *Front Bulletin* communique is set beside it: two sheets on the desk on a
 * wide screen, two tabs on a phone. Both names are invented.
 *
 * Each day is an edition. The newest one is the front page; every earlier
 * one is a previous edition that can be opened in its place (`?ed=YYYY-MM-DD`).
 * Same source and the same non-fog-of-war rule as the SITREP panel: one wire
 * report both coalitions read.
 *
 * Nothing on the sheet is invented client-side. The headline and paragraphs
 * are the writer's, the short stories are the analysis's own sentences
 * (`bfdb/src/news.rs`), and the decks, returns and tables are the day's facts
 * restated.
 */
import { useRef, useState } from 'react'
import type { ReactNode } from 'react'
import { useQuery } from '@tanstack/react-query'
import { useSearchParams } from 'react-router-dom'
import { API_ROOT, api } from '../api'
import type { LossTally, NewsAltVoice, NewsDay, NewsItem, PilotSortie } from '../api'
import { BRAND } from '../config/redstorm'
import './news/newspaper.css'

const PAPER = BRAND.papers.wire
const ALT_PAPER = BRAND.papers.alt

// ── Formatting ──────────────────────────────────────────────────────────

/** A digest's day is a UTC calendar date; read it as one, not in local time. */
function utc(day: string): Date | null {
  const d = new Date(`${day}T12:00:00Z`)
  return Number.isNaN(d.getTime()) ? null : d
}

/** "Thursday, October 15, 2026", as a dateline band would set it. */
function longDate(day: string): string {
  const d = utc(day)
  if (!d) return day
  return d.toLocaleDateString('en-US', {
    weekday: 'long', day: 'numeric', month: 'long', year: 'numeric', timeZone: 'UTC',
  })
}

/** Period-style month abbreviations: "Oct. 15", "March 3", "Sept. 9". */
const OLD_MONTH = ['Jan.', 'Feb.', 'March', 'April', 'May', 'June', 'July', 'Aug.', 'Sept.', 'Oct.', 'Nov.', 'Dec.']
function oldDate(day: string): string {
  const d = utc(day)
  if (!d) return day
  return `${OLD_MONTH[d.getUTCMonth()]} ${d.getUTCDate()}`
}

function roman(n: number): string {
  if (!Number.isFinite(n) || n < 1 || n > 3999) return String(n)
  const table: [number, string][] = [
    [1000, 'M'], [900, 'CM'], [500, 'D'], [400, 'CD'], [100, 'C'], [90, 'XC'],
    [50, 'L'], [40, 'XL'], [10, 'X'], [9, 'IX'], [5, 'V'], [4, 'IV'], [1, 'I'],
  ]
  let out = ''
  for (const [v, s] of table) while (n >= v) { out += s; n -= v }
  return out
}

function plural(n: number, one: string, many = `${one}s`): string {
  return `${n} ${n === 1 ? one : many}`
}

/** Older digests predate per-campaign faction names. */
function sides(day: NewsDay) {
  return { blue: day.factions?.blue ?? 'Blue', red: day.factions?.red ?? 'Red' }
}

/**
 * The first deck under the headline: the day's numbers as a sentence. Only
 * facts the server filed, so it never says more than the dispatch does.
 */
function deckLine(day: NewsDay): string {
  const f = day.facts
  const { blue, red } = sides(day)
  const parts: string[] = []
  if (f.blue_captures > 0 || f.red_captures > 0) {
    parts.push(`${blue} took ${plural(f.blue_captures, 'objective')}, ${red} ${f.red_captures}`)
  }
  if (f.air_kills > 0) parts.push(`${plural(f.air_kills, 'aircraft', 'aircraft')} brought down`)
  if (f.ground_kills > 0) parts.push(`${plural(f.ground_kills, 'ground unit')} destroyed`)
  if (parts.length === 0) return 'A quiet day along the line'
  return parts.join('; ')
}

/** The kill feed's category keys, as a reader would name them. */
const CATEGORY_LABEL: Record<string, string> = {
  AIRCRAFT: 'Aircraft',
  HELO: 'Helicopters',
  'AIR DEF': 'Air defence',
  RADAR: 'Radars',
  ARMOR: 'Tanks',
  APC: 'Armoured vehicles',
  ARTY: 'Artillery',
  INFANTRY: 'Infantry',
  GROUND: 'Other ground',
  LOGISTICS: 'Supply vehicles',
  SHIP: 'Ships',
  NAVAL: 'Ships',
}
const CATEGORY_ORDER = Object.keys(CATEGORY_LABEL)

function categoryLabel(k: string): string {
  return CATEGORY_LABEL[k] ?? k.charAt(0) + k.slice(1).toLowerCase()
}

/** The analysis's angles, as the heading over a short story. */
const KICKER: Record<string, string> = {
  objective_traded: 'A Contested Prize',
  objective_taken: 'Ground Changes Hands',
  objective_taken_by: 'Ground Changes Hands',
  axis_activity: 'At the Front',
  front_broken: 'The Line Is Broken',
  front_stalled: 'Deadlock on the Line',
  static_front: 'Deadlock on the Line',
  logistics_struck: 'Supply Lines Struck',
  pressure: 'A Garrison Hard Pressed',
  top_gun: 'Ace of the Day',
  pilot_standout: 'Pilot of the Day',
  air_war: 'The War in the Air',
  air_war_lopsided: 'The War in the Air',
  sead: 'Air Defences Silenced',
  streak: 'The Advance Goes On',
  country_sector: 'The Theatre of War',
  attrition_spike: 'A Costly Day',
  first_loss: 'First Blood',
  losses_cumulative: 'The Long War',
  weapon_of_the_day: 'Weapon of the Day',
  holdings: 'The State of the Map',
  opening_day: 'The Opening Day',
  opening_line: 'The Opening Day',
  quiet: 'All Quiet',
}
function kicker(angle: string): string {
  return KICKER[angle] ?? angle.replace(/_/g, ' ')
}

/** The written dispatch, or the analysis's sentences when no writer is set up. */
function storyParagraphs(day: NewsDay): string[] {
  if (day.body && day.body.length > 0) return day.body
  return day.items.map(it => it.text)
}

/**
 * The analysis's sentences that can stand as stories of their own. Only when
 * there is a written dispatch -- otherwise they already are the lead story --
 * and never the losses sentence, which the ledger prints in full.
 */
function shortStories(day: NewsDay): NewsItem[] {
  if (!day.body || day.body.length === 0) return []
  return day.items.filter(it => it.angle !== 'losses_tally')
}

// ── Pieces of the sheet ─────────────────────────────────────────────────

function Masthead({ day }: { day: NewsDay }) {
  const f = day.facts
  return (
    <header>
      <div className="np-mast">
        <div className="np-ear">
          <strong>Wire Service</strong>
          <em>Filed from the southern flank.</em>
        </div>
        <h1 className="np-title">{PAPER}</h1>
        <div className="np-ear np-ear--right">
          <strong>{day.final_ ? 'Final Edition' : 'Late Edition'}</strong>
          {day.final_ ? 'Complete returns to the close of the day.' : 'Wire reports still arriving.'}
        </div>
      </div>
      <div className="np-dateline">
        {/* `round` is the round's id: a volume number only while it reads as one. */}
        <span>{day.round > 0 && day.round < 4000 ? `Vol. ${roman(day.round)}` : 'Southern Flank'}{f.campaign_day ? `.—No. ${f.campaign_day}` : ''}.</span>
        <span>{longDate(day.day)}.</span>
        <span>
          {f.campaign_day ? `Day ${f.campaign_day} of the Campaign` : 'Campaign Edition'}
          {f.theatre ? ` · ${f.theatre}` : ''}
        </span>
      </div>
    </header>
  )
}

function LeadStory({ day, deckItem }: { day: NewsDay; deckItem: NewsItem | undefined }) {
  const f = day.facts
  const { blue, red } = sides(day)
  const paras = storyParagraphs(day)
  const place = (f.theatre ?? 'The Front').toUpperCase()
  return (
    <div className="np-col np-col--lead">
      <div className="np-decks">
        <p className="np-deck1">{deckLine(day)}</p>
        {deckItem && (
          <>
            <hr className="np-dash" />
            <p className="np-deck2">{deckItem.text}</p>
          </>
        )}
        <hr className="np-dash" />
        <p className="np-deck3">
          {blue} Holds {f.blue_held} Objectives, {red} {f.red_held}
        </p>
        <hr className="np-dash np-dash--long" />
      </div>
      <LeadPicture day={day} />
      <p className="np-special">Special Dispatch to {PAPER}.</p>
      <div className="np-body">
        {paras.map((p, i) => (
          <p key={i}>
            {i === 0 && <span className="np-inline-date">{place}, {oldDate(day.day)}.—</span>}
            {p}
          </p>
        ))}
      </div>
      {!day.final_ && <p className="np-developing">This dispatch will be revised as later reports come in.</p>}
    </div>
  )
}

/**
 * A picture printed the way the paper prints them: greyscale, a coarse
 * halftone screen, a thin frame. Hides itself if the file will not load
 * rather than leaving a broken-image box on the page.
 */
function Halftone({ src, alt, caption, credit, lazy, className }: {
  src: string
  alt: string
  caption?: ReactNode
  credit?: string
  lazy?: boolean
  className?: string
}) {
  const [failed, setFailed] = useState(false)
  if (failed) return null
  return (
    <figure className={`np-photo${className ? ` ${className}` : ''}`}>
      <span className="np-photo-frame">
        <img src={src} alt={alt} loading={lazy ? 'lazy' : undefined} decoding="async" onError={() => setFailed(true)} />
      </span>
      {(caption || credit) && (
        <figcaption>
          {caption}
          {credit && <small>{credit}</small>}
        </figcaption>
      )}
    </figure>
  )
}

/** A previous edition's picture. Spans only: it sits inside a button. */
function Thumb({ src }: { src: string }) {
  const [failed, setFailed] = useState(false)
  if (failed) return null
  return (
    <span className="np-photo-frame np-thumb">
      <img src={src} alt="" loading="lazy" decoding="async" onError={() => setFailed(true)} />
    </span>
  )
}

/** The picture filed with the day, or a notice that one is still coming. */
function LeadPicture({ day }: { day: NewsDay }) {
  if (day.image) {
    return (
      <Halftone
        // Keyed so a picture that failed on one edition is retried on the next.
        key={day.image}
        className="np-photo--lead"
        src={`${API_ROOT}${day.image}`}
        alt={day.headline}
        caption={<>{day.facts.theatre ? `On the ${day.facts.theatre} front` : 'At the front'}, {oldDate(day.day)}.</>}
        credit={day.image_provisional ? 'Early-edition illustration' : 'Illustration drawn for this dispatch'}
      />
    )
  }
  if (day.image_pending) {
    return (
      <div className="np-photo-pending">
        <span>Photograph to follow.</span>
        <small>
          {day.final_
            ? 'The picture for this dispatch is being drawn.'
            : 'The early-edition picture is drawn a few hours into the day.'}
        </small>
      </div>
    )
  }
  return null
}

/** The angles that name a pilot, and so can carry a picture of their aircraft. */
const PILOT_ANGLES = new Set(['top_gun', 'pilot_standout', 'objective_taken_by'])

/**
 * The aircraft a pilot flies most, from their sortie record -- the same
 * choice the Pilots page makes for its header picture.
 */
function useMostFlown(pilot: string | undefined): string | null {
  const { data: pilots } = useQuery({
    queryKey: ['all-pilots'],
    queryFn: api.allPilots,
    enabled: !!pilot,
    staleTime: 10 * 60_000,
  })
  const ucid = pilot ? pilots?.find(p => p.name === pilot)?.ucid : undefined
  const { data: sorties } = useQuery({
    queryKey: ['pilot-sorties', ucid],
    queryFn: () => api.pilotSorties(ucid!),
    enabled: !!ucid,
    staleTime: 10 * 60_000,
  })
  return sorties ? mostFlown(sorties) : null
}

function mostFlown(sorties: PilotSortie[]): string | null {
  const counts = new Map<string, number>()
  for (const s of sorties) counts.set(s.aircraft, (counts.get(s.aircraft) ?? 0) + 1)
  let best: string | null = null, n = 0
  for (const [ac, c] of counts) if (c > n) { best = ac; n = c }
  return best
}

function PilotPortrait({ pilot }: { pilot: string }) {
  const aircraft = useMostFlown(pilot)
  if (!aircraft) return null
  return (
    <Halftone
      key={aircraft}
      className="np-photo--small"
      src={`/images/aircraft/${encodeURIComponent(aircraft)}.jpg`}
      alt={aircraft}
      lazy
      caption={<>{pilot}’s mount: the {aircraft.replace(/_/g, ' ')}.</>}
      credit="The type this pilot flies most"
    />
  )
}

function MiddleColumn({ stories }: { stories: NewsItem[] }) {
  return (
    <div className="np-col np-col--mid">
      {stories.map((it, i) => {
        const pilot = PILOT_ANGLES.has(it.angle) ? it.vars?.pilot : undefined
        return (
          <article key={`${it.angle}-${it.subject}-${i}`} className="np-brief">
            {i > 0 && <hr className="np-dash" />}
            <h3>{kicker(it.angle)}</h3>
            {pilot && <PilotPortrait pilot={pilot} />}
            <p>{it.text}</p>
          </article>
        )
      })}
    </div>
  )
}

function ReturnRow({ label, blue, red }: { label: string; blue: string | number; red: string | number }) {
  return (
    <tr>
      <td><span className="np-leader">{label}</span></td>
      <td>{blue}</td>
      <td>{red}</td>
    </tr>
  )
}

function Bulletin({ day }: { day: NewsDay }) {
  const f = day.facts
  const { blue, red } = sides(day)
  // The same objective can change hands several times in a day; list it once.
  const traded = new Map<string, number>()
  for (const n of f.changed_hands) traded.set(n, (traded.get(n) ?? 0) + 1)
  const losses = (side: 'blue' | 'red') => sumSide(f.losses, side)
  return (
    <div className="np-col np-col--side">
      <section className="np-box">
        <h2 className="np-box-title">Official Returns</h2>
        <p className="np-box-sub">The state of the war tonight.</p>
        <table className="np-returns">
          <thead>
            <tr><th><span className="sr-only">Return</span></th><th>{blue}</th><th>{red}</th></tr>
          </thead>
          <tbody>
            <ReturnRow label="Objectives held" blue={f.blue_held} red={f.red_held} />
            {f.blue_airbases != null && f.red_airbases != null && (
              <ReturnRow label="Airbases" blue={f.blue_airbases} red={f.red_airbases} />
            )}
            {f.blue_logi != null && f.red_logi != null && (
              <ReturnRow label="Supply health" blue={`${Math.round(f.blue_logi)}%`} red={`${Math.round(f.red_logi)}%`} />
            )}
            <ReturnRow label="Captures today" blue={f.blue_captures} red={f.red_captures} />
            {f.losses && <ReturnRow label="Losses today" blue={losses('blue')} red={losses('red')} />}
          </tbody>
        </table>
      </section>

      <section className="np-box">
        <h2 className="np-box-title">Changed Hands</h2>
        <p className="np-box-sub">Positions taken today.</p>
        {traded.size === 0 ? (
          <p className="np-quiet">None. The line held.</p>
        ) : (
          <ul className="np-list">
            {[...traded].map(([name, n]) => (
              <li key={name}>
                <span>{name}</span>
                <span>{n > 1 ? `${n} times` : 'taken'}</span>
              </li>
            ))}
          </ul>
        )}
      </section>

      {f.fighting_in && Object.keys(f.fighting_in).length > 0 && (
        <section className="np-box">
          <h2 className="np-box-title">Where the Fighting Was</h2>
          <ul className="np-list">
            {Object.entries(f.fighting_in)
              .sort((a, b) => b[1] - a[1])
              .map(([country, n]) => (
                <li key={country}>
                  <span>{country}</span>
                  <span>{plural(n, 'capture')}</span>
                </li>
              ))}
          </ul>
        </section>
      )}

      <section className="np-box">
        <h2 className="np-box-title">In the Air</h2>
        <p className="np-quiet" style={{ fontStyle: 'normal' }}>
          {plural(f.air_kills, 'aircraft', 'aircraft')} brought down; {plural(f.ground_kills, 'ground unit')} destroyed.
        </p>
      </section>
    </div>
  )
}

function sumSide(m: Record<string, LossTally> | undefined, side: 'blue' | 'red'): number {
  if (!m) return 0
  return Object.values(m).reduce((n, t) => n + (t?.[side] ?? 0), 0)
}

function Ledger({ day }: { day: NewsDay }) {
  const f = day.facts
  const { blue, red } = sides(day)
  const today = f.losses ?? {}
  const total = f.losses_total ?? {}
  const keys = [...new Set([...Object.keys(today), ...Object.keys(total)])].sort((a, b) => {
    const ia = CATEGORY_ORDER.indexOf(a), ib = CATEGORY_ORDER.indexOf(b)
    return (ia < 0 ? 99 : ia) - (ib < 0 ? 99 : ib) || a.localeCompare(b)
  })
  const countries = f.held_by_country
    ? Object.entries(f.held_by_country)
        .map(([c, t]) => ({ c, blue: t?.blue ?? 0, red: t?.red ?? 0 }))
        .sort((a, b) => b.blue + b.red - (a.blue + a.red))
    : []
  const showCountries = countries.length >= 2
  if (keys.length === 0 && !showCountries) return null
  const hasTotal = Object.keys(total).length > 0
  const cell = (n: number | undefined) => (n ? n : <span className="np-dim">—</span>)
  return (
    <section>
      <h2 className="np-section-head">The Ledger</h2>
      <div className={`np-tables${showCountries && keys.length > 0 ? '' : ' np-tables--one'}`}>
        {keys.length > 0 && (
          <div>
            <table className="np-table">
              <caption>Equipment Destroyed</caption>
              <thead>
                <tr>
                  <th><span className="sr-only">Return</span></th>
                  <th colSpan={2}>To-day</th>
                  {hasTotal && <th colSpan={2}>Since the Opening</th>}
                </tr>
                <tr>
                  <th>Category</th>
                  <th>{blue}</th>
                  <th>{red}</th>
                  {hasTotal && <th>{blue}</th>}
                  {hasTotal && <th>{red}</th>}
                </tr>
              </thead>
              <tbody>
                {keys.map(k => (
                  <tr key={k}>
                    <td><span className="np-leader">{categoryLabel(k)}</span></td>
                    <td>{cell(today[k]?.blue)}</td>
                    <td>{cell(today[k]?.red)}</td>
                    {hasTotal && <td>{cell(total[k]?.blue)}</td>}
                    {hasTotal && <td>{cell(total[k]?.red)}</td>}
                  </tr>
                ))}
              </tbody>
              <tfoot>
                <tr>
                  <td>All losses</td>
                  <td>{sumSide(today, 'blue')}</td>
                  <td>{sumSide(today, 'red')}</td>
                  {hasTotal && <td>{sumSide(total, 'blue')}</td>}
                  {hasTotal && <td>{sumSide(total, 'red')}</td>}
                </tr>
              </tfoot>
            </table>
          </div>
        )}
        {showCountries && (
          <div>
            <table className="np-table">
              <caption>Ground Held, by Country</caption>
              <thead>
                <tr>
                  <th>Country</th>
                  <th>{blue}</th>
                  <th>{red}</th>
                </tr>
              </thead>
              <tbody>
                {countries.map(r => (
                  <tr key={r.c}>
                    <td><span className="np-leader">{r.c}</span></td>
                    <td>{cell(r.blue)}</td>
                    <td>{cell(r.red)}</td>
                  </tr>
                ))}
              </tbody>
              <tfoot>
                <tr>
                  <td>All objectives</td>
                  <td>{f.blue_held}</td>
                  <td>{f.red_held}</td>
                </tr>
              </tfoot>
            </table>
          </div>
        )}
      </div>
    </section>
  )
}

/**
 * The same day as the other side's paper prints it: a front communique that
 * spins what the wire reported (bfdb's two-voice writer). Its own sheet, its
 * own faces and a red rule, so nobody takes it for the wire.
 */
function AltDispatch({ day, alt }: { day: NewsDay; alt: NewsAltVoice }) {
  const place = (day.facts.theatre ?? 'The Front').toUpperCase()
  return (
    <article className="fb" aria-label={ALT_PAPER}>
      <header className="fb-mast">
        <p className="fb-flag">Communiqué of the Front Command</p>
        <h2 className="fb-title">{ALT_PAPER}</h2>
        <p className="fb-sub">
          {oldDate(day.day)}{day.facts.campaign_day ? ` · Day ${day.facts.campaign_day} of operations` : ''} · Issued to all units
        </p>
      </header>
      <h3 className="fb-headline">{alt.headline}</h3>
      <div className="fb-body">
        {alt.body.map((p, i) => (
          <p key={i}>
            {i === 0 && <span className="fb-dateline">{place}.— </span>}
            {p}
          </p>
        ))}
      </div>
      <p className="fb-foot">
        As transmitted{alt.written_by && alt.written_by !== 'templates' ? ` (written by ${alt.written_by})` : ''}.
        The other side's account of the same day; the figures in the returns are the server's.
      </p>
    </article>
  )
}

/** The dev-only sample for `?altmock`. Marked as a sample in every line. */
const SAMPLE_ALT: NewsAltVoice = {
  voice: 'soviet',
  headline: '[Sample] Front units hold firm on every axis',
  body: [
    '[Sample text for layout only.] Units of the Front repelled the aggressor along the whole line and inflicted heavy losses on his air forces.',
    '[Sample] The bridgehead remains under pressure. The communique of the Front Command will report fully when the day is filed.',
  ],
  written_by: 'sample',
}

type Voice = 'wire' | 'alt'

/**
 * Both papers on the desk: the wire on the left, the communique on the
 * right. On a narrow desk they are tabs. Both are always in the page; CSS
 * (a container query on the desk) decides which shows.
 */
function TwoVoices({ day, alt, wire }: { day: NewsDay; alt: NewsAltVoice; wire: ReactNode }) {
  const [voice, setVoice] = useState<Voice>('wire')
  const tab = (v: Voice, label: string) => (
    <button
      type="button"
      role="tab"
      id={`np-tab-${v}`}
      aria-selected={voice === v}
      aria-controls={`np-voice-${v}`}
      className={`np-voice-tab np-voice-tab--${v}`}
      onClick={() => setVoice(v)}
    >
      {label}
    </button>
  )
  return (
    <>
      <div className="np-voice-tabs" role="tablist" aria-label="Whose account">
        {tab('wire', PAPER)}
        {tab('alt', ALT_PAPER)}
      </div>
      <div className="np-dual">
        <div id="np-voice-wire" role="tabpanel" aria-labelledby="np-tab-wire" className="np-voice np-voice--wire" data-active={voice === 'wire'}>
          {wire}
        </div>
        <div id="np-voice-alt" role="tabpanel" aria-labelledby="np-tab-alt" className="np-voice np-voice--alt" data-active={voice === 'alt'} data-voice={alt.voice}>
          <AltDispatch day={day} alt={alt} />
        </div>
      </div>
    </>
  )
}

function PreviousEditions({ days, current, onOpen }: { days: NewsDay[]; current: string; onOpen: (day: string) => void }) {
  const others = days.filter(d => d.day !== current)
  if (others.length === 0) return null
  return (
    <section>
      <h2 className="np-section-head">Previous Editions</h2>
      <div className="np-issues">
        {others.map(d => {
          const lead = storyParagraphs(d)[0] ?? ''
          return (
            <button key={d.day} type="button" className="np-issue" onClick={() => onOpen(d.day)}>
              <span className="np-issue-date">
                {d.facts.campaign_day ? `Day ${d.facts.campaign_day}, ` : ''}{oldDate(d.day)}
              </span>
              {d.image && <Thumb src={`${API_ROOT}${d.image}`} />}
              <span className="np-issue-head">{d.headline}</span>
              <hr className="np-dash" />
              {lead && <span className="np-issue-snip">{lead}</span>}
            </button>
          )
        })}
      </div>
    </section>
  )
}

// ── Page ────────────────────────────────────────────────────────────────

export default function NewsPage() {
  const { data, isLoading, error } = useQuery({
    queryKey: ['news', 'archive'],
    queryFn: () => api.news(200),
    refetchInterval: 5 * 60_000,
  })
  const [params, setParams] = useSearchParams()
  const deskRef = useRef<HTMLDivElement>(null)

  const days = data?.days ?? []
  const wanted = params.get('ed')
  const found = days.find(d => d.day === wanted) ?? days[0]
  // Dev builds only: `?altmock` sets a labelled sample communique beside an
  // edition that has none, to lay out the two-voice sheet before bfdb's
  // writer files one. The literal guard drops it from a production bundle.
  const edition = import.meta.env.DEV && found && !found.alt && params.has('altmock')
    ? { ...found, alt: SAMPLE_ALT }
    : found
  const isLatest = edition != null && edition.day === days[0]?.day

  const open = (day: string) => {
    setParams(prev => {
      const next = new URLSearchParams(prev)
      if (day === days[0]?.day) next.delete('ed')
      else next.set('ed', day)
      return next
    })
    deskRef.current?.scrollTo({ top: 0, behavior: 'smooth' })
  }

  // The strongest of the short stories heads the lead as its second deck; the
  // rest run down the middle column.
  const stories = edition ? shortStories(edition) : []
  const [deckItem, ...rest] = stories
  // Six of the rest, always keeping a pilot's story if there is one: it is
  // the one that carries a picture.
  const midStories = rest.slice(0, 6)
  const pilotStory = rest.find(it => PILOT_ANGLES.has(it.angle) && it.vars?.pilot)
  if (pilotStory && !midStories.includes(pilotStory)) midStories.push(pilotStory)
  const hasMid = midStories.length > 0

  const paper = edition && (
    <div className="np">
      <Masthead day={edition} />
      {!isLatest && (
        <p className="np-backissue">
          A previous edition, of {longDate(edition.day)}.{' '}
          <button type="button" className="np-back" onClick={() => open(days[0].day)}>
            Return to to-day's paper.
          </button>
        </p>
      )}
      <div className="np-banner">
        <h2 className="np-headline">{edition.headline}</h2>
      </div>
      <div className={`np-front${hasMid ? '' : ' np-front--nomid'}`}>
        <LeadStory day={edition} deckItem={deckItem} />
        {hasMid && <MiddleColumn stories={midStories} />}
        <Bulletin day={edition} />
      </div>
      <Ledger day={edition} />
      <PreviousEditions days={days} current={edition.day} onOpen={open} />
      <p className="np-colophon">
        {PAPER} is written each day from what actually happened on
        the server: captures, equipment destroyed, the state of each
        side's rear, and how long a line has held. The figures are the
        server's; the prose is written fresh each day
        {edition.written_by && edition.written_by !== 'templates' ? ` (this edition by ${edition.written_by})` : ''}.
        Both coalitions read the same paper.
      </p>
    </div>
  )

  return (
    <div className="np-desk" ref={deskRef}>
      {edition && (edition.alt
        // A fresh tab choice per edition.
        ? <TwoVoices key={edition.day} day={edition} alt={edition.alt} wire={paper} />
        : paper)}
      {!edition && (
      <div className="np">
        {isLoading && <p className="np-notice">The presses are running…</p>}

        {error != null && (
          <p className="np-notice">
            The paper could not be fetched. If this persists the news generator
            may not be running — it starts a minute after bfdb does.
          </p>
        )}

        {!isLoading && error == null && !edition && (
          <p className="np-notice">
            Nothing has been filed yet. The first edition goes to press within
            the hour of the campaign's opening day.
          </p>
        )}

      </div>
      )}
    </div>
  )
}
