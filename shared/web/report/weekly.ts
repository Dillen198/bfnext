// The weekly campaign report poster: bfdb's /api/report/weekly turned into
// what the poster shows (`posterModel`), the dispatch drafted from it
// (`draftDispatch`) and the Discord post that goes with the picture
// (`postText`). Pure: no React, no fetch. The posters themselves are in
// WeeklyPoster.tsx; the admin page in ReportStudio.tsx; the page the bot
// captures for the weekly schedule in ReportPrint.tsx.

export type Side = 'Blue' | 'Red'

export interface SideForces {
  objectives: number
  airfields: number
  fobs: number
  farps: number
  fleets: string[]
  naval_bases: number
  sam_sites: number
  command_centres: number
  factories: number
  deployed: number
  captured: number
  air_kills: number
  ground_kills: number
}

export interface ReportCapture { time: string; objective: string; side: string; by: string[] }
export interface ReportStory {
  id: string; time: string; title: string; text: string
  caused_by: string[]; side: string | null; effects: string[]
}
export interface ReportLastStand {
  side: string | null
  ordered: string | null
  outcome: 'ordered' | 'intercepted' | 'detonated' | string
  time: string
  title: string
  by: string[]
  this_week: boolean
}
export interface ReportPilot {
  name: string; side: string | null; squadron: string | null
  air_kills: number; ground_kills: number; captures: number; deploys: number; score: number
}
export interface ReportSquadron {
  tag: string; name: string; pilots: number
  air_kills: number; ground_kills: number; captures: number; score: number
}
export interface ReportWill {
  blue: number; red: number; blue_trend: number | null; red_trend: number | null; threshold: number
}

export interface WeeklyReportData {
  round: number | null
  round_start: string | null
  since: string
  until: string
  forces: { blue: SideForces; red: SideForces; neutral: number }
  captures: ReportCapture[]
  capture_count: number
  contested: { name: string; captures: number }[]
  story: ReportStory[]
  last_stand: ReportLastStand | null
  will: ReportWill | null
  pilots: ReportPilot[]
  squadrons: ReportSquadron[]
  active_pilots: number
}

/** GET /api/report/weekly */
export interface WeeklyReportResponse {
  instance: string
  label: string
  theme: string | null
  brand_name: string | null
  dcs_server_name: string | null
  blue_label: string | null
  red_label: string | null
  days: number
  since: string
  until: string
  round_number?: number | null
  round_name?: string | null
  scenario?: string | null
  round_active?: boolean
  report: WeeklyReportData | null
}

/** Which poster: the 1985 situation room, or the Vector Strike dashboard look. */
export type PosterTheme = 'redstorm' | 'levant'

export interface BoardRow {
  label: string
  blue: number
  red: number
  blueNote?: string
  redNote?: string
}

export interface PosterEvent {
  time: string
  kind: 'capture' | 'story' | 'laststand'
  side: Side | null
  title: string
  detail: string
}

export interface PosterModel {
  theme: PosterTheme
  brand: string
  server: string
  instance: string
  blue: string
  red: string
  roundNo: number | null
  /** Day of the war at the end of the window (day 1 is the round's first). */
  day: number | null
  /** "3–10 NOV 2026" */
  week: string
  until: Date
  days: number
  board: BoardRow[]
  /** Objective share for the front bar, 0..1 for Blue. */
  blueShare: number
  events: PosterEvent[]
  lastStand: ReportLastStand | null
  pilots: ReportPilot[]
  squadrons: ReportSquadron[]
  totals: { captures: number; airKills: number; groundKills: number; activePilots: number; deployed: number }
  /** Objectives each side took in the window. */
  taken: { blue: number; red: number }
  /** Each side's kills in the window. */
  kills: { blue: { air: number; ground: number }; red: { air: number; ground: number } }
  will: ReportWill | null
  contested: { name: string; captures: number }[]
  /** No round, or nothing at all happened in the window. */
  quiet: boolean
}

export interface Dispatch {
  headline: string
  standfirst: string
  /** Paragraphs, separated by blank lines. */
  body: string
}

const MONTHS = ['JAN', 'FEB', 'MAR', 'APR', 'MAY', 'JUN', 'JUL', 'AUG', 'SEP', 'OCT', 'NOV', 'DEC']
const DAY_MS = 86_400_000

export const n = (v: number) => v.toLocaleString('en-US')
const plural = (v: number, one: string, many = `${one}s`) => `${n(v)} ${v === 1 ? one : many}`

/** "3–10 NOV 2026", or "28 OCT – 4 NOV 2026" across a month. */
export function weekRange(since: Date, until: Date): string {
  // The window ends at `until`, exclusive: name the last day it covers.
  const last = new Date(until.getTime() - 1)
  const a = `${since.getUTCDate()}`
  const b = `${last.getUTCDate()} ${MONTHS[last.getUTCMonth()]} ${last.getUTCFullYear()}`
  if (since.getUTCMonth() === last.getUTCMonth() && since.getUTCFullYear() === last.getUTCFullYear()) {
    return a === `${last.getUTCDate()}` ? b : `${a}–${b}`
  }
  return `${a} ${MONTHS[since.getUTCMonth()]} – ${b}`
}

const sideOf = (s: string | null | undefined): Side | null => (s === 'Blue' || s === 'Red' ? s : null)

/** What the poster shows, from bfdb's answer. `blue`/`red` are the
 *  campaign's own names for the sides when bfdb has none. */
export function posterModel(r: WeeklyReportResponse, fallback: { blue: string; red: string; brand: string }): PosterModel {
  const rep = r.report
  const blue = r.blue_label || fallback.blue
  const red = r.red_label || fallback.red
  const theme: PosterTheme = (r.theme ?? '').toLowerCase() === 'redstorm' ? 'redstorm' : 'levant'
  const until = new Date(rep?.until ?? r.until)
  const since = new Date(rep?.since ?? r.since)
  const name = (s: string | null) => (s === 'Blue' ? blue : s === 'Red' ? red : 'Neither side')
  const f = rep?.forces
  const zero: SideForces = {
    objectives: 0, airfields: 0, fobs: 0, farps: 0, fleets: [], naval_bases: 0, sam_sites: 0,
    command_centres: 0, factories: 0, deployed: 0, captured: 0, air_kills: 0, ground_kills: 0,
  }
  const B = f?.blue ?? zero
  const R = f?.red ?? zero
  const fobs = (s: SideForces) => s.fobs + s.farps
  const board: BoardRow[] = [
    { label: 'Objectives', blue: B.objectives, red: R.objectives,
      blueNote: B.captured ? `+${B.captured} this week` : undefined,
      redNote: R.captured ? `+${R.captured} this week` : undefined },
    { label: 'Airfields', blue: B.airfields, red: R.airfields,
      blueNote: fobs(B) ? `+${plural(fobs(B), 'FOB')}` : undefined,
      redNote: fobs(R) ? `+${plural(fobs(R), 'FOB')}` : undefined },
    { label: 'Fleets', blue: B.fleets.length, red: R.fleets.length,
      blueNote: B.fleets.join(', ') || undefined, redNote: R.fleets.join(', ') || undefined },
    { label: 'SAM sites', blue: B.sam_sites, red: R.sam_sites },
    { label: 'Deployed', blue: B.deployed, red: R.deployed, blueNote: 'this week', redNote: 'this week' },
  ]
  const held = B.objectives + R.objectives
  const blueShare = held > 0 ? B.objectives / held : 0.5

  const events: PosterEvent[] = []
  for (const c of rep?.captures ?? []) {
    events.push({
      time: c.time, kind: 'capture', side: sideOf(c.side),
      title: `${name(c.side)} take ${c.objective}`,
      detail: c.by.length ? `Captured by ${c.by.slice(0, 3).join(', ')}${c.by.length > 3 ? ' and others' : ''}` : 'Captured',
    })
  }
  const ls = rep?.last_stand ?? null
  for (const s of rep?.story ?? []) {
    if (s.id.startsWith('last_stand_')) continue // drawn as the Last Stand itself
    events.push({
      time: s.time, kind: 'story', side: sideOf(s.side), title: s.title,
      detail: s.text || s.effects.slice(0, 2).join(' · ') || (s.caused_by.length ? `Set off by ${s.caused_by.join(', ')}` : ''),
    })
  }
  if (ls?.this_week) {
    events.push({ time: ls.time, kind: 'laststand', side: sideOf(ls.side), title: lastStandTitle(ls, { blue, red }), detail: lastStandLine(ls, { blue, red }) })
  }
  // Newest first; the Last Stand and story events before a pile of captures.
  const weight = (e: PosterEvent) => (e.kind === 'laststand' ? 2 : e.kind === 'story' ? 1 : 0)
  events.sort((a, b) => weight(b) - weight(a) || b.time.localeCompare(a.time))

  const start = rep?.round_start ? new Date(rep.round_start) : null
  const day = start ? Math.max(1, Math.floor((until.getTime() - 1 - start.getTime()) / DAY_MS) + 1) : null
  const totals = {
    captures: rep?.capture_count ?? 0,
    airKills: B.air_kills + R.air_kills,
    groundKills: B.ground_kills + R.ground_kills,
    activePilots: rep?.active_pilots ?? 0,
    deployed: B.deployed + R.deployed,
  }
  return {
    theme,
    brand: r.brand_name || fallback.brand,
    server: r.dcs_server_name || r.label,
    instance: r.instance,
    blue, red,
    roundNo: r.round_number ?? null,
    day,
    week: weekRange(since, until),
    until,
    days: r.days,
    board,
    blueShare,
    events: events.slice(0, 7),
    lastStand: ls,
    pilots: rep?.pilots ?? [],
    squadrons: rep?.squadrons ?? [],
    totals,
    taken: { blue: B.captured, red: R.captured },
    kills: { blue: { air: B.air_kills, ground: B.ground_kills }, red: { air: R.air_kills, ground: R.ground_kills } },
    will: rep?.will ?? null,
    contested: rep?.contested ?? [],
    quiet: !rep || (totals.captures + totals.airKills + totals.groundKills + totals.deployed === 0 && !rep.story.length),
  }
}

function lastStandTitle(ls: ReportLastStand, sides: { blue: string; red: string }): string {
  const who = ls.side === 'Blue' ? sides.blue : ls.side === 'Red' ? sides.red : 'A side'
  if (ls.outcome === 'intercepted') return 'The Last Stand is shot down'
  if (ls.outcome === 'detonated') return 'Nuclear detonation'
  return `${who} reach for the Last Stand`
}

function lastStandLine(ls: ReportLastStand, sides: { blue: string; red: string }): string {
  const enemy = ls.side === 'Blue' ? sides.red : ls.side === 'Red' ? sides.blue : 'the enemy'
  if (ls.outcome === 'intercepted') {
    return ls.by.length ? `${enemy} pilots ${ls.by.slice(0, 3).join(', ')} brought down the nuclear-armed aircraft.` : `${enemy} brought down the nuclear-armed aircraft.`
  }
  if (ls.outcome === 'detonated') return ls.title.replace(/^The Last Stand:\s*/i, '')
  return 'A nuclear strike has been ordered. It has not flown yet.'
}

const DAY_WORDS = ['ONE', 'TWO', 'THREE', 'FOUR', 'FIVE', 'SIX', 'SEVEN', 'EIGHT', 'NINE', 'TEN', 'ELEVEN', 'TWELVE',
  'THIRTEEN', 'FOURTEEN', 'FIFTEEN', 'SIXTEEN', 'SEVENTEEN', 'EIGHTEEN', 'NINETEEN', 'TWENTY']
/** "DAY TWELVE", or "DAY 31" past twenty. */
export function dayWords(day: number | null): string {
  if (!day) return 'THIS WEEK'
  return `DAY ${DAY_WORDS[day - 1] ?? String(day)}`
}

/** The dispatch the poster carries until an admin rewrites it: a headline,
 *  a standfirst and two or three short paragraphs, from the numbers. */
export function draftDispatch(m: PosterModel): Dispatch {
  const [B, R] = [m.board[0].blue, m.board[0].red]
  const { blue: blueTook, red: redTook } = m.taken
  const top = m.pilots[0]
  const sq = m.squadrons[0]
  const ls = m.lastStand?.this_week ? m.lastStand : null

  let headline: string
  if (ls?.outcome === 'intercepted') headline = 'Last Stand shot down'
  else if (ls?.outcome === 'detonated') headline = 'Nuclear fire on the front'
  else if (ls) headline = `${ls.side === 'Blue' ? m.blue : m.red} order the Last Stand`
  else if (m.quiet) headline = 'A quiet week at the front'
  else if (blueTook > redTook) headline = `${m.blue} gain ground`
  else if (redTook > blueTook) headline = `${m.red} push forward`
  else if (blueTook + redTook > 0) headline = 'The line holds, just'
  else headline = 'Stalemate in the air'

  const kills = m.totals.airKills + m.totals.groundKills
  const standBits: string[] = []
  if (m.totals.captures) standBits.push(`${plural(m.totals.captures, 'objective')} changed hands`)
  if (kills) standBits.push(`${plural(kills, 'kill')} claimed`)
  if (m.totals.activePilots) standBits.push(`${plural(m.totals.activePilots, 'pilot')} in the fight`)
  const span = m.days === 7 ? 'the week' : `the last ${plural(m.days, 'day')}`
  const standfirst = standBits.length
    ? `${standBits.join(', ').replace(/, ([^,]*)$/, ' and $1')} in ${span}`
    : `No objective changed hands in ${span}`

  const paras: string[] = []
  paras.push(`${m.blue} hold ${plural(B, 'objective')} and ${m.red} ${n(R)}. ` +
    (blueTook || redTook
      ? `This week ${m.blue} took ${n(blueTook)} and ${m.red} ${n(redTook)}.`
      : 'Neither side took ground this week.'))
  const story = m.events.filter(e => e.kind === 'story').slice(0, 2)
  if (ls) paras.push(lastStandLine(ls, { blue: m.blue, red: m.red }))
  if (story.length) paras.push(`${story.map(s => s.title).join('. ')}.`)
  if (m.contested[0]) {
    const c = m.contested[0]
    paras.push(`${c.name} changed hands ${c.captures === 2 ? 'twice' : `${n(c.captures)} times`}, more than anywhere else.`)
  }
  if (top) {
    paras.push(`${top.name}${top.squadron ? ` [${top.squadron}]` : ''} had the best week: ` +
      [top.air_kills && plural(top.air_kills, 'air kill'), top.ground_kills && plural(top.ground_kills, 'ground kill'),
        top.captures && plural(top.captures, 'capture'), top.deploys && plural(top.deploys, 'deployment')]
        .filter(Boolean).join(', ') + '.' +
      (sq && sq.tag !== top.squadron ? ` ${sq.name || sq.tag} [${sq.tag}] led the squadrons.` : ''))
  }
  if (m.will) paras.push(`Will to fight stands at ${m.blue} ${Math.round(m.will.blue)}, ${m.red} ${Math.round(m.will.red)}.`)
  return { headline, standfirst, body: paras.join('\n\n') }
}

/** The dispatch's paragraphs. */
export const paragraphs = (body: string) => body.split(/\n\s*\n/).map(p => p.trim()).filter(Boolean)

/** Discord's limit on a message. */
export const DISCORD_MAX = 2000

/** The text posted with the poster: short, for people scrolling past. */
export function postText(m: PosterModel, d: Dispatch, url?: string): string {
  const lines: string[] = []
  const when = m.day ? `Day ${m.day}` : m.week
  lines.push(`**${m.brand}: the week at the front** (${when}${m.roundNo ? `, Round ${m.roundNo}` : ''})`)
  lines.push(`**${d.headline}.** ${d.standfirst}.`.replace(/\.\./g, '.'))
  const top = m.pilots.slice(0, 3).map(p => `**${p.name}**`)
  if (top.length) lines.push(`Pilots of the week: ${top.join(', ')}`)
  if (m.squadrons[0]) lines.push(`Squadron of the week: **${m.squadrons[0].name || m.squadrons[0].tag}** [${m.squadrons[0].tag}]`)
  if (m.lastStand?.this_week && m.lastStand.outcome !== 'ordered') lines.push(lastStandLine(m.lastStand, { blue: m.blue, red: m.red }))
  if (url) lines.push(url)
  const text = lines.join('\n')
  return text.length > DISCORD_MAX ? `${text.slice(0, DISCORD_MAX - 1)}…` : text
}

/** POST /api/admin/ops/report/post: what the bot posts (cog_report.py). */
export interface ReportPostBody {
  instance: string
  server: string | null
  text: string
  /** base64 PNG (or JPEG), no data: prefix */
  image: string
  filename: string
}

/** bfdb's OPS proxy takes 4 MB of body; base64 is 4/3 of the bytes. */
export const MAX_IMAGE_B64 = 3_900_000

/** The Discord post for a poster snapshot (a data: URL), or an Error saying
 *  why it cannot go. */
export function reportPostBody(r: WeeklyReportResponse, m: PosterModel, text: string, dataUrl: string): ReportPostBody {
  const body = text.trim()
  if (!body) throw new Error('the post has no text')
  if (body.length > DISCORD_MAX) throw new Error(`the post text is ${body.length} characters; Discord takes ${DISCORD_MAX}`)
  const m2 = /^data:image\/(png|jpeg);base64,/.exec(dataUrl)
  if (!m2) throw new Error('the snapshot is not a PNG or JPEG')
  const image = dataUrl.slice(m2[0].length)
  if (image.length > MAX_IMAGE_B64) throw new Error('the poster is too big to send (over 4 MB)')
  const filename = m2[1] === 'jpeg' ? posterFilename(m).replace(/\.png$/, '.jpg') : posterFilename(m)
  return { instance: r.instance, server: r.dcs_server_name, text: body, image, filename }
}

/** "weekly-report-redstorm-2026-11-10.png" */
export function posterFilename(m: PosterModel): string {
  const day = new Date(m.until.getTime() - 1).toISOString().slice(0, 10)
  const slug = m.instance.toLowerCase().replace(/[^a-z0-9]+/g, '-').replace(/^-|-$/g, '')
  return `weekly-report-${slug || 'campaign'}-${day}.png`
}

/** The fonts each poster is set in (Google Fonts). */
export const POSTER_FONTS: Record<PosterTheme, string> = {
  redstorm: 'https://fonts.googleapis.com/css2?family=Stardos+Stencil:wght@400;700&family=Courier+Prime:wght@400;700&family=VT323&family=Old+Standard+TT:ital,wght@0,400;0,700;1,400&display=block',
  levant: 'https://fonts.googleapis.com/css2?family=Oswald:wght@500;600;700&family=Barlow:wght@400;500;600&family=Barlow+Condensed:wght@500;600;700&display=block',
}

/** Each poster's size, in CSS px. */
export const POSTER_SIZE: Record<PosterTheme, { width: number; height: number }> = {
  redstorm: { width: 1600, height: 900 },
  levant: { width: 1600, height: 1000 },
}
