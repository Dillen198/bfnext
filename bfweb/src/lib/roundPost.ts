// The Discord post an admin pastes next to a round's PDF: the result, the
// numbers, the names worth calling out. Written the way a server admin would
// write it by hand -- short lines, plain words, no emoji wall -- and each
// round picks its own phrasing (stable per round), so two posts in a row
// don't read like the same form filled in twice.

import type { Round, RoundReport, RoundSummary } from '../api'

export interface RoundPostInput {
  round: Pick<Round, 'id' | 'number' | 'name' | 'scenario' | 'start' | 'end' | 'active' | 'winner'>
  summary?: RoundSummary | null
  report?: RoundReport | null
  /** The campaign's names for the sides, "BLUFOR" and "REDFOR". */
  blue: string
  red: string
  /** A link to the round's page, put at the bottom when given. */
  url?: string
  /** For tests: what "now" is when the round is still on. */
  now?: number
}

const n = (v: number) => v.toLocaleString('en-US')
const times = (v: number) => (v === 1 ? 'once' : v === 2 ? 'twice' : `${v} times`)

function span(startIso: string, end: number): string {
  const h = Math.max(0, (end - new Date(startIso).getTime()) / 3_600_000)
  if (h < 1.5) return `${Math.max(1, Math.round(h * 60))} minutes`
  if (h < 48) return `${Math.round(h)} hours`
  const d = Math.round((h / 24) * 2) / 2
  return `${String(d)} days`
}

/** One of `xs`, the same one every time for this round and slot. */
function pick<T>(xs: readonly T[], seed: number, slot: number): T {
  const h = Math.abs(Math.imul(seed ^ (slot * 0x9e3779b1), 0x85ebca6b) ^ (seed >>> 13))
  return xs[h % xs.length]
}

export function roundPost({ round, summary: s, report: rep, blue, red, url, now = Date.now() }: RoundPostInput): string {
  const no = round.number ?? round.id
  const seed = round.id
  const scen = (round.name || round.scenario)?.trim()
  const dur = span(round.start, round.end ? new Date(round.end).getTime() : now)
  const W = round.winner === 'Blue' ? blue : round.winner === 'Red' ? red : null
  const L = round.winner === 'Blue' ? red : round.winner === 'Red' ? blue : null
  const out: string[] = []

  // headline and where/how long
  if (round.active) {
    out.push(pick([
      `**Round ${no} so far**`,
      `**Round ${no}, ${dur} in**`,
      `**Where Round ${no} stands**`,
    ], seed, 1))
    if (scen) out.push(pick([`${scen}, ${dur} in and still going.`, `Still going on ${scen} after ${dur}.`], seed, 2))
  } else if (W) {
    out.push(pick([
      `**Round ${no} is over. ${W} won it.**`,
      `**${W} won Round ${no}.**`,
      `**That's Round ${no} done, and it goes to ${W}.**`,
      `**Round ${no} goes to ${W}.**`,
    ], seed, 1))
    if (scen) {
      out.push(pick([
        `${scen}, ${dur} start to finish. Hard luck ${L}.`,
        `${dur} on ${scen} before ${L} ran out of road.`,
        `${scen} took ${dur} to settle.`,
      ], seed, 2))
    }
  } else {
    out.push(pick([`**Round ${no} is over, no winner this time.**`, `**Round ${no} ended without a winner.**`], seed, 1))
    if (scen) out.push(`${scen}, ${dur}.`)
  }

  // the numbers
  if (s && s.pilots > 0) {
    const hrs = n(Math.round(s.hours))
    out.push('')
    out.push(pick([
      `${n(s.pilots)} pilots put in ${hrs} hours between them: ${n(s.air_kills)} air kills, ${n(s.ground_kills)} ground kills, ${n(s.captures)} captures and ${n(s.supply_runs)} supply runs.`,
      `By the numbers: ${n(s.pilots)} pilots, ${hrs} hours flown, ${n(s.air_kills)} air-to-air kills, ${n(s.ground_kills)} on the ground, ${n(s.captures)} captures, ${n(s.supply_runs)} supply runs.`,
      `${n(s.pilots)} of you showed up and flew ${hrs} hours. That got us ${n(s.air_kills)} air kills, ${n(s.ground_kills)} ground kills, ${n(s.captures)} captures and ${n(s.supply_runs)} supply runs.`,
    ], seed, 3))
  }

  if (rep) {
    const last = rep.buckets[rep.buckets.length - 1]
    if (round.active && last) {
      out.push(`${blue} hold ${last.blue_held}, ${red} hold ${last.red_held} of ${rep.objectives} objectives.`)
    }
    const k = rep.buckets.reduce((a, b) => ({ b: a.b + b.blue_air + b.blue_ground, r: a.r + b.red_air + b.red_ground }), { b: 0, r: 0 })
    if (k.b + k.r > 0) {
      out.push(pick([
        `Kill board ${round.active ? 'is at' : 'finished'} ${blue} ${n(k.b)}, ${red} ${n(k.r)}.`,
        `${blue} killed ${n(k.b)}, ${red} killed ${n(k.r)}.`,
      ], seed, 4))
    }
    const [c0, c1] = rep.contested
    if (c0 && c0.captures > 1) {
      out.push(c1 && c1.captures > 1
        ? pick([
            `${c0.name} changed hands ${times(c0.captures)}, ${c1.name} ${times(c1.captures)}. Nobody wanted to let those go.`,
            `Most fought over was ${c0.name} (${times(c0.captures)}), then ${c1.name} (${times(c1.captures)}).`,
          ], seed, 5)
        : `${c0.name} changed hands ${times(c0.captures)}, more than anywhere else.`)
    }
  }

  // shoutouts: one line per pilot, so someone who led three things is named
  // once, the way a person would write it
  const mvp = s?.top[0]
  const mvpName = mvp ? mvp.name || mvp.ucid : null
  const L4 = s?.leaders
  const led = new Map<string, { title: string; said: string }[]>()
  if (L4) {
    const lines: [typeof L4.air, string, (v: string) => string][] = [
      [L4.air, 'top ace', v => `${v} air kills`],
      [L4.ground, 'most ground kills', v => `${v} ground kills`],
      [L4.captures, 'most captures', v => `${v} captures`],
      [L4.logistics, 'kept everyone supplied', v => `${v} logistics jobs`],
    ]
    for (const [l, title, say] of lines) {
      if (!l || l.value <= 0) continue
      led.set(l.name, [...(led.get(l.name) ?? []), { title, said: say(n(l.value)) }])
    }
  }
  const and = (xs: string[]) => (xs.length < 2 ? xs.join('') : `${xs.slice(0, -1).join(', ')} and ${xs[xs.length - 1]}`)
  const shout: string[] = []
  if (mvp && mvpName) {
    const bits = ([
      [mvp.air_kills, 'A/A'], [mvp.ground_kills, 'A/G'], [mvp.captures, 'captures'], [mvp.logistics, 'logi'],
    ] as const).filter(([v]) => v > 0).map(([v, w]) => `${n(v)} ${w}`)
    const titles = (led.get(mvpName) ?? []).map(t => t.title)
    led.delete(mvpName)
    shout.push(`MVP: **${mvpName}**${titles.length ? `, ${and(titles)}` : ''}${bits.length ? ` (${bits.join(', ')})` : ''}`)
  }
  for (const [who, ts] of led) {
    shout.push(ts.length === 1 ? `**${who}**, ${ts[0].title} (${ts[0].said})` : `**${who}**: ${and(ts.map(t => t.said))}`)
  }
  if (shout.length) {
    out.push('')
    out.push(pick(['Shoutouts:', 'Names to know:', 'Credit where it\'s due:'], seed, 6))
    for (const l of shout) out.push(`- ${l}`)
  }

  // sign-off
  out.push('')
  out.push(round.active
    ? pick(['Still all to play for. Get in there.', 'Plenty left in this one, see you on the server.', 'Report attached, more when it\'s over.'], seed, 7)
    : pick([
        'Full report attached. Thanks to everyone who flew, see you next round.',
        'Whole breakdown is in the PDF. Good flying all.',
        'Report\'s attached if you want the details. o7',
        'PDF has the rest. Thanks for flying, next round soon.',
      ], seed, 7))
  if (url) out.push(url)

  return out.join('\n')
}
