// What the campaign already knows that a cartridge can take, so a pilot
// does not type it in: the comms card's presets, the side's known threats,
// its own airfields and NDBs. Plain logic; the panel is MissionPanel.tsx.
//
// Left out on purpose: Hornet TACAN stations and MiG-29 airfields/RSBN must
// match DCS's own beacon and runway databases exactly (anything else is
// deleted on load), which the dashboard does not have.

import type { CommsChannel, MapObjective, NavaidEntry, SituationReport, TacPicture, ThreatArea } from '../../api'
import { getAt, type Json, type JsonObject, type Seg } from '../model'
import { distanceNm, toDcsXY, type DcsTerrain } from '../nav'
import type { Target } from '../targets'
import { MAX_CHANNELS, type Plan, type PlanChannel } from '../types'
import type { Ctx } from './core'
import { numbered } from './places'

export interface MissionData {
  side: 'Blue' | 'Red'
  comms: CommsChannel[]
  threats: ThreatArea[]
  objectives: MapObjective[]
  navaids: NavaidEntry[]
  /** The whole briefing report, for the cartridge map's picture. */
  report: SituationReport | null
  /** A live picture handed in (the mock); otherwise the map streams it. */
  tac?: TacPicture | null
}

export interface MissionFill {
  id: 'comms' | 'threats' | 'dest' | 'adf'
  title: string
  /** What goes in, in a pilot's words. */
  detail: string
  /** The cartridge page it lands on, to open after filling. */
  pageSegs: Seg[] | null
  count: number
  /** Cartridge changes... */
  writes?: [Seg[], Json][]
  /** ...or the plan's own presets, which every cartridge of the plan uses. */
  comms?: { comm1: PlanChannel[]; comm2: PlanChannel[] }
  /** The cartridge already says this. */
  done: boolean
  /** Entries there now that filling replaces. */
  replaces: number
}

/** The comms card's numbered presets as the plan's COMM 1 (UHF, 225 MHz up)
 *  and COMM 2 (the rest). */
export function commsFromCard(comms: CommsChannel[]): { comm1: PlanChannel[]; comm2: PlanChannel[] } {
  const comm1: PlanChannel[] = []
  const comm2: PlanChannel[] = []
  for (const c of comms) {
    if (!c.preset || c.preset < 1 || c.preset > MAX_CHANNELS) continue
    const list = c.freq_mhz >= 225 ? comm1 : comm2
    if (list.some(x => x.preset === c.preset)) continue
    list.push({
      preset: c.preset,
      freq_mhz: c.freq_mhz,
      label: c.label.slice(0, 24),
      modulation: c.modulation.toUpperCase().startsWith('FM') ? 'FM' : 'AM',
    })
  }
  const byPreset = (a: PlanChannel, b: PlanChannel) => a.preset - b.preset
  return { comm1: comm1.sort(byPreset), comm2: comm2.sort(byPreset) }
}

const sameComms = (a: PlanChannel[], b: PlanChannel[]) =>
  a.length === b.length && a.every((c, i) => c.preset === b[i].preset && c.freq_mhz === b[i].freq_mhz && c.modulation === b[i].modulation)

/** Where a cartridge keeps its radio presets, if it has any. */
const RADIO_PAGES: Seg[][] = [['COMM'], ['Presets', 'COMM'], ['Program_1', 'Radio'], ['Radios']]

/** Three letters for a place on a display: "Kutaisi" -> "KUT". */
export function shortName(name: string, n = 3): string {
  return name.toUpperCase().replace(/[^A-Z0-9]/g, '').slice(0, n) || 'PT'
}

/** A threat's label on a display: "4x air defence" -> "4AD". */
function threatText(t: ThreatArea, n: number): string {
  const m = /^(\d+)x/.exec(t.label)
  return (m ? `${m[1]}AD` : 'AD').slice(0, n)
}

/** Everything the mission can fill in this cartridge. */
export function missionFills(ctx: Ctx, target: Target, plan: Plan, map: DcsTerrain | null, m: MissionData): MissionFill[] {
  const out: MissionFill[] = []
  const native = target.tool === 'native'
  const listAt = (segs: Seg[]) => (getAt(ctx.value, segs) as Json[] | undefined) ?? []
  const tmplAt = (segs: Seg[]) => (ctx.idx.get(segs)?.item ?? null) as JsonObject | null
  const at = (lat: number, lon: number) => (map ? toDcsXY(lat, lon, map.terrain) : null)

  // Radio presets: the plan's, written into whichever radios the jet has.
  const radios = RADIO_PAGES.find(s => getAt(ctx.value, s) !== undefined)
  const card = commsFromCard(m.comms)
  const presets = card.comm1.length + card.comm2.length
  if (radios && presets) {
    out.push({
      id: 'comms', title: 'Radio presets from the comms card',
      detail: `${presets} preset${presets > 1 ? 's' : ''}: ${card.comm1.length} UHF on COMM 1, ${card.comm2.length} VHF/FM on COMM 2. They go into every cartridge of this plan.`,
      pageSegs: radios, count: presets, comms: card,
      done: plan.include_comms && sameComms(plan.comm1, card.comm1) && sameComms(plan.comm2, card.comm2),
      replaces: plan.comm1.length + plan.comm2.length,
    })
  }

  const threats = m.threats.filter(t => t.lat || t.lon)
  // F-16: pre-planned threats 56-70, as Custom rings sized by the intel.
  if (native && target.code === 'F-16C_50' && threats.length && map) {
    const segs: Seg[] = ['MPD', 'THREAT_PTS']
    const tmpl = tmplAt(segs)
    if (tmpl) {
      const list: JsonObject[] = []
      for (const t of threats.slice(0, ctx.idx.get(segs)?.maxItems ?? 15)) {
        const p = numbered(list, tmpl)
        Object.assign(p, at(t.lat, t.lon), { def_num: 1, threatName: 'Custom', text: threatText(t, 3), ring: true, elev: 0 })
        if (t.radius_m) p.radius = Math.round(t.radius_m)
        list.push(p)
      }
      out.push(threatFill(segs, list, listAt(segs), threats.length, 'pre-planned threats 56-70'))
    }
  }
  // Hornet: SA format MEZ rings.
  if (native && target.code === 'FA-18C_hornet' && threats.length && map) {
    const segs: Seg[] = ['SA', 'MEZ_THRTS']
    const tmpl = tmplAt(segs)
    if (tmpl) {
      const list: JsonObject[] = []
      for (const t of threats.slice(0, ctx.idx.get(segs)?.maxItems ?? 40)) {
        const p = numbered(list, tmpl)
        const nm = t.radius_m ? Math.min(500, Math.max(1, Math.round((t.radius_m / 1852) * 10) / 10)) : 20
        Object.assign(p, at(t.lat, t.lon), { threat_type: 'Custom', threat_ring_radius: nm, text: threatText(t, 5) })
        list.push(p)
      }
      out.push(threatFill(segs, list, listAt(segs), threats.length, 'MEZ threat rings on the SA format'))
    }
  }

  // F-16: the side's airfields and FARPs as destination points 81-99.
  const bases = m.objectives.filter(o => o.owner === m.side && (o.lat || o.lon) && /airbase|farp|fob/i.test(o.kind))
  if (native && target.code === 'F-16C_50' && bases.length && map) {
    const segs: Seg[] = ['MPD', 'DEST']
    const tmpl = tmplAt(segs)
    if (tmpl) {
      const from = plan.waypoints[0]
      const rank = (o: MapObjective) => (/airbase/i.test(o.kind) ? 0 : /farp/i.test(o.kind) ? 1 : 2)
      const dist = (o: MapObjective) => (from ? distanceNm([from.lat, from.lon], [o.lat, o.lon]) : 0)
      const pick = [...bases].sort((a, b) => rank(a) - rank(b) || dist(a) - dist(b) || a.name.localeCompare(b.name))
        .slice(0, ctx.idx.get(segs)?.maxItems ?? 19)
      const list: JsonObject[] = []
      for (const o of pick) {
        const p = numbered(list, tmpl)
        Object.assign(p, at(o.lat, o.lon), { alt: 0, text: shortName(o.name), note: o.name.slice(0, 30) })
        list.push(p)
      }
      const now = listAt(segs)
      out.push({
        id: 'dest', title: 'Your airfields and FARPs as destination points',
        detail: `${list.length} of your side's bases as DEST 81${list.length > 1 ? `-${80 + list.length}` : ''}, airfields first${from ? ', nearest your route first' : ''}.`,
        pageSegs: segs, count: list.length, writes: [[segs, list]],
        done: sameXY(now, list), replaces: now.length,
      })
    }
  }

  // Apache: NDBs as mission 1's ADF presets.
  const ndbs = m.navaids.filter(n => n.ndb_khz != null && n.ndb_khz >= 100 && n.ndb_khz <= 2199.5)
  if (native && target.code === 'AH-64D_BLK_II' && ndbs.length) {
    const segs: Seg[] = ['NAV', 'Mission_1', 'ADF']
    const adf = getAt(ctx.value, segs) as JsonObject | undefined
    if (adf) {
      const writes: [Seg[], Json][] = []
      ndbs.slice(0, 10).forEach((n, i) => {
        const call = /\b([A-Z]{2,3})$/.exec(n.tacan ?? '')?.[1] ?? shortName(n.objective)
        writes.push([[...segs, `Preset_${i + 1}`], { Freq: n.ndb_khz!, ID: call.replace(/[^A-Z]/g, '').slice(0, 3) }])
      })
      writes.push([[...segs, 'isEnabled'], true])
      out.push({
        id: 'adf', title: 'NDBs as ADF presets',
        detail: `${Math.min(10, ndbs.length)} of your side's NDBs as mission 1 ADF presets 1-${Math.min(10, ndbs.length)}.`,
        pageSegs: ['NAV', 'Mission_1'], count: Math.min(10, ndbs.length), writes,
        done: writes.every(([s, v]) => JSON.stringify(getAt(ctx.value, s)) === JSON.stringify(v)),
        replaces: 0,
      })
    }
  }
  return out
}

function threatFill(segs: Seg[], list: JsonObject[], now: Json[], known: number, where: string): MissionFill {
  const more = known > list.length ? ` (the ${list.length} surest of ${known})` : ''
  return {
    id: 'threats', title: 'Known threats from intel',
    detail: `${list.length} air-defence site${list.length > 1 ? 's' : ''}${more} as ${where}, ringed by the intel's range. Intel gives no type, so each is Custom.`,
    pageSegs: segs, count: list.length, writes: [[segs, list]],
    done: sameXY(now, list), replaces: now.length,
  }
}

const sameXY = (a: Json[], b: JsonObject[]) =>
  a.length === b.length && a.every((x, i) => {
    const p = x as JsonObject
    return Math.abs((p.x as number) - (b[i].x as number)) < 1 && Math.abs((p.y as number) - (b[i].y as number)) < 1
  })
