// A cartridge as cockpit pages: the sidebar of the cartridge editor. Each
// top-level section is a page, except a section that is just a folder of
// labelled sub-sections (the F-16's MPD holds NAV, DEST, GEO, THREAT, ROE and
// CMDS): its sub-sections become pages of their own, grouped under it.

import { getAt, toPointer, type AnnotationIndex, type Json, type JsonObject, type Seg } from '../model'
import { entriesOf, isMatrix, isMatrixList, isObj, isRecordTable, isScalar } from './core'
import { switchFor, type LoadSwitch } from './switches'

export interface Page {
  /** JSON Pointer of the page's node ("" for the general page). */
  key: string
  segs: Seg[]
  label: string
  help?: string
  /** The folder this page sits in, if any. */
  group?: string
  /** Filled by the exporter from the route; nothing to edit. */
  managed?: boolean
  /** Show only the node's own fields, not its sub-sections (which are pages). */
  scalarsOnly?: boolean
  /** The switch that loads it into the jet, if it has one. */
  sw?: LoadSwitch
}

const hasLabel = (idx: AnnotationIndex, segs: Seg[]) => !!idx.get(segs)?.label || !!idx.section(String(segs[0]))

export function buildPages(value: Json, idx: AnnotationIndex, switches: LoadSwitch[], hidden: Set<string>): Page[] {
  if (!isObj(value)) return []
  const pages: Page[] = []
  const general: string[] = []
  const skip = (segs: Seg[]) => {
    const a = idx.get(segs)
    return !!a?.hidden || hidden.has(toPointer(segs)) || a?.managed === 'name' || a?.managed === 'terrain'
  }
  const sectionOrder = (idx.ann.sections ?? []).map(s => s.path)
  const top = Object.keys(value).sort((x, y) => {
    const rx = sectionOrder.indexOf(x)
    const ry = sectionOrder.indexOf(y)
    return (rx < 0 ? 999 : rx) - (ry < 0 ? 999 : ry)
  })

  for (const k of top) {
    const segs: Seg[] = [k]
    if (skip(segs)) continue
    const v = value[k]
    const a = idx.get(segs)
    const label = idx.section(k)?.label ?? a?.label ?? k
    const help = idx.section(k)?.help ?? a?.help
    if (isScalar(v)) { general.push(k); continue }
    const folder = isObj(v) && !isRecordTable(v, a) && !isMatrixList(v) && !isMatrix(v)
    const kids = folder
      ? entriesOf(v, a).filter(([ck, cv]) => !isScalar(cv) && !skip([k, ck]))
      : []
    const labelled = kids.filter(([ck]) => hasLabel(idx, [k, ck]))
    if (folder && kids.length >= 2 && labelled.length >= 2) {
      const own = Object.entries(v as JsonObject).filter(([ck, cv]) => isScalar(cv) && !skip([k, ck]))
      if (own.length) pages.push({ key: toPointer(segs), segs, label: `${label}: general`, help, group: label, scalarsOnly: true, sw: switchFor(switches, toPointer(segs)) })
      for (const [ck] of kids) {
        const cs: Seg[] = [k, ck]
        const ca = idx.get(cs)
        pages.push({
          key: toPointer(cs), segs: cs, group: label,
          label: ca?.label ?? String(ck), help: ca?.help,
          managed: ca?.managed === 'route',
          sw: switchFor(switches, toPointer(cs)),
        })
      }
    } else {
      pages.push({ key: toPointer(segs), segs, label, help, managed: a?.managed === 'route', sw: switchFor(switches, toPointer(segs)) })
    }
  }
  if (general.length) {
    pages.push({ key: '', segs: [], label: 'Cartridge settings', scalarsOnly: true, help: 'Settings that apply to the whole cartridge.' })
  }
  return pages
}

/** Overrides at or under a page. */
export function pageChanges(page: Page, overrides: Record<string, unknown>): number {
  if (page.key === '') return Object.keys(overrides).filter(k => k.split('/').length === 2).length
  return Object.keys(overrides).filter(k => k === page.key || k.startsWith(page.key + '/')).length
}

/** Whether the route fills anything on a page (managed fields inside it). */
export function routeFilled(page: Page, idx: AnnotationIndex, value: Json): boolean {
  if (page.managed) return true
  const v = getAt(value, page.segs)
  if (!isObj(v)) return false
  return Object.keys(v).some(k => idx.get([...page.segs, k])?.managed === 'route')
}
