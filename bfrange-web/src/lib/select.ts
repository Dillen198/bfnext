/**
 * One name for each thing on the live range, shared by the map, the control
 * dock and the search palette (`/?focus=<key>`), so picking a tanker in any
 * of them selects the same tanker everywhere.
 *
 *   player:<ucid>  station:<id>  tanker:<id>  carrier:<id>  deck:<id>
 *   hz:<id>        site:<iads id>/<site id>   jam:<id>      spawn:<id>
 */
import type { RangeLive } from '../types'
import type { LatLon } from './geo'

export type DockTab = 'feed' | 'traffic' | 'boat' | 'threats' | 'stations'

/** Where a selected thing is now, and the zoom that shows it with its surroundings. */
export function locate(live: RangeLive, key: string): { pos: LatLon; zoom: number } | null {
  const [kind, ...rest] = key.split(':')
  const id = rest.join(':')
  switch (kind) {
    case 'player': { const p = live.players.find(x => x.ucid === id); return p ? { pos: p.pos, zoom: 10.5 } : null }
    case 'station': { const s = live.stations.find(x => x.id === id); return s ? { pos: s.pos, zoom: 12 } : null }
    case 'tanker': { const t = live.tankers.find(x => x.id === id); return t ? { pos: t.pos, zoom: 9.5 } : null }
    case 'carrier': { const c = live.carriers.find(x => x.id === id); return c ? { pos: c.pos, zoom: 11 } : null }
    case 'deck': { const d = (live.ship_decks ?? []).find(x => x.id === id); return d ? { pos: d.pos, zoom: 11.5 } : null }
    case 'hz': { const h = (live.hot_zones ?? []).find(x => x.id === id); return h ? { pos: h.pos, zoom: 9.5 } : null }
    case 'jam': { const j = (live.jammers ?? []).find(x => x.id === id); return j ? { pos: j.pos, zoom: 10 } : null }
    case 'spawn': { const s = live.spawns.find(x => x.id === id); return s ? { pos: s.pos, zoom: 10 } : null }
    case 'site': {
      const [net, site] = id.split('/')
      const s = (live.iads ?? []).find(n => n.id === net)?.sites.find(x => x.id === site)
      return s ? { pos: s.pos, zoom: 11 } : null
    }
  }
  return null
}

/** Which dock tab lists a selected thing. */
export function tabFor(key: string): DockTab {
  const kind = key.split(':')[0]
  if (kind === 'station') return 'stations'
  if (kind === 'carrier' || kind === 'deck') return 'boat'
  if (kind === 'hz' || kind === 'site' || kind === 'jam') return 'threats'
  return 'traffic'
}
