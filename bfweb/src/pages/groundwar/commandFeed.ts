// The command map's feeds and the plain values its components share
// (kept apart from `command.tsx` so that file only exports components).
import { useEffect, useMemo, useState } from 'react'
import { useQuery } from '@tanstack/react-query'
import { api, connectTacmap, type AirTrack, type Asset, type CommandPicture, type OrderTarget, type TacPicture } from '../../api'
import type { CommandMock } from '../commandMock'
import type { Side } from './theme'

// ── Feeds ────────────────────────────────────────────────────────────────

/** Our side's assets, every 3 s (bfdb shares one engine read between viewers). */
export function useCommandFeed(side: Side | undefined, instance: string | undefined, mock: CommandMock | null, enabled: boolean) {
  return useQuery<CommandPicture>({
    queryKey: ['command', instance, side ?? '', mock ? 'mock' : ''],
    queryFn: () => (mock ? Promise.resolve(mock.picture()) : api.command.picture(side)),
    refetchInterval: 3000,
    enabled,
    retry: false,
  })
}

/** The enemy aircraft our side can see (/ws/tacmap, fog-of-war). */
export function useEnemyAir(enabled: boolean): AirTrack[] {
  const [pic, setPic] = useState<TacPicture | null>(null)
  useEffect(() => {
    if (!enabled) return
    return connectTacmap((f) => setPic(f.picture), () => {})
  }, [enabled])
  return useMemo(() => (pic?.air ?? []).filter((t) => t.iff !== 'friendly' && !t.stale), [pic])
}

export interface Layers {
  air: boolean
  ground: boolean
  naval: boolean
  logi: boolean
  enemyAir: boolean
}
export const ALL_LAYERS: Layers = { air: true, ground: true, naval: true, logi: true, enemyAir: true }

export function shown(a: Asset, l: Layers): boolean {
  switch (a.kind) {
    case 'air': return l.air
    case 'naval': return l.naval
    case 'convoy': return l.logi
    default: return l.ground
  }
}

export type AssetMode = 'none' | 'move' | 'fire' | 'station' | 'sail' | 'barrage' | 'fmove'

export const MODE_TEXT: Record<AssetMode, string> = {
  none: '',
  move: 'MOVE · CLICK THE MAP',
  fire: 'FIRE · CLICK THE TARGET',
  station: 'STATION · CLICK THE MAP',
  sail: 'SAIL · CLICK OPEN WATER',
  barrage: 'BARRAGE · CLICK THE TARGET (EVERY BATTERY IN RANGE)',
  fmove: 'MOVE FORMATIONS · CLICK THE MAP',
}

/** How the map asks for an order's target, for the mode bar. */
export const TARGET_TEXT: Record<OrderTarget, string> = {
  land: 'CLICK A POINT ON LAND',
  point: 'CLICK A POINT',
  sea: 'CLICK A POINT AT SEA',
  own_base: 'CLICK ONE OF OUR BASES',
  enemy_base: 'CLICK AN ENEMY BASE',
  transfer: 'CLICK THE BASE TO SEND FROM, THEN THE BASE TO SEND TO',
}
