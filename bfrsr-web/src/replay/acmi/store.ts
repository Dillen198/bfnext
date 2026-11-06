// A local recording, wholly in memory, as the viewer's TrackSource.

import { sampleSeg, trailOf, type RecMeta, type Seg, type State, type TrackSource } from '../data'

/** Finest bucket of the presence index (ms); each level is 4x the last. */
const BASE_MS = 10_000

/**
 * Who is alive when, for 10k+ objects: each object goes in the level whose
 * bucket size is at least its lifetime, in the (at most two) buckets of
 * that level it overlaps. A lookup reads one bucket per level, so it sees
 * only objects alive near `t`, and the index holds two entries per object.
 */
class Presence {
  private levels: { size: number; buckets: number[][] }[] = []

  constructor(meta: RecMeta) {
    const objs = meta.objects
    for (let i = 0; i < objs.length; i++) {
      const { t0, t1 } = objs[i]
      const d = Math.max(0, t1 - t0)
      let lv = 0, size = BASE_MS
      while (size < d) { lv++; size *= 4 }
      while (this.levels.length <= lv) this.levels.push({ size: BASE_MS * 4 ** this.levels.length, buckets: [] })
      const b = this.levels[lv].buckets
      const from = Math.floor(t0 / size), to = Math.floor(t1 / size)
      for (let k = Math.max(0, from); k <= to; k++) (b[k] ??= []).push(i)
    }
  }

  at(t: number, meta: RecMeta): number[] {
    const out: number[] = []
    const objs = meta.objects
    for (const { size, buckets } of this.levels) {
      const b = buckets[Math.floor(t / size)]
      if (!b) continue
      for (const i of b) {
        const o = objs[i]
        if (o.t0 <= t && t <= o.t1) out.push(i)
      }
    }
    return out
  }
}

/** A recording read from a local .acmi: everything is loaded. */
export class LocalTrackStore implements TrackSource {
  readonly meta: RecMeta
  readonly name: string
  readonly tracks: Map<number, Seg>
  version = 0
  onChange: (() => void) | null = null
  error: string | null = null
  private presence: Presence

  constructor(data: { meta: RecMeta; tracks: Map<number, Seg> }, name = '') {
    this.meta = data.meta
    this.tracks = data.tracks
    this.name = name
    this.presence = new Presence(data.meta)
  }

  around(): void {}

  isLoaded(): boolean {
    return true
  }

  present(t: number): number[] {
    return this.presence.at(t, this.meta)
  }

  state(idx: number, t: number): State | null {
    const o = this.meta.objects[idx]
    if (!o || t < o.t0 || t > o.t1) return null
    const s = this.tracks.get(idx)
    return s ? sampleSeg(s, t) : null
  }

  wholeTrack(idx: number): Seg | null {
    return this.tracks.get(idx) ?? null
  }

  trail(idx: number, t: number, len: number, step = 0): [number, number, number][] {
    const s = this.tracks.get(idx)
    return trailOf(s ? [s] : [], t, len, step, this.state(idx, t))
  }
}
