/** Seeded PRNG so the fixtures are identical on every reload. */
export class Rng {
  private s: number
  constructor(seed: number) {
    this.s = seed >>> 0 || 0x9e3779b9
  }
  /** mulberry32, [0, 1) */
  next(): number {
    let t = (this.s += 0x6d2b79f5)
    t = Math.imul(t ^ (t >>> 15), t | 1)
    t ^= t + Math.imul(t ^ (t >>> 7), t | 61)
    return ((t ^ (t >>> 14)) >>> 0) / 4294967296
  }
  range(lo: number, hi: number): number {
    return lo + (hi - lo) * this.next()
  }
  int(lo: number, hi: number): number {
    return Math.floor(this.range(lo, hi + 1))
  }
  pick<T>(xs: readonly T[]): T {
    return xs[Math.floor(this.next() * xs.length)]
  }
  chance(p: number): boolean {
    return this.next() < p
  }
  /** Standard normal (Box–Muller). */
  gauss(mean = 0, sd = 1): number {
    const u = 1 - this.next()
    const v = this.next()
    return mean + sd * Math.sqrt(-2 * Math.log(u)) * Math.cos(2 * Math.PI * v)
  }
  /** Weighted pick from [item, weight] pairs. */
  weighted<T>(xs: readonly (readonly [T, number])[]): T {
    const total = xs.reduce((a, [, w]) => a + w, 0)
    let r = this.next() * total
    for (const [x, w] of xs) {
      r -= w
      if (r <= 0) return x
    }
    return xs[xs.length - 1][0]
  }
}

/** A stable 32-bit hash of a string, for per-entity seeds. */
export function hash(s: string): number {
  let h = 2166136261
  for (let i = 0; i < s.length; i++) {
    h ^= s.charCodeAt(i)
    h = Math.imul(h, 16777619)
  }
  return h >>> 0
}
