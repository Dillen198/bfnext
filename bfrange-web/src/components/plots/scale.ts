import { useCallback, useEffect, useRef, useState, type PointerEvent as RPointerEvent } from 'react'

export interface Scale {
  (v: number): number
  invert(p: number): number
  domain: [number, number]
  range: [number, number]
}

export function linear(domain: [number, number], range: [number, number]): Scale {
  const [d0, d1] = domain
  const [r0, r1] = range
  const k = (r1 - r0) / (d1 - d0 || 1)
  const f = ((v: number) => r0 + (v - d0) * k) as Scale
  f.invert = (p: number) => d0 + (p - r0) / k
  f.domain = domain
  f.range = range
  return f
}

/** "Nice" tick values across [lo, hi]. */
export function ticks(lo: number, hi: number, n = 5): number[] {
  const span = hi - lo
  if (!(span > 0)) return [lo]
  const raw = span / n
  const mag = Math.pow(10, Math.floor(Math.log10(raw)))
  const step = [1, 2, 2.5, 5, 10].map(m => m * mag).find(s => span / s <= n) ?? 10 * mag
  const out: number[] = []
  for (let v = Math.ceil(lo / step) * step; v <= hi + step * 1e-9; v += step) out.push(Math.round(v / step) * step)
  return out
}

/** Round up to a tidy axis limit. */
export function niceCeil(v: number): number {
  if (v <= 0) return 1
  const mag = Math.pow(10, Math.floor(Math.log10(v)))
  for (const m of [1, 1.2, 1.5, 2, 2.5, 3, 4, 5, 6, 8, 10]) if (m * mag >= v) return m * mag
  return 10 * mag
}

/** SVG pointer position in viewBox units, or null when the pointer leaves. */
export function useSvgPointer<T extends SVGSVGElement>() {
  const ref = useRef<T>(null)
  const [pt, setPt] = useState<{ x: number; y: number } | null>(null)
  const onPointerMove = useCallback((e: RPointerEvent<T>) => {
    const svg = ref.current
    if (!svg) return
    const ctm = svg.getScreenCTM()
    if (!ctm) return
    const p = new DOMPoint(e.clientX, e.clientY).matrixTransform(ctm.inverse())
    setPt({ x: p.x, y: p.y })
  }, [])
  const onPointerLeave = useCallback(() => setPt(null), [])
  return { ref, pt, handlers: { onPointerMove, onPointerDown: onPointerMove, onPointerLeave } }
}

/** Play/pause a time cursor across [t0, t1] at `speed`x real time. */
export function usePlayback(t0: number, t1: number, speed = 4) {
  const [t, setT] = useState(t1)
  const [playing, setPlaying] = useState(false)
  const raf = useRef<number | null>(null)
  useEffect(() => {
    if (!playing) return
    let last = performance.now()
    const step = (now: number) => {
      const dt = ((now - last) / 1000) * speed
      last = now
      setT(cur => {
        const n = cur + dt
        if (n >= t1) {
          setPlaying(false)
          return t1
        }
        return n
      })
      raf.current = requestAnimationFrame(step)
    }
    raf.current = requestAnimationFrame(step)
    return () => {
      if (raf.current !== null) cancelAnimationFrame(raf.current)
    }
  }, [playing, t1, speed])
  const play = useCallback(() => {
    setT(cur => (cur >= t1 - 1e-6 ? t0 : cur))
    setPlaying(true)
  }, [t0, t1])
  const pause = useCallback(() => setPlaying(false), [])
  return { t, setT, playing, play, pause }
}

/** Index of the sample whose `t` is closest to (and not after) `t`. */
export function indexAtTime<S extends { t: number }>(samples: S[], t: number): number {
  let lo = 0
  let hi = samples.length - 1
  if (hi < 0) return -1
  if (t <= samples[0].t) return 0
  if (t >= samples[hi].t) return hi
  while (hi - lo > 1) {
    const mid = (lo + hi) >> 1
    if (samples[mid].t <= t) lo = mid
    else hi = mid
  }
  return lo
}
