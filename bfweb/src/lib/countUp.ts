import { useEffect, useRef, useState } from 'react'

const REDUCED = typeof window !== 'undefined' && typeof window.matchMedia === 'function'
  && window.matchMedia('(prefers-reduced-motion: reduce)').matches

/**
 * A number that runs up to `target` when it first shows, and glides to each
 * new value after that, easing out over `ms`. The front-line bars drive both
 * their percentages and their fills from it, so the two move together.
 * With reduced motion asked for, it is simply `target`.
 */
export function useCountUp(target: number, ms = 1200): number {
  const [value, setValue] = useState(0)
  const current = useRef(0)
  useEffect(() => {
    if (REDUCED) return
    const from = current.current
    const start = performance.now()
    let raf = 0
    const step = (t: number) => {
      const k = Math.min(1, (t - start) / ms)
      const eased = 1 - Math.pow(1 - k, 3)
      current.current = from + (target - from) * eased
      setValue(current.current)
      if (k < 1) raf = requestAnimationFrame(step)
    }
    raf = requestAnimationFrame(step)
    return () => cancelAnimationFrame(raf)
  }, [target, ms])
  return REDUCED ? target : value
}
