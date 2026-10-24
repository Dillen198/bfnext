import { useEffect, useRef, useState } from 'react'

// Fires once when the element first scrolls into view, then stops observing.
// rootMargin pulls the trigger point up slightly so reveals start just before
// the element is fully on screen rather than snapping in at the last second.
// threshold is a fraction of the TARGET's own height, not the viewport's — for
// very tall sections (e.g. a ~6000px block on a ~800px mobile viewport) even a
// small percentage can be physically impossible to satisfy, which permanently
// stuck those sections (and visually everything after them) at opacity 0. Use
// 0 so a single visible pixel is enough, independent of the element's size.
export function useInView<T extends HTMLElement>(threshold = 0) {
  const ref = useRef<T | null>(null)
  // Without IntersectionObserver there is nothing to wait for: start visible.
  const [inView, setInView] = useState(() => typeof IntersectionObserver === 'undefined')

  useEffect(() => {
    const el = ref.current
    if (!el || inView) return
    const observer = new IntersectionObserver(
      ([entry]) => {
        if (entry.isIntersecting) {
          setInView(true)
          observer.disconnect()
        }
      },
      { threshold, rootMargin: '0px 0px -10% 0px' }
    )
    observer.observe(el)
    return () => observer.disconnect()
  }, [threshold, inView])

  return { ref, inView }
}
