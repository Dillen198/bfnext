/**
 * Animated backdrop: an LED dot matrix drifting under a slow scan bar, sitting
 * behind the whole app. A display, not a radar scope -- a sweep implies the
 * page is a sensor picture, which is the map's job, not the dashboard's.
 *
 * The performance constraint shapes the whole thing. Every frame here is a
 * compositor transform -- a dot cell translated by exactly one cell so it
 * loops seamlessly, and one gradient band travelling down. Nothing
 * animates a property that would force layout or repaint (no background-position,
 * no opacity pulsing on a large surface, no box-shadow, no canvas, no rAF loop),
 * so the main thread does no work at all once the layers are painted once.
 *
 * It is decoration, so it yields: `prefers-reduced-motion` stops the motion and
 * leaves the static grid, and the light theme drops the sweep entirely (a green
 * glow over a white dashboard reads as a rendering fault, not an effect).
 */
export default function Backdrop() {
  return (
    <div className="vs-backdrop" aria-hidden="true">
      <div className="vs-backdrop-grid" />
      <div className="vs-backdrop-sweep" />
    </div>
  )
}
