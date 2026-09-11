/**
 * Animated backdrop: a drifting tactical grid with a slow radar sweep over it,
 * sitting behind the whole app.
 *
 * The performance constraint shapes the whole thing. Every frame here is a
 * compositor transform -- a grid tile translated by exactly one tile so it
 * loops seamlessly, and one pre-rasterised conic gradient rotating. Nothing
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
