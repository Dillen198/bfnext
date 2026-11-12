// Can this browser draw a MapLibre map at all?
//
// MapLibre needs a WebGL context (WebGL 2, or 1 as a fallback). DCS World's
// embedded browser -- where the cockpit panel shows this dashboard -- starts
// Chromium 106 with --disable-gpu and ships without the software renderer's
// Vulkan manifest, so it has neither: every MapLibre map there was an empty
// box. The pages that use one ask here first and show the Leaflet map
// (components/TacMap, no WebGL) instead.
//
// `?nogl=1` pretends there is no WebGL, to see the fallback in a normal
// browser.

export interface WebGLSupport {
  ok: boolean
  /** Why not, when the browser said: the context-creation error's message. */
  why?: string
}

let cached: WebGLSupport | undefined

function probe(): WebGLSupport {
  try {
    if (new URLSearchParams(window.location.search).get('nogl') === '1') {
      return { ok: false, why: 'turned off with ?nogl=1' }
    }
  } catch { /* no URL: carry on and ask the browser */ }
  try {
    const canvas = document.createElement('canvas')
    let why: string | undefined
    canvas.addEventListener('webglcontextcreationerror', (e) => {
      why = (e as WebGLContextEvent).statusMessage || why
    }, { once: true })
    // The same attributes MapLibre asks with, so the answer is MapLibre's.
    const attrs: WebGLContextAttributes = { failIfMajorPerformanceCaveat: false }
    const gl = canvas.getContext('webgl2', attrs) ?? canvas.getContext('webgl', attrs)
    if (!gl) return { ok: false, why: why || 'the browser offers no WebGL' }
    // Let the context go now rather than when the canvas is collected:
    // browsers cap how many can be live at once.
    gl.getExtension('WEBGL_lose_context')?.loseContext()
    return { ok: true }
  } catch (e) {
    return { ok: false, why: e instanceof Error ? e.message : String(e) }
  }
}

export function webglSupport(): WebGLSupport {
  cached ??= probe()
  return cached
}

/** For tests: forget the cached answer. */
export function resetWebglSupport(): void {
  cached = undefined
}
