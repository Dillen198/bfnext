// The posters' web fonts, loaded on demand: the dashboards don't otherwise
// carry the Red Storm poster's typewriter faces or the Levant poster's.

import { useEffect, useState } from 'react'
import { POSTER_FONTS, type PosterTheme } from './weekly'

/** Load a poster's fonts once; true when they are in (or after 4 s). */
export function usePosterFonts(theme: PosterTheme): boolean {
  const [ready, setReady] = useState(false)
  useEffect(() => {
    const href = POSTER_FONTS[theme]
    let link = document.querySelector<HTMLLinkElement>(`link[data-poster-fonts="${theme}"]`)
    if (!link) {
      link = document.createElement('link')
      link.rel = 'stylesheet'
      link.href = href
      link.dataset.posterFonts = theme
      document.head.appendChild(link)
    }
    let live = true
    const done = () => { if (live) setReady(true) }
    const t = setTimeout(done, 4000)
    const fams = theme === 'redstorm'
      ? ['400 20px "Stardos Stencil"', '400 20px "Courier Prime"', '700 20px "Courier Prime"', '400 20px VT323', '700 20px "Old Standard TT"', 'italic 400 20px "Old Standard TT"', '400 20px "Old Standard TT"']
      : ['700 20px Oswald', '600 20px Oswald', '400 20px Barlow', '500 20px Barlow', '600 20px "Barlow Condensed"', '700 20px "Barlow Condensed"']
    const onLoad = () => Promise.all(fams.map(f => document.fonts.load(f).catch(() => undefined)))
      .then(() => document.fonts.ready).then(() => { clearTimeout(t); done() })
    if (link.sheet) void onLoad()
    else link.addEventListener('load', () => void onLoad(), { once: true })
    return () => { live = false; clearTimeout(t) }
  }, [theme])
  return ready
}
