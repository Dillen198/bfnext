import { useEffect } from 'react'

/**
 * Load a page's Google Fonts when it first shows, once per stylesheet. Only
 * the hub needs every server's faces; each server page asks for its own.
 * Undefined loads nothing (a page that may or may not wear a theme).
 */
export function useFonts(href: string | undefined) {
  useEffect(() => {
    if (!href || document.querySelector(`link[data-fonts="${href}"]`)) return
    const link = document.createElement('link')
    link.rel = 'stylesheet'
    link.href = href
    link.dataset.fonts = href
    document.head.appendChild(link)
  }, [href])
}

export const FONTS = {
  desert: 'https://fonts.googleapis.com/css2?family=Oswald:wght@500;600;700&family=Barlow+Condensed:wght@500;600;700&family=Barlow:wght@400;500;600&display=swap',
  staff: 'https://fonts.googleapis.com/css2?family=Stardos+Stencil:wght@400;700&family=Courier+Prime:ital,wght@0,400;0,700;1,400&family=Barlow+Condensed:wght@500;600;700&display=swap',
  chart: 'https://fonts.googleapis.com/css2?family=Big+Shoulders+Stencil:opsz,wght@10..72,600..800&family=Archivo:wdth,wght@62..125,400..700&family=IBM+Plex+Mono:wght@400;500;600&display=swap',
} as const
