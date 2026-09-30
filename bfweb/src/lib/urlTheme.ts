export type Theme = 'dark' | 'light'

/** `?theme=dark|light` wins over the stored choice and the OS setting: the
 *  Discord snapshot is rendered by a headless browser with a fresh profile
 *  (which reports a light OS theme) and must come out dark. */
export function urlTheme(): Theme | null {
  const t = new URLSearchParams(window.location.search).get('theme')
  return t === 'dark' || t === 'light' ? t : null
}
