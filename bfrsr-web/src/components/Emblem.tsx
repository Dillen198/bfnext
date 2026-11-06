import type { CSSProperties } from 'react'
import { BRAND } from '../config/redstorm'

/**
 * The campaign emblem: a storm front over a compass ring (our own art, no
 * real insignia). Both variants render and CSS shows the one for the theme
 * (the .logo-mark rules in index.css). `onDark` forces the light-on-dark
 * mark, for places that are dark in either theme, like the console bar.
 */
export function Emblem({ size = 32, alt = '', style, onDark = false }: {
  size?: number
  alt?: string
  style?: CSSProperties
  onDark?: boolean
}) {
  const s: CSSProperties = { width: size, height: size, objectFit: 'contain', flexShrink: 0, display: 'block', ...style }
  if (onDark) return <img src={BRAND.emblem.onDark} alt={alt} style={s} />
  return (
    <>
      <img src={BRAND.emblem.onDark} alt={alt} className="logo-mark logo-mark--light" style={s} />
      <img src={BRAND.emblem.onLight} alt={alt} className="logo-mark logo-mark--dark" style={s} />
    </>
  )
}
