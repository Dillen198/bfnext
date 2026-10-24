import type { CSSProperties } from 'react'

interface Props {
  size?: number
  style?: CSSProperties
  alt?: string
}

/**
 * Campaign logo mark. Renders the light-on-dark logo by default and swaps to
 * the dark-on-light variant under [data-theme="light"] (see the .logo-mark
 * rules in index.css). Both files live in /public — vs-vectorstrike_hd-white.png (dark themes)
 * and vs-vectorstrike_hd-dark.png (light themes).
 */
export default function LogoMark({ size = 28, style, alt = '' }: Props) {
  const s: CSSProperties = { width: size, height: size, objectFit: 'contain', flexShrink: 0, ...style }
  return (
    <>
      <img src="/vs-vectorstrike_hd-white.png" alt={alt} className="logo-mark logo-mark--light" style={s} />
      <img src="/vs-vectorstrike_hd-dark.png" alt={alt} className="logo-mark logo-mark--dark" style={s} />
    </>
  )
}
