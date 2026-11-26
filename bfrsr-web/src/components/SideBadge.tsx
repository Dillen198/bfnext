import { campaign } from '../config/campaign'

interface Props { side: string; size?: 'sm' | 'xs' }

/** A side's tag, in its own ink: NATO steel blue, Soviet red. The names are
 *  the server's (`campaign.blueLabel` / `redLabel`). */
export default function SideBadge({ side, size = 'sm' }: Props) {
  const label = side === 'Blue' ? campaign.blueLabel : side === 'Red' ? campaign.redLabel : 'Neut'
  const color = side === 'Blue' ? 'var(--blue)' : side === 'Red' ? 'var(--red)' : 'var(--text-dim)'
  const bg = side === 'Blue' ? 'var(--blue-bg)' : side === 'Red' ? 'var(--red-bg)' : 'transparent'
  const px = size === 'xs' ? 'px-1.5 py-0.5 text-[9px]' : 'px-2 py-0.5 text-[10px]'
  return (
    <span
      className={`inline-flex items-center gap-1 border font-bold tracking-wider uppercase ${px}`}
      style={{ color, background: bg, borderColor: 'currentColor', borderRadius: 2, fontFamily: 'var(--font-mono)' }}
    >
      <span className="w-1.5 h-1.5 flex-shrink-0" style={{ background: color, transform: 'rotate(45deg)' }} />
      {label}
    </span>
  )
}
