interface Props { side: string; size?: 'sm' | 'xs' }

const cfg: Record<string, { label: string; dot: string; text: string; bg: string }> = {
  Red:     { label: 'RED',     dot: 'bg-red-500',    text: 'text-red-400',    bg: 'bg-red-500/10 border-red-500/25' },
  Blue:    { label: 'BLUE',    dot: 'bg-blue-500',   text: 'text-blue-400',   bg: 'bg-blue-500/10 border-blue-500/25' },
  Neutral: { label: 'NEUT',   dot: 'bg-slate-500',  text: 'text-slate-400',  bg: 'bg-slate-500/10 border-slate-500/25' },
}

export default function SideBadge({ side, size = 'sm' }: Props) {
  const c = cfg[side] ?? cfg.Neutral
  const px = size === 'xs' ? 'px-1.5 py-0.5 text-[9px]' : 'px-2 py-0.5 text-[10px]'
  return (
    <span className={`inline-flex items-center gap-1 rounded border font-bold tracking-wider ${px} ${c.bg} ${c.text}`}>
      <span className={`w-1.5 h-1.5 rounded-full flex-shrink-0 ${c.dot}`} />
      {c.label}
    </span>
  )
}
