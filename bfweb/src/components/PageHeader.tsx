import type { ReactNode } from 'react'

interface Props {
  title: string
  sub?: string
  right?: ReactNode
}

export default function PageHeader({ title, sub, right }: Props) {
  return (
    <div
      className="flex items-center justify-between px-5 py-3.5 border-b border-[#1e3a5f]/50 flex-shrink-0"
      style={{ background: 'linear-gradient(180deg, #060d1c 0%, #040b16 100%)' }}
    >
      <div>
        <h1 className="text-[13px] font-bold tracking-[0.2em] text-slate-100 uppercase">{title}</h1>
        {sub && <p className="text-[10px] text-slate-600 mt-0.5 tracking-wide">{sub}</p>}
      </div>
      {right && <div className="flex items-center gap-3">{right}</div>}
    </div>
  )
}
