import type { ReactNode } from 'react'

interface Props {
  label: string
  value: ReactNode
  sub?: string
  color?: string
}

export default function StatCard({ label, value, sub, color = 'text-blue-400' }: Props) {
  return (
    <div className="bg-[#111827] border border-[#1e3a5f] rounded-xl p-5">
      <div className="text-xs text-slate-500 uppercase tracking-widest mb-2">{label}</div>
      <div className={`text-3xl font-bold ${color}`}>{value}</div>
      {sub && <div className="text-xs text-slate-500 mt-1">{sub}</div>}
    </div>
  )
}
