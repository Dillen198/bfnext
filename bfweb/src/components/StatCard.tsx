import type { ReactNode } from 'react'

interface Props {
  label: string
  value: ReactNode
  sub?: string
  color?: string
}

export default function StatCard({ label, value, sub, color = 'text-blue-400' }: Props) {
  return (
    <div style={{ background: 'var(--bg-card)', border: '1px solid var(--border)', borderRadius: 12, padding: '1.25rem' }}>
      <div className="uppercase tracking-widest" style={{ fontSize: '0.75rem', color: 'var(--text-dim)', marginBottom: '0.5rem' }}>{label}</div>
      <div className={`text-3xl font-bold ${color}`}>{value}</div>
      {sub && <div style={{ fontSize: '0.75rem', color: 'var(--text-dim)', marginTop: '0.25rem' }}>{sub}</div>}
    </div>
  )
}
