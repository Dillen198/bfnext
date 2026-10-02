import type { ReactNode } from 'react'
import { useNavigate } from 'react-router-dom'
import { TONE_VAR, fmtDateTime, qualityTone, scoreTone, type Tone } from '../lib/format'
import { gradeName, gradeStyle } from '../lib/lso'
import { PASS_OUTCOME_LABEL } from '../lib/headline'
import type { GreeniePass } from '../types'

/** A quality word (SHACK, DEADEYE, PERFECT ...) on the site's score scale. */
export function QualityChip({ q }: { q: string }) {
  const tone = qualityTone(q)
  return <ToneChip tone={tone}>{q}</ToneChip>
}

export function ToneChip({ tone, children }: { tone: Tone; children: ReactNode }) {
  const c = TONE_VAR[tone]
  return (
    <span className="chip" style={{ color: c, background: `color-mix(in srgb, ${c} 16%, transparent)` }}>
      {children}
    </span>
  )
}

/** A 0..5 score, coloured on the scale. */
export function ScoreChip({ score }: { score: number | null }) {
  if (score === null) return <span className="chip outline">NO SCORE</span>
  return <ToneChip tone={scoreTone(score)}>{score.toFixed(1)}</ToneChip>
}

/** The LSO grade as the board would write it. */
export function GradeBadge({ grade, large = false }: { grade: string; large?: boolean }) {
  const s = gradeStyle(grade)
  return (
    <span
      className="mono inline-grid place-items-center font-bold"
      title={gradeName(grade)}
      style={{
        background: s.bg,
        color: s.fg,
        minWidth: large ? 64 : 34,
        height: large ? 44 : 22,
        padding: '0 6px',
        fontSize: large ? 22 : 12,
        borderRadius: 2,
        border: grade === 'C' ? '1px solid var(--line-2)' : undefined,
      }}
    >
      {grade}
    </span>
  )
}

/** One greenie-board square. */
export function GreenieSquare({ pass }: { pass: GreeniePass }) {
  const nav = useNavigate()
  const s = gradeStyle(pass.grade)
  const tip = `${fmtDateTime(pass.ts)} · ${pass.grade} (${gradeName(pass.grade)})${pass.wire ? ` · #${pass.wire} wire` : ''} · ${PASS_OUTCOME_LABEL[pass.outcome] ?? pass.outcome} · Case ${pass.case}${pass.night ? ' · night' : ''}`
  return (
    <button
      className={`gsq${pass.grade === 'C' ? ' cut' : ''}`}
      style={{ background: s.bg, color: s.fg }}
      title={tip}
      aria-label={tip}
      onClick={() => nav(`/result/${encodeURIComponent(pass.id)}`)}
    >
      {pass.grade.slice(0, 4)}
      {pass.night && <span className="night" style={{ borderRightColor: s.fg }} />}
      {pass.wire && <span className="wire">{pass.wire}</span>}
    </button>
  )
}
