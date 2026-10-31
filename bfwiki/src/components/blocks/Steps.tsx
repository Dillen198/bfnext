import { useId, useState } from 'react'
import type { Step } from './parse'
import { resolveSrc } from '../../lib/resolveSrc'

/**
 * The ```steps block: a walkthrough, one step at a time, with Back/Next, a
 * progress bar and numbered stops to jump between. Each step's text is
 * markdown, drawn by the page's own renderer so links and {{cfg:}} numbers
 * behave as they do everywhere else on the page.
 */
export default function Steps({
  steps,
  renderText,
}: {
  steps: Step[]
  renderText: (markdown: string) => React.ReactNode
}) {
  const [at, setAt] = useState(0)
  const uid = useId()
  const n = steps.length
  const cur = steps[Math.min(at, n - 1)]
  const pct = n > 1 ? (at / (n - 1)) * 100 : 100
  const titleId = `${uid}-title`

  return (
    <section className="mk-block mk-steps" aria-roledescription="walkthrough" aria-labelledby={titleId}>
      <div className="mk-steps-top">
        <ol className="mk-steps-dots">
          {steps.map((s, i) => (
            <li key={i}>
              <button
                type="button"
                className={`mk-steps-dot${i === at ? ' mk-on' : ''}${i < at ? ' mk-done' : ''}`}
                aria-current={i === at ? 'step' : undefined}
                aria-label={`Step ${i + 1}: ${s.title}`}
                title={s.title}
                onClick={() => setAt(i)}
              >
                {i + 1}
              </button>
            </li>
          ))}
        </ol>
        <div className="mk-steps-count">
          Step {at + 1} of {n}
        </div>
      </div>
      <div
        className="mk-steps-bar"
        role="progressbar"
        aria-label="Walkthrough progress"
        aria-valuemin={1}
        aria-valuemax={n}
        aria-valuenow={at + 1}
      >
        <div className="mk-steps-fill" style={{ width: `${pct}%` }} />
      </div>
      <div className="mk-steps-body" aria-live="polite">
        <p className="mk-steps-title" id={titleId}>
          <span className="mk-steps-num">{at + 1}</span> {cur.title}
        </p>
        {cur.image && <img className="mk-steps-img" src={resolveSrc(cur.image)} alt={cur.alt ?? cur.title} />}
        {cur.text && <div className="mk-steps-text">{renderText(cur.text)}</div>}
      </div>
      <div className="mk-steps-nav">
        <button type="button" className="vs-btn-ghost mk-steps-btn" disabled={at === 0} onClick={() => setAt(a => Math.max(0, a - 1))}>
          ‹ Back
        </button>
        {at < n - 1 ? (
          <button type="button" className="vs-btn mk-steps-btn" onClick={() => setAt(a => Math.min(n - 1, a + 1))}>
            Next ›
          </button>
        ) : (
          <button type="button" className="vs-btn-outline mk-steps-btn" disabled={n === 1} onClick={() => setAt(0)}>
            Start over
          </button>
        )}
      </div>
    </section>
  )
}
