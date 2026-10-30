import { useState } from 'react'
import './blocks.css'
import { BLOCK_EXAMPLES, fenced } from './examples'

/**
 * The editor's help panel: every mockup block with a copy-paste example.
 * Only the editor shows it, and the editor is admin-only.
 */
export default function BlocksReference({ onInsert }: { onInsert?: (text: string) => void }) {
  const [copied, setCopied] = useState<string | null>(null)

  async function copy(lang: string, text: string) {
    try {
      await navigator.clipboard.writeText(text)
      setCopied(lang)
      setTimeout(() => setCopied(c => (c === lang ? null : c)), 1500)
    } catch {
      // No clipboard (an insecure origin, a refused permission): the
      // example is on screen to select by hand.
      setCopied(null)
    }
  }

  return (
    <details className="mk-ref">
      <summary>Blocks: F10 menu, message, panel, wheel, walkthrough</summary>
      <div className="mk-ref-body">
        <p className="mk-ref-intro">
          Fenced code blocks whose language is one of these draw as live replicas of the game. Paste an example and edit
          it; the preview draws it as you type. A block whose JSON is wrong shows a red box saying where.
        </p>
        {BLOCK_EXAMPLES.map(ex => {
          const text = fenced(ex)
          return (
            <section key={ex.lang} className="mk-ref-item" aria-label={`${ex.name} block`}>
              <div className="mk-ref-head">
                <code>```{ex.lang}</code>
                <strong>{ex.name}</strong>
                <span className="mk-ref-actions">
                  <button type="button" className="vs-btn-ghost mk-ref-btn" onClick={() => copy(ex.lang, text)}>
                    {copied === ex.lang ? 'COPIED' : 'COPY'}
                  </button>
                  {onInsert && (
                    <button type="button" className="vs-btn-outline mk-ref-btn" onClick={() => onInsert(`\n${text}\n`)}>
                      INSERT
                    </button>
                  )}
                </span>
              </div>
              <p className="mk-ref-about">{ex.about}</p>
              <pre className="mk-ref-code">{text}</pre>
            </section>
          )
        })}
      </div>
    </details>
  )
}
