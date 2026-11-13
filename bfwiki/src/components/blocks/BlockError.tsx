/**
 * What a mockup block shows when its body does not parse: which block, what
 * is wrong and where, and the source underneath -- so the author sees their
 * mistake and the reader still sees the text, never a blank section.
 */
export default function BlockError({ lang, error, source }: { lang: string; error: string; source: string }) {
  return (
    <div className="mk-error">
      <p className="mk-error-head">
        <strong>This <code>{lang}</code> block could not be drawn:</strong> {error}.
      </p>
      <pre className="mk-error-src">
        <code>{source}</code>
      </pre>
    </div>
  )
}
