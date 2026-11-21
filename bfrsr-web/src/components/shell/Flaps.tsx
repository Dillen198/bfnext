/** A split-flap readout: one tile per character, a hinge across each. */
export default function Flaps({ text, className = '', label }: { text: string; className?: string; label?: string }) {
  return (
    <span className={`rsr-flaps ${className}`} role="img" aria-label={label ?? text}>
      {[...text].map((ch, i) => (
        <span key={i} className={`rsr-flap${ch === ':' || ch === ' ' ? ' rsr-flap--sep' : ''}`} aria-hidden="true">{ch}</span>
      ))}
    </span>
  )
}
