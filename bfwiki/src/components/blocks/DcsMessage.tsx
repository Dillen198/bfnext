/**
 * The box DCS prints trigger and script messages in, top right of the
 * screen: light text on a translucent dark band, wrapped to the box, line
 * breaks kept as written.
 */
export function DcsMessageBox({ text, live = false }: { text: string; live?: boolean }) {
  return (
    <div className="mk-dcsmsg" role={live ? 'status' : undefined}>
      {text}
    </div>
  )
}

/** A slice of the DCS screen, for the replicas to sit on. */
export function DcsScreen({ children, label, className = '' }: { children: React.ReactNode; label: string; className?: string }) {
  return (
    <figure className={`mk-screen ${className}`} aria-label={label}>
      {children}
    </figure>
  )
}

/** The ```dcsmessage block: the message box alone, where DCS puts it. */
export default function DcsMessage({ text }: { text: string }) {
  return (
    <div className="mk-block">
      <DcsScreen label="DCS message" className="mk-screen--msg">
        <div className="mk-screen-tr">
          <DcsMessageBox text={text} />
        </div>
      </DcsScreen>
    </div>
  )
}
