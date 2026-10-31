import { Link } from 'react-router-dom'
import { Lock } from '@icons'
import { isActive, type FKey } from './keys'

/** One function key. A locked key still navigates (to login, or to the
 *  page's own explainer) rather than swallowing the click. */
function FunctionKey({ k, active }: { k: FKey; active: boolean }) {
  const shortcut = `Alt+${k.code.slice(1)}`
  return (
    // A plain Link: the lit key is decided here (isActive), not by the
    // router, so a locked key pointing at /login never lights on that page.
    <Link
      to={k.locked ? k.locked.go : k.to}
      className={['rsr-fkey', k.locked ? 'locked' : '', active ? 'active' : ''].filter(Boolean).join(' ')}
      aria-current={active ? 'page' : undefined}
      title={k.locked ? `${k.label} - ${k.locked.reason}` : `${k.label} (${shortcut})`}
      aria-keyshortcuts={shortcut}
    >
      <span className="rsr-fkey-code" aria-hidden="true">{k.code}</span>
      <span>{k.label}</span>
      {k.locked && <span className="rsr-fkey-lock" aria-label="locked"><Lock size={11} /></span>}
    </Link>
  )
}

/** The row of function keys, with the one for this page lit. */
export default function FunctionKeyStrip({ keys, pathname, search }: { keys: FKey[]; pathname: string; search: string }) {
  return (
    <div className="rsr-fkeys-scroll">
      {keys.map(k => (
        <FunctionKey key={k.code} k={k} active={!k.locked && isActive(k.to, pathname, search)} />
      ))}
    </div>
  )
}
