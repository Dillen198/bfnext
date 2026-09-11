import { useInstance } from '../context/InstanceContext'
import { Server } from 'lucide-react'

/**
 * Which DCS server's numbers this wiki is quoting.
 *
 * Renders nothing when the bfdb behind it fronts a single server — most
 * deployments — so a one-server setup never grows a control that does nothing.
 * With several, picking one re-fetches `/api/wiki/facts` and every
 * `{{cfg:...}}` on the page re-renders with that server's config values.
 */
export default function InstanceSelect({ style }: { style?: React.CSSProperties }) {
  const { instances, current, multi, select } = useInstance()
  if (!multi) return null

  return (
    <label
      title="Which DCS server this wiki quotes campaign numbers from"
      style={{ display: 'flex', alignItems: 'center', gap: 6, ...style }}
    >
      <Server size={13} style={{ color: 'var(--text-dim)' }} aria-hidden />
      <span className="sr-only">Server</span>
      <select
        className="wiki-instance-select"
        value={current?.id ?? ''}
        onChange={e => select(e.target.value)}
      >
        {instances.map(i => (
          <option key={i.id} value={i.id}>
            {i.label}
            {i.public === false ? ' (internal)' : ''}
          </option>
        ))}
      </select>
    </label>
  )
}
