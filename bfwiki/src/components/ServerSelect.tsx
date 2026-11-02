import { SERVERS } from '@shared/servers/profiles'
import { Server } from '@icons'
import { useServerLook } from '../context/ServerLookContext'
import { useInstance } from '../context/InstanceContext'

/**
 * The top bar's server picker: the wiki's look, and with it which campaign's
 * numbers the pages quote. Other servers this bfdb fronts (ones the site has
 * no page for) are still here, for their numbers, in the plain look.
 */
export default function ServerSelect() {
  const { server, choose } = useServerLook()
  const { instances, current, select } = useInstance()
  const others = instances.filter(i => !SERVERS.some(s => s.instanceId === i.id))
  const value = server ? server.slug : current && others.some(o => o.id === current.id) ? `i:${current.id}` : ''

  const onChange = (v: string) => {
    if (v.startsWith('i:')) {
      choose(undefined)
      select(v.slice(2))
    } else {
      choose(v || undefined)
    }
  }

  return (
    <label title="Which server this wiki is about" style={{ display: 'flex', alignItems: 'center', gap: 6 }}>
      <Server size={13} style={{ color: 'var(--text-dim)' }} aria-hidden />
      <span className="sr-only">Server</span>
      <select className="wiki-instance-select" value={value} onChange={e => onChange(e.target.value)}>
        <option value="">All servers</option>
        {SERVERS.map(s => <option key={s.slug} value={s.slug}>{s.title}</option>)}
        {others.map(i => (
          <option key={i.id} value={`i:${i.id}`}>
            {i.label}{i.public === false ? ' (internal)' : ''}
          </option>
        ))}
      </select>
    </label>
  )
}
