import { Link } from 'react-router-dom'
import { ArrowRight } from '@icons'
import { SERVERS, type ServerProfile } from '../config/servers'
import { StatusTag } from '../components/Bits'
import { FONTS, useFonts } from '@shared/servers/fonts'
import { useServerLive } from './useServerLive'
import { TileArt } from '@shared/servers/TileArt'
import '@shared/servers/tiles.css'

function Tile({ p }: { p: ServerProfile }) {
  const live = useServerLive(p)
  return (
    <Link to={`/servers/${p.slug}`} className={`tile tile-${p.theme}`}>
      <TileArt p={p} />
      <div className="tile-body">
        <div className="tile-top">
          <span className="tile-label">{p.label}</span>
          {p.instanceId ? <StatusTag state={live.state} /> : <span className="status tone-idle">Practice</span>}
        </div>
        <h3 className="tile-title">{p.title}</h3>
        <div className="tile-era">{p.era}</div>
        <p className="tile-summary">{p.summary}</p>
        <div className="tile-foot">
          <span className="tile-meta">
            {p.instanceId
              ? live.round
                ? `Round ${live.round.n}${live.round.day ? ` · day ${live.round.day}` : ''}${live.airborne.known ? ` · ${live.airborne.total} on` : ''}`
                : `${p.blueLabel} v ${p.redLabel}`
              : 'Everything graded'}
          </span>
          <span className="tile-cta">Briefing <ArrowRight size={13} /></span>
        </div>
      </div>
    </Link>
  )
}

/** Every server as a tile in its own look. `exclude` leaves out the page you are on. */
export default function ServerTiles({ exclude }: { exclude?: string }) {
  useFonts(FONTS.desert)
  useFonts(FONTS.staff)
  useFonts(FONTS.chart)
  const list = SERVERS.filter(s => s.slug !== exclude)
  return (
    <div className="tiles" style={{ ['--n' as string]: list.length }}>
      {list.map(p => <Tile key={p.slug} p={p} />)}
    </div>
  )
}
