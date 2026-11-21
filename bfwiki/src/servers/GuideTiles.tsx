import { Link } from 'react-router-dom'
import { ArrowRight } from '@icons'
import { SERVERS } from '@shared/servers/profiles'
import { FONTS, useFonts } from '@shared/servers/fonts'
import { TileArt } from '@shared/servers/TileArt'
import '@shared/servers/tiles.css'

/** Every server's field guide, as the site's tiles in each server's own look. */
export default function GuideTiles() {
  useFonts(FONTS.desert)
  useFonts(FONTS.staff)
  useFonts(FONTS.chart)
  return (
    <div className="tiles" style={{ ['--n' as string]: SERVERS.length }}>
      {SERVERS.map(p => (
        <Link key={p.slug} to={`/servers/${p.slug}`} className={`tile tile-${p.theme}`}>
          <TileArt p={p} />
          <div className="tile-body">
            <div className="tile-top"><span className="tile-label">{p.label}</span></div>
            <h3 className="tile-title">{p.title}</h3>
            <div className="tile-era">{p.era}</div>
            <p className="tile-summary">{p.summary}</p>
            <div className="tile-foot">
              <span className="tile-meta">Briefing, story and rules</span>
              <span className="tile-cta">Field guide <ArrowRight size={13} /></span>
            </div>
          </div>
        </Link>
      ))}
    </div>
  )
}
