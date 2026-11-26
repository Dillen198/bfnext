import { ArrowRight, ExternalLink } from '@icons'
import type { ServerProfile } from '../config/servers'
import { links, wiki } from '../config/campaign'
import { BrowserName, NoticeBox } from '../components/Bits'
import { COMPLEX, DECKS, INTRO, LEGEND, MARKINGS, RULES, SECTORS, TANKERS, TRAINER } from '@shared/servers/range'
import ServerTiles from './ServerTiles'
import '@shared/servers/themes.css'
import { FONTS, useFonts } from '@shared/servers/fonts'
import { useServerLive } from './useServerLive'
import './range.css'

const TOC = [
  ['complex', 'The complex'],
  ['sectors', 'Sectors'],
  ['tankers', 'Tankers & decks'],
  ['story', 'Story'],
  ['fly', 'Fly'],
] as const

/** A compass rose in chart magenta, the way an aeronautical chart draws one. */
function Rose() {
  const c = 150
  return (
    <svg className="rg-rose" viewBox="0 0 300 300" role="img" aria-label="Compass rose">
      <circle className="rg-rose-ring" cx={c} cy={c} r="130" />
      <circle className="rg-rose-ring" cx={c} cy={c} r="104" />
      {Array.from({ length: 72 }, (_, i) => {
        const a = (i * 5 * Math.PI) / 180
        const r2 = i % 18 === 0 ? 100 : i % 2 === 0 ? 118 : 123
        return <line key={i} className="rg-rose-tick" x1={c + 130 * Math.sin(a)} y1={c - 130 * Math.cos(a)} x2={c + r2 * Math.sin(a)} y2={c - r2 * Math.cos(a)} />
      })}
      {[0, 30, 60, 90, 120, 150, 180, 210, 240, 270, 300, 330].map(d => {
        const a = (d * Math.PI) / 180
        return (
          <text key={d} className="rg-rose-num" x={c + 90 * Math.sin(a)} y={c - 90 * Math.cos(a) + 4}>
            {d === 0 ? 'N' : String(d / 10).padStart(2, '0')}
          </text>
        )
      })}
      <path className="rg-rose-north" d={`M${c} ${c - 62} L${c + 8} ${c} L${c} ${c - 8} L${c - 8} ${c} Z`} />
      <circle className="rg-rose-hub" cx={c} cy={c} r="3" />
    </svg>
  )
}

export default function RangePage({ profile }: { profile: ServerProfile }) {
  useFonts(FONTS.chart)
  const live = useServerLive(profile)
  return (
    <div className="srv srv-range">
      <header className="rg-hero">
        <div className="rg-graticule" aria-hidden="true" />
        <div className="wrap rg-hero-grid">
          <div>
            <div className="rg-eyebrow">VS · Range control · 13°N 145°E</div>
            <h1 className="rg-title">Training Range<span>Mariana Islands</span></h1>
            <p className="rg-lede">{INTRO}</p>
            <div className="rg-chips">
              <span>No campaign</span><span>No lives</span><span>No points</span><span>Every slot open</span><span>Everything graded</span>
            </div>
            <div className="rg-ctas">
              <a className="rg-btn" href={profile.site.url}>{profile.site.label} <ArrowRight size={14} /></a>
              <a className="rg-btn ghost" href="#fly">How to join</a>
            </div>
          </div>
          <Rose />
        </div>
        <nav className="wrap rg-toc" aria-label="On this page">
          {TOC.map(([id, label]) => <a key={id} href={`#${id}`}>{label}</a>)}
        </nav>
      </header>

      {live.notices.some(n => n.level !== 'info') && (
        <div className="wrap rg-notices">{live.notices.filter(n => n.level !== 'info').map(n => <NoticeBox key={n.id} notice={n} />)}</div>
      )}

      <section id="complex" className="rg-section">
        <div className="wrap rg-complex">
          <div>
            <h2 className="rg-h"><span>A</span>The Mariana Islands Range Complex</h2>
            <p className="rg-p">{COMPLEX}</p>
            <p className="rg-p">{MARKINGS}</p>
            <h3 className="rg-h3">Fly it like the real thing</h3>
            <ol className="rg-rules">{RULES.map(r => <li key={r}>{r}</li>)}</ol>
          </div>
          <aside className="rg-legend" aria-label="Map colour key">
            <div className="rg-legend-h">Legend · F10 map</div>
            <ul>
              {LEGEND.map(l => (
                <li key={l.kind}><span className={`rg-swatch k-${l.kind}`} />{l.use}<em>{l.colour}</em></li>
              ))}
              <li><span className="rg-line solid" />Low-level route<em>White line</em></li>
              <li><span className="rg-line csar" />CSAR<em>Lime dashed</em></li>
              <li><span className="rg-line dashed" />Pattern<em>White dashed</em></li>
            </ul>
          </aside>
        </div>
      </section>

      <section id="sectors" className="rg-section">
        <div className="wrap">
          <h2 className="rg-h"><span>B</span>Sector directory</h2>
          <p className="rg-p rg-narrow">F10 &gt; Range &gt; Sectors lists them nearest first with bearing and range.</p>
          <div className="rg-dir">
            {SECTORS.map(g => (
              <div key={g.group} className="rg-group">
                <h3 className="rg-group-h">{g.group}</h3>
                <table className="rg-table">
                  <tbody>
                    {g.rows.map(r => (
                      <tr key={r.id}>
                        <th scope="row"><span className={`rg-swatch k-${r.kind}`} />{r.id}</th>
                        <td>{r.what}</td>
                      </tr>
                    ))}
                  </tbody>
                </table>
              </div>
            ))}
          </div>
        </div>
      </section>

      <section id="tankers" className="rg-section">
        <div className="wrap rg-two">
          <div>
            <h2 className="rg-h"><span>C</span>Tankers</h2>
            <div className="rg-scroll">
              <table className="rg-table rg-tankers">
                <thead><tr><th>Track</th><th>Type</th><th>TACAN</th><th>Freq</th><th>Alt</th><th>Speed</th></tr></thead>
                <tbody>
                  {TANKERS.map(t => (
                    <tr key={t.track}>
                      <th scope="row"><span className="rg-swatch k-tanker" />{t.track}</th>
                      <td>{t.type}{t.note && <small> · {t.note}</small>}</td>
                      <td className="m">{t.tacan}</td><td className="m">{t.freq}</td><td className="m">{t.alt}</td><td className="m">{t.speed}</td>
                    </tr>
                  ))}
                </tbody>
              </table>
            </div>
          </div>
          <div>
            <h2 className="rg-h"><span>D</span>Decks</h2>
            <ul className="rg-decks">
              {DECKS.map(d => (
                <li key={d.aircraft}>
                  <span className="rg-hull">{d.ship}</span>
                  <span className="rg-ac">{d.aircraft}<small>{d.role}</small></span>
                  <span className="rg-cs">{d.callsign}</span>
                </li>
              ))}
            </ul>
            <div className="rg-trainer">
              <div className="rg-legend-h">Missile trainer</div>
              <p>{TRAINER}</p>
            </div>
          </div>
        </div>
      </section>

      <section id="story" className="rg-section">
        <div className="wrap">
          <h2 className="rg-h"><span>E</span>Story</h2>
          <div className="rg-nostory">
            <p><strong>None, by design.</strong> The range is a practice server: no campaign, no sides to win and no story events. What it keeps is your record: every bomb, pass and landing, graded, on the range site.</p>
            <a className="rg-link" href={profile.site.url}>Results and the greenie board <ArrowRight size={12} /></a>
          </div>
        </div>
      </section>

      <section id="fly" className="rg-section">
        <div className="wrap rg-two">
          <div>
            <h2 className="rg-h"><span>F</span>Fly</h2>
            <p className="rg-p">Find the server in the DCS multiplayer browser. Every slot is open; launch from Andersen, Won Pat or the decks.</p>
            <BrowserName name={profile.browserName} />
          </div>
          <ul className="rg-links">
            <li><a href={profile.site.url}>{profile.site.label} <ArrowRight size={12} /></a></li>
            <li><a href={wiki(`servers/${profile.slug}`)}>Field guide in the wiki <ArrowRight size={12} /></a></li>
            <li><a href={links.discord} target="_blank" rel="noopener noreferrer">Discord <ExternalLink size={11} /></a></li>
          </ul>
        </div>
      </section>

      <section className="rg-section rg-others">
        <div className="wrap">
          <h2 className="rg-h"><span>·</span>Other servers</h2>
          <ServerTiles exclude={profile.slug} />
        </div>
      </section>
    </div>
  )
}
