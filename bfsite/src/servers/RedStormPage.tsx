import { Fragment } from 'react'
import { ArrowRight, ExternalLink } from '@icons'
import type { ServerProfile } from '../config/servers'
import { links, wiki } from '../config/campaign'
import { BrowserName, NoticeBox, StatusTag, TerritoryBar } from '../components/Bits'
import { AIRCRAFT, ENDING, FLYING, GLANCE, ORDERS, SITUATION } from '@shared/servers/redstorm'
import { STORY, WILL_COSTS } from '@shared/servers/redstormStory'
import ServerTiles from './ServerTiles'
import '@shared/servers/themes.css'
import { FONTS, useFonts } from '@shared/servers/fonts'
import { useServerLive } from './useServerLive'
import './redstorm.css'

const TOC = [
  ['situation', '1. Situation'],
  ['mission', '2. Mission'],
  ['ends', '3. How it ends'],
  ['story', '4. Story'],
  ['flying', '5. Flying'],
] as const

const sideName = (s: 'Blue' | 'Red') => (s === 'Blue' ? 'NATO' : 'Soviet')

/** "{by}" in an announcement is the pilots the engine credits: shown as a redaction. */
function Announcement({ text }: { text: string }) {
  const parts = text.split('{by}')
  return (
    <p className="rs-ev-text">
      {parts.map((p, i) => (
        <Fragment key={i}>
          {i > 0 && <span className="rs-redact" title="The pilots credited">pilots credited</span>}
          {p}
        </Fragment>
      ))}
    </p>
  )
}

/** The front's three parts as a staff overlay: NATO west, the Front east. */
function Overlay() {
  const rows = [
    { y: 46, name: 'Coast', blue: ['Gagra', 'Gudauta'], red: ['Sochi'], river: 'Psou' },
    { y: 116, name: 'Centre', blue: ['Zestafoni', 'Sachhere'], red: ['Surami', 'Gori', 'Tskhinvali'] },
    { y: 186, name: 'Mountains', blue: ['Ambrolauri'], red: ['Kazbegi'], river: 'Mamison rd' },
  ]
  return (
    <svg className="rs-overlay" viewBox="0 0 520 230" role="img" aria-label="Staff overlay of the front's three parts: on the coast Gagra and Gudauta face Sochi; in the centre Zestafoni and Sachhere face Surami, Gori and Tskhinvali; in the mountains Ambrolauri faces Kazbegi">
      <rect className="rs-ov-paper" x="1" y="1" width="518" height="228" />
      {Array.from({ length: 12 }, (_, i) => <line key={`v${i}`} className="rs-ov-grid" x1={40 + i * 40} y1="1" x2={40 + i * 40} y2="229" />)}
      {Array.from({ length: 5 }, (_, i) => <line key={`h${i}`} className="rs-ov-grid" x1="1" y1={40 + i * 40} x2="519" y2={40 + i * 40} />)}
      <path className="rs-ov-feba" d="M262 8 C250 50 276 80 258 116 S272 170 254 222" />
      <text className="rs-ov-feba-l" x="270" y="20">FEBA</text>
      {rows.map(r => (
        <g key={r.name}>
          <text className="rs-ov-axis" x="12" y={r.y - 18}>{r.name.toUpperCase()}</text>
          {r.blue.map((n, i) => {
            const x = 224 - (r.blue.length - 1 - i) * 74
            return (
              <g key={n} className="rs-ov-unit b">
                <rect x={x - 22} y={r.y - 12} width="44" height="24" />
                <path d={`M${x - 22} ${r.y - 12} L${x + 22} ${r.y + 12} M${x + 22} ${r.y - 12} L${x - 22} ${r.y + 12}`} />
                <text x={x} y={r.y + 26}>{n}</text>
              </g>
            )
          })}
          {r.red.map((n, i) => {
            const x = 300 + i * 74
            return (
              <g key={n} className="rs-ov-unit r">
                <path d={`M${x} ${r.y - 16} L${x + 18} ${r.y} L${x} ${r.y + 16} L${x - 18} ${r.y} Z`} />
                <text x={x} y={r.y + 30}>{n}</text>
              </g>
            )
          })}
          <path className="rs-ov-arrow r" d={`M${300 - 6} ${r.y - 22} q-14 -2 -24 6`} />
        </g>
      ))}
      <text className="rs-ov-note" x="508" y="222" textAnchor="end">OVERLAY 1 · NOT TO SCALE</text>
    </svg>
  )
}

export default function RedStormPage({ profile }: { profile: ServerProfile }) {
  useFonts(FONTS.staff)
  const live = useServerLive(profile)
  return (
    <div className="srv srv-redstorm">
      <div className="rs-classif" aria-hidden="true">Secret · NATO eyes only · Red Storm Rising</div>
      <header className="rs-hero">
        <div className="wrap">
          <div className="rs-folder">
            <div className="rs-folder-tab">RSR / SF-85</div>
            <div className="rs-stamp" aria-hidden="true">Secret</div>
            <div className="rs-hero-grid">
              <div>
                <div className="rs-eyebrow">Red Storm Rising · Fowl Engine dynamic campaign · Caucasus</div>
                <h1 className="rs-title">Southern Flank <span>1985</span></h1>
                <div className="rs-dtg">DTG 150600 JUN 85</div>
                <p className="rs-lede">
                  The war in Central Europe is a week old. On its southern flank a US carrier group is in the Black Sea,
                  and <b className="nato">NATO</b> has come ashore in western Georgia to cut the supply lines of the Soviet{' '}
                  <b className="ussr">Transcaucasus Front</b>. You fly for one side in a war that carries on between
                  sessions until one side can’t go on.
                </p>
                <div className="rs-ctas">
                  <a className="rs-btn" href={profile.site.url}>{profile.site.label} <ArrowRight size={14} /></a>
                  <a className="rs-btn ghost" href="#flying">How to join</a>
                </div>
              </div>
              <Overlay />
            </div>
          </div>
          <div className="rs-sitrep" role="group" aria-label="Live state">
            <span className="rs-k">Sitrep</span>
            <StatusTag state={live.state} />
            <span>{live.round ? `Round ${live.round.n}${live.round.day ? `, day ${live.round.day}` : ''}` : 'No round on the wire'}</span>
            <span>{live.airborne.known ? `${live.airborne.total} pilots up (${live.airborne.blue} NATO / ${live.airborne.red} Soviet)` : 'Pilots: no report'}</span>
            <span>Lives {profile.livesEnforced ? 'enforced' : 'unlimited'}</span>
            <div className="rs-terr">
              <TerritoryBar front={live.up ? live.front : undefined} loading={live.up && live.frontLoading} blueLabel="NATO" redLabel="Soviet" />
            </div>
          </div>
          {live.notices.filter(n => n.level !== 'info').map(n => <NoticeBox key={n.id} notice={n} />)}
          <nav className="rs-toc" aria-label="On this page">
            {TOC.map(([id, label]) => <a key={id} href={`#${id}`}>{label}</a>)}
          </nav>
        </div>
      </header>

      <section id="situation" className="rs-section">
        <div className="wrap rs-sit">
          <div className="rs-sheet">
            <h2 className="rs-h"><span>1.</span>Situation</h2>
            {SITUATION.map((p, i) => (
              <p key={i} className="rs-para">
                <span className="rs-pn">{String.fromCharCode(97 + i)}.</span>
                {p.lead && <b>{p.lead} </b>}
                {p.body}
              </p>
            ))}
          </div>
          <aside className="rs-card rs-glance">
            <h3 className="rs-card-h">At a glance</h3>
            <dl>
              {GLANCE.map(g => (
                <div key={g.k}>
                  <dt>{g.k}</dt>
                  <dd>
                    {g.both ?? (
                      <>
                        <span className="nato">{g.blue}</span>
                        <span className="rs-sep"> · </span>
                        <span className="ussr">{g.red}</span>
                      </>
                    )}
                  </dd>
                </div>
              ))}
            </dl>
          </aside>
        </div>
      </section>

      <section id="mission" className="rs-section">
        <div className="wrap">
          <h2 className="rs-h"><span>2.</span>Mission</h2>
          <div className="rs-orders">
            {ORDERS.map(o => (
              <article key={o.side} className={`rs-order ${o.side === 'Blue' ? 'b' : 'r'}`}>
                <div className="rs-order-from">{o.from}</div>
                <h3>{o.mission}</h3>
                <ol>
                  {o.tasks.map(t => <li key={t.lead}><b>{t.lead}</b> {t.body}</li>)}
                </ol>
                {o.closing.map(c => <p key={c} className="rs-closing">{c}</p>)}
              </article>
            ))}
          </div>
        </div>
      </section>

      <section id="ends" className="rs-section">
        <div className="wrap rs-ends">
          <div className="rs-sheet">
            <h2 className="rs-h"><span>3.</span>How the war ends</h2>
            {ENDING.map(p => <p key={p} className="rs-para">{p}</p>)}
            <p className="rs-para">Territory, stocks, aircraft and ground forces carry over between sessions; nothing resets when you land.</p>
          </div>
          <div className="rs-card">
            <h3 className="rs-card-h">What a loss costs the side that suffers it</h3>
            <table className="rs-will">
              <tbody>
                {WILL_COSTS.map(([loss, cost]) => (
                  <tr key={loss}><td>{loss}</td><td className="rs-cost">{cost}</td></tr>
                ))}
              </tbody>
            </table>
          </div>
        </div>
      </section>

      <section id="story" className="rs-section">
        <div className="wrap">
          <h2 className="rs-h"><span>4.</span>Story events</h2>
          <p className="rs-intro">
            Eighteen moments the war can turn on. None runs on a clock: each fires once, when players or the AI make
            its condition true, and is announced to both sides with the names of the pilots who did it.
          </p>
          <div className="rs-events">
            {STORY.map(e => (
              <article key={e.title} className={`rs-ev ${e.side === 'Blue' ? 'b' : 'r'}`}>
                <div className="rs-ev-head">
                  <span className="rs-flash">Flash</span>
                  <span className="rs-ev-side">{sideName(e.side)}</span>
                </div>
                <h3>{e.title}</h3>
                <p className="rs-when"><span>When:</span> {e.when}</p>
                <Announcement text={e.text} />
                <ul className="rs-effs">{e.effects.map(f => <li key={f}>{f}</li>)}</ul>
              </article>
            ))}
          </div>
        </div>
      </section>

      <section id="flying" className="rs-section">
        <div className="wrap">
          <h2 className="rs-h"><span>5.</span>Flying on this server</h2>
          <div className="rs-rules">
            {FLYING.map(f => (
              <article key={f.title} className="rs-card">
                <h3 className="rs-card-h">{f.title}</h3>
                <p>{f.body}</p>
              </article>
            ))}
          </div>
          <div className="rs-air">
            <div className="rs-aircol b">
              <h3>NATO <small>{AIRCRAFT.blue.note}</small></h3>
              <ul>{AIRCRAFT.blue.list.map(a => <li key={a}>{a}</li>)}</ul>
            </div>
            <div className="rs-aircol n">
              <h3>Both sides</h3>
              <ul>{AIRCRAFT.both.map(a => <li key={a}>{a}</li>)}</ul>
            </div>
            <div className="rs-aircol r">
              <h3>Soviet <small>{AIRCRAFT.red.note}</small></h3>
              <ul>{AIRCRAFT.red.list.map(a => <li key={a}>{a}</li>)}</ul>
            </div>
          </div>
          <div className="rs-join">
            <div>
              <div className="rs-k">Find it in the DCS server browser</div>
              <BrowserName name={profile.browserName} />
            </div>
            <ul className="rs-links">
              <li><a href={profile.site.url}>{profile.site.label} <ArrowRight size={12} /></a></li>
              <li><a href={wiki(`servers/${profile.slug}`)}>Field guide in the wiki <ArrowRight size={12} /></a></li>
              <li><a href={wiki('gameplay/points-and-lives')}>Points &amp; lives <ArrowRight size={12} /></a></li>
              <li><a href={links.discord} target="_blank" rel="noopener noreferrer">Discord <ExternalLink size={11} /></a></li>
            </ul>
          </div>
        </div>
      </section>

      <section className="rs-section rs-others">
        <div className="wrap">
          <h2 className="rs-h"><span>--</span>Other servers</h2>
          <ServerTiles exclude={profile.slug} />
        </div>
      </section>
    </div>
  )
}
