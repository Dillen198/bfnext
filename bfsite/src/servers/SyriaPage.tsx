import { Fragment, type ReactNode } from 'react'
import { ArrowRight, ExternalLink } from '@icons'
import type { ServerProfile } from '../config/servers'
import { links, wiki } from '../config/campaign'
import { BrowserName, NoticeBox, StatusTag, TerritoryBar } from '../components/Bits'
import { STATE_LABEL } from '../status'
import { ENDING, FLASH, FRONTS, GLANCE, OOB, ORDERS, SAMS, SIDES, SITUATION, TURNING_POINTS } from '@shared/servers/syria'
// Syria's war_will uses the Red Storm values (Syria/tools/add_story.py).
import { WILL_COSTS } from '@shared/servers/redstormStory'
import ServerTiles from './ServerTiles'
import '@shared/servers/themes.css'
import { FONTS, useFonts } from '@shared/servers/fonts'
import { useServerLive } from './useServerLive'
import './syria.css'

const TOC = [
  ['sitrep', 'Sitrep'],
  ['situation', 'Situation'],
  ['orders', 'Orders'],
  ['fronts', 'Fronts'],
  ['oob', 'Order of battle'],
  ['story', 'Turning points'],
  ['fly', 'Fly'],
] as const

/** A strip of tape with a word on it. */
function Tape({ children, tone }: { children: ReactNode; tone?: 'b' | 'r' | 'rust' }) {
  return <span className={`ds-tape${tone ? ` ${tone}` : ''}`}>{children}</span>
}

/** "{by}" is the pilots who make it happen. */
function Callout({ text }: { text: string }) {
  return (
    <p>
      {text.split('{by}').map((part, i) => (
        <Fragment key={i}>
          {i > 0 && <span className="ds-by">pilots credited</span>}
          {part}
        </Fragment>
      ))}
    </p>
  )
}

function Head({ n, children }: { n: string; children: ReactNode }) {
  return <h2 className="ds-h"><span>{n}</span>{children}</h2>
}

export default function SyriaPage({ profile }: { profile: ServerProfile }) {
  useFonts(FONTS.desert)
  const live = useServerLive(profile)
  const total = OOB.reduce((n, [, b, r]) => n + b + r, 0)
  return (
    <div className="srv srv-desert">
      <header className="ds-hero">
        <div className="ds-hero-img" role="img" aria-label="Two jets fighting low over a burning battlefield at dusk" />
        <div className="ds-hero-scrim" />
        <div className="wrap ds-hero-body">
          <Tape tone="rust">Operation Bronze Helm</Tape>
          <h1 className="ds-title">Levant<span>Ablaze</span></h1>
          <p className="ds-lede">{profile.summary}</p>
          <div className="ds-sides">
            {([['b', SIDES.blue], ['r', SIDES.red]] as const).map(([tone, side], i) => (
              <Fragment key={tone}>
                {i > 0 && <div className="vs">vs</div>}
                <div className={`ds-side ${tone}`}>
                  <img src={side.flag} alt="" className="ds-flag" />
                  <div>
                    <strong>{side.name}</strong>
                    <ul className="ds-members" aria-label={`${side.name} members`}>
                      {side.members.map(m => <li key={m}>{m}</li>)}
                    </ul>
                    <span>From {side.ground}</span>
                  </div>
                </div>
              </Fragment>
            ))}
          </div>
          <div className="ds-ctas">
            <a className="ds-btn" href={profile.site.url}>{profile.site.label} <ArrowRight size={14} /></a>
            <a className="ds-btn ghost" href="#fly">How to join</a>
          </div>
        </div>
        <nav className="wrap ds-toc" aria-label="On this page">
          {TOC.map(([id, label]) => <a key={id} href={`#${id}`}>{label}</a>)}
        </nav>
      </header>

      <section id="sitrep" className="ds-section ds-sitrep">
        <div className="wrap">
          <div className="ds-readouts">
            <div><span className="ds-k">Server</span><StatusTag state={live.state} /></div>
            <div><span className="ds-k">Round</span><span className="ds-v">{live.round ? <>{live.round.n}{live.round.day ? <small>day {live.round.day}</small> : null}</> : '—'}</span></div>
            <div><span className="ds-k">Pilots on</span><span className="ds-v">{live.airborne.known ? <>{live.airborne.total}<small>{live.airborne.blue} / {live.airborne.red}</small></> : '—'}</span></div>
            <div><span className="ds-k">Lives</span><span className="ds-v">{profile.livesEnforced ? 'Enforced' : 'Unlimited'}</span></div>
            <div className="wide">
              <span className="ds-k">Front line</span>
              <TerritoryBar front={live.up ? live.front : undefined} loading={live.up && live.frontLoading} blueLabel={profile.blueLabel} redLabel={profile.redLabel} />
            </div>
          </div>
          {live.notices.filter(n => n.level !== 'info').map(n => <NoticeBox key={n.id} notice={n} />)}
          {!live.up && live.state !== 'unknown' && (
            <p className="ds-note">{STATE_LABEL[live.state]}: live numbers return when the server does.</p>
          )}
        </div>
      </section>

      <section id="situation" className="ds-section">
        <div className="wrap">
          <Head n="01">Situation</Head>
          <blockquote className="ds-flash"><Tape tone="r">Flash</Tape>{FLASH}</blockquote>
          <div className="ds-sit">
            <div className="ds-prose">
              {SITUATION.map((p, i) => (
                <p key={i}>{p.lead && <b>{p.lead} </b>}{p.body}</p>
              ))}
            </div>
            <div className="ds-side-col">
              <figure className="ds-photo">
                <img src="/media/air-war.jpg" alt="A camouflaged SAM battery dug into a canyon at dusk" loading="lazy" />
                <figcaption><Tape>The shield</Tape></figcaption>
              </figure>
              <aside className="ds-glance">
                <dl>
                  {GLANCE.map(g => (
                    <div key={g.k}>
                      <dt>{g.k}</dt>
                      <dd>{g.both ?? <><span className="b">{g.blue}</span> · <span className="r">{g.red}</span></>}</dd>
                    </div>
                  ))}
                </dl>
              </aside>
            </div>
          </div>
        </div>
      </section>

      <section id="orders" className="ds-section ds-band">
        <div className="wrap">
          <Head n="02">Orders</Head>
          <div className="ds-orders">
            {ORDERS.map(o => (
              <article key={o.side} className={`ds-order ${o.side === 'Blue' ? 'b' : 'r'}`}>
                <Tape tone={o.side === 'Blue' ? 'b' : 'r'}>{o.from}</Tape>
                <h3>{o.mission}</h3>
                <ol>{o.tasks.map(t => <li key={t.lead}><b>{t.lead}</b> {t.body}</li>)}</ol>
                {o.closing.map(c => <p key={c} className="ds-closing">{c}</p>)}
              </article>
            ))}
          </div>
        </div>
      </section>

      <section id="fronts" className="ds-section">
        <div className="wrap">
          <Head n="03">Fronts</Head>
          <p className="ds-sub">Where the fighting is, measured when the campaign was rebalanced: an objective counts as live within 120 km of an enemy one.</p>
          <div className="ds-fronts">
            {FRONTS.map(f => (
              <article key={f.code} className={`ds-front ${f.status}`}>
                <div className="ds-front-head">
                  <span className="ds-code">{f.code}</span>
                  <Tape tone={f.status === 'hot' ? 'rust' : undefined}>{f.status === 'hot' ? 'Hot' : f.status === 'warm' ? 'Contested' : 'Quiet'}</Tape>
                </div>
                <h3>{f.name}</h3>
                <p>{f.body}</p>
              </article>
            ))}
          </div>
        </div>
      </section>

      <section id="oob" className="ds-section ds-band">
        <div className="wrap ds-oob">
          <div>
            <Head n="04">Order of battle</Head>
            <p className="ds-sub">{total} objectives at the start of a round, every one on its real place.</p>
            <table className="ds-table">
              <thead><tr><th>Objective</th><th className="b">{profile.blueLabel}</th><th className="r">{profile.redLabel}</th></tr></thead>
              <tbody>
                {OOB.map(([kind, b, r]) => (
                  <tr key={kind}>
                    <td>{kind}</td>
                    <td className="b"><span className="ds-bar" style={{ width: `${(b / 29) * 100}%` }} />{b}</td>
                    <td className="r"><span className="ds-bar" style={{ width: `${(r / 29) * 100}%` }} />{r}</td>
                  </tr>
                ))}
              </tbody>
            </table>
          </div>
          <div className="ds-sams">
            <Tape tone="rust">Hidden air defence</Tape>
            <div className="ds-samrow">
              <div className="b"><span className="ds-big">{SAMS.blue.count}</span><span>{profile.blueLabel} sites</span><small>{SAMS.blue.types}</small></div>
              <div className="r"><span className="ds-big">{SAMS.red.count}</span><span>{profile.redLabel} sites</span><small>{SAMS.red.types}</small></div>
            </div>
            <p>{SAMS.note}</p>
            <a className="ds-link" href={wiki('advanced/modern-war')}>How the air defence fights <ArrowRight size={12} /></a>
          </div>
        </div>
      </section>

      <section id="story" className="ds-section">
        <div className="wrap">
          <Head n="05">Turning points</Head>
          <p className="ds-sub">Fourteen moments the war can turn on. None runs on a clock: each fires once, when players or the AI make its condition true, and is announced to both sides with the names of the pilots who did it.</p>
          <div className="ds-events">
            {TURNING_POINTS.map(e => (
              <article key={e.title} className={`ds-ev ${e.side === 'Blue' ? 'b' : 'r'}`}>
                <Tape tone={e.side === 'Blue' ? 'b' : 'r'}>{e.side === 'Blue' ? SIDES.blue.name : SIDES.red.name} loss</Tape>
                <h3>{e.title}</h3>
                <div className="ds-when"><span>When</span> {e.when}</div>
                <Callout text={e.text} />
                <ul className="ds-effs">{e.effects.map(f => <li key={f}>{f}</li>)}</ul>
              </article>
            ))}
          </div>
          <div className="ds-ends">
            <div className="ds-prose">
              <h3 className="ds-h3">How the war ends</h3>
              {ENDING.map(t => <p key={t}>{t}</p>)}
            </div>
            <table className="ds-table ds-will">
              <caption>What a loss costs the side that suffers it</caption>
              <tbody>{WILL_COSTS.map(([k, v]) => <tr key={k}><td>{k}</td><td className="ds-cost">{v}</td></tr>)}</tbody>
            </table>
          </div>
          <a className="ds-link" href={profile.site.url + '/news'}>The war diary writes up every day of the round <ArrowRight size={12} /></a>
        </div>
      </section>

      <section id="fly" className="ds-section ds-fly-band">
        <div className="wrap ds-fly">
          <div>
            <Head n="06">Fly</Head>
            <p className="ds-sub">Find the server in the DCS multiplayer browser, pick a coalition and a slot.</p>
            <BrowserName name={profile.browserName} />
          </div>
          <ul className="ds-links">
            <li><a href={profile.site.url}>{profile.site.label}<ArrowRight size={12} /></a></li>
            <li><a href={wiki(`servers/${profile.slug}`)}>Field guide in the wiki<ArrowRight size={12} /></a></li>
            <li><a href={wiki('playbooks/first-sortie')}>Your first sortie<ArrowRight size={12} /></a></li>
            <li><a href={wiki('reference/aircraft-roster')}>Aircraft roster<ArrowRight size={12} /></a></li>
            <li><a href={links.discord} target="_blank" rel="noopener noreferrer">Discord<ExternalLink size={11} /></a></li>
          </ul>
        </div>
      </section>

      <section className="ds-section ds-others">
        <div className="wrap">
          <Head n="··">Other servers</Head>
          <ServerTiles exclude={profile.slug} />
        </div>
      </section>
    </div>
  )
}
