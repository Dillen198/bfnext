import { Fragment, useEffect, type ReactNode } from 'react'
import { Link, useParams } from 'react-router-dom'
import { ArrowRight, ExternalLink, Server } from '@icons'
import { VS_URLS } from '@shared/web/urls'
import { FONTS, useFonts } from '@shared/servers/fonts'
import { THEME_CLASS } from '@shared/servers/profiles'
import '@shared/servers/themes.css'
import { useInstance } from '../context/InstanceContext'
import { useServerLook } from '../context/ServerLookContext'
import { SERVERS, guideBySlug, type Guide } from './guides'
import './guide.css'

/** "{by}" in an announcement is the pilots the engine credits. */
function Announcement({ text }: { text: string }) {
  return (
    <p className="g-ev-text">
      {text.split('{by}').map((part, i) => (
        <Fragment key={i}>
          {i > 0 && <span className="g-by">pilots credited</span>}
          {part}
        </Fragment>
      ))}
    </p>
  )
}

/** Point the wiki's {{cfg:...}} numbers at this server, where bfdb fronts it. */
function QuoteNumbers({ guide }: { guide: Guide }) {
  const { instances, current, multi, select } = useInstance()
  const id = guide.profile.instanceId
  if (!multi || !id || !instances.some(i => i.id === id)) return null
  if (current?.id === id) {
    return <span className="g-quote is-on"><Server size={13} aria-hidden /> The wiki is quoting this server’s numbers</span>
  }
  return (
    <button type="button" className="g-quote" onClick={() => select(id)}>
      <Server size={13} aria-hidden /> Quote this server’s numbers across the wiki
    </button>
  )
}

function Section({ id, title, children }: { id: string; title: string; children: ReactNode }) {
  return (
    <section id={id} className="g-section">
      <h2 className="g-h">{title}</h2>
      {children}
    </section>
  )
}

export default function ServerGuidePage() {
  const { slug } = useParams()
  const guide = guideBySlug(slug)
  const theme = guide?.profile.theme ?? 'desert'
  useFonts(FONTS[theme])
  // Opening a server's guide dresses the whole wiki as that server.
  const { server, choose } = useServerLook()
  useEffect(() => {
    if (guide && server?.slug !== guide.profile.slug) choose(guide.profile.slug)
  }, [guide, server, choose])
  useEffect(() => {
    if (guide) document.title = `${guide.profile.title}: field guide — Vector Strike Wiki`
  }, [guide])

  if (!guide) {
    return (
      <div>
        <div className="g-missing">No field guide for that server.</div>
        <Link to="/" className="g-link">Back to the wiki <ArrowRight size={12} /></Link>
      </div>
    )
  }

  const p = guide.profile
  const toc = [
    ['situation', 'Situation'],
    guide.orders && ['orders', 'Orders'],
    guide.ending && ['ends', 'How it ends'],
    ['story', guide.story?.title ?? 'Story'],
    guide.rules && ['rules', 'Rules'],
    guide.tables && ['reference', 'Reference'],
    ['related', 'In the wiki'],
  ].filter((x): x is [string, string] => !!x)

  return (
    <article className={`guide ${THEME_CLASS[theme]}`}>
      <header className="g-hero">
        <div className="g-hero-in">
          <div className="g-kicker">{guide.kicker}</div>
          <h1 className="g-title">{p.title}</h1>
          <p className="g-lede">{p.summary}</p>
          {guide.sides && (
            <div className="g-sides">
              {guide.sides.map(s => (
                <div key={s.name} className={`g-side ${s.tone}`}>
                  {s.flag && <img src={s.flag} alt="" className="g-flag" />}
                  <div>
                    <strong>{s.name}</strong>
                    {s.members && <ul className="g-members">{s.members.map(m => <li key={m}>{m}</li>)}</ul>}
                    <span>{s.note}</span>
                  </div>
                </div>
              ))}
            </div>
          )}
          <div className="g-actions">
            <a className="g-btn" href={`${VS_URLS.site}/servers/${p.slug}`}>Server page <ExternalLink size={12} /></a>
            <a className="g-btn ghost" href={p.site.url}>{p.site.label} <ExternalLink size={12} /></a>
            <QuoteNumbers guide={guide} />
          </div>
          <div className="g-browser">
            <span>In the DCS server browser</span>
            <code>{p.browserName}</code>
          </div>
        </div>
        <nav className="g-toc" aria-label="On this page">
          {toc.map(([id, label]) => <a key={id} href={`#${id}`}>{label}</a>)}
        </nav>
      </header>

      <Section id="situation" title="Situation">
        {guide.flash && <blockquote className="g-flash"><span className="g-tag r">Flash</span>{guide.flash}</blockquote>}
        <div className="g-sit">
          <div className="g-prose">
            {guide.situation.map((s, i) => <p key={i}>{s.lead && <b>{s.lead} </b>}{s.body}</p>)}
          </div>
          {guide.glance && (
            <dl className="g-glance">
              {guide.glance.map(g => (
                <div key={g.k}>
                  <dt>{g.k}</dt>
                  <dd>{g.both ?? <><span className="b">{g.blue}</span> · <span className="r">{g.red}</span></>}</dd>
                </div>
              ))}
            </dl>
          )}
        </div>
      </Section>

      {guide.orders && (
        <Section id="orders" title="Orders">
          <div className="g-orders">
            {guide.orders.map(o => (
              <div key={o.side} className={`g-order ${o.side === 'Blue' ? 'b' : 'r'}`}>
                <span className={`g-tag ${o.side === 'Blue' ? 'b' : 'r'}`}>{o.from}</span>
                <h3>{o.mission}</h3>
                <ol>{o.tasks.map(t => <li key={t.lead}><b>{t.lead}</b> {t.body}</li>)}</ol>
                {o.closing.map(c => <p key={c} className="g-closing">{c}</p>)}
              </div>
            ))}
          </div>
        </Section>
      )}

      {guide.ending && (
        <Section id="ends" title="How the war ends">
          <div className="g-sit">
            <div className="g-prose">{guide.ending.text.map(t => <p key={t}>{t}</p>)}</div>
            <table className="g-table">
              <caption>What a loss costs the side that suffers it</caption>
              <tbody>{guide.ending.costs.map(([k, v]) => <tr key={k}><td>{k}</td><td className="g-num">{v}</td></tr>)}</tbody>
            </table>
          </div>
        </Section>
      )}

      <Section id="story" title={guide.story?.title ?? 'Story'}>
        {guide.story ? (
          <>
            <p className="g-intro">{guide.story.intro}</p>
            <div className="g-events">
              {guide.story.events.map(e => (
                <div key={e.title} className={`g-ev ${e.side === 'Blue' ? 'b' : 'r'}`}>
                  <span className={`g-tag ${e.side === 'Blue' ? 'b' : 'r'}`}>{e.side === 'Blue' ? p.blueLabel : p.redLabel}</span>
                  <h3>{e.title}</h3>
                  <div className="g-when"><span>When</span> {e.when}</div>
                  <Announcement text={e.text} />
                  {e.effects && <ul className="g-effs">{e.effects.map(f => <li key={f}>{f}</li>)}</ul>}
                </div>
              ))}
            </div>
            {guide.story.note && <p className="g-note">{guide.story.note}</p>}
          </>
        ) : (
          <p className="g-nostory">{guide.noStory}</p>
        )}
      </Section>

      {guide.rules && (
        <Section id="rules" title="Rules of the theatre">
          <div className="g-rules">
            {guide.rules.map(r => (
              <div key={r.title} className="g-rule">
                <h3>{r.title}</h3>
                <p>{r.body}</p>
              </div>
            ))}
          </div>
        </Section>
      )}

      {guide.tables && (
        <Section id="reference" title="Reference">
          <div className="g-tables">
            {guide.tables.map(t => (
              <div key={t.title} className="g-scroll">
                <table className="g-table">
                  <caption>{t.title}</caption>
                  {t.head && <thead><tr>{t.head.map(h => <th key={h}>{h}</th>)}</tr></thead>}
                  <tbody>
                    {t.rows.map((r, i) => (
                      <tr key={i}>
                        {r.cells.map((c, j) => (
                          <td key={j} className={j === 0 ? 'g-first' : undefined}>
                            {j === 0 && r.chip && <span className={`g-chip k-${r.chip}`} aria-hidden="true" />}
                            {c}
                          </td>
                        ))}
                      </tr>
                    ))}
                  </tbody>
                </table>
              </div>
            ))}
          </div>
        </Section>
      )}

      <Section id="related" title="In the wiki">
        <ul className="g-related">
          {guide.related.map(r => (
            <li key={r.slug}><Link to={`/${r.slug}`}>{r.label} <ArrowRight size={12} /></Link></li>
          ))}
        </ul>
        <div className="g-others">
          <span>Other field guides</span>
          {SERVERS.filter(s => s.slug !== p.slug).map(s => (
            <Link key={s.slug} to={`/servers/${s.slug}`} className={`g-other g-dot-${s.theme}`}>{s.title}</Link>
          ))}
        </div>
      </Section>
    </article>
  )
}
