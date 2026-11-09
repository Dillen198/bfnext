import { useState } from 'react'
import { useQuery } from '@tanstack/react-query'
import PageHeader from '../components/PageHeader'
import { api } from '../api'
import { campaign } from '../config/campaign'
import { useDashLook } from '../lib/look'
import { Card, Head, Tape } from '../components/Levant'
import { useInstance } from '../context/InstanceContext'
import { SERVERS } from '@shared/servers/profiles'
import {
  Info,
  Server,
  Pilot,
  Support,
  ExternalLink,
  ArrowRight,
} from '@icons'

const FEATURES = [
  'Territory Control', 'Logistics', 'Cargo Transport', 'Troop Deployments',
  'JTAC', 'EWR', 'Kill Tracking', 'Commander System',
  'Pilot Lives', 'F10 Menus', 'Persistent Stats', 'Combined Arms',
]
/**
 * What Vector Strike's build of Fowl Engine adds to the original
 * (estokes/bfnext). Each line is something in this repository: the engine
 * modules in bflib/src (groundwar, hq, economy, modern_war, airlife, ai_fill,
 * atis, navaids, carp, cockpit), the story events (bfprotocols cfg/story),
 * livewx, and bfdb's news, gci, milatc and replay, plus bfrange and the web
 * apps. Keep it to what is built and switched on, not plans.
 */
const DIFFERENCES: { group: string; items: { title: string; body: string }[] }[] = [
  {
    group: 'In the war',
    items: [
      { title: 'A ground war that fights itself', body: 'Each side’s ground commander raises formations from the garrisons nearest the front, pushes them along the offensive, counter-attacks bases under threat and pulls the battered ones back. Any formation can be taken over from F10 or this dashboard; the AI commands whatever nobody else does.' },
      { title: 'A theatre HQ on each side', body: 'An AI commander reads what its side can actually see, picks a main effort, the bases to hold and the ones to resupply first, and plans its operations around the pilots already flying. A human commander’s orders come first.' },
      { title: 'A war economy', body: 'Treasury income, base funding and holding bonuses pay for what each side fields. Ground units are bought from the treasury and have to get to the front: nothing appears out of thin air.' },
      { title: 'Modern war', body: 'Jammers that break the enemy’s GCI, SAM sites with finite magazines that only logistics refill, drone and missile raids on infrastructure, sea-drone attacks and supply trains on the railway. Modelled on the wars in Ukraine and Iran.' },
      { title: 'A story that turns', body: 'Turning points fire once, when players or the AI make them true: a command post destroyed, a front broken. Both sides hear about it, with the names of the pilots who did it.' },
      { title: 'Alive on a quiet night', body: 'An AI wingman on request, the engine flying a side’s own sorties while few of its pilots are up, and airliners crossing the map behind the front.' },
    ],
  },
  {
    group: 'In the cockpit',
    items: [
      { title: 'Real weather', body: 'Live METAR weather and the real time of day are written into each mission before it loads, and every field has its own ATIS.' },
      { title: 'Radios where DCS has none', body: 'TACAN, NDB and carrier navaids generated for the FARPs, FOBs, logistics hubs and ships DCS leaves bare.' },
      { title: 'C-130 airlift', body: 'Physical cargo you load, fly and drop, with a computed air release point from the live wind.' },
      { title: 'Voice on the radio', body: 'Live GCI calls the air picture over SRS like an AWACS controller, and the training range has military ATC by voice.' },
      { title: 'A panel in the sim', body: 'The cockpit plugin puts the F10 menu, your status and the EWR picture in a panel inside DCS.' },
    ],
  },
  {
    group: 'Around the war',
    items: [
      { title: 'This dashboard', body: 'A live TacMap with fog of war, recon intel you mark here that appears on the F10 map, a briefing for each side, and rankings, ranks, squadrons and XP.' },
      { title: 'Every sortie replayable', body: 'Watch any sortie again in the browser, straight from the server’s own Tacview recording; nobody needs Tacview installed.' },
      { title: 'A daily war diary', body: 'Each day of the campaign written up as a dispatch from its own events, with a picture, read by both sides.' },
      { title: 'A training range', body: 'Bombing, strafing, carrier passes, refuelling, missile defence and helicopter work, every pass scored.' },
    ],
  },
]

const PRESENTATION = 'https://docs.google.com/presentation/d/1EAOe0iK-1s6i0UV5ObxSD86gGBj1Ixz6FOotQn5XPdc/edit#slide=id.g2b6a346170f_1_35'

// Role badge colour mapping
const ROLE_COLORS: Record<string, string> = {
  Creator:              '#f59e0b',
  'Lead Developer':     'var(--accent)',
  'Tester & Contributor': '#64748b',
}

export default function AboutPage() {
  // The campaign version in config is hand-maintained; this is what the
  // running bfdb was actually built from, so the two disagreeing is itself
  // useful information.
  const { data: build } = useQuery({
    queryKey: ['version'],
    queryFn: api.version,
    staleTime: 10 * 60_000,
    retry: false,
  })

  // In the campaign's look the page is laid out like SITREP and the site.
  const look = useDashLook()
  if (look.profile) return <LevantAbout build={build} theme={look.profile.theme} />

  return (
    <div className="flex-1 overflow-y-auto">
      <PageHeader art="carrier-quarter"
        icon={Info}
        title="ABOUT"
        sub="Campaign system & credits"
      />

      <div className="p-6 space-y-8 max-w-3xl">

        {/* ── Fowl Engine ── */}
        <section>
          <SectionHeading>FOWL ENGINE</SectionHeading>

          <div className="vs-card p-5 space-y-3">
            <div className="flex items-center gap-2 mb-1">
              <Support size={16} style={{ color: 'var(--accent)' }} />
              <span className="font-display" style={{ fontSize: '0.95rem', letterSpacing: '0.14em', color: 'var(--text)' }}>
                Dynamic Persistent Campaign System for DCS World
              </span>
            </div>

            <p style={{ fontSize: '0.8rem', color: 'var(--text-muted)', lineHeight: 1.7 }}>
              Fowl Engine is a persistent, multiplayer dynamic campaign system for DCS World.
              It enables full-scale warfare campaigns with territory control, logistics management,
              objective capturing, and coordinated combined arms operations — all tracked in
              real time across every sortie.
            </p>

            <p style={{ fontSize: '0.8rem', color: 'var(--text-muted)', lineHeight: 1.7 }}>
              Every action shapes the battlefield: supply convoys can be interdicted, FARPs
              destroyed, troops deployed, and territory flipped. Campaigns persist across
              sessions with full stat tracking, kill feeds, and leaderboards.
            </p>

            <p style={{ fontSize: '0.8rem', color: 'var(--text-muted)', lineHeight: 1.7 }}>
              Originally created by <strong style={{ color: 'var(--text)' }}>Eric Stokes</strong>.
            </p>

            <div className="flex flex-wrap gap-2 pt-1">
              {FEATURES.map(tag => (
                <span
                  key={tag}
                  style={{
                    fontSize: '0.6rem', letterSpacing: '0.12em', textTransform: 'uppercase',
                    color: 'var(--accent)', border: '1px solid var(--accent)',
                    padding: '2px 8px', borderRadius: '2px', opacity: 0.85,
                  }}
                >
                  {tag}
                </span>
              ))}
            </div>

            <div className="flex flex-wrap gap-4 pt-2">
              <a
                href={campaign.discord}
                target="_blank" rel="noreferrer"
                className="flex items-center gap-1.5 transition-opacity hover:opacity-80"
                style={{ fontSize: '0.68rem', color: '#5865F2', textDecoration: 'none', letterSpacing: '0.1em' }}
              >
                <DiscordIcon />
                JOIN DISCORD
              </a>
              <a
                href={PRESENTATION}
                target="_blank" rel="noreferrer"
                className="flex items-center gap-1.5 transition-opacity hover:opacity-80"
                style={{ fontSize: '0.68rem', color: 'var(--accent)', textDecoration: 'none', letterSpacing: '0.1em' }}
              >
                <ExternalLink size={11} />
                CAMPAIGN PRESENTATION
              </a>
            </div>
          </div>
        </section>

        {/* ── This campaign ── */}
        <section>
          <SectionHeading>THIS CAMPAIGN</SectionHeading>
          <div className="vs-card p-5 space-y-3">
            <div className="grid grid-cols-2 gap-4" style={{ fontSize: '0.78rem' }}>
              <InfoRow label="Campaign">{campaign.name}</InfoRow>
              <InfoRow label="Campaign version">{campaign.version}</InfoRow>
              <InfoRow label="Server build">
                {build ? (
                  <span className="font-mono-vs" style={{ fontSize: '0.72rem' }} title={`built ${build.built}`}>
                    <span style={{ color: 'var(--accent)' }}>{build.git}</span>
                    <span style={{ color: 'var(--text-dim)' }}> · {build.version}</span>
                  </span>
                ) : (
                  <span style={{ color: 'var(--text-dim)' }}>—</span>
                )}
              </InfoRow>
              <InfoRow label="Server">{campaign.server}</InfoRow>
              {campaign.serverIp && (
                <InfoRow label="Server IP">
                  <span className="font-mono-vs" style={{ color: 'var(--accent)', fontSize: '0.72rem' }}>
                    {campaign.serverIp}
                  </span>
                </InfoRow>
              )}
              <InfoRow label="Blue Faction">{campaign.blueLabel}</InfoRow>
              <InfoRow label="Red Faction">{campaign.redLabel}</InfoRow>
            </div>
          </div>
        </section>

        {/* ── Engine Credits ── */}
        <section>
          <SectionHeading>ENGINE CREDITS</SectionHeading>

          <div className="space-y-3">
            {campaign.engineCredits.map((c) => {
              const roleColor = ROLE_COLORS[c.role] ?? 'var(--text-muted)'
              return (
                <div
                  key={c.name}
                  className="vs-card p-4 flex items-start gap-4"
                >
                  {/* Avatar monogram */}
                  <div
                    style={{
                      width: 40, height: 40, borderRadius: '3px', flexShrink: 0,
                      background: 'var(--bg-elevated)',
                      border: `1px solid ${roleColor}`,
                      display: 'flex', alignItems: 'center', justifyContent: 'center',
                      fontFamily: 'var(--font-display)',
                      fontSize: '1.1rem', letterSpacing: '0.06em', color: roleColor,
                    }}
                  >
                    {c.name.slice(0, 2).toUpperCase()}
                  </div>

                  <div className="flex-1 min-w-0">
                    <div className="flex items-center gap-2 flex-wrap mb-1">
                      <span
                        className="font-display"
                        style={{ fontSize: '0.95rem', letterSpacing: '0.1em', color: 'var(--text)' }}
                      >
                        {c.name}
                      </span>
                      {c.handle && (
                        <span style={{ fontSize: '0.65rem', color: 'var(--text-muted)', letterSpacing: '0.05em' }}>
                          ({c.handle})
                        </span>
                      )}
                      {/* Role badge */}
                      <span
                        style={{
                          fontSize: '0.55rem', letterSpacing: '0.14em', textTransform: 'uppercase',
                          color: roleColor, border: `1px solid ${roleColor}`,
                          padding: '1px 7px', borderRadius: '2px',
                        }}
                      >
                        {c.role}
                      </span>
                    </div>
                  </div>
                </div>
              )
            })}
          </div>
        </section>

        {/* ── Server Credits (optional) ── */}
        {campaign.serverCredits.length > 0 && (
          <section>
            <SectionHeading>SERVER TEAM</SectionHeading>
            <div className="vs-card p-5">
              <div className="flex items-center gap-2 mb-3">
                <Pilot size={14} style={{ color: 'var(--accent)' }} />
                <span style={{ fontSize: '0.65rem', letterSpacing: '0.14em', textTransform: 'uppercase', color: 'var(--text-dim)' }}>
                  Operated By
                </span>
              </div>
              <div className="space-y-2">
                {campaign.serverCredits.map((s) => (
                  <div
                    key={s.name}
                    className="flex items-center justify-between"
                    style={{
                      padding: '0.5rem 0.75rem',
                      background: 'rgba(56,189,248,0.05)',
                      border: '1px solid rgba(56,189,248,0.12)',
                      borderRadius: '4px',
                    }}
                  >
                    <div className="flex items-center gap-3">
                      <div style={{ width: 6, height: 6, borderRadius: '50%', background: 'var(--accent)', flexShrink: 0 }} />
                      <span className="font-display" style={{ fontSize: '0.85rem', letterSpacing: '0.1em', color: 'var(--text)' }}>
                        {s.name}
                      </span>
                    </div>
                    <span style={{ fontSize: '0.65rem', color: 'var(--text-muted)', letterSpacing: '0.08em' }}>
                      {s.role}
                    </span>
                  </div>
                ))}
              </div>
            </div>
          </section>
        )}

        {/* ── Special Thanks ── */}
        <section>
          <SectionHeading>SPECIAL THANKS</SectionHeading>
          <div className="space-y-3">

            <div className="vs-card p-5">
              <div className="flex items-start gap-4">
                <img
                  src="https://www.digitalcombatsimulator.com/images/logos/Eagle_Dynamics.png"
                  alt="Eagle Dynamics"
                  style={{ width: 'clamp(90px, 30vw, 200px)', height: 'auto', flexShrink: 0, objectFit: 'contain' }}
                />
                <div className="min-w-0" style={{ flex: 1 }}>
                  <div className="font-display" style={{ fontSize: '0.95rem', letterSpacing: '0.14em', color: 'var(--text)', marginBottom: '0.4rem' }}>
                    Eagle Dynamics
                  </div>
                  <p style={{ fontSize: '0.78rem', color: 'var(--text-muted)', lineHeight: 1.6, marginBottom: '0.5rem' }}>
                    Developers of DCS World — the premier military flight simulator that
                    Fowl Engine is built upon.
                  </p>
                  <a
                    href="https://www.digitalcombatsimulator.com"
                    target="_blank" rel="noreferrer"
                    className="flex items-center gap-1.5 transition-opacity hover:opacity-80"
                    style={{ fontSize: '0.65rem', color: 'var(--accent)', textDecoration: 'none', letterSpacing: '0.1em' }}
                  >
                    <ExternalLink size={10} />
                    DIGITALCOMBATSIMULATOR.COM
                  </a>
                </div>
              </div>
            </div>

            <div className="vs-card p-5">
              <div className="flex items-start gap-4">
                <img
                  src="/hacker.jpg"
                  alt="Hacker"
                  style={{ width: 48, height: 48, borderRadius: '8px', flexShrink: 0, objectFit: 'cover' }}
                />
                <div className="min-w-0" style={{ flex: 1 }}>
                  <div className="font-display" style={{ fontSize: '0.95rem', letterSpacing: '0.14em', color: 'var(--text)', marginBottom: '0.4rem' }}>
                    Hacker
                  </div>
                  <p style={{ fontSize: '0.78rem', color: 'var(--text-muted)', lineHeight: 1.6 }}>
                    A persistent, 24/7 campaign lives or dies on having a machine that never goes offline —
                    and Vector Strike has one because <strong style={{ color: 'var(--text)' }}>Hacker</strong>,
                    founder of <strong style={{ color: 'var(--text)' }}>61st ATP — Arctic Terns &amp; Penguins</strong>,
                    generously donated the server it runs on. Every objective captured, every sortie flown, and
                    every name on the leaderboard is only possible because of that support. On behalf of the
                    whole squadron: thank you.
                  </p>
                </div>
              </div>
            </div>

            <div className="vs-card p-5">
              <div className="flex items-start gap-4">
                <img
                  src="/special-k.png"
                  alt="Special K"
                  style={{ width: 48, height: 48, borderRadius: '8px', flexShrink: 0, objectFit: 'contain' }}
                />
                <div className="min-w-0" style={{ flex: 1 }}>
                  <div className="font-display" style={{ fontSize: '0.95rem', letterSpacing: '0.14em', color: 'var(--text)', marginBottom: '0.4rem' }}>
                    DCSServerBot
                  </div>
                  <p style={{ fontSize: '0.78rem', color: 'var(--text-muted)', lineHeight: 1.6, marginBottom: '0.5rem' }}>
                    Built by <strong style={{ color: 'var(--text)' }}>Special K</strong>, DCSServerBot is the
                    Discord framework running our server bot — mission control, player management, and the live
                    campaign integration that ties Vector Strike's Discord presence to the fight in the air.
                  </p>
                  <a
                    href="https://github.com/Special-K-s-Flightsim-Bots/DCSServerBot"
                    target="_blank" rel="noreferrer"
                    className="flex items-center gap-1.5 transition-opacity hover:opacity-80"
                    style={{ fontSize: '0.65rem', color: 'var(--accent)', textDecoration: 'none', letterSpacing: '0.1em' }}
                  >
                    <ExternalLink size={10} />
                    GITHUB.COM/DCSSERVERBOT
                  </a>
                </div>
              </div>
            </div>

          </div>
        </section>

        {/* ── Server info footer ── */}
        {campaign.serverIp && (
          <div
            className="flex items-center gap-2"
            style={{ fontSize: '0.68rem', color: 'var(--text-dim)', letterSpacing: '0.1em' }}
          >
            <Server size={11} />
            <span>DCS SERVER</span>
            <span className="font-mono-vs" style={{ color: 'var(--text-muted)' }}>{campaign.serverIp}</span>
          </div>
        )}

      </div>
    </div>
  )
}

// ── Levant Ablaze's look ──────────────────────────────────────────────────────

/** Engine-credit roles as tape: the creator's in rust. */
const ROLE_TAPE: Record<string, 'rust' | undefined> = { Creator: 'rust' }

/**
 * The page in the campaign's look (levant.css). It is about Vector Strike, the
 * community and its servers, first; Fowl Engine, the campaign engine they all
 * run on, second; then the people behind both.
 */
function LevantAbout({ build, theme }: { build?: { git: string; version: string; built: string }; theme: string }) {
  // The live server's own name, as the DCS browser lists it; the config's
  // is a fallback for a bfdb that does not report one.
  const { current } = useInstance()
  const serverName = current?.dcs_server_name || campaign.server
  const [first, ...rest] = campaign.name.split(' ')
  return (
    <div className="lv-page">
      <header className="lv-hero">
        <div className="lv-hero-img" {...(theme === 'desert'
          ? { role: 'img', 'aria-label': 'Two jets fighting low over a burning battlefield at dusk' }
          : { 'aria-hidden': true })} />
        <div className="lv-hero-scrim" />
        <div className="lv-wrap lv-hero-body">
          <div className="lv-hero-top lv-about-top">
            <div>
              <Tape tone="rust">About</Tape>
              <h1 className="lv-title">{first} <span>{rest.join(' ')}</span></h1>
              <div className="lv-round">Persistent PvP campaigns for DCS World · powered by <b>Fowl Engine</b></div>
            </div>
            <div className="lv-ctas">
              <a className="ds-btn" href={campaign.discord} target="_blank" rel="noreferrer"><DiscordIcon /> Join Discord</a>
              <a className="ds-btn ghost" href={PRESENTATION} target="_blank" rel="noreferrer">Engine presentation <ExternalLink size={13} /></a>
            </div>
          </div>
        </div>
      </header>

      <section className="lv-section lv-sitrep" aria-label="This server">
        <div className="lv-wrap lv-about-facts">
          <div className="lv-readout"><span className="ds-k">Campaign</span><span className="lv-fact">{campaign.name}</span></div>
          <div className="lv-readout"><span className="ds-k">Campaign version</span><span className="lv-fact">{campaign.version}</span></div>
          <div className="lv-readout">
            <span className="ds-k">Server build</span>
            <span className="lv-fact" title={build ? `built ${build.built}` : undefined}>
              {build ? <>{build.git}<small> · {build.version}</small></> : '—'}
            </span>
          </div>
          <div className="lv-readout lv-readout-wide"><span className="ds-k">Server</span><span className="lv-fact">{serverName}</span></div>
          {campaign.serverIp && <div className="lv-readout"><span className="ds-k">Server IP</span><span className="lv-fact">{campaign.serverIp}</span></div>}
          <div className="lv-readout">
            <span className="ds-k">Factions</span>
            <span className="lv-fact"><span className="b">{campaign.blueLabel}</span><small> vs </small><span className="r">{campaign.redLabel}</span></span>
          </div>
        </div>
      </section>

      <section className="lv-section">
        <div className="lv-wrap lv-about-engine">
          <div>
            <Head n="01">{campaign.name}</Head>
            <div className="lv-prose">
              <p><b>{campaign.name} is a DCS World community running persistent PvP campaigns.</b> Both coalitions are
                flown by players, and the war carries on between sessions: what you capture, supply or destroy is still
                there the next time anyone logs in.</p>
              <p>Every campaign here runs on <b>Fowl Engine</b>. {campaign.name} builds the campaigns, runs the servers and
                this dashboard, and keeps the war going around the clock.</p>
            </div>
          </div>
          <Card tape="Our servers" tone="rust">
            <ul className="lv-servers">
              {SERVERS.map(sv => (
                <li key={sv.slug} className={`t-${sv.theme}`}>
                  <div className="lv-srv-head">
                    <strong>{sv.title}</strong>
                    <span>{sv.label}</span>
                  </div>
                  <Copyable k="In the server browser" text={sv.browserName} />
                  {sv.address && <Copyable k="Address" text={sv.address} />}
                </li>
              ))}
            </ul>
          </Card>
        </div>
      </section>

      <section className="lv-section lv-band">
        <div className="lv-wrap lv-about-engine">
          <div>
            <Head n="02">Our Fowl Engine</Head>
            <div className="lv-prose">
              <p><b>Fowl Engine is the campaign engine behind {campaign.name}:</b> a persistent, multiplayer dynamic campaign system for DCS World. It runs full-scale
                warfare campaigns with territory control, logistics, objective capture and coordinated combined arms,
                all tracked in real time across every sortie.</p>
              <p>Every action shapes the battlefield: supply convoys can be interdicted, FARPs destroyed, troops deployed
                and territory flipped. Campaigns persist across sessions with full stat tracking, kill feeds and leaderboards.</p>
              <p>Originally created by <b>Eric Stokes</b>.</p>
            </div>
          </div>
          <Card tape="From the original engine" tone="rust">
            <ul className="lv-features">{FEATURES.map(f => <li key={f}>{f}</li>)}</ul>
          </Card>
        </div>
      </section>

      <section className="lv-section">
        <div className="lv-wrap">
          <Head n="03">What makes us different</Head>
          <p className="ds-sub">{campaign.name} runs its own build of Fowl Engine. The original gives the war its bones; this is what we built on top of it.</p>
          {DIFFERENCES.map(g => (
            <div key={g.group} className="lv-diff-group">
              <h3 className="lv-h3">{g.group}</h3>
              <div className="lv-diffs">
                {g.items.map(d => (
                  <article key={d.title} className="lv-diff">
                    <h4>{d.title}</h4>
                    <p>{d.body}</p>
                  </article>
                ))}
              </div>
            </div>
          ))}
        </div>
      </section>

      <section className="lv-section lv-band">
        <div className="lv-wrap">
          <Head n="04">Credits</Head>
          <h3 className="lv-h3 first">Fowl Engine</h3>
          <div className="lv-credits">
            {campaign.engineCredits.map(c => (
              <article key={c.name} className={`lv-credit${c.role === 'Creator' ? ' lead' : ''}`}>
                <Avatar name={c.name} src={c.avatar} />
                <div>
                  <strong>{c.name}</strong>
                  {c.handle && <span className="lv-handle">{c.handle}</span>}
                  <Tape tone={ROLE_TAPE[c.role]} className="sm">{c.role}</Tape>
                </div>
              </article>
            ))}
          </div>
          {campaign.serverCredits.length > 0 && (
            <>
              <h3 className="lv-h3">{campaign.name} team</h3>
              <div className="lv-credits">
                {campaign.serverCredits.map(m => (
                  <article key={m.name} className="lv-credit">
                    <Avatar name={m.name} src={m.avatar} />
                    <div><strong>{m.name}</strong><Tape className="sm">{m.role}</Tape></div>
                  </article>
                ))}
              </div>
            </>
          )}
        </div>
      </section>

      <section className="lv-section">
        <div className="lv-wrap">
          <Head n="05">Special thanks</Head>
          <div className="lv-thanks">
            <Card tape="Eagle Dynamics">
              <div className="lv-thank">
                <img className="wide" src="https://www.digitalcombatsimulator.com/images/logos/Eagle_Dynamics.png" alt="Eagle Dynamics" />
                <p>Developers of DCS World, the military flight simulator Fowl Engine is built on.</p>
                <a className="ds-link" href="https://www.digitalcombatsimulator.com" target="_blank" rel="noreferrer">digitalcombatsimulator.com <ArrowRight size={12} /></a>
              </div>
            </Card>
            <Card tape="Hacker" tone="rust">
              <div className="lv-thank">
                <img src="/hacker.jpg" alt="Hacker" />
                <p>A persistent, 24/7 campaign lives or dies on having a machine that never goes offline, and Vector Strike
                  has one because <b>Hacker</b>, founder of <b>61st ATP (Arctic Terns &amp; Penguins)</b>, generously donated
                  the server it runs on. Every objective captured, every sortie flown and every name on the leaderboard is
                  only possible because of that support. On behalf of the whole squadron: thank you.</p>
              </div>
            </Card>
            <Card tape="DCSServerBot">
              <div className="lv-thank">
                <img src="/special-k.png" alt="Special K" className="contain" />
                <p>Built by <b>Special K</b>, DCSServerBot is the Discord framework running our server bot: mission control,
                  player management, and the live campaign integration that ties Vector Strike&apos;s Discord to the fight in the air.</p>
                <a className="ds-link" href="https://github.com/Special-K-s-Flightsim-Bots/DCSServerBot" target="_blank" rel="noreferrer">github.com/DCSServerBot <ArrowRight size={12} /></a>
              </div>
            </Card>
          </div>
        </div>
      </section>
    </div>
  )
}

/** A label, a value and a button that copies the value (a server's browser name, its address). */
function Copyable({ k, text }: { k: string; text: string }) {
  const [copied, setCopied] = useState(false)
  const copy = () => {
    navigator.clipboard?.writeText(text).then(() => {
      setCopied(true)
      setTimeout(() => setCopied(false), 1500)
    }, () => { /* no clipboard (insecure origin): the text is still selectable */ })
  }
  return (
    <div className="lv-copy">
      <span className="ds-k">{k}</span>
      <code>{text}</code>
      <button type="button" onClick={copy} aria-label={`Copy ${k.toLowerCase()}`}>{copied ? 'Copied' : 'Copy'}</button>
    </div>
  )
}

/**
 * Pictures for the people in the credits, by name. The live server's
 * campaign.json sends its own credit lists (and replaces the ones built in),
 * so a picture is looked up here rather than relying on the file to carry
 * it; an `avatar` in the config still wins.
 */
const AVATARS: Record<string, string> = {
  killerdog: '/credits/killerdog.png',
}

/** A credit's picture, or their initials when there is none (or it fails to load). */
function Avatar({ name, src }: { name: string; src?: string }) {
  const [failed, setFailed] = useState(false)
  const pic = src || AVATARS[name.toLowerCase()]
  if (!pic || failed) return <span className="lv-mono" aria-hidden="true">{name.slice(0, 2)}</span>
  return <img className="lv-mono lv-avatar" src={pic} alt="" onError={() => setFailed(true)} />
}

// ── Local helpers ─────────────────────────────────────────────────────────────

function SectionHeading({ children }: { children: React.ReactNode }) {
  return (
    <div
      className="mb-4 font-display"
      style={{
        fontSize: '1.1rem', letterSpacing: '0.18em',
        color: 'var(--text)',
        borderLeft: '3px solid var(--accent)',
        paddingLeft: '0.75rem',
      }}
    >
      {children}
    </div>
  )
}

function InfoRow({ label, children }: { label: string; children: React.ReactNode }) {
  return (
    <div>
      <div style={{ fontSize: '0.6rem', letterSpacing: '0.12em', textTransform: 'uppercase', color: 'var(--text-dim)', marginBottom: '0.2rem' }}>
        {label}
      </div>
      <div style={{ color: 'var(--text)', fontSize: '0.8rem' }}>{children}</div>
    </div>
  )
}

function DiscordIcon() {
  return (
    <svg width="12" height="12" viewBox="0 0 24 24" fill="currentColor">
      <path d="M20.317 4.37a19.791 19.791 0 0 0-4.885-1.515.074.074 0 0 0-.079.037c-.21.375-.444.864-.608 1.25a18.27 18.27 0 0 0-5.487 0 12.64 12.64 0 0 0-.617-1.25.077.077 0 0 0-.079-.037A19.736 19.736 0 0 0 3.677 4.37a.07.07 0 0 0-.032.027C.533 9.046-.32 13.58.099 18.057a.082.082 0 0 0 .031.057 19.9 19.9 0 0 0 5.993 3.03.078.078 0 0 0 .084-.028 14.09 14.09 0 0 0 1.226-1.994.076.076 0 0 0-.041-.106 13.107 13.107 0 0 1-1.872-.892.077.077 0 0 1-.008-.128 10.2 10.2 0 0 0 .372-.292.074.074 0 0 1 .077-.01c3.928 1.793 8.18 1.793 12.062 0a.074.074 0 0 1 .078.01c.12.098.246.198.373.292a.077.077 0 0 1-.006.127 12.299 12.299 0 0 1-1.873.892.077.077 0 0 0-.041.107c.36.698.772 1.362 1.225 1.993a.076.076 0 0 0 .084.028 19.839 19.839 0 0 0 6.002-3.03.077.077 0 0 0 .032-.054c.5-5.177-.838-9.674-3.549-13.66a.061.061 0 0 0-.031-.03z"/>
    </svg>
  )
}
