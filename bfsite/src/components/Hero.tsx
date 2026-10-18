import { Link } from 'react-router-dom'
import { ArrowRight, Support } from '@icons'
import { campaign, links, wiki } from '../config/campaign'
import { roundDay, useServerStates, useTheatreLive, type ServerRow } from '../live'
import { StatusTag, TerritoryBar } from './Bits'

function LiveCell({ row }: { row: ServerRow }) {
  const t = row.theatre
  const up = row.state === 'live' || row.state === 'standby'
  const { front, frontLoading, airborne } = useTheatreLive(up ? t.instance : undefined)
  const round = up ? t.instance?.active_round : undefined
  const day = round ? roundDay(round.start) : undefined
  const why = row.notices[0]?.title
  return (
    <div className="live-cell">
      <div className="live-head">
        <span className="live-name">{t.label}</span>
        <StatusTag state={row.state} />
      </div>
      <div className="live-meta">
        {round ? `Round ${round.id}${day ? ` · day ${day}` : ''}`
          : up ? 'Between rounds'
          : why ?? (row.state === 'unknown' ? 'Checking…' : 'No live data')}
      </div>
      <div className="live-row">
        <span className="live-big">{airborne.known ? airborne.total : '—'}</span>
        <span className="live-unit">
          {airborne.known ? `pilots on · ${airborne.blue} ${t.blueLabel} / ${airborne.red} ${t.redLabel}` : 'pilots on'}
        </span>
      </div>
      <div style={{ marginTop: 10 }}>
        <TerritoryBar front={front} loading={frontLoading} blueLabel={t.blueLabel} redLabel={t.redLabel} />
      </div>
    </div>
  )
}

export default function Hero() {
  const { rows } = useServerStates()
  return (
    <section id="top" className="hero theme-locked-dark">
      <div className="hero-img" role="img" aria-label="Two jets dogfighting low over a burning battlefield at sunset" />
      <div className="hero-scrim" />
      <div className="hero-grid" />
      <div className="hud-corner tl" />
      <div className="hud-corner tr" />

      <div className="wrap hero-body">
        <div className="hero-eyebrow fade-up d1">
          <span>DCS World</span><span className="sep">/</span>
          <span>Persistent PvP</span><span className="sep">/</span>
          <span>24/7</span>
        </div>
        <h1 className="hero-title fade-up d2">
          <span>Vector</span>
          <span className="accent">Strike</span>
        </h1>
        <p className="hero-lede fade-up d3">{campaign.pitch}</p>
        <div className="hero-ctas fade-up d4">
          <a href={links.discord} target="_blank" rel="noopener noreferrer" className="vs-btn">
            <Support size={15} />
            Join the Discord
          </a>
          <a href={links.dashboard} className="vs-btn vs-btn-outline">
            See the war live
            <ArrowRight size={14} />
          </a>
          <a href={wiki('playbooks/first-sortie')} className="text-link" style={{ marginLeft: 4 }}>
            New here? Your first sortie <ArrowRight size={12} />
          </a>
        </div>
      </div>

      <div className="wrap fade-up d5">
        <div className="live-strip" style={{ ['--cols' as string]: rows.length }}>
          {rows.map(r => <LiveCell key={r.theatre.key} row={r} />)}
          <div className="live-cta">
            <Link to="/servers" className="text-link">Servers <ArrowRight size={12} /></Link>
            <Link to="/status" className="text-link">Status <ArrowRight size={12} /></Link>
          </div>
        </div>
      </div>
    </section>
  )
}
