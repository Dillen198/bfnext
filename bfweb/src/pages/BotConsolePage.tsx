// SERVER BOT: DCSServerBot's own console, in the dashboard. Every control is
// one of the bot's actions (core/actions.py), reached through bfdb and the
// FowlEngine plugin (console.py), so it behaves, refuses and audits exactly as
// the same command does from Discord. See pages/bot/*.

import { useEffect, useState } from 'react'
import { useQuery, useQueryClient } from '@tanstack/react-query'
import { Navigate, useSearchParams } from 'react-router-dom'
import { api, type BotFleet } from '../api'
import { useAuth } from '../context/AuthContext'
import { canSeeBotConsole, parseFleetEvent } from '../lib/botconsole'
import PageHeader from '../components/PageHeader'
import { Alert, CheckCircle2, RefreshCw, Server } from '@icons'
import { Btn, Empty, NoticeBar, RunnerProvider, useCatalog } from './bot/ui'
import { DIM, MONO, RED, errText } from './bot/style'
import ServersTab from './bot/ServersTab'
import PlayersTab from './bot/PlayersTab'
import BansTab from './bot/BansTab'
import NodesTab from './bot/NodesTab'
import { ActionsTab, AuditTab, MusicTab, StatsTab } from './bot/MoreTabs'
import { AccessTab, PresetsTab, SettingsTab } from './bot/ConfigTabs'

const TABS = [
  { key: 'servers', label: 'Servers' },
  { key: 'players', label: 'Players' },
  { key: 'bans', label: 'Bans' },
  { key: 'nodes', label: 'Nodes' },
  { key: 'stats', label: 'Statistics' },
  { key: 'mods', label: 'Mods' },
  { key: 'music', label: 'Music' },
  { key: 'audit', label: 'Audit' },
  { key: 'settings', label: 'Settings', admin: true },
  { key: 'presets', label: 'Presets', admin: true },
  { key: 'access', label: 'Access' },
  { key: 'all', label: 'All actions' },
] as const
type TabKey = typeof TABS[number]['key']

function Tile({ label, value, tone }: { label: string; value: string | number; tone?: 'ok' | 'warn' | 'bad' }) {
  const color = tone === 'bad' ? RED : tone === 'warn' ? 'var(--yellow)' : tone === 'ok' ? 'var(--accent)' : 'var(--text)'
  return (
    <div className="vs-card" style={{ padding: '0.6rem 0.9rem', minWidth: 120 }}>
      <div style={DIM}>{label}</div>
      <div style={{ fontSize: '1.1rem', color, ...MONO }}>{value}</div>
    </div>
  )
}

function Summary({ fleet }: { fleet: BotFleet }) {
  const up = fleet.servers.filter(s => ['RUNNING', 'PAUSED'].includes((s.status ?? '').toUpperCase())).length
  const paused = fleet.servers.filter(s => (s.status ?? '').toUpperCase() === 'PAUSED').length
  const players = fleet.servers.reduce((n, s) => n + s.players.length, 0)
  const nodesUp = fleet.nodes.filter(n => n.online).length
  const maint = fleet.servers.filter(s => s.maintenance).length
  return (
    <div className="flex gap-3" style={{ flexWrap: 'wrap' }}>
      <Tile label="Servers up" value={`${up} / ${fleet.servers.length}`} tone={up === fleet.servers.length ? 'ok' : 'warn'} />
      <Tile label="Players" value={players} />
      <Tile label="Nodes online" value={`${nodesUp} / ${fleet.nodes.length}`} tone={nodesUp === fleet.nodes.length ? 'ok' : 'bad'} />
      {paused > 0 && <Tile label="Paused" value={paused} tone="warn" />}
      {maint > 0 && <Tile label="Maintenance" value={maint} tone="warn" />}
    </div>
  )
}

/** The bot pushes the fleet whenever it changes (server-sent events through
 *  bfdb); while that stream is open the page doesn't poll. Returns whether it is. */
function useFleetStream(): boolean {
  const qc = useQueryClient()
  const [live, setLive] = useState(false)
  useEffect(() => {
    const url = api.bot.streamUrl()
    if (!url || typeof EventSource === 'undefined') return
    const es = new EventSource(url, { withCredentials: true })
    es.addEventListener('fleet', e => {
      const doc = parseFleetEvent((e as MessageEvent<string>).data)
      if (doc) {
        qc.setQueryData(['bot', 'fleet'], doc)
        setLive(true)
      }
    })
    // EventSource reconnects by itself; until it does, polling takes over
    es.onerror = () => setLive(false)
    return () => es.close()
  }, [qc])
  return live
}

function Body() {
  const [params, setParams] = useSearchParams()
  const catalog = useCatalog()
  const isAdmin = (catalog.data?.roles ?? []).includes('admin')
  const tabs = TABS.filter(t => !('admin' in t) || isAdmin)
  const tab = (tabs.find(t => t.key === params.get('tab'))?.key ?? 'servers') as TabKey
  const live = useFleetStream()
  const fleet = useQuery({
    queryKey: ['bot', 'fleet'],
    queryFn: api.bot.fleet,
    refetchInterval: live ? false : 5_000,
    retry: false,
  })
  const setTab = (t: TabKey) => setParams(p => { const n = new URLSearchParams(p); n.set('tab', t); return n }, { replace: true })
  const err = catalog.error ?? fleet.error
  return (
    <div className="flex-1 overflow-auto p-4 space-y-4">
      {err && (
        <div className="vs-card flex items-start gap-2" style={{ padding: '0.75rem 1rem', borderColor: 'rgba(239,68,68,0.4)' }}>
          <Alert size={13} style={{ color: RED, flexShrink: 0, marginTop: 2 }} />
          <div style={{ fontSize: '0.7rem', color: RED, lineHeight: 1.5 }}>
            {errText(err)}
            {fleet.data && <span style={{ color: 'var(--text-dim)' }}> -- showing the last good snapshot. (bfdb or the bot may be restarting.)</span>}
          </div>
        </div>
      )}
      {catalog.data && !catalog.data.available && (
        <div className="vs-card flex items-start gap-2" style={{ padding: '0.75rem 1rem', borderColor: 'rgba(234,179,8,0.4)' }}>
          <Alert size={13} style={{ color: 'var(--yellow)', flexShrink: 0, marginTop: 2 }} />
          <div style={{ fontSize: '0.7rem', color: 'var(--text)', lineHeight: 1.5 }}>
            {catalog.data.reason ?? 'This bot has no action layer.'} The server list still shows; its controls need the newer bot
            {catalog.data.bot_version ? ` (this one is ${catalog.data.bot_version})` : ''}.
          </div>
        </div>
      )}
      <NoticeBar />
      {fleet.data && (
        <div className="flex items-center gap-2" style={{ fontSize: '0.6rem', color: 'var(--text-dim)' }}>
          {fleet.isFetching ? <RefreshCw size={10} /> : <CheckCircle2 size={10} style={{ color: 'var(--accent)' }} />}
          {live ? 'live, pushed by the bot' : 'refreshes every 5 s'} · {new Date(fleet.data.generated).toLocaleTimeString()}
          {catalog.data?.bot_version && <> · DCSServerBot {catalog.data.bot_version}</>}
          {catalog.data?.roles?.length ? <> · you: {catalog.data.roles.join(', ')}</> : null}
        </div>
      )}
      {fleet.data && <Summary fleet={fleet.data} />}
      <div className="flex gap-1" style={{ flexWrap: 'wrap', borderBottom: '1px solid var(--border)', paddingBottom: 8 }}>
        {tabs.map(t => <Btn key={t.key} active={tab === t.key} onClick={() => setTab(t.key)}>{t.label}</Btn>)}
      </div>
      {!fleet.data && !fleet.error && <Empty>Loading…</Empty>}
      {fleet.data && (
        <>
          {tab === 'servers' && <ServersTab fleet={fleet.data} />}
          {tab === 'players' && <PlayersTab fleet={fleet.data} />}
          {tab === 'bans' && <BansTab />}
          {tab === 'nodes' && <NodesTab fleet={fleet.data} />}
          {tab === 'stats' && <StatsTab fleet={fleet.data} />}
          {tab === 'mods' && <ActionsTab only={['modmanager']} fleet={fleet.data} />}
          {tab === 'music' && <MusicTab fleet={fleet.data} />}
          {tab === 'settings' && <SettingsTab />}
          {tab === 'presets' && <PresetsTab />}
          {tab === 'access' && <AccessTab />}
          {tab === 'audit' && <AuditTab />}
          {tab === 'all' && <ActionsTab fleet={fleet.data} />}
        </>
      )}
    </div>
  )
}

export default function BotConsolePage() {
  const { user, loading } = useAuth()
  if (loading) return null
  if (!user) return <Navigate to="/login" replace />
  if (!canSeeBotConsole(user)) return <Navigate to="/" replace />
  return (
    <div className="flex flex-col flex-1 overflow-hidden">
      <PageHeader art="carrier-quarter" icon={Server} title="SERVER BOT" sub="DCSServerBot: servers, missions, players, bans, nodes" />
      <RunnerProvider><Body /></RunnerProvider>
    </div>
  )
}
