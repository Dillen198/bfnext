import { lazy, Suspense, useState } from 'react'
import { QueryClient, QueryClientProvider, useQuery } from '@tanstack/react-query'
import { Server, Activity, Config, Terminal, Download, Shield, FolderCog, Save, Rocket, Layers, Play } from '@icons'
import { mgr, inTauri, type ServerView } from './tauri'
import { Dot, Pill, Btn, Note, ServerTag } from './ui'
import logo from './logo.png'
import { OK, AMBER, RED, MONO, DIM, needsSetup } from './style'
import { pickServer, usePickedServer, resolveServer } from './pick'
import Overview from './Overview'
import Setup from './Setup'
import Settings from './Settings'
import Logs from './Logs'
import BotConfig from './BotConfig'
import Backup from './Backup'
import RemoteBackup from './RemoteBackup'
import NewBox from './NewBox'
import Servers from './Servers'
import Dcs from './Dcs'
import { ErrorBoundary } from '@shared/web/ErrorBoundary'
import { OpsTargetContext } from '../context/OpsTarget'

// The dashboard's own OPS page, unchanged, over the local transport.
const OpsPage = lazy(() => import('../pages/OpsPage'))

// Its queries are keyed ['ops', ...] with no server in them: each box gets a
// cache of its own, so switching boxes never shows the last one's data.
const opsClients = new Map<string, QueryClient>()
function opsClient(id: string): QueryClient {
  let c = opsClients.get(id)
  if (!c) {
    c = new QueryClient({ defaultOptions: { queries: { refetchOnWindowFocus: false } } })
    opsClients.set(id, c)
  }
  return c
}

type Tab = 'servers' | 'dcs' | 'ops' | 'logs' | 'backup' | 'botconfig' | 'overview' | 'setup' | 'newbox' | 'settings'

// The first group acts on the server picked at the top; the second is this PC.
const SERVER_TABS: { key: Tab; label: string; icon: typeof Server }[] = [
  { key: 'servers', label: 'ALL SERVERS', icon: Layers },
  { key: 'dcs', label: 'DCS CONTROL', icon: Play },
  { key: 'ops', label: 'SERVER OPS', icon: Server },
  { key: 'logs', label: 'LIVE LOGS', icon: Terminal },
  { key: 'backup', label: 'BACKUP', icon: Save },
  { key: 'botconfig', label: 'BOT CONFIG', icon: FolderCog },
]
const PC_TABS: { key: Tab; label: string; icon: typeof Server }[] = [
  { key: 'overview', label: 'OVERVIEW', icon: Activity },
  { key: 'setup', label: 'SETUP', icon: Shield },
  { key: 'newbox', label: 'NEW BOX', icon: Rocket },
  { key: 'settings', label: 'SETTINGS', icon: Config },
]

/** The server picker: every tab above the line acts on the one picked here. */
function Picker({ servers, current }: { servers: ServerView[]; current: ServerView | null }) {
  if (!servers.length) return <span style={{ fontSize: '0.68rem', color: AMBER }}>No server yet -- add one under ALL SERVERS</span>
  return (
    <div className="flex items-center gap-2" style={{ flexWrap: 'wrap' }} role="radiogroup" aria-label="Server box">
      {servers.map(s => {
        const on = s.id === current?.id
        return (
          <button key={s.id} role="radio" aria-checked={on} onClick={() => pickServer(s.id)} title={s.url ?? s.problem ?? ''} style={{
            display: 'inline-flex', alignItems: 'center', gap: 7, padding: '5px 12px', borderRadius: 3, cursor: 'pointer',
            background: on ? (s.local ? 'rgba(106,171,31,0.14)' : 'rgba(234,179,8,0.12)') : 'none',
            border: `1px solid ${on ? (s.local ? 'rgba(106,171,31,0.55)' : AMBER) : 'var(--border)'}`,
            color: on ? 'var(--text)' : 'var(--text-muted)', fontFamily: 'var(--font-display)', letterSpacing: '0.1em', fontSize: '0.78rem',
          }}>
            <Dot state={s.problem ? 'bad' : 'ok'} />{s.name}
            <span style={{ ...MONO, fontSize: '0.52rem', opacity: 0.7 }}>{s.local ? 'THIS PC' : 'REMOTE'}</span>
          </button>
        )
      })}
    </div>
  )
}

export default function ManagerApp() {
  const { data: state, error } = useQuery({
    queryKey: ['mgr', 'state'],
    queryFn: mgr.state,
    refetchInterval: 3_000,
    retry: false,
  })
  const picked = usePickedServer()
  const server = resolveServer(state, picked)
  const [tab, setTab] = useState<Tab | null>(null)
  const current: Tab = tab ?? (needsSetup(state) ? 'setup' : state?.config.controller_only ? 'servers' : 'overview')
  // BOT CONFIG's editor holds unsaved text only while mounted: leaving the tab
  // asks first
  const [cfgDirty, setCfgDirty] = useState(false)
  const [pendingTab, setPendingTab] = useState<Tab | null>(null)
  const go = (t: Tab) => {
    if (current === 'botconfig' && t !== 'botconfig' && cfgDirty) setPendingTab(t)
    else setTab(t)
  }

  const svc = state?.service
  const svcState = !svc?.installed ? 'bad' : svc.state === 'running' ? 'ok' : 'warn'
  const bot = state?.agent?.bot
  const botState = !state?.agent_fresh ? 'off' : bot?.running ? 'ok' : bot?.problem ? 'bad' : 'warn'
  const controller = !!state?.config.controller_only

  const navBtn = (t: { key: Tab; label: string; icon: typeof Server }) => {
    const active = current === t.key
    return (
      <button key={t.key} onClick={() => go(t.key)} style={{
        display: 'flex', alignItems: 'center', gap: 10, padding: '8px 10px', borderRadius: 3, cursor: 'pointer',
        background: active ? 'rgba(106,171,31,0.12)' : 'none', border: `1px solid ${active ? 'rgba(106,171,31,0.35)' : 'transparent'}`,
        color: active ? 'var(--accent-bright, var(--accent))' : 'var(--text-muted)', fontSize: '0.68rem',
        letterSpacing: '0.12em', textAlign: 'left',
      }}>
        <t.icon size={14} />
        {t.label}
        {t.key === 'setup' && needsSetup(state) && <span className="ml-auto"><Dot state="warn" /></span>}
      </button>
    )
  }
  const noServer = (
    <div className="p-6"><Note tone="warn">No server box to act on yet. Add one under ALL SERVERS{controller ? '' : ', or finish Setup for this PC'}.</Note></div>
  )

  return (
    <div style={{ display: 'flex', height: '100vh', background: 'var(--bg)', color: 'var(--text)' }}>
      <aside style={{ width: 210, flexShrink: 0, borderRight: '1px solid var(--border)', background: 'var(--bg-chrome)',
                      display: 'flex', flexDirection: 'column' }}>
        <div style={{ padding: '18px 16px 14px', borderBottom: '1px solid var(--border)' }}>
          <div className="flex items-center gap-2">
            <img src={logo} alt="" style={{ width: 30, height: 30, objectFit: 'contain' }} />
            <div>
              <div style={{ fontFamily: 'var(--font-display)', fontSize: '1.05rem', letterSpacing: '0.12em', lineHeight: 1 }}>FOWL ENGINE</div>
              <div style={{ ...DIM, fontSize: '0.55rem', marginTop: 3 }}>MANAGER {state ? `v${state.version}` : ''}</div>
            </div>
          </div>
        </div>
        <nav style={{ padding: '10px 8px', display: 'flex', flexDirection: 'column', gap: 2, overflowY: 'auto' }}>
          <div style={{ ...DIM, fontSize: '0.52rem', padding: '2px 10px 4px' }}>picked server</div>
          {SERVER_TABS.map(navBtn)}
          <div style={{ ...DIM, fontSize: '0.52rem', padding: '12px 10px 4px', borderTop: '1px solid var(--border)', marginTop: 8 }}>this pc</div>
          {PC_TABS.map(navBtn)}
        </nav>
        <div style={{ marginTop: 'auto', padding: '12px 14px', borderTop: '1px solid var(--border)', fontSize: '0.62rem', lineHeight: 1.9 }}>
          {controller ? (
            <div className="flex items-center gap-2"><Dot state="ok" /> controller (no bot here)</div>
          ) : (
            <>
              <div className="flex items-center gap-2"><Dot state={svcState} /> service {svc?.installed ? svc.state : 'not installed'}</div>
              <div className="flex items-center gap-2"><Dot state={botState} /> bot {!state?.agent_fresh ? 'unknown' : bot?.running ? 'running' : bot?.paused ? 'paused' : 'down'}</div>
            </>
          )}
          {state?.agent?.update?.update_available && (
            <div className="flex items-center gap-2" style={{ color: AMBER }}><Download size={11} /> update {state.agent.update.latest?.version}</div>
          )}
          <div style={{ color: 'var(--text-dim)', ...MONO, marginTop: 4 }}>{state?.hostname}</div>
        </div>
      </aside>

      <main style={{ flex: 1, minWidth: 0, display: 'flex', flexDirection: 'column', overflow: 'hidden' }}>
        <header className="flex items-center gap-3" style={{
          padding: '8px 16px', borderBottom: '1px solid var(--border)', background: 'var(--bg-chrome)', flexWrap: 'wrap',
        }}>
          <span style={{ ...DIM, fontSize: '0.55rem' }}>acting on</span>
          {server ? <ServerTag name={server.name} local={server.local} /> : <span style={{ color: AMBER, fontSize: '0.7rem' }}>—</span>}
          <span className="ml-auto"><Picker servers={state?.servers ?? []} current={server} /></span>
        </header>
        {!inTauri && (
          <div style={{ padding: '6px 16px', fontSize: '0.66rem', color: AMBER, borderBottom: '1px solid var(--border)' }}>
            Preview mode: this is the manager UI in a plain browser. Open the installed Fowl Engine Manager to control the server.
          </div>
        )}
        {error && !state && (
          <div style={{ padding: '10px 16px', fontSize: '0.7rem', color: RED }}>
            {error instanceof Error ? error.message : String(error)}
          </div>
        )}
        {state && !state.elevated && inTauri && (
          <div style={{ padding: '6px 16px', fontSize: '0.66rem', color: AMBER, borderBottom: '1px solid var(--border)' }}>
            Not running as administrator -- installing or controlling the service will fail. Right-click → Run as administrator.
          </div>
        )}
        {pendingTab && (
          <div className="flex items-center gap-2" style={{ padding: '6px 16px', fontSize: '0.68rem', color: AMBER, borderBottom: '1px solid var(--border)' }}>
            BOT CONFIG has unsaved changes.
            <Btn danger onClick={() => { setCfgDirty(false); setTab(pendingTab); setPendingTab(null) }}>Discard and leave</Btn>
            <Btn onClick={() => setPendingTab(null)}>Stay</Btn>
          </div>
        )}
        <div style={{ flex: 1, overflow: 'auto', display: 'flex', flexDirection: 'column' }}>
          {current === 'servers' && state && (
            <Servers state={state} current={server} onGo={(id, t) => { pickServer(id); setTab(t) }} />
          )}
          {current === 'dcs' && (server ? <Dcs key={server.id} server={server} /> : noServer)}
          {current === 'logs' && <Logs key={server?.id ?? 'none'} server={server} />}
          {current === 'backup' && state && (
            server && !server.local ? <RemoteBackup key={server.id} server={server} /> : <Backup state={state} />
          )}
          {current === 'botconfig' && (server && !server.local ? (
            <div className="p-6" style={{ fontSize: '0.74rem', lineHeight: 1.6, maxWidth: 820 }}>
              <Note>
                BOT CONFIG edits this PC's DCSServerBot files directly, so it only works on the box itself. For <b>{server.name}</b>,
                edit its <span style={MONO}>fowlengine.yaml</span> under SERVER OPS → Configuration: everything but the keys that name a
                program, a path, a URL or an update source (those are changed on the box itself, so a remote login can't turn into
                running a command there).{' '}
                <Btn primary onClick={() => setTab('ops')}>Open SERVER OPS for {server.name}</Btn>
              </Note>
            </div>
          ) : <BotConfig onDirtyChange={setCfgDirty} />)}
          {current === 'ops' && (
            !server ? noServer : server.problem ? (
              <div className="p-6" style={{ fontSize: '0.74rem', lineHeight: 1.6 }}>
                <div style={{ color: AMBER, marginBottom: 8 }}>The Server OPS view can't reach {server.name} yet.</div>
                <div style={{ color: 'var(--text-muted)' }}>{server.problem}</div>
                <div style={{ color: 'var(--text-dim)', marginTop: 8 }}>
                  {server.local
                    ? <>It talks to the FowlEngine plugin through DCSServerBot's WebService, with the key in fowlengine.yaml
                        (<span style={MONO}>ops_api.api_key</span>, or <span style={MONO}>bfdb.dcsserverbot_api_key</span>).</>
                    : <>Edit it under ALL SERVERS.</>}
                </div>
              </div>
            ) : (
              <OpsTargetContext.Provider value={(state?.servers.length ?? 0) > 1 || !server.local ? server.name : null}>
                <QueryClientProvider client={opsClient(server.id)}>
                  <ErrorBoundary key={server.id}>
                    <Suspense fallback={<div className="p-6" style={DIM}>Loading…</div>}>
                      <OpsPage />
                    </Suspense>
                  </ErrorBoundary>
                </QueryClientProvider>
              </OpsTargetContext.Provider>
            )
          )}
          {current === 'overview' && <Overview state={state} onSetup={() => setTab('setup')} />}
          {current === 'setup' && state && <Setup state={state} onDone={() => setTab('overview')} onRestore={() => setTab('backup')}
                                                  onNewBox={() => setTab('newbox')} onController={() => setTab('servers')} />}
          {current === 'newbox' && <NewBox />}
          {current === 'settings' && state && <Settings state={state} />}
        </div>
        <footer style={{ padding: '5px 14px', borderTop: '1px solid var(--border)', fontSize: '0.58rem', color: 'var(--text-dim)',
                         display: 'flex', gap: 14, ...MONO }}>
          <span>{state?.exe}</span>
          <span className="ml-auto">{state?.config.auto_update ? <span style={{ color: OK }}>auto-update on</span> : 'auto-update off'}</span>
          {server?.url && <Pill color="var(--text-dim)">ops {server.url}</Pill>}
        </footer>
      </main>
    </div>
  )
}
