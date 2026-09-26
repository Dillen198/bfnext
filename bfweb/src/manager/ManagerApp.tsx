import { lazy, Suspense, useState } from 'react'
import { useQuery } from '@tanstack/react-query'
import { Server, Activity, Config, Terminal, Download, Shield } from '@icons'
import { mgr, inTauri } from './tauri'
import { Dot, Pill } from './ui'
import logo from './logo.png'
import { OK, AMBER, RED, MONO, DIM, needsSetup } from './style'
import Overview from './Overview'
import Setup from './Setup'
import Settings from './Settings'
import Logs from './Logs'

// The dashboard's own OPS page, unchanged, over the local transport.
const OpsPage = lazy(() => import('../pages/OpsPage'))

type Tab = 'overview' | 'ops' | 'setup' | 'settings' | 'logs'

const TABS: { key: Tab; label: string; icon: typeof Server }[] = [
  { key: 'overview', label: 'OVERVIEW', icon: Activity },
  { key: 'ops', label: 'SERVER OPS', icon: Server },
  { key: 'setup', label: 'SETUP', icon: Shield },
  { key: 'settings', label: 'SETTINGS', icon: Config },
  { key: 'logs', label: 'LOGS', icon: Terminal },
]

export default function ManagerApp() {
  const { data: state, error } = useQuery({
    queryKey: ['mgr', 'state'],
    queryFn: mgr.state,
    refetchInterval: 3_000,
    retry: false,
  })
  const [tab, setTab] = useState<Tab | null>(null)
  const current: Tab = tab ?? (needsSetup(state) ? 'setup' : 'overview')

  const svc = state?.service
  const svcState = !svc?.installed ? 'bad' : svc.state === 'running' ? 'ok' : 'warn'
  const bot = state?.agent?.bot
  const botState = !state?.agent_fresh ? 'off' : bot?.running ? 'ok' : bot?.problem ? 'bad' : 'warn'

  return (
    <div style={{ display: 'flex', height: '100vh', background: 'var(--bg)', color: 'var(--text)' }}>
      <aside style={{ width: 210, flexShrink: 0, borderRight: '1px solid var(--border)', background: 'var(--bg-chrome)',
                      display: 'flex', flexDirection: 'column' }}>
        <div style={{ padding: '18px 16px 14px', borderBottom: '1px solid var(--border)' }}>
          <div className="flex items-center gap-2">
            <img src={logo} alt="" style={{ width: 30, height: 30, objectFit: 'contain' }} />
            <div>
              <div style={{ fontFamily: 'var(--font-display, "Bebas Neue")', fontSize: '1.05rem', letterSpacing: '0.12em', lineHeight: 1 }}>FOWL ENGINE</div>
              <div style={{ ...DIM, fontSize: '0.55rem', marginTop: 3 }}>MANAGER {state ? `v${state.version}` : ''}</div>
            </div>
          </div>
        </div>
        <nav style={{ padding: '10px 8px', display: 'flex', flexDirection: 'column', gap: 2 }}>
          {TABS.map(t => {
            const active = current === t.key
            return (
              <button key={t.key} onClick={() => setTab(t.key)} style={{
                display: 'flex', alignItems: 'center', gap: 10, padding: '9px 10px', borderRadius: 3, cursor: 'pointer',
                background: active ? 'rgba(106,171,31,0.12)' : 'none', border: `1px solid ${active ? 'rgba(106,171,31,0.35)' : 'transparent'}`,
                color: active ? 'var(--accent-bright, var(--accent))' : 'var(--text-muted)', fontSize: '0.68rem',
                letterSpacing: '0.12em', textAlign: 'left',
              }}>
                <t.icon size={14} />
                {t.label}
                {t.key === 'setup' && needsSetup(state) && <span className="ml-auto"><Dot state="warn" /></span>}
              </button>
            )
          })}
        </nav>
        <div style={{ marginTop: 'auto', padding: '12px 14px', borderTop: '1px solid var(--border)', fontSize: '0.62rem', lineHeight: 1.9 }}>
          <div className="flex items-center gap-2"><Dot state={svcState} /> service {svc?.installed ? svc.state : 'not installed'}</div>
          <div className="flex items-center gap-2"><Dot state={botState} /> bot {!state?.agent_fresh ? 'unknown' : bot?.running ? 'running' : bot?.paused ? 'paused' : 'down'}</div>
          {state?.agent?.update?.update_available && (
            <div className="flex items-center gap-2" style={{ color: AMBER }}><Download size={11} /> update {state.agent.update.latest?.version}</div>
          )}
          <div style={{ color: 'var(--text-dim)', ...MONO, marginTop: 4 }}>{state?.hostname}</div>
        </div>
      </aside>

      <main style={{ flex: 1, minWidth: 0, display: 'flex', flexDirection: 'column', overflow: 'hidden' }}>
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
        <div style={{ flex: 1, overflow: 'auto', display: 'flex', flexDirection: 'column' }}>
          {current === 'overview' && <Overview state={state} onSetup={() => setTab('setup')} />}
          {current === 'setup' && state && <Setup state={state} onDone={() => setTab('overview')} />}
          {current === 'settings' && state && <Settings state={state} />}
          {current === 'logs' && <Logs />}
          {current === 'ops' && (
            state?.ops_error ? (
              <div className="p-6" style={{ fontSize: '0.74rem', lineHeight: 1.6 }}>
                <div style={{ color: AMBER, marginBottom: 8 }}>The Server OPS view can't reach the bot yet.</div>
                <div style={{ color: 'var(--text-muted)' }}>{state.ops_error}</div>
                <div style={{ color: 'var(--text-dim)', marginTop: 8 }}>
                  It talks to the FowlEngine plugin through DCSServerBot's WebService, with the key in fowlengine.yaml
                  (<span style={MONO}>bfdb.dcsserverbot_api_key</span>).
                </div>
              </div>
            ) : (
              <Suspense fallback={<div className="p-6" style={DIM}>Loading…</div>}>
                <OpsPage />
              </Suspense>
            )
          )}
        </div>
        <footer style={{ padding: '5px 14px', borderTop: '1px solid var(--border)', fontSize: '0.58rem', color: 'var(--text-dim)',
                         display: 'flex', gap: 14, ...MONO }}>
          <span>{state?.exe}</span>
          <span className="ml-auto">{state?.config.auto_update ? <span style={{ color: OK }}>auto-update on</span> : 'auto-update off'}</span>
          {state?.ops_target && <Pill color="var(--text-dim)">ops {state.ops_target}</Pill>}
        </footer>
      </main>
    </div>
  )
}
