import { useState } from 'react'
import {
  Aircraft,
  Alert,
  Capture,
  Cas,
  Comms,
  Csar,
  Defend,
  Intercept,
  Logistics,
  Objective,
  Recon,
  Sam,
  Sead,
  Strike,
  Supply,
  Tacmap,
  type IconComponent,
  Clock,
} from '../icons'
import type { SituationReport, Task, Hotspot, Urgency } from '../api'
import BriefingMap from './BriefingMap'

const URGENCY_COLOR = (u: Urgency) =>
  u === 'critical' ? '#f04747' : u === 'high' ? '#f0a030' : '#8ec83f'

const KIND_ICON: Record<Task['kind'], IconComponent> = {
  defend: Defend,
  capture: Capture,
  strike: Strike,
  sead: Sead,
  cas: Cas,
  intercept: Intercept,
  logistics: Logistics,
  recon: Recon,
  csar: Csar,
}

function Card({
  title, icon: Icon, count, children, accent,
}: {
  title: string
  icon: IconComponent
  count?: number | string
  children: React.ReactNode
  accent?: string
}) {
  return (
    <div className="vs-card" style={{ overflow: 'hidden', flexShrink: 0 }}>
      <div
        style={{
          display: 'flex', alignItems: 'center', gap: 8, padding: '9px 13px',
          borderBottom: '1px solid var(--border)',
        }}
      >
        <Icon size={13} style={{ color: accent ?? 'var(--accent)' }} />
        <span style={{ fontSize: '0.7rem', fontWeight: 700, letterSpacing: '0.1em', textTransform: 'uppercase' }}>
          {title}
        </span>
        {count != null && (
          <span style={{ marginLeft: 'auto', fontSize: '0.64rem', color: 'var(--text-dim)', fontFamily: 'var(--font-mono)' }}>
            {count}
          </span>
        )}
      </div>
      {children}
    </div>
  )
}

const dim: React.CSSProperties = { fontSize: '0.7rem', color: 'var(--text-dim)', padding: '10px 13px' }
const mono: React.CSSProperties = { fontFamily: 'var(--font-mono)' }

function Bar({ value, warn }: { value: number; warn?: boolean }) {
  return (
    <span
      style={{
        display: 'inline-block', width: 42, height: 5, borderRadius: 3,
        background: 'var(--bg-elevated)', overflow: 'hidden', verticalAlign: 'middle',
      }}
    >
      <span
        style={{
          display: 'block', height: '100%', width: `${Math.max(0, Math.min(100, value))}%`,
          background: warn || value < 25 ? '#f04747' : value < 60 ? '#f0a030' : '#8ec83f',
        }}
      />
    </span>
  )
}

// ── Tasking ────────────────────────────────────────────────────────────────

function TaskRow({
  task, index, selected, onSelect,
}: { task: Task; index: number; selected: boolean; onSelect: () => void }) {
  const Icon = KIND_ICON[task.kind] ?? Objective
  const color = URGENCY_COLOR(task.urgency)
  return (
    <button
      onClick={onSelect}
      style={{
        display: 'block', width: '100%', textAlign: 'left', border: 'none', cursor: 'pointer',
        padding: '9px 13px', borderBottom: '1px solid var(--border)',
        background: selected ? 'var(--bg-elevated)' : 'transparent',
        borderLeft: `3px solid ${color}`,
        color: 'var(--text)',
      }}
    >
      <div style={{ display: 'flex', alignItems: 'center', gap: 7 }}>
        <span
          style={{
            ...mono, minWidth: 17, height: 17, borderRadius: 3, background: color, color: '#0b0f08',
            fontSize: '0.6rem', fontWeight: 800, display: 'inline-flex',
            alignItems: 'center', justifyContent: 'center',
          }}
        >
          {index}
        </span>
        <Icon size={12} style={{ color, flexShrink: 0 }} />
        <span style={{ fontSize: '0.74rem', fontWeight: 700, letterSpacing: '0.02em' }}>{task.title}</span>
        {task.range_nm != null && task.bearing_deg != null && (
          <span style={{ ...mono, marginLeft: 'auto', fontSize: '0.64rem', color: 'var(--text-dim)' }}>
            {String(task.bearing_deg).padStart(3, '0')}°/{task.range_nm.toFixed(0)}nm
          </span>
        )}
      </div>
      <div style={{ fontSize: '0.7rem', color: 'var(--text-muted)', lineHeight: 1.5, marginTop: 4 }}>
        {task.detail}
      </div>
      {selected && (
        <div style={{ marginTop: 6, paddingTop: 6, borderTop: '1px solid var(--border)' }}>
          {task.success && (
            <div style={{ fontSize: '0.68rem', color: 'var(--text-muted)' }}>
              <strong style={{ color: 'var(--text)' }}>Done when:</strong> {task.success}
            </div>
          )}
          {task.roles.length > 0 && (
            <div style={{ fontSize: '0.66rem', color: 'var(--text-dim)', marginTop: 3 }}>
              Wants: {task.roles.join(', ')}
            </div>
          )}
          <div style={{ ...mono, fontSize: '0.62rem', color: 'var(--text-dim)', marginTop: 3 }}>
            {task.lat.toFixed(4)}, {task.lon.toFixed(4)}
          </div>
        </div>
      )}
    </button>
  )
}

// ── Hotspots ───────────────────────────────────────────────────────────────

function HotspotRow({ h }: { h: Hotspot }) {
  const color = URGENCY_COLOR(h.risk)
  const side = h.owner === 'Blue' ? '#4a8fd4' : h.owner === 'Red' ? '#cc4444' : '#8a8a6a'
  return (
    <div style={{ padding: '8px 13px', borderBottom: '1px solid var(--border)', borderLeft: `3px solid ${color}` }}>
      <div style={{ display: 'flex', alignItems: 'center', gap: 7, flexWrap: 'wrap' }}>
        <span style={{ width: 7, height: 7, borderRadius: '50%', background: side, flexShrink: 0 }} />
        <span style={{ fontSize: '0.73rem', fontWeight: 700 }}>{h.objective}</span>
        <span style={{ fontSize: '0.62rem', color: 'var(--text-dim)' }}>{h.kind}</span>
        {h.capture_progress && (
          <span style={{ ...mono, fontSize: '0.62rem', color: '#f04747', fontWeight: 700 }}>
            {h.capture_progress[0].toUpperCase()} TIMER {h.capture_progress[1]}s / ~{h.capture_progress[2]}s
          </span>
        )}
        {h.captureable && !h.capture_progress && (
          <span style={{ ...mono, fontSize: '0.62rem', color: '#f0a030', fontWeight: 700 }}>CAPTURABLE</span>
        )}
        {h.in_capture_hold && (
          <span style={{ ...mono, fontSize: '0.62rem', color: 'var(--text-dim)' }}>CONSOLIDATING</span>
        )}
      </div>
      <div style={{ display: 'flex', gap: 12, marginTop: 5, ...mono, fontSize: '0.63rem', color: 'var(--text-dim)' }}>
        <span>HP <Bar value={h.health} /> {h.health}%</span>
        <span>LOGI <Bar value={h.logi} /> {h.logi}%</span>
        <span>SUP <Bar value={h.supply} /> {h.supply}%</span>
      </div>
      <div style={{ fontSize: '0.69rem', color: 'var(--text-muted)', marginTop: 5, lineHeight: 1.5 }}>
        {h.status}
      </div>
      {h.repair_outlook && (
        <div style={{ fontSize: '0.66rem', color: 'var(--text-dim)', marginTop: 2 }}>{h.repair_outlook}</div>
      )}
    </div>
  )
}

// ── Tab ────────────────────────────────────────────────────────────────────

export default function SituationTab({ report }: { report: SituationReport }) {
  const [selectedTaskId, setSelectedTaskId] = useState<string | null>(null)
  const p = report.posture
  const a = report.air
  const l = report.logistics

  const stat = (label: string, value: React.ReactNode, tint?: string) => (
    <div style={{ minWidth: 84 }}>
      <div style={{ fontSize: '0.58rem', letterSpacing: '0.1em', textTransform: 'uppercase', color: 'var(--text-dim)' }}>
        {label}
      </div>
      <div style={{ ...mono, fontSize: '0.92rem', fontWeight: 700, color: tint ?? 'var(--text)' }}>{value}</div>
    </div>
  )

  return (
    <div style={{ display: 'flex', flex: 1, minHeight: 0, minWidth: 0, flexDirection: 'column', gap: 12 }}>
      {/* ── Headline ─────────────────────────────────────────────── */}
      <div className="vs-card" style={{ padding: '12px 14px', flexShrink: 0 }}>
        <div style={{ fontSize: '0.82rem', lineHeight: 1.65, color: 'var(--text)' }}>{report.headline}</div>
        {p.last_stand && (
          <div
            style={{
              marginTop: 8, padding: '6px 10px', borderRadius: 4,
              background: '#f0474722', border: '1px solid #f0474766',
              color: '#f87171', fontSize: '0.72rem', fontWeight: 700,
            }}
          >
            <Alert size={12} style={{ verticalAlign: -2, marginRight: 5 }} />
            {p.last_stand}
          </div>
        )}
        <div style={{ display: 'flex', gap: 20, flexWrap: 'wrap', marginTop: 11 }}>
          {stat('Territory', `${p.territory_pct.toFixed(0)}%`)}
          {stat('Yours', `${p.friendly_objectives} (${p.friendly_primary}P)`, '#4a8fd4')}
          {stat('Enemy', `${p.enemy_objectives} (${p.enemy_primary}P)`, '#cc4444')}
          {stat('Neutral', p.neutral_objectives)}
          {stat('Treasury', `${p.treasury} pts`)}
          {stat('Pilots', `${p.players_friendly} v ${p.players_enemy}`)}
          {stat('Convoys', l.convoys_active)}
          {report.mission_time && stat('Mission', report.mission_time)}
        </div>
        {report.weather && (
          <div style={{ fontSize: '0.7rem', color: 'var(--text-muted)', marginTop: 10 }}>
            <strong style={{ color: 'var(--text)' }}>WX</strong> {report.weather.summary} · QNH{' '}
            {report.weather.qnh_inhg.toFixed(2)} inHg / {report.weather.qnh_hpa.toFixed(0)} hPa ·{' '}
            {report.weather.temp_c.toFixed(0)}°C
          </div>
        )}
        {p.victory_condition && (
          <div style={{ fontSize: '0.68rem', color: 'var(--text-dim)', marginTop: 4 }}>
            Victory: {p.victory_condition}
          </div>
        )}
      </div>

      {/* ── Map + panels ─────────────────────────────────────────── */}
      <div
        style={{
          display: 'grid', gridTemplateColumns: 'minmax(0, 1.15fr) minmax(340px, 1fr)',
          gap: 12, flex: 1, minHeight: 0,
        }}
        className="briefing-split"
      >
        <div className="vs-card" style={{ overflow: 'hidden', minHeight: 380, position: 'relative' }}>
          <BriefingMap
            report={report}
            selectedTaskId={selectedTaskId}
            onSelectTask={setSelectedTaskId}
          />
          <div
            style={{
              position: 'absolute', bottom: 8, left: 8, padding: '6px 9px', borderRadius: 4,
              background: 'var(--bg-card)', border: '1px solid var(--border)',
              fontSize: '0.6rem', color: 'var(--text-dim)', lineHeight: 1.7, pointerEvents: 'none',
            }}
          >
            <div><span style={{ color: '#4a8fd4' }}>●</span> blue · <span style={{ color: '#cc4444' }}>●</span> red · hollow = damaged</div>
            <div><span style={{ color: '#f04747' }}>◌</span> known SAM ring (your intel) · <span style={{ color: '#facc15' }}>▲</span> hub</div>
            <div>numbered pins = tasking, click to expand</div>
          </div>
        </div>

        <div style={{ overflowY: 'auto', minHeight: 0, display: 'flex', flexDirection: 'column', gap: 12, paddingRight: 2 }}>
          <Card title="Tasking" icon={Objective} count={report.tasking.length}>
            {report.tasking.length === 0 ? (
              <div style={dim}>
                Nothing pressing. Soften an enemy objective before it can be taken, or resupply your own.
              </div>
            ) : (
              report.tasking.map((t, i) => (
                <TaskRow
                  key={t.id}
                  task={t}
                  index={i + 1}
                  selected={selectedTaskId === t.id}
                  onSelect={() => setSelectedTaskId(selectedTaskId === t.id ? null : t.id)}
                />
              ))
            )}
          </Card>

          <Card title="Hotspots" icon={Alert} count={report.hotspots.length}>
            {report.hotspots.length === 0 ? (
              <div style={dim}>No objective is in contact or takeable right now.</div>
            ) : (
              report.hotspots.map((h) => <HotspotRow key={h.objective} h={h} />)
            )}
          </Card>

          <Card
            title="Air Picture"
            icon={Aircraft}
            count={a.radar_blind ? 'BLIND' : `${a.hostile_tracks} hostile`}
            accent={a.radar_blind ? '#f04747' : undefined}
          >
            <div style={{ padding: '9px 13px', fontSize: '0.71rem', lineHeight: 1.6 }}>
              {a.radar_blind ? (
                <span style={{ color: '#f87171' }}>
                  Your radar net is reporting nothing — that is <strong>blind</strong>, not clear. Assume
                  hostile air is up and unobserved.
                </span>
              ) : (
                <>
                  <div style={mono}>
                    {a.hostile_tracks} hostile ({a.stale_tracks} coasting) · {a.friendly_airborne} friendly airborne
                  </div>
                  {a.axis && <div style={{ color: 'var(--text-muted)', marginTop: 3 }}>{a.axis}</div>}
                  {a.nearest && (
                    <div style={{ color: 'var(--text-muted)', marginTop: 5 }}>
                      Nearest: <strong style={{ color: 'var(--text)' }}>{a.nearest.class}</strong>{' '}
                      <span style={mono}>
                        {String(a.nearest.bearing_deg).padStart(3, '0')}°/{a.nearest.range_nm.toFixed(0)}nm
                      </span>{' '}
                      off {a.nearest.near}, {a.nearest.alt_ft.toLocaleString()} ft, {a.nearest.speed_kts} kt
                    </div>
                  )}
                </>
              )}
            </div>
          </Card>

          <Card title="Known Air Defence" icon={Sam} count={report.threats.length} accent="#f04747">
            {report.threats.length === 0 ? (
              <div style={dim}>
                Nothing held — no recon, SF, JTAC or ELINT contact on enemy air defence. Every strike you
                plan right now flies into an unmapped SAM picture.
              </div>
            ) : (
              report.threats.map((t, i) => (
                <div key={i} style={{ padding: '7px 13px', borderBottom: '1px solid var(--border)' }}>
                  <div style={{ display: 'flex', gap: 8, alignItems: 'baseline', flexWrap: 'wrap' }}>
                    <span style={{ fontSize: '0.72rem', fontWeight: 700 }}>{t.label}</span>
                    {t.near && <span style={{ fontSize: '0.65rem', color: 'var(--text-dim)' }}>near {t.near}</span>}
                  </div>
                  <div style={{ ...mono, fontSize: '0.63rem', color: 'var(--text-muted)', marginTop: 3 }}>
                    {t.radius_m ? `${(t.radius_m / 1852).toFixed(0)}nm ring` : 'ring unknown'} ·{' '}
                    {(t.confidence * 100).toFixed(0)}% conf · ±{(t.uncertainty_m / 1000).toFixed(1)}km ·{' '}
                    {t.source} · {t.age_s}s old
                  </div>
                </div>
              ))
            )}
          </Card>

          <Card title="Logistics" icon={Logistics} count={`${l.hubs.length} hubs`}>
            <div style={{ padding: '8px 13px' }}>
              {l.hubs.length === 0 ? (
                <div style={{ fontSize: '0.7rem', color: 'var(--text-dim)' }}>No logistics hub held.</div>
              ) : (
                l.hubs.map((h) => (
                  <div key={h.objective} style={{ fontSize: '0.7rem', marginBottom: 5 }}>
                    <span style={{ fontWeight: 700 }}>{h.objective}</span>
                    {h.threatened && <span style={{ color: '#f87171', marginLeft: 6, fontSize: '0.62rem', fontWeight: 700 }}>THREAT</span>}
                    <div style={{ ...mono, fontSize: '0.63rem', color: 'var(--text-dim)', marginTop: 2 }}>
                      supply <Bar value={h.supply} /> {h.supply}% · fuel <Bar value={h.fuel} /> {h.fuel}% · feeding {h.feeding}
                    </div>
                  </div>
                ))
              )}
            </div>
            <div style={{ padding: '8px 13px', borderTop: '1px solid var(--border)' }}>
              <div style={{ fontSize: '0.58rem', letterSpacing: '0.1em', textTransform: 'uppercase', color: 'var(--text-dim)', marginBottom: 4 }}>
                Cannot sustain themselves ({l.gaps.length})
              </div>
              {l.gaps.length === 0 ? (
                <div style={{ fontSize: '0.69rem', color: 'var(--text-muted)' }}>
                  Every objective can pay for its own repairs.
                </div>
              ) : (
                l.gaps.slice(0, 12).map((g) => (
                  <div key={g.objective} style={{ fontSize: '0.69rem', color: 'var(--text-muted)', marginBottom: 3 }}>
                    <strong style={{ color: 'var(--text)' }}>{g.objective}</strong> — {g.note}
                  </div>
                ))
              )}
            </div>
          </Card>

          <Card title="Comms Card" icon={Comms} count={report.comms.length}>
            <div style={{ overflowX: 'auto' }}>
              <table style={{ borderCollapse: 'collapse', width: '100%', minWidth: 320 }}>
                <thead>
                  <tr>
                    {['CH', 'FREQ', 'MOD', 'STATION'].map((h) => (
                      <th
                        key={h}
                        style={{
                          padding: '6px 10px', fontSize: '0.58rem', letterSpacing: '0.09em',
                          textTransform: 'uppercase', color: 'var(--text-dim)', textAlign: 'left',
                          borderBottom: '1px solid var(--border)',
                        }}
                      >
                        {h}
                      </th>
                    ))}
                  </tr>
                </thead>
                <tbody>
                  {report.comms.map((c, i) => (
                    <tr key={i}>
                      <td style={{ ...mono, padding: '5px 10px', fontSize: '0.68rem', color: 'var(--text-dim)', borderBottom: '1px solid var(--border)' }}>
                        {c.preset ?? '—'}
                      </td>
                      <td style={{ ...mono, padding: '5px 10px', fontSize: '0.7rem', color: '#facc15', borderBottom: '1px solid var(--border)', whiteSpace: 'nowrap' }}>
                        {c.freq_mhz.toFixed(3)}
                      </td>
                      <td style={{ ...mono, padding: '5px 10px', fontSize: '0.66rem', color: 'var(--text-dim)', borderBottom: '1px solid var(--border)' }}>
                        {c.modulation}
                      </td>
                      <td style={{ padding: '5px 10px', fontSize: '0.7rem', borderBottom: '1px solid var(--border)' }}>
                        {c.label}
                        {c.live && (
                          <span style={{ ...mono, color: '#8ec83f', fontSize: '0.58rem', fontWeight: 800, marginLeft: 6 }}>
                            UP
                          </span>
                        )}
                        {c.purpose && (
                          <div style={{ fontSize: '0.62rem', color: 'var(--text-dim)', marginTop: 1 }}>{c.purpose}</div>
                        )}
                        {c.note && (
                          <div style={{ fontSize: '0.62rem', color: 'var(--text-muted)', marginTop: 1 }}>{c.note}</div>
                        )}
                      </td>
                    </tr>
                  ))}
                </tbody>
              </table>
            </div>
            {report.flight_channels.length > 0 && (
              <div style={{ padding: '8px 13px', borderTop: '1px solid var(--border)' }}>
                <div style={{ fontSize: '0.58rem', letterSpacing: '0.1em', textTransform: 'uppercase', color: 'var(--text-dim)', marginBottom: 4 }}>
                  Intra-flight
                </div>
                <div style={{ ...mono, fontSize: '0.67rem', color: 'var(--text-muted)' }}>
                  {report.flight_channels.map(([n, f]) => `${n} ${f.toFixed(3)}`).join('   ')}
                </div>
              </div>
            )}
          </Card>

          <Card title="On Station" icon={Supply} count={report.support.length}>
            {report.support.length === 0 ? (
              <div style={dim}>No AWACS, tanker or JTAC up for your coalition.</div>
            ) : (
              report.support.map((s, i) => (
                <div key={i} style={{ padding: '6px 13px', borderBottom: '1px solid var(--border)', fontSize: '0.7rem' }}>
                  <span style={{ fontWeight: 700 }}>{s.label}</span>
                  <span style={{ ...mono, color: '#facc15', marginLeft: 8 }}>
                    {s.freq_mhz != null ? s.freq_mhz.toFixed(3) : '—'}
                  </span>
                  {s.tacan && <span style={{ ...mono, color: 'var(--text-dim)', marginLeft: 8 }}>{s.tacan}</span>}
                  {s.note && (
                    <div style={{ fontSize: '0.64rem', color: 'var(--text-muted)', marginTop: 1 }}>{s.note}</div>
                  )}
                </div>
              ))
            )}
          </Card>

          <Card title="Last Hour" icon={Clock} count={report.recent.length}>
            {report.recent.length === 0 ? (
              <div style={dim}>Nothing has changed hands or taken damage recently.</div>
            ) : (
              report.recent.slice(0, 15).map((e, i) => (
                <div
                  key={i}
                  style={{
                    padding: '5px 13px', borderBottom: '1px solid var(--border)', fontSize: '0.69rem',
                    color: e.good === true ? '#8ec83f' : e.good === false ? '#f87171' : 'var(--text-muted)',
                  }}
                >
                  <span style={{ ...mono, fontSize: '0.62rem', color: 'var(--text-dim)', marginRight: 7 }}>
                    {new Date(e.at).toISOString().slice(11, 16)}Z
                  </span>
                  {e.text}
                </div>
              ))
            )}
          </Card>

          <div style={{ fontSize: '0.6rem', color: 'var(--text-dim)', padding: '0 2px 4px', display: 'flex', gap: 5, alignItems: 'center' }}>
            <Tacmap size={10} />
            Built for {report.side} only — threat rings and the air picture are your coalition's own sensors.
          </div>
        </div>
      </div>
    </div>
  )
}
