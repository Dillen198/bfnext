// ──────────────────────────────────────────────────────────────────────────────
// ManualSection.tsx — Pilot Field Manual for Vector Strike
// Full-page campaign manual section for the bfsite public website.
// No admin commands. No server config. Pilot-facing only.
// ──────────────────────────────────────────────────────────────────────────────

import React, { useState } from 'react'
import { SERVERS, DEFAULT_SERVER_ID } from '../config/servers'

const SUBSECTION_HEADING: React.CSSProperties = {
  fontFamily: "'Bebas Neue', sans-serif",
  fontSize: 'clamp(1.4rem, 3vw, 1.9rem)',
  color: 'var(--text)',
  letterSpacing: '0.1em',
  margin: '0 0 0.25rem 0',
}

const BODY_TEXT: React.CSSProperties = {
  fontSize: '0.875rem',
  color: 'var(--text-muted)',
  lineHeight: 1.7,
  margin: 0,
}

const LABEL_STYLE: React.CSSProperties = {
  fontFamily: "'Bebas Neue', sans-serif",
  fontSize: '0.65rem',
  letterSpacing: '0.3em',
  textTransform: 'uppercase' as const,
  color: 'var(--accent)',
}

// ── Shared image placeholder ─────────────────────────────────────────────────

function ImgPlaceholder({ label, height = 180 }: { label: string; height?: number }) {
  return (
    <div
      style={{
        height,
        background: '#0d0d0d',
        border: '1px solid var(--border)',
        borderRadius: '2px',
        display: 'flex',
        alignItems: 'center',
        justifyContent: 'center',
        flexDirection: 'column' as const,
        gap: '0.5rem',
        userSelect: 'none',
      }}
    >
      {/* corner brackets */}
      <div style={{ position: 'relative', padding: '0.75rem 1.25rem' }}>
        <div style={{
          position: 'absolute', top: 0, left: 0,
          width: 12, height: 12,
          borderTop: '2px solid var(--accent)',
          borderLeft: '2px solid var(--accent)',
        }} />
        <div style={{
          position: 'absolute', bottom: 0, right: 0,
          width: 12, height: 12,
          borderBottom: '2px solid var(--accent)',
          borderRight: '2px solid var(--accent)',
        }} />
        <span
          style={{
            fontFamily: "'Bebas Neue', sans-serif",
            fontSize: '0.85rem',
            letterSpacing: '0.25em',
            color: 'var(--text-muted)',
          }}
        >
          {label}
        </span>
      </div>
    </div>
  )
}

// ── Command row (reusable) ────────────────────────────────────────────────────

interface CmdRow {
  cmd: string
  desc: string
}

function CommandTable({ rows }: { rows: CmdRow[] }) {
  return (
    <div
      style={{
        border: '1px solid var(--border)',
        borderRadius: '2px',
        overflow: 'hidden',
      }}
    >
      {/* Header */}
      <div
        style={{
          display: 'grid',
          gridTemplateColumns: '220px 1fr',
          background: '#0d0d0d',
          borderBottom: '1px solid var(--border)',
          padding: '0.55rem 1rem',
        }}
      >
        <span style={{ ...LABEL_STYLE }}>Command</span>
        <span style={{ ...LABEL_STYLE }}>Description</span>
      </div>

      {/* Rows */}
      {rows.map((row, i) => (
        <div
          key={i}
          style={{
            display: 'grid',
            gridTemplateColumns: '220px 1fr',
            padding: '0.65rem 1rem',
            background: i % 2 === 0 ? 'var(--bg-card)' : 'var(--bg-elevated)',
            borderBottom: i < rows.length - 1 ? '1px solid var(--border)' : 'none',
            alignItems: 'start',
          }}
        >
          <code
            style={{
              fontFamily: "'JetBrains Mono', 'Consolas', monospace",
              fontSize: '0.8rem',
              color: 'var(--accent)',
              paddingTop: '1px',
            }}
          >
            {row.cmd}
          </code>
          <span style={{ fontSize: '0.84rem', color: 'var(--text-muted)', lineHeight: 1.55 }}>
            {row.desc}
          </span>
        </div>
      ))}
    </div>
  )
}

// ── Subsection wrapper ────────────────────────────────────────────────────────

function Subsection({
  number,
  title,
  accent,
  children,
}: {
  number: string
  title: string
  accent?: string
  children: React.ReactNode
}) {
  return (
    <div style={{ marginBottom: '4rem' }}>
      {/* Number + title row */}
      <div style={{ display: 'flex', alignItems: 'baseline', gap: '1rem', marginBottom: '1.5rem' }}>
        <span
          style={{
            fontFamily: "'Bebas Neue', sans-serif",
            fontSize: '2.2rem',
            color: 'var(--accent)',
            lineHeight: 1,
            opacity: 0.6,
            flexShrink: 0,
          }}
        >
          {number}
        </span>
        <div>
          <h3 style={SUBSECTION_HEADING}>
            {title}{accent ? (
              <> <span style={{ color: 'var(--accent)' }}>{accent}</span></>
            ) : null}
          </h3>
          <div style={{ width: 40, height: 2, background: 'var(--accent)', marginTop: '0.4rem', opacity: 0.5 }} />
        </div>
      </div>
      {children}
    </div>
  )
}

// ── Callout box ───────────────────────────────────────────────────────────────

function Callout({ type = 'warn', children }: { type?: 'warn' | 'info'; children: React.ReactNode }) {
  const isWarn = type === 'warn'
  return (
    <div
      style={{
        background: isWarn ? 'rgba(77,124,15,0.06)' : 'rgba(59,130,246,0.06)',
        border: `1px solid ${isWarn ? 'rgba(77,124,15,0.25)' : 'rgba(59,130,246,0.25)'}`,
        borderLeft: `4px solid ${isWarn ? 'var(--accent)' : 'var(--blue)'}`,
        borderRadius: '2px',
        padding: '1rem 1.25rem',
        display: 'flex',
        alignItems: 'flex-start',
        gap: '0.75rem',
      }}
    >
      <span
        style={{
          fontFamily: "'Bebas Neue', sans-serif",
          fontSize: '0.65rem',
          letterSpacing: '0.25em',
          color: isWarn ? 'var(--accent)' : 'var(--blue)',
          paddingTop: '2px',
          flexShrink: 0,
        }}
      >
        {isWarn ? '⚠ CAUTION' : 'ℹ NOTE'}
      </span>
      <p style={{ ...BODY_TEXT, color: 'var(--text)' }}>{children}</p>
    </div>
  )
}

// ── Tip card ──────────────────────────────────────────────────────────────────

function TipCard({ number, text }: { number: number; text: string }) {
  return (
    <div
      style={{
        background: 'var(--bg-card)',
        border: '1px solid var(--border)',
        borderTop: '2px solid var(--accent)',
        borderRadius: '2px',
        padding: '1.25rem 1.25rem 1.25rem 1rem',
        display: 'flex',
        gap: '1rem',
        alignItems: 'flex-start',
      }}
    >
      <span
        style={{
          fontFamily: "'Bebas Neue', sans-serif",
          fontSize: '1.8rem',
          color: 'var(--accent)',
          lineHeight: 1,
          opacity: 0.5,
          flexShrink: 0,
          minWidth: '1.6rem',
          textAlign: 'right' as const,
        }}
      >
        {String(number).padStart(2, '0')}
      </span>
      <p style={{ ...BODY_TEXT, color: 'var(--text)', lineHeight: 1.6, marginTop: '2px' }}>{text}</p>
    </div>
  )
}

// ── Inline info row (for lists without a table) ────────────────────────────────

function InfoRow({ label, value }: { label: string; value: string }) {
  return (
    <div
      style={{
        display: 'flex',
        gap: '1rem',
        alignItems: 'baseline',
        padding: '0.5rem 0',
        borderBottom: '1px solid var(--border)',
      }}
    >
      <span
        style={{
          fontFamily: "'Bebas Neue', sans-serif",
          fontSize: '0.75rem',
          letterSpacing: '0.15em',
          color: 'var(--accent)',
          minWidth: 140,
          flexShrink: 0,
        }}
      >
        {label}
      </span>
      <span style={{ fontSize: '0.84rem', color: 'var(--text-muted)', lineHeight: 1.55 }}>{value}</span>
    </div>
  )
}

// ── Server selector (only interactive once more than one server exists) ────────

function ServerSelector({
  activeId,
  onChange,
}: {
  activeId: string
  onChange: (id: string) => void
}) {
  if (SERVERS.length <= 1) {
    return (
      <div style={{ display: 'flex', alignItems: 'center', gap: '0.6rem', marginTop: '1.5rem' }}>
        <span style={{ ...LABEL_STYLE, color: 'var(--text-dim)' }}>Server</span>
        <span style={{ fontSize: '0.8rem', color: 'var(--text-muted)' }}>{SERVERS[0].label}</span>
      </div>
    )
  }
  return (
    <div style={{ display: 'flex', alignItems: 'center', gap: '0.75rem', marginTop: '1.5rem', flexWrap: 'wrap' as const }}>
      <span style={{ ...LABEL_STYLE, color: 'var(--text-dim)' }}>Server</span>
      <div style={{ display: 'flex', gap: '0.5rem', flexWrap: 'wrap' as const }}>
        {SERVERS.map((s) => {
          const active = s.id === activeId
          return (
            <button
              key={s.id}
              onClick={() => onChange(s.id)}
              style={{
                fontFamily: "'Bebas Neue', sans-serif",
                fontSize: '0.75rem',
                letterSpacing: '0.1em',
                color: active ? 'var(--bg)' : 'var(--text-muted)',
                background: active ? 'var(--accent)' : 'transparent',
                border: `1px solid ${active ? 'var(--accent)' : 'var(--border)'}`,
                borderRadius: '2px',
                padding: '0.3rem 0.8rem',
                cursor: 'pointer',
              }}
            >
              {s.label}
            </button>
          )
        })}
      </div>
    </div>
  )
}

// ──────────────────────────────────────────────────────────────────────────────
// Main Component
// ──────────────────────────────────────────────────────────────────────────────

export default function ManualSection() {
  const [serverId, setServerId] = useState(DEFAULT_SERVER_ID)
  const server = SERVERS.find((s) => s.id === serverId) ?? SERVERS[0]
  return (
    <section
      id="manual"
      style={{
        background: 'var(--bg)',
        padding: '7rem 0 6rem',
        borderTop: '1px solid var(--border)',
        position: 'relative',
        overflow: 'hidden',
      }}
    >
      {/* ── Subtle grid overlay ─────────────────────────────────────────────── */}
      <div
        aria-hidden="true"
        style={{
          position: 'absolute',
          inset: 0,
          backgroundImage:
            'linear-gradient(var(--border) 1px, transparent 1px), linear-gradient(90deg, var(--border) 1px, transparent 1px)',
          backgroundSize: '60px 60px',
          opacity: 0.18,
          pointerEvents: 'none',
        }}
      />

      <div className="max-w-7xl mx-auto px-6" style={{ position: 'relative', zIndex: 1 }}>

        {/* ── Section Header ─────────────────────────────────────────────────── */}
        <div style={{ marginBottom: '5rem' }}>
          <div className="vs-section-label" style={{ marginBottom: '1rem' }}>
            Vector Strike · Campaign Doctrine
          </div>

          <h2
            style={{
              fontFamily: "'Bebas Neue', sans-serif",
              fontSize: 'clamp(3rem, 8vw, 5.5rem)',
              color: 'var(--text)',
              letterSpacing: '0.06em',
              lineHeight: 0.95,
              margin: '0 0 1.5rem 0',
            }}
          >
            PILOT{' '}
            <span style={{ color: 'var(--accent)' }}>FIELD</span>
            <br />
            MANUAL
          </h2>

          {/* Decorative line + subtitle */}
          <div style={{ display: 'flex', alignItems: 'center', gap: '1.5rem', maxWidth: 600 }}>
            <div
              style={{
                flex: 1,
                height: 1,
                background: 'linear-gradient(to right, var(--accent), transparent)',
                opacity: 0.6,
              }}
            />
          </div>
          <p
            style={{
              fontSize: '1rem',
              color: 'var(--text-muted)',
              marginTop: '1.25rem',
              maxWidth: 520,
              lineHeight: 1.65,
            }}
          >
            Everything you need to know to fly, fight, and win. Read this before you strap in.
          </p>

          {/* Doc tag strip */}
          <div style={{ display: 'flex', gap: '1rem', marginTop: '1.5rem', flexWrap: 'wrap' as const }}>
            {['UNCLASSIFIED', 'BLUFOR / REDFOR', 'ALL AIRFRAMES', 'REV. 3.0'].map((tag) => (
              <span
                key={tag}
                style={{
                  fontFamily: "'Bebas Neue', sans-serif",
                  fontSize: '0.65rem',
                  letterSpacing: '0.2em',
                  color: 'var(--text-dim)',
                  border: '1px solid var(--border)',
                  padding: '0.2rem 0.6rem',
                  borderRadius: '2px',
                }}
              >
                {tag}
              </span>
            ))}
          </div>

          <ServerSelector activeId={serverId} onChange={setServerId} />
        </div>

        {/* ── Divider ──────────────────────────────────────────────────────── */}
        <div className="vs-divider" style={{ marginBottom: '4rem' }} />

        {/* ══════════════════════════════════════════════════════════════════ */}
        {/* 01 — GETTING STARTED                                              */}
        {/* ══════════════════════════════════════════════════════════════════ */}
        <Subsection number="01" title="GETTING" accent="STARTED">
          <div className="grid grid-cols-1 lg:grid-cols-2 gap-8 items-start">
            <div>
              <p style={{ ...BODY_TEXT, marginBottom: '1.5rem' }}>
                Vector Strike runs 24/7 on a dedicated server. No mods required beyond what DCS ships
                with — just connect, pick a slot, and fly.
              </p>

              <div style={{ marginBottom: '1.25rem' }}>
                <InfoRow
                  label="Server Name"
                  value="The Coop · Operation Vector Strike"
                />
                <InfoRow
                  label="Find It"
                  value='DCS Multiplayer → search "Vector Strike" in the server browser'
                />
                <InfoRow
                  label="Password"
                  value="Available in Discord → #server-info channel"
                />
                <InfoRow
                  label="Slot Selection"
                  value="Pick your aircraft, then choose BLUFOR (Blue) or REDFOR (Red)"
                />
                <InfoRow
                  label="Discord"
                  value="discord.gg/wAsBEfse — required for briefings, coordination, and the password"
                />
              </div>

              <Callout type="info">
                Join Discord before your first flight. Mission briefings, ATIS, and real-time
                coordination all happen there. New pilots are always welcome.
              </Callout>
            </div>

            <ImgPlaceholder label="SERVER BROWSER" height={220} />
          </div>
        </Subsection>

        {/* ══════════════════════════════════════════════════════════════════ */}
        {/* 02 — F10 RADIO MENU                                               */}
        {/* ══════════════════════════════════════════════════════════════════ */}
        <Subsection number="02" title="THE F10" accent="RADIO MENU">
          <p style={{ ...BODY_TEXT, marginBottom: '1.5rem', maxWidth: 680 }}>
            The <strong style={{ color: 'var(--text)' }}>F10 Radio Menu</strong> is your primary
            interface with the campaign system. Press <kbd style={{
              fontFamily: "'JetBrains Mono', monospace",
              fontSize: '0.78rem',
              background: 'var(--bg-elevated)',
              border: '1px solid var(--border)',
              borderRadius: '2px',
              padding: '0.1rem 0.4rem',
              color: 'var(--text)',
            }}>F10</kbd> in-game, select "Radio Menu", then choose your coalition. Most campaign
            actions — deploying units, calling JTAC, checking supply — are accessed from here.
          </p>

          <CommandTable
            rows={[
              {
                cmd: 'Atis',
                desc: 'Shows weather at nearby airfields: QNH (barometric pressure), temperature, wind direction and speed.',
              },
              {
                cmd: 'Register',
                desc: 'Register your pilot profile with the campaign. Do this on first join — required before the system tracks your stats.',
              },
              {
                cmd: 'Menu → Action',
                desc: 'Deploy troops or vehicles, call in strikes, and manage active deployments at objectives.',
              },
              {
                cmd: 'Menu → Cargo',
                desc: 'Request or configure cargo transport runs between logistics hubs. Essential for resupply.',
              },
              {
                cmd: 'Menu → Troop',
                desc: 'Move infantry squads to nearby objectives. Troops can capture and hold ground.',
              },
              {
                cmd: 'Menu → JTAC',
                desc: 'Request Joint Terminal Attack Controller (JTAC) targeting support. JTAC will designate high-value targets with a laser for CAS runs.',
              },
              {
                cmd: 'Menu → EWR',
                desc: 'Pull an Early Warning Radar report: known enemy contacts, threat picture, and airspace status.',
              },
              {
                cmd: 'Menu → Action → Artillery',
                desc: 'Call a fire-for-effect artillery strike on a target area. Available when friendly artillery groups are in range. Costs supply.',
              },
              {
                cmd: 'Status',
                desc: 'Check your pilot profile: lives remaining, campaign score, and current sortie info.',
              },
            ]}
          />

          <p style={{ ...BODY_TEXT, marginTop: '1rem', fontSize: '0.8rem' }}>
            Menu options may vary by aircraft slot and current objective status. Options grayed out or absent
            indicate insufficient supply or no valid targets in range.
          </p>
        </Subsection>

        {/* ══════════════════════════════════════════════════════════════════ */}
        {/* 03 — CHAT COMMANDS                                                */}
        {/* ══════════════════════════════════════════════════════════════════ */}
        <Subsection number="03" title="CHAT" accent="COMMANDS">
          <p style={{ ...BODY_TEXT, marginBottom: '1.5rem', maxWidth: 640 }}>
            All in-game chat commands use the <code style={{
              fontFamily: "'JetBrains Mono', monospace",
              color: 'var(--accent)',
              fontSize: '0.9rem',
            }}>-</code> prefix. Type them directly in the DCS in-game chat window. Results appear in your chat.
          </p>

          <CommandTable
            rows={[
              { cmd: 'blue / red', desc: 'Register and join the blue or red team. Required before you can take a combat slot.' },
              { cmd: '-switch <color>', desc: 'Switch sides to blue or red. You must be in spectators. Limited uses per round.' },
              { cmd: '-lives', desc: 'Shows how many lives you have remaining in the current campaign round.' },
              { cmd: '-time', desc: 'Shows how long until the next scheduled server restart.' },
              { cmd: '-balance', desc: 'Shows your current points balance.' },
              { cmd: '-status', desc: 'Full campaign status: your side, points, kill streak, objective counts, and active convoy count.' },
              { cmd: '-transfer <amount> <player>', desc: 'Transfer points to another player by name.' },
              { cmd: '-transfer <amount> objective:<name>', desc: 'Donate points directly to an objective to fund its logistics.' },
              { cmd: '-delete <groupid>', desc: 'Delete a unit group you deployed and receive a partial points refund.' },
              { cmd: '-action <name> <args>', desc: 'Execute a commander action by name (e.g. spawn AI units, call strikes). Use -action help for a full list.' },
              { cmd: '-jtac <id> status', desc: 'Request a 9-line from the specified JTAC.' },
              { cmd: '-jtac <id> shift', desc: 'Manually shift the JTAC to its next target.' },
              { cmd: '-jtac <id> autoshift', desc: 'Toggle automatic target shifting on the JTAC.' },
              { cmd: '-jtac <id> smoke', desc: 'Request the JTAC to smoke the current target.' },
              { cmd: '-jtac <id> pointer', desc: 'Toggle the IR pointer on the JTAC target.' },
              { cmd: '-jtac <id> code <code>', desc: 'Set the laser code for the specified JTAC.' },
              { cmd: '-jtac <id> arty <id|all> <n>', desc: 'Direct nearby artillery to fire N rounds at the JTAC target.' },
              { cmd: '-jtac <id> bomber [mission]', desc: 'Call a bomber strike on the current JTAC target.' },
              { cmd: '-bind <token>', desc: 'Link your DCS pilot to your web dashboard account using the token from the Pilots page.' },
              { cmd: '-help', desc: 'Display the full command list in chat.' },
            ]}
          />
        </Subsection>

        {/* ══════════════════════════════════════════════════════════════════ */}
        {/* 04 — TERRITORY & OBJECTIVES                                       */}
        {/* ══════════════════════════════════════════════════════════════════ */}
        <Subsection number="04" title="TERRITORY &amp;" accent="OBJECTIVES">
          <div className="grid grid-cols-1 lg:grid-cols-2 gap-8 items-start">
            <div>
              <p style={{ ...BODY_TEXT, marginBottom: '1.5rem' }}>
                The campaign map is divided into objectives. Capture and hold them to push the
                front line and deprive the enemy of resources. Objectives are not just flags — each
                type provides a specific strategic function.
              </p>

              <div style={{ marginBottom: '1.5rem' }}>
                {[
                  { label: 'Airbase', value: 'Provides coalition aircraft slots, respawn points, and full repair/rearm capability.' },
                  { label: 'FARP', value: 'Forward Arming & Refueling Point. Extends range for helicopters and fast movers. Needs supply to function.' },
                  { label: 'FOB', value: 'Forward Operating Base. Spawns ground units and holds territory between larger objectives.' },
                  { label: 'Logistics Hub', value: 'Supplies resources to nearby objectives via automated convoys. Critical chokepoint.' },
                  { label: 'Factory', value: 'Produces ground units over time. Destroy enemy factories to starve their offensive.' },
                  { label: 'Naval Base', value: 'Spawns and resupplies naval assets. Controls coastal and maritime operations.' },
                  { label: 'SAM Site', value: 'High-value integrated air defence site. Position is classified — not shown on the F10 map or the dashboard. Must be located and destroyed by players. Can be recaptured by either side.' },
                ].map((item) => (
                  <InfoRow key={item.label} label={item.label} value={item.value} />
                ))}
              </div>

              <Callout type="info">
                An objective becomes capturable once its health is at 20% or below and every
                defending infantry unit is eliminated — logistics and structures alone won't do
                it. Move capture-capable troops into the zone once it's capturable to begin the
                takeover. SAM Sites work differently — see{' '}
                <strong style={{ color: 'var(--text)' }}>VICTORY &amp; CAPTURE</strong> below for
                the full breakdown.
              </Callout>

              <div style={{ marginTop: '1.5rem' }}>
                <p style={{ ...LABEL_STYLE, marginBottom: '0.5rem' }}>How To Capture — Step By Step</p>
                <InfoRow label="1. Soften" value="Reduce the objective's health to 20% or below and kill every defending infantry unit — CAS, artillery, or ground troops all count." />
                <InfoRow label="2. Deploy Troops" value="F10 → Menu → Troop → move a capture-capable squad into the objective's zone." />
                <InfoRow label="3. Hold It" value="Keep enemy troops out of the zone until the capture timer runs out. Track progress in the panel messages or with -status." />
              </div>
            </div>

            <ImgPlaceholder label="OBJECTIVE TYPES" height={260} />
          </div>
        </Subsection>

        {/* ══════════════════════════════════════════════════════════════════ */}
        {/* 05 — LIVES SYSTEM                                                 */}
        {/* ══════════════════════════════════════════════════════════════════ */}
        <Subsection number="05" title="THE LIVES" accent="SYSTEM">
          <p style={{ ...BODY_TEXT, marginBottom: '1.5rem', maxWidth: 680 }}>
            Vector Strike ties losses to consequences, but not with a single shared pool. Lives are
            tracked separately per aircraft role — Standard, Intercept, Attack, Recon, and
            Logistics (see <strong style={{ color: 'var(--text)' }}>AIRCRAFT ROSTER</strong> below
            for the exact counts) — so running dry in a Standard-role fighter doesn't ground you
            from flying Recon or Logistics.
          </p>

          <div className="grid grid-cols-1 sm:grid-cols-2 gap-4 mb-6">
            {[
              { heading: 'SHOT DOWN', body: 'Being destroyed by enemy fire costs one life of that role. Eject if you can — a downed pilot is still recoverable.' },
              { heading: 'CRASH / ACCIDENT', body: 'Controlled flight into terrain, mid-air collision, or running out of fuel all count as deaths.' },
              { heading: 'EJECTING', body: 'Ejecting spawns a downed pilot unit at your location. A friendly helicopter can rescue you via the CSAR menu and restore your life.' },
              { heading: 'LIFE REFILL', body: 'Each role\'s life pool refills on its own rolling timer rather than at round start — check -lives in-game to see your current count and role.' },
            ].map((card) => (
              <div
                key={card.heading}
                style={{
                  background: 'var(--bg-card)',
                  border: '1px solid var(--border)',
                  borderLeft: '3px solid var(--accent)',
                  borderRadius: '2px',
                  padding: '1rem 1.25rem',
                }}
              >
                <h4
                  style={{
                    fontFamily: "'Bebas Neue', sans-serif",
                    fontSize: '0.9rem',
                    letterSpacing: '0.15em',
                    color: 'var(--text)',
                    margin: '0 0 0.4rem 0',
                  }}
                >
                  {card.heading}
                </h4>
                <p style={{ ...BODY_TEXT }}>{card.body}</p>
              </div>
            ))}
          </div>

          <Callout type="info">
            Life limits are a campaign setting and can be switched off entirely — if lives aren't
            being taken on death, that's the server running with unlimited lives, not a bug.
            Check <strong style={{ color: 'var(--text)' }}>-status</strong> or{' '}
            <strong style={{ color: 'var(--text)' }}>-lives</strong> in-game to see what's active.
          </Callout>
          <div style={{ height: '1.5rem' }} />

          {/* CSAR */}
          <div
            style={{
              background: 'rgba(59,130,246,0.05)',
              border: '1px solid rgba(59,130,246,0.2)',
              borderLeft: '4px solid var(--blue)',
              borderRadius: '2px',
              padding: '1.25rem 1.5rem',
              marginBottom: '1.5rem',
            }}
          >
            <h4
              style={{
                fontFamily: "'Bebas Neue', sans-serif",
                fontSize: '1rem',
                letterSpacing: '0.18em',
                color: 'var(--blue)',
                margin: '0 0 0.75rem 0',
              }}
            >
              CSAR — COMBAT SEARCH & RESCUE
            </h4>
            <p style={{ ...BODY_TEXT, marginBottom: '1rem' }}>
              When a pilot ejects, a downed pilot unit spawns at their crash site. A friendly
              helicopter crew can locate and extract them, restoring the lost life. The rescuing
              pilot also earns bonus campaign points for the recovery. If you're already at max
              lives for the rescued role, the credit rolls down to the next role instead of being
              wasted.
            </p>
            <div className="grid grid-cols-1 sm:grid-cols-3 gap-3">
              {[
                { step: '01', title: 'EJECT', body: 'Pilot ejects — a downed pilot unit spawns at the crash location on the map.' },
                { step: '02', title: 'LOCATE & SMOKE', body: 'A helicopter uses the F10 → CSAR menu to request green smoke on the nearest downed pilot.' },
                { step: '03', title: 'PICK UP & DELIVER', body: 'Helicopter lands nearby, picks up the pilot, and delivers them to a friendly base or FARP to restore the life.' },
              ].map(s => (
                <div key={s.step} style={{ display: 'flex', gap: '0.75rem', alignItems: 'flex-start' }}>
                  <span
                    style={{
                      fontFamily: "'Bebas Neue', sans-serif",
                      fontSize: '1.4rem',
                      color: 'var(--blue)',
                      opacity: 0.5,
                      lineHeight: 1,
                      flexShrink: 0,
                    }}
                  >
                    {s.step}
                  </span>
                  <div>
                    <div
                      style={{
                        fontFamily: "'Bebas Neue', sans-serif",
                        fontSize: '0.75rem',
                        letterSpacing: '0.15em',
                        color: 'var(--text)',
                        marginBottom: '0.25rem',
                      }}
                    >
                      {s.title}
                    </div>
                    <p style={{ ...BODY_TEXT, fontSize: '0.8rem' }}>{s.body}</p>
                  </div>
                </div>
              ))}
            </div>
          </div>

          <Callout type="warn">
            Blue-on-blue / fratricide results in an immediate life penalty. Know your ROE.
            Check the F10 map and confirm targets before firing.
          </Callout>
        </Subsection>

        {/* ══════════════════════════════════════════════════════════════════ */}
        {/* 06 — LOGISTICS & SUPPLY                                           */}
        {/* ══════════════════════════════════════════════════════════════════ */}
        <Subsection number="06" title="LOGISTICS &amp;" accent="SUPPLY">
          <p style={{ ...BODY_TEXT, marginBottom: '1.5rem', maxWidth: 680 }}>
            The supply chain is the backbone of any sustained offensive. Objectives need a steady
            flow of resources to remain combat-effective. Cutting the enemy's supply is often more
            decisive than destroying their aircraft.
          </p>

          <div className="grid grid-cols-1 sm:grid-cols-2 lg:grid-cols-3 gap-4 mb-6">
            {[
              {
                title: 'CONVOYS',
                body: 'Automated ground convoys run between logistics hubs and forward objectives. No player action needed — but they can be interdicted.',
              },
              {
                title: 'INTERDICT ENEMY SUPPLY',
                body: 'Destroying enemy convoys degrades their objective supply over time. Sustained interdiction can neutralize entire sectors.',
              },
              {
                title: 'STRATEGIC INFRASTRUCTURE',
                body: 'Real map buildings near objectives — warehouses, fuel depots, industrial complexes — are marked with orange boxes on the F10 map. Destroy them and that objective\'s logistics rating drops permanently, on top of anything convoy interdiction achieves.',
              },
              {
                title: 'FARP RESUPPLY',
                body: 'FARPs require fuel and ammunition supply to rearm and refuel aircraft. An unsupplied FARP is useless — protect your hubs.',
              },
              {
                title: 'SUPPLY SCORE',
                body: 'Destroying enemy logistics convoys earns campaign score. Every truck counts.',
              },
              {
                title: 'FACTORIES',
                body: 'Enemy factories that are active will replenish ground forces. Strike them early to limit spawns.',
              },
              {
                title: 'CARGO RUNS',
                body: 'Player-piloted cargo missions (via F10 → Cargo) can accelerate resupply when automated convoys are destroyed.',
              },
            ].map((item) => (
              <div
                key={item.title}
                className="feature-card"
              >
                <h4
                  style={{
                    fontFamily: "'Bebas Neue', sans-serif",
                    fontSize: '0.95rem',
                    letterSpacing: '0.15em',
                    color: 'var(--text)',
                    margin: '0 0 0.5rem 0',
                  }}
                >
                  {item.title}
                </h4>
                <p style={{ ...BODY_TEXT }}>{item.body}</p>
              </div>
            ))}
          </div>
        </Subsection>

        {/* ══════════════════════════════════════════════════════════════════ */}
        {/* 07 — JTAC & CAS                                                   */}
        {/* ══════════════════════════════════════════════════════════════════ */}
        <Subsection number="07" title="JTAC &amp;" accent="CAS">
          <div className="grid grid-cols-1 lg:grid-cols-2 gap-8 items-start">
            <div>
              <p style={{ ...BODY_TEXT, marginBottom: '1.5rem' }}>
                Close Air Support (CAS) is the most effective way to break an enemy ground advance.
                JTAC (Joint Terminal Attack Controller) assets designate and track targets for
                your bomb runs. Always coordinate before you roll in.
              </p>

              <div style={{ marginBottom: '1.5rem' }}>
                {[
                  { label: 'Request JTAC', value: 'F10 Radio Menu → Menu → JTAC. Select the nearest JTAC asset to your target area.' },
                  { label: 'Laser Designation', value: 'JTAC will lase the target. Confirm laser code on Discord before weapons release.' },
                  { label: '9-Line Brief', value: 'JTAC passes a 9-line CAS brief on the tactical frequency. Copy it before your attack run.' },
                  { label: 'Deconfliction', value: 'Confirm ground force positions before running in. Do NOT roll hot without JTAC clearance.' },
                  { label: 'RTB after attack', value: 'Report BDA (Battle Damage Assessment) to JTAC after each pass.' },
                ].map((item) => (
                  <InfoRow key={item.label} label={item.label} value={item.value} />
                ))}
              </div>

              <Callout type="warn">
                Never run a CAS attack without positive JTAC clearance. Friendlies may be
                inside the target area. Fratricide penalties apply.
              </Callout>
            </div>

            <div>
              <ImgPlaceholder label="9-LINE BRIEF" height={220} />
              <div
                style={{
                  marginTop: '1rem',
                  background: 'var(--bg-card)',
                  border: '1px solid var(--border)',
                  borderRadius: '2px',
                  padding: '1rem 1.25rem',
                }}
              >
                <p style={{ ...LABEL_STYLE, marginBottom: '0.5rem' }}>9-Line Fields</p>
                {[
                  '1 — IP to Target',
                  '2 — Heading from IP to Target',
                  '3 — Distance from IP to Target',
                  '4 — Target Elevation',
                  '5 — Target Description',
                  '6 — Target Location (MGRS/LL)',
                  '7 — Mark Type (laser / smoke / IR)',
                  '8 — Friendlies',
                  '9 — Egress Direction',
                ].map((line) => (
                  <p
                    key={line}
                    style={{
                      fontFamily: "'JetBrains Mono', monospace",
                      fontSize: '0.72rem',
                      color: 'var(--text-muted)',
                      margin: '0.2rem 0',
                      lineHeight: 1.4,
                    }}
                  >
                    {line}
                  </p>
                ))}
              </div>
            </div>
          </div>
        </Subsection>

        {/* ══════════════════════════════════════════════════════════════════ */}
        {/* 08 — C-130 HERCULES                                               */}
        {/* ══════════════════════════════════════════════════════════════════ */}
        <Subsection number="08" title="C-130" accent="HERCULES">
          <div style={{ marginBottom: '1.5rem' }}>
            <div
              style={{
                display: 'inline-flex',
                alignItems: 'center',
                gap: '0.5rem',
                background: 'rgba(77,124,15,0.08)',
                border: '1px solid rgba(77,124,15,0.3)',
                borderRadius: '2px',
                padding: '0.3rem 0.8rem',
                marginBottom: '1.25rem',
              }}
            >
              <span style={{ fontFamily: "'Bebas Neue', sans-serif", fontSize: '0.65rem', letterSpacing: '0.22em', color: 'var(--accent)' }}>
                ✦ SPECIAL ROLE — STRATEGIC AIRLIFTER
              </span>
            </div>
            <p style={{ ...BODY_TEXT, maxWidth: 700, marginBottom: '1rem' }}>
              The C-130 Hercules is the backbone of coalition logistics. Unlike every other aircraft
              in the campaign, the Hercules can physically carry and deliver cargo, troops, and
              vehicles to forward positions — including unprepared landing zones deep inside enemy
              territory. Flying one is a force-multiplier mission that wins campaigns.
            </p>
            <p style={{ ...BODY_TEXT, maxWidth: 700 }}>
              The C-130 slot features a custom airdrop and auto-unpack system built specifically for
              Vector Strike. The moment cargo touches the ground (via parachute or LAPES run), the
              engine automatically registers the delivery, unpacks the asset, and credits the
              supplying pilot — no manual input required.
            </p>
          </div>

          {/* ── Airdrop System ─────────────────────────────────────────── */}
          <div style={{ marginBottom: '2.5rem' }}>
            <h4
              style={{
                fontFamily: "'Bebas Neue', sans-serif",
                fontSize: '1.1rem',
                letterSpacing: '0.15em',
                color: 'var(--text)',
                margin: '0 0 1rem 0',
                borderLeft: '3px solid var(--accent)',
                paddingLeft: '0.75rem',
              }}
            >
              AIRDROP SYSTEM
            </h4>
            <div className="grid grid-cols-1 lg:grid-cols-2 gap-8 items-start">
              <div>
                <p style={{ ...BODY_TEXT, marginBottom: '1.25rem' }}>
                  Airdrop missions deliver supply crates, vehicles, and ammunition pallets to
                  objectives that are cut off from ground convoy routes. Standard parachute drops
                  work from any altitude above the minimum safe threshold. LAPES (Low Altitude
                  Parachute Extraction System) runs must be performed below 15 ft AGL at low speed.
                </p>
                <div style={{ marginBottom: '1.25rem' }}>
                  <InfoRow label="Min Drop Altitude" value="600 ft AGL for standard chute deployment — below this the chute will not open" />
                  <InfoRow label="LAPES Run" value="Below 15 ft AGL, 120 kts or less — cargo extracts horizontally via drogue chute" />
                  <InfoRow label="Drop Zone" value="Must be within the target objective's capture radius — check the F10 map before committing" />
                  <InfoRow label="Delivery Credit" value="Credit is awarded when the cargo contacts the ground inside the DZ — automatic, no chat command needed" />
                  <InfoRow label="Crate Types" value="Supply crates, ammo pallets, vehicle kits, and FOB construction packs (varies by mission)" />
                </div>
                <Callout type="info">
                  You can check available cargo load options via <strong style={{ color: 'var(--text)' }}>F10 → Menu → Cargo</strong> before takeoff.
                  Load type determines what gets spawned on the ground after auto-unpack.
                </Callout>
              </div>
              <div className="space-y-4">
                <ImgPlaceholder label="AIRDROP — PARACHUTE RUN" height={180} />
                <ImgPlaceholder label="LAPES — LOW ALTITUDE EXTRACTION" height={150} />
              </div>
            </div>
          </div>

          {/* ── Auto-Unpack System ─────────────────────────────────────── */}
          <div style={{ marginBottom: '2.5rem' }}>
            <h4
              style={{
                fontFamily: "'Bebas Neue', sans-serif",
                fontSize: '1.1rem',
                letterSpacing: '0.15em',
                color: 'var(--text)',
                margin: '0 0 1rem 0',
                borderLeft: '3px solid var(--accent)',
                paddingLeft: '0.75rem',
              }}
            >
              AUTO-UNPACK — HOW IT WORKS
            </h4>
            <p style={{ ...BODY_TEXT, marginBottom: '1.25rem', maxWidth: 700 }}>
              Unlike manual cargo systems that require a player on the ground to trigger unpacking,
              Vector Strike's auto-unpack engine handles everything the moment delivery is confirmed.
              This is what happens behind the scenes:
            </p>
            <div
              style={{
                background: 'var(--bg-card)',
                border: '1px solid var(--border)',
                borderRadius: '2px',
                overflow: 'hidden',
                marginBottom: '1.25rem',
              }}
            >
              {[
                {
                  step: '01',
                  title: 'IMPACT DETECTION',
                  body: 'The server detects the cargo crate landing within the target objective\'s radius. Position, velocity, and zone membership are all validated simultaneously.',
                },
                {
                  step: '02',
                  title: 'CRATE CLASSIFICATION',
                  body: 'The system identifies crate type (supply, ammo, vehicle kit, construction pack) from the load manifest set at departure. Mixed loads are handled — each crate type is processed independently.',
                },
                {
                  step: '03',
                  title: 'ASSET SPAWN',
                  body: 'The correct asset is spawned at or near the landing point. Vehicles are placed on suitable terrain. Construction packs trigger FOB or FARP build sequences automatically.',
                },
                {
                  step: '04',
                  title: 'SUPPLY CREDIT',
                  body: 'The objective\'s supply level is updated in the campaign database. The delivering pilot receives score credit proportional to the cargo value and distance flown.',
                },
                {
                  step: '05',
                  title: 'PILOT NOTIFICATION',
                  body: 'An in-game message confirms delivery: crate type, objective credited, and supply delta. No manual confirmation needed.',
                },
              ].map((item, i, arr) => (
                <div
                  key={item.step}
                  style={{
                    display: 'grid',
                    gridTemplateColumns: '56px 1fr',
                    padding: '1rem 1.25rem',
                    background: i % 2 === 0 ? 'var(--bg-card)' : 'var(--bg-elevated)',
                    borderBottom: i < arr.length - 1 ? '1px solid var(--border)' : 'none',
                    alignItems: 'start',
                    gap: '1rem',
                  }}
                >
                  <span
                    style={{
                      fontFamily: "'Bebas Neue', sans-serif",
                      fontSize: '1.6rem',
                      color: 'var(--accent)',
                      opacity: 0.5,
                      lineHeight: 1,
                    }}
                  >
                    {item.step}
                  </span>
                  <div>
                    <h5
                      style={{
                        fontFamily: "'Bebas Neue', sans-serif",
                        fontSize: '0.8rem',
                        letterSpacing: '0.18em',
                        color: 'var(--text)',
                        margin: '0 0 0.3rem 0',
                      }}
                    >
                      {item.title}
                    </h5>
                    <p style={{ ...BODY_TEXT, fontSize: '0.82rem' }}>{item.body}</p>
                  </div>
                </div>
              ))}
            </div>
            <Callout type="info">
              Multiple C-130s can deliver to the same objective simultaneously. Deliveries stack —
              each crate is processed individually. There is no race condition or delivery conflict.
            </Callout>
          </div>

          {/* ── C-130 F10 Menu ─────────────────────────────────────────── */}
          <div style={{ marginBottom: '2rem' }}>
            <h4
              style={{
                fontFamily: "'Bebas Neue', sans-serif",
                fontSize: '1.1rem',
                letterSpacing: '0.15em',
                color: 'var(--text)',
                margin: '0 0 1rem 0',
                borderLeft: '3px solid var(--accent)',
                paddingLeft: '0.75rem',
              }}
            >
              C-130 F10 MENU OPTIONS
            </h4>
            <p style={{ ...BODY_TEXT, marginBottom: '1rem', maxWidth: 640 }}>
              The C-130 slot has an expanded F10 Radio Menu with dedicated cargo and troop transport commands not available to other aircraft.
            </p>
            <CommandTable
              rows={[
                {
                  cmd: 'Cargo → Load',
                  desc: 'Select cargo type for your current flight: supply crates, ammo pallets, vehicle kits, or FOB construction packs. Must be done at a friendly airbase or FARP before departure.',
                },
                {
                  cmd: 'Cargo → Status',
                  desc: 'Check what is currently loaded in your aircraft and its total cargo weight.',
                },
                {
                  cmd: 'Cargo → Drop Now',
                  desc: 'Manually trigger a cargo drop at your current position (if within a valid drop zone). Normally you can simply open the ramp and drop at the correct location.',
                },
                {
                  cmd: 'Troop → Embark',
                  desc: 'Board a squad of troops at a friendly FOB or objective. Must be on the ground inside the objective radius.',
                },
                {
                  cmd: 'Troop → Disembark',
                  desc: 'Deploy embarked troops at your current location. Troops auto-spawn and begin holding or advancing on the nearest objective.',
                },
                {
                  cmd: 'Troop → Status',
                  desc: 'Shows how many troops are currently aboard and their unit type.',
                },
              ]}
            />
          </div>

          {/* ── C-130 Tips ─────────────────────────────────────────────── */}
          <div className="grid grid-cols-1 sm:grid-cols-3 gap-4">
            {[
              {
                title: 'PLAN YOUR ROUTE',
                body: 'Check the EWR before departure. C-130s are large, slow, and cannot defend themselves against fighters or SAMs. Fly low, use terrain masking, and request escort when operating near contested airspace.',
              },
              {
                title: 'COORDINATE DZ TIMING',
                body: 'Announce your drop run in Discord before committing. Other pilots may be operating in the same area. A C-130 on final approach is extremely vulnerable — you want friendlies between you and any threat.',
              },
              {
                title: 'HIGH-VALUE LOADS FIRST',
                body: 'FOB construction packs and vehicle kits have the highest campaign impact. Supply crates are faster to load but worth less. Prioritise construction if your coalition is building forward positions.',
              },
            ].map((card) => (
              <div
                key={card.title}
                style={{
                  background: 'var(--bg-card)',
                  border: '1px solid var(--border)',
                  borderTop: '2px solid var(--accent)',
                  borderRadius: '2px',
                  padding: '1.25rem',
                }}
              >
                <h4
                  style={{
                    fontFamily: "'Bebas Neue', sans-serif",
                    fontSize: '0.9rem',
                    letterSpacing: '0.14em',
                    color: 'var(--text)',
                    margin: '0 0 0.5rem 0',
                  }}
                >
                  {card.title}
                </h4>
                <p style={{ ...BODY_TEXT }}>{card.body}</p>
              </div>
            ))}
          </div>
        </Subsection>

        {/* ══════════════════════════════════════════════════════════════════ */}
        {/* 09 — TIPS & BEST PRACTICES                                        */}
        {/* ══════════════════════════════════════════════════════════════════ */}
        <Subsection number="09" title="TIPS &amp;" accent="BEST PRACTICES">
          <p style={{ ...BODY_TEXT, marginBottom: '2rem', maxWidth: 640 }}>
            Hard-earned lessons from pilots who have already lost lives learning them.
          </p>

          <div className="grid grid-cols-1 sm:grid-cols-2 gap-4">
            {[
              'Communicate on Discord before big attacks. A coordinated push by three pilots beats six lone wolves every time.',
              'Guard your logistics routes. Automated convoys are soft targets — the enemy knows this too.',
              'Check EWR before crossing contested airspace. Knowing where the SAMs are before you blunder into them is the difference between a mission and a statistic.',
              'Land at friendly bases to rearm. Don\'t waste lives attempting hot-pit refuels under fire when a safe base is 50nm away.',
              'Coordinate with JTAC for high-value targets. A laser-guided weapon on a confirmed target is more effective than carpet bombing the grid square.',
              'Watch the kill feed on the dashboard for situational awareness. If friendlies are dying in a sector, something is wrong — check in.',
            ].map((tip, i) => (
              <TipCard key={i} number={i + 1} text={tip} />
            ))}
          </div>
        </Subsection>

        {/* ══════════════════════════════════════════════════════════════════ */}
        {/* 10 — VICTORY & CAPTURE                                            */}
        {/* ══════════════════════════════════════════════════════════════════ */}
        <Subsection number="10" title="VICTORY &amp;" accent="CAPTURE">
          <p style={{ ...BODY_TEXT, marginBottom: '1.5rem', maxWidth: 680 }}>
            A round is won by territory, not kill count. The coalition that controls the required
            share of all objectives on the map wins the round — contested or neutral objectives
            don't count toward either side. Here's exactly what that takes.
          </p>

          <div className="grid grid-cols-1 sm:grid-cols-2 gap-4 mb-6">
            {[
              {
                heading: 'CAPTURE AN OBJECTIVE',
                body: 'Bring the objective\'s health to 20% or below and eliminate every defending infantry unit — CAS, artillery, whatever it takes. Move capture-capable troops into the zone; once it\'s held uncontested a countdown starts (both sides get a panel warning). Enemy troops re-entering the zone reset the timer. Some objectives also require a minimum share of the garrison destroyed before the timer can even start.',
              },
              {
                heading: 'CAPTURE A SAM SITE',
                body: 'SAM sites capture the instant your troops hold the zone — there is no timer. But the site\'s launchers, radars, and infantry are still live threats: clear them first or your troop transport won\'t survive the approach. Position is classified — not shown on the F10 map or dashboard, so you have to find it yourself (EWR tracks help).',
              },
              {
                heading: 'HURT ENEMY SUPPLY',
                body: 'Two ways in: destroy the automated convoys running between logistics hubs and forward objectives, or bomb the real map buildings — warehouses, fuel depots, industrial complexes — marked with orange boxes near objectives. Both directly and permanently degrade that objective\'s logistics rating.',
              },
              {
                heading: 'LAST STAND',
                body: 'When a coalition is reduced to its last primary objective (an airbase, naval base, or FARP), a countdown timer arms. If they can\'t retake ground before it expires, that coalition loses the round outright.',
              },
            ].map((card) => (
              <div
                key={card.heading}
                style={{
                  background: 'var(--bg-card)',
                  border: '1px solid var(--border)',
                  borderLeft: '3px solid var(--accent)',
                  borderRadius: '2px',
                  padding: '1rem 1.25rem',
                }}
              >
                <h4
                  style={{
                    fontFamily: "'Bebas Neue', sans-serif",
                    fontSize: '0.9rem',
                    letterSpacing: '0.15em',
                    color: 'var(--text)',
                    margin: '0 0 0.4rem 0',
                  }}
                >
                  {card.heading}
                </h4>
                <p style={{ ...BODY_TEXT }}>{card.body}</p>
              </div>
            ))}
          </div>

          <Callout type="info">
            Capturing an airbase or naval base flips its coalition and repairs one step of its
            logistics and services automatically. Warehouse stock and supply routes transfer to
            the new owner immediately.
          </Callout>
        </Subsection>

        {/* ══════════════════════════════════════════════════════════════════ */}
        {/* 11 — AIRCRAFT ROSTER                                              */}
        {/* ══════════════════════════════════════════════════════════════════ */}
        <Subsection number="11" title="AIRCRAFT" accent="ROSTER">
          <p style={{ ...BODY_TEXT, marginBottom: '1rem', maxWidth: 680 }}>
            Every airframe belongs to a role, and every role has its own life pool — how many
            times you can lose that class of aircraft before you're grounded, and how long until
            it refills. Lives are tracked per role, not per airframe: burn through your Standard
            pool and you can still fly Recon.
          </p>

          <div style={{ marginBottom: '1.5rem' }}>
            {server.lifeRoles.map((r) => (
              <InfoRow key={r.role} label={r.role} value={`${r.blurb} ${r.lives} lives, refills every ${r.resetHrs}h.`} />
            ))}
          </div>

          <div className="grid grid-cols-1 lg:grid-cols-2 gap-6 mb-6">
            {/* BLUFOR panel */}
            <div
              style={{
                background: 'var(--bg-card)',
                border: '1px solid var(--border)',
                borderLeft: '3px solid var(--blue)',
                borderRadius: '2px',
                padding: '1.25rem 1.5rem',
              }}
            >
              <h4
                style={{
                  fontFamily: "'Bebas Neue', sans-serif",
                  fontSize: '1rem',
                  letterSpacing: '0.2em',
                  color: 'var(--blue)',
                  margin: '0 0 1rem 0',
                }}
              >
                BLUFOR
              </h4>
              {server.roster.blue.map((grp) => (
                <div key={grp.role} style={{ marginBottom: '1rem' }}>
                  <div style={{ display: 'flex', alignItems: 'baseline', gap: '0.5rem', marginBottom: '0.4rem' }}>
                    <span style={{ ...LABEL_STYLE, color: 'var(--text)' }}>{grp.role}</span>
                    <span style={{ fontSize: '0.68rem', color: 'var(--text-dim)' }}>{grp.lives} lives / 6h</span>
                  </div>
                  <div style={{ display: 'flex', flexWrap: 'wrap' as const, gap: '0.4rem' }}>
                    {grp.aircraft.map((name) => (
                      <span
                        key={name}
                        style={{
                          fontSize: '0.78rem',
                          color: 'var(--text-muted)',
                          border: '1px solid var(--border)',
                          borderRadius: '2px',
                          padding: '0.2rem 0.55rem',
                        }}
                      >
                        {name}
                      </span>
                    ))}
                  </div>
                </div>
              ))}
            </div>

            {/* REDFOR panel */}
            <div
              style={{
                background: 'var(--bg-card)',
                border: '1px solid var(--border)',
                borderLeft: '3px solid var(--red)',
                borderRadius: '2px',
                padding: '1.25rem 1.5rem',
              }}
            >
              <h4
                style={{
                  fontFamily: "'Bebas Neue', sans-serif",
                  fontSize: '1rem',
                  letterSpacing: '0.2em',
                  color: 'var(--red)',
                  margin: '0 0 1rem 0',
                }}
              >
                REDFOR
              </h4>
              {server.roster.red.map((grp) => (
                <div key={grp.role} style={{ marginBottom: '1rem' }}>
                  <div style={{ display: 'flex', alignItems: 'baseline', gap: '0.5rem', marginBottom: '0.4rem' }}>
                    <span style={{ ...LABEL_STYLE, color: 'var(--text)' }}>{grp.role}</span>
                    <span style={{ fontSize: '0.68rem', color: 'var(--text-dim)' }}>{grp.lives} lives / 6h</span>
                  </div>
                  <div style={{ display: 'flex', flexWrap: 'wrap' as const, gap: '0.4rem' }}>
                    {grp.aircraft.map((name) => (
                      <span
                        key={name}
                        style={{
                          fontSize: '0.78rem',
                          color: 'var(--text-muted)',
                          border: '1px solid var(--border)',
                          borderRadius: '2px',
                          padding: '0.2rem 0.55rem',
                        }}
                      >
                        {name}
                      </span>
                    ))}
                  </div>
                </div>
              ))}
            </div>
          </div>

          <Callout type="info">
            A handful of airframes — {server.sharedAircraft} — are available to both coalitions.
            Everything else is side-exclusive. Lives shown here are per-round design values;
            check <strong style={{ color: 'var(--text)' }}>-lives</strong> in-game for whether
            life limits are currently active.
          </Callout>
        </Subsection>

        {/* ── Footer note ────────────────────────────────────────────────── */}
        <div className="vs-divider" style={{ marginBottom: '2.5rem' }} />
        <div
          style={{
            display: 'flex',
            justifyContent: 'space-between',
            alignItems: 'center',
            flexWrap: 'wrap' as const,
            gap: '1rem',
          }}
        >
          <p
            style={{
              fontFamily: "'JetBrains Mono', monospace",
              fontSize: '0.7rem',
              color: 'var(--text-dim)',
              margin: 0,
              letterSpacing: '0.05em',
            }}
          >
            VECTOR STRIKE · PILOT FIELD MANUAL · REV. 3.0 · UNCLASSIFIED · ALL PILOT SKILL LEVELS
          </p>
          <a
            href="https://discord.gg/wAsBEfse"
            target="_blank"
            rel="noopener noreferrer"
            className="vs-btn"
            style={{ fontSize: '0.9rem', padding: '0.6rem 1.5rem' }}
          >
            JOIN DISCORD
          </a>
        </div>

      </div>
    </section>
  )
}
