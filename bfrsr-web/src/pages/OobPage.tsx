// ORDER OF BATTLE (MORE menu): each side's air order of battle, as the
// regiment rolls on the operations room wall. One line per airframe type:
// how many are on strength, how many were lost this round, and a strength
// bar of what is left of the round's starting number.
//
// The data is the engine's `ai_air_stock` (`/api/oob`, ENGINE.md 3.1, E6).
// Losses are public; a side's stock is classified to everyone but that
// side's aircrew and the staff, and bfdb sends it as null.
//
// ?mock (dev builds) renders a fixture as a NATO pilot sees it; ?mock=admin
// shows both sides' stock.

import { Aircraft } from '@icons'
import type { OobEntry, OrderOfBattle } from '../api'
import { campaign } from '../config/campaign'
import PageHeader from '../components/PageHeader'
import QueryState from '../components/QueryState'
import { useRound } from '../context/RoundContext'
import { oobByRole, oobEmpty, oobStrength, oobTotals, useOrderOfBattle, type OobTotals } from '../lib/oob'

type SideKey = 'blue' | 'red'

const sideLabel = (s: SideKey) => (s === 'blue' ? campaign.blueLabel : campaign.redLabel)

/** The red ink stamp over a side's classified stock. */
function Classified({ size = 'sm' }: { size?: 'sm' | 'md' }) {
  return (
    <span className={`stamp${size === 'sm' ? ' stamp--sm' : ''} oob-classified`} title="Stock is shown only to that side's aircrew and the staff">
      Classified
    </span>
  )
}

/** One side's totals: aircraft on strength and lost, across every type. */
function SideTotal({ side, t }: { side: SideKey; t: OobTotals }) {
  return (
    <div className={`vs-card oob-total oob-roll oob-roll--${side}`} data-testid={`oob-total-${side}`}>
      <span className={`rsr-side-tag rsr-side-tag--${side}`}>{sideLabel(side)}</span>
      <div className="oob-total-fig">
        <span className="oob-label">On strength</span>
        {t.stock == null
          ? <Classified size="md" />
          : <span className="rsr-fig" data-testid={`oob-stock-${side}`}>{t.stock}{t.unlimited ? '+' : ''}</span>}
      </div>
      <div className="oob-total-fig">
        <span className="oob-label">Lost this round</span>
        <span className="rsr-fig rsr-fig--red" data-testid={`oob-lost-${side}`}>{t.lost}</span>
      </div>
      <div className="oob-total-fig">
        <span className="oob-label">Types</span>
        <span className="rsr-fig rsr-fig--dim">{t.types}</span>
      </div>
    </div>
  )
}

function StrengthBar({ r }: { r: OobEntry }) {
  const left = oobStrength(r)
  if (left == null) {
    return <div className="oob-bar oob-bar--redacted" aria-hidden="true" />
  }
  return (
    <div className={`oob-bar${r.unlimited ? ' oob-bar--unl' : ''}`} role="meter" aria-valuemin={0} aria-valuemax={100}
      aria-valuenow={Math.round(left * 100)} aria-label={`${r.type} strength left`}>
      <div className="oob-bar-stock" style={{ width: `${left * 100}%` }} />
      {!r.unlimited && <div className="oob-bar-lost" style={{ width: `${(1 - left) * 100}%` }} />}
    </div>
  )
}

function RollRow({ r }: { r: OobEntry }) {
  return (
    <div className="oob-row" data-testid="oob-row">
      <span className="oob-type" title={r.type}>
        <span>{r.type}</span>
        {r.unlimited && <span className="oob-unl" title="An unlimited-aircraft base holds this type: it never runs out">∞ Unltd</span>}
      </span>
      {r.stock == null
        ? <Classified />
        : <span className="rsr-fig oob-num">{r.stock}</span>}
      <span className={`rsr-fig oob-num${r.lost > 0 ? ' rsr-fig--red' : ' rsr-fig--dim'}`}>{r.lost}</span>
      <StrengthBar r={r} />
    </div>
  )
}

/** A side's regiment roll, by role, each role's types by name. */
function Roll({ side, rows }: { side: SideKey; rows: OobEntry[] }) {
  const groups = oobByRole(rows)
  const classified = rows.length > 0 && rows.every(r => r.stock == null)
  return (
    <section className={`vs-card p-0 oob-roll oob-roll--${side}`} aria-label={`${sideLabel(side)} order of battle`} data-testid={`oob-roll-${side}`}>
      <div className="rsr-panel-head flex items-center gap-2 px-4 py-2.5">
        <span className="rsr-panel-title" style={{ fontSize: '0.7rem', color: `var(--${side})` }}>{sideLabel(side)} · regiment roll</span>
        <span className="ml-auto oob-label">{rows.length} type{rows.length === 1 ? '' : 's'}</span>
      </div>
      {rows.length === 0 ? (
        <div className="awaiting">No airframes on file for {sideLabel(side)}.</div>
      ) : (
        <>
          <div className="oob-row oob-row--head oob-label">
            <span>Airframe</span><span>On strength</span><span>Lost</span>
          </div>
          {groups.map(g => (
            <div key={g.role}>
              <div className="oob-role">{g.role}</div>
              {g.rows.map(r => <RollRow key={r.type} r={r} />)}
            </div>
          ))}
          {classified && (
            <div className="oob-label" style={{ padding: '8px 14px', borderTop: '1px solid var(--border)' }}>
              Strength classified. Losses are reported to all.
            </div>
          )}
        </>
      )}
    </section>
  )
}

/** The page body for an order of battle already fetched (tests render this). */
export function OrderOfBattleView({ oob }: { oob: OrderOfBattle }) {
  if (oobEmpty(oob)) {
    return (
      <div className="vs-card">
        <div className="awaiting">
          <span className="awaiting-cursor" aria-hidden="true" />
          <div>
            Awaiting traffic
            <small>No order of battle on file for this round. The engine reports one once air stocks are on.</small>
          </div>
        </div>
      </div>
    )
  }
  return (
    <>
      <div className="oob-totals">
        <SideTotal side="blue" t={oobTotals(oob.blue)} />
        <SideTotal side="red" t={oobTotals(oob.red)} />
      </div>
      <div className="oob-cols">
        <Roll side="blue" rows={oob.blue} />
        <Roll side="red" rows={oob.red} />
      </div>
      <p className="oob-label" style={{ margin: 0, lineHeight: 1.6 }}>
        On strength: airframes in the warehouses of the bases a side holds. Lost: this round, player and AI.
        The bar is what is left of the round's starting number. ∞ Unltd: an unlimited base holds the type.
      </p>
    </>
  )
}

export default function OobPage() {
  const { selectedRound } = useRound()
  const q = useOrderOfBattle(selectedRound)
  return (
    <div style={{ display: 'flex', flexDirection: 'column', flex: 1, overflow: 'hidden' }}>
      <PageHeader title="ORDER OF BATTLE" file="Order of battle" icon={Aircraft}
        stamp={<span className="stamp">Secret</span>}
        sub={`Air order of battle · ${selectedRound != null ? 'round on file' : 'this round'}`} />
      <div className="vs-page">
        <QueryState what="order of battle" isLoading={q.isLoading} isError={q.isError} error={q.error} onRetry={q.refetch} />
        {q.data && <OrderOfBattleView oob={q.data} />}
      </div>
    </div>
  )
}
