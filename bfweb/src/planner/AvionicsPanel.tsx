import { useState, type CSSProperties } from 'react'
import { useQuery } from '@tanstack/react-query'
import { Download } from '@icons'
import Panel from '../components/Panel'
import SystemsEditor from './SystemsEditor'
import { buildDcsDtc, shareString } from './dcsdtc'
import { buildNative, nativeReady } from './native'
import { download, ExportError } from './exports'
import { mirageDtc, thunderAirspace, thunderRadio, viggenIni, type OtherExport } from './others'
import { dcsdtcTarget, hasModel, loadModel, targetsFor, type Target } from './targets'
import type { Overrides } from './model'
import { aircraftDef, type NativeType, type Plan } from './types'

export type Msg = { kind: 'ok' | 'warn' | 'err'; text: string }

interface Props {
  plan: Plan
  readOnly: boolean
  onSystems: (key: string, ov: Overrides) => void
  /** Mission clock at waypoint i: seconds from midnight, or null if untimed. */
  tosSeconds: (i: number) => number | null
  /** The same as "HH:MM:SS". */
  tosText: (i: number) => string | null
  onMsg: (m: Msg) => void
  side: 'Blue' | 'Red'
}

const btn: CSSProperties = { padding: '4px 8px', fontSize: '0.7rem', display: 'flex', gap: 5, alignItems: 'center' }

/**
 * Everything the jet holds beyond the route, per export target: a native
 * DCS cartridge and/or a DCS-DTC preset. The pilot edits any setting; only
 * changes are saved with the plan, and the exports fill in the rest.
 */
export default function AvionicsPanel({ plan, readOnly, onSystems, tosSeconds, tosText, onMsg, side }: Props) {
  const def = aircraftDef(plan.aircraft)
  const targets = targetsFor(def)
  const [pick, setPick] = useState<string | null>(null)
  const active: Target | undefined = targets.find(t => t.key === pick) ?? targets[0]

  const { data: model, error, isLoading } = useQuery({
    queryKey: ['planner-model', active?.key],
    queryFn: () => loadModel(active!),
    enabled: !!active && hasModel(active),
    staleTime: Infinity,
  })

  if (!active) {
    const other = (label: string, f: () => OtherExport, mime = 'text/plain') => (
      <button type="button" className="vs-btn-outline" style={btn} disabled={!plan.waypoints.length}
        onClick={() => {
          try {
            const out = f()
            download(out.filename, out.text, mime)
            onMsg({ kind: out.warnings.length ? 'warn' : 'ok', text: `${out.warnings.join(' ')} Put ${out.filename} in ${out.folder}.`.trim() })
          } catch (e) {
            onMsg({ kind: 'err', text: e instanceof ExportError ? e.message : String(e) })
          }
        }}>
        <Download size={11} /> {label}
      </button>
    )
    return (
      <Panel title="Avionics" size="sm">
        <div style={{ padding: 10, display: 'flex', flexDirection: 'column', gap: 8, fontSize: '0.66rem', color: 'var(--text-dim)', lineHeight: 1.6 }}>
          {def.other === 'M-2000C' && (
            <>
              <div>The Mirage 2000C loads a data cartridge from Saved Games\DCS\Datacartridges: up to 20 BUTs. A route of 10 or fewer lands in BUT 11-20 and keeps the mission&apos;s own.</div>
              <div style={{ display: 'flex', gap: 6 }}>{other('M-2000C cartridge (.dtc)', () => mirageDtc(plan))}</div>
            </>
          )}
          {def.other === 'AJS37' && (
            <>
              <div>The Viggen loads CustomCartridge.ini from Saved Games\DCS_AJS37 (beside the DCS folder): departure LS, route B1-B9 with target points, arrival L1.</div>
              <div style={{ display: 'flex', gap: 6 }}>{other('Viggen cartridge (.ini)', () => viggenIni(plan, i => (tosSeconds(i) == null || tosSeconds(0) == null ? null : tosSeconds(i)! - tosSeconds(0)!)))}</div>
            </>
          )}
          {def.other === 'JF-17' && (
            <>
              <div>The JF-17 takes its route from the mission, not a file. What you can load: your planned route drawn on the HSD (air space, DTC Config &gt; Air Space = Preset) and your radio presets as presets 22+.</div>
              <div style={{ display: 'flex', gap: 6, flexWrap: 'wrap' }}>
                {other('HSD route line', () => thunderAirspace(plan, side))}
                {other('Radio presets', () => thunderRadio(plan))}
              </div>
            </>
          )}
          {!def.other && <div>Neither DCS&apos;s cartridge nor DCS-DTC supports the {def.label}. The kneeboard card and route XML still work.</div>}
        </div>
      </Panel>
    )
  }

  const ov = (plan.systems?.[active.key] ?? {}) as Overrides
  const changes = Object.keys(ov).length

  const run = async (f: () => Promise<{ filename: string; text: string; warnings: string[] }>, how: string, mime = 'application/json') => {
    try {
      const out = await f()
      download(out.filename, out.text, mime)
      onMsg(out.warnings.length ? { kind: 'warn', text: `${out.warnings.join(' ')} ${how}` } : { kind: 'ok', text: how })
    } catch (e) {
      onMsg({ kind: 'err', text: e instanceof ExportError ? e.message : String(e) })
    }
  }

  const exportNative = () => run(async () => {
    const m = await loadModel(active)
    const out = buildNative(plan, active.code as NativeType, m.defaults, ov, tosSeconds)
    return out
  }, 'Put the .dtc in Saved Games\\DCS\\DTC, then in the jet open the DTC (briefing "Open DTC", or F8 > Ground Crew > DTC Interface on a friendly field) and load it.')

  const dt = active.tool === 'dcsdtc' ? dcsdtcTarget(active.code) : undefined
  const exportDcsDtc = (partial: boolean) => run(async () => {
    const m = await loadModel(active)
    return buildDcsDtc(plan, dt!, m.defaults, ov, partial, tosText)
  }, partial
    ? 'In DCS-DTC: open the aircraft, Load/Save, "Load from file", pick this file and tick what to load.'
    : `Put it in Documents\\DCS-DTC\\Presets\\${active.code}\\ and restart DCS-DTC; it shows as a preset.`)

  const copyShare = async () => {
    try {
      const m = await loadModel(active)
      const out = buildDcsDtc(plan, dt!, m.defaults, ov, true, tosText)
      await navigator.clipboard.writeText(await shareString(out.text))
      onMsg({ kind: 'ok', text: 'Copied. In DCS-DTC: Load/Save, "Load from clipboard".' })
    } catch (e) {
      onMsg({ kind: 'err', text: String(e) })
    }
  }

  const nativePending = active.tool === 'native' && !nativeReady(active.code as NativeType)

  return (
    <Panel
      title="Avionics"
      size="sm"
      count={changes ? `${changes} changed` : undefined}
      right={targets.length > 1 ? (
        <div style={{ display: 'flex', border: '1px solid var(--border)', borderRadius: 4, overflow: 'hidden' }}>
          {targets.map(t => (
            <button key={t.key} type="button" onClick={() => setPick(t.key)}
              style={{ padding: '3px 9px', fontSize: '0.62rem', fontWeight: 700, border: 'none', cursor: 'pointer',
                background: t.key === active.key ? 'var(--accent)' : 'var(--bg-elevated)', color: t.key === active.key ? '#0b0f08' : 'var(--text-dim)' }}>
              {t.tool === 'native' ? 'DCS DTC' : 'DCS-DTC'}
            </button>
          ))}
        </div>
      ) : undefined}
      bodyStyle={{ maxHeight: 560, overflowY: 'auto' }}
    >
      <div style={{ padding: 10, display: 'flex', flexDirection: 'column', gap: 8 }}>
        <div style={{ fontSize: '0.62rem', color: 'var(--text-dim)', lineHeight: 1.6 }}>
          {active.tool === 'native'
            ? `DCS's own Data Transfer Cartridge for the ${def.label}: every section the Mission Editor's DTC manager has. The route fills the nav pages; tabs you change are switched on to load.`
            : `DCS-DTC (github.com/the-paid-actor/dcs-dtc) preset for the ${def.label}. Uploads: ${dt?.uploads || 'nothing yet (stores only)'}.`}
        </div>

        <div style={{ display: 'flex', gap: 6, flexWrap: 'wrap' }}>
          {active.tool === 'native' ? (
            <button type="button" className="vs-btn-outline" style={btn} disabled={!plan.waypoints.length || nativePending} onClick={exportNative}
              title={nativePending ? 'This aircraft\'s cartridge exporter is not in this build yet' : 'Native DCS cartridge'}>
              <Download size={11} /> .dtc cartridge
            </button>
          ) : (
            <>
              <button type="button" className="vs-btn-outline" style={btn} disabled={!plan.waypoints.length} onClick={() => exportDcsDtc(true)}
                title="Only the route and what you changed; everything else null, so it doesn't overwrite your other DCS-DTC settings">
                <Download size={11} /> Load-from-file
              </button>
              <button type="button" className="vs-btn-outline" style={btn} disabled={!plan.waypoints.length} onClick={() => exportDcsDtc(false)}
                title="A complete preset for DCS-DTC's Presets folder">
                <Download size={11} /> Whole preset
              </button>
              <button type="button" className="vs-btn-ghost" style={btn} disabled={!plan.waypoints.length} onClick={copyShare}
                title="DCS-DTC's compressed clipboard string">
                Copy share string
              </button>
            </>
          )}
        </div>

        {!hasModel(active) && <div style={{ fontSize: '0.66rem', color: '#f0a030' }}>Settings for this target are not in this build yet.</div>}
        {isLoading && <div style={{ fontSize: '0.66rem', color: 'var(--text-dim)' }}>Loading {def.label} settings…</div>}
        {error && <div style={{ fontSize: '0.66rem', color: '#f04747' }}>{(error as Error).message}</div>}
        {model && (
          <SystemsEditor
            defaults={model.defaults}
            annotations={model.annotations}
            overrides={ov}
            readOnly={readOnly}
            onChange={next => onSystems(active.key, next)}
          />
        )}
      </div>
    </Panel>
  )
}
