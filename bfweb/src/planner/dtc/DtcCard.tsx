import { useState } from 'react'
import Panel from '../../components/Panel'
import { download, ExportError } from '../exports'
import { mirageDtc, thunderAirspace, thunderRadio, viggenIni, type OtherExport } from '../others'
import { targetsFor, type Target } from '../targets'
import type { Overrides } from '../model'
import { aircraftDef, type Plan } from '../types'
import DtcWorkspace, { METHOD, type Msg } from './DtcWorkspace'
import { CopyPath, DownloadButton, Step, Steps } from './guides'

interface Props {
  plan: Plan
  readOnly: boolean
  onSystems: (key: string, ov: Overrides) => void
  tosSeconds: (i: number) => number | null
  tosText: (i: number) => string | null
  side: 'Blue' | 'Red'
}

/**
 * The planner's way into the jet: pick how you load it, open the cartridge
 * editor, download. Aircraft with their own cartridge files (Mirage, Viggen,
 * JF-17) get their steps right here.
 */
export default function DtcCard({ plan, readOnly, onSystems, tosSeconds, tosText, side }: Props) {
  const def = aircraftDef(plan.aircraft)
  const targets = targetsFor(def)
  const [pick, setPick] = useState<string | null>(null)
  const [open, setOpen] = useState(false)
  const [msg, setMsg] = useState<Msg | null>(null)
  const target: Target | undefined = targets.find(t => t.key === pick) ?? targets[0]

  const changes = (t: Target) => Object.keys(plan.systems?.[t.key] ?? {}).length
  const noRoute = plan.waypoints.length === 0

  const other = (label: string, f: () => OtherExport) => (
    <DownloadButton label={label} disabled={noRoute} title={noRoute ? 'Add your route on the map first' : undefined}
      onClick={() => {
        try {
          const out = f()
          download(out.filename, out.text, 'text/plain')
          setMsg({ kind: out.warnings.length ? 'warn' : 'ok', text: [`Downloaded ${out.filename}.`, ...out.warnings].join(' ') })
        } catch (e) {
          setMsg({ kind: 'err', text: e instanceof ExportError ? e.message : String(e) })
        }
      }} />
  )

  return (
    <Panel title="Data cartridge (DTC)" size="sm">
      <div style={{ padding: 12, display: 'flex', flexDirection: 'column', gap: 10, fontSize: '0.72rem', lineHeight: 1.55 }}>
        {target ? (
          <>
            <div style={{ color: 'var(--text-dim)' }}>
              Put your route and jet settings (radios, countermeasures, threats…) straight into the {def.label}, instead of typing them in the cockpit.
            </div>
            <div style={{ display: 'flex', gap: 8, flexWrap: 'wrap' }} role="group" aria-label="How you will load it">
              {targets.map((t, i) => (
                <button key={t.key} type="button" className="dtc-method" aria-pressed={t.key === target.key} onClick={() => setPick(t.key)}>
                  <div style={{ fontWeight: 700, fontSize: '0.74rem', display: 'flex', gap: 6, alignItems: 'center' }}>
                    {METHOD[t.tool].name}
                    {i === 0 && targets.length > 1 && <span style={{ fontSize: '0.56rem', fontWeight: 700, color: '#0b0f08', background: 'var(--accent)', borderRadius: 8, padding: '1px 5px' }}>RECOMMENDED</span>}
                  </div>
                  <div style={{ fontSize: '0.64rem', color: 'var(--text-dim)' }}>{METHOD[t.tool].blurb}</div>
                  {changes(t) > 0 && <div style={{ fontSize: '0.62rem', color: 'var(--accent)', marginTop: 2 }}>{changes(t)} setting{changes(t) === 1 ? '' : 's'} changed</div>}
                </button>
              ))}
            </div>
            <div>
              <button type="button" className="vs-btn" onClick={() => setOpen(true)} style={{ padding: '7px 14px', fontSize: '0.74rem' }}>
                {readOnly ? 'View cartridge' : 'Set up & download cartridge'}
              </button>
              <span style={{ marginLeft: 10, fontSize: '0.64rem', color: 'var(--text-dim)' }}>
                {noRoute ? 'Add your route on the map first.' : 'Your route fills the navigation pages for you.'}
              </span>
            </div>
            {open && (
              <DtcWorkspace plan={plan} targets={targets} target={target} onTarget={t => setPick(t.key)} readOnly={readOnly}
                onSystems={onSystems} tosSeconds={tosSeconds} tosText={tosText} onClose={() => setOpen(false)} />
            )}
          </>
        ) : def.other === 'M-2000C' ? (
          <Steps>
            <Step n={1} title="Download your Mirage cartridge">{other('Download .dtc', () => mirageDtc(plan))}</Step>
            <Step n={2} title="Put it in your Mirage cartridge folder"><CopyPath path="%USERPROFILE%\Saved Games\DCS\Datacartridges" /> (create it if missing)</Step>
            <Step n={3} title="Load it in the jet">Open the cartridge slot on the PSM, scroll to your cartridge, insert it; the MIP light is on while it loads (about 15 s). Up to 10 points land in BUT 11-20 and keep the mission&apos;s own BUTs.</Step>
          </Steps>
        ) : def.other === 'AJS37' ? (
          <Steps>
            <Step n={1} title="Download your Viggen cartridge">{other('Download CustomCartridge.ini', () => viggenIni(plan, i => (tosSeconds(i) == null || tosSeconds(0) == null ? null : tosSeconds(i)! - tosSeconds(0)!)))}</Step>
            <Step n={2} title="Put it next to your DCS folder"><CopyPath path="%USERPROFILE%\Saved Games\DCS_AJS37" /> (not inside the DCS folder)</Step>
            <Step n={3} title="Load it in the jet">Select the custom cartridge (LCtrl+LAlt+C), insert it behind the seat, then BER mode, REF/LOLA, IN, type 9099, press LS SKU. 000000 means loaded.</Step>
          </Steps>
        ) : def.other === 'JF-17' ? (
          <Steps>
            <Step n={1} title="Your JF-17 route comes from the mission">No file can carry it. What you can load: your route drawn on the HSD, and your radio presets.</Step>
            <Step n={2} title="Download the extras">
              <div style={{ display: 'flex', gap: 6, flexWrap: 'wrap' }}>{other('HSD route line', () => thunderAirspace(plan, side))}{other('Radio presets', () => thunderRadio(plan))}</div>
            </Step>
            <Step n={3} title="Put them in the JF-17 customization folders">
              <CopyPath path="%USERPROFILE%\Saved Games\DCS\Mods\aircraft\JF-17\Customization\airspace" />
              <CopyPath path="%USERPROFILE%\Saved Games\DCS\Mods\aircraft\JF-17\Customization\radio" />
              Then ground crew: DTC config, Air Space = Preset, Update DTC.
            </Step>
          </Steps>
        ) : (
          <div style={{ color: 'var(--text-dim)' }}>
            The {def.label} has no data cartridge in DCS or DCS-DTC. Your kneeboard card below still has the whole route.
          </div>
        )}
        {msg && <div role="status" style={{ color: msg.kind === 'err' ? '#f04747' : msg.kind === 'warn' ? '#f0a030' : 'var(--accent)' }}>{msg.text}</div>}
      </div>
    </Panel>
  )
}
