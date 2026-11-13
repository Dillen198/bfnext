// "How do I get this into the jet?" -- numbered steps per loading method,
// with the download in the step that needs it and the folder ready to copy.

import { useState, type CSSProperties, type ReactNode } from 'react'
import { Download, ExternalLink } from '@icons'
import type { NativeType } from '../types'

const stepNo: CSSProperties = {
  flex: '0 0 auto', width: 20, height: 20, borderRadius: 10, display: 'flex', alignItems: 'center', justifyContent: 'center',
  background: 'var(--accent)', color: '#0b0f08', fontSize: '0.66rem', fontWeight: 800,
}

export function Steps({ children }: { children: ReactNode }) {
  return <ol style={{ listStyle: 'none', margin: 0, padding: 0, display: 'flex', flexDirection: 'column', gap: 10 }}>{children}</ol>
}

export function Step({ n, title, children }: { n: number; title: string; children?: ReactNode }) {
  return (
    <li style={{ display: 'flex', gap: 10, alignItems: 'flex-start' }}>
      <span style={stepNo} aria-hidden>{n}</span>
      <div style={{ flex: 1, minWidth: 0, fontSize: '0.72rem', lineHeight: 1.55 }}>
        <div style={{ fontWeight: 700, color: 'var(--text)' }}>{title}</div>
        {children && <div style={{ color: 'var(--text-muted, var(--text-dim))' }}>{children}</div>}
      </div>
    </li>
  )
}

/** A folder path with a copy button. */
export function CopyPath({ path }: { path: string }) {
  const [done, setDone] = useState(false)
  return (
    <span style={{ display: 'inline-flex', alignItems: 'center', gap: 6, flexWrap: 'wrap', margin: '2px 0' }}>
      <code style={{ fontFamily: 'var(--font-mono)', fontSize: '0.68rem', padding: '2px 6px', borderRadius: 3, background: 'var(--bg-elevated)', border: '1px solid var(--border)', wordBreak: 'break-all' }}>{path}</code>
      <button type="button" className="vs-btn-ghost" style={{ padding: '2px 6px', fontSize: '0.62rem' }}
        onClick={() => {
          navigator.clipboard?.writeText(path).then(() => { setDone(true); setTimeout(() => setDone(false), 1500) }).catch(() => undefined)
        }}>
        {done ? 'Copied' : 'Copy'}
      </button>
    </span>
  )
}

export function DownloadButton({ label, onClick, disabled, primary = true, title }: {
  label: string; onClick: () => void; disabled?: boolean; primary?: boolean; title?: string
}) {
  return (
    <button type="button" className={primary ? 'vs-btn' : 'vs-btn-outline'} disabled={disabled} onClick={onClick} title={title}
      style={{ display: 'inline-flex', gap: 6, alignItems: 'center', padding: '6px 12px', fontSize: '0.72rem', marginTop: 4 }}>
      <Download size={12} /> {label}
    </button>
  )
}

/** Cockpit tips per aircraft, from flown missions and the manuals. */
const NATIVE_TIPS: Record<NativeType, string> = {
  'F-16C_50': 'Your route is steerpoints 1 onward (keep 25 free: it is the bullseye). Set the CMDS knob to STBY before you load.',
  'FA-18C_hornet': 'Your route is sequence 1 (2 and 3 as well if it is longer than 15 points).',
  'AH-64D_BLK_II': 'On the DTU page press MASTER LOAD. Your route is route 1 of mission 1.',
  'F-14BU': 'The RIO picks flight plan 2 on the CDNU (MDL > FPLN). Plan 1 stays the mission\'s own route.',
  'MiG-29 Fulcrum': 'Your route is program 1, with its first three en-route points as steer points.',
}

export function NativeGuide({ type, map, fileName, onDownload, disabled, why }: {
  type: NativeType; map?: string; fileName: string; onDownload: () => void; disabled: boolean; why?: string
}) {
  return (
    <Steps>
      <Step n={1} title="Download your cartridge">
        <DownloadButton label={`Download ${fileName}`} onClick={onDownload} disabled={disabled} title={why} />
        {why && <div style={{ color: '#f0a030', marginTop: 4 }}>{why}</div>}
      </Step>
      <Step n={2} title="Put it in your DCS cartridge folder">
        <CopyPath path="%USERPROFILE%\Saved Games\DCS\DTC" />
        <div>Create the DTC folder if it is not there.</div>
      </Step>
      <Step n={3} title="Load it in the jet">
        On the briefing screen press <b>Open DTC</b>, or later, parked at a friendly airfield: <b>F8 &gt; Ground Crew &gt; DTC Interface</b>.
        Pick <b>{fileName.replace(/\.dtc$/, '')}</b> and press <b>Load</b>.
        {map && <> It only loads on the <b>{map}</b> map.</>}
        <div style={{ marginTop: 4 }}>{NATIVE_TIPS[type]}</div>
      </Step>
    </Steps>
  )
}

export function DcsDtcGuide({ code, label, onLoadFile, onWhole, onShare, disabled, storeOnly }: {
  code: string; label: string; onLoadFile: () => void; onWhole: () => void; onShare: () => void; disabled: boolean; storeOnly?: boolean
}) {
  return (
    <Steps>
      <Step n={1} title="Install DCS-DTC (once)">
        A free Windows app that types your data into the cockpit for you.{' '}
        <a href="https://github.com/the-paid-actor/dcs-dtc/releases" target="_blank" rel="noreferrer" style={{ color: 'var(--accent)', display: 'inline-flex', alignItems: 'center', gap: 3 }}>
          Get it on GitHub <ExternalLink size={10} />
        </a>
      </Step>
      <Step n={2} title="Download your preset">
        <div style={{ display: 'flex', gap: 6, flexWrap: 'wrap', alignItems: 'center' }}>
          <DownloadButton label="Download preset" onClick={onLoadFile} disabled={disabled} />
          <button type="button" className="vs-btn-ghost" style={{ padding: '4px 8px', fontSize: '0.66rem', marginTop: 4 }} disabled={disabled} onClick={onShare}
            title="Copy DCS-DTC's share code instead, for Load/Save > Load from clipboard">
            or copy share code
          </button>
        </div>
      </Step>
      <Step n={3} title="Open it in DCS-DTC">
        Pick the <b>{label}</b>, open your preset, then <b>Load/Save &gt; Load from file</b> and choose the file.
        Tick what you want: it only replaces what you tick, the rest of your preset stays.
      </Step>
      <Step n={4} title="Send it to the jet">
        {storeOnly
          ? <>DCS-DTC cannot type into this aircraft yet: use the DCS cartridge instead.</>
          : <>Sit in the cockpit and press <b>Upload</b> in DCS-DTC. The server must allow own-aircraft export (this campaign's server does).</>}
      </Step>
      <li style={{ fontSize: '0.64rem', color: 'var(--text-dim)', lineHeight: 1.5 }}>
        Prefer a whole new preset?{' '}
        <button type="button" className="vs-btn-ghost" style={{ padding: '1px 6px', fontSize: '0.64rem' }} disabled={disabled} onClick={onWhole}>Download full preset</button>{' '}
        and put it in <CopyPath path={`%USERPROFILE%\\Documents\\DCS-DTC\\Presets\\${code}`} />, then restart DCS-DTC.
      </li>
    </Steps>
  )
}
