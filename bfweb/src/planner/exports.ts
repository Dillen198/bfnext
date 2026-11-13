// Exports shared by every aircraft: the CombatFlite XML route (DCS-DTC's
// waypoint page imports it for any aircraft it supports; elevation there is
// the steerpoint elevation in feet), file names, and the browser download.
// The cartridges themselves are built in native.ts, dcsdtc.ts and others.ts.

import type { Plan } from './types'

export class ExportError extends Error {}

function xmlEscape(s: string): string {
  return s.replace(/[<>&'"]/g, c => ({ '<': '&lt;', '>': '&gt;', '&': '&amp;', "'": '&apos;', '"': '&quot;' })[c]!)
}

/** Steerpoint names, falling back to the number for an unnamed one. */
function wptName(name: string, i: number): string {
  return name.trim() || `WP${i + 1}`
}

/** A file name DCS's DTC manager accepts (it refuses `*\/?<>|:"`). */
export function safeName(name: string): string {
  return name.replace(/[*/?<>|\\:"]/g, '').replace(/\s+/g, ' ').trim().slice(0, 60) || 'Flight plan'
}

/** The route as a CombatFlite XML export, which DCS-DTC imports on the
 *  waypoint page of every aircraft ("Import > From CombatFlite XML Export"). */
export function combatFliteXml(plan: Plan): string {
  const wpts = plan.waypoints.map((w, i) => [
    '      <Waypoint>',
    `        <Name>${xmlEscape(wptName(w.name, i))}</Name>`,
    '        <Position>',
    `          <Latitude>${w.lat.toFixed(6)}</Latitude>`,
    `          <Longitude>${w.lon.toFixed(6)}</Longitude>`,
    `          <Altitude>${Math.round(w.elev_ft)}</Altitude>`,
    '        </Position>',
    '      </Waypoint>',
  ].join('\n'))
  return [
    '<?xml version="1.0" encoding="utf-8"?>',
    '<Objects>',
    `  <Route Name="${xmlEscape(safeName(plan.name))}">`,
    '    <Waypoints>',
    ...wpts,
    '    </Waypoints>',
    '  </Route>',
    '</Objects>',
    '',
  ].join('\n')
}

/** Save `content` as a file in the browser. */
export function download(filename: string, content: string | Blob, mime: string) {
  const url = URL.createObjectURL(typeof content === 'string' ? new Blob([content], { type: mime }) : content)
  const a = document.createElement('a')
  a.href = url
  a.download = filename
  document.body.appendChild(a)
  a.click()
  a.remove()
  setTimeout(() => URL.revokeObjectURL(url), 1000)
}
