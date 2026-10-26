import { describe, expect, it } from 'vitest'
import { courseTrue, distanceNm, fmtClock, fromDcsXY, legs, parseClock, terrainAt, toDcsXY } from './nav'
import { combatFliteXml, safeName } from './exports'
import { newPlan, type Plan, type PlanWaypoint } from './types'

const wp = (name: string, lat: number, lon: number, extra: Partial<PlanWaypoint> = {}): PlanWaypoint => ({
  name, lat, lon, alt_ft: 20000, elev_ft: 0, target: false, note: '', ...extra,
})

// Batumi and Kobuleti, on the Caucasus map.
const BATUMI: [number, number] = [41.609596846220164, 41.600236917191026]
const KOBULETI: [number, number] = [41.9295, 41.8714]

function plan(over: Partial<Plan> = {}): Plan {
  return {
    ...newPlan('F-16C_50'),
    name: 'CAS north',
    waypoints: [wp('HOME', ...BATUMI), wp('TGT', ...KOBULETI, { target: true, elev_ft: 59 })],
    ...over,
  }
}

describe('nav', () => {
  it('measures a leg', () => {
    const d = distanceNm(BATUMI, KOBULETI)
    expect(d).toBeGreaterThan(20)
    expect(d).toBeLessThan(23)
    const c = courseTrue(BATUMI, KOBULETI)
    expect(c).toBeGreaterThan(25)
    expect(c).toBeLessThan(40)
    expect(courseTrue([0, 0], [0, 1])).toBeCloseTo(90, 5)
  })

  it('times legs at the plan speed', () => {
    const [leg] = legs(plan({ speed_kts: 360 }), () => 6)
    expect(leg.ete_s).toBeCloseTo((leg.dist_nm / 360) * 3600, 6)
    expect(leg.course_mag).toBeCloseTo((leg.course_true - 6 + 360) % 360, 6)
  })

  it('parses and prints mission clock times', () => {
    expect(parseClock('08:30')).toBe(8 * 3600 + 30 * 60)
    expect(parseClock('23:59:59')).toBe(86399)
    expect(parseClock('24:00')).toBeNull()
    expect(parseClock('8:30')).toBeNull()
    expect(fmtClock(86400 + 61)).toBe('00:01:01')
  })

  it('finds the DCS map and its terrain id', () => {
    expect(terrainAt(...BATUMI)?.id).toBe('Caucasus')
    expect(terrainAt(34.5, 36.5)?.id).toBe('Syria')
    expect(terrainAt(0, -150)).toBeNull()
  })

  it('projects to DCS map metres, x north and y east', () => {
    // Batumi's airfield in the DCS world database: x -355810.7, y 617386.2.
    const { x, y } = toDcsXY(...BATUMI, terrainAt(...BATUMI)!.terrain)
    expect(x).toBeCloseTo(-355810.7, 0)
    expect(y).toBeCloseTo(617386.2, 0)
    const back = fromDcsXY(x, y, terrainAt(...BATUMI)!.terrain)
    expect(back.lat).toBeCloseTo(BATUMI[0], 7)
    expect(back.lon).toBeCloseTo(BATUMI[1], 7)
  })
})

describe('DCS-DTC export', () => {
  it('writes a CombatFlite XML route DCS-DTC can import', () => {
    const xml = combatFliteXml(plan({ waypoints: [wp('A&B <1>', 41.5, 41.6, { elev_ft: 1234.4 })] }))
    // The shape DCS-DTC's CombatFliteXML importer walks: Waypoints >
    // Waypoint > Name, Position > Latitude/Longitude/Altitude.
    expect(xml).toMatch(/<Waypoints>\s*<Waypoint>\s*<Name>A&amp;B &lt;1&gt;<\/Name>\s*<Position>\s*<Latitude>41\.500000<\/Latitude>\s*<Longitude>41\.600000<\/Longitude>\s*<Altitude>1234<\/Altitude>\s*<\/Position>\s*<\/Waypoint>\s*<\/Waypoints>/)
    expect(xml.match(/<Waypoint>/g)).toHaveLength(1)
  })
})

describe('file names', () => {
  it('strips characters the DTC manager refuses', () => {
    expect(safeName(' a/b:c*? ')).toBe('abc')
    expect(safeName('***')).toBe('Flight plan')
  })
})
