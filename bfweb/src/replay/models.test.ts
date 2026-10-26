import { describe, expect, it } from 'vitest'
import { existsSync, readFileSync } from 'node:fs'
import db from '../../../shared/models/tacview.json'
import { realOf, tacviewType, shortNameOf } from './models'
import live from './live-types.json'

const catalog = readFileSync(new URL('../../../shared/models/catalog.ts', import.meta.url), 'utf8')

describe('realOf', () => {
  it("draws the common DCS types with Tacview's meshes, matched as Tacview matches them", () => {
    // its database, exact or ignoring separators
    expect(realOf('air', 'FA-18C_hornet')).toBe('FixedWing.F-18C')
    expect(realOf('air', 'MiG-29S')).toBe('FixedWing.MiG-29')
    expect(realOf('air', 'E-3A')).toBe('FixedWing.E-3')
    expect(realOf('air', 'A-50')).toBe('FixedWing.A-50')
    expect(realOf('air', 'AJS37')).toBe('FixedWing.AJ 37')
    expect(realOf('helo', 'UH-60A')).toBe('Rotorcraft.UH-60')
    expect(realOf('sam', 'ZSU-23-4 Shilka')).toBe('AntiAircraft.AAA.ZSU-23-4')
    expect(realOf('sam', 'S_75M_Volhov')).toBe('AntiAircraft.SAM.SA-2.LN')
    expect(realOf('sam', '5p73 s-125 ln')).toBe('AntiAircraft.SAM.SA-3.5P73')
    expect(realOf('carrier', 'KUZNECOW')).toBe('Watercraft.Admiral Kuznetsov')
    expect(realOf('carrier', 'Stennis')).toBe('Watercraft.CVN-74')
    expect(realOf('missile', 'AIM_120C')).toBe('Missile.AIM-120C')
    // the longest name it knows that the recorded one starts with
    expect(realOf('air', 'F-16C_50')).toBe('FixedWing.F-16')
    expect(realOf('helo', 'CH-47Fbl1')).toBe('Rotorcraft.Chinook')
    expect(realOf('air', 'KC135MPRS')).toBe('FixedWing.KC-135C')
    // short names too, without splitting a number
    expect(realOf('air', 'J-10A')).toBe('FixedWing.J-10')
    expect(realOf('air', 'A-4E-C')).toBe('FixedWing.A-4')
    expect(realOf('air', 'F4U-1D')).toBe('FixedWing.F4U')
    expect(realOf('air', 'A400M')).toBe('FixedWing.A400M')
    // lookalikes for what Tacview does not know
    expect(realOf('armor', 'CHAP_T90M')).toBe('Vehicle.Tank.T-80U')
    expect(realOf('armor', 'CHAP_MATV')).toBe('Vehicle.Car.Humvee')
    expect(realOf('ship', 'USS_Arleigh_Burke_IIa')).toBe('Watercraft.F710')
    expect(realOf('air', 'WingLoong-I')).toBe('FixedWing.MQ-9')
    // and a kind's generic mesh when nothing else fits
    expect(realOf('air', 'Totally_Unknown_Jet')).toBe('Core.FixedWing')
    expect(realOf('rocket', 'S_8KOM')).toBe('Rocket.Hydra 70') // Tacview's own pick for the S-8
  })

  it('never takes a name match across kinds', () => {
    // "F-16" as a ground object's name is not a jet
    expect(realOf('vehicle', 'F-16 tow tractor')).not.toMatch(/^FixedWing/)
  })

  it("reads Tacview's names and ranges", () => {
    expect(shortNameOf('F-16C_50')).toBe('F-16C')
    expect(tacviewType('FA-18C_hornet')?.long).toMatch(/Hornet/)
    expect(tacviewType('ZU-23 Emplacement')?.engage).toBeGreaterThan(1000)
  })

  it("every mesh Tacview's database or the catalogue names is installed", () => {
    const named = new Set<string>(db.shapes)
    for (const m of catalog.matchAll(/'((?:FixedWing|Rotorcraft|Rotocraft|AntiAircraft|Vehicle|Watercraft|Missile|Bomb|Rocket|Core|Other)\.[^']+)'/g)) named.add(m[1])
    named.delete('Core.Empty') // Tacview's invisible placeholder: no geometry
    const missing = [...named].filter(r => !existsSync(new URL(`../../public/models/${r}.glb`, import.meta.url)))
    expect(missing).toEqual([])
  })

  it('every type seen on the live servers has a model, and every model a file', () => {
    for (const [k, n] of live as [string, string, number][]) {
      const r = realOf(k, n)
      expect(r, `${k} ${n}`).toBeTruthy()
      expect(existsSync(new URL(`../../public/models/${r}.glb`, import.meta.url)), `${n} -> ${r}.glb`).toBe(true)
    }
  })
})
