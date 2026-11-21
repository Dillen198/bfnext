import { describe, it, expect } from 'vitest'
import {
  kd, sl, fmtHours, fmtDuration, fmtTimeZ, ago,
  windDir, visStr, cloudStr, classifyTargetClass,
} from './format'

describe('kd', () => {
  it('divides total kills by deaths', () => {
    expect(kd(3, 1, 2)).toBe('2.00')
  })
  it('reports infinity for kills with no deaths, not zero', () => {
    expect(kd(4, 0, 0)).toBe('∞')
  })
  it('reports 0.00 for a pilot with nothing at all', () => {
    expect(kd(0, 0, 0)).toBe('0.00')
  })
})

describe('sl', () => {
  it('is landings over sorties', () => {
    expect(sl(4, 3)).toBe('0.75')
  })
  it('is unknown rather than zero with no sorties', () => {
    expect(sl(0, 0)).toBe('—')
  })
})

describe('fmtHours', () => {
  it('pads the minutes', () => {
    expect(fmtHours(3.0833)).toBe('3h 05m')
  })
  // Rounding to the minute BEFORE splitting is what stops "1h 60m".
  it('carries 59.7 minutes into the next hour', () => {
    expect(fmtHours(1.9995)).toBe('2h 00m')
  })
  it('handles zero', () => {
    expect(fmtHours(0)).toBe('0h 00m')
  })
})

describe('fmtDuration', () => {
  it('drops the hour component under an hour', () => {
    expect(fmtDuration(420)).toBe('7m')
  })
  it('shows hours and minutes above an hour', () => {
    expect(fmtDuration(7620)).toBe('2h 7m')
  })
  it('treats non-positive as unknown', () => {
    expect(fmtDuration(0)).toBe('—')
    expect(fmtDuration(-5)).toBe('—')
  })
})

describe('fmtTimeZ', () => {
  // The regression this exists for: the old version formatted the viewer's
  // local clock and appended "Z" regardless, so a UTC+3 viewer read every
  // timestamp three hours late.
  it('renders UTC regardless of the host timezone', () => {
    expect(fmtTimeZ('2026-09-12T23:03:59Z')).toBe('23:03Z')
  })
  it('does not roll the day forward for a late-evening UTC time', () => {
    expect(fmtTimeZ('2026-09-12T22:26:54.284Z')).toBe('22:26Z')
  })
})

describe('ago', () => {
  const now = Date.parse('2026-09-13T12:00:00Z')
  it('counts minutes under an hour', () => {
    expect(ago('2026-09-13T11:30:00Z', now)).toBe('30m ago')
  })
  it('counts hours under a day', () => {
    expect(ago('2026-09-13T03:00:00Z', now)).toBe('9h ago')
  })
  it('counts days beyond that', () => {
    expect(ago('2026-09-10T12:00:00Z', now)).toBe('3d ago')
  })
  it('never goes negative for a clock skewed into the future', () => {
    expect(ago('2026-09-13T12:05:00Z', now)).toBe('0m ago')
  })
})

describe('windDir', () => {
  it('maps the cardinals', () => {
    expect(windDir(0)).toBe('N')
    expect(windDir(90)).toBe('E')
    expect(windDir(180)).toBe('S')
    expect(windDir(270)).toBe('W')
  })
  it('rounds to the nearest 45 degrees', () => {
    expect(windDir(220)).toBe('SW')
  })
  it('wraps past 360 instead of falling off the array', () => {
    expect(windDir(360)).toBe('N')
    expect(windDir(350)).toBe('N')
  })
})

describe('visStr', () => {
  it('treats the DCS unlimited sentinel as 10KM+', () => {
    expect(visStr(9999)).toBe('10KM+')
    expect(visStr(80000)).toBe('10KM+')
  })
  it('renders kilometres to one decimal', () => {
    expect(visStr(4500)).toBe('4.5KM')
  })
  it('is unknown for null/zero', () => {
    expect(visStr(null)).toBe('—')
    expect(visStr(0)).toBe('—')
  })
})

describe('cloudStr', () => {
  it('converts metres to feet rounded to the nearest hundred', () => {
    expect(cloudStr(1800)).toBe('5,900FT')
  })
  it('calls a zero base clear', () => {
    expect(cloudStr(0)).toBe('CLEAR')
  })
})

describe('classifyTargetClass', () => {
  it('recognises fixed wing', () => {
    expect(classifyTargetClass('FA-18C_hornet')).toBe('AIR')
    expect(classifyTargetClass('MiG-29S')).toBe('AIR')
  })
  it('recognises armour', () => {
    expect(classifyTargetClass('T-72B')).toBe('ARMOR')
    expect(classifyTargetClass('BMP-2')).toBe('ARMOR')
  })
  it('recognises air defence', () => {
    expect(classifyTargetClass('S-300PS 40B6M tr')).toBe('AD')
    expect(classifyTargetClass('ZSU-23-4 Shilka')).toBe('AD')
  })
  // The ordering bug this guards: "SA-" prefixes both the SA-342 Gazelle and
  // the SA-10 battery, so a naive /sa-\d/ sweep classified the helicopter as
  // an air-defence site.
  it('calls the SA-342 Gazelle a helicopter, not a SAM', () => {
    expect(classifyTargetClass('SA342M')).toBe('HELO')
  })
  it('still calls SA-10 air defence', () => {
    expect(classifyTargetClass('SA-10 site')).toBe('AD')
  })
  it('recognises ships and infantry', () => {
    expect(classifyTargetClass('CVN_75 carrier')).toBe('NAVAL')
    expect(classifyTargetClass('Infantry AK')).toBe('INF')
  })
  it('falls back to ground for anything unknown or empty', () => {
    expect(classifyTargetClass('Some Unmapped Thing')).toBe('GND')
    expect(classifyTargetClass(null)).toBe('GND')
    expect(classifyTargetClass('')).toBe('GND')
  })
})
