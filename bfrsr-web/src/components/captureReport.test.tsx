import { describe, expect, it, vi } from 'vitest'
import { renderToStaticMarkup } from 'react-dom/server'
import type { CaptureReport, CaptureTarget } from '../api'
import { captureHeading, locatedTargets, targetBounds, targetLine } from './captureReport'

// maplibre needs WebGL; the panel's list is what is under test here.
vi.mock('react-map-gl/maplibre', () => ({
  default: ({ children }: { children?: unknown }) => <div data-testid="map">{children as never}</div>,
  Marker: ({ children }: { children?: unknown }) => <>{children as never}</>,
}))
vi.mock('maplibre-gl/dist/maplibre-gl.css', () => ({}))
vi.mock('@shared/web/theme', () => ({ useTheme: () => ({ theme: 'dark', toggle: () => {} }) }))

const { default: CaptureBlockers } = await import('./CaptureBlockers')

const unit = (over: Partial<CaptureTarget> = {}): CaptureTarget => ({
  kind: 'unit', label: 'T-72B', class: 'armor', count: 1, infantry: false,
  lat: 33.5, lon: 36.3, grid: '37S BA 12345 67890', ...over,
})

const report = (over: Partial<CaptureReport> = {}): CaptureReport => ({
  perspective: 'attacker', open: false, summary: 'Not capturable yet.',
  health_threshold: 20, kills_needed: 0, infantry_left: 0,
  blockers: [], targets: [], unlocated: 0, troops: [], notes: [], ...over,
})

describe('capture report helpers', () => {
  it('heads the panel from the viewer’s side', () => {
    expect(captureHeading(report())).toBe("Why can't this be captured?")
    expect(captureHeading(report({ perspective: 'defender' }))).toBe('What the enemy needs to capture it')
    expect(captureHeading(report({ open: true }))).toBe('Capturable now')
    expect(captureHeading(report({ in_progress: { side: 'Red', held_secs: 10, needed_secs: 180 } })))
      .toBe('Capture in progress')
  })

  it('words located and unlocated targets', () => {
    expect(targetLine(unit())).toEqual({ what: 'T-72B (armor)', where: '37S BA 12345 67890' })
    expect(targetLine(unit({ label: null, count: 3, class: 'infantry', lat: null, lon: null, grid: null })))
      .toEqual({ what: '3 infantry units', where: 'position unknown' })
    expect(targetLine(unit({ grid: null })).where).toBe('33.5000, 36.3000')
  })

  it('maps only what has a position, padded', () => {
    const r = report({ targets: [unit(), unit({ lat: null, lon: null }), unit({ lat: 33.6, lon: 36.1 })] })
    expect(locatedTargets(r).map(x => x.i)).toEqual([0, 2])
    expect(targetBounds([])).toBeNull()
    const b = targetBounds([{ lat: 33.5, lon: 36.3 }, { lat: 33.6, lon: 36.1 }], 0.01)!
    expect(b[0][0]).toBeCloseTo(36.09); expect(b[0][1]).toBeCloseTo(33.49)
    expect(b[1][0]).toBeCloseTo(36.31); expect(b[1][1]).toBeCloseTo(33.61)
  })
})

describe('CaptureBlockers panel', () => {
  it('names the dead end and tells players to report it', () => {
    const html = renderToStaticMarkup(
      <CaptureBlockers objectiveName="HJ04" report={report({
        summary: 'Can’t be captured: nothing here counts toward its health.',
        blockers: [{ kind: 'nothing_to_destroy', text: 'Nothing here counts toward its health.', action: '' }],
      })} />,
    )
    expect(html).toContain('Nothing here counts toward its health.')
    expect(html).toContain('Report it to the admins')
    expect(html).toContain('HJ04')
    expect(html).not.toContain('→')
    expect(html).not.toContain('data-testid="map"')
  })

  it('lists blockers with their actions, targets with grids, and draws the map', () => {
    const html = renderToStaticMarkup(
      <CaptureBlockers objectiveName="Hama" report={report({
        kills_needed: 4, infantry_left: 1, unlocated: 2,
        blockers: [
          { kind: 'health', text: 'Health is 60%.', action: 'Destroy 4 more garrison units.' },
          { kind: 'infantry', text: '1 infantry defender is still alive.', action: 'Hunt down the infantry.' },
        ],
        targets: [
          unit({ label: 'Soldier AK', class: 'infantry', infantry: true }),
          unit(),
          unit({ label: null, count: 2, class: 'ship', lat: null, lon: null, grid: null }),
        ],
        notes: ['2 defenders are not on your intel picture.'],
      })} />,
    )
    expect(html).toContain('→ Destroy 4 more garrison units.')
    expect(html).toContain('Soldier AK (infantry)')
    expect(html).toContain('37S BA 12345 67890')
    expect(html).toContain('2 ship units')
    expect(html).toContain('position unknown')
    expect(html).toContain('Still standing (4)')
    expect(html).toContain('data-testid="map"')
    expect(html).toContain('not on your intel picture')
  })
})
