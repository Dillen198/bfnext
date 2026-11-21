/**
 * The weather briefing, from the live picture: the wind, the sky
 * (`SkyInfo`) and the altimeter, decoded for the page and written up as a
 * METAR-style line. DCS has no dew point, so the line carries the
 * temperature alone (`26/`), as a station with a failed dew point sensor
 * reports it.
 */
import type { RangeLive, SkyInfo } from '../types'
import type { MissionClock } from './missionClock'

const FT = 3.28084
const pad = (n: number, w: number) => String(Math.round(n)).padStart(w, '0')

export type FlightCategory = 'VFR' | 'MVFR' | 'IFR' | 'LIFR'

export const CATEGORY_COLOR: Record<FlightCategory, string> = {
  VFR: 'var(--datum)',
  MVFR: 'var(--sky)',
  IFR: 'var(--wave)',
  LIFR: '#d36bff',
}

const COVER_WORD: Record<string, string> = { FEW: 'Few', SCT: 'Scattered', BKN: 'Broken', OVC: 'Overcast' }

/** Cloud base above the range's ground, feet; null with no cloud. */
export function baseAglFt(sky: SkyInfo): number | null {
  return sky.cover && sky.base_m != null ? Math.max(0, (sky.base_m - sky.ground_m) * FT) : null
}

/** The ceiling: the base of a broken or overcast layer, feet AGL; null = no ceiling. */
export function ceilingFt(sky: SkyInfo): number | null {
  return sky.cover === 'BKN' || sky.cover === 'OVC' ? baseAglFt(sky) : null
}

export function flightCategory(sky: SkyInfo): FlightCategory {
  const c = ceilingFt(sky) ?? Infinity
  const v = sky.visibility_m
  if (c < 500 || v < 1600) return 'LIFR'
  if (c < 1000 || v < 4800) return 'IFR'
  if (c <= 3000 || v <= 8000) return 'MVFR'
  return 'VFR'
}

export function visibilityText(m: number): string {
  if (m >= 10_000) return '10 km+'
  if (m >= 5000) return `${Math.round(m / 1000)} km`
  return `${(m / 1000).toFixed(1)} km`
}

/** "Scattered 4,500 ft" ("SCT 4,500 ft" short) / "Clear". */
export function cloudText(sky: SkyInfo, short = false): string {
  const b = baseAglFt(sky)
  if (!sky.cover || b == null) return 'Clear'
  return `${short ? sky.cover : COVER_WORD[sky.cover] ?? sky.cover} ${(Math.round(b / 100) * 100).toLocaleString('en-US')} ft`
}

/** The present weather: precipitation, fog, mist, dust; null for none. */
export function presentWeather(sky: SkyInfo): { code: string; text: string } | null {
  const parts: { code: string; text: string }[] = []
  switch (sky.precip) {
    case 'rain': parts.push({ code: 'RA', text: 'rain' }); break
    case 'thunderstorm': parts.push({ code: 'TSRA', text: 'thunderstorms' }); break
    case 'snow': parts.push({ code: 'SN', text: 'snow' }); break
    case 'snowstorm': parts.push({ code: '+SN', text: 'heavy snow' }); break
  }
  if (sky.fog_visibility_m != null) {
    parts.push(sky.fog_visibility_m < 1000 ? { code: 'FG', text: 'fog' } : { code: 'BR', text: 'mist' })
  }
  if (sky.dust_visibility_m != null) parts.push({ code: 'DU', text: 'dust' })
  if (!parts.length) return null
  return { code: parts.map(p => p.code).join(' '), text: parts.map(p => p.text).join(', ') }
}

/**
 * `RNG 011507Z 24507KT 9999 SCT045 26/ Q1013 A2992`: CAVOK when it is, the
 * time group only when the theatre's Zulu offset is known.
 */
export function metar(live: RangeLive, clock: MissionClock | null): string {
  const w = live.wind
  const sky = live.sky
  const out = ['RNG']
  if (clock?.zulu && clock.zuluDay != null) out.push(`${pad(clock.zuluDay, 2)}${clock.zulu.slice(0, 2)}${clock.zulu.slice(3, 5)}Z`)
  const kt = Math.round(w.surface_kts)
  out.push(kt < 1 ? '00000KT' : `${pad(((Math.round(w.surface_from_deg / 10) * 10) % 360) || 360, 3)}${pad(kt, 2)}KT`)
  if (sky) {
    const wx = presentWeather(sky)
    const base = baseAglFt(sky)
    const cavok = sky.visibility_m >= 10_000 && !wx && (base == null || base >= 5000)
    if (cavok) out.push('CAVOK')
    else {
      out.push(sky.visibility_m >= 9999 ? '9999' : pad(Math.floor(sky.visibility_m / 100) * 100, 4))
      if (wx) out.push(wx.code)
      out.push(sky.cover && base != null ? `${sky.cover}${pad(base / 100, 3)}${sky.precip === 'thunderstorm' ? 'CB' : ''}` : 'NSC')
    }
  }
  const t = Math.round(w.temperature_c)
  out.push(`${t < 0 ? 'M' : ''}${pad(Math.abs(t), 2)}/`)
  out.push(`Q${pad(w.qnh_hpa, 4)}`, `A${pad(w.qnh_hpa * 2.953, 4)}`)
  return out.join(' ')
}

/** The winds at a few heights for the briefing, from the atmosphere profile. */
export function windsAloft(live: RangeLive, feet = [2000, 5000, 10_000, 20_000]) {
  const layers = live.wind.layers ?? []
  if (!layers.length) return []
  return feet.map(ft => {
    const m = ft / FT
    const l = layers.reduce((a, b) => (Math.abs(b.alt_m - m) < Math.abs(a.alt_m - m) ? b : a))
    return { ft, from: l.wind_from_deg, kts: l.wind_kts, temp: l.temp_c, close: Math.abs(l.alt_m - m) < 700 }
  }).filter(x => x.close)
}
