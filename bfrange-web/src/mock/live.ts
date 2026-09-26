/**
 * Live range picture that moves with the wall clock: players fly orbits or
 * run-ins, tankers fly racetracks, the carrier steams into the wind.
 */
import { KT, NM, destination, fromLocal, norm360 } from '../lib/geo'
import { windOverDeck } from '../lib/wod'
import type { GeoPt, LivePlayer, LiveSpawn, LiveTanker, RangeLive } from '../types'
import { MOCK_QNH_HPA, mockAtmo, mockLayer } from './atmo'
import { SECTORS } from './sectors'
import { ARENAS, CARRIERS, MISSION_DATE, PADS, PILOTS, SLING_COURSES, STATIONS, TANKERS, THEATRE, pilotByName } from './world'

const r1 = (x: number) => Math.round(x * 10) / 10

/**
 * Position on a racetrack (two legs joined by semicircles, left turns) for
 * time `t` s. Local frame: u along the leg, w to its right.
 */
function racetrack(c: GeoPt, legDeg: number, legM: number, speedKts: number, t: number, phase = 0) {
  const r = 9000
  const half = legM / 2
  const per = 2 * legM + 2 * Math.PI * r
  let s = (((t * speedKts * KT + phase * per) % per) + per) % per
  let u: number, w: number, hdg: number
  if (s < legM) {
    u = -half + s; w = r; hdg = legDeg
  } else if ((s -= legM) < Math.PI * r) {
    const f = s / r
    u = half + r * Math.sin(f); w = r * Math.cos(f); hdg = legDeg - (f * 180) / Math.PI
  } else if ((s -= Math.PI * r) < legM) {
    u = half - s; w = -r; hdg = legDeg + 180
  } else {
    const f = (s - legM) / r
    u = -half - r * Math.sin(f); w = -r * Math.cos(f); hdg = legDeg + 180 - (f * 180) / Math.PI
  }
  const L = (legDeg * Math.PI) / 180
  const pos = fromLocal(c, u * Math.cos(L) - w * Math.sin(L), u * Math.sin(L) + w * Math.cos(L))
  return { pos, hdg: norm360(hdg) }
}

/** A circle of radius r around c. */
function orbit(c: GeoPt, r: number, speedKts: number, t: number, phase = 0, cw = true) {
  const w = (speedKts * KT) / r
  const ang = phase + (cw ? 1 : -1) * w * t
  const brg = (ang * 180) / Math.PI
  return { pos: destination(c, brg, r), hdg: norm360(brg + (cw ? 90 : -90)) }
}

export function buildLive(now: number, spawns: LiveSpawn[]): RangeLive {
  const t = now / 1000
  const cv = CARRIERS[0]
  const station = (id: string) => STATIONS.find(s => s.id === id)!

  const player = (
    name: string, unit_type: string, pos: { lat: number; lon: number }, alt_m: number,
    hdg: number, speed: number, in_air: boolean, activity: string | null,
  ): LivePlayer => {
    const p = pilotByName(name)
    return {
      ucid: p.ucid, name, unit_type, callsign: p.callsign, side: 'blue',
      pos: { lat: pos.lat, lon: pos.lon, alt_m }, heading_deg: hdg, speed_kts: speed, in_air, activity,
    }
  }

  const tex = racetrack(TANKERS[0].centre, TANKERS[0].leg_deg, TANKERS[0].leg_m, TANKERS[0].speed_kts, t, 0.1)
  const casperPat = orbit({ ...destination(cv.pos, cv.brc_deg + 180, 1400), alt_m: 0 }, 2200, 150, t, 0.3, false)
  const sprocket = orbit(station('range_a_circle').pos, 7000, 420, t, 1.2)
  const dutch = orbit(station('range_a_strafe_1').pos, 5200, 300, t, 2.1)
  const holly = orbit(ARENAS[0].pos, 2500, 360, t, 0.2)
  const nitro = orbit(ARENAS[1].pos, 30000, 480, t, 3.8)
  const tuna = orbit(SLING_COURSES[0].dz, 1400, 60, t, 0.6)
  const kestrel = orbit(STATIONS.find(s => s.id === 'gun_lane_1')!.pos, 1800, 90, t, 1.5)

  const players: LivePlayer[] = [
    player('Casper', 'T-45', casperPat.pos, 180, casperPat.hdg, 150, true, `${cv.name} pattern`),
    player('Viper', 'F-16C_50', destination(tex.pos, tex.hdg + 180, 40), 6700, tex.hdg, 275, true, 'AAR Texaco 1-1'),
    player('Sprocket', 'FA-18C_hornet', sprocket.pos, 3600, sprocket.hdg, 420, true, 'Range A — Bomb Circle'),
    player('Dutch', 'A-10C_2', dutch.pos, 900, dutch.hdg, 300, true, 'Range A — Strafe Pit 1'),
    player('Hollywood', 'F-14B', holly.pos, 4500, holly.hdg, 360, true, 'BFM Box'),
    player('Nitro', 'FA-18C_hornet', nitro.pos, 8200, nitro.hdg, 480, true, 'BVR Lane: duel vs Viper'),
    player('Tuna', 'UH-1H', tuna.pos, 90, tuna.hdg, 60, true, 'Sling Course 1'),
    player('Kestrel', 'AH-64D_BLK_II', kestrel.pos, 120, kestrel.hdg, 90, true, 'Gunnery Lane 1'),
    player('Rook', 'AV8BNA', PADS[0].pos, 18, 70, 0, false, null),
  ]

  const tankers: LiveTanker[] = TANKERS.map((tk, i) => {
    const r = racetrack(tk.centre, tk.leg_deg, tk.leg_m, tk.speed_kts, t, i * 0.23)
    const state: LiveTanker['state'] = tk.id === 'mauler' ? 'spawning' : tk.id === 'il78' ? 'rtb' : 'on_station'
    return {
      id: tk.id,
      callsign: tk.callsign,
      unit_type: tk.unit_type,
      method: tk.method,
      pos: { lat: r.pos.lat, lon: r.pos.lon, alt_m: tk.alt_ft * 0.3048 },
      heading_deg: Math.round(r.hdg),
      speed_kts: tk.speed_kts,
      alt_ft: tk.alt_ft,
      tacan: tk.tacan,
      freq_mhz: tk.freq_mhz,
      state,
      receivers: tk.id === 'tex11' ? ['Viper'] : tk.id === 'arc2' ? ['Casper'] : [],
      owner: tk.id === 'mauler' ? 'Sprocket' : null,
      recovery_for: tk.recovery_for ?? null,
    }
  })

  // the boat steams slowly along its BRC, wrapping every hour
  const cvDrift = ((t % 3600) / 3600) * cv.speed_kts * KT * 3600 * 0.1
  const cvPos = destination(cv.pos, cv.brc_deg, cvDrift)
  const minute = Math.floor((t / 60) % 60)
  const open = minute % 30 < 20

  // DCS's atmosphere over the range reference point (Range A)
  const ref = STATIONS[0].pos
  const refAtmo = mockAtmo(ref.alt_m, t)
  const at2000 = mockLayer(2000, t)

  return {
    server_time: new Date(now).toISOString(),
    theatre: THEATRE,
    mission_time: new Date(now + 3 * 3600_000).toISOString().slice(11, 19),
    mission_date: MISSION_DATE,
    night: false,
    wind: {
      layers: refAtmo,
      surface_from_deg: refAtmo[0].wind_from_deg,
      surface_kts: refAtmo[0].wind_kts,
      alt_from_deg: at2000.wind_from_deg,
      alt_kts: at2000.wind_kts,
      temperature_c: refAtmo[0].temp_c,
      qnh_hpa: MOCK_QNH_HPA,
    },
    players,
    stations: STATIONS.map(s => ({
      id: s.id,
      name: s.name,
      kind: s.kind,
      pos: s.pos,
      hot_by:
        s.id === 'range_a_circle' ? ['Sprocket']
        : s.id === 'range_a_strafe_1' ? ['Dutch']
        : s.id === 'gun_lane_1' ? ['Kestrel']
        : [],
      targets_alive: s.id === 'range_b_array' ? 7 : s.id === 'range_b_convoy' ? 4 : s.id === 'gun_lane_1' ? 5 : s.targets,
      targets_total: s.targets,
      laser_code: s.laser_code ?? null,
      rings_m: s.rings_m,
      note: s.note ?? null,
      elev_m: s.pos.alt_m,
      atmo: mockAtmo(s.pos.alt_m, t),
    })),
    tankers,
    carriers: CARRIERS.map((c, i) => {
      // the true wind at the anemometer, and what the deck makes of it
      const tw = mockLayer(c.deck_height_m, t)
      const wod = windOverDeck({
        ship_heading_deg: c.brc_deg, ship_speed_kts: c.speed_kts,
        wind_from_deg: tw.wind_from_deg, wind_kts: tw.wind_kts, deck_angle_deg: c.deck_angle_deg,
      })
      return {
        id: c.id,
        name: c.name,
        unit_type: c.unit_type,
        pos: i === 0 ? { lat: cvPos.lat, lon: cvPos.lon, alt_m: 0 } : c.pos,
        brc_deg: c.brc_deg,
        fb_deg: norm360(c.brc_deg + c.deck_angle_deg),
        speed_kts: c.speed_kts,
        wind_over_deck_kts: r1(wod.wod_kts),
        wind_over_deck_angle_deg: r1(wod.rel_deck_deg),
        recovery_open: i === 0 ? open : true,
        next_window: i === 0 ? (open ? `closes ${String((Math.floor(minute / 30) * 30 + 20) % 60).padStart(2, '0')} past` : `opens ${String((Math.floor(minute / 30) * 30 + 30) % 60).padStart(2, '0')} past`) : null,
        tacan: c.tacan,
        icls: c.icls,
        link4_mhz: c.link4_mhz,
        tower_mhz: c.tower_mhz,
        recovery_tanker: i === 0 ? 'Arco 2 (A-6E)' : null,
        pattern: i === 0 ? ['Casper', 'Blackjack'] : [],
        case: 1,
        true_wind_from_deg: tw.wind_from_deg,
        true_wind_kts: tw.wind_kts,
        deck_angle_deg: c.deck_angle_deg,
      }
    }),
    spawns,
    arenas: ARENAS.map(a => ({
      id: a.id,
      name: a.name,
      pos: a.pos,
      radius_m: a.radius_m,
      occupants: a.id === 'bfm' ? ['Hollywood'] : ['Nitro', 'Viper'],
      status: a.id === 'bfm' ? 'open' : 'duel: Nitro vs Viper',
    })),
    // a fresh copy each poll, as the real API sends
    sectors: structuredClone(SECTORS),
    uptime_s: (t % 14_400) + 600,
  }
}

/** Where a catalogue item appears when spawned (near the station or the player). */
export function spawnPosition(item: string, near: GeoPt): GeoPt {
  const station = STATIONS.find(s => item.includes('armor') ? s.id === 'range_b_array' : item.includes('ship') ? s.id === 'ship_box' : item.includes('opfor') ? s.id === 'gun_lane_1' : false)
  if (station) return station.pos
  if (item.includes('sling')) return SLING_COURSES[0].dz
  const p = destination(near, 60, 25 * NM)
  return { lat: p.lat, lon: p.lon, alt_m: 6000 }
}

export const MOCK_ME = PILOTS[0]
