import { useEffect, useMemo, useRef, useState } from 'react'
import { api, connectTacmap, type TacFrame, type TacPicture, type AirTrack, type GroundContact } from '../api'
import { getTerrainFromReferencePoint, type Terrain } from './dcs/terrain'
import { type TacviewObject, type TacviewState, newTacviewState } from './tacview'
import { type Tag } from './tacview/record/objectProperty'

// peace-eye colours by `coalition`: "Enemies" -> Friend hue, "Allies" ->
// Hostile hue (its authors fly for red / "Enemies"). We keep that mapping so
// entity.ts / the map layer colour stops stay verbatim: a friendly contact is
// tagged "Enemies", a hostile one "Allies".
function coalitionOf(iff: AirTrack['iff'] | 'hostile'): string | undefined {
  return iff === 'friendly' ? 'Enemies' : iff === 'hostile' ? 'Allies' : undefined
}

function airTags(t: AirTrack): Tag[] {
  const tags: Tag[] = ['Air']
  tags.push(t.class === 'helo' ? 'Rotorcraft' : 'FixedWing')
  return tags
}
function groundTags(c: GroundContact): Tag[] {
  const tags: Tag[] = ['Ground']
  switch (c.class) {
    case 'airdefense': tags.push('AntiAircraft'); break
    case 'armor':      tags.push('Tank'); break
    case 'artillery':  tags.push('Vehicle'); break
    case 'infantry':   tags.push('Infantry'); break
    case 'naval':      tags.push('Sea', 'Warship'); break
    case 'airbase':    tags.push('Static'); break
    default:           tags.push('Vehicle'); break
  }
  return tags
}

const AIR_NAME: Record<AirTrack['class'], string> = {
  fighter: 'Fighter', bomber: 'Bomber', attack: 'Attacker', helo: 'Helicopter',
  awacs: 'AWACS', tanker: 'Tanker', transport: 'Transport', unknown: 'Aircraft',
}
const GND_NAME: Record<GroundContact['class'], string> = {
  armor: 'Armor', airdefense: 'SAM', artillery: 'Artillery', infantry: 'Infantry',
  airbase: 'Airbase', naval: 'Ship', unknown: 'Ground unit',
}

export interface ScopeFeed {
  state: TacviewState
  terrain: Terrain | undefined
  denied: boolean
  reason: 'login' | 'nocoalition' | null
  status: 'open' | 'closed' | 'error'
  /** engine threat range (metres) per remapped object id, for auto rings */
  threatRanges: Record<number, number>
  /** connected and receiving frames, but the picture has no bullseye and no
   *  air/ground contacts — usually the engine isn't publishing `query-tacmap`
   *  yet (old bflib.dll) or the coalition simply has nothing on sensors. */
  empty: boolean
}

/** Bridge `/ws/tacmap` -> the shape peace-eye's MainView expects. */
export function useScopeFeed(): ScopeFeed {
  const [picture, setPicture] = useState<TacPicture | null>(null)
  const [reason, setReason] = useState<'login' | 'nocoalition' | null>(null)
  const [status, setStatus] = useState<'open' | 'closed' | 'error'>('closed')
  // A campaign objective position — used to pick the theatre map when the
  // tactical picture itself has no positioned entity (no contacts, no
  // bullseye), so the scope still renders instead of hanging on the spinner.
  const [objSample, setObjSample] = useState<[number, number] | null>(null)
  const retry = useRef<number | null>(null)

  useEffect(() => {
    let cancelled = false
    let tries = 0
    const poll = () => {
      api.objectives()
        .then((os) => {
          if (cancelled) return
          const valid = os.filter((x) => x.lat !== 0 || x.lon !== 0)
          if (valid.length) {
            // Centroid of the campaign objectives — a stable theatre anchor.
            setObjSample([
              valid.reduce((s, o) => s + o.lat, 0) / valid.length,
              valid.reduce((s, o) => s + o.lon, 0) / valid.length,
            ])
          } else if (tries++ < 20) {
            // engine may not have published objectives yet on a fresh start
            window.setTimeout(poll, 5000)
          }
        })
        .catch(() => {
          if (!cancelled && tries++ < 20) window.setTimeout(poll, 5000)
        })
    }
    poll()
    return () => { cancelled = true }
  }, [])
  // Stable small integer ids: JSON round-trips the engine's u64 track ids
  // through an f64, and peace-eye keys objects by `number`. Assign our own.
  const idMap = useRef<Map<string, number>>(new Map())
  const nextId = useRef(1)

  useEffect(() => {
    let closed = false
    let cleanup: (() => void) | null = null
    const open = () => {
      if (closed) return
      cleanup = connectTacmap(
        (frame: TacFrame) => {
          setPicture(frame.picture)
          setReason(frame.picture ? null : frame.reason ?? null)
        },
        (s) => {
          setStatus(s)
          if ((s === 'closed' || s === 'error') && !closed) {
            if (retry.current) window.clearTimeout(retry.current)
            retry.current = window.setTimeout(open, 3000)
          }
        },
      )
    }
    open()
    return () => {
      closed = true
      if (retry.current) window.clearTimeout(retry.current)
      cleanup?.()
    }
  }, [])

  const remap = (key: string): number => {
    let id = idMap.current.get(key)
    if (id === undefined) {
      id = nextId.current++
      idMap.current.set(key, id)
    }
    return id
  }

  return useMemo<ScopeFeed>(() => {
    if (!picture) {
      return {
        state: newTacviewState(),
        terrain: undefined,
        denied: true,
        reason,
        status,
        threatRanges: {},
        empty: false,
      }
    }

    const empty =
      picture.bullseye.length === 0 &&
      picture.air.length === 0 &&
      picture.ground.length === 0

    // Reference point = the DCS map's centre (peace-eye's model: absolute =
    // reference + relative). Detect the theatre from any positioned entity,
    // falling back to a campaign objective so the scope renders even when the
    // tactical picture is empty.
    const anySample =
      picture.bullseye[0] ??
      picture.air[0] ??
      picture.ground[0] ??
      null
    const sampleLL: [number, number] | null = anySample
      ? [anySample.lat, anySample.lon]
      : objSample
    let terrain = sampleLL ? getTerrainFromReferencePoint(sampleLL[0], sampleLL[1]) : undefined
    if (!terrain && sampleLL) {
      // Fallback for theatres peace-eye doesn't ship (Kola, Sinai, …): a
      // generic transverse-mercator centred on the picture.
      terrain = {
        name: 'Theatre',
        center: sampleLL,
        airports: [],
        projection: {
          centralMeridian: sampleLL[1],
          falseEasting: 0,
          falseNorthing: 0,
          scaleFactor: 0.9996,
        },
      }
    }

    const refLat = terrain?.center[0] ?? 0
    const refLon = terrain?.center[1] ?? 0

    const objects: Record<number, TacviewObject> = {}
    const threatRanges: Record<number, number> = {}

    for (const t of picture.air) {
      const id = remap(`a${t.id}`)
      // Prefer the exact DCS type name so entity.ts's AircraftToSidcIcon
      // lookup can draw a real aircraft-specific symbol; fall back to the
      // coarse class label when the engine couldn't identify the type.
      const nm = t.unit_type ?? t.label ?? AIR_NAME[t.class]
      objects[id] = {
        estimatedSpeed: t.speed_kts,
        estimatedAltitudeRate: t.vspd_ms * 0.19685, // m/s -> kft/min
        coords: {
          latitude: t.lat - refLat,
          longitude: t.lon - refLon,
          altitude: t.alt_m,
          heading: t.heading,
        },
        name: nm,
        type: airTags(t),
        coalition: coalitionOf(t.iff),
        group: t.label ?? undefined,
        pilot: t.label ?? undefined,
      }
    }
    for (const c of picture.ground) {
      const id = remap(`g${c.id}`)
      objects[id] = {
        estimatedSpeed: 0,
        estimatedAltitudeRate: 0,
        coords: {
          latitude: c.lat - refLat,
          longitude: c.lon - refLon,
        },
        name: c.count > 1 ? `${c.count}× ${GND_NAME[c.class]}` : GND_NAME[c.class],
        type: groundTags(c),
        coalition: 'Allies', // enemy laydown -> hostile hue
        group: `${GND_NAME[c.class]} (${(c.confidence * 100).toFixed(0)}%)`,
      }
      if (c.threat_range_m != null) threatRanges[id] = c.threat_range_m
      else if (c.class === 'airdefense') threatRanges[id] = 12000
    }

    const bull = picture.bullseye
    const mkBull = (s: 'Blue' | 'Red'): TacviewObject | undefined => {
      const b = bull.find((x) => x.side === s)
      if (!b) return undefined
      return {
        estimatedSpeed: 0,
        estimatedAltitudeRate: 0,
        coords: { latitude: b.lat - refLat, longitude: b.lon - refLon },
        name: 'Bullseye',
        type: ['Bullseye'],
      }
    }

    const state: TacviewState = {
      header: undefined,
      globalProperties: {
        referenceTime: picture.time,
        referenceLatitude: refLat,
        referenceLongitude: refLon,
        title: terrain?.name,
      },
      objects,
      blueBullseye: mkBull('Blue'),
      redBullseye: mkBull('Red'),
    }

    return { state, terrain, denied: false, reason: null, status, threatRanges, empty }
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [picture, status, reason, objSample])
}
