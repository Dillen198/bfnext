/**
 * The carrier's flight deck, drawn in the frame the groove tracker grades
 * in, so the ship, its wires and the pilot's track line up to the metre.
 *
 * The plans are `shared/carriers/decks.json`, the same file bfdb's result
 * cards draw from (`bfdb/src/range/deckplan.rs`): traced from scale plans
 * and overhead photographs (sources in the file) for the Nimitz, Forrestal,
 * Admiral Kuznetsov, Charles de Gaulle and the Tarawa-class LHA. The grading
 * block in it is the engine's (`bfprotocols` `CarrierKind::deck`, from MOOSE
 * AIRBOSS): the landing point is the target wire measured from the stern
 * along the landing area, and the landing-area centreline crosses the stern
 * `stern_offset_m` to starboard of the keel. A bfdb test keeps the two equal.
 *
 * Two frames:
 *  - ship: f = metres forward of the stern, r = metres right of the keel
 *  - landing (the record's): x = metres AFT of the landing point along the
 *    final bearing, y = metres right of the landing-area centreline
 */
import decks from '../../../shared/carriers/decks.json'

export type DeckKind = 'nimitz' | 'forrestal' | 'kuznetsov' | 'cdg' | 'tarawa'

type P = [number, number]

export interface DeckGeom {
  kind: DeckKind
  name: string
  /** landing area angle to the ship's axis, degrees, negative = to port */
  angle_deg: number
  /** the stern, metres from the ship's reference point (what Tacview and DCS report as its position); negative = aft */
  stern_m: number
  /** landing-area centreline at the stern, metres right of the keel */
  stern_offset_m: number
  /** flight deck height above the water, metres */
  deck_height_m: number
  /** wires, metres from the stern along the landing area (1-wire first); empty = no wires */
  wires_m: number[]
  /** the wire the landing point is on, 1-based (0 = no wires) */
  target_wire: number
  glideslope_deg: number
  length_m: number
  draft_m: number
  /** flight deck outline, ship frame */
  outline: P[]
  /** the hull at the waterline */
  waterline: P[]
  /** the island, ship frame, and its upper block */
  island: P[]
  island_top: P[]
  elevators: P[][]
  /** how far the landing area runs forward of the stern, metres */
  landing_len_m: number
  /** half the painted landing-area width, metres */
  landing_half_w_m: number
  /** the optical landing system (lens) and the LSO platform, ship frame */
  lens: P | null
  lso: P | null
  /** catapult tracks, ship frame start/end */
  cats: [P, P][]
  /** ski-jump take-off runs */
  runs: [P, P][]
  /** helicopter / VTOL spots */
  spots: P[]
  ski_jump: P | null
  /** side elevation, (f, z above the waterline) */
  side: { hull: P[]; island: P[][]; near_elevators: P[]; silhouette: P[] }
}

interface RawClass {
  name: string
  match: string
  scale_of?: string
  scale_f?: number
  scale_r?: number
  length: number
  draft: number
  grading: { stern_m: number; stern_offset_m: number; deck_height_m: number; angle_deg: number; wires_m: number[]; target_wire: number; glideslope_deg: number }
  landing_len: number
  landing_half_w: number
  outline?: number[][]
  waterline?: number[][]
  island?: number[][]
  island_top?: number[][]
  elevators?: number[][][]
  cats?: number[][][]
  runs?: number[][][]
  spots?: number[][]
  lens?: number[] | null
  lso?: number[] | null
  ski_jump?: number[] | null
  side?: { hull?: number[][]; island?: number[][][]; near_elevators?: number[][]; silhouette?: number[][] }
}

const RAW = decks.classes as unknown as Record<DeckKind, RawClass>
const ORDER: DeckKind[] = ['forrestal', 'kuznetsov', 'cdg', 'tarawa', 'nimitz']

function build(kind: DeckKind): DeckGeom {
  const c = RAW[kind]
  const b = c.scale_of ? RAW[c.scale_of as DeckKind] : c
  const sf = c.scale_of ? c.scale_f ?? 1 : 1
  const sr = c.scale_of ? c.scale_r ?? 1 : 1
  const pt = (q: number[]): P => [q[0] * sf, q[1] * sr]
  const pts = (v?: number[][]) => (v ?? []).map(pt)
  const seg = (v?: number[][][]) => (v ?? []).map(s => [pt(s[0]), pt(s[1])] as [P, P])
  const g = c.grading
  return {
    kind,
    name: c.name,
    angle_deg: g.angle_deg,
    stern_m: g.stern_m,
    stern_offset_m: g.stern_offset_m,
    deck_height_m: g.deck_height_m,
    wires_m: g.wires_m.filter(w => w > 0),
    target_wire: g.target_wire,
    glideslope_deg: g.glideslope_deg,
    length_m: c.length,
    draft_m: c.draft,
    outline: pts(b.outline),
    waterline: pts(b.waterline),
    island: pts(b.island),
    island_top: pts(b.island_top),
    elevators: (b.elevators ?? []).map(e => e.map(pt)),
    landing_len_m: c.landing_len,
    landing_half_w_m: c.landing_half_w,
    lens: b.lens ? pt(b.lens) : null,
    lso: b.lso ? pt(b.lso) : null,
    cats: seg(b.cats),
    runs: seg(b.runs),
    spots: pts(b.spots),
    ski_jump: b.ski_jump ? pt(b.ski_jump) : null,
    side: {
      hull: (b.side?.hull ?? []).map(q => [q[0] * sf, q[1]] as P),
      island: (b.side?.island ?? []).map(v => v.map(q => [q[0] * sf, q[1]] as P)),
      near_elevators: (b.side?.near_elevators ?? []).map(q => [q[0] * sf, q[1] * sf] as P),
      silhouette: (b.side?.silhouette ?? []).map(q => [q[0] * sf, q[1]] as P),
    },
  }
}

export const DECKS: Record<DeckKind, DeckGeom> = {
  nimitz: build('nimitz'),
  forrestal: build('forrestal'),
  kuznetsov: build('kuznetsov'),
  cdg: build('cdg'),
  tarawa: build('tarawa'),
}

/** The deck for a DCS carrier type name ("CVN_73", "Stennis", "Forrestal", "KUZNECOW", "LHA_Tarawa", "CdG"...). */
export function deckFor(carrierType: string | null | undefined): DeckGeom {
  const t = (carrierType ?? '').toLowerCase()
  for (const k of ORDER) {
    if (RAW[k].match.split('|').some(m => m && t.includes(m.toLowerCase()))) return DECKS[k]
  }
  return DECKS.nimitz
}

const D2R = Math.PI / 180

/** The engine's landing point: the target wire (60 m when the deck has none). */
export function landingPointFromStern(g: DeckGeom): number {
  return g.target_wire > 0 ? g.wires_m[g.target_wire - 1] ?? 60 : 60
}

/** Ship frame (f fwd of stern, r right of keel) → landing frame (x aft, y right). */
export function shipToLanding(g: DeckGeom, f: number, r: number): P {
  const a = -g.angle_deg * D2R
  const fwd: P = [-Math.cos(a), Math.sin(a)]
  const right: P = [Math.sin(a), Math.cos(a)]
  const w3 = landingPointFromStern(g)
  // the keel at the stern sits stern_offset to port of the landing centreline
  const kx = w3 - g.stern_offset_m * right[0]
  const ky = 0 - g.stern_offset_m * right[1]
  return [kx + f * fwd[0] + r * right[0], ky + f * fwd[1] + r * right[1]]
}

/** Metres along the landing area from the stern → landing-frame x. */
export const alongLanding = (g: DeckGeom, d: number) => landingPointFromStern(g) - d

/** Everything a plot or the 3D view needs, already in the landing frame. */
export function deckInLanding(g: DeckGeom) {
  const tr = (p: P) => shipToLanding(g, p[0], p[1])
  const w3 = landingPointFromStern(g)
  return {
    outline: g.outline.map(tr),
    waterline: g.waterline.map(tr),
    island: g.island.map(tr),
    island_top: g.island_top.map(tr),
    elevators: g.elevators.map(e => e.map(tr)),
    lens: g.lens ? tr(g.lens) : null,
    lso: g.lso ? tr(g.lso) : null,
    cats: g.cats.map(([a, b]) => [tr(a), tr(b)] as [P, P]),
    runs: g.runs.map(([a, b]) => [tr(a), tr(b)] as [P, P]),
    spots: g.spots.map(tr),
    /** x of each wire, landing frame */
    wires: g.wires_m.map(d => w3 - d),
    /** the ramp (stern) and the forward end of the landing area, landing-frame x */
    ramp_x: w3,
    landing_end_x: w3 - g.landing_len_m,
    /** the bow, landing-frame x of the ship's forward-most point */
    bow_x: Math.min(...g.outline.map(p => tr(p)[0])),
    /** the keel extended aft: lateral position of the wake at landing-frame x */
    wakeY: (x: number) => {
      const s = tr([0, 0])
      return s[1] - (x - s[0]) * Math.tan(-g.angle_deg * D2R)
    },
  }
}

/**
 * The landing point from the ship's reference point, in the ship's axes:
 * metres forward (negative = aft) and right of it, as the engine works it
 * out (`bflib`'s `deck_now`: stern + landing-area direction × target wire).
 */
export function landingFromRef(g: DeckGeom): { fwd: number; right: number } {
  const a = g.angle_deg * D2R
  const w3 = landingPointFromStern(g)
  return { fwd: g.stern_m + w3 * Math.cos(a), right: g.stern_offset_m + w3 * Math.sin(a) }
}

/** Ship frame (f fwd of stern, r right of keel) → model frame (metres forward / right of the reference point). */
export const shipToModel = (g: DeckGeom, f: number, r: number): P => [g.stern_m + f, r]
