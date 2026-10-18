/**
 * The 3D debrief's models, all ours: built from primitives to each type's
 * published length, span and height (no model files to license or load).
 * Every model faces +Y (forward), Z up, X to starboard, origin at the
 * middle of the aircraft (for a ship: its reference point, the position DCS
 * and Tacview report), sized in metres.
 *
 * Aircraft come from one parametric builder (fuselage of revolution, wing
 * and tail planforms, canopy, twin or single fins, engine pods, rotors), so
 * a new type is a row in AIR, not new geometry.
 */
import * as THREE from 'three'

// ─── materials ─────────────────────────────────────────────────────────────

const mat = (color: string, opts: Partial<THREE.MeshStandardMaterialParameters> = {}) =>
  new THREE.MeshStandardMaterial({ color, roughness: 0.7, metalness: 0.15, flatShading: true, ...opts })

export const MATS = {
  navy: mat('#8e99a4'),
  navyDark: mat('#5e6872'),
  blue: mat('#7f93a8'),
  red: mat('#a88a7f'),
  self: mat('#c7cdd3'),
  glass: mat('#1a2633', { roughness: 0.15, metalness: 0.6 }),
  weapon: mat('#d9d4c4'),
  threat: mat('#e0574d'),
  hull: mat('#6b737b'),
  hullLow: mat('#3d1f1f'),
  island: mat('#7b848d'),
  olive: mat('#5b6142'),
  sand: mat('#8a7d5c'),
  rotor: new THREE.MeshBasicMaterial({ color: '#202830', transparent: true, opacity: 0.35, side: THREE.DoubleSide, depthWrite: false }),
  lensAmber: new THREE.MeshBasicMaterial({ color: '#ffb23e' }),
  lensGreen: new THREE.MeshBasicMaterial({ color: '#38d77c' }),
}

// ─── aircraft ──────────────────────────────────────────────────────────────

type Plan = 'swept' | 'delta' | 'straight' | 'heavy'
interface AirSpec {
  /** length, span, height, metres */
  L: number
  span: number
  H: number
  plan: Plan
  /** 1, 2 fins; cant degrees for twin fins */
  fins?: 1 | 2
  cant?: number
  /** fuselage max radius as a share of length */
  r?: number
  canards?: boolean
  /** engine pods: 'a10' (rear top), 'wing2', 'wing4' */
  pods?: 'a10' | 'wing2' | 'wing4'
  dish?: boolean
  /** helicopter: main rotor diameter; tandem / coaxial */
  rotor?: number
  rotors?: 'single' | 'tandem' | 'coax'
}

/** DCS type names → published dimensions. First match wins. */
const AIR: [RegExp, AirSpec][] = [
  [/FA-18|F\/A-18|Hornet/i, { L: 17.1, span: 12.3, H: 4.7, plan: 'swept', fins: 2, cant: 20 }],
  [/F-14/i, { L: 19.1, span: 16.0, H: 4.9, plan: 'swept', fins: 2, cant: 5, r: 0.07 }],
  [/T-45/i, { L: 11.99, span: 9.39, H: 4.08, plan: 'swept', fins: 1 }],
  [/F-16/i, { L: 15.06, span: 9.96, H: 5.09, plan: 'swept', fins: 1 }],
  [/F-15/i, { L: 19.43, span: 13.05, H: 5.63, plan: 'swept', fins: 2, cant: 0, r: 0.07 }],
  [/F-22/i, { L: 18.9, span: 13.56, H: 5.08, plan: 'swept', fins: 2, cant: 27 }],
  [/F-35/i, { L: 15.7, span: 10.7, H: 4.33, plan: 'swept', fins: 2, cant: 25 }],
  [/F-4/i, { L: 19.2, span: 11.7, H: 5.0, plan: 'swept', fins: 1 }],
  [/F-5/i, { L: 14.45, span: 8.13, H: 4.08, plan: 'swept', fins: 1, r: 0.05 }],
  [/A-10/i, { L: 16.26, span: 17.53, H: 4.47, plan: 'straight', fins: 2, cant: 0, pods: 'a10' }],
  [/AV8|AV-8|Harrier/i, { L: 14.12, span: 9.25, H: 3.55, plan: 'swept', fins: 1, r: 0.08 }],
  [/Su-33|Su-27|Su-30|J-11|J-15/i, { L: 21.9, span: 14.7, H: 5.92, plan: 'swept', fins: 2, cant: 0 }],
  [/Su-25/i, { L: 15.53, span: 14.36, H: 4.8, plan: 'straight', fins: 1 }],
  [/MiG-29/i, { L: 17.3, span: 11.4, H: 4.73, plan: 'swept', fins: 2, cant: 6 }],
  [/MiG-21/i, { L: 14.5, span: 7.15, H: 4.13, plan: 'delta', fins: 1, r: 0.05 }],
  [/Rafale/i, { L: 15.27, span: 10.9, H: 5.34, plan: 'delta', fins: 1, canards: true }],
  [/M-?2000|Mirage/i, { L: 14.36, span: 9.13, H: 5.2, plan: 'delta', fins: 1 }],
  [/JF-17/i, { L: 14.93, span: 9.45, H: 4.77, plan: 'swept', fins: 1 }],
  [/L-39/i, { L: 12.13, span: 9.46, H: 4.77, plan: 'straight', fins: 1, r: 0.07 }],
  [/S-3|Viking/i, { L: 16.26, span: 20.93, H: 6.93, plan: 'heavy', fins: 1, pods: 'wing2', r: 0.08 }],
  [/E-2|Hawkeye/i, { L: 17.6, span: 24.6, H: 5.6, plan: 'heavy', fins: 2, cant: 0, pods: 'wing2', dish: true, r: 0.07 }],
  [/KC-?130|C-130/i, { L: 29.8, span: 40.4, H: 11.6, plan: 'heavy', fins: 1, pods: 'wing4' }],
  [/KC-?135|KC135|Boeing|E-3|AWACS/i, { L: 41.5, span: 39.9, H: 12.7, plan: 'heavy', fins: 1, pods: 'wing4', r: 0.045 }],
  [/IL-78|Il-76/i, { L: 46.6, span: 50.5, H: 14.8, plan: 'heavy', fins: 1, pods: 'wing4' }],
  // helicopters: L = fuselage length
  [/CH-47|Chinook/i, { L: 15.9, span: 3.8, H: 5.7, plan: 'straight', rotor: 18.3, rotors: 'tandem' }],
  [/AH-64/i, { L: 15.5, span: 5.2, H: 4.7, plan: 'straight', rotor: 14.6 }],
  [/AH-1/i, { L: 13.6, span: 3.3, H: 4.1, plan: 'straight', rotor: 14.6 }],
  [/UH-60|SH-60|MH-60/i, { L: 15.3, span: 3.0, H: 5.1, plan: 'straight', rotor: 16.4 }],
  [/UH-1/i, { L: 12.7, span: 2.6, H: 4.4, plan: 'straight', rotor: 14.6 }],
  [/Mi-8|Mi-17/i, { L: 18.2, span: 3.0, H: 5.6, plan: 'straight', rotor: 21.3 }],
  [/Mi-24/i, { L: 17.5, span: 6.5, H: 6.5, plan: 'straight', rotor: 17.3 }],
  [/Ka-50|Ka-52/i, { L: 13.5, span: 7.3, H: 4.9, plan: 'straight', rotor: 14.5, rotors: 'coax' }],
  [/OH-58|Kiowa/i, { L: 10.5, span: 2.0, H: 3.9, plan: 'straight', rotor: 10.7 }],
  [/SA342|Gazelle/i, { L: 9.5, span: 2.0, H: 3.2, plan: 'straight', rotor: 10.5 }],
]
const FIGHTER: AirSpec = { L: 15.5, span: 10.5, H: 4.8, plan: 'swept', fins: 1 }
const HELO: AirSpec = { L: 13, span: 2.8, H: 4.2, plan: 'straight', rotor: 14.6 }

export function airSpec(name: string | undefined, helo: boolean): AirSpec {
  for (const [re, s] of AIR) if (re.test(name ?? '')) return s
  return helo ? HELO : FIGHTER
}

/** A fuselage of revolution along +Y, `L` long, `R` max radius. */
function fuselage(L: number, R: number, nose = 0.18): THREE.BufferGeometry {
  const h = L / 2
  const prof: [number, number][] = [
    [0.0, -h], [0.42 * R, -h], [0.7 * R, -h + 0.12 * L], [R, -h + 0.38 * L],
    [R, h - (nose + 0.18) * L], [0.82 * R, h - nose * L], [0.45 * R, h - nose * 0.45 * L], [0.04 * R, h],
  ]
  const g = new THREE.LatheGeometry(prof.map(([r, y]) => new THREE.Vector2(r, y)), 14)
  return g
}

/** A flat planform in the XY plane (x = span, y = forward), `t` thick, extruded up Z and centred on it. */
function planform(pts: [number, number][], t: number): THREE.BufferGeometry {
  const s = new THREE.Shape(pts.map(([x, y]) => new THREE.Vector2(x, y)))
  const g = new THREE.ExtrudeGeometry(s, { depth: t, bevelEnabled: false })
  g.translate(0, 0, -t / 2)
  return g
}

/** Both wings (or both tailplanes): half span `hs`, root chord `rc` with its LE at `yLE`, tip chord `tc`, LE sweep. */
function wings(rootX: number, hs: number, yLE: number, rc: number, tc: number, sweepDeg: number, t: number): THREE.BufferGeometry {
  const back = Math.tan((sweepDeg * Math.PI) / 180) * (hs - rootX)
  const right: [number, number][] = [[rootX, yLE], [hs, yLE - back], [hs, yLE - back - tc], [rootX, yLE - rc]]
  const left: [number, number][] = right.map(([x, y]) => [-x, y] as [number, number]).reverse()
  return planform([...right, ...left], t)
}

/** A fin in the YZ plane: root chord `rc` at height `z0`, LE at `yLE`, height `h`, tip chord `tc`. */
function fin(yLE: number, rc: number, tc: number, h: number, sweepDeg: number, t: number): THREE.BufferGeometry {
  const back = Math.tan((sweepDeg * Math.PI) / 180) * h
  const s = new THREE.Shape([
    new THREE.Vector2(yLE, 0), new THREE.Vector2(yLE - back, h), new THREE.Vector2(yLE - back - tc, h), new THREE.Vector2(yLE - rc, 0),
  ])
  const g = new THREE.ExtrudeGeometry(s, { depth: t, bevelEnabled: false })
  g.translate(0, 0, -t / 2)
  // shape x → forward (Y), shape y → up (Z), thickness → X
  g.rotateX(Math.PI / 2)
  g.rotateZ(Math.PI / 2)
  return g
}

function rotorDisc(d: number, blades: number): THREE.Group {
  const g = new THREE.Group()
  const disc = new THREE.Mesh(new THREE.CircleGeometry(d / 2, 40), MATS.rotor)
  g.add(disc)
  const hub = new THREE.Group()
  for (let i = 0; i < blades; i++) {
    const b = new THREE.Mesh(new THREE.BoxGeometry(0.45, d / 2, 0.06), MATS.navyDark)
    b.position.y = d / 4
    const arm = new THREE.Group()
    arm.rotation.z = (i / blades) * Math.PI * 2
    arm.add(b)
    hub.add(arm)
  }
  hub.userData.spin = 2 * Math.PI * 4.5
  g.add(hub)
  return g
}

/** Build an aircraft to its spec. `skin` colours the airframe. */
export function aircraft(name: string | undefined, helo: boolean, skin: THREE.Material): THREE.Group {
  const sp = airSpec(name, helo)
  const g = new THREE.Group()
  g.userData.length = sp.L
  const isHelo = !!sp.rotor
  const R = sp.L * (sp.r ?? (isHelo ? 0.11 : sp.plan === 'heavy' ? 0.055 : 0.06))
  const fus = new THREE.Mesh(fuselage(sp.L, R, isHelo ? 0.12 : 0.2), skin)
  g.add(fus)

  if (isHelo) {
    // tail boom over the back half, rotor mast on top
    const boom = new THREE.Mesh(new THREE.CylinderGeometry(R * 0.18, R * 0.32, sp.L * 0.55, 8), skin)
    boom.position.set(0, -sp.L * 0.45, R * 0.35)
    g.add(boom)
    const canopy = new THREE.Mesh(new THREE.SphereGeometry(1, 12, 8), MATS.glass)
    canopy.scale.set(R * 0.75, sp.L * 0.12, R * 0.6)
    canopy.position.set(0, sp.L * 0.3, R * 0.2)
    g.add(canopy)
    const top = sp.H - 0.4 - R
    const d = sp.rotor!
    if (sp.rotors === 'tandem') {
      const f = rotorDisc(d, 3); f.position.set(0, sp.L * 0.38, top); g.add(f)
      const a = rotorDisc(d, 3); a.position.set(0, -sp.L * 0.4, top + 0.9); g.add(a)
    } else if (sp.rotors === 'coax') {
      const a = rotorDisc(d, 3); a.position.set(0, 0, top); g.add(a)
      const b = rotorDisc(d, 3); b.position.set(0, 0, top - 1.1); g.add(b)
    } else {
      const m = rotorDisc(d, 4); m.position.set(0, sp.L * 0.02, top); g.add(m)
      const tr = rotorDisc(d * 0.19, 2)
      tr.rotation.y = Math.PI / 2
      tr.position.set(R * 0.3, -sp.L * 0.72, R * 0.8)
      g.add(tr)
    }
    const fin1 = new THREE.Mesh(fin(-sp.L * 0.62, sp.L * 0.12, sp.L * 0.07, sp.H * 0.35, 30, 0.12), skin)
    fin1.position.z = R * 0.3
    g.add(fin1)
    return g
  }

  const hs = sp.span / 2
  const t = Math.max(0.12, sp.L * 0.012)
  if (sp.plan === 'delta') {
    g.add(new THREE.Mesh(wings(R * 0.7, hs, sp.L * 0.12, sp.L * 0.55, sp.L * 0.03, 52, t), skin))
    if (sp.canards) {
      const c = new THREE.Mesh(wings(R * 0.7, hs * 0.4, sp.L * 0.3, sp.L * 0.1, sp.L * 0.04, 45, t * 0.7), skin)
      c.position.z = R * 0.35
      g.add(c)
    }
  } else if (sp.plan === 'straight') {
    g.add(new THREE.Mesh(wings(R * 0.7, hs, sp.L * 0.12, sp.L * 0.2, sp.L * 0.12, 5, t), skin))
    g.add(new THREE.Mesh(wings(R * 0.5, hs * 0.36, -sp.L * 0.36, sp.L * 0.12, sp.L * 0.08, 8, t * 0.8), skin))
  } else if (sp.plan === 'heavy') {
    const w = new THREE.Mesh(wings(R * 0.8, hs, sp.L * 0.12, sp.L * 0.2, sp.L * 0.07, 30, t), skin)
    w.position.z = -R * 0.3
    g.add(w)
    g.add(new THREE.Mesh(wings(R * 0.4, hs * 0.33, -sp.L * 0.36, sp.L * 0.13, sp.L * 0.06, 32, t * 0.8), skin))
  } else {
    g.add(new THREE.Mesh(wings(R * 0.7, hs, sp.L * 0.1, sp.L * 0.36, sp.L * 0.1, 38, t), skin))
    g.add(new THREE.Mesh(wings(R * 0.6, hs * 0.42, -sp.L * 0.3, sp.L * 0.15, sp.L * 0.06, 42, t * 0.8), skin))
  }
  // fins
  const finH = Math.max(1, sp.H - 1.4 - R)
  const finOf = () => new THREE.Mesh(fin(-sp.L * 0.26, sp.L * 0.2, sp.L * 0.08, finH, 40, t), skin)
  if ((sp.fins ?? 1) === 2) {
    const off = Math.max(R * 0.7, sp.span * (sp.plan === 'heavy' ? 0.17 : 0.1))
    for (const side of [-1, 1]) {
      const f = finOf()
      f.rotation.y = (side * (sp.cant ?? 15) * Math.PI) / 180
      f.position.set(side * off, 0, R * 0.4)
      g.add(f)
    }
  } else {
    const f = finOf()
    f.position.z = R * 0.6
    g.add(f)
  }
  // canopy
  const can = new THREE.Mesh(new THREE.SphereGeometry(1, 12, 8), MATS.glass)
  can.scale.set(R * 0.55, sp.L * (sp.plan === 'heavy' ? 0.04 : 0.11), R * 0.55)
  can.position.set(0, sp.L * (sp.plan === 'heavy' ? 0.42 : 0.25), R * 0.75)
  g.add(can)
  // engines
  const pod = (x: number, y: number, z: number, len: number, rad: number) => {
    const p = new THREE.Mesh(new THREE.CylinderGeometry(rad, rad * 0.9, len, 10), MATS.navyDark)
    p.position.set(x, y, z)
    g.add(p)
  }
  if (sp.pods === 'a10') { pod(-1.6, -sp.L * 0.18, R + 0.7, 3.2, 0.62); pod(1.6, -sp.L * 0.18, R + 0.7, 3.2, 0.62) }
  if (sp.pods === 'wing2') { pod(-hs * 0.35, sp.L * 0.08, -R * 0.8, sp.L * 0.2, R * 0.45); pod(hs * 0.35, sp.L * 0.08, -R * 0.8, sp.L * 0.2, R * 0.45) }
  if (sp.pods === 'wing4') for (const x of [-0.62, -0.32, 0.32, 0.62]) pod(x * hs, sp.L * 0.1 - Math.abs(x) * sp.L * 0.12, -R * 0.9, sp.L * 0.1, R * 0.35)
  if (sp.dish) {
    const d = new THREE.Mesh(new THREE.CylinderGeometry(3.65, 3.65, 0.75, 24), MATS.navy)
    d.rotation.x = Math.PI / 2
    d.position.set(0, -sp.L * 0.05, R + 2.2)
    g.add(d)
  }
  return g
}

// ─── weapons ───────────────────────────────────────────────────────────────

/** Weapon names → length, diameter (metres). */
const WEAPONS: [RegExp, number, number][] = [
  [/Mk-?84|GBU-?(10|24|31)|BLU-109/i, 3.84, 0.46],
  [/Mk-?83|GBU-?(16|32)/i, 3.0, 0.36],
  [/GBU-?12|GBU-?38|GBU-?54|Mk-?82/i, 3.0, 0.27],
  [/CBU|Rockeye|BL755/i, 2.34, 0.33],
  [/AIM-?120|AMRAAM/i, 3.66, 0.18],
  [/AIM-?7|Sparrow/i, 3.66, 0.2],
  [/AIM-?9|Sidewinder/i, 2.87, 0.13],
  [/AIM-?54|Phoenix/i, 3.96, 0.38],
  [/AGM-?65|Maverick/i, 2.49, 0.3],
  [/AGM-?88|HARM/i, 4.17, 0.25],
  [/AGM-?84|Harpoon|SLAM/i, 4.4, 0.34],
  [/R-27/i, 4.0, 0.23],
  [/R-73|R-60/i, 2.9, 0.17],
  [/R-77/i, 3.6, 0.2],
  [/Hydra|FFAR|S-8|S-5|Zuni|APKWS/i, 1.6, 0.07],
  [/AGM-?114|Hellfire/i, 1.63, 0.18],
]

export function weapon(name: string | undefined, kind: string, skin: THREE.Material): THREE.Group {
  const n = name ?? ''
  let L = kind === 'bomb' ? 3.0 : kind === 'rocket' ? 1.6 : 3.4
  let D = kind === 'bomb' ? 0.3 : 0.18
  for (const [re, l, d] of WEAPONS) if (re.test(n)) { L = l; D = d; break }
  const g = new THREE.Group()
  g.userData.length = L
  const body = new THREE.Mesh(new THREE.CylinderGeometry(D / 2, D / 2, L * 0.8, 10), skin)
  body.position.y = -L * 0.05
  g.add(body)
  const nose = new THREE.Mesh(new THREE.ConeGeometry(D / 2, L * 0.2, 10), skin)
  nose.position.y = L * 0.45
  g.add(nose)
  const fw = kind === 'bomb' ? D * 1.1 : D * 1.4
  for (let i = 0; i < 4; i++) {
    const f = new THREE.Mesh(new THREE.BoxGeometry(fw, L * 0.14, 0.02), skin)
    f.position.y = -L * 0.38
    f.rotation.y = (i * Math.PI) / 2
    f.position.x = 0
    g.add(f)
  }
  return g
}

// ─── ships and ground units ────────────────────────────────────────────────

/** Ship names → length, beam (metres). */
const SHIPS: [RegExp, number, number][] = [
  [/PERRY|FFG/i, 136, 14],
  [/Arleigh|DDG|USS_Arleigh/i, 154, 20],
  [/TICONDEROG|CG-?47|CG_/i, 173, 17],
  [/Type_?054|054A/i, 134, 16],
  [/Type_?052|052/i, 157, 17],
  [/MOSCOW|Moskva|Slava/i, 186, 21],
  [/NEUSTRASH|Neustrashimy/i, 129, 15],
  [/KILO|Submarine|SOM/i, 73, 10],
  [/LHA|Tarawa/i, 250, 32],
]

export function ship(name: string | undefined): THREE.Group {
  let L = 120, B = 15
  for (const [re, l, b] of SHIPS) if (re.test(name ?? '')) { L = l; B = b; break }
  const g = new THREE.Group()
  g.userData.length = L
  const hb = B / 2
  const s = new THREE.Shape([
    new THREE.Vector2(-hb * 0.8, -L / 2), new THREE.Vector2(hb * 0.8, -L / 2), new THREE.Vector2(hb, -L * 0.2),
    new THREE.Vector2(hb * 0.9, L * 0.25), new THREE.Vector2(0, L / 2), new THREE.Vector2(-hb * 0.9, L * 0.25), new THREE.Vector2(-hb, -L * 0.2),
  ])
  const free = Math.max(4, L * 0.05)
  const hull = new THREE.Mesh(new THREE.ExtrudeGeometry(s, { depth: free, bevelEnabled: false }), MATS.hull)
  g.add(hull)
  const sup = new THREE.Mesh(new THREE.BoxGeometry(B * 0.6, L * 0.22, free * 1.6), MATS.island)
  sup.position.set(0, L * 0.05, free + free * 0.8)
  g.add(sup)
  const mast = new THREE.Mesh(new THREE.CylinderGeometry(0.4, 0.6, free * 2.4, 6), MATS.island)
  mast.rotation.x = Math.PI / 2
  mast.position.set(0, L * 0.02, free * 2.6 + free * 1.2)
  g.add(mast)
  return g
}

/** Ground units: a box the unit's size, in its colour. */
export function groundUnit(kind: string): THREE.Group {
  const g = new THREE.Group()
  const [l, w, h, m] =
    kind === 'armor' ? [7, 3.5, 2.4, MATS.olive]
    : kind === 'sam' ? [9, 3, 3.4, MATS.olive]
    : kind === 'infantry' ? [0.5, 0.5, 1.8, MATS.olive]
    : kind === 'static' ? [12, 10, 6, MATS.sand]
    : [6.5, 2.5, 3, MATS.olive]
  g.userData.length = l
  const b = new THREE.Mesh(new THREE.BoxGeometry(w, l, h), m)
  b.position.z = h / 2
  g.add(b)
  if (kind === 'armor') {
    const tur = new THREE.Mesh(new THREE.BoxGeometry(w * 0.6, l * 0.4, h * 0.4), m)
    tur.position.set(0, 0, h + h * 0.2)
    g.add(tur)
    const gun = new THREE.Mesh(new THREE.CylinderGeometry(0.08, 0.08, l * 0.6, 6), m)
    gun.position.set(0, l * 0.45, h + h * 0.2)
    g.add(gun)
  }
  return g
}
