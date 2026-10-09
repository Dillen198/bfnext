// Low-poly models for the 3D theatre, one per recognisable family, so an F-15
// reads differently from an F-16, an AWACS has its dish and a Chinook has two
// rotors. Built from primitives (no model files to license or load). Every
// model faces +Y (north), Z up, origin at the centre of mass, sized in metres.

import * as THREE from 'three'
import { mergeGeometries } from 'three/examples/jsm/utils/BufferGeometryUtils.js'
import type { Kind } from './data'

export type Family =
  | 'fighter' | 'twinfighter' | 'delta' | 'attack' | 'heavy' | 'bomber' | 'awacs' | 'drone'
  | 'helo' | 'attackhelo' | 'tandemhelo'
  | 'missile' | 'bomb'
  | 'sam' | 'radar' | 'armor' | 'vehicle' | 'infantry' | 'static' | 'ship' | 'carrier'

const RULES: [RegExp, Family][] = [
  [/E-3|A-50|E-2|KJ-2000|AWACS/i, 'awacs'],
  [/MQ-9|MQ-1|RQ-|WingLoong|Reaper|Predator|Bayraktar|TB2|Orion|Heron|drone/i, 'drone'],
  [/B-52|B-1|Tu-22|Tu-95|Tu-142|Tu-160|H-6/i, 'bomber'],
  [/C-130|C-17|C-5|Il-76|Il-78|An-2[0-9]|An-30|An-12|Yak-40|KC-?135|KC-?10|KC130|A400|Boeing|Airbus|A320|A330|B7[0-9]7|CIV|Falcon|C-47|L-39C?$|Learjet/i, 'heavy'],
  [/A-10|Su-25|Su-24|Tornado|Su-17|MiG-27|A-4|Harrier|AV8/i, 'attack'],
  [/M-?2000|Mirage|Rafale|Typhoon|Eurofighter|J-10|JAS39|Gripen|AJS37|Viggen|J-?7|MiG-21|F-106|Kfir/i, 'delta'],
  [/F-14|F-15|F-?A-18|F-18|Hornet|Su-2[7-9]|Su-3[0-5]|Su-57|MiG-29|MiG-3[15]|J-11|J-15|F-22|F-35|F-4/i, 'twinfighter'],
  [/CH-47|Chinook|CH-46/i, 'tandemhelo'],
  [/AH-64|AH-1|Mi-28|Ka-50|Ka-52|Mi-24|Tiger|Apache|Cobra/i, 'attackhelo'],
]

/** The model family for a recorded object. */
export function familyOf(kind: Kind, name?: string | null): Family {
  const n = name ?? ''
  switch (kind) {
    case 'air':
      for (const [re, f] of RULES) if (f !== 'tandemhelo' && f !== 'attackhelo' && re.test(n)) return f
      return 'fighter'
    case 'helo':
      if (/CH-47|Chinook|CH-46/i.test(n)) return 'tandemhelo'
      if (/AH-64|AH-1|Mi-28|Ka-50|Ka-52|Mi-24|Tiger|Apache|Cobra/i.test(n)) return 'attackhelo'
      return 'helo'
    case 'missile': case 'rocket': case 'torpedo': return 'missile'
    case 'bomb': return 'bomb'
    case 'sam': return /SR|TR|radar|EWR|STR|Flap|Dome|search|track|P-1[49]|55G6|1L13|Kub 1S91|SNR|RLS|9S|AN\/MPQ|MPQ|Tin Shield/i.test(n) ? 'radar' : 'sam'
    case 'armor': return 'armor'
    case 'vehicle': return 'vehicle'
    case 'infantry': return 'infantry'
    case 'static': return 'static'
    case 'ship': return 'ship'
    case 'carrier': return 'carrier'
  }
}

/** Approximate length (m) of each family's model. */
export const LENGTH: Record<Family, number> = {
  fighter: 15, twinfighter: 19, delta: 15, attack: 16, heavy: 35, bomber: 45, awacs: 46, drone: 11,
  helo: 17, attackhelo: 17, tandemhelo: 30,
  missile: 4, bomb: 2.5,
  sam: 8, radar: 8, armor: 7, vehicle: 6, infantry: 1.8, static: 10, ship: 120, carrier: 330,
}

/** Smallest the model is drawn on screen, px (it is scaled up beyond its
 *  real size when far away, as Tacview does). */
export const MIN_PX: Record<Family, number> = {
  fighter: 34, twinfighter: 36, delta: 34, attack: 34, heavy: 40, bomber: 42, awacs: 42, drone: 28,
  helo: 30, attackhelo: 30, tandemhelo: 34,
  missile: 12, bomb: 9,
  sam: 12, radar: 12, armor: 10, vehicle: 9, infantry: 5, static: 9, ship: 24, carrier: 34,
}

const box = (w: number, l: number, h: number, x = 0, y = 0, z = 0) => new THREE.BoxGeometry(w, l, h).translate(x, y, z)
const tube = (r1: number, r2: number, l: number, y = 0, z = 0, x = 0) => new THREE.CylinderGeometry(r1, r2, l, 10).translate(x, y, z)
const cone = (r: number, l: number, y = 0, z = 0) => new THREE.ConeGeometry(r, l, 10).translate(0, y, z)

/** A flat wing panel: a trapezoid in the XY plane, root at x=0. */
function wing(span: number, root: number, tip: number, sweep: number, y: number, z: number, thick = 0.2): THREE.BufferGeometry {
  const sh = new THREE.Shape()
  sh.moveTo(0, root / 2)
  sh.lineTo(span, root / 2 - sweep)
  sh.lineTo(span, root / 2 - sweep - tip)
  sh.lineTo(0, -root / 2)
  sh.closePath()
  const g = new THREE.ExtrudeGeometry(sh, { depth: thick, bevelEnabled: false })
  g.translate(0, y, z - thick / 2)
  return g
}
const mirror = (g: THREE.BufferGeometry) => g.clone().scale(-1, 1, 1)
const both = (g: THREE.BufferGeometry) => [g, mirror(g)]
/** A vertical fin: the wing shape stood up on the YZ plane. */
function fin(height: number, root: number, tip: number, sweep: number, y: number, x = 0, z = 0): THREE.BufferGeometry {
  return wing(height, root, tip, sweep, 0, 0, 0.18).rotateY(-Math.PI / 2).translate(x, y, z)
}

function build(f: Family): THREE.BufferGeometry {
  const merge = (gs: THREE.BufferGeometry[]) => {
    // ExtrudeGeometry is indexed; Box/Cylinder too -- make them all non-indexed
    // so mergeGeometries never refuses a mix.
    const g = mergeGeometries(gs.map(x => (x.index ? x.toNonIndexed() : x)).map(x => { x.deleteAttribute('uv'); return x }))
    if (!g) throw new Error(`model ${f} failed to merge`)
    g.computeVertexNormals()
    return g
  }
  switch (f) {
    case 'fighter':
      return merge([
        tube(0.7, 0.55, 12, -0.5), cone(0.7, 3.2, 7.1), box(0.9, 2.2, 0.8, 0, 3.4, 0.6),
        ...both(wing(4.6, 4.2, 1.2, 3.0, -1.2, 0)), ...both(wing(2.4, 2.0, 0.8, 1.2, -5.6, 0, 0.15)),
        fin(3.0, 3.0, 1.0, 2.2, -5.0, 0, 0.5),
      ])
    case 'twinfighter':
      return merge([
        tube(0.8, 0.6, 15, -0.5), cone(0.8, 3.6, 8.8), box(1.0, 2.6, 0.9, 0, 4.6, 0.7), box(3.0, 9, 0.7, 0, -2.5, 0),
        ...both(wing(5.8, 5.6, 1.4, 3.6, -1.6, 0)), ...both(wing(2.8, 2.6, 1.0, 1.4, -7.4, 0, 0.15)),
        fin(3.4, 3.4, 1.2, 2.4, -6.4, 1.1, 0.6), fin(3.4, 3.4, 1.2, 2.4, -6.4, -1.1, 0.6),
      ])
    case 'delta':
      return merge([
        tube(0.7, 0.6, 12, -0.5), cone(0.7, 3.2, 7.1), box(0.9, 2.2, 0.8, 0, 3.6, 0.6),
        ...both(wing(4.6, 8.4, 0.6, 7.6, -1.8, 0)), ...both(wing(1.6, 1.4, 0.5, 0.8, 3.2, 0.2, 0.12)),
        fin(3.2, 4.0, 1.0, 3.2, -4.4, 0, 0.5),
      ])
    case 'attack':
      return merge([
        tube(0.8, 0.6, 13, 0), cone(0.8, 2.4, 7.7), box(1.0, 2.0, 0.9, 0, 4.4, 0.7),
        ...both(wing(8.2, 3.0, 1.6, 0.4, 0, 0)), tube(0.6, 0.6, 3.2, -3.0, 1.0, 1.3), tube(0.6, 0.6, 3.2, -3.0, 1.0, -1.3),
        ...both(wing(3.0, 1.8, 1.2, 0.2, -6.4, 0, 0.15)), fin(2.6, 2.0, 1.2, 0.6, -6.6, 3.0, 0.4), fin(2.6, 2.0, 1.2, 0.6, -6.6, -3.0, 0.4),
      ])
    case 'heavy':
      return merge([
        tube(2.0, 2.0, 30, 0), cone(2.0, 4, 17), cone(2.0, 4, -17).rotateX(Math.PI),
        ...both(wing(19, 6, 2.2, 3.0, 0.5, 1.0, 0.4)), tube(0.7, 0.7, 3.2, 1.8, -0.4, 6), tube(0.7, 0.7, 3.2, 1.8, -0.4, -6),
        tube(0.7, 0.7, 3.2, 1.6, -0.4, 11), tube(0.7, 0.7, 3.2, 1.6, -0.4, -11),
        ...both(wing(7, 4, 1.6, 2.4, -15, 0.8, 0.25)), fin(6.5, 5.5, 2.4, 3.4, -14.5, 0, 1.6),
      ])
    case 'bomber':
      return merge([
        tube(2.0, 1.8, 40, 0), cone(2.0, 6, 23),
        ...both(wing(22, 9, 2.0, 12, 1.0, 0.6, 0.4)),
        ...both(wing(7, 4, 1.6, 3, -19, 0.6, 0.25)), fin(7, 6, 2.6, 4, -18, 0, 1.8),
      ])
    case 'awacs':
      return merge([
        tube(2.0, 2.0, 40, 0), cone(2.0, 4, 22), cone(2.0, 4, -22).rotateX(Math.PI),
        ...both(wing(20, 6.5, 2.2, 5.0, 1, 0.8, 0.4)), tube(0.7, 0.7, 3.4, 2, -0.6, 7), tube(0.7, 0.7, 3.4, 2, -0.6, -7),
        ...both(wing(7, 4, 1.6, 2.8, -18, 0.8, 0.25)), fin(6.5, 5.5, 2.4, 3.4, -17.5, 0, 1.6),
        new THREE.CylinderGeometry(4.6, 4.6, 1.1, 24).rotateX(Math.PI / 2).translate(0, -6, 5.2),
        box(0.4, 1.6, 3.0, 0, -6, 3.2),
      ])
    case 'drone':
      return merge([
        tube(0.5, 0.35, 10, 0), cone(0.5, 1.4, 5.7),
        ...both(wing(10, 1.2, 0.6, 0.3, 0.6, 0.2, 0.12)),
        wing(2.6, 1.2, 0.5, 0.8, -4.6, 0.4, 0.1).rotateY(-0.6), mirror(wing(2.6, 1.2, 0.5, 0.8, -4.6, 0.4, 0.1).rotateY(-0.6)),
      ])
    case 'helo':
      return merge([
        box(2.2, 6.0, 2.2, 0, 1.4, 0), cone(1.1, 1.4, 5.1, 0), box(0.5, 8.0, 0.6, 0, -5.4, 0.4), fin(1.8, 1.4, 0.8, 0.6, -9.2, 0, 0.6),
        new THREE.CylinderGeometry(8, 8, 0.1, 28).rotateX(Math.PI / 2).translate(0, 1.4, 1.9),
        tube(0.15, 0.15, 1.0, 1.4, 1.4),
      ])
    case 'attackhelo':
      return merge([
        box(1.2, 7.0, 2.0, 0, 1.2, 0), cone(0.6, 1.6, 5.4, 0.2), box(0.5, 7.5, 0.6, 0, -5.0, 0.3),
        ...both(wing(1.8, 1.2, 0.8, 0.2, 0.8, -0.2, 0.15)), fin(2.0, 1.4, 0.8, 0.6, -8.8, 0, 0.6),
        new THREE.CylinderGeometry(7.3, 7.3, 0.1, 28).rotateX(Math.PI / 2).translate(0, 1.0, 1.8),
      ])
    case 'tandemhelo':
      return merge([
        box(3.6, 15, 3.4, 0, 0, 0), box(1.0, 2.4, 2.6, 0, -6.6, 2.4),
        new THREE.CylinderGeometry(9, 9, 0.12, 28).rotateX(Math.PI / 2).translate(0, 6.2, 2.6),
        new THREE.CylinderGeometry(9, 9, 0.12, 28).rotateX(Math.PI / 2).translate(0, -6.6, 3.9),
      ])
    case 'missile':
      return merge([tube(0.18, 0.18, 3.6), cone(0.18, 0.6, 2.1), box(1.0, 0.4, 0.05, 0, -1.6), box(0.05, 0.4, 1.0, 0, -1.6)])
    case 'bomb':
      return merge([tube(0.25, 0.25, 2.0), cone(0.25, 0.6, 1.3), box(0.8, 0.3, 0.05, 0, -1.0), box(0.05, 0.3, 0.8, 0, -1.0)])
    case 'sam':
      return merge([box(3, 7, 1.8, 0, 0, 0.9), box(1.8, 5, 1.6, 0, -0.5, 2.5).rotateX(0.5).translate(0, 0, 0.6)])
    case 'radar':
      return merge([box(3, 7, 1.8, 0, 0, 0.9), box(0.4, 0.4, 2.4, 0, 0, 3.0), box(5.0, 0.3, 2.2, 0, 0, 4.6)])
    case 'armor':
      return merge([box(3.4, 7, 1.6, 0, 0, 0.8), box(2.2, 3, 1.0, 0, -0.3, 2.1), box(0.3, 4, 0.3, 0, 3, 2.1)])
    case 'vehicle':
      return merge([box(2.4, 2.0, 2.4, 0, 2.0, 1.2), box(2.4, 4.0, 2.6, 0, -1.0, 1.3)])
    case 'infantry':
      return box(0.6, 0.6, 1.8, 0, 0, 0.9)
    case 'static':
      return box(10, 10, 5, 0, 0, 2.5)
    case 'ship':
      return merge([box(15, 120, 9, 0, 0, 2), cone(7.5, 20, 69, 2), box(9, 22, 12, 0, -10, 10), box(1.2, 1.2, 14, 0, -6, 20)])
    case 'carrier':
      return merge([box(38, 330, 18, 0, 0, 7), box(8, 30, 22, 14, -20, 24)])
  }
}

const cache = new Map<Family, THREE.BufferGeometry>()
export function modelOf(f: Family): THREE.BufferGeometry {
  let g = cache.get(f)
  if (!g) cache.set(f, (g = build(f)))
  return g
}
