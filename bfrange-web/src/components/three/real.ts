/**
 * The dashboard's real 3D models (bfweb/public/models, served here at
 * /models/ -- see vite.config.ts), picked by the shared catalogue
 * (shared/models/catalog.ts) as the dashboard's flight replay picks them,
 * except that a type the catalogue only lends a lookalike to (a T-45 as an
 * F-16) keeps our own build (`ownModelOf`). Each is merged into one geometry and shaded flat in the
 * object's own colour; until it has loaded (or if it fails) the scene keeps
 * the model built from primitives.
 */
import * as THREE from 'three'
import { mergeGeometries } from 'three/examples/jsm/utils/BufferGeometryUtils.js'
import { GLTFLoader } from 'three/examples/jsm/loaders/GLTFLoader.js'
import { ownModelOf, type Real } from '../../../../shared/models/catalog'

export { ownModelOf, type Real }

const cache = new Map<Real, Promise<THREE.BufferGeometry | null>>()
const loader = new GLTFLoader()

/** A real model's geometry, merged and lit; null when the file is missing or broken. */
export function realGeometry(r: Real): Promise<THREE.BufferGeometry | null> {
  let p = cache.get(r)
  if (!p) {
    p = loader
      .loadAsync(`${import.meta.env.BASE_URL}models/${encodeURIComponent(r)}.glb`)
      .then(gltf => {
        const parts: THREE.BufferGeometry[] = []
        gltf.scene.updateMatrixWorld(true)
        gltf.scene.traverse(o => {
          const m = o as THREE.Mesh
          if (!m.isMesh) return
          const g = m.geometry.clone().applyMatrix4(m.matrixWorld)
          for (const k of Object.keys(g.attributes)) if (k !== 'position') g.deleteAttribute(k)
          parts.push(g.index ? g.toNonIndexed() : g)
        })
        const g = parts.length ? mergeGeometries(parts) : null
        g?.computeVertexNormals()
        g?.computeBoundingBox()
        return g
      })
      .catch(() => null)
    cache.set(r, p)
  }
  return p
}

/** Nose-to-tail length (m) of a loaded model. */
export function realLength(g: THREE.BufferGeometry): number {
  const b = g.boundingBox
  return b ? b.max.y - b.min.y : 15
}
