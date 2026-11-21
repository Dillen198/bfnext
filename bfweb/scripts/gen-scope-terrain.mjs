// Regenerate src/scope/data/terrain/*.json from a pydcs checkout.
//
//   git clone --depth 1 https://github.com/pydcs/dcs /tmp/pydcs
//   node scripts/gen-scope-terrain.mjs /tmp/pydcs
//
// For each DCS theatre it pulls the transverse-mercator projection params
// and every airport, classifies each as an airfield (has a runway) or a
// helipad (no runway), and projects the DCS x/z metres to WGS84 lat/lon.
// pydcs is LGPL-3.0; only factual coordinates/constants are extracted.

import fs from 'node:fs'
import path from 'node:path'
import proj4 from 'proj4'

const pydcs = process.argv[2] || '/tmp/pydcs'
const terrainDir = path.join(pydcs, 'dcs', 'terrain')
const outDir = path.join(path.dirname(new URL(import.meta.url).pathname).replace(/^\/([A-Za-z]:)/, '$1'), '..', 'src', 'scope', 'data', 'terrain')

const MAPS = [
  ['caucasus', 'Caucasus'],
  ['nevada', 'Nevada'],
  ['normandy', 'Normandy'],
  ['persiangulf', 'PersianGulf'],
  ['thechannel', 'TheChannel'],
  ['syria', 'Syria'],
  ['marianaislands', 'MarianaIslands'],
  ['sinai', 'Sinai'],
  ['kola', 'Kola'],
  ['falklands', 'Falklands'],
  ['germany', 'Germany'],
]

function parseProjection(dir) {
  const src = fs.readFileSync(path.join(dir, 'projection.py'), 'utf8')
  const num = (k) => Number(src.match(new RegExp(`${k}=([-\\d.eE]+)`))[1])
  return {
    centralMeridian: num('central_meridian'),
    falseEasting: num('false_easting'),
    falseNorthing: num('false_northing'),
    scaleFactor: num('scale_factor'),
  }
}

function projDef(p) {
  return [
    '+proj=tmerc', '+lat_0=0', `+lon_0=${p.centralMeridian}`,
    `+k_0=${p.scaleFactor}`, `+x_0=${p.falseEasting}`, `+y_0=${p.falseNorthing}`,
    '+towgs84=0,0,0,0,0,0,0', '+units=m', '+vunits=m', '+ellps=WGS84', '+no_defs',
  ].join(' ')
}

function parseAirports(dir, def) {
  const src = fs.readFileSync(path.join(dir, 'airports.py'), 'utf8')
  // split into class blocks
  const blocks = src.split(/^class\s+\w+\(Airport\):/m).slice(1)
  const out = []
  for (const b of blocks) {
    const nameM = b.match(/^\s*name\s*=\s*"([^"]+)"/m)
    const posM = b.match(/super\(\)\.__init__\(mapping\.Point\(\s*([-\d.eE]+)\s*,\s*([-\d.eE]+)/)
    if (!nameM || !posM) continue
    const name = nameM[1]
    const x = Number(posM[1]) // DCS north
    const z = Number(posM[2]) // DCS east
    const hasRunway = /self\.runways\.append\(Runway\(/.test(b)
    // proj4 ignores +axis; feed [east, north] -> [lon, lat]
    const [lon, lat] = proj4(def).inverse([z, x])
    out.push({ name, position: [lat, lon], heli: !hasRunway })
  }
  return out
}

fs.mkdirSync(outDir, { recursive: true })
for (const [slug, name] of MAPS) {
  const dir = path.join(terrainDir, slug)
  if (!fs.existsSync(dir)) { console.warn(`skip ${slug}: not in pydcs`); continue }
  const projection = parseProjection(dir)
  const airports = parseAirports(dir, projDef(projection))
  const lat = airports.reduce((s, a) => s + a.position[0], 0) / airports.length
  const lon = airports.reduce((s, a) => s + a.position[1], 0) / airports.length
  const data = { name, center: [lat, lon], airports, projection }
  fs.writeFileSync(path.join(outDir, `${slug}.json`), JSON.stringify(data, null, 2) + '\n')
  const heli = airports.filter((a) => a.heli).length
  console.log(`${name.padEnd(16)} ${airports.length} airports (${airports.length - heli} airfield, ${heli} helipad)  center ${lat.toFixed(2)},${lon.toFixed(2)}`)
}
