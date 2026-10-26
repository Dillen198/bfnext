import { describe, expect, it } from 'vitest'
import { buildFromText, classify } from './build'
import { AcmiParser, parseLines, parseNum, parseTransform } from './parse'
import { buildFromBlob } from './read'
import { LocalTrackStore } from './store'
import type { TrackSource } from '../data'

const HEAD = 'FileType=text/acmi/tacview\nFileVersion=2.2\n'

describe('ACMI parser', () => {
  it('reads frames, objects, removals, escapes, continuations, BOM and comments', () => {
    const text = '﻿FileType=text/acmi/tacview\r\nFileVersion=2.2\r\n' +
      '0,ReferenceTime=2011-06-02T05:00:00Z,Title=A\\, B\n' +
      '// a comment\n' +
      '#1.5\n' +
      '3000102,T=1|2|3,Name=F-16C_50\n' +
      '0,Comments=line one\\\nline two\n' +
      'A1,Name=x\\\\\n' +
      '-3000102\n' +
      'zz,Name=not an id\n' +
      '#oops\n' +
      '#2'
    expect(parseLines(text)).toEqual([
      { k: 'object', id: 0, props: [['ReferenceTime', '2011-06-02T05:00:00Z'], ['Title', 'A, B']] },
      { k: 'frame', t: 1.5 },
      { k: 'object', id: 0x3000102, props: [['T', '1|2|3'], ['Name', 'F-16C_50']] },
      { k: 'object', id: 0, props: [['Comments', 'line one\nline two']] },
      // an even number of backslashes does not continue the line
      { k: 'object', id: 0xa1, props: [['Name', 'x\\']] },
      { k: 'remove', id: 0x3000102 },
      { k: 'frame', t: 2 },
    ])
  })

  it('reads lines split across chunks', () => {
    const lines: string[] = []
    const text = HEAD + '#0\na1,T=1|2|3\\\n,Name=x\n#1\n-a1\n'
    for (let cut = 1; cut < text.length; cut++) {
      const out = streamParse([text.slice(0, cut), text.slice(cut)], [])
      lines.push(JSON.stringify(out))
    }
    expect(new Set(lines).size).toBe(1)
    expect(JSON.parse(lines[0])).toEqual([
      { k: 'frame', t: 0 },
      { k: 'object', id: 0xa1, props: [['T', '1|2|3\n'], ['Name', 'x']] },
      { k: 'frame', t: 1 },
      { k: 'remove', id: 0xa1 },
    ])
  })

  it('keeps long ids exact', () => {
    const out = parseLines(HEAD + 'ffffffffffffffff,Name=a\n00000000000001,Name=b\n')
    expect(out.map(l => (l.k === 'object' ? l.id : null))).toEqual(['ffffffffffffffff', 1])
  })

  it('rejects other files', () => {
    expect(() => parseLines('hello\nworld\n')).toThrow(/not an ACMI/)
    expect(() => parseLines('')).toThrow(/empty/)
  })

  it('parses T forms with left-out components', () => {
    const out = new Float64Array(6)
    const tf = (s: string) => { const att = parseTransform(s, 0, s.length, out); return [att, ...out] }
    expect(tf('1|2|3')).toEqual([false, 1, 2, 3, NaN, NaN, NaN])
    expect(tf('1|2|3|4|5')).toEqual([false, 1, 2, 3, NaN, NaN, NaN])
    expect(tf('1||3|10|20|30')).toEqual([true, 1, NaN, 3, 10, 20, 30])
    expect(tf('1|2|3|10|20|30|7|8|90')).toEqual([true, 1, 2, 3, 10, 20, 30])
    expect(tf('||-5.5')).toEqual([false, NaN, NaN, -5.5, NaN, NaN, NaN])
  })

  it('parses numbers like parseFloat, and only numbers', () => {
    for (const s of ['0', '-0.5', '41.123456789', '0.10150000000000001', '1e-5', '-2.5E3', '.5', '5.', '123456789012345678']) {
      expect(parseNum(s, 0, s.length)).toBe(parseFloat(s))
    }
    for (const s of ['', '-', 'abc', '0x10', 'Infinity', '1.2.3', '1,5']) expect(parseNum(s, 0, s.length)).toBeNaN()
  })
})

function streamParse(chunks: string[], out: unknown[]) {
  const p = new AcmiParser({
    frame: t => out.push({ k: 'frame', t }),
    remove: id => out.push({ k: 'remove', id }),
    object: (id, props) => out.push({ k: 'object', id, props: props.toArray() }),
  })
  for (const c of chunks) p.push(c)
  p.end()
  return out
}

describe('classify', () => {
  it('maps Tacview types to kinds', () => {
    expect(classify('Air+FixedWing')).toBe('air')
    expect(classify('Air+Rotorcraft')).toBe('helo')
    expect(classify('Weapon+Missile')).toBe('missile')
    expect(classify('Projectile+Shell')).toBe('shell')
    expect(classify('Misc+Decoy+Flare')).toBe('flare')
    expect(classify('Misc+Decoy+Chaff')).toBe('chaff')
    expect(classify('Misc+Decoy')).toBe('decoy')
    expect(classify('Misc+Explosion')).toBe('explosion')
    expect(classify('Ground+Light+Human+Air+Parachutist')).toBe('parachutist')
    expect(classify('Navaid+Static+Bullseye')).toBe('bullseye')
    expect(classify('Navaid+Static+Waypoint')).toBe('misc')
    expect(classify('Misc+Shrapnel')).toBeNull()
    expect(classify('Weapon+Beam')).toBeNull()
    expect(classify('Ground+AntiAircraft')).toBe('sam')
    expect(classify('Ground+Heavy+Armor+Vehicle+Tank')).toBe('armor')
    expect(classify('Sea+Watercraft+AircraftCarrier')).toBe('carrier')
  })
})

describe('ACMI builder', () => {
  /** What DCS writes: no Destroyed/TakenOff/Landed lines. Port of the Rust
   *  test `derives_events_without_event_lines`. */
  it('derives takeoff, landing, hit, miss, kill and loss from the tracks', () => {
    let t = HEAD +
      '0,ReferenceTime=2026-10-08T10:00:00Z,RecordingTime=2026-10-08T12:00:00Z,ReferenceLongitude=41,ReferenceLatitude=42\n#0\n' +
      'a1,T=0.1|0.1|100|0|0|90,Type=Air+FixedWing,Name=F-16C_50,Pilot=Viper,Color=Blue,AGL=1\n' +
      'b2,T=0.3|0.1|5000|0|0|270,Type=Air+FixedWing,Name=MiG-29S,Pilot=Ivan,Color=Red,AGL=4800\n' +
      'e5,T=0.5|0.3|6000|0|0|270,Type=Air+FixedWing,Name=Su-27,Pilot=Boris,Color=Red,AGL=5800\n'
    let lon = 0.1, agl = 1
    for (let i = 1; i <= 400; i++) {
      const ts = i * 0.5
      if (ts > 20 && ts < 150) { lon += 0.0015; agl = Math.min(agl + 25, 4000) }
      else if (ts >= 150 && ts < 170) { lon += 0.0002; agl = 1 }
      t += `#${ts}\na1,T=${lon}||${agl + 100}|0|0|90,AGL=${agl}\n`
      const alt = agl + 100
      if (i === 120) t += `c3,T=${lon}|0.1|${alt}|0|0|90,Type=Weapon+Missile,Name=AIM_120C,Color=Blue\n`
      if (i > 120 && i <= 130) {
        const f = (i - 120) / 10
        t += `c3,T=${lon + (0.3 - lon) * f}||${alt + (5000 - alt) * f}\n`
      }
      if (i === 131) t += '-c3\n'
      if (i === 133) t += '-b2\n'
      if (i === 140) t += `d4,T=${lon}|0.1|${alt}|0|0|0,Type=Weapon+Missile,Name=AIM_120C,Color=Blue\n`
      if (i > 140 && i <= 150) t += `d4,T=|${0.1 + (i - 140) * 0.05}|\n`
      if (i === 151) t += '-d4\n'
      if (i < 200) t += `e5,T=${0.5 - i * 0.0005}||\n`
      if (i === 200) t += '-e5\n'
    }
    const { meta } = buildFromText(t)
    expect(meta.v).toBe(4)
    const idx = (n: string) => meta.objects.findIndex(o => o.n === n)
    const [f16, mig, su] = [idx('F-16C_50'), idx('MiG-29S'), idx('Su-27')]
    const find = (k: string) => meta.events.filter(e => e.k === k)

    const takeoff = find('takeoff')
    expect(takeoff).toHaveLength(1)
    expect(takeoff[0].t).toBeGreaterThanOrEqual(20_000)
    expect(takeoff[0].t).toBeLessThan(30_000)
    expect(find('landing')).toHaveLength(1)

    const fired = find('fired')
    expect(fired).toHaveLength(2)
    expect(fired.every(e => e.o === f16)).toBe(true)
    const hit = fired.find(e => e.hit)!
    expect(hit.tg).toBe(mig)
    const miss = fired.find(e => !e.hit)!
    expect(miss.md!).toBeGreaterThan(1000)

    const kill = find('kill')
    expect(kill).toHaveLength(1)
    expect([kill[0].o, kill[0].by]).toEqual([mig, f16])
    const lost = find('lost')
    expect(lost).toHaveLength(1)
    expect(lost[0].o).toBe(su)

    const flight = (p: string) => meta.flights.find(f => f.pilot === p)!
    const viper = flight('Viper')
    expect([viper.shots, viper.hits, viper.kills, viper.fate]).toEqual([2, 1, 1, 'landed'])
    expect(flight('Ivan').fate).toBe('destroyed')
    expect(flight('Boris').fate).toBe('destroyed')
    expect(meta.objects[mig].d).toBe(kill[0].t)
    expect(viper.start_ms).toBe(Date.parse('2026-10-08T12:00:00Z') + viper.t0)
  })

  /** Port of `builds_tracks_events_and_flights`, plus the v4 additions. */
  it('builds tracks, events, flights, channels and effects', () => {
    let text = HEAD +
      '0,ReferenceTime=2026-10-08T10:00:00Z,RecordingTime=2026-10-08T12:00:00Z,ReferenceLongitude=41,ReferenceLatitude=42,Title=Test\n' +
      '#0\n' +
      'a1,T=0.1|0.1|5000|0|0|90,Type=Air+FixedWing,Name=F-16C_50,Pilot=Viper,Color=Blue\n' +
      'b2,T=0.2|0.1|5000|0|0|270,Type=Air+FixedWing,Name=MiG-29S,Pilot=Ivan,Color=Red\n' +
      '0,Event=TakenOff|a1|\n'
    for (let i = 1; i <= 100; i++) {
      const t = i * 0.1
      text += `#${t}\na1,T=${0.1 + t * 0.0001}||\n`
      if (i === 5) text += 'a1,FuelWeight=3000,LockedTarget=b2\n'
      if (i === 10) {
        text += 'c3,T=0.1001|0.1|5000|0|0|90,Type=Weapon+Missile,Name=AIM-120C,Color=Blue\n'
        // a shell right on the MiG: kept, never a target
        text += 'd4,T=0.2|0.1|5000,Type=Projectile+Shell\n'
        text += '0,Event=Message|a1|Fox three\n'
      }
      if (i === 20) text += 'a1,FuelWeight=2900\n'
      if (i > 10) text += `c3,T=${0.1001 + (t - 1) * (0.0999 / 9)}||\n`
    }
    text += '0,Event=Destroyed|b2|\n-c3\n-b2\n#400\na1,T=0.2||,LockedTarget=0\n0,Event=Landed|a1|\n0,Event=Bookmark|The end\n'
    const { meta, tracks } = buildFromText(text)
    expect(meta.duration_ms).toBe(400_000)
    expect(meta.chunks).toBe(1)
    expect(meta.chunk_ms).toBe(400_000)
    expect(meta.start_ms).toBe(Date.parse('2026-10-08T12:00:00Z'))
    expect(meta.title).toBe('Test')
    expect(meta.objects).toHaveLength(4)
    const idx = (n: string) => meta.objects.findIndex(o => o.n === n)
    const [f16, mig, aim] = [idx('F-16C_50'), idx('MiG-29S'), idx('AIM-120C')]
    const shell = meta.objects.findIndex(o => o.k === 'shell')
    expect(shell).toBe(3)
    expect(meta.objects[shell]).toMatchObject({ ty: 'Projectile+Shell', id: 'd4' })
    expect(meta.objects[f16]).toMatchObject({ ty: 'Air+FixedWing', id: 'a1', k: 'air', f: 1 })
    expect(meta.objects[aim].par).toBe(f16)

    const ev = meta.events
    const kill = ev.find(e => e.k === 'kill')!
    expect([kill.o, kill.by]).toEqual([mig, f16])
    const fired = ev.find(e => e.k === 'fired')!
    expect(fired.o).toBe(f16)
    expect(fired.tg).toBe(mig) // not the shell sitting on the MiG
    expect(ev.some(e => e.k === 'takeoff' && e.o === f16)).toBe(true)
    expect(ev.find(e => e.k === 'message')).toMatchObject({ t: 1000, o: f16, text: 'Fox three' })
    expect(ev.find(e => e.k === 'bookmark')).toMatchObject({ t: 400_000, o: null, text: 'The end' })
    expect(ev.map(e => e.t)).toEqual([...ev.map(e => e.t)].sort((a, b) => a - b))

    const viper = meta.flights.find(f => f.pilot === 'Viper')!
    expect([viper.shots, viper.kills, viper.fate]).toEqual([1, 1, 'landed'])
    expect(meta.flights.find(f => f.pilot === 'Ivan')!.fate).toBe('destroyed')

    // The F-16: every sample once, in order, thinned to 100 ms, and its last
    // state at 400 s.
    const s = tracks.get(f16)!
    expect(s.t[0]).toBe(0)
    expect(s.t[s.t.length - 1]).toBe(400_000)
    for (let i = 1; i < s.t.length; i++) expect(s.t[i]).toBeGreaterThan(s.t[i - 1])
    expect(s.lon[s.t.length - 1]).toBeCloseTo(41.2, 9)
    expect(s.lat[0]).toBeCloseTo(42.1, 9)
    expect(s.roll).toBeDefined()
    // channels: fuel backfilled with its first value, the lock as a meta
    // index (-1 before the first lock and after it is dropped)
    const fuel = s.ch!.FuelWeight, lock = s.ch!.LockedTarget
    const at = (ms: number) => s.t.indexOf(ms)
    expect(fuel[0]).toBe(3000)
    expect(fuel[at(1000)]).toBe(3000)
    expect(fuel[at(2000)]).toBe(2900)
    expect(lock[0]).toBe(-1)
    expect(lock[at(500)]).toBe(mig)
    expect(lock[at(400_000)]).toBe(-1)
    // the missile ends where it ended
    const m = tracks.get(aim)!
    expect(m.t[m.t.length - 1]).toBe(10_000)
    // the shell: one state, kept to its end
    expect(tracks.get(shell)!.t.length).toBe(1)
  })

  it('drops shrapnel and keeps the rest; thins per kind', () => {
    let text = HEAD + '#0\n1,T=1|1|1,Type=Misc+Shrapnel\n2,T=1|1|1,Type=Ground+Static+Building\n3,T=1|1|1,Type=Ground+Vehicle\n4,T=1|1|1,Type=Misc+Decoy+Flare\n'
    for (let i = 1; i <= 50; i++) text += `#${i * 0.1}\n1,T=${1 + i * 1e-5}||\n2,T=${1 + i * 1e-5}||\n3,T=${1 + i * 1e-5}||\n4,T=${1 + i * 1e-5}||\n`
    const { meta, tracks } = buildFromText(text)
    expect(meta.objects.map(o => o.k)).toEqual(['static', 'vehicle', 'flare'])
    // static: first and last; vehicle: each second + last; flare: 500 ms + last
    expect([...tracks.get(0)!.t]).toEqual([0, 5000])
    expect([...tracks.get(1)!.t]).toEqual([0, 1000, 2000, 3000, 4000, 5000])
    expect([...tracks.get(2)!.t]).toEqual([0, 500, 1000, 1500, 2000, 2500, 3000, 3500, 4000, 4500, 5000])
  })
})

describe('LocalTrackStore', () => {
  it('finds who is present quickly and samples them', () => {
    let text = HEAD + '#0\n'
    // 2000 short-lived shells and a few long-lived aircraft
    for (let s = 0; s <= 3600; s++) {
      text += `#${s}\n`
      if (s === 0) for (let a = 1; a <= 3; a++) text += `a${a},T=${a}|1|1000|0|0|0,Type=Air+FixedWing,Name=A${a}\n`
      for (let a = 1; a <= 3; a++) text += `a${a},T=${a + s * 1e-4}||\n`
      if (s % 2 === 0 && s < 4000) text += `${(0x1000 + s).toString(16)},T=1|1|1,Type=Projectile+Shell\n`
      if (s % 2 === 0 && s >= 4) text += `-${(0x1000 + s - 4).toString(16)}\n`
    }
    const store = new LocalTrackStore(buildFromText(text), 'test.txt.acmi')
    expect(store.name).toBe('test.txt.acmi')
    expect((store as TrackSource).isLoaded(0)).toBe(true)
    const brute = (t: number) => store.meta.objects.flatMap((o, i) => (o.t0 <= t && t <= o.t1 ? [i] : []))
    for (const t of [0, 1, 999, 5_000, 123_456, 1_800_000, 3_599_999, 3_600_000, 4_000_000]) {
      expect(store.present(t).sort((a, b) => a - b)).toEqual(brute(t))
    }
    const st = store.state(0, 1_800_500)!
    expect(st.lon).toBeCloseTo(1 + 1800.5e-4, 6)
    expect(st.alt).toBeCloseTo(1000, 3)
    expect(st.gs).toBeGreaterThan(5)
    expect(store.state(0, 4_000_000)).toBeNull()
    expect(store.wholeTrack(0)!.t.length).toBe(3601)
    const trail = store.trail(0, 100_000, 10_000, 2_000)
    expect(trail.length).toBe(7)
    expect(trail[trail.length - 1][0]).toBeCloseTo(1.01, 6)
  })
})

describe('reading files', () => {
  const text = (frames: number) => {
    let s = HEAD + '0,RecordingTime=2026-10-08T12:00:00Z\n'
    for (let i = 0; i < frames; i++) s += `#${i}\n1,T=${(i * 0.001).toFixed(3)}|0|1000,Type=Air+FixedWing\n`
    return s
  }

  it('reads plain text', async () => {
    const { meta, tracks } = await buildFromBlob(new Blob([text(100)]))
    expect(meta.duration_ms).toBe(99_000)
    expect(tracks.get(0)!.t.length).toBe(100)
  })

  it('reads a stored zip, and a cut-off one', async () => {
    const bytes = new TextEncoder().encode(text(20_000))
    const zip = makeZip(bytes, bytes, 0)
    const progress: number[] = []
    const whole = await buildFromBlob(new Blob([zip]), f => progress.push(f))
    expect(whole.tracks.get(0)!.t.length).toBe(20_000)
    expect(progress[progress.length - 1]).toBe(1)
    const cut = await buildFromBlob(new Blob([zip.slice(0, Math.floor(zip.length * 2 / 3))]))
    const n = cut.tracks.get(0)!.t.length
    expect(n).toBeGreaterThan(5_000)
    expect(n).toBeLessThan(20_000)
  })

  it('reads a deflated zip, and a cut-off one', async () => {
    const bytes = new TextEncoder().encode(text(20_000))
    const deflated = new Uint8Array(await new Response(new Blob([bytes]).stream().pipeThrough(new CompressionStream('deflate-raw'))).arrayBuffer())
    const zip = makeZip(bytes, deflated, 8)
    const whole = await buildFromBlob(new Blob([zip]))
    expect(whole.tracks.get(0)!.t.length).toBe(20_000)
    // as Tacview leaves it: sizes in a data descriptor that never came
    const cut = await buildFromBlob(new Blob([makeZip(bytes, deflated, 8, true).slice(0, 30 + 8 + Math.floor(deflated.length * 2 / 3))]))
    const n = cut.tracks.get(0)!.t.length
    expect(n).toBeGreaterThan(5_000)
    expect(n).toBeLessThan(20_000)
  })
})

/** A one-entry zip; `streamed` writes zero sizes and a data descriptor. */
function makeZip(raw: Uint8Array, data: Uint8Array, method: number, streamed = false): Uint8Array<ArrayBuffer> {
  const name = new TextEncoder().encode('rec.txt.acmi')
  const local = new DataView(new ArrayBuffer(30))
  local.setUint32(0, 0x04034b50, true)
  local.setUint16(4, 20, true)
  local.setUint16(6, streamed ? 8 : 0, true)
  local.setUint16(8, method, true)
  local.setUint32(18, streamed ? 0 : data.length, true)
  local.setUint32(22, streamed ? 0 : raw.length, true)
  local.setUint16(26, name.length, true)
  const desc = new DataView(new ArrayBuffer(streamed ? 16 : 0))
  if (streamed) { desc.setUint32(0, 0x08074b50, true); desc.setUint32(8, data.length, true); desc.setUint32(12, raw.length, true) }
  const cd = new DataView(new ArrayBuffer(46))
  cd.setUint32(0, 0x02014b50, true)
  cd.setUint16(10, method, true)
  cd.setUint32(20, data.length, true)
  cd.setUint32(24, raw.length, true)
  cd.setUint16(28, name.length, true)
  const cdOff = 30 + name.length + data.length + desc.byteLength
  const eocd = new DataView(new ArrayBuffer(22))
  eocd.setUint32(0, 0x06054b50, true)
  eocd.setUint16(8, 1, true)
  eocd.setUint16(10, 1, true)
  eocd.setUint32(12, 46 + name.length, true)
  eocd.setUint32(16, cdOff, true)
  const parts = [new Uint8Array(local.buffer), name, data, new Uint8Array(desc.buffer), new Uint8Array(cd.buffer), name, new Uint8Array(eocd.buffer)]
  const out = new Uint8Array(parts.reduce((n, p) => n + p.length, 0))
  let at = 0
  for (const p of parts) { out.set(p, at); at += p.length }
  return out
}
