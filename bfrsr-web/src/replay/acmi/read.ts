// Read a local .acmi (zip or text) into replay data. Runs in the worker
// (worker.ts) and in tests; no DOM needed beyond Blob, streams and
// DecompressionStream.
//
// `.zip.acmi` is a zip holding one text entry. It is read from its local
// header rather than the central directory, because Tacview writes the zip
// while it records: a recording cut off by a hard restart has no directory,
// and its deflate data simply stops. That is read quietly up to the cut.

import { Builder, type AcmiData } from './build'
import { AcmiParser } from './parse'

export type Progress = (fraction: number) => void

function abortError(): Error {
  return new DOMException('The read was aborted', 'AbortError')
}

/** Read and build a recording. Progress is compressed bytes read / size. */
export async function buildFromBlob(blob: Blob, onProgress?: Progress, signal?: AbortSignal): Promise<AcmiData> {
  const builder = new Builder()
  const parser = new AcmiParser(builder)
  const td = new TextDecoder('utf-8')
  const size = blob.size || 1
  let read = 0, reported = -1
  const progress = (n: number) => {
    read += n
    const f = Math.min(1, read / size)
    if (onProgress && f - reported >= 0.002) { reported = f; onProgress(f) }
  }
  const text = (bytes: Uint8Array) => parser.push(td.decode(bytes, { stream: true }))

  const magic = new Uint8Array(await blob.slice(0, 4).arrayBuffer())
  const zip = magic.length === 4 && magic[0] === 0x50 && magic[1] === 0x4b && magic[2] === 3 && magic[3] === 4
  if (zip) {
    const entry = await firstEntry(blob)
    progress(entry.start)
    const src = blob.slice(entry.start, entry.end).stream()
    if (entry.method === 0) await pump(src, b => { progress(b.byteLength); text(b) }, signal, true)
    else await inflate(src, progress, text, signal)
  } else {
    await pump(blob.stream(), b => { progress(b.byteLength); text(b) }, signal, false)
  }
  parser.push(td.decode())
  parser.end()
  onProgress?.(1)
  return builder.finish({ fileEndMs: 'lastModified' in blob ? (blob as File).lastModified : undefined })
}

/** Read a stream to its end, calling `f` per chunk. With `quiet`, a read
 *  error ends the stream instead of failing. */
async function pump(src: ReadableStream<Uint8Array>, f: (b: Uint8Array) => void, signal: AbortSignal | undefined, quiet: boolean) {
  const r = src.getReader()
  try {
    for (;;) {
      if (signal?.aborted) throw abortError()
      let res: ReadableStreamReadResult<Uint8Array>
      try { res = await r.read() } catch (e) { if (quiet) return; throw e }
      if (res.done) return
      f(res.value)
    }
  } finally {
    r.releaseLock()
  }
}

const tick = () => new Promise<void>(res => setTimeout(res, 0))

/**
 * Inflate raw deflate data, handing out text as it comes. Stops quietly at
 * the first error -- the point where a cut-off recording's data ends. The
 * output is read eagerly and drained before the stream is closed, so the
 * truncation error raised at close loses nothing already inflated.
 */
async function inflate(src: ReadableStream<Uint8Array>, progress: (n: number) => void, out: (b: Uint8Array) => void, signal?: AbortSignal) {
  if (typeof DecompressionStream === 'undefined') throw new Error('this browser cannot unzip; unzip the .zip.acmi and drop the .txt.acmi')
  const ds = new DecompressionStream('deflate-raw')
  const writer = ds.writable.getWriter()
  const reader = ds.readable.getReader()
  let failure: unknown = null
  const reading = (async () => {
    for (;;) {
      let res: ReadableStreamReadResult<Uint8Array>
      // A deflate error is where the data ends.
      try { res = await reader.read() } catch { return }
      if (res.done) return
      // An error of our own (not a recording) is not.
      try { out(res.value) } catch (e) {
        failure = e
        reader.cancel().catch(() => {})
        return
      }
    }
  })()
  const r = src.getReader()
  try {
    for (;;) {
      if (signal?.aborted) throw abortError()
      if (failure) break
      const { done, value } = await r.read()
      if (done) break
      progress(value.byteLength)
      await writer.write(value as Uint8Array<ArrayBuffer>)
    }
    await tick()
    await writer.close()
  } catch (e) {
    if ((e as Error)?.name === 'AbortError') {
      reader.cancel().catch(() => {})
      throw e
    }
    // cut off (or our own failure, rethrown below): keep what was read
  } finally {
    r.releaseLock()
  }
  await tick()
  reader.cancel().catch(() => {})
  await reading
  if (failure) throw failure
}

interface Entry { method: number; start: number; end: number }

/** The first zip entry's data range and method, from its local header;
 *  the size from the header, else the central directory, else a data
 *  descriptor, else everything to the end of the file. */
async function firstEntry(blob: Blob): Promise<Entry> {
  const h = new DataView(await blob.slice(0, 30).arrayBuffer())
  if (h.byteLength < 30) throw new Error('the zip is cut off before its first entry')
  const gp = h.getUint16(6, true)
  const method = h.getUint16(8, true)
  if (method !== 0 && method !== 8) throw new Error(`zip compression method ${method} is not supported`)
  const csize = h.getUint32(18, true)
  const start = 30 + h.getUint16(26, true) + h.getUint16(28, true)
  let size: number | null = !(gp & 8) && csize !== 0 && csize !== 0xffffffff ? csize : null
  size ??= await centralSize(blob)
  size ??= await descriptorSize(blob, start)
  const end = size === null ? blob.size : Math.min(blob.size, start + size)
  return { method, start, end }
}

/** The first entry's compressed size from the central directory, if the
 *  zip has one. */
async function centralSize(blob: Blob): Promise<number | null> {
  const tailLen = Math.min(blob.size, 22 + 0xffff)
  const tail = new DataView(await blob.slice(blob.size - tailLen).arrayBuffer())
  for (let p = tailLen - 22; p >= 0; p--) {
    if (tail.getUint32(p, true) !== 0x06054b50) continue
    const cdOff = tail.getUint32(p + 16, true)
    if (cdOff + 46 > blob.size) return null
    const cd = new DataView(await blob.slice(cdOff, cdOff + 46).arrayBuffer())
    if (cd.getUint32(0, true) !== 0x02014b50 || cd.getUint32(42, true) !== 0) return null
    const s = cd.getUint32(20, true)
    return s === 0xffffffff ? null : s
  }
  return null
}

/** The size from a `PK\7\8` data descriptor that sits right after the
 *  entry's data, searched for in the last 64 KiB. */
async function descriptorSize(blob: Blob, start: number): Promise<number | null> {
  const from = Math.max(start, blob.size - 65536)
  const tail = new DataView(await blob.slice(from).arrayBuffer())
  for (let p = tail.byteLength - 16; p >= 0; p--) {
    if (tail.getUint32(p, true) !== 0x08074b50) continue
    const s = tail.getUint32(p + 8, true)
    if (s === from + p - start) return s
  }
  return null
}
