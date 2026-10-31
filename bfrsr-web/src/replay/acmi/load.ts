// Open a local Tacview recording in the browser: the file is read and
// built in a worker and nothing is uploaded.

import type { Seg } from '../data'
import type { AcmiData } from './build'

export { buildFromText, type AcmiData } from './build'

export interface WorkerRequest { file: Blob }
export type WorkerReply =
  | { type: 'progress'; fraction: number }
  | { type: 'done'; meta: AcmiData['meta']; tracks: [number, Seg][] }
  | { type: 'error'; message: string }

/**
 * Read a `.zip.acmi` or `.txt.acmi` into replay data, off the main thread.
 * `onProgress` gets the fraction of the file read (0..1). Aborting the
 * signal stops the worker and rejects with an `AbortError`.
 */
export function readAcmiFile(file: Blob, onProgress?: (fraction: number) => void, signal?: AbortSignal): Promise<AcmiData> {
  return new Promise((resolve, reject) => {
    if (signal?.aborted) { reject(new DOMException('The read was aborted', 'AbortError')); return }
    const worker = new Worker(new URL('./worker.ts', import.meta.url), { type: 'module' })
    const stop = () => {
      worker.terminate()
      signal?.removeEventListener('abort', onAbort)
    }
    const onAbort = () => { stop(); reject(new DOMException('The read was aborted', 'AbortError')) }
    signal?.addEventListener('abort', onAbort)
    worker.onmessage = (e: MessageEvent<WorkerReply>) => {
      const m = e.data
      if (m.type === 'progress') onProgress?.(m.fraction)
      else if (m.type === 'done') { stop(); resolve({ meta: m.meta, tracks: new Map(m.tracks) }) }
      else { stop(); reject(new Error(m.message)) }
    }
    worker.onerror = e => { stop(); reject(new Error(e.message || 'the recording reader failed')) }
    worker.postMessage({ file } satisfies WorkerRequest)
  })
}
