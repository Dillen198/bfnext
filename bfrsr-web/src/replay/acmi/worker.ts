// The .acmi reader's worker: reads the dropped file off the main thread and
// hands the tracks back by transfer (a few slab buffers, see build.ts).

import { buildFromBlob } from './read'
import type { WorkerReply, WorkerRequest } from './load'

const post = (msg: WorkerReply, transfer: Transferable[] = []) => self.postMessage(msg, { transfer })

self.onmessage = async (e: MessageEvent<WorkerRequest>) => {
  try {
    const { meta, tracks } = await buildFromBlob(e.data.file, fraction => post({ type: 'progress', fraction }))
    const buffers = new Set<ArrayBuffer>()
    for (const seg of tracks.values()) {
      for (const k of ['t', 'lon', 'lat', 'alt', 'roll', 'pitch', 'yaw', 'ias', 'mach', 'aoa', 'agl'] as const) {
        const a = seg[k]
        if (a) buffers.add(a.buffer as ArrayBuffer)
      }
      if (seg.ch) for (const a of Object.values(seg.ch)) buffers.add(a.buffer as ArrayBuffer)
    }
    post({ type: 'done', meta, tracks: [...tracks] }, [...buffers])
  } catch (err) {
    post({ type: 'error', message: String((err as Error)?.message ?? err) })
  }
}
