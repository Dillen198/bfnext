// Handing a local .acmi file from wherever it was dropped (the recordings
// list, the viewer itself) to the local viewer at /replay/file. A File cannot
// travel in a URL, so it waits here; a reload of /replay/file finds nothing
// and asks for the file again.

let pending: File | null = null

export function setPendingFile(f: File) { pending = f }

export function takePendingFile(): File | null {
  const f = pending
  pending = null
  return f
}

/** Is this a file we can read? Tacview writes `.acmi`, `.zip.acmi`, `.txt.acmi`. */
export function isAcmi(f: File): boolean {
  return /\.acmi$/i.test(f.name)
}

/** The first .acmi among dropped files. */
export function acmiFrom(dt: DataTransfer | null): File | null {
  if (!dt) return null
  for (const f of Array.from(dt.files)) if (isAcmi(f)) return f
  return null
}
