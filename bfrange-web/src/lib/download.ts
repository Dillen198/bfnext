/** Save a Blob as a file in the browser. */
export function saveBlob(blob: Blob, name: string) {
  const url = URL.createObjectURL(blob)
  const a = document.createElement('a')
  a.href = url
  a.download = name
  document.body.appendChild(a)
  a.click()
  a.remove()
  setTimeout(() => URL.revokeObjectURL(url), 5000)
}

/** Fetch a URL (with the session cookie) and save it; returns false on 404. */
export async function downloadUrl(url: string, base: string): Promise<boolean> {
  const res = await fetch(url, { credentials: url.startsWith('data:') ? 'omit' : 'include' })
  if (res.status === 404) return false
  if (!res.ok) throw new Error(`HTTP ${res.status}`)
  const blob = await res.blob()
  const ext = blob.type.includes('svg') ? 'svg' : blob.type.includes('png') ? 'png' : 'bin'
  saveBlob(blob, `${base}.${ext}`)
  return true
}
