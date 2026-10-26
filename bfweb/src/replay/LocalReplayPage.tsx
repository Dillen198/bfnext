// /replay/file -- a Tacview file from this computer, read whole in the
// browser (a worker; never uploaded) and played in the same viewer as the
// servers' recordings.

import { useEffect, useMemo, useState } from 'react'
import { Link } from 'react-router-dom'
import { readAcmiFile, type AcmiData } from './acmi/load'
import { LocalTrackStore } from './acmi/store'
import { takePendingFile } from './local'
import { OpenFile } from './ReplayListPage'
import { Viewer } from './ReplayPage'
import './replay.css'

export default function LocalReplayPage() {
  const [file] = useState(takePendingFile)
  const [progress, setProgress] = useState(0)
  const [data, setData] = useState<AcmiData | null>(null)
  const [error, setError] = useState<string | null>(null)

  useEffect(() => {
    if (!file) return
    const ac = new AbortController()
    readAcmiFile(file, setProgress, ac.signal)
      .then(setData)
      .catch(e => { if (!ac.signal.aborted) setError(String(e?.message ?? e)) })
    return () => ac.abort()
  }, [file])

  const store = useMemo(() => (data && file ? new LocalTrackStore(data, file.name) : null), [data, file])

  if (store && file) return <Viewer source={store} rec={null} fileName={file.name} />
  return (
    <div className="rp-state">
      <div className="rp-state-head">TACVIEW FILE</div>
      {!file && (
        <>
          <div>Pick a Tacview recording from this computer to watch it here.</div>
          <OpenFile />
        </>
      )}
      {file && !error && (
        <div className="rp-loading">
          <div>Reading {file.name} ({(file.size / 1e6).toFixed(1)} MB)…</div>
          <div className="rp-progress"><div style={{ width: `${Math.round(progress * 100)}%` }} /></div>
        </div>
      )}
      {error && (
        <>
          <div style={{ color: 'var(--red)' }}>Could not read {file?.name}: {error}</div>
          <OpenFile compact />
        </>
      )}
      <Link to="/replay" className="rp-btn">All recordings</Link>
    </div>
  )
}
