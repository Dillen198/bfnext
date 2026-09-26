/**
 * `/result/:id` — the debrief. What the engine measured (left), the same
 * event drawn interactively from its track (right), and the rendered card
 * that went to Discord, with downloads for the card and the Tacview file.
 */
import { useState } from 'react'
import { Link, useParams } from 'react-router-dom'
import { useQuery } from '@tanstack/react-query'
import { Download, Link as LinkIcon } from '@icons'
import { api, apiUrl } from '../api'
import { ScoreChip } from '../components/Chips'
import { Panel } from '../components/Controls'
import { KindIcon } from '../components/KindIcon'
import { ErrorState, Loading } from '../components/States'
import { useToast } from '../context/ToastContext'
import { KIND_LABEL, airframe, fmtDateTime } from '../lib/format'
import { downloadUrl, saveBlob } from '../lib/download'
import { headline } from '../lib/headline'
import { Facts } from './debrief/Facts'
import { Interactive } from './debrief/Interactive'

export default function ResultPage() {
  const { id = '' } = useParams()
  const toast = useToast()
  const [busy, setBusy] = useState<'' | 'png' | 'acmi'>('')
  const [cardFailed, setCardFailed] = useState(false)
  const q = useQuery({ queryKey: ['result', id], queryFn: () => api.result(id), staleTime: 5 * 60_000 })

  if (q.isLoading) return <Loading label="Opening the debrief" />
  if (q.error || !q.data) {
    return (
      <div className="wrap page">
        <ErrorState error={q.error ?? new Error('not found')} retry={() => q.refetch()} />
        <p className="mt-4"><Link to="/results" className="underline">Back to results</Link></p>
      </div>
    )
  }
  const rec = q.data
  const kind = rec.result.kind
  const cardSvg = rec.card_svg ? apiUrl(rec.card_svg) : api.cardUrl(rec.id, 'svg')
  const cardPng = rec.card_png ? apiUrl(rec.card_png) : api.cardUrl(rec.id, 'png')
  const safeName = rec.id.replace(/[^\w.-]+/g, '_')

  async function downloadPng() {
    setBusy('png')
    try {
      const ok = await downloadUrl(cardPng, `${safeName}-card`)
      if (!ok) toast('No card image for this result yet.', false)
    } catch (e) {
      toast(`Could not download the card: ${e instanceof Error ? e.message : e}`, false)
    } finally {
      setBusy('')
    }
  }

  async function downloadAcmi() {
    setBusy('acmi')
    try {
      const b = await api.tacview(rec.id)
      if (!b) toast('No Tacview recording covers this result.', false)
      else saveBlob(b, `${safeName}.acmi`)
    } catch (e) {
      toast(`Could not fetch the Tacview file: ${e instanceof Error ? e.message : e}`, false)
    } finally {
      setBusy('')
    }
  }

  async function copyLink() {
    try {
      await navigator.clipboard.writeText(window.location.href)
      toast('Link copied.')
    } catch {
      toast('Could not copy: your browser blocked clipboard access.', false)
    }
  }

  return (
    <div className="wrap page">
      <div className="page-head">
        <div className="min-w-0">
          <div className="caps flex items-center gap-2">
            <KindIcon kind={kind} size={14} /> Debrief · {KIND_LABEL[kind]}
          </div>
          <h1 className="display mt-1">
            {rec.pilot.ucid ? <Link to={`/pilot/${encodeURIComponent(rec.pilot.ucid)}`} className="hover:underline">{rec.pilot.name}</Link> : rec.pilot.name}
          </h1>
          <p className="sub m-0 mt-1 text-[13px]">
            {airframe(rec.unit_type)}{rec.callsign ? ` · ${rec.callsign}` : ''} · {fmtDateTime(rec.ts)}
            <span className="dim"> · {rec.theatre}: {rec.mission_date} ({rec.mission_time})</span>
          </p>
        </div>
        <div className="actions">
          <ScoreChip score={rec.score} />
          <button className="btn-range" onClick={downloadPng} disabled={busy !== ''}>
            <Download size={14} /> {busy === 'png' ? 'Downloading…' : 'Card PNG'}
          </button>
          <button className="btn-range" onClick={downloadAcmi} disabled={busy !== ''}>
            <Download size={14} /> {busy === 'acmi' ? 'Fetching…' : 'Tacview'}
          </button>
          <button className="btn-range" onClick={copyLink}>
            <LinkIcon size={14} /> Copy link
          </button>
        </div>
      </div>

      <p className="mono text-[13px] muted -mt-2 mb-4">{rec.headline ?? headline(rec)}</p>

      <div className="grid gap-4 xl:grid-cols-[380px_minmax(0,1fr)]">
        <Panel title="What the range measured" className="self-start">
          <Facts rec={rec} />
        </Panel>
        <Panel title="Interactive debrief">
          <Interactive rec={rec} />
        </Panel>
      </div>

      <Panel title="Result card" className="mt-4" right={<span className="text-[12px] dim">the image posted to Discord</span>}>
        {cardFailed || !cardSvg ? (
          <div className="muted text-[13px] py-6 text-center">The card image is not available for this result.</div>
        ) : (
          <img
            src={cardSvg}
            alt={`Result card: ${rec.headline ?? headline(rec)}`}
            className="block w-full h-auto mx-auto rounded-[2px]"
            style={{ maxWidth: kind === 'bomb' ? 640 : 900 }}
            onError={() => setCardFailed(true)}
          />
        )}
      </Panel>
    </div>
  )
}
