// The admin REPORT page's body (`/admin/report` in bfweb and bfrsr-web): the
// weekly report poster for the selected server, drawn live from bfdb, with
// its dispatch and the Discord post text editable, Export PNG, and Post to
// Discord (through bfdb's OPS proxy to the FowlEngine bot, which posts to
// the server's report_channel). Each app hands in its own API calls and its
// html-to-image snapshot, so this file needs neither.

import { useEffect, useMemo, useRef, useState, type ChangeEvent, type CSSProperties, type RefObject } from 'react'
import { useQuery } from '@tanstack/react-query'
import {
  DISCORD_MAX, draftDispatch, MAX_IMAGE_B64, posterFilename, posterModel, postText, POSTER_SIZE, reportPostBody,
  type Dispatch, type ReportPostBody, type WeeklyReportResponse,
} from './weekly'
import { WeeklyPoster } from './WeeklyPoster'
import { usePosterFonts } from './fonts'
import { defaultLogo, siteFor } from './site'

export type { ReportPostBody }

/** GET /api/admin/ops/report/status: the bot's view of the server. */
export interface ReportScheduleStatus {
  ok?: boolean
  server: string | null
  scheduled: boolean
  weekday: string | null
  hour: number | null
  days: number | null
  next: string | null
  last_posted: string | null
  channel: string | null
}

export interface ReportApi {
  fetch: (days: number) => Promise<WeeklyReportResponse>
  post: (body: ReportPostBody) => Promise<{ ok?: boolean; message?: string; url?: string | null }>
  status?: () => Promise<ReportScheduleStatus>
}

/** The element as a data: URL of `type`, at its own size. */
export type Snapshot = (el: HTMLElement, opts: { width: number; height: number; type: 'png' | 'jpeg' }) => Promise<string>

const label: CSSProperties = { fontSize: '0.62rem', letterSpacing: '0.14em', textTransform: 'uppercase', color: 'var(--text-dim)' }
const field: CSSProperties = {
  display: 'block', width: '100%', boxSizing: 'border-box', background: 'var(--bg)', color: 'var(--text)',
  border: '1px solid var(--border)', borderRadius: 4, fontSize: '0.8rem', padding: '6px 8px', fontFamily: 'inherit',
}
const small: CSSProperties = { fontSize: '0.68rem', padding: '5px 12px' }

function useWidth(ref: RefObject<HTMLElement | null>): number {
  const [w, setW] = useState(0)
  useEffect(() => {
    const el = ref.current
    if (!el) return
    const ro = new ResizeObserver(() => setW(el.clientWidth))
    ro.observe(el)
    setW(el.clientWidth)
    return () => ro.disconnect()
  }, [ref])
  return w
}

function download(dataUrl: string, filename: string) {
  const a = document.createElement('a')
  a.href = dataUrl
  a.download = filename
  document.body.appendChild(a)
  a.click()
  a.remove()
}

const fmtWhen = (iso: string | null) => (iso ? new Date(iso).toUTCString().replace(/:\d\d GMT$/, 'Z') : '')

export function ReportStudio({ api, snapshot, fallback, canPost, instanceKey, printPath = '/report/print' }: {
  api: ReportApi
  snapshot: Snapshot
  /** The campaign's names when bfdb has none. */
  fallback: { blue: string; red: string; brand: string }
  /** OPS admins may post; others may still look and export. */
  canPost: boolean
  /** The selected server, for the query cache. */
  instanceKey: string
  /** The chromeless page the weekly schedule captures. */
  printPath?: string
}) {
  const [days, setDays] = useState(7)
  const q = useQuery({ queryKey: ['weekly-report', instanceKey, days], queryFn: () => api.fetch(days) })
  const status = useQuery({
    queryKey: ['weekly-report-status', instanceKey],
    queryFn: () => api.status!(),
    enabled: !!api.status && canPost,
    retry: false,
  })
  const model = useMemo(() => (q.data ? posterModel(q.data, fallback) : null), [q.data, fallback])
  const draft = useMemo(() => (model ? draftDispatch(model) : null), [model])
  const [edits, setEdits] = useState<Partial<Dispatch>>({})
  const dispatch: Dispatch | null = draft ? { ...draft, ...edits } : null
  const site = model ? siteFor(model) : undefined
  const draftText = model && dispatch ? postText(model, dispatch, site?.dashboard) : ''
  const [textEdit, setTextEdit] = useState<string | null>(null)
  const text = textEdit ?? draftText
  const fontsReady = usePosterFonts(model?.theme ?? 'levant')

  const box = useRef<HTMLDivElement>(null)
  const poster = useRef<HTMLDivElement>(null)
  const width = useWidth(box)
  const size = POSTER_SIZE[model?.theme ?? 'levant']
  const scale = width > 0 ? Math.min(1, width / size.width) : 0.5

  const [busy, setBusy] = useState<null | 'export' | 'post'>(null)
  const [msg, setMsg] = useState<{ ok: boolean; text: string; url?: string | null } | null>(null)

  const shoot = async (type: 'png' | 'jpeg') => {
    const el = poster.current?.firstElementChild as HTMLElement | null
    if (!el) throw new Error('the poster is not drawn yet')
    await document.fonts.ready
    return snapshot(el, { ...size, type })
  }
  const exportPng = async () => {
    if (!model) return
    setBusy('export'); setMsg(null)
    try {
      download(await shoot('png'), posterFilename(model))
    } catch (e) {
      setMsg({ ok: false, text: `Export failed: ${e instanceof Error ? e.message : String(e)}` })
    } finally { setBusy(null) }
  }
  const post = async () => {
    if (!model || !q.data) return
    const where = status.data?.channel ? `#${status.data.channel}` : "the server's report channel"
    if (!window.confirm(`Post this poster and its text to ${where} on Discord?\n\nEveryone in the channel will see it.`)) return
    setBusy('post'); setMsg(null)
    try {
      let url = await shoot('png')
      // a busy poster can come out too big for bfdb's proxy as a PNG
      if (url.length > MAX_IMAGE_B64) url = await shoot('jpeg')
      const r = await api.post(reportPostBody(q.data, model, text, url))
      setMsg({ ok: true, text: r.message || 'Posted.', url: r.url })
      void status.refetch()
    } catch (e) {
      setMsg({ ok: false, text: `Not posted: ${e instanceof Error ? e.message : String(e)}` })
    } finally { setBusy(null) }
  }

  const setD = (k: keyof Dispatch) => (e: ChangeEvent<HTMLInputElement | HTMLTextAreaElement>) =>
    setEdits(p => ({ ...p, [k]: e.target.value }))
  const st = status.data

  return (
    <div style={{ display: 'flex', flexWrap: 'wrap', gap: 16, alignItems: 'flex-start' }}>
      <div className="vs-card p-0" style={{ flex: '1 1 340px', maxWidth: 440, minWidth: 0 }}>
        <div className="flex items-center gap-2 px-4 pt-3.5 pb-2.5" style={{ borderBottom: '1px solid var(--border)' }}>
          <span style={label}>Weekly report</span>
          <select value={days} onChange={e => { setDays(Number(e.target.value)); setEdits({}); setTextEdit(null) }}
            aria-label="Days the report covers" style={{ ...field, width: 'auto', marginLeft: 'auto', padding: '3px 6px', fontSize: '0.7rem' }}>
            {[7, 14, 3, 1].map(d => <option key={d} value={d}>last {d === 1 ? 'day' : `${d} days`}</option>)}
          </select>
          <button type="button" className="vs-btn-ghost" style={small} onClick={() => void q.refetch()} disabled={q.isFetching}>
            {q.isFetching ? '…' : 'REFRESH'}
          </button>
        </div>
        {q.isError && <p role="alert" style={{ padding: '12px 16px', color: 'var(--red)', fontSize: '0.75rem' }}>
          {q.error instanceof Error ? q.error.message : 'The report did not load.'} This needs a bfdb with /api/report/weekly.
        </p>}
        {q.data && !q.data.report && <p style={{ padding: '12px 16px', color: 'var(--text-dim)', fontSize: '0.75rem' }}>
          This server has no round yet: the poster shows an empty board.
        </p>}
        {dispatch && (
          <div style={{ padding: '12px 16px', display: 'grid', gap: 10 }}>
            <label style={{ display: 'grid', gap: 4 }}><span style={label}>Headline</span>
              <input value={dispatch.headline} onChange={setD('headline')} maxLength={60} style={field} /></label>
            <label style={{ display: 'grid', gap: 4 }}><span style={label}>Standfirst</span>
              <input value={dispatch.standfirst} onChange={setD('standfirst')} maxLength={160} style={field} /></label>
            <label style={{ display: 'grid', gap: 4 }}><span style={label}>Dispatch (blank line between paragraphs)</span>
              <textarea value={dispatch.body} onChange={setD('body')} rows={9} spellCheck style={{ ...field, resize: 'vertical', lineHeight: 1.45 }} /></label>
            <label style={{ display: 'grid', gap: 4 }}>
              <span style={label}>Discord post text · {text.length}/{DISCORD_MAX}</span>
              <textarea value={text} onChange={e => setTextEdit(e.target.value)} rows={7} spellCheck maxLength={DISCORD_MAX}
                style={{ ...field, resize: 'vertical', lineHeight: 1.45, fontFamily: 'var(--font-mono, monospace)', fontSize: '0.72rem' }} /></label>
            <div className="flex items-center gap-2" style={{ flexWrap: 'wrap' }}>
              {(Object.keys(edits).length > 0 || textEdit !== null) && (
                <button type="button" className="vs-btn-ghost" style={small} onClick={() => { setEdits({}); setTextEdit(null) }}>RESET TEXT</button>
              )}
              <button type="button" className="vs-btn-outline" style={{ ...small, marginLeft: 'auto' }} onClick={() => void exportPng()}
                disabled={!model || busy !== null || !fontsReady}>{busy === 'export' ? 'EXPORTING…' : 'EXPORT PNG'}</button>
              <button type="button" className="vs-btn" style={small} onClick={() => void post()}
                disabled={!model || busy !== null || !canPost || !fontsReady || !text.trim()}
                title={canPost ? 'Post the poster and the text to the server’s report channel' : 'Posting needs the OPS role'}>
                {busy === 'post' ? 'POSTING…' : 'POST TO DISCORD'}</button>
            </div>
            {msg && (
              <p role={msg.ok ? 'status' : 'alert'} style={{ margin: 0, fontSize: '0.72rem', color: msg.ok ? 'var(--accent-bright)' : 'var(--red)' }}>
                {msg.text} {msg.url && <a href={msg.url} target="_blank" rel="noreferrer" style={{ color: 'inherit' }}>Open in Discord</a>}
              </p>
            )}
          </div>
        )}
        {canPost && api.status && (
          <div style={{ padding: '10px 16px 14px', borderTop: '1px solid var(--border)', fontSize: '0.7rem', color: 'var(--text-dim)', lineHeight: 1.5 }}>
            <div style={label}>Schedule</div>
            {status.isError && <>The bot did not answer ({status.error instanceof Error ? status.error.message : 'error'}).</>}
            {st && (
              <>
                Posts go to {st.channel ? <b style={{ color: 'var(--text)' }}>#{st.channel}</b> : <b style={{ color: 'var(--red)' }}>no channel yet (set report_channel)</b>}.{' '}
                {st.scheduled
                  ? <>Weekly on {st.weekday} at {String(st.hour).padStart(2, '0')}:00Z, covering {st.days} days. Next: {fmtWhen(st.next)}.</>
                  : <>No weekly schedule: set <code>weekly_report: {'{enabled: true}'}</code> in this server's section of fowlengine.yaml.</>}
                {st.last_posted && <> Last scheduled post: {fmtWhen(st.last_posted)}.</>}
              </>
            )}
            {q.data && (
              <div style={{ marginTop: 4 }}>
                The schedule draws <a href={`${printPath}?instance=${encodeURIComponent(q.data.instance)}&days=${days}`} target="_blank" rel="noreferrer" style={{ color: 'var(--accent-bright)' }}>this page</a> with the automatic dispatch.
              </div>
            )}
          </div>
        )}
      </div>

      <div ref={box} style={{ flex: '3 1 600px', minWidth: 0 }}>
        {model && dispatch ? (
          <div style={{ width: size.width * scale, height: size.height * scale, overflow: 'hidden', boxShadow: '0 6px 24px rgba(0,0,0,.45)' }}>
            <div ref={poster} style={{ width: size.width, height: size.height, transform: `scale(${scale})`, transformOrigin: '0 0' }}>
              <WeeklyPoster model={model} dispatch={dispatch} site={site} logo={defaultLogo(model)} />
            </div>
          </div>
        ) : (
          <div className="vs-card" style={{ padding: 24, color: 'var(--text-dim)', fontSize: '0.8rem' }}>{q.isLoading ? 'Loading the week…' : 'No report.'}</div>
        )}
      </div>
    </div>
  )
}
