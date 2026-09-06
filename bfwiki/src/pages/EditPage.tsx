import { useEffect, useRef, useState } from 'react'
import { useNavigate, useSearchParams } from 'react-router-dom'
import { useQuery, useQueryClient } from '@tanstack/react-query'
import { Trash2, Image as ImageIcon } from 'lucide-react'
import { api } from '../api'
import { useAuth } from '../context/AuthContext'
import WikiMarkdown from '../components/WikiMarkdown'

export default function EditPage({ isNew = false }: { isNew?: boolean }) {
  const { user, loading: authLoading } = useAuth()
  const navigate = useNavigate()
  const queryClient = useQueryClient()
  const [params] = useSearchParams()
  const existingSlug = params.get('slug') ?? ''

  const { data: existing } = useQuery({
    queryKey: ['wiki', 'page', existingSlug],
    queryFn: () => api.wiki.get(existingSlug),
    enabled: !isNew && !!existingSlug,
  })

  const [slug, setSlug] = useState(isNew ? (params.get('slug') ?? '') : existingSlug)
  const [title, setTitle] = useState('')
  const [section, setSection] = useState('')
  const [order, setOrder] = useState(0)
  const [content, setContent] = useState('')
  const [saving, setSaving] = useState(false)
  const [deleting, setDeleting] = useState(false)
  const [uploading, setUploading] = useState(false)
  const [error, setError] = useState<string | null>(null)

  const textareaRef = useRef<HTMLTextAreaElement>(null)
  const fileInputRef = useRef<HTMLInputElement>(null)

  useEffect(() => {
    if (existing) {
      setTitle(existing.title)
      setSection(existing.section)
      setOrder(existing.order)
      setContent(existing.content)
    }
  }, [existing])

  // Not logged in / not admin -- bounce home. Rendered after hooks so the
  // hook order stays stable across renders.
  if (!authLoading && !user?.is_admin) {
    navigate('/login')
    return null
  }

  function insertAtCursor(text: string) {
    const el = textareaRef.current
    if (!el) {
      setContent(c => c + text)
      return
    }
    const start = el.selectionStart ?? content.length
    const end = el.selectionEnd ?? content.length
    const next = content.slice(0, start) + text + content.slice(end)
    setContent(next)
    // Restore focus + cursor just after the inserted text once React re-renders.
    requestAnimationFrame(() => {
      el.focus()
      const pos = start + text.length
      el.setSelectionRange(pos, pos)
    })
  }

  async function handleImageFiles(files: FileList | File[]) {
    const images = Array.from(files).filter(f => f.type.startsWith('image/'))
    if (images.length === 0) return
    setUploading(true)
    setError(null)
    try {
      for (const file of images) {
        const { url } = await api.wiki.uploadImage(file)
        const alt = file.name.replace(/\.[^.]+$/, '').replace(/[_-]+/g, ' ')
        insertAtCursor(`\n![${alt}](${url})\n`)
      }
    } catch (err) {
      setError(err instanceof Error ? err.message : 'Image upload failed')
    } finally {
      setUploading(false)
    }
  }

  async function handleSave() {
    if (!slug || !title || !section) {
      setError('Slug, title, and section are required.')
      return
    }
    setSaving(true)
    setError(null)
    try {
      await api.wiki.save(slug, { title, section, order, content })
      await queryClient.invalidateQueries({ queryKey: ['wiki'] })
      navigate(`/${slug}`)
    } catch (err) {
      setError(err instanceof Error ? err.message : 'Save failed')
    } finally {
      setSaving(false)
    }
  }

  async function handleDelete() {
    if (!existingSlug) return
    if (!confirm(`Delete "${title}"? This can't be undone.`)) return
    setDeleting(true)
    try {
      await api.wiki.delete(existingSlug)
      await queryClient.invalidateQueries({ queryKey: ['wiki'] })
      navigate('/')
    } catch (err) {
      setError(err instanceof Error ? err.message : 'Delete failed')
      setDeleting(false)
    }
  }

  return (
    <div>
      <div style={{ fontFamily: "'Bebas Neue', sans-serif", fontSize: '1.5rem', letterSpacing: '0.06em', color: 'var(--text)', marginBottom: '1.25rem' }}>
        {isNew ? 'NEW PAGE' : 'EDIT PAGE'}
      </div>

      <div style={{ display: 'flex', gap: 12, marginBottom: 12, flexWrap: 'wrap' }}>
        <div style={{ flex: '1 1 220px' }}>
          <label className="vs-label">Slug (URL path)</label>
          <input
            className="vs-input"
            value={slug}
            onChange={e => setSlug(e.target.value.trim())}
            placeholder="gameplay/my-new-topic"
            disabled={!isNew}
          />
        </div>
        <div style={{ flex: '1 1 220px' }}>
          <label className="vs-label">Section (sidebar group)</label>
          <input className="vs-input" value={section} onChange={e => setSection(e.target.value)} placeholder="Core Gameplay" />
        </div>
        <div style={{ width: 100 }}>
          <label className="vs-label">Order</label>
          <input className="vs-input" type="number" value={order} onChange={e => setOrder(Number(e.target.value))} />
        </div>
      </div>

      <div style={{ marginBottom: 12 }}>
        <label className="vs-label">Title</label>
        <input className="vs-input" value={title} onChange={e => setTitle(e.target.value)} placeholder="Page title" />
      </div>

      <div style={{ marginBottom: 12 }}>
        <div style={{ display: 'flex', alignItems: 'center', justifyContent: 'space-between', marginBottom: '0.3rem' }}>
          <label className="vs-label" style={{ margin: 0 }}>Content (Markdown)</label>
          <button
            type="button"
            className="vs-btn-outline"
            style={{ display: 'inline-flex', alignItems: 'center', gap: 6, padding: '4px 10px', borderRadius: 2, fontSize: '0.65rem', letterSpacing: '0.08em', fontFamily: "'Bebas Neue', sans-serif", cursor: uploading ? 'wait' : 'pointer' }}
            onClick={() => fileInputRef.current?.click()}
            disabled={uploading}
          >
            <ImageIcon size={12} /> {uploading ? 'UPLOADING…' : 'INSERT IMAGE'}
          </button>
          <input
            ref={fileInputRef}
            type="file"
            accept="image/*"
            multiple
            style={{ display: 'none' }}
            onChange={e => { if (e.target.files) handleImageFiles(e.target.files); e.target.value = '' }}
          />
        </div>
        <div style={{ display: 'grid', gridTemplateColumns: '1fr 1fr', gap: 12 }}>
          <textarea
            ref={textareaRef}
            className="vs-textarea"
            style={{ minHeight: 480 }}
            value={content}
            onChange={e => setContent(e.target.value)}
            onDragOver={e => e.preventDefault()}
            onDrop={e => { e.preventDefault(); handleImageFiles(e.dataTransfer.files) }}
            onPaste={e => {
              const files = Array.from(e.clipboardData.files)
              if (files.some(f => f.type.startsWith('image/'))) {
                e.preventDefault()
                handleImageFiles(files)
              }
            }}
            placeholder="# Heading&#10;&#10;Write markdown here… (drag & drop, paste, or use Insert Image for screenshots)"
          />
          <div className="wiki-prose" style={{ background: 'var(--bg-card)', border: '1px solid var(--border)', borderRadius: 3, padding: '1rem 1.25rem', overflowY: 'auto', maxHeight: 480, fontSize: '0.82rem' }}>
            <WikiMarkdown slug={slug}>{content || '*Preview appears here*'}</WikiMarkdown>
          </div>
        </div>
        <div style={{ fontSize: '0.65rem', color: 'var(--text-dim)', marginTop: 6 }}>
          Tip: drag an image file onto the editor, paste a screenshot from your clipboard, or click "Insert Image".
        </div>
      </div>

      {error && <div style={{ color: '#ef4444', fontSize: '0.78rem', marginBottom: 10 }}>{error}</div>}

      <div style={{ display: 'flex', gap: 10, alignItems: 'center' }}>
        <button className="vs-btn" onClick={handleSave} disabled={saving}>
          {saving ? 'SAVING…' : 'SAVE'}
        </button>
        <button className="vs-btn-ghost" style={{ padding: '6px 14px', borderRadius: 2, cursor: 'pointer' }} onClick={() => navigate(-1)}>
          Cancel
        </button>
        {!isNew && (
          <button
            className="vs-btn-danger"
            style={{ marginLeft: 'auto', display: 'inline-flex', alignItems: 'center', gap: 6, padding: '6px 14px', borderRadius: 2, cursor: 'pointer', fontSize: '0.72rem' }}
            onClick={handleDelete}
            disabled={deleting}
          >
            <Trash2 size={13} /> {deleting ? 'DELETING…' : 'DELETE PAGE'}
          </button>
        )}
      </div>
    </div>
  )
}
