/**
 * The kneeboard page as a PNG, for `Saved Games\DCS\Kneeboard`: the sheet is
 * rendered to SVG markup, its map tiles are inlined (an SVG drawn as an
 * image may not fetch anything itself) and it is drawn onto a canvas at
 * twice its size, 1536 × 2048.
 */
import { createElement } from 'react'
import { KneeboardSheet, SHEET_H, SHEET_W } from '../components/KneeboardSheet'
import type { KbTheme } from './kneeboardInk'
import type { RangeLive } from '../types'

const SCALE = 2

async function dataUrl(url: string): Promise<string | null> {
  try {
    const res = await fetch(url, { mode: 'cors', credentials: 'omit' })
    if (!res.ok) return null
    const blob = await res.blob()
    return await new Promise(resolve => {
      const r = new FileReader()
      r.onload = () => resolve(typeof r.result === 'string' ? r.result : null)
      r.onerror = () => resolve(null)
      r.readAsDataURL(blob)
    })
  } catch {
    return null
  }
}

export async function kneeboardPng(live: RangeLive, theme: KbTheme): Promise<Blob> {
  const { renderToStaticMarkup } = await import('react-dom/server')
  let svg = renderToStaticMarkup(createElement(KneeboardSheet, { live, theme }))
  const urls = [...new Set([...svg.matchAll(/href="(https:\/\/[^"]+)"/g)].map(m => m[1]))]
  const inlined = await Promise.all(urls.map(dataUrl))
  urls.forEach((u, i) => {
    svg = svg.split(`href="${u}"`).join(`href="${inlined[i] ?? ''}"`)
  })
  const src = URL.createObjectURL(new Blob([svg], { type: 'image/svg+xml' }))
  try {
    const img = new Image()
    await new Promise<void>((resolve, reject) => {
      img.onload = () => resolve()
      img.onerror = () => reject(new Error('the chart would not draw'))
      img.src = src
    })
    const canvas = document.createElement('canvas')
    canvas.width = SHEET_W * SCALE
    canvas.height = SHEET_H * SCALE
    const ctx = canvas.getContext('2d')
    if (!ctx) throw new Error('no canvas')
    ctx.drawImage(img, 0, 0, canvas.width, canvas.height)
    return await new Promise((resolve, reject) => canvas.toBlob(b => (b ? resolve(b) : reject(new Error('no PNG'))), 'image/png'))
  } finally {
    URL.revokeObjectURL(src)
  }
}
