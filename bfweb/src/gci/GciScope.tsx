import { useEffect, useRef, useCallback } from 'react'
import { geoToPpi } from './geo'
import { drawTerrainShadows } from './terrainShadow'
import type { GciAnchor, GciTrack, WsGciMsg } from './types'
import { clsLabel, iffLabel, srcLabel } from './types'
import type { Objective } from '../api'

const COL_FRIEND = '#4a8fd4'
const COL_HOSTILE = '#cc4444'
const COL_UNKNOWN = '#c9a227'
const COL_GRID = 'rgba(106, 171, 31, 0.22)'
const COL_TEXT = '#8ec83f'
const COL_DIM = 'rgba(142, 200, 63, 0.45)'

function trackColor(t: GciTrack): string {
  const k = iffLabel(t.iff)
  if (k === 'friendly') return COL_FRIEND
  if (k === 'hostile') return COL_HOSTILE
  return COL_UNKNOWN
}

export interface GciScopeProps {
  msg: WsGciMsg | null
  anchor: GciAnchor | null
  tracks: GciTrack[]
  rangeNm: number
  showDonors: boolean
  showTerrain: boolean
  showJamZones: boolean
  showTags: boolean
  showFogOfWar: boolean
  minAlt: number
  maxAlt: number
  selectedId: number | null
  focusDonorIndex: number | null
  objectives: Objective[]
  onSelect: (id: number | null) => void
  onFocusDonor: (index: number) => void
}

export default function GciScope({
  msg,
  anchor,
  tracks,
  rangeNm,
  showDonors,
  showTerrain,
  showJamZones,
  showTags,
  showFogOfWar,
  minAlt,
  maxAlt,
  selectedId,
  focusDonorIndex,
  objectives,
  onSelect,
  onFocusDonor,
}: GciScopeProps) {
  const canvasRef = useRef<HTMLCanvasElement>(null)
  const containerRef = useRef<HTMLDivElement>(null)
  const requestRef = useRef<number>(0)
  const historyRef = useRef<Map<number, { x: number; y: number; t: number }[]>>(new Map())

  const paint = useCallback(() => {
    const canvas = canvasRef.current
    const container = containerRef.current
    if (!canvas || !container || !anchor) return

    const dpr = window.devicePixelRatio || 1
    const w = container.clientWidth
    const h = container.clientHeight
    if (w < 10 || h < 10) return

    canvas.width = w * dpr
    canvas.height = h * dpr
    canvas.style.width = `${w}px`
    canvas.style.height = `${h}px`

    const ctx = canvas.getContext('2d')
    if (!ctx) return
    ctx.setTransform(dpr, 0, 0, dpr, 0, 0)

    const cx = w / 2
    const cy = h / 2
    const pxPerNm = (Math.min(w, h) * 0.45) / rangeNm

    ctx.fillStyle = '#060806'
    ctx.fillRect(0, 0, w, h)

    ctx.strokeStyle = COL_GRID
    ctx.lineWidth = 1
    ctx.font = '10px JetBrains Mono, Consolas, monospace'
    ctx.fillStyle = COL_DIM

    for (let r = 1; r <= rangeNm; r += rangeNm <= 50 ? 10 : rangeNm <= 100 ? 25 : 50) {
      if (r > rangeNm) break
      ctx.beginPath()
      ctx.arc(cx, cy, r * pxPerNm, 0, Math.PI * 2)
      ctx.stroke()
      ctx.fillText(`${r}`, cx + 4, cy - r * pxPerNm + 12)
    }

    for (let a = 0; a < 360; a += 30) {
      const rad = (a * Math.PI) / 180
      ctx.beginPath()
      ctx.moveTo(cx, cy)
      ctx.lineTo(cx + Math.sin(rad) * rangeNm * pxPerNm, cy - Math.cos(rad) * rangeNm * pxPerNm)
      ctx.stroke()
      if (a % 90 === 0) {
        const lx = cx + Math.sin(rad) * (rangeNm * pxPerNm + 14)
        const ly = cy - Math.cos(rad) * (rangeNm * pxPerNm + 14)
        ctx.fillText(['N', 'E', 'S', 'W'][a / 90] ?? '', lx - 4, ly + 4)
      }
    }

    if (!msg) {
      ctx.fillStyle = COL_DIM
      ctx.font = '12px JetBrains Mono, Consolas, monospace'
      ctx.fillText('AWAITING GCI PICTURE…', cx - 72, cy)
      return
    }

    const { donors, terrain, jam_zones } = msg

    if (showFogOfWar) {
      ctx.save()
      ctx.fillStyle = 'rgba(2, 4, 2, 0.85)'
      ctx.fillRect(0, 0, w, h)
      ctx.globalCompositeOperation = 'destination-out'
      for (const d of donors) {
        const p = geoToPpi(d.lat, d.lon, anchor.lat, anchor.lon, cx, cy, pxPerNm)
        const rPx = (d.range_m / 1852) * pxPerNm
        const fogGrad = ctx.createRadialGradient(p.x, p.y, rPx * 0.8, p.x, p.y, rPx)
        fogGrad.addColorStop(0, 'rgba(0, 0, 0, 1)')
        fogGrad.addColorStop(1, 'rgba(0, 0, 0, 0)')
        ctx.beginPath()
        ctx.arc(p.x, p.y, rPx, 0, Math.PI * 2)
        ctx.fillStyle = fogGrad
        ctx.fill()
      }
      ctx.restore()
    }

    if (showJamZones) {
      if (jam_zones && jam_zones.length > 0) {
        for (const z of jam_zones) {
          const p = geoToPpi(z.lat, z.lon, anchor.lat, anchor.lon, cx, cy, pxPerNm)
          const rPx = z.radius_nm * pxPerNm
          const a = 0.08 + (z.strength / 100) * 0.12
          ctx.fillStyle = `rgba(160, 80, 200, ${a})`
          ctx.beginPath()
          ctx.arc(p.x, p.y, rPx, 0, Math.PI * 2)
          ctx.fill()
          
          // Jamming noise effect
          ctx.save()
          ctx.clip()
          const noiseCount = Math.floor(rPx * rPx * 0.05 * (z.strength / 100))
          for (let i = 0; i < noiseCount; i++) {
            const nx = p.x - rPx + Math.random() * rPx * 2
            const ny = p.y - rPx + Math.random() * rPx * 2
            ctx.fillStyle = `rgba(220, 200, 255, ${Math.random() * 0.4})`
            ctx.fillRect(nx, ny, Math.random() * 3, Math.random() * 3)
          }
          ctx.restore()

          ctx.strokeStyle = `rgba(160, 80, 200, ${a + 0.15})`
          ctx.setLineDash([6, 8])
          ctx.stroke()
          ctx.setLineDash([])
        }
      }

      for (const t of tracks) {
        if (t.alt_ft < minAlt || t.alt_ft > maxAlt) continue
        const jam = t.jam ?? 0;
        if (jam > 10) {
          const p = geoToPpi(t.lat, t.lon, anchor.lat, anchor.lon, cx, cy, pxPerNm)
          const rPx = (15 + (jam / 100) * 15) * pxPerNm
          const a = 0.06 + (jam / 100) * 0.12
          ctx.fillStyle = `rgba(190, 60, 180, ${a})`
          ctx.beginPath()
          ctx.arc(p.x, p.y, rPx, 0, Math.PI * 2)
          ctx.fill()
          
          // Jamming noise effect
          ctx.save()
          ctx.clip()
          const noiseCount = Math.floor(rPx * rPx * 0.03 * (jam / 100))
          for (let i = 0; i < noiseCount; i++) {
            const nx = p.x - rPx + Math.random() * rPx * 2
            const ny = p.y - rPx + Math.random() * rPx * 2
            ctx.fillStyle = `rgba(255, 180, 255, ${Math.random() * 0.5})`
            ctx.fillRect(nx, ny, Math.random() * 4, Math.random() * 2)
          }
          ctx.restore()

          ctx.strokeStyle = `rgba(190, 60, 180, ${a + 0.1})`
          ctx.setLineDash([4, 6])
          ctx.stroke()
          ctx.setLineDash([])
        }
      }
    }

    if (showTerrain && terrain && terrain.length > 0) {
      drawTerrainShadows(ctx, terrain, anchor.lat, anchor.lon, cx, cy, pxPerNm, rangeNm)
    }

    if (showDonors) {
      for (let i = 0; i < donors.length; i++) {
        const d = donors[i]!
        const p = geoToPpi(d.lat, d.lon, anchor.lat, anchor.lon, cx, cy, pxPerNm)
        const rPx = (d.range_m / 1852) * pxPerNm
        const focused = focusDonorIndex === i
        const isBlue = d.side === 'Blue' || d.side === 'blue'
        ctx.strokeStyle = focused
          ? isBlue ? 'rgba(74,143,212,0.85)' : 'rgba(204,68,68,0.85)'
          : isBlue ? 'rgba(74,143,212,0.2)' : 'rgba(204,68,68,0.2)'
        ctx.lineWidth = focused ? 2 : 1
        ctx.beginPath()
        ctx.arc(p.x, p.y, rPx, 0, Math.PI * 2)
        ctx.stroke()
        if (focused) {
          ctx.beginPath()
          ctx.arc(p.x, p.y, 5, 0, Math.PI * 2)
          ctx.fillStyle = isBlue ? COL_FRIEND : COL_HOSTILE
          ctx.fill()
        }
      }
      ctx.lineWidth = 1
    }

    // Draw objectives
    for (const obj of objectives) {
      const p = geoToPpi(obj.lat, obj.lon, anchor.lat, anchor.lon, cx, cy, pxPerNm)
      if (p.rngNm > rangeNm * 1.5) continue // Skip far offscreen
      
      const rPx = 10 // Fixed size for objective marks
      ctx.strokeStyle = obj.owner === 'Blue' ? COL_FRIEND : obj.owner === 'Red' ? COL_HOSTILE : COL_UNKNOWN
      ctx.lineWidth = 1.5
      ctx.setLineDash([4, 4])
      ctx.beginPath()
      ctx.arc(p.x, p.y, rPx, 0, Math.PI * 2)
      ctx.stroke()
      ctx.setLineDash([])
      
      if (showTags) {
        ctx.fillStyle = ctx.strokeStyle
        ctx.font = '9px JetBrains Mono, Consolas, monospace'
        ctx.fillText(obj.name, p.x + rPx + 4, p.y + 3)
      }
    }

    // PPI center marker (sensor / bullseye / track focus)
    ctx.strokeStyle = 'rgba(201, 162, 39, 0.7)'
    ctx.beginPath()
    ctx.arc(cx, cy, 6, 0, Math.PI * 2)
    ctx.stroke()
    ctx.beginPath()
    ctx.moveTo(cx - 10, cy)
    ctx.lineTo(cx + 10, cy)
    ctx.moveTo(cx, cy - 10)
    ctx.lineTo(cx, cy + 10)
    ctx.stroke()

    ctx.fillStyle = COL_TEXT
    ctx.font = '9px JetBrains Mono, Consolas, monospace'
    ctx.fillText(`PPI ⊙ ${anchor.label}`, 8, h - 8)

    const now = performance.now()
    const sweepPeriod = 4500 // 4.5 seconds per 360 rotation
    const sweepAngle = ((now % sweepPeriod) / sweepPeriod) * Math.PI * 2
    
    // Radar sweep line and trailing phosphor gradient
    ctx.save()
    ctx.translate(cx, cy)
    ctx.rotate(sweepAngle)
    ctx.beginPath()
    ctx.moveTo(0, 0)
    ctx.lineTo(0, -rangeNm * pxPerNm)
    ctx.strokeStyle = 'rgba(160, 255, 80, 0.85)'
    ctx.lineWidth = 1.5
    ctx.stroke()
    
    ctx.beginPath()
    ctx.moveTo(0, 0)
    ctx.arc(0, 0, rangeNm * pxPerNm, -Math.PI / 2, -Math.PI / 2 - 0.6, true)
    ctx.closePath()
    const sweepGrad = ctx.createLinearGradient(0, -rangeNm * pxPerNm, -rangeNm * pxPerNm * 0.6, 0)
    sweepGrad.addColorStop(0, 'rgba(140, 220, 80, 0.35)')
    sweepGrad.addColorStop(1, 'rgba(140, 220, 80, 0.0)')
    ctx.fillStyle = sweepGrad
    ctx.fill()
    ctx.restore()

    for (const t of tracks) {
      if (t.alt_ft < minAlt || t.alt_ft > maxAlt) continue
      const p = geoToPpi(t.lat, t.lon, anchor.lat, anchor.lon, cx, cy, pxPerNm)
      if (p.rngNm > rangeNm) continue

      const col = trackColor(t)
      const sel = t.id === selectedId
      const contested = t.contested || (t.jam ?? 0) > 25
      const hdgRad = ((t.hdg - 90) * Math.PI) / 180
      const lead = 0.35 * pxPerNm * Math.min(t.spd_kts, 600) / 60

      const trackPpiAngle = Math.atan2(p.y - cy, p.x - cx)
      const sweepGlobalAngle = sweepAngle - Math.PI / 2
      let angleDiff = sweepGlobalAngle - trackPpiAngle
      while (angleDiff < 0) angleDiff += Math.PI * 2
      while (angleDiff >= Math.PI * 2) angleDiff -= Math.PI * 2
      
      const trackFade = Math.max(0.15, 1.0 - (angleDiff / (Math.PI * 2)) * 1.5)

      // Update and draw snail trail (history)
      let hist = historyRef.current.get(t.id)
      if (!hist) {
        hist = []
        historyRef.current.set(t.id, hist)
      }
      const lastHist = hist[hist.length - 1]
      if (!lastHist || now - lastHist.t > 3000) {
        hist.push({ x: p.x, y: p.y, t: now })
        if (hist.length > 5) hist.shift()
      }

      ctx.globalAlpha = trackFade * 0.7
      for (let i = 0; i < hist.length - 1; i++) {
        const pt = hist[i]!
        ctx.beginPath()
        ctx.arc(pt.x, pt.y, 1.5, 0, Math.PI * 2)
        ctx.fillStyle = col
        ctx.fill()
      }

      ctx.strokeStyle = col
      ctx.fillStyle = col
      ctx.globalAlpha = (t.stale ? 0.45 : contested ? 0.55 : 1) * trackFade

      ctx.beginPath()
      ctx.moveTo(p.x, p.y)
      ctx.lineTo(p.x + Math.cos(hdgRad) * lead, p.y + Math.sin(hdgRad) * lead)
      ctx.stroke()

      const sz = sel ? 7 : 5
      ctx.beginPath()
      if (iffLabel(t.iff) === 'hostile') {
        ctx.rect(p.x - sz, p.y - sz, sz * 2, sz * 2)
      } else if (iffLabel(t.iff) === 'friendly') {
        ctx.arc(p.x, p.y, sz, 0, Math.PI * 2)
      } else {
        ctx.moveTo(p.x, p.y - sz)
        ctx.lineTo(p.x + sz, p.y)
        ctx.lineTo(p.x, p.y + sz)
        ctx.lineTo(p.x - sz, p.y)
        ctx.closePath()
      }
      if (sel) {
        ctx.fill()
      } else {
        ctx.stroke()
      }

      if (contested) {
        ctx.strokeStyle = 'rgba(200, 100, 220, 0.75)'
        ctx.setLineDash([3, 4])
        ctx.beginPath()
        ctx.arc(p.x, p.y, sz + 5, 0, Math.PI * 2)
        ctx.stroke()
        ctx.setLineDash([])
      }

      if (showTags && (sel || p.rngNm < rangeNm * 0.6)) {
        ctx.globalAlpha = 1
        ctx.fillStyle = 'rgba(8,11,6,0.92)'
        const tag = `${t.tn} F${Math.round(t.alt_ft / 100)} N${t.spd_kts} ${t.brg.toString().padStart(3, '0')}/${t.rng_nm}`
        const tag2 = `${clsLabel(t.cls)} ${srcLabel(t.src)}${contested ? ' J' : ''}${t.stale ? ' *' : ''} ${t.age}s`
        const tw = Math.max(ctx.measureText(tag).width, ctx.measureText(tag2).width) + 8
        const tx = p.x + 10
        const ty = p.y - 18
        ctx.fillRect(tx, ty, tw, 22)
        ctx.strokeStyle = col
        ctx.strokeRect(tx, ty, tw, 22)
        ctx.fillStyle = col
        ctx.fillText(tag, tx + 4, ty + 10)
        ctx.fillStyle = COL_DIM
        ctx.fillText(tag2, tx + 4, ty + 20)
      }

      if (sel && msg.bull && msg.bull.length > 0) {
        const bull = msg.bull[0]!
        const r = 6371000
        const p1 = (bull.lat * Math.PI) / 180
        const p2 = (t.lat * Math.PI) / 180
        const dp = ((t.lat - bull.lat) * Math.PI) / 180
        const dl = ((t.lon - bull.lon) * Math.PI) / 180
        const aDist = Math.sin(dp / 2) ** 2 + Math.cos(p1) * Math.cos(p2) * Math.sin(dl / 2) ** 2
        const rngNm = Math.round((2 * r * Math.asin(Math.sqrt(aDist))) / 1852)
        
        const y = Math.sin(dl) * Math.cos(p2)
        const x = Math.cos(p1) * Math.sin(p2) - Math.sin(p1) * Math.cos(p2) * Math.cos(dl)
        const brg = Math.round(((Math.atan2(y, x) * 180) / Math.PI + 360) % 360)
        
        const aspectAngle = Math.abs(t.hdg - brg) % 360
        let aspectStr = 'UNK'
        if (aspectAngle < 30 || aspectAngle > 330) aspectStr = 'DRAG'
        else if (aspectAngle > 150 && aspectAngle < 210) aspectStr = 'HOT'
        else aspectStr = 'FLNK'
        
        const alt = Math.round(t.alt_ft / 1000)
        const braaText = `BRAA ${brg.toString().padStart(3, '0')} / ${rngNm} / ${alt}K / ${aspectStr}`
        
        ctx.fillStyle = 'rgba(255, 220, 50, 0.95)'
        ctx.fillText(braaText, p.x + 10, p.y + 35)
      }

      ctx.globalAlpha = 1
    }
    
    // Draw CRT vignette/overlay for aesthetics
    const crtGrad = ctx.createRadialGradient(cx, cy, rangeNm * pxPerNm * 0.7, cx, cy, rangeNm * pxPerNm)
    crtGrad.addColorStop(0, 'rgba(0, 0, 0, 0)')
    crtGrad.addColorStop(1, 'rgba(0, 10, 0, 0.4)')
    ctx.fillStyle = crtGrad
    ctx.beginPath()
    ctx.arc(cx, cy, rangeNm * pxPerNm, 0, Math.PI * 2)
    ctx.fill()

  }, [
    msg,
    anchor,
    tracks,
    rangeNm,
    showDonors,
    showTerrain,
    showJamZones,
    showTags,
    showFogOfWar,
    minAlt,
    maxAlt,
    selectedId,
    focusDonorIndex,
    objectives,
  ])

  useEffect(() => {
    let cancel = false
    const loop = () => {
      if (cancel) return
      paint()
      requestRef.current = requestAnimationFrame(loop)
    }
    requestRef.current = requestAnimationFrame(loop)
    
    const ro = new ResizeObserver(() => paint())
    if (containerRef.current) ro.observe(containerRef.current)
    
    return () => {
      cancel = true
      if (requestRef.current) cancelAnimationFrame(requestRef.current)
      ro.disconnect()
    }
  }, [paint])

  const onClick = (e: React.MouseEvent<HTMLCanvasElement>) => {
    if (!msg || !anchor || !canvasRef.current || !containerRef.current) return
    const rect = canvasRef.current.getBoundingClientRect()
    const x = e.clientX - rect.left
    const y = e.clientY - rect.top
    const w = containerRef.current.clientWidth
    const h = containerRef.current.clientHeight
    const cx = w / 2
    const cy = h / 2
    const pxPerNm = (Math.min(w, h) * 0.45) / rangeNm

    let bestTrack: { id: number; d: number } | null = null
    for (const t of tracks) {
      const p = geoToPpi(t.lat, t.lon, anchor.lat, anchor.lon, cx, cy, pxPerNm)
      const d = Math.hypot(p.x - x, p.y - y)
      if (d < 16 && (!bestTrack || d < bestTrack.d)) bestTrack = { id: t.id, d }
    }
    if (bestTrack) {
      onSelect(bestTrack.id)
      return
    }

    let bestDonor: { i: number; d: number } | null = null
    for (let i = 0; i < msg.donors.length; i++) {
      const d = msg.donors[i]!
      const p = geoToPpi(d.lat, d.lon, anchor.lat, anchor.lon, cx, cy, pxPerNm)
      const dist = Math.hypot(p.x - x, p.y - y)
      if (dist < 14 && (!bestDonor || dist < bestDonor.d)) bestDonor = { i, d: dist }
    }
    if (bestDonor) {
      onFocusDonor(bestDonor.i)
    }
  }

  return (
    <div ref={containerRef} className="gci-scope-wrap">
      <canvas ref={canvasRef} className="gci-scope-canvas" onClick={onClick} />
    </div>
  )
}
