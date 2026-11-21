// The weekly report poster as an image, for Export PNG and Post to Discord
// (shared/web/report/ReportStudio.tsx). html-to-image copies every computed
// style and the web fonts into an SVG the browser rasterises, so the PNG is
// the poster as drawn. At its own size: the page shows it scaled down, but
// the scale is on a wrapper, not on the poster.

import { toJpeg, toPng } from 'html-to-image'
import type { Snapshot } from '@shared/web/report/ReportStudio'

export const posterSnapshot: Snapshot = (el, { width, height, type }) => {
  const opts = { width, height, pixelRatio: 1, cacheBust: true, style: { margin: '0', transform: 'none' } }
  return type === 'png' ? toPng(el, opts) : toJpeg(el, { ...opts, quality: 0.92, backgroundColor: '#000' })
}
