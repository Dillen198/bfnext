import { StrictMode } from 'react'
import { createRoot } from 'react-dom/client'
import './index.css'
import App from './App.tsx'
import { ThemeProvider } from './context/ThemeContext'
import { campaign, campaignDefaults, type CampaignConfig } from './config/campaign'
import { API_ROOT } from './api'

// ── Camo patterns (SVG data URIs) ────────────────────────────────────────────

const CAMO_PATTERNS: Record<string, (opacity: number) => string> = {
  digital: (op) => {
    const o = (hex: string, a: number) => hex + Math.round(a * op * 255).toString(16).padStart(2, '0')
    const svg = `<svg xmlns='http://www.w3.org/2000/svg' width='128' height='128'>
      <rect x='0'   y='0'   width='32' height='16' fill='${o('#1b2116', 1)}'/>
      <rect x='40'  y='0'   width='20' height='24' fill='${o('#141910', 1)}'/>
      <rect x='72'  y='0'   width='28' height='12' fill='${o('#222b1a', 1)}'/>
      <rect x='108' y='4'   width='20' height='20' fill='${o('#1b2116', 1)}'/>
      <rect x='0'   y='20'  width='16' height='28' fill='${o('#222b1a', 1)}'/>
      <rect x='24'  y='16'  width='36' height='16' fill='${o('#0d1009', 1)}'/>
      <rect x='68'  y='16'  width='16' height='32' fill='${o('#141910', 1)}'/>
      <rect x='92'  y='20'  width='28' height='12' fill='${o('#222b1a', 1)}'/>
      <rect x='0'   y='56'  width='24' height='20' fill='${o('#1b2116', 1)}'/>
      <rect x='32'  y='48'  width='20' height='28' fill='${o('#0d1009', 1)}'/>
      <rect x='60'  y='52'  width='32' height='16' fill='${o('#141910', 1)}'/>
      <rect x='100' y='48'  width='28' height='24' fill='${o('#222b1a', 1)}'/>
      <rect x='8'   y='80'  width='28' height='16' fill='${o('#141910', 1)}'/>
      <rect x='44'  y='76'  width='16' height='28' fill='${o('#1b2116', 1)}'/>
      <rect x='68'  y='72'  width='32' height='20' fill='${o('#0d1009', 1)}'/>
      <rect x='108' y='76'  width='20' height='28' fill='${o('#141910', 1)}'/>
      <rect x='0'   y='100' width='36' height='28' fill='${o('#222b1a', 1)}'/>
      <rect x='44'  y='108' width='24' height='20' fill='${o('#1b2116', 1)}'/>
      <rect x='76'  y='96'  width='20' height='32' fill='${o('#141910', 1)}'/>
      <rect x='104' y='104' width='24' height='24' fill='${o('#0d1009', 1)}'/>
    </svg>`
    return `url("data:image/svg+xml,${encodeURIComponent(svg)}")`
  },

  hex: (op) => {
    const c = `rgba(106,171,31,${op})`
    const svg = `<svg xmlns='http://www.w3.org/2000/svg' width='60' height='52'>
      <polygon points='30,1 59,16 59,36 30,51 1,36 1,16' fill='none' stroke='${c}' stroke-width='0.8'/>
      <polygon points='0,26 14,18 14,34' fill='none' stroke='${c}' stroke-width='0.8'/>
      <polygon points='60,26 46,18 46,34' fill='none' stroke='${c}' stroke-width='0.8'/>
    </svg>`
    return `url("data:image/svg+xml,${encodeURIComponent(svg)}")`
  },

  terrain: (op) => {
    const c = `rgba(106,171,31,${op})`
    const svg = `<svg xmlns='http://www.w3.org/2000/svg' width='120' height='120'>
      <path d='M0,60 Q20,40 40,60 Q60,80 80,60 Q100,40 120,60' fill='none' stroke='${c}' stroke-width='0.7'/>
      <path d='M0,40 Q20,20 40,40 Q60,60 80,40 Q100,20 120,40' fill='none' stroke='${c}' stroke-width='0.7'/>
      <path d='M0,80 Q20,60 40,80 Q60,100 80,80 Q100,60 120,80' fill='none' stroke='${c}' stroke-width='0.7'/>
      <path d='M0,20 Q20,0  40,20 Q60,40 80,20 Q100,0  120,20' fill='none' stroke='${c}' stroke-width='0.5'/>
      <path d='M0,100 Q20,80 40,100 Q60,120 80,100 Q100,80 120,100' fill='none' stroke='${c}' stroke-width='0.5'/>
    </svg>`
    return `url("data:image/svg+xml,${encodeURIComponent(svg)}")`
  },

  multicam: (op) => {
    const svg = `<svg xmlns='http://www.w3.org/2000/svg' width='200' height='200'>
      <ellipse cx='40'  cy='30'  rx='35' ry='18' fill='rgba(27,33,22,${op})'/>
      <ellipse cx='120' cy='20'  rx='28' ry='14' fill='rgba(34,43,26,${op})'/>
      <ellipse cx='170' cy='50'  rx='22' ry='30' fill='rgba(20,25,16,${op})'/>
      <ellipse cx='60'  cy='80'  rx='40' ry='16' fill='rgba(34,43,26,${op})'/>
      <ellipse cx='150' cy='90'  rx='30' ry='20' fill='rgba(27,33,22,${op})'/>
      <ellipse cx='20'  cy='130' rx='18' ry='28' fill='rgba(20,25,16,${op})'/>
      <ellipse cx='90'  cy='140' rx='45' ry='18' fill='rgba(34,43,26,${op})'/>
      <ellipse cx='170' cy='150' rx='26' ry='22' fill='rgba(27,33,22,${op})'/>
      <ellipse cx='50'  cy='180' rx='38' ry='16' fill='rgba(20,25,16,${op})'/>
      <ellipse cx='140' cy='185' rx='30' ry='14' fill='rgba(34,43,26,${op})'/>
    </svg>`
    return `url("data:image/svg+xml,${encodeURIComponent(svg)}")`
  },
}

// ── Background & pattern injection ────────────────────────────────────────────

function applyBackground(cfg: CampaignConfig) {
  let el = document.getElementById('campaign-bg-style') as HTMLStyleElement | null
  if (!el) {
    el = document.createElement('style')
    el.id = 'campaign-bg-style'
    document.head.appendChild(el)
  }

  const layers: string[] = []
  const sizes:  string[] = []
  const blends: string[] = []

  // Camo pattern layer (on top)
  const patternFn = CAMO_PATTERNS[cfg.camoPattern]
  if (patternFn && cfg.camoPattern !== 'none') {
    layers.push(patternFn(cfg.camoOpacity))
    sizes.push(cfg.camoPattern === 'digital' ? '128px 128px' : cfg.camoPattern === 'hex' ? '60px 52px' : cfg.camoPattern === 'terrain' ? '120px 120px' : '200px 200px')
    blends.push('normal')
  }

  // Background image layer (below pattern)
  if (cfg.backgroundImage) {
    layers.push(`url('${cfg.backgroundImage}')`)
    sizes.push(cfg.backgroundImageSize)
    blends.push(cfg.backgroundImageBlend)
  }

  const hasLayers = layers.length > 0
  el.textContent = hasLayers ? `
    body::before {
      content: '';
      position: fixed;
      inset: 0;
      z-index: 0;
      pointer-events: none;
      background-image: ${layers.join(', ')};
      background-size: ${sizes.join(', ')};
      background-position: center;
      background-attachment: fixed;
      mix-blend-mode: ${blends[0] ?? 'normal'};
      opacity: ${cfg.backgroundImage && !patternFn ? cfg.backgroundImageOpacity : 1};
    }
    #root { position: relative; z-index: 1; }
  ` : ''
}

// ── Config application ────────────────────────────────────────────────────────

/** Apply a CampaignConfig to the live `campaign` object and CSS variables. */
function applyCampaignConfig(cfg: Partial<CampaignConfig>) {
  // Deep-merge: scalar values only (arrays replaced wholesale if provided)
  Object.assign(campaign, cfg)

  // Inject color tokens as CSS custom properties so every component
  // picks them up via var(--accent) etc. without importing campaign.ts.
  const root = document.documentElement
  root.style.setProperty('--accent',        campaign.accentColor)
  root.style.setProperty('--accent-bright', campaign.accentHoverColor)
  root.style.setProperty('--accent-hover',  campaign.accentHoverColor)
  root.style.setProperty('--border-accent', campaign.accentColor)
  root.style.setProperty('--scrollbar-h',   campaign.accentColor + '80')
  root.style.setProperty('--blue',          campaign.blueColor)
  root.style.setProperty('--red',           campaign.redColor)

  // Background/surface tokens are theme-dependent. Injecting them inline on
  // <html> would beat the light-theme stylesheet override (inline styles win
  // over any selector), so scope the campaign-configured dark surfaces to
  // [data-theme="dark"] instead. The light palette lives in index.css.
  let darkTokens = document.getElementById('campaign-dark-tokens') as HTMLStyleElement | null
  if (!darkTokens) {
    darkTokens = document.createElement('style')
    darkTokens.id = 'campaign-dark-tokens'
    document.head.appendChild(darkTokens)
  }
  darkTokens.textContent = `:root[data-theme="dark"] {
    --bg:          ${campaign.bgColor};
    --bg-card:     ${campaign.bgCardColor};
    --bg-elevated: ${campaign.bgElevatedColor};
    --bg-chrome:   ${campaign.bgColor};
    --border:      ${campaign.borderColor};
  }`

  // Apply background image and camo pattern
  applyBackground(campaign)
}

async function bootstrap() {
  // Resolve the theme before first paint so campaign tokens land on the right
  // [data-theme] and there's no dark/light flash. ThemeProvider keeps it in
  // sync afterwards.
  try {
    const stored = localStorage.getItem('bfweb-theme')
    const theme = stored === 'light' || stored === 'dark'
      ? stored
      : (window.matchMedia?.('(prefers-color-scheme: light)').matches ? 'light' : 'dark')
    document.documentElement.setAttribute('data-theme', theme)
  } catch {
    document.documentElement.setAttribute('data-theme', 'dark')
  }

  // Start with compiled-in defaults
  applyCampaignConfig(campaignDefaults)

  // Fetch server-provided config (set via --config flag on bfdb) and overlay it
  try {
    const res = await fetch(`${API_ROOT}/api/config`)
    if (res.ok) {
      const remote: Partial<CampaignConfig> = await res.json()
      if (remote && typeof remote === 'object' && Object.keys(remote).length > 0) {
        applyCampaignConfig(remote)
        console.info(`[campaign] Config loaded from ${API_ROOT}/api/config`)
      }
    }
  } catch {
    // Network offline or pre-production dev mode — use defaults silently
  }

  createRoot(document.getElementById('root')!).render(
    <StrictMode>
      <ThemeProvider>
        <App />
      </ThemeProvider>
    </StrictMode>,
  )
}

bootstrap()
