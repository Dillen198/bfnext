import { StrictMode } from 'react'
import { createRoot } from 'react-dom/client'
import './index.css'
import './styles/console.css'
import App from './App.tsx'
import { initialTheme, ThemeProvider, type ThemeOptions } from '@shared/web/theme'
import { campaign, campaignDefaults, type CampaignConfig } from './config/campaign'
import { API_ROOT } from './api'
import { ErrorBoundary } from '@shared/web/ErrorBoundary'
import { installChunkReload } from '@shared/web/chunkReload'
import { deadline } from '@shared/web/api'
import { syncSideColors, withBrand, type ThemeName } from './config/redstorm'

// `?theme=` wins (the Discord snapshot must render dark), then the stored
// choice, then the OS. Light is paper on the desk; dark is the consoles.
//
// The side colours that script reads (`campaign.blueColor` and friends) are
// per theme, so text drawn in them keeps its contrast on paper and on glass;
// Layout remounts the page when the theme changes so they are re-read.
const THEME: ThemeOptions = {
  storageKey: 'bfrsr-theme',
  urlParam: true,
  apply(theme) {
    document.documentElement.setAttribute('data-theme', theme)
    applySideColors(theme)
  },
}

function applySideColors(theme: ThemeName) {
  syncSideColors(theme)
  document.querySelector('meta[name="theme-color"]')?.setAttribute('content', theme === 'light' ? '#e4ddca' : '#0d1013')
}

// ── Config application ────────────────────────────────────────────────────────

/**
 * Merge bfdb's campaign config into the live `campaign` object, with this
 * dashboard's identity over it (config/redstorm.ts). The colours themselves
 * are the stylesheet's (index.css), per theme: nothing is written inline, so
 * the light and dark palettes are never overridden by the main campaign's.
 */
function applyCampaignConfig(cfg: Partial<CampaignConfig>, theme: ThemeName) {
  Object.assign(campaign, withBrand(cfg, theme))
}

// A tab left open across a deploy asks for route chunks that are gone.
installChunkReload()

async function bootstrap() {
  // Resolve the theme before first paint so campaign tokens land on the right
  // [data-theme] and there's no dark/light flash. ThemeProvider keeps it in
  // sync afterwards.
  const theme = initialTheme(THEME)
  document.documentElement.setAttribute('data-theme', theme)

  // Start with compiled-in defaults
  applyCampaignConfig(campaignDefaults, theme)

  // Fetch server-provided config (set via --config flag on bfdb) and overlay it
  try {
    // Nothing renders until this settles, so it gets a short deadline: an
    // unreachable bfdb must not leave the page blank.
    const res = await fetch(`${API_ROOT}/api/config`, { signal: deadline(8_000) })
    if (res.ok) {
      const remote: Partial<CampaignConfig> = await res.json()
      if (remote && typeof remote === 'object' && Object.keys(remote).length > 0) {
        applyCampaignConfig(remote, theme)
        console.info(`[campaign] Config loaded from ${API_ROOT}/api/config`)
      }
    }
  } catch {
    // Network offline or pre-production dev mode — use defaults silently
  }
  applySideColors(theme)

  createRoot(document.getElementById('root')!).render(
    <StrictMode>
      <ErrorBoundary>
        <ThemeProvider options={THEME}>
          <App />
        </ThemeProvider>
      </ErrorBoundary>
    </StrictMode>,
  )
}

bootstrap()
