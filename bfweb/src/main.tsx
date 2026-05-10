import { StrictMode } from 'react'
import { createRoot } from 'react-dom/client'
import './index.css'
import App from './App.tsx'
import { campaign, campaignDefaults, type CampaignConfig } from './config/campaign'

/** Apply a CampaignConfig to the live `campaign` object and CSS variables. */
function applyCampaignConfig(cfg: Partial<CampaignConfig>) {
  // Deep-merge: scalar values only (arrays replaced wholesale if provided)
  Object.assign(campaign, cfg)

  // Inject color tokens as CSS custom properties so every component
  // picks them up via var(--accent) etc. without importing campaign.ts.
  const root = document.documentElement
  root.style.setProperty('--accent',        campaign.accentColor)
  root.style.setProperty('--accent-hover',  campaign.accentHoverColor)
  root.style.setProperty('--border-accent', campaign.accentColor)
  root.style.setProperty('--scrollbar-h',   campaign.accentColor + '80')
  root.style.setProperty('--blue',          campaign.blueColor)
  root.style.setProperty('--red',           campaign.redColor)
  root.style.setProperty('--bg',            campaign.bgColor)
  root.style.setProperty('--bg-card',       campaign.bgCardColor)
  root.style.setProperty('--bg-elevated',   campaign.bgElevatedColor)
  root.style.setProperty('--border',        campaign.borderColor)
}

async function bootstrap() {
  // Start with compiled-in defaults
  applyCampaignConfig(campaignDefaults)

  // Fetch server-provided config (set via --config flag on bfdb) and overlay it
  try {
    const res = await fetch('/api/config')
    if (res.ok) {
      const remote: Partial<CampaignConfig> = await res.json()
      if (remote && typeof remote === 'object' && Object.keys(remote).length > 0) {
        applyCampaignConfig(remote)
        console.info('[campaign] Config loaded from /api/config')
      }
    }
  } catch {
    // Network offline or pre-production dev mode — use defaults silently
  }

  createRoot(document.getElementById('root')!).render(
    <StrictMode>
      <App />
    </StrictMode>,
  )
}

bootstrap()
