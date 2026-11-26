import { SERVERS, THEME_CLASS, type ServerProfile } from '@shared/servers/profiles'
import { FONTS, useFonts } from '@shared/servers/fonts'
import { VS_URLS } from '@shared/web/urls'
import { useInstance } from '../context/InstanceContext'
import { elsewhere } from './elsewhere'
import '@shared/servers/themes.css'
import '../components/dash-themes.css'
import '../components/levant.css'
import '../components/levant-motion.css'

/** The campaign this dashboard belongs to: Levant Ablaze, the modern war. */
export const MODERN: ServerProfile | undefined = SERVERS.find(s => s.instanceId && s.site.url === VS_URLS.dashboard)

export interface DashLook {
  /** The profile whose look the dashboard wears, if any. */
  profile?: ServerProfile
  /**
   * Classes for the wrapper that carries the palette: `lv-look` for the
   * layout and parts the campaign look is built from (levant.css and
   * friends), then the look's own class. Empty for the plain look.
   */
  className: string
  /** True when the dashboard is looking at that campaign itself, not merely wearing its look. */
  onCampaign: boolean
}

/**
 * The dashboard wears the modern campaign's look from the first paint, not
 * once /api/instances answers: before, it opened olive-drab and changed
 * clothes a second later, and stayed olive whenever bfdb was down. Only a
 * campaign with a dashboard of its own (Red Storm, which an admin can still
 * look at here, on the admin pages) gets the plain look, so its numbers are
 * never dressed as the Levant's.
 */
export function useDashLook(): DashLook {
  const { current } = useInstance()
  const profile = elsewhere(current?.id) ? undefined : MODERN
  useFonts(profile && FONTS[profile.theme])
  return {
    profile,
    className: profile ? `dash-themed lv-look ${THEME_CLASS[profile.theme]}` : '',
    onCampaign: !!profile && (!current || current.id === profile.instanceId),
  }
}
