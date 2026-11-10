// Where a poster points and whose mark it carries, by campaign.

import type { ReactNode } from 'react'
import { VS_URLS } from '../urls'
import type { PosterModel } from './weekly'

/** The public addresses a poster's footer and post link to, by campaign. */
export function siteFor(m: Pick<PosterModel, 'theme'>): { dashboard: string; wiki: string } {
  return { dashboard: m.theme === 'redstorm' ? VS_URLS.redstorm : VS_URLS.dashboard, wiki: VS_URLS.wiki }
}

/** The campaign's mark for the masthead: each app's /public has these. */
export function defaultLogo(m: Pick<PosterModel, 'theme'>): ReactNode {
  return m.theme === 'redstorm'
    ? <img src="/redstorm/emblem-dark.svg" alt="" />
    : <img src="/vs-vectorstrike_hd-white.png" alt="" />
}
