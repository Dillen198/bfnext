import { Calculator, Carrier, Comms, Pilot, Plus, Rankings, Briefing, Tacmap, type IconComponent } from '@icons'

export interface NavItem {
  to: string
  label: string
  /** shorter label for the phone tab bar */
  short?: string
  icon: IconComponent
  end?: boolean
  /** what the page is for, shown in the search palette */
  hint: string
  /** on the phone tab bar */
  tab?: boolean
}

/** The site's pages, grouped by what a pilot is doing: flying, debriefing, competing. */
export const NAV_GROUPS: { label: string; items: NavItem[] }[] = [
  {
    label: 'Range',
    items: [
      { to: '/', label: 'Range control', short: 'Live', icon: Tacmap, end: true, hint: 'Live map, traffic, carriers, tankers, threats and results as they land', tab: true },
      { to: '/spawn', label: 'Spawn', icon: Plus, hint: 'Put adversaries, targets, tankers and ships on the range', tab: true },
      { to: '/brief', label: 'Range brief', short: 'Brief', icon: Comms, hint: 'Frequencies, TACANs, laser codes, coordinates and weather: the kneeboard card', tab: true },
    ],
  },
  {
    label: 'Debrief',
    items: [
      { to: '/me', label: 'My record', short: 'Me', icon: Pilot, hint: 'Your scorecards, insights, qualifications and trends', tab: true },
      { to: '/results', label: 'Results', icon: Briefing, hint: 'Every graded result, filterable', tab: true },
      { to: '/greenie', label: 'Greenie board', icon: Carrier, hint: 'Every carrier pass, by pilot' },
    ],
  },
  {
    label: 'Compete',
    items: [
      { to: '/leaderboards', label: 'Leaderboards', icon: Rankings, hint: 'Bombing CEP, strafe, LSO, AAR, duels, SEAD and more' },
    ],
  },
  {
    label: 'Tools',
    items: [
      { to: '/calc', label: 'Calculators', icon: Calculator, hint: 'Bomb release, wind over deck, tanker planner' },
    ],
  },
]

export const NAV: NavItem[] = NAV_GROUPS.flatMap(g => g.items)

/** The page a path belongs to (for the top bar title). */
export function navFor(path: string): NavItem | undefined {
  if (path === '/') return NAV[0]
  if (path.startsWith('/pilot/')) return NAV.find(n => n.to === '/me')
  if (path.startsWith('/result/') || path.startsWith('/stations/')) return NAV.find(n => n.to === '/results')
  return NAV.find(n => n.to !== '/' && path.startsWith(n.to))
}
