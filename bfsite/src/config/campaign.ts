// ─────────────────────────────────────────────────────────────────────────────
// Vector Strike — Campaign Configuration
// Edit this file to customise all branding, links, and copy on the website.
// ─────────────────────────────────────────────────────────────────────────────

export const campaign = {
  name: 'Vector Strike',
  shortName: 'VS',
  tagline: 'Where every sortie shapes the war.',
  version: 'v3.0',
  description:
    'Vector Strike is a dynamic, persistent multiplayer campaign for DCS World. ' +
    'Coalition forces battle for territory control across multiple theaters. ' +
    'Every mission matters — capture objectives, destroy logistics, defend your ' +
    'airfields, and tip the balance of power. Your kills are tracked, your ' +
    'achievements recorded. The war never stops.',
  discord: 'https://discord.gg/XyGkb3WAXS',
  server: 'The Coop · Operation Vector Strike',
  dashboardUrl: 'https://dashboard.vectorstrike.org',
  wikiUrl: 'https://wiki.vectorstrike.org',
  blueLabel: 'BLUFOR',
  redLabel: 'REDFOR',

  features: [
    {
      icon: 'Map',
      title: 'Dynamic Territory',
      description: 'Objectives change hands in real time. Capture airbases, FARPs, and logistics hubs to shift the front line.',
    },
    {
      icon: 'Package',
      title: 'Persistent Logistics',
      description: 'Supply chains fuel the war effort. Interdict enemy convoys, protect your own, and watch the resource economy evolve.',
    },
    {
      icon: 'Radio',
      title: 'Commander System',
      description: 'Coordinate with a dedicated commander role that allocates points, assigns objectives, and directs coalition strategy.',
    },
    {
      icon: 'Crosshair',
      title: 'Kill Tracking',
      description: 'Every air kill, ground kill, and friendly fire incident is recorded and attributed. Real-time kill feed for both sides.',
    },
    {
      icon: 'Layers',
      title: 'Interactive Tactical Map',
      description: 'Full-featured ops dashboard with NATO symbology, threat rings, mission planning tools, and live unit positions.',
    },
    {
      icon: 'BarChart3',
      title: 'Pilot Leaderboards',
      description: 'Comprehensive stats for every pilot: air kills, ground kills, K/D, logistics support, score ranking, and more.',
    },
    {
      icon: 'Truck',
      title: 'Cargo Transport',
      description: 'Helicopter crews can sling-load and transport supplies between FARPs and objectives, keeping the front line stocked.',
    },
    {
      icon: 'Users',
      title: 'Troop Deployments',
      description: 'Deploy infantry squads to hold objectives, defend positions, or assault enemy-held ground in combined arms operations.',
    },
    {
      icon: 'Radar',
      title: 'JTAC System',
      description: 'Dedicated JTAC units provide real-time targeting data, laser designation, and 9-line briefs to attack aircraft.',
    },
    {
      icon: 'Eye',
      title: 'Early Warning Radar',
      description: 'EWR networks detect and report enemy air contacts, feeding threat data directly into the coalition picture.',
    },
    {
      icon: 'Antenna',
      title: 'Networked SAM Defense',
      description: 'SAM sites share a live sensor picture, go dark until there\'s a real threat, and defend each other from anti-radiation missiles.',
    },
    {
      icon: 'Shield',
      title: 'Pilot Lives & Slots',
      description: 'Slot authorisation and a persistent lives system tie pilot actions to real consequences — losses actually matter.',
    },
    {
      icon: 'Menu',
      title: 'F10 Radio Menus',
      description: 'All campaign actions — deploying units, calling missions, managing cargo — are accessible through in-game F10 menus.',
    },
  ],

  joinSteps: [
    {
      number: '01',
      title: 'Own DCS World',
      description: 'Download DCS World from Eagle Dynamics. Any aircraft module that flies in our current theater will get you in the fight.',
    },
    {
      number: '02',
      title: 'Join Our Discord',
      description: 'Hit the Discord link below. Find the server details, current theater briefing, and connect with other pilots.',
    },
    {
      number: '03',
      title: 'Connect to the Server',
      description: 'Search for "The Coop · Operation Vector Strike" in the DCS multiplayer browser, or use the direct IP from Discord.',
    },
    {
      number: '04',
      title: 'Pick a Side & Fly',
      description: 'Choose BLUFOR or REDFOR, take a slot, and enter the AO. The campaign is persistent — your actions matter.',
    },
  ],
} as const
