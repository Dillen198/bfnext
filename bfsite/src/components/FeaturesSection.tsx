import { campaign } from '../config/campaign'
import {
  Antenna,
  Comms as Radio,
  Crosshair,
  Eye,
  Layers,
  Logistics as Truck,
  Menu,
  Pilot as Users,
  Radar,
  Rankings as BarChart3,
  Shield,
  Supply as Package,
  Tacmap as Map,
  type IconComponent,
} from '@icons'
import Reveal from './Reveal'

const ICONS: Record<string, IconComponent> = {
  Map, Package, Radio, Crosshair, Layers, BarChart3, Truck, Users, Radar, Eye, Shield, Menu, Antenna,
}

export default function FeaturesSection() {
  return (
    <section id="features" style={{ background: 'var(--bg-alt)', padding: '6rem 0', borderTop: '1px solid var(--border)' }}>
      <div className="max-w-7xl mx-auto px-6">

        {/* Header */}
        <div className="text-center mb-14">
          <div className="vs-section-label mb-4">What makes it different</div>
          <h2
            style={{
              fontFamily: "'Bebas Neue', sans-serif",
              fontSize: 'clamp(2.5rem, 6vw, 3.5rem)',
              color: 'var(--text)',
              letterSpacing: '0.08em',
              margin: 0,
            }}
          >
            CAMPAIGN <span style={{ color: 'var(--accent)' }}>SYSTEMS</span>
          </h2>
        </div>

        {/* Grid */}
        <div className="grid grid-cols-1 sm:grid-cols-2 lg:grid-cols-3 gap-5">
          {campaign.features.map((f, i) => {
            const Icon = ICONS[f.icon] ?? Crosshair
            return (
              <Reveal key={i} delay={(i % 3) * 100} className="feature-card">
                <div
                  className="flex items-center justify-center w-10 h-10 mb-4"
                  style={{ background: 'rgba(77,124,15,0.1)', borderRadius: '2px', border: '1px solid rgba(77,124,15,0.2)' }}
                >
                  <Icon size={18} style={{ color: 'var(--accent)' }} />
                </div>
                <h3
                  style={{
                    fontFamily: "'Bebas Neue', sans-serif",
                    fontSize: '1.15rem',
                    letterSpacing: '0.12em',
                    color: 'var(--text)',
                    margin: '0 0 0.6rem 0',
                  }}
                >
                  {f.title}
                </h3>
                <p style={{ fontSize: '0.85rem', color: 'var(--text-muted)', lineHeight: 1.65, margin: 0 }}>
                  {f.description}
                </p>
              </Reveal>
            )
          })}
        </div>
      </div>
    </section>
  )
}
