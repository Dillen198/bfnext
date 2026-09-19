import Nav from './components/Nav'
import HeroSection from './components/HeroSection'
import AlertBanner from './components/AlertBanner'
import AboutSection from './components/AboutSection'
import FeaturesSection from './components/FeaturesSection'
import HowToJoin from './components/HowToJoin'
import StatsSection from './components/StatsSection'
import ManualSection from './components/ManualSection'
import SpecialThanksSection from './components/SpecialThanksSection'
import Footer from './components/Footer'

export default function App() {
  return (
    <div style={{ minHeight: '100vh', background: 'var(--bg)' }}>
      <Nav />
      <HeroSection />
      <AlertBanner />
      <AboutSection />
      <FeaturesSection />
      <HowToJoin />
      <StatsSection />
      <ManualSection />
      <SpecialThanksSection />
      <Footer />
    </div>
  )
}
