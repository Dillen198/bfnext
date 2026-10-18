import Nav from './components/Nav'
import Hero from './components/Hero'
import Situation from './components/Situation'
import Mission from './components/Mission'
import Execution from './components/Execution'
import Intel from './components/Intel'
import Community from './components/Community'
import Footer from './components/Footer'

export default function App() {
  return (
    <>
      <Nav />
      <main>
        <Hero />
        <Situation />
        <Mission />
        <Execution />
        <Intel />
        <Community />
      </main>
      <Footer />
    </>
  )
}
