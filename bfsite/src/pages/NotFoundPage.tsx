import { Link } from 'react-router-dom'
import { ArrowRight } from '@icons'
import { SectionHead } from '../components/Bits'

export default function NotFoundPage() {
  return (
    <section className="section page-first" style={{ minHeight: '70vh' }}>
      <div className="wrap">
        <SectionHead
          num={0}
          para="Off the map"
          title={<>Nothing <em>at these coordinates.</em></>}
          lede="That page doesn’t exist. The field manual lives on the wiki now, if that’s what you were after."
        />
        <Link to="/" className="vs-btn vs-btn-outline">Back to base <ArrowRight size={14} /></Link>
      </div>
    </section>
  )
}
