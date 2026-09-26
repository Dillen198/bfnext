/** `/calc` — bomb release, wind over deck and tanker planning. */
import { useSearchParams } from 'react-router-dom'
import { AarCalc } from './calc/AarCalc'
import { BombCalc } from './calc/BombCalc'
import { WodCalc } from './calc/WodCalc'

const TOOLS = [
  { id: 'bomb', label: 'Bomb release', sub: 'Where to pickle for your altitude, speed and dive, flown through the mission’s own wind.' },
  { id: 'wod', label: 'Wind over deck', sub: 'What the deck feels right now, and the course that gives 25–30 kt down the landing area.' },
  { id: 'aar', label: 'Tanker planner', sub: 'How much gas to take and how long you will be plugged in.' },
] as const

type Tool = (typeof TOOLS)[number]['id']

export default function CalcPage() {
  const [sp, setSp] = useSearchParams()
  const tool = (TOOLS.some(t => t.id === sp.get('tool')) ? sp.get('tool') : 'bomb') as Tool
  const cur = TOOLS.find(t => t.id === tool)!
  return (
    <div className="wrap page">
      <div className="page-head">
        <div>
          <h1 className="display">Calculators</h1>
          <p className="sub m-0 mt-1">{cur.sub}</p>
        </div>
      </div>
      <div className="tabs-range mb-4" role="tablist">
        {TOOLS.map(t => (
          <button key={t.id} role="tab" aria-selected={tool === t.id} onClick={() => setSp({ tool: t.id }, { replace: true })}>
            {t.label}
          </button>
        ))}
      </div>
      {tool === 'bomb' && <BombCalc />}
      {tool === 'wod' && <WodCalc />}
      {tool === 'aar' && <AarCalc />}
    </div>
  )
}
