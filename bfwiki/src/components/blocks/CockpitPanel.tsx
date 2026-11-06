import { useState } from 'react'
import { PANEL_TABS, type CockpitPanelSpec, type PanelRow } from './parse'

/**
 * The ```cockpitpanel block: the Vector Strike in-game panel, drawn with the
 * plugin's own colours, fonts and layout (bfcockpit.lua: `C`, `build_skins`,
 * `TABS`, `layout`, `draw_rows`). The window chrome is a picture; the button
 * rows work, and put what the server would answer in the status line, the
 * way `set_note` does in the game.
 */

const EMBLEM = `${import.meta.env.BASE_URL}cockpit/emblem-64.png`

function Row({ row, tab, onPress }: { row: PanelRow; tab: string; onPress: (says?: string) => void }) {
  switch (row.kind) {
    case 'heading':
      return <div className="cp-row cp-head">{row.text}</div>
    case 'gap':
      return <div className="cp-gap" />
    case 'text':
      return <div className={`cp-row cp-text cp-${row.style}`}>{row.text || ' '}</div>
    case 'button': {
      const skin = row.go ? 'cp-go' : row.sub ? 'cp-sub' : 'cp-cmd'
      // MENU's command rows carry the star that pins them to the wheel.
      const star = tab === 'MENU' && !row.sub && !row.go
      return (
        <div className="cp-line">
          <button
            type="button"
            className={`cp-btn ${skin}`}
            disabled={!row.enabled}
            onClick={() => onPress(row.says)}
          >
            {row.label}
          </button>
          {star && (
            <span className="cp-btn cp-star" aria-hidden="true">
              ☆
            </span>
          )}
        </div>
      )
    }
  }
}

export default function CockpitPanel({ spec }: { spec: CockpitPanelSpec }) {
  const [note, setNote] = useState<{ text: string; n: number } | null>(null)
  const crumb = spec.crumb ?? (spec.tab === 'MENU' ? 'F10 > Other' : undefined)
  const status = note?.text ?? spec.status ?? 'Vector Strike'

  return (
    <div className="mk-block">
      <figure className="mk-panel-stage" aria-label={`Vector Strike cockpit panel, ${spec.tab} tab`}>
        <div className="cp">
          <div className="cp-titlebar">Vector Strike</div>
          <div className="cp-client">
            <div className="cp-toolbar" aria-hidden="true">
              {PANEL_TABS.map(t => (
                <span key={t} className={`cp-tab${t === spec.tab ? ' cp-on' : ''}`}>
                  {t}
                </span>
              ))}
              <span className="cp-tools">
                <span className="cp-tab cp-tool">-</span>
                <span className="cp-tab cp-tool">+</span>
                <span className="cp-tab cp-tool">X</span>
              </span>
            </div>
            <p className="sr-only">Showing the {spec.tab} tab.</p>
            {crumb !== undefined && (
              <div className="cp-crumbrow">
                <span className={`cp-tab cp-back${crumb === 'F10 > Other' ? ' cp-off' : ''}`} aria-hidden="true">
                  {'< Back'}
                </span>
                <span className="cp-crumb">{crumb}</span>
              </div>
            )}
            <div className="cp-pane">
              {spec.rows.map((r, i) => (
                <Row key={i} row={r} tab={spec.tab} onPress={says => says && setNote(n => ({ text: says, n: (n?.n ?? 0) + 1 }))} />
              ))}
            </div>
            <div className="cp-status">
              <img className="cp-emblem" src={EMBLEM} alt="" width={18} height={18} />
              <span className="cp-status-text" role="status">
                {/* Keyed so a repeated answer flashes again. */}
                <span key={note?.n ?? 0} className={note ? 'cp-status-note' : undefined}>
                  {status}
                </span>
              </span>
            </div>
          </div>
        </div>
      </figure>
    </div>
  )
}
