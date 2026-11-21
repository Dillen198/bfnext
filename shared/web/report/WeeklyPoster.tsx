// The weekly report poster, in each campaign's own look: the 1985 situation
// room for Red Storm Rising (dark, amber, NATO blue, Soviet red, a paper
// dispatch) and the Vector Strike dashboard for everything else (Levant
// Ablaze). Fixed-size cards (POSTER_SIZE) drawn with their own scoped CSS,
// so the PNG is the same whichever page or theme it is drawn on.

import type { ReactNode } from 'react'
import { dayWords, n, paragraphs, type Dispatch, type PosterModel } from './weekly'

export interface PosterProps {
  model: PosterModel
  dispatch: Dispatch
  /** Addresses for the footer ("redstorm.vectorstrike.org"). */
  site?: { dashboard?: string; wiki?: string }
  /** The campaign's emblem or logo, for the masthead. */
  logo?: ReactNode
  /** The root's id (the print page's `weekly-poster`, which the bot clips). */
  id?: string
}

const host = (u?: string) => (u ? u.replace(/^https?:\/\//, '').replace(/\/.*$/, '') : '')

/** A headline's size: long ones step down so they stay on two lines. */
function fit(text: string, sizes: [number, number][]): number {
  for (const [max, px] of sizes) if (text.length <= max) return px
  return sizes[sizes.length - 1][1]
}

function sideClass(s: string | null) {
  return s === 'Blue' ? 'b' : s === 'Red' ? 'r' : ''
}

// ── Red Storm Rising: Southern Flank 1985 ───────────────────────────────────

const RSR_CSS = `
.wr-rsr{--bg:#0d1013;--panel:rgba(24,28,32,.96);--edge:#050607;--rule:#2a3036;--cream:#eef0ea;--grey:#aab2b8;--muted:#8c959c;
  --amber:#ffb000;--amber-dim:#b37b00;--blue:#8db4d9;--red:#e0574a;--paper:#ddd6c4;--ink:#1d1b17;
  box-sizing:border-box;width:1600px;height:900px;background:var(--bg);color:var(--cream);font-family:"Courier Prime",monospace;
  padding:20px 24px;display:flex;flex-direction:column;gap:12px;overflow:hidden;text-align:left;
  background-image:repeating-linear-gradient(0deg,rgba(255,255,255,.012) 0 1px,transparent 1px 3px)}
.wr-rsr *{box-sizing:border-box}
.wr-rsr,.wr-rsr .grid,.wr-rsr .row{justify-items:stretch;align-items:stretch}
.wr-rsr .st{font-family:"Stardos Stencil",Impact,sans-serif;letter-spacing:.16em;text-transform:uppercase}
.wr-rsr .lbl{font:400 13px/1 "Courier Prime",monospace;letter-spacing:.14em;text-transform:uppercase;color:var(--muted)}
.wr-rsr .panel{background:var(--panel);border:1px solid var(--edge);box-shadow:inset 0 0 0 1px var(--rule);min-height:0;overflow:hidden}
.wr-rsr .ph{display:flex;justify-content:space-between;align-items:center;padding:10px 14px;border-bottom:1px solid var(--rule)}
.wr-rsr .ph .st{font-size:17px;color:var(--grey)}
.wr-rsr .top{display:flex;align-items:center;gap:20px;padding-bottom:12px;border-bottom:1px solid var(--rule)}
.wr-rsr .top .logo{height:64px;display:flex;align-items:center}
.wr-rsr .top .logo img,.wr-rsr .top .logo svg{height:64px;width:auto}
.wr-rsr .title{font-size:34px;color:var(--cream)}
.wr-rsr .sub{margin-top:6px}
.wr-rsr .clock{display:flex;gap:6px;margin-left:auto;align-items:flex-end}
.wr-rsr .grp{text-align:center}
.wr-rsr .digits{display:flex;gap:3px}
.wr-rsr .d{background:#0a0b0a;border:1px solid #000;box-shadow:inset 0 -18px 0 rgba(255,255,255,.03);color:var(--amber);font:400 46px/1 VT323,monospace;padding:4px 7px 0;min-width:34px;text-align:center}
.wr-rsr .cap{font:400 11px/1 "Courier Prime",monospace;color:var(--muted);letter-spacing:.2em;margin-top:5px}
.wr-rsr .srv{margin-left:22px;text-align:right;max-width:300px}
.wr-rsr .srv b{display:block;font:700 19px/1.15 "Courier Prime",monospace;color:var(--amber);letter-spacing:.04em}
.wr-rsr .fkeys{display:flex;gap:6px}
.wr-rsr .fkey{border:1px solid var(--rule);padding:7px 12px;font:400 14px/1 "Courier Prime",monospace;color:var(--grey);letter-spacing:.12em}
.wr-rsr .fkey b{color:var(--amber);margin-right:6px}
.wr-rsr .fkey.on{background:#181c20;color:var(--cream);border-color:#3a424a}
.wr-rsr .fkey.flash{border-color:var(--red);color:var(--red)}
.wr-rsr .grid{display:grid;grid-template-columns:330px 1fr 470px;gap:12px;flex:1;min-height:0}
.wr-rsr .forces{display:flex;flex-direction:column}
.wr-rsr .row{display:grid;grid-template-columns:1fr 1fr;border-bottom:1px solid var(--rule)}
.wr-rsr .cell{padding:10px 14px;min-width:0}
.wr-rsr .cell+.cell{border-left:1px solid var(--rule)}
.wr-rsr .big{font:400 46px/1 VT323,monospace}
.wr-rsr .big.b{color:var(--blue)} .wr-rsr .big.r{color:var(--red)}
.wr-rsr .k{font:400 12px/1.2 "Courier Prime",monospace;letter-spacing:.14em;color:var(--amber);text-transform:uppercase}
.wr-rsr .s{font:400 12.5px/1.3 "Courier Prime",monospace;color:var(--muted);margin-top:2px;white-space:nowrap;overflow:hidden;text-overflow:ellipsis}
.wr-rsr .flags{display:flex;align-items:center;gap:10px;padding:10px 14px;border-bottom:1px solid var(--rule)}
.wr-rsr .flag{height:30px;width:45px;border:1px solid #000;flex-shrink:0;position:relative}
.wr-rsr .flag.nato{background:#004990}
.wr-rsr .flag.ussr{background:#c8102e}
.wr-rsr .flag.ussr:before{content:"\\2605";position:absolute;left:6px;top:1px;color:#ffd700;font-size:13px}
.wr-rsr .vs{font:700 15px/1 "Courier Prime",monospace;color:var(--grey);letter-spacing:.14em;text-transform:uppercase;white-space:nowrap;overflow:hidden;text-overflow:ellipsis}
.wr-rsr .end{padding:10px 14px;font:400 13.5px/1.45 "Courier Prime",monospace;color:var(--grey);margin-top:auto}
.wr-rsr .end b{color:var(--cream)}
.wr-rsr .paper{background:var(--paper);color:var(--ink);padding:22px 26px 14px;position:relative;max-height:calc(100% - 12px);overflow:hidden;
  box-shadow:0 10px 30px rgba(0,0,0,.6);transform:rotate(-.6deg);margin:6px 4px}
.wr-rsr .mast{font:400 15px/1 "Stardos Stencil",Impact,sans-serif;letter-spacing:.5em;text-align:center;border-bottom:3px double var(--ink);padding-bottom:8px;text-transform:none}
.wr-rsr .ed{display:flex;justify-content:space-between;font:400 12px/1 "Courier Prime",monospace;letter-spacing:.12em;padding:7px 0;border-bottom:1px solid var(--ink)}
.wr-rsr .paper h1{font:700 54px/1 "Old Standard TT",serif;margin:14px 0 6px;text-transform:uppercase;letter-spacing:-.01em;color:var(--ink)}
.wr-rsr .paper h2{font:italic 400 21px/1.25 "Old Standard TT",serif;margin:0 0 12px;color:var(--ink)}
.wr-rsr .cols{column-count:2;column-gap:22px;column-fill:balance;max-height:470px}
.wr-rsr .paper p{font:400 17px/1.5 "Old Standard TT",serif;margin:0 0 10px;text-align:justify;hyphens:auto;color:var(--ink)}
.wr-rsr .paper p:first-child:first-letter{font:700 46px/.8 "Old Standard TT",serif;float:left;margin:4px 6px 0 0}
.wr-rsr .figs{break-inside:avoid;border:1px solid var(--ink);padding:8px 10px;margin:0 0 10px}
.wr-rsr .figs .t{font:400 12px/1 "Stardos Stencil",Impact,sans-serif;letter-spacing:.3em;border-bottom:1px solid var(--ink);padding-bottom:5px;margin-bottom:6px}
.wr-rsr .figs div.l{display:flex;justify-content:space-between;font:400 14px/1.6 "Courier Prime",monospace}
.wr-rsr .tty{margin:0;padding:4px 16px 6px}
.wr-rsr .tty li{list-style:none;padding:7px 0;border-bottom:1px dashed var(--rule)}
.wr-rsr .tty li:last-child{border-bottom:0}
.wr-rsr .tty .h{font:700 15.5px/1.2 "Courier Prime",monospace;color:var(--amber);letter-spacing:.03em;white-space:nowrap;overflow:hidden;text-overflow:ellipsis}
.wr-rsr .tty .h:before{content:"> ";color:var(--amber-dim)}
.wr-rsr .tty .h.b{color:var(--blue)} .wr-rsr .tty .h.r{color:var(--red)}
.wr-rsr .tty .bd{font:400 13.5px/1.35 "Courier Prime",monospace;color:var(--grey);margin-top:2px;padding-left:18px;display:-webkit-box;-webkit-line-clamp:2;-webkit-box-orient:vertical;overflow:hidden}
.wr-rsr .aces{padding:2px 16px 8px}
.wr-rsr .aces .t{font:400 14px/1 "Stardos Stencil",Impact,sans-serif;letter-spacing:.16em;color:var(--grey);text-transform:uppercase;padding:8px 0 6px;border-top:1px solid var(--rule)}
.wr-rsr .ace{display:grid;grid-template-columns:22px 1fr auto;gap:8px;font:400 14px/1.6 "Courier Prime",monospace;color:var(--grey)}
.wr-rsr .ace .i{color:var(--amber-dim)} .wr-rsr .ace .nm{color:var(--cream);white-space:nowrap;overflow:hidden;text-overflow:ellipsis}
.wr-rsr .ace .nm.b{color:var(--blue)} .wr-rsr .ace .nm.r{color:var(--red)}
.wr-rsr .ace .v{color:var(--amber);font-family:VT323,monospace;font-size:20px;line-height:1.1}
.wr-rsr .flash{margin:4px 14px 14px;border:2px solid var(--red);background:rgba(224,87,74,.08);padding:10px 14px}
.wr-rsr .flash .t{font:400 19px/1 "Stardos Stencil",Impact,sans-serif;letter-spacing:.2em;color:var(--red);text-transform:uppercase}
.wr-rsr .flash .bd{font:400 14px/1.45 "Courier Prime",monospace;color:var(--cream);margin-top:7px}
.wr-rsr .quiet{padding:16px;font:400 14px/1.5 "Courier Prime",monospace;color:var(--muted)}
.wr-rsr .foot{display:flex;justify-content:space-between;align-items:center;border-top:1px solid var(--rule);padding-top:10px;
  font:400 14px/1 "Courier Prime",monospace;letter-spacing:.16em;color:var(--muted);text-transform:uppercase}
.wr-rsr .foot b{color:var(--amber);font-weight:700}
`

function NatoFlag() {
  return (
    <span className="flag nato" aria-hidden>
      <svg viewBox="0 0 45 30" width="43" height="28" style={{ display: 'block' }}>
        <line x1="22.5" y1="4" x2="22.5" y2="26" stroke="#fff" strokeWidth="1" />
        <line x1="8" y1="15" x2="37" y2="15" stroke="#fff" strokeWidth="1" />
        <polygon points="22.5,8 25,15 22.5,22 20,15" fill="#fff" />
        <circle cx="22.5" cy="15" r="5.5" fill="none" stroke="#fff" strokeWidth="1" />
      </svg>
    </span>
  )
}

function Digits({ text }: { text: string }) {
  return <div className="digits">{text.split('').map((c, i) => <span key={i} className="d">{c}</span>)}</div>
}

export function RedStormPoster({ model: m, dispatch, site, logo, id }: PosterProps) {
  const until = new Date(m.until.getTime() - 1)
  const MON = ['JAN', 'FEB', 'MAR', 'APR', 'MAY', 'JUN', 'JUL', 'AUG', 'SEP', 'OCT', 'NOV', 'DEC'][until.getUTCMonth()]
  const paras = paragraphs(dispatch.body)
  const ls = m.lastStand?.this_week ? m.lastStand : null
  const flashLine = ls ? (m.events.find(e => e.kind === 'laststand')?.detail ?? '') : ''
  const sideB = m.blue.toUpperCase()
  const sideR = m.red.toUpperCase()
  const events = m.events.filter(e => e.kind !== 'laststand').slice(0, 5)
  return (
    <div className="wr-rsr" id={id} data-poster="redstorm">
      <style>{RSR_CSS}</style>
      <div className="top">
        {logo && <div className="logo">{logo}</div>}
        <div style={{ minWidth: 0 }}>
          <div className="st title">{m.brand}</div>
          <div className="lbl sub">Weekly situation report · {m.week}{m.roundNo ? ` · Round ${m.roundNo}` : ''}</div>
        </div>
        <div className="clock">
          <div className="grp"><Digits text={String(m.day ?? 0).padStart(3, '0')} /><div className="cap">DAY</div></div>
          <div className="grp"><Digits text={String(until.getUTCDate()).padStart(2, '0')} /><div className="cap">{MON}</div></div>
          <div className="grp"><Digits text={String(until.getUTCFullYear()).slice(2)} /><div className="cap">YEAR</div></div>
        </div>
        <div className="srv"><span className="lbl">DCS server</span><b>{m.server}</b></div>
      </div>

      <div className="fkeys">
        <div className="fkey on"><b>F1</b>SITUATION</div><div className="fkey"><b>F2</b>FORCES</div>
        <div className="fkey"><b>F3</b>THE DISPATCH</div><div className="fkey"><b>F4</b>THIS WEEK</div>
        <div className={`fkey${ls ? ' flash' : ''}`}><b>F5</b>LAST STAND</div>
      </div>

      <div className="grid">
        <div className="panel forces">
          <div className="ph"><span className="st">Forces</span><span className="lbl">{m.day ? `Day ${m.day}` : 'This week'}</span></div>
          <div className="flags"><NatoFlag /><span className="vs">{sideB} &nbsp;VS&nbsp; {sideR}</span><span className="flag ussr" aria-hidden /></div>
          {m.board.map(r => (
            <div className="row" key={r.label}>
              <div className="cell"><div className="k">{r.label}</div><div className="big b">{n(r.blue)}</div>{r.blueNote && <div className="s">{r.blueNote}</div>}</div>
              <div className="cell"><div className="k">{r.label}</div><div className="big r">{n(r.red)}</div>{r.redNote && <div className="s">{r.redNote}</div>}</div>
            </div>
          ))}
          <div className="end">
            {m.will
              ? <><b>Will to fight:</b> {m.blue} {Math.round(m.will.blue)}, {m.red} {Math.round(m.will.red)}. Under {Math.round(m.will.threshold)} for six hours, a side asks for terms.</>
              : <><b>This week:</b> {n(m.totals.captures)} captures, {n(m.totals.airKills)} air and {n(m.totals.groundKills)} ground kills.</>}
          </div>
        </div>

        <div style={{ minHeight: 0 }}>
          <div className="paper">
            <div className="mast">The Allied Dispatch</div>
            <div className="ed"><span>{dayWords(m.day)}</span><span>{until.getUTCDate()} {MON} {until.getUTCFullYear()}</span><span>WEEKLY EDITION</span></div>
            <h1 style={{ fontSize: fit(dispatch.headline, [[18, 54], [28, 46], [40, 38]]) || 34 }}>{dispatch.headline}</h1>
            <h2>{dispatch.standfirst}</h2>
            <div className="cols">
              {paras.map((p, i) => <p key={i}>{p}</p>)}
              <div className="figs">
                <div className="t">The week in figures</div>
                <div className="l"><span>Objectives taken</span><span>{n(m.totals.captures)}</span></div>
                <div className="l"><span>Air kills</span><span>{n(m.totals.airKills)}</span></div>
                <div className="l"><span>Ground kills</span><span>{n(m.totals.groundKills)}</span></div>
                <div className="l"><span>Units deployed</span><span>{n(m.totals.deployed)}</span></div>
                <div className="l"><span>Pilots in action</span><span>{n(m.totals.activePilots)}</span></div>
              </div>
            </div>
          </div>
        </div>

        <div className="panel" style={{ display: 'flex', flexDirection: 'column' }}>
          <div className="ph"><span className="st">This week</span><span className="lbl" style={{ color: 'var(--amber)' }}>Teletype</span></div>
          {events.length > 0 ? (
            <ul className="tty">
              {events.map((e, i) => (
                <li key={i}><div className={`h ${sideClass(e.side)}`}>{e.title}</div>{e.detail && <div className="bd">{e.detail}</div>}</li>
              ))}
            </ul>
          ) : <div className="quiet">No captures or story events this week. The front held where it was.</div>}
          {m.pilots.length > 0 && (
            <div className="aces">
              <div className="t">Pilots of the week</div>
              {m.pilots.slice(0, ls ? 4 : 5).map((p, i) => (
                <div className="ace" key={p.name}>
                  <span className="i">{i + 1}.</span>
                  <span className={`nm ${sideClass(p.side)}`}>{p.name}{p.squadron ? ` [${p.squadron}]` : ''}</span>
                  <span className="v">{n(p.score)}</span>
                </div>
              ))}
            </div>
          )}
          {ls && (
            <div className="flash" style={{ marginTop: 'auto' }}>
              <div className="t">Flash · The Last Stand</div>
              <div className="bd">{flashLine}</div>
            </div>
          )}
        </div>
      </div>

      <div className="foot">
        <span>{site?.dashboard ? <><b>{host(site.dashboard)}</b> · live plot</> : 'Live plot on the dashboard'}</span>
        <span>Week of {m.week}</span>
        <span>{site?.wiki ? <><b>{host(site.wiki)}</b> · how to fly it</> : ''}</span>
      </div>
    </div>
  )
}

// ── Vector Strike dashboard (Levant Ablaze and the rest) ────────────────────

const LV_CSS = `
.wr-lv{--bg:#120e0a;--panel:rgba(29,24,18,.92);--panel-solid:#1d1812;--line:#3a2f22;--top:#8a6a45;--cream:#e3d2ad;--tan:#c4b08a;
  --muted:#8f7f63;--orange:#e07a3f;--tag:#c8642f;--tagtext:#1a0d05;--blue:#6aa6dc;--red:#d9493b;--green:#8ec83f;
  box-sizing:border-box;width:1600px;height:1000px;background:var(--bg);color:var(--cream);font-family:Barlow,system-ui,sans-serif;
  display:flex;flex-direction:column;gap:14px;padding:22px 26px;overflow:hidden;text-align:left}
.wr-lv *{box-sizing:border-box}
.wr-lv,.wr-lv .lv-hero,.wr-lv .sides,.wr-lv .lv-stats,.wr-lv .low,.wr-lv .fronts,.wr-lv .aces,.wr-lv .new li{justify-items:stretch;align-items:stretch}
.wr-lv .tag{display:inline-block;background:var(--tag);color:var(--tagtext);font:700 15px/1 "Barlow Condensed",sans-serif;letter-spacing:.16em;text-transform:uppercase;padding:6px 10px 5px}
.wr-lv .lbl{font:600 14px/1 "Barlow Condensed",sans-serif;letter-spacing:.18em;text-transform:uppercase;color:var(--muted)}
.wr-lv .big{font:700 54px/1 Oswald,sans-serif;color:var(--cream)}
.wr-lv .panel{background:var(--panel);border:1px solid var(--line);border-top:4px solid var(--top);position:relative;min-height:0;overflow:hidden}
.wr-lv .bar{display:flex;align-items:center;gap:18px;padding-bottom:12px;border-bottom:1px solid var(--line)}
.wr-lv .bar .logo{height:46px;display:flex;align-items:center}
.wr-lv .bar .logo img{height:46px;width:46px}
.wr-lv .brand{font:700 26px/1 Oswald,sans-serif;letter-spacing:.06em}
.wr-lv .chip{border:1px solid #e0b04a;background:rgba(201,162,39,.1);color:var(--cream);font:500 14px/1 Barlow,sans-serif;padding:6px 10px}
.wr-lv .right{margin-left:auto;display:flex;gap:28px;align-items:center}
.wr-lv .right div{text-align:right}
.wr-lv .right b{display:block;font:600 22px/1 Oswald,sans-serif;color:var(--cream);margin-top:4px}
.wr-lv .lv-hero{display:grid;grid-template-columns:1.25fr 1fr;gap:14px;height:300px}
.wr-lv .sit{background:radial-gradient(120% 140% at 100% 0%,rgba(224,122,63,.28) 0%,rgba(18,14,10,0) 55%),linear-gradient(90deg,#1b140d 0%,#140f0a 100%);
  border:1px solid var(--line);border-top:4px solid var(--orange);padding:24px 30px;display:flex;flex-direction:column;min-width:0}
.wr-lv .sit h1{margin:14px 0 0;font:700 92px/.9 Oswald,sans-serif;text-transform:uppercase;letter-spacing:.01em;white-space:nowrap;overflow:hidden;text-overflow:ellipsis}
.wr-lv .sit h1 span{color:var(--orange)}
.wr-lv .sit .sub{margin-top:12px;font:600 19px/1.2 "Barlow Condensed",sans-serif;letter-spacing:.2em;text-transform:uppercase;color:var(--tan)}
.wr-lv .sit .flash{margin-top:auto;font:500 17px/1.45 Barlow,sans-serif;color:var(--cream);max-width:760px;border-left:3px solid var(--orange);padding-left:12px}
.wr-lv .sit .flash b{font:600 19px/1.3 Oswald,sans-serif;letter-spacing:.02em;text-transform:uppercase;display:block;margin-bottom:2px}
.wr-lv .sides{display:grid;grid-template-rows:1fr auto 1fr;gap:10px;min-width:0}
.wr-lv .side{padding:14px 20px;display:flex;align-items:center;gap:20px}
.wr-lv .side.nato{border-top-color:var(--blue)} .wr-lv .side.rsii{border-top-color:var(--red)}
.wr-lv .side .nm{font:600 30px/1 Oswald,sans-serif;letter-spacing:.04em}
.wr-lv .side.nato .nm{color:var(--blue)} .wr-lv .side.rsii .nm{color:var(--red)}
.wr-lv .side .who{font:500 15px/1.3 Barlow,sans-serif;color:var(--tan);margin-top:6px;white-space:nowrap;overflow:hidden;text-overflow:ellipsis;max-width:220px}
.wr-lv .side .nums{margin-left:auto;display:flex;gap:22px;text-align:right}
.wr-lv .side .nums b{display:block;font:700 40px/1 Oswald,sans-serif;color:var(--cream);margin-top:6px}
.wr-lv .front{padding:10px 20px;display:flex;align-items:center;gap:14px}
.wr-lv .front .bb{flex:1;height:10px;position:relative}
.wr-lv .front .tick{position:absolute;top:-5px;width:3px;height:20px;background:var(--cream)}
.wr-lv .lv-stats{display:grid;grid-template-columns:repeat(5,1fr);border:1px solid var(--line);background:var(--panel-solid)}
.wr-lv .lv-stat{padding:14px 20px;border-right:1px solid var(--line);border-left:3px solid var(--top);min-width:0}
.wr-lv .lv-stat:last-child{border-right:0}
.wr-lv .lv-stat .big{margin-top:10px;white-space:nowrap}
.wr-lv .lv-stat .big small{font-size:30px;color:var(--muted)}
.wr-lv .lv-stat .s{margin-top:6px;font:500 15px/1.2 Barlow,sans-serif;color:var(--muted);white-space:nowrap;overflow:hidden;text-overflow:ellipsis}
.wr-lv .low{display:grid;grid-template-columns:1fr 1.15fr;gap:14px;flex:1;min-height:0}
.wr-lv .low .panel{padding:0 0 14px;display:flex;flex-direction:column}
.wr-lv .head{display:flex;align-items:center;justify-content:space-between;padding:12px 18px 0}
.wr-lv .live{font:700 13px/1 "Barlow Condensed",sans-serif;letter-spacing:.16em;color:var(--green);border:1px solid rgba(142,200,63,.5);padding:4px 8px}
.wr-lv .story{padding:12px 20px 0;font:400 17px/1.5 Barlow,sans-serif;color:var(--cream);overflow:hidden}
.wr-lv .story p{margin:0 0 9px}
.wr-lv .story h3{margin:0 0 4px;font:700 26px/1.1 Oswald,sans-serif;text-transform:uppercase;color:var(--cream)}
.wr-lv .story .stand{font:500 16px/1.35 Barlow,sans-serif;color:var(--orange);margin:0 0 10px}
.wr-lv .fronts{display:grid;grid-template-columns:repeat(3,1fr);gap:8px;padding:6px 20px 0;margin-top:auto}
.wr-lv .fronts div{border:1px solid var(--line);padding:9px 12px;background:rgba(18,14,10,.6);min-width:0}
.wr-lv .fronts b{display:block;font:600 18px/1 Oswald,sans-serif;color:var(--cream);margin-top:6px;white-space:nowrap;overflow:hidden;text-overflow:ellipsis}
.wr-lv .fronts span.x{font:500 13.5px/1.3 Barlow,sans-serif;color:var(--tan);display:block;margin-top:5px;white-space:nowrap;overflow:hidden;text-overflow:ellipsis}
.wr-lv .new{list-style:none;margin:0;padding:6px 18px 0}
.wr-lv .new li{display:grid;grid-template-columns:34px 1fr;gap:12px;align-items:start;padding:8px 0;border-bottom:1px solid var(--line)}
.wr-lv .new li:last-child{border-bottom:0}
.wr-lv .new .n{font:700 22px/1 Oswald,sans-serif;color:var(--orange);text-align:center;padding-top:2px}
.wr-lv .new .t{font:600 19px/1.15 Oswald,sans-serif;letter-spacing:.02em;color:var(--cream);text-transform:uppercase;white-space:nowrap;overflow:hidden;text-overflow:ellipsis}
.wr-lv .new .t.b{color:var(--blue)} .wr-lv .new .t.r{color:var(--red)} .wr-lv .new .t.x{color:var(--orange)}
.wr-lv .new .d{font:400 15px/1.35 Barlow,sans-serif;color:var(--tan);margin-top:3px;white-space:nowrap;overflow:hidden;text-overflow:ellipsis}
.wr-lv .aces{display:grid;grid-template-columns:repeat(3,1fr);gap:8px;padding:10px 18px 0;margin-top:auto}
.wr-lv .ace{border:1px solid var(--line);padding:9px 12px;background:rgba(18,14,10,.6);min-width:0}
.wr-lv .ace b{display:block;font:600 19px/1.1 Oswald,sans-serif;margin-top:5px;white-space:nowrap;overflow:hidden;text-overflow:ellipsis}
.wr-lv .ace b.b{color:var(--blue)} .wr-lv .ace b.r{color:var(--red)}
.wr-lv .ace span.x{font:500 13.5px/1.3 Barlow,sans-serif;color:var(--tan);display:block;margin-top:4px}
.wr-lv .quiet{padding:16px 20px;font:400 16px/1.5 Barlow,sans-serif;color:var(--muted)}
.wr-lv .foot{display:flex;justify-content:space-between;font:600 15px/1 "Barlow Condensed",sans-serif;letter-spacing:.18em;text-transform:uppercase;color:var(--muted)}
.wr-lv .foot b{color:var(--cream)}
`

export function LevantPoster({ model: m, dispatch, site, logo, id }: PosterProps) {
  const words = m.brand.trim().split(/\s+/)
  const lead = words.length > 1 ? words.slice(0, -1).join(' ') : ''
  const last = words[words.length - 1] ?? ''
  const pct = Math.round(m.blueShare * 1000) / 10
  const paras = paragraphs(dispatch.body)
  const ls = m.lastStand?.this_week ? m.lastStand : null
  const fronts: { k: string; v: string; x: string }[] = []
  if (m.contested[0]) fronts.push({ k: 'Most fought over', v: m.contested[0].name, x: `changed hands ${m.contested[0].captures} times` })
  if (m.squadrons[0]) fronts.push({ k: 'Squadron of the week', v: `${m.squadrons[0].name || m.squadrons[0].tag} [${m.squadrons[0].tag}]`, x: `${n(m.squadrons[0].pilots)} pilots · ${n(m.squadrons[0].score)} pts` })
  const fleetB = m.board[2].blueNote
  const fleetR = m.board[2].redNote
  if (fleetB || fleetR) fronts.push({ k: 'At sea', v: [fleetB, fleetR].filter(Boolean).join(' · '), x: `${m.blue} ${m.board[2].blue} · ${m.red} ${m.board[2].red} groups` })
  while (fronts.length < 3) fronts.push({ k: 'Deployed', v: `${n(m.totals.deployed)} units`, x: 'crates, troops and FARPs this week' })
  const evs = m.events.slice(0, 6)
  return (
    <div className="wr-lv" id={id} data-poster="levant">
      <style>{LV_CSS}</style>
      <div className="bar">
        {logo && <div className="logo">{logo}</div>}
        <div className="brand">VECTOR STRIKE</div>
        <div className="chip">{m.brand} · weekly report</div>
        <div className="right">
          <div><span className="lbl">Week</span><b>{m.week}</b></div>
          <div><span className="lbl">Server</span><b>{m.server.replace(/^\[VS\]\s*Vector Strike\s*\|?\s*/i, '') || m.server}</b></div>
          <div><span className="lbl">{m.roundNo ? `Round ${m.roundNo}` : 'Campaign'}</span><b>{m.day ? `Day ${m.day}` : 'Live'}</b></div>
        </div>
      </div>

      <div className="lv-hero">
        <div className="sit">
          <div><span className="tag">Weekly situation report</span></div>
          <h1 style={{ fontSize: fit(m.brand, [[14, 92], [20, 76], [28, 60]]) || 52 }}>{lead && <>{lead} </>}<span>{last}</span></h1>
          <div className="sub">{m.day ? `Day ${m.day} · ` : ''}{m.week}</div>
          <div className="flash"><b>{dispatch.headline}</b>{dispatch.standfirst}.</div>
        </div>
        <div className="sides">
          <div className="panel side nato">
            <div style={{ minWidth: 0 }}><div className="nm">{m.blue}</div><div className="who">{m.taken.blue ? `took ${n(m.taken.blue)} this week` : 'held the line'}</div></div>
            <div className="nums"><div><span className="lbl">Objectives</span><b>{n(m.board[0].blue)}</b></div><div><span className="lbl">Airbases</span><b>{n(m.board[1].blue)}</b></div></div>
          </div>
          <div className="panel front">
            <span className="lbl" style={{ color: 'var(--blue)' }}>{m.blue}</span>
            <div className="bb" style={{ background: `linear-gradient(90deg,var(--blue) 0 ${pct}%,var(--red) ${pct}% 100%)` }}>
              <span className="tick" style={{ left: `${pct}%` }} />
            </div>
            <span className="lbl" style={{ color: 'var(--red)' }}>{m.red}</span>
          </div>
          <div className="panel side rsii">
            <div style={{ minWidth: 0 }}><div className="nm">{m.red}</div><div className="who">{m.taken.red ? `took ${n(m.taken.red)} this week` : 'held the line'}</div></div>
            <div className="nums"><div><span className="lbl">Objectives</span><b>{n(m.board[0].red)}</b></div><div><span className="lbl">Airbases</span><b>{n(m.board[1].red)}</b></div></div>
          </div>
        </div>
      </div>

      <div className="lv-stats">
        <div className="lv-stat"><div className="lbl">Captures</div><div className="big">{n(m.totals.captures)}</div><div className="s">objectives changed hands</div></div>
        <div className="lv-stat"><div className="lbl">Air kills</div><div className="big">{n(m.totals.airKills)}</div><div className="s">{m.blue} {n(m.kills.blue.air)} · {m.red} {n(m.kills.red.air)}</div></div>
        <div className="lv-stat"><div className="lbl">Ground kills</div><div className="big">{n(m.totals.groundKills)}</div><div className="s">{m.blue} {n(m.kills.blue.ground)} · {m.red} {n(m.kills.red.ground)}</div></div>
        <div className="lv-stat"><div className="lbl">Hidden SAM sites</div><div className="big">{n(m.board[3].blue)} <small>/</small> {n(m.board[3].red)}</div><div className="s">{m.blue} / {m.red} still standing</div></div>
        <div className="lv-stat"><div className="lbl">{m.will ? 'Will to fight' : 'Pilots'}</div>
          {m.will
            ? <><div className="big">{Math.round(m.will.blue)} <small>/</small> {Math.round(m.will.red)}</div><div className="s">below {Math.round(m.will.threshold)} for 6 h: ask for terms</div></>
            : <><div className="big">{n(m.totals.activePilots)}</div><div className="s">flew and scored this week</div></>}
        </div>
      </div>

      <div className="low">
        <div className="panel">
          <div className="head"><span className="tag">The dispatch</span><span className="lbl">{m.week}</span></div>
          <div className="story">
            <h3>{dispatch.headline}</h3>
            <p className="stand">{dispatch.standfirst}.</p>
            {paras.slice(0, 4).map((p, i) => <p key={i}>{p}</p>)}
          </div>
          <div className="fronts">
            {fronts.slice(0, 3).map(f => <div key={f.k}><span className="lbl">{f.k}</span><b>{f.v}</b><span className="x">{f.x}</span></div>)}
          </div>
        </div>

        <div className="panel">
          <div className="head"><span className="tag">This week</span>{ls ? <span className="live" style={{ color: 'var(--red)', borderColor: 'rgba(217,73,59,.6)' }}>● LAST STAND</span> : <span className="live">● LIVE</span>}</div>
          {evs.length > 0 ? (
            <ul className="new">
              {evs.map((e, i) => (
                <li key={i}><div className="n">{String(i + 1).padStart(2, '0')}</div>
                  <div style={{ minWidth: 0 }}><div className={`t ${e.kind === 'laststand' ? 'x' : sideClass(e.side)}`}>{e.title}</div>{e.detail && <div className="d">{e.detail}</div>}</div></li>
              ))}
            </ul>
          ) : <div className="quiet">No captures or story events this week. The front held where it was.</div>}
          {m.pilots.length > 0 && (
            <div className="aces">
              {m.pilots.slice(0, 3).map((p, i) => (
                <div className="ace" key={p.name}>
                  <span className="lbl">{i === 0 ? 'Pilot of the week' : `No. ${i + 1}`}</span>
                  <b className={sideClass(p.side)}>{p.name}</b>
                  <span className="x">{[p.air_kills && `${p.air_kills} A/A`, p.ground_kills && `${p.ground_kills} A/G`, p.captures && `${p.captures} caps`].filter(Boolean).join(' · ') || `${n(p.score)} pts`}</span>
                </div>
              ))}
            </div>
          )}
        </div>
      </div>

      <div className="foot">
        <span>{site?.wiki ? <><b>{host(site.wiki)}</b> · full details</> : ''}</span>
        <span>Territory, stocks and aircraft carry over between sessions</span>
        <span>{site?.dashboard ? <><b>{host(site.dashboard)}</b> · live map</> : ''}</span>
      </div>
    </div>
  )
}

/** The poster for the model's theme, at its full size. */
export function WeeklyPoster(props: PosterProps) {
  return props.model.theme === 'redstorm' ? <RedStormPoster {...props} /> : <LevantPoster {...props} />
}

