/**
 * The written half of a debrief: what the engine measured, per discipline,
 * laid out like the result card's text so the two read together.
 */
import type { ReactNode } from 'react'
import { Link } from 'react-router-dom'
import { GradeBadge, QualityChip, ScoreChip } from '../../components/Chips'
import { airframe, fmt, fmtClock, fmtSecs, fmtSigned, mToFt, pad3, weaponName } from '../../lib/format'
import { engagementOutcomeLabel, missileOutcomeLabel, PASS_OUTCOME_LABEL } from '../../lib/headline'
import { gradeName, parseCall } from '../../lib/lso'
import { NM } from '../../lib/geo'
import type { RangeRecord } from '../../types'

type Row = [string, ReactNode]

function KV({ rows }: { rows: Row[] }) {
  return (
    <dl className="kv">
      {rows.map(([k, v], i) => (
        <div key={`${i}-${k}`} className="contents">
          <dt>{k}</dt>
          <dd>{v}</dd>
        </div>
      ))}
    </dl>
  )
}

function Section({ title, children }: { title: string; children: ReactNode }) {
  return (
    <section className="mt-4 first:mt-0">
      <h3 className="caps mb-2 mt-0">{title}</h3>
      {children}
    </section>
  )
}

const yn = (b: boolean | null | undefined) => (b === null || b === undefined ? '—' : b ? 'yes' : 'no')
const ft = (m: number) => `${fmt(mToFt(m))} ft`
const nm = (m: number) => `${fmt(m / NM, 1)} nm`

export function Facts({ rec }: { rec: RangeRecord }) {
  const r = rec.result
  switch (r.kind) {
    case 'trap': {
      const calls = r.lso_comment.split(/\s+/).filter(Boolean).map(parseCall)
      const eng = r.engine_grade
      return (
        <>
          <div className="flex items-center gap-3 flex-wrap">
            <GradeBadge grade={r.grade} large />
            <div>
              <div className="font-semibold text-[17px]">{gradeName(r.grade)}</div>
              <div className="muted text-[12.5px]">
                {r.points === null ? 'does not count' : `${fmt(r.points, 1)} points`} · {PASS_OUTCOME_LABEL[r.outcome]}
                {r.wire ? ` · #${r.wire} wire${r.wire_from_dcs ? '' : ' (estimated)'}` : ''}
              </div>
            </div>
          </div>
          <Section title="LSO comment">
            <div className="mono text-[15px] font-medium tracking-wide">{r.lso_comment || '— no deviations called'}</div>
            {calls.length > 0 && (
              <ul className="m-0 mt-2 p-0 list-none flex flex-col gap-1">
                {calls.map((c, i) => (
                  <li key={i} className="grid grid-cols-[64px_1fr] gap-2 text-[13px]">
                    <span className="mono" style={{ color: c.magnitude === 'lot' ? 'var(--wave)' : c.magnitude === 'normal' ? 'var(--tone-fair)' : 'var(--tone-good)' }}>{c.raw}</span>
                    <span>{r.lso_description[i] ?? c.text}</span>
                  </li>
                ))}
              </ul>
            )}
            {r.dcs_comment && <div className="mono text-[11.5px] dim mt-2">DCS: {r.dcs_comment}</div>}
            {eng && (eng.grade !== r.grade || eng.comment !== r.lso_comment) && (
              <div className="text-[12px] muted mt-2">
                Range engine graded it <span className="mono" style={{ color: 'var(--chalk)' }}>{eng.grade}</span>
                {eng.comment ? <> · <span className="mono">{eng.comment}</span></> : null}
                {r.source === 'dcs' ? ' (DCS’s LSO grade wins)' : ''}
              </div>
            )}
          </Section>
          <Section title="The pass">
            <KV rows={[
              ['Carrier', r.carrier],
              ['Case · light', `${['I', 'II', 'III'][r.case - 1] ?? r.case} · ${r.night ? 'night' : 'day'}`],
              ['Groove time', fmtSecs(r.groove_time_s)],
              ['Wind over deck', r.wind_over_deck_kts === null ? '—' : `${fmt(r.wind_over_deck_kts, 1)} kt`],
              ['Final bearing', r.final_bearing_deg === null ? '—' : `${pad3(r.final_bearing_deg)}°`],
              ['Hook down', yn(r.hook_down)],
              ['Graded by', r.source === 'dcs' ? 'DCS Supercarrier LSO' : 'range engine'],
            ]} />
          </Section>
          <Section title="Pattern">
            <KV rows={[
              ['Break altitude', r.pattern.break_alt_ft === null ? '—' : `${fmt(r.pattern.break_alt_ft)} ft`],
              ['Abeam', `${r.pattern.abeam_distance_nm === null ? '—' : `${fmt(r.pattern.abeam_distance_nm, 2)} nm`} · ${r.pattern.abeam_alt_ft === null ? '—' : `${fmt(r.pattern.abeam_alt_ft)} ft`}`],
              ['At the 90', r.pattern.ninety_alt_ft === null ? '—' : `${fmt(r.pattern.ninety_alt_ft)} ft`],
              ['Wake altitude', r.pattern.wake_alt_ft === null ? '—' : `${fmt(r.pattern.wake_alt_ft)} ft`],
              ['Break to deck', fmtSecs(r.pattern.pattern_time_s)],
            ]} />
            {r.pattern.notes.length > 0 && (
              <ul className="m-0 mt-2 pl-4 text-[12.5px] muted">{r.pattern.notes.map(n => <li key={n}>{n}</li>)}</ul>
            )}
          </Section>
        </>
      )
    }
    case 'bomb':
      return (
        <>
          <div className="flex items-baseline gap-3 flex-wrap">
            <span className="num text-[34px] font-medium leading-none">{fmt(r.miss_m, 1)}<span className="text-[15px] muted"> m</span></span>
            <QualityChip q={r.quality} />
            <span className="muted text-[13px]">{r.clock} o'clock · φ {fmt(r.radial_deg, 1)}°</span>
          </div>
          <Section title="Weapon">
            <KV rows={[
              ['Weapon', weaponName(r.weapon, r.weapon_display)],
              ['Class · guidance', `${r.weapon_class} · ${r.guidance || 'none'}`],
              ...(r.laser_code !== undefined ? [['Laser code', String(r.laser_code)] as Row] : []),
              ['Target', r.target],
              ['Range', r.range],
              ['Graded against', `GOOD ≤ ${fmt(r.good_radius_m)} m`],
              ['Long / short', `${fmtSigned(r.long_m, 1)} m ${r.long_m >= 0 ? '(long)' : '(short)'}`],
              ['Left / right', `${fmtSigned(r.cross_m, 1)} m ${r.cross_m >= 0 ? '(right)' : '(left)'}`],
              ['Time of fall', fmtSecs(r.time_of_flight_s)],
              ['Hit the target object', yn(r.target_hit)],
            ]} />
          </Section>
          <Section title="Release">
            <KV rows={[
              ['Altitude', `${ft(r.release.alt_agl_m)} AGL`],
              ['TAS · GS', `${fmt(r.release.tas_kts)} · ${fmt(r.release.gs_kts)} kt`],
              ['Mach', fmt(r.release.mach, 2)],
              ['Track', `${pad3(r.release.heading_deg)}°`],
              ['Dive', `${fmt(r.release.dive_deg, 1)}°`],
              ['Slant · ground range', `${fmt(r.release.slant_range_m)} · ${fmt(r.release.ground_range_m)} m`],
              ['Wind', `${pad3(r.release.wind_from_deg)}° / ${fmt(r.release.wind_kts)} kt`],
            ]} />
            <Link to={`/calc?tool=bomb&alt=${Math.round(mToFt(r.release.alt_agl_m))}&tas=${Math.round(r.release.tas_kts)}&dive=${fmt(r.release.dive_deg, 0)}&hdg=${Math.round(r.release.heading_deg)}&wf=${Math.round(r.release.wind_from_deg)}&wk=${Math.round(r.release.wind_kts)}&w=${encodeURIComponent(r.weapon)}&elev=${Math.round(mToFt(r.target_pos.alt_m))}&station=${encodeURIComponent(r.station_id)}`}
              className="btn-range sm mt-3">Open these numbers in the release calculator</Link>
          </Section>
        </>
      )
    case 'strafe':
      return (
        <>
          <div className="flex items-baseline gap-3 flex-wrap">
            <span className="num text-[34px] font-medium leading-none">{fmt(r.accuracy_pct, 1)}<span className="text-[15px] muted">%</span></span>
            <QualityChip q={r.quality} />
          </div>
          <Section title="Pass">
            <KV rows={[
              ['Pit', `${r.pit} · ${r.range}`],
              ['Gun', r.gun],
              ['Hits / rounds', `${r.hits} / ${r.rounds_fired}`],
              ['Run-in', `${pad3(r.run_in_heading_deg)}°`],
              ['Entry altitude', `${ft(r.entry_alt_agl_m)} AGL`],
              ['Closest range', `${fmt(r.min_range_m)} m`],
              ['Foul line', `${fmt(r.foul_line_m)} m${r.foul_line_crossed ? ' · CROSSED' : ''}`],
            ]} />
            {r.invalid_reason && <div className="chip bad mt-2">{r.invalid_reason}</div>}
          </Section>
        </>
      )
    case 'aar':
      return (
        <>
          <div className="flex items-baseline gap-3 flex-wrap">
            <span className="display text-[44px]" style={{ color: 'var(--chalk)' }}>{r.grade}</span>
            <ScoreChip score={rec.score} />
            <span className="muted text-[13px]">{fmt(r.fuel_lbs)} lb taken</span>
          </div>
          <Section title="Calls">
            <ul className="m-0 pl-4 text-[13px] flex flex-col gap-1">{r.calls.map(c => <li key={c}>{c}</li>)}</ul>
          </Section>
          <Section title="Session">
            <KV rows={[
              ['Tanker', `${r.tanker} · ${airframe(r.tanker_type)}`],
              ['Method', r.method],
              ['Join (1 nm → contact)', r.join_time_s === null ? '—' : fmtClock(r.join_time_s)],
              ['Contacts · disconnects', `${r.contacts} · ${r.disconnects}`],
              ['Time connected', fmtClock(r.time_connected_s)],
              ['Fuel', `${fmt(r.fuel_lbs)} lb · ${fmt(r.fuel_kg)} kg`],
              ['Onload rate', `${fmt(r.onload_rate_lbs_min)} lb/min`],
              ['Pre-contact closure', r.precontact_closure_kts === null ? '—' : `${fmt(r.precontact_closure_kts, 1)} kt`],
              ['Overshoot', yn(r.overshoot)],
              ['Tanker altitude · speed', `${fmt(r.alt_ft)} ft · ${fmt(r.speed_kts)} kt`],
              ['Session', fmtClock(r.session_s)],
            ]} />
          </Section>
          <Section title="Stability in contact (SD)">
            <KV rows={[
              ['Fore-aft', `${fmt(r.stability.fore_aft_sd_m, 2)} m`],
              ['Lateral', `${fmt(r.stability.lateral_sd_m, 2)} m`],
              ['Vertical', `${fmt(r.stability.vertical_sd_m, 2)} m`],
            ]} />
          </Section>
        </>
      )
    case 'missile': {
      const mine = r.perspective === 'target'
      return (
        <>
          <div className="flex items-baseline gap-3 flex-wrap">
            <span className="display text-[34px]" style={{ color: r.outcome === 'kill' ? (mine ? 'var(--wave)' : 'var(--datum)') : mine ? 'var(--datum)' : 'var(--haze)' }}>
              {r.outcome === 'kill' ? (mine ? 'You’d be dead' : 'Splash') : missileOutcomeLabel(r.outcome)}
            </span>
          </div>
          <p className="muted text-[13px] mt-1 mb-0">
            {mine
              ? r.outcome === 'kill'
                ? `The trainer destroyed the ${weaponName(r.weapon)} ${fmt(r.min_distance_m)} m from you: inside the ${fmt(r.kill_radius_m)} m kill radius.`
                : `The ${weaponName(r.weapon)} never got inside ${fmt(r.kill_radius_m)} m: closest ${fmt(r.min_distance_m)} m.`
              : r.outcome === 'kill'
                ? `Your ${weaponName(r.weapon)} reached ${fmt(r.min_distance_m)} m of ${r.target.name}: a kill.`
                : `Your ${weaponName(r.weapon)} was defeated at ${fmt(r.min_distance_m)} m.`}
          </p>
          <Section title="Launch">
            <KV rows={[
              ['Shooter', `${r.shooter.name} · ${airframe(r.shooter_type)}`],
              ['Target', `${r.target.name} · ${airframe(r.target_type)}`],
              ['Weapon', `${weaponName(r.weapon)} (${r.weapon_category.toUpperCase()})`],
              ['Range', nm(r.launch.range_m)],
              ['Aspect', `${fmt(r.launch.aspect_deg)}° ${r.launch.aspect_deg < 45 ? '(hot)' : r.launch.aspect_deg > 135 ? '(cold)' : '(flank)'}`],
              ['Altitudes', `${ft(r.launch.shooter_alt_m)} → ${ft(r.launch.target_alt_m)}`],
              ['Speeds', `${fmt(r.launch.shooter_speed_kts)} / ${fmt(r.launch.target_speed_kts)} kt`],
              ['Closure', `${fmt(r.launch.closure_kts)} kt`],
              ['Time of flight', fmtSecs(r.time_of_flight_s)],
            ]} />
          </Section>
          <Section title="Defence">
            <KV rows={[
              ['First turn', r.defense.reaction_s === null ? 'none' : `${fmt(r.defense.reaction_s, 1)} s after launch`],
              ['Beaming', fmtSecs(r.defense.beam_s)],
              ['Dragging', fmtSecs(r.defense.drag_s)],
              ['Hot (nose on)', fmtSecs(r.defense.hot_s)],
              ['Altitude change', `${fmtSigned(mToFt(r.defense.alt_change_m), 0)} ft`],
              ['Went low', yn(r.defense.went_low)],
            ]} />
          </Section>
        </>
      )
    }
    case 'engagement':
      return (
        <>
          <div className="display text-[38px]" style={{ color: r.outcome === 'win' ? 'var(--datum)' : r.outcome === 'loss' ? 'var(--wave)' : 'var(--haze)' }}>
            {engagementOutcomeLabel(r.outcome)}
          </div>
          <Section title="Engagement">
            <KV rows={[
              ['Setup', r.setup],
              ['Adversary', r.opponent?.ucid ? <Link to={`/pilot/${encodeURIComponent(r.opponent.ucid)}`} className="underline">{r.adversary}</Link> : `${airframe(r.adversary)}${r.adversary_skill ? ` (${r.adversary_skill})` : ''}`],
              ['Duration', fmtClock(r.duration_s)],
              ['Shots · trainer kills', `${r.shots_fired} · ${r.trainer_kills}`],
              ['Gun hits', String(r.gun_hits)],
            ]} />
            {r.notes.length > 0 && <ul className="m-0 mt-2 pl-4 text-[12.5px] muted">{r.notes.map(n => <li key={n}>{n}</li>)}</ul>}
          </Section>
        </>
      )
    case 'anti_ship':
      return (
        <>
          <div className="display text-[38px]" style={{ color: r.hit ? 'var(--datum)' : 'var(--wave)' }}>{r.hit ? (r.ship_sunk ? 'Hit · sunk' : 'Hit') : 'Miss'}</div>
          <Section title="Attack">
            <KV rows={[
              ['Ship', `${r.ship} · ${r.ship_type}`],
              ['Weapon', weaponName(r.weapon)],
              ['Launch range', `${nm(r.launch_range_m)}${r.weapon_max_range_m ? ` · ${fmt((r.launch_range_m / r.weapon_max_range_m) * 100)}% of max` : ''}`],
              ['Damage', `${fmt(r.damage * 100)}%`],
              ['Time of flight', fmtSecs(r.time_of_flight_s)],
              ['Intercepted', yn(r.intercepted)],
            ]} />
          </Section>
        </>
      )
    case 'sling':
      return (
        <>
          <div className="flex items-baseline gap-3"><span className="num text-[34px]">{fmt(r.distance_m, 1)}<span className="text-[15px] muted"> m</span></span><QualityChip q={r.quality} /></div>
          <Section title="Load">
            <KV rows={[
              ['Carried', r.method === 'internal' ? 'Inside (DCS dynamic cargo)' : 'Slung'],
              ['Delivered to', r.course], ['Cargo', `${r.cargo} · ${fmt(r.mass_kg)} kg`], ['Pickup → set-down', fmtClock(r.time_s)],
              ['Damage', `${fmt(r.damage * 100)}%`],
            ]} />
          </Section>
        </>
      )
    case 'landing':
      return (
        <>
          <div className="flex items-baseline gap-3"><span className="num text-[34px]">{fmt(r.distance_m, 1)}<span className="text-[15px] muted"> m</span></span><QualityChip q={r.quality} /></div>
          <Section title="Touchdown">
            <KV rows={[
              ['Drill', r.drill], ['Pad', r.pad], ['Sink rate', `${fmt(r.touchdown_fpm)} fpm`],
              ['Heading error', r.heading_error_deg === null ? '—' : `${fmtSigned(r.heading_error_deg, 1)}°`],
              ['Hover before touchdown', fmtSecs(r.hover_s)],
            ]} />
          </Section>
        </>
      )
    case 'troops':
      return (
        <>
          <div className="flex items-baseline gap-3"><span className="num text-[34px]">{fmtClock(r.total_time_s)}</span><QualityChip q={r.quality} /></div>
          <Section title="Insertion">
            <KV rows={[['LZ', r.lz], ['Troops', String(r.troops)], ['Load time', fmtSecs(r.load_time_s)], ['Landing distance', `${fmt(r.landing_distance_m, 1)} m`]]} />
          </Section>
        </>
      )
    case 'gunnery':
      return (
        <>
          <div className="flex items-baseline gap-3"><span className="num text-[34px]">{r.targets_killed}/{r.targets_total}</span><ScoreChip score={rec.score} /></div>
          <Section title="Lane">
            <KV rows={[
              ['Lane', r.lane], ['Shots · hits', `${r.shots} · ${r.hits} (${fmt((r.hits / Math.max(1, r.shots)) * 100)}%)`],
              ['First-round hits', String(r.first_round_hits)], ['Time', fmtClock(r.time_s)],
            ]} />
          </Section>
        </>
      )
    case 'cas':
      return (
        <>
          <div className="flex items-baseline gap-3 flex-wrap">
            <span className="display text-[34px]" style={{ color: r.correct_target ? 'var(--datum)' : 'var(--wave)' }}>{r.correct_target ? 'On target' : 'Wrong target'}</span>
            {r.danger_close && <span className="chip warn">DANGER CLOSE</span>}
          </div>
          <Section title="9-line to impact">
            <KV rows={[
              ['JTAC', r.jtac], ['Target', r.target], ['Weapon', weaponName(r.weapon)],
              ['9-line → impact', fmtClock(r.time_to_impact_s)], ['Miss', `${fmt(r.miss_m, 1)} m`],
              ['Nearest friendly', r.nearest_friendly_m === null ? '—' : `${fmt(r.nearest_friendly_m)} m`],
              ['Laser code', r.laser_code === null ? '—' : String(r.laser_code)],
            ]} />
          </Section>
        </>
      )
  }
}
