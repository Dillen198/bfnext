/**
 * Tanker planning: how much gas to take and how long it takes.
 *
 * The receiver keeps burning while plugged in, so the net fill rate is the
 * transfer rate minus the burn. Everything here is pounds and minutes.
 */

export interface AarPlanInput {
  /** fuel now, lb */
  fuel_lb: number
  /** cruise burn, lb/min */
  burn_lb_min: number
  /** distance to the tanker, nm */
  dist_nm: number
  /** transit ground speed, kt */
  gs_kts: number
  /** bingo, lb */
  bingo_lb: number
  /** fuel you need AFTER leaving the tanker, above bingo, lb */
  mission_lb: number
  /** max internal (+ tanks) fuel, lb */
  capacity_lb: number
  /** tanker-to-receiver transfer rate, lb/min */
  transfer_lb_min: number
}

export interface AarPlan {
  transit_min: number
  fuel_at_tanker_lb: number
  /** arrive with less than bingo */
  arrive_below_bingo: boolean
  /** fuel to leave the tanker with */
  fill_to_lb: number
  /** limited by tank capacity */
  capped: boolean
  /** net fuel you need to gain on the tanker */
  onload_lb: number
  /** what the tanker actually passes (net + burn while connected) */
  transferred_lb: number
  /** time connected, minutes (Infinity if the transfer rate cannot beat the burn) */
  time_connected_min: number
  feasible: boolean
}

export function planAar(i: AarPlanInput): AarPlan {
  const transit = i.gs_kts > 0 ? (i.dist_nm / i.gs_kts) * 60 : 0
  const atTanker = i.fuel_lb - i.burn_lb_min * transit
  const want = i.bingo_lb + i.mission_lb
  const fillTo = Math.min(i.capacity_lb, want)
  const onload = Math.max(0, fillTo - Math.max(0, atTanker))
  const net = i.transfer_lb_min - i.burn_lb_min
  const feasible = onload === 0 || net > 0
  const t = onload === 0 ? 0 : net > 0 ? onload / net : Infinity
  return {
    transit_min: transit,
    fuel_at_tanker_lb: atTanker,
    arrive_below_bingo: atTanker < i.bingo_lb,
    fill_to_lb: fillTo,
    capped: want > i.capacity_lb,
    onload_lb: onload,
    transferred_lb: Number.isFinite(t) ? i.transfer_lb_min * t : Infinity,
    time_connected_min: t,
    feasible,
  }
}

export interface ReceiverPreset {
  id: string
  label: string
  capacity_lb: number
  burn_lb_min: number
  bingo_lb: number
  gs_kts: number
  method: 'boom' | 'drogue'
}

/** Rough cruise numbers; every field is editable in the planner. */
export const RECEIVERS: ReceiverPreset[] = [
  { id: 'fa18', label: 'F/A-18C (internal)', capacity_lb: 10_800, burn_lb_min: 95, bingo_lb: 3_500, gs_kts: 420, method: 'drogue' },
  { id: 'f14', label: 'F-14B (internal)', capacity_lb: 16_200, burn_lb_min: 120, bingo_lb: 4_000, gs_kts: 420, method: 'drogue' },
  { id: 'f16', label: 'F-16C (internal)', capacity_lb: 7_000, burn_lb_min: 70, bingo_lb: 2_000, gs_kts: 440, method: 'boom' },
  { id: 'f15e', label: 'F-15E (int + CFT)', capacity_lb: 23_000, burn_lb_min: 150, bingo_lb: 4_500, gs_kts: 440, method: 'boom' },
  { id: 'a10', label: 'A-10C', capacity_lb: 11_000, burn_lb_min: 45, bingo_lb: 2_500, gs_kts: 300, method: 'boom' },
  { id: 'av8', label: 'AV-8B', capacity_lb: 7_500, burn_lb_min: 85, bingo_lb: 2_000, gs_kts: 400, method: 'drogue' },
  { id: 'm2000', label: 'M-2000C', capacity_lb: 6_950, burn_lb_min: 80, bingo_lb: 1_800, gs_kts: 430, method: 'drogue' },
]

export interface TankerPreset {
  type: string
  label: string
  method: 'boom' | 'drogue'
  /** a typical DCS transfer rate for planning, lb/min */
  transfer_lb_min: number
}

export const TANKERS: TankerPreset[] = [
  { type: 'KC-135', label: 'KC-135 (boom)', method: 'boom', transfer_lb_min: 1_500 },
  { type: 'KC135MPRS', label: 'KC-135MPRS (drogue)', method: 'drogue', transfer_lb_min: 1_000 },
  { type: 'KC130', label: 'KC-130 (drogue)', method: 'drogue', transfer_lb_min: 1_000 },
  { type: 'S-3B Tanker', label: 'S-3B (buddy)', method: 'drogue', transfer_lb_min: 700 },
  { type: 'A-6E', label: 'A-6E (buddy)', method: 'drogue', transfer_lb_min: 700 },
  { type: 'IL-78M', label: 'IL-78M (drogue)', method: 'drogue', transfer_lb_min: 1_100 },
]
