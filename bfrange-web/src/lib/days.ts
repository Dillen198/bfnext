/** Time-window choices for the filters; 0 = all time. */
export const DAY_OPTIONS = [7, 30, 90, 0] as const

/**
 * `days` for bfdb's windowed aggregates (greenie, leaderboards, impacts),
 * which clamp to 1..365 and default to 30: "All" (0) asks for the full year.
 * The results list is different -- there, no `days` means all time.
 */
export const windowDays = (d: number) => (d > 0 ? Math.min(d, 365) : 365)
