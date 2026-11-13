import { useMemo } from 'react'
import { api } from '../api'
import { campaign } from '../config/campaign'
import { ReportPrint } from '@shared/web/report/ReportPrint'
import { LEVANT_FIXTURE, REDSTORM_FIXTURE } from '@shared/web/report/fixture'

/** `/report/print`: the weekly report poster alone, for the FowlEngine bot's
 *  weekly schedule (see shared/web/report/ReportPrint.tsx). No chrome, no
 *  login. `?fixture=redstorm|levant` draws sample data in a dev build. */
export default function ReportPrintPage() {
  const fallback = useMemo(
    () => ({ blue: campaign.blueLabel, red: campaign.redLabel, brand: campaign.name }),
    [],
  )
  const which = import.meta.env.DEV ? new URLSearchParams(window.location.search).get('fixture') : null
  const fixture = which === 'redstorm' ? REDSTORM_FIXTURE : which === 'levant' ? LEVANT_FIXTURE : null
  return <ReportPrint fetch={api.weeklyReport} fallback={fallback} fixture={fixture} />
}
