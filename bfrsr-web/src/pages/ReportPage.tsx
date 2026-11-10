import { useMemo } from 'react'
import { Navigate } from 'react-router-dom'
import { api } from '../api'
import { campaign } from '../config/campaign'
import { useAuth } from '../context/AuthContext'
import { useInstance } from '../context/InstanceContext'
import PageHeader from '../components/PageHeader'
import { posterSnapshot } from '../lib/posterSnapshot'
import { ReportStudio, type ReportApi } from '@shared/web/report/ReportStudio'
import { Printer } from '@icons'

const reportApi: ReportApi = {
  fetch: days => api.weeklyReport(days),
  post: body => api.ops.postReport(body),
  status: () => api.ops.reportStatus(),
}

/** Admins: the campaign's weekly report poster in the situation-room look,
 *  to export or post to Discord (shared/web/report). */
export default function ReportPage() {
  const { user, loading } = useAuth()
  const { current } = useInstance()
  const fallback = useMemo(
    () => ({ blue: campaign.blueLabel, red: campaign.redLabel, brand: campaign.name }),
    // eslint-disable-next-line react-hooks/exhaustive-deps
    [current?.id, campaign.blueLabel, campaign.redLabel],
  )
  if (loading) return null
  if (!user) return <Navigate to="/login" replace />
  if (!user.is_admin) return <Navigate to="/" replace />
  const canPost = user.is_admin || !!user.roles?.includes('ops')
  return (
    <div className="flex flex-col flex-1 overflow-hidden">
      <PageHeader icon={Printer} title="WEEKLY REPORT" file="Weekly report" stamp={<span className="stamp">Restricted</span>}
        sub="The week on the Southern Flank on one page, for Discord" />
      <div className="flex-1 overflow-auto p-4">
        <ReportStudio key={current?.id ?? 'default'} api={reportApi} snapshot={posterSnapshot} fallback={fallback} canPost={canPost}
          instanceKey={current?.id ?? 'default'} />
      </div>
    </div>
  )
}
