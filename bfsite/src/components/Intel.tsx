import { useQuery } from '@tanstack/react-query'
import { ArrowRight, Rankings, Tacmap, Wiki } from '@icons'
import { API_ROOT, api, type NewsDay } from '../api'
import { dashboard, links, wiki } from '../config/campaign'
import { useTheatres } from '../live'
import { SectionHead } from './Bits'
import Reveal from './Reveal'

function dateLine(day: string): string {
  const d = new Date(`${day}T12:00:00Z`)
  if (Number.isNaN(d.getTime())) return day
  return d.toLocaleDateString('en-GB', { weekday: 'long', day: 'numeric', month: 'long', year: 'numeric', timeZone: 'UTC' })
}

/** The latest War Diary front page, in miniature. Falls back to a plain pitch with no paper to show. */
function DiaryCard() {
  const { defaultInstance, theatres } = useTheatres()
  const news = useQuery({
    queryKey: ['news', defaultInstance],
    queryFn: () => api.latestNews(defaultInstance),
    enabled: defaultInstance !== undefined,
    refetchInterval: 5 * 60_000,
  })
  const day: NewsDay | undefined = news.data?.days[0]
  const server = theatres.find(t => t.key === defaultInstance)?.label
  const excerpt = day?.body?.[0] ?? day?.items?.map(i => i.text).filter(Boolean).slice(0, 2).join(' ')

  return (
    <a className={`panel diary${day?.image ? ' has-img' : ''}`} href={dashboard('/news')}>
      {day?.image && (
        <div className="diary-img">
          <img src={`${API_ROOT}${day.image}`} alt="" loading="lazy" />
        </div>
      )}
      <div className="diary-body">
        <div className="diary-masthead">The War Diary</div>
        <div className="diary-date">
          {day ? `${dateLine(day.day)}${server ? ` · ${server}` : ''}` : 'Filed daily by the front-line correspondent'}
        </div>
        <h3 className="diary-headline">
          {day ? day.headline : 'Every day of the campaign, written up as a front page'}
        </h3>
        <p className="diary-excerpt">
          {excerpt ||
            'Who took what, who lost what, and the ace of the day: a newspaper for each day of the war, read by both sides. No sign-in needed.'}
        </p>
        <span className="text-link">Read the paper <ArrowRight size={12} /></span>
      </div>
    </a>
  )
}

export default function Intel() {
  return (
    <section id="intel" className="section alt">
      <div className="wrap">
        <SectionHead
          num={4}
          para="Service support"
          title={<>Intel, <em>off the cockpit.</em></>}
          lede="The war keeps a record you can read from anywhere: a live dashboard, a daily paper, and a wiki that explains every system in the campaign."
        />

        <div className="intel-grid">
          <Reveal><DiaryCard /></Reveal>

          <div className="intel-side">
            <Reveal delay={80}>
              <a className="panel linkcard" href={links.dashboard}>
                <span className="linkcard-icon"><Tacmap size={20} /></span>
                <div>
                  <h3>Ops dashboard</h3>
                  <p>The SITREP, the tactical map and every objective’s state, live from the server. Sign in to see your coalition’s picture.</p>
                  <div className="linkcard-links">
                    <span className="text-link">Open SITREP <ArrowRight size={12} /></span>
                  </div>
                </div>
              </a>
            </Reveal>
            <Reveal delay={140}>
              <a className="panel linkcard" href={dashboard('/leaderboard')}>
                <span className="linkcard-icon"><Rankings size={20} /></span>
                <div>
                  <h3>Pilot records</h3>
                  <p>Leaderboards, pilot careers, squadrons and a debrief for every sortie you fly.</p>
                  <div className="linkcard-links">
                    <span className="text-link">Leaderboard <ArrowRight size={12} /></span>
                  </div>
                </div>
              </a>
            </Reveal>
            <Reveal delay={200}>
              <div className="panel linkcard">
                <span className="linkcard-icon"><Wiki size={20} /></span>
                <div>
                  <h3>The wiki</h3>
                  <p>The field manual: menus, logistics, capture rules, lives and points, quoted from the live server config.</p>
                  <div className="linkcard-links">
                    <a className="text-link" href={links.wiki}>Wiki home <ArrowRight size={12} /></a>
                    <a className="text-link" href={wiki('gameplay/points-and-lives')}>Points &amp; lives</a>
                    <a className="text-link" href={wiki('reference/faq')}>FAQ</a>
                  </div>
                </div>
              </div>
            </Reveal>
          </div>
        </div>
      </div>
    </section>
  )
}
