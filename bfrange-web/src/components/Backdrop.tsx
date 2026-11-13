/**
 * The wall behind the pages: one line drawing per page, of the carrier, a
 * Hornet or a Tomcat, traced from the site's own 3D models
 * (scripts/wireframes.mjs), tinted from the theme and faded out towards the
 * text, with a low amber glow off the meatball and a little sky. Each page
 * group gets its own. Purely decoration: hidden from screen readers and
 * from print.
 */
import carrier from '../assets/backdrops/carrier.svg'
import hornet from '../assets/backdrops/hornet.svg'
import tomcat from '../assets/backdrops/tomcat.svg'

type Scene = 'deck' | 'ramp' | 'cap'

const ART: Record<Scene, { src: string; cls: string }> = {
  // the carrier from the stern quarter
  deck: { src: carrier, cls: 'bd-carrier' },
  // the Hornet large, coming at you
  ramp: { src: hornet, cls: 'bd-hornet-big' },
  // the Tomcat from above, wings out
  cap: { src: tomcat, cls: 'bd-tomcat' },
}

function sceneFor(path: string): Scene {
  if (/^\/(leaderboards|calc|spawn)/.test(path)) return 'cap'
  if (/^\/(result|pilot|me|results)/.test(path)) return 'ramp'
  return 'deck'
}

export function Backdrop({ path }: { path: string }) {
  const scene = sceneFor(path)
  const a = ART[scene]
  return (
    <div className={`backdrop bd-${scene}`} aria-hidden>
      <div className={`bd-art ${a.cls}`} style={{ maskImage: `url("${a.src}")`, WebkitMaskImage: `url("${a.src}")` }} />
    </div>
  )
}
