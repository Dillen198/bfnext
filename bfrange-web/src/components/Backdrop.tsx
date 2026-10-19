/**
 * The wall behind the pages: line drawings of a Hornet, a Tomcat and the
 * carrier, traced from the site's own 3D models (scripts/wireframes.mjs),
 * tinted from the theme and faded out towards the text, with a low amber
 * glow off the meatball and a little sky. Each page group gets its own
 * arrangement. Purely decoration: hidden from screen readers and from print.
 */
import carrier from '../assets/backdrops/carrier.svg'
import hornet from '../assets/backdrops/hornet.svg'
import tomcat from '../assets/backdrops/tomcat.svg'

type Scene = 'deck' | 'ramp' | 'cap'

const ART: Record<Scene, { src: string; cls: string }[]> = {
  // the carrier from the stern quarter, a Hornet up and to the right
  deck: [{ src: carrier, cls: 'bd-carrier' }, { src: hornet, cls: 'bd-hornet' }],
  // the Hornet large, coming at you, the carrier small and far off
  ramp: [{ src: hornet, cls: 'bd-hornet-big' }, { src: carrier, cls: 'bd-carrier-far' }],
  // the Tomcat from above, wings out
  cap: [{ src: tomcat, cls: 'bd-tomcat' }, { src: carrier, cls: 'bd-carrier-far' }],
}

function sceneFor(path: string): Scene {
  if (/^\/(leaderboards|calc|spawn)/.test(path)) return 'cap'
  if (/^\/(result|pilot|me|results)/.test(path)) return 'ramp'
  return 'deck'
}

export function Backdrop({ path }: { path: string }) {
  const scene = sceneFor(path)
  return (
    <div className={`backdrop bd-${scene}`} aria-hidden>
      {ART[scene].map(a => (
        <div key={a.cls} className={`bd-art ${a.cls}`} style={{ maskImage: `url("${a.src}")`, WebkitMaskImage: `url("${a.src}")` }} />
      ))}
    </div>
  )
}
