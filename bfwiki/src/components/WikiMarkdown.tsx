import ReactMarkdown from 'react-markdown'
import remarkGfm from 'remark-gfm'
import { Link, useLocation } from 'react-router-dom'
import { API_ROOT } from '../api'

// Rewrites bare "/api/..." image paths (used by seed content and any
// hand-typed markdown) to include API_ROOT, so images still resolve when
// bfwiki and bfdb are hosted on different origins (e.g. Vercel frontend +
// a separately-hosted bfdb). Uploaded-via-editor images already come back
// as full API_ROOT-prefixed URLs from api.wiki.uploadImage, so this is a
// no-op for those -- it only matters for relative paths.
function resolveSrc(src?: string): string | undefined {
  if (src && src.startsWith('/api/')) return `${API_ROOT}${src}`
  return src
}

function isExternal(href: string): boolean {
  return /^([a-z]+:)?\/\//i.test(href) || href.startsWith('mailto:') || href.startsWith('tel:')
}

// Seed content (and cross-links authored in the editor) is written as inter-file
// markdown links -- e.g. "[JTAC](../f10-menu/jtac.md)" or "[Actions](./actions.md)".
// Rendered verbatim those become "<a href>" full-page navigations to a ".md" URL
// that the SPA (and bfdb) has no route for, so every internal link 404s. Resolve
// them here against the current page path, drop the ".md" extension, and hand the
// result to react-router so navigation stays client-side.
function resolveHref(href: string, basePath: string): string | null {
  if (!href || href.startsWith('#') || isExternal(href)) return null
  const hashIdx = href.indexOf('#')
  const hash = hashIdx >= 0 ? href.slice(hashIdx) : ''
  const path = hashIdx >= 0 ? href.slice(0, hashIdx) : href
  if (!path) return null
  let resolved: string
  try {
    resolved = new URL(path, `http://x${basePath}`).pathname
  } catch {
    return null
  }
  resolved = resolved.replace(/\.(md|markdown)$/i, '')
  return resolved + hash
}

export default function WikiMarkdown({ children, slug }: { children: string; slug?: string }) {
  const location = useLocation()
  // Prefer an explicit slug (the edit-page preview passes it) over the router
  // location so relative links resolve against the page being edited, not "/edit".
  const basePath = slug ? `/${slug}` : location.pathname

  return (
    <ReactMarkdown
      remarkPlugins={[remarkGfm]}
      components={{
        img: ({ src, alt, title }) => <img src={resolveSrc(src)} alt={alt} title={title} />,
        a: ({ href, children, title }) => {
          if (href && isExternal(href)) {
            return (
              <a href={href} target="_blank" rel="noopener noreferrer" title={title}>
                {children}
              </a>
            )
          }
          const to = href ? resolveHref(href, basePath) : null
          if (to) {
            return (
              <Link to={to} title={title}>
                {children}
              </Link>
            )
          }
          return (
            <a href={href} title={title}>
              {children}
            </a>
          )
        },
      }}
    >
      {children}
    </ReactMarkdown>
  )
}
