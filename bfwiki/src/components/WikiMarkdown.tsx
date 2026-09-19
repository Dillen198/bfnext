import ReactMarkdown from 'react-markdown'
import remarkGfm from 'remark-gfm'
import { Link, useLocation } from 'react-router-dom'
import { API_ROOT } from '../api'
import { useInstance } from '../context/InstanceContext'
import { applyCfgTokens, applyListTokens } from '../lib/cfgTokens'
import StatusHexes, { HexLegend, type HexSpec } from './StatusHexes'
import MapSymbolList, { type SymbolRow } from './MapSymbol'

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


/**
 * A fenced block whose language we render ourselves rather than as code.
 *
 * ```statushexes
 * { "health": 82, "logi": 45, "supply": 12, "fuel": 90 }
 * ```
 *
 * Returns the language when `node` is such a block, so `pre` can swap the
 * whole <pre><code> for a component instead of printing JSON at the reader.
 */
function customBlock(node: unknown): { lang: string; text: string } | null {
  const el = node as
    | { children?: { tagName?: string; properties?: { className?: unknown }; children?: { value?: string }[] }[] }
    | undefined
  const code = el?.children?.find(c => c.tagName === 'code')
  if (!code) return null
  const classes = Array.isArray(code.properties?.className)
    ? (code.properties!.className as unknown[]).map(String)
    : []
  const lang = classes
    .find(c => c.startsWith('language-'))
    ?.slice('language-'.length)
  if (lang !== 'statushexes' && lang !== 'hexlegend' && lang !== 'mapsymbols') return null
  const text = (code.children ?? []).map(c => c.value ?? '').join('')
  return { lang, text }
}

export default function WikiMarkdown({ children, slug }: { children: string; slug?: string }) {
  const location = useLocation()
  // Numbers in a page belong to one DCS server instance -- see lib/cfgTokens.
  const { facts } = useInstance()
  // Lists first: a rendered row may itself contain a {{cfg:}} token.
  const body = applyCfgTokens(applyListTokens(children, facts?.facts), facts?.facts)
  // Prefer an explicit slug (the edit-page preview passes it) over the router
  // location so relative links resolve against the page being edited, not "/edit".
  const basePath = slug ? `/${slug}` : location.pathname

  return (
    <ReactMarkdown
      remarkPlugins={[remarkGfm]}
      components={{
        pre: ({ node, children }) => {
          const block = customBlock(node)
          if (!block) return <pre>{children}</pre>
          if (block.lang === 'hexlegend') return <HexLegend />
          if (block.lang === 'mapsymbols') {
            try {
              return <MapSymbolList rows={JSON.parse(block.text) as SymbolRow[]} />
            } catch {
              return <pre>{children}</pre>
            }
          }
          let spec: HexSpec
          try {
            spec = JSON.parse(block.text) as HexSpec
          } catch {
            // Bad JSON in a page should show the author their mistake, not
            // blank the section.
            return <pre>{children}</pre>
          }
          return <StatusHexes {...spec} />
        },
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
      {body}
    </ReactMarkdown>
  )
}
