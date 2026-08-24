import ReactMarkdown from 'react-markdown'
import remarkGfm from 'remark-gfm'
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

export default function WikiMarkdown({ children }: { children: string }) {
  return (
    <ReactMarkdown
      remarkPlugins={[remarkGfm]}
      components={{
        img: ({ src, alt, ...rest }) => <img src={resolveSrc(src)} alt={alt} {...rest} />,
      }}
    >
      {children}
    </ReactMarkdown>
  )
}
