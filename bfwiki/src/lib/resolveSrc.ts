/**
 * Rewrites bare "/api/..." paths (used by seed content and any hand-typed
 * markdown) to include the API root, so images and downloads still resolve
 * when bfwiki and bfdb are hosted on different origins (e.g. Vercel frontend
 * + a separately-hosted bfdb). Uploaded-via-editor images already come back
 * as full URLs from api.wiki.uploadImage, so this is a no-op for those -- it
 * only matters for relative paths.
 *
 * Reads VITE_API_BASE itself (the same value api.ts's API_ROOT holds) so the
 * mockup blocks can use it without pulling the API client into their tests.
 */
const API_ROOT: string = import.meta.env.VITE_API_BASE ?? ''

export function resolveSrc(src?: string): string | undefined {
  if (src && src.startsWith('/api/')) return `${API_ROOT}${src}`
  return src
}
