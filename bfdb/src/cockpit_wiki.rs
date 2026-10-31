//! The wiki for the in-game cockpit panel (the WIKI tab of
//! `bfcockpit/Scripts/Hooks/bfcockpit.lua`).
//!
//! The panel reads the same pages as bfwiki (`/api/wiki/pages/<slug>`) and the
//! same numbers (`/api/wiki/facts`). What it can't use as they are is the
//! sidebar and the pictures: it is drawn with DCS's dxgui widgets, which show
//! a picture only from a local file in a format DCS loads, at a size the
//! script has to state up front. So these routes, all public like the rest of
//! the wiki's read side:
//!
//! * `GET /api/cockpit/wiki/index` -- every page's slug, title, section, order
//!   and `updated_at`, in the sidebar's order. The panel caches page text and
//!   refetches a page only when its `updated_at` changed.
//! * `GET /api/cockpit/wiki/search?q=` -- full-text search over titles and
//!   bodies. A few dozen pages of Markdown: a scan is cheaper than an index.
//! * `GET /api/cockpit/wiki/image/<uuid>/info?w=` -- what the picture below
//!   will be (`png` or `jpg`, width, height) without making it, so the panel
//!   knows the file name and the row height before it downloads anything.
//! * `GET /api/cockpit/wiki/image/<uuid>?w=` -- the picture as a file DCS
//!   loads: a PNG no wider than `w` (bigger PNGs are scaled down), an SVG
//!   rasterised to PNG at width `w` with resvg, or a JPEG as it is (DCS shows
//!   JPEG briefing pictures, and no JPEG decoder is in the build to resize
//!   one; dxgui scales it to the size the script gives). WebP is refused with
//!   415: DCS has no loader for it, and the panel shows the alt text instead.
//!   `X-Image-Width` / `X-Image-Height` carry the size.
//!
//! Pictures are keyed by an id that never changes content, so both are cached
//! for a year, and the panel keeps each one on disk.

use crate::*;
use resvg::{tiny_skia, usvg};

/// The width asked for when the request names none.
const DEFAULT_W: u32 = 640;
const MIN_W: u32 = 64;
const MAX_W: u32 = 1600;
/// A drawing taller than this (at the asked width) is drawn smaller.
const MAX_H: u32 = 4096;
const MAX_HITS: usize = 30;
const MAX_QUERY: usize = 200;
const MAX_TERMS: usize = 8;

/// `GET /api/cockpit/wiki/index`
pub(crate) async fn api_index(
    db: StatsDb,
) -> std::result::Result<impl warp::Reply, Error> {
    let pages = task::block_in_place(|| db.wiki_list_pages())?;
    Ok(warp::reply::json(&index_json(&pages)))
}

/// The pages in the order `wiki_list_pages` sorts them (section rank, then
/// `order`), and the sections in the order they first appear in that list --
/// the order bfwiki's sidebar shows them in.
pub(crate) fn index_json(pages: &[(std::string::String, WikiPage)]) -> serde_json::Value {
    let mut sections: Vec<&str> = Vec::new();
    let rows: Vec<_> = pages
        .iter()
        .map(|(slug, p)| {
            if !sections.contains(&p.section.as_str()) {
                sections.push(&p.section);
            }
            serde_json::json!({
                "slug": slug,
                "title": p.title,
                "section": p.section,
                "order": p.order,
                "updated_at": p.updated_at,
            })
        })
        .collect();
    serde_json::json!({ "sections": sections, "pages": rows })
}

/// `GET /api/cockpit/wiki/search?q=<words>[&limit=n]`
pub(crate) async fn api_search(
    q: Q,
    db: StatsDb,
) -> std::result::Result<impl warp::Reply, Error> {
    let query = q.get("q").map(|s| s.trim()).unwrap_or("");
    if query.len() > MAX_QUERY {
        return Err(websec::bad_request("search for fewer words").into());
    }
    let limit = q
        .get("limit")
        .and_then(|s| s.trim().parse::<usize>().ok())
        .unwrap_or(MAX_HITS)
        .clamp(1, MAX_HITS);
    let hits = if query.is_empty() {
        Vec::new()
    } else {
        let pages = task::block_in_place(|| db.wiki_list_pages())?;
        search(&pages, query, limit)
    };
    Ok(warp::reply::json(&serde_json::json!({ "query": query, "hits": hits })))
}

#[derive(Debug, Serialize)]
pub(crate) struct Hit {
    slug: std::string::String,
    title: std::string::String,
    section: std::string::String,
    snippet: std::string::String,
    #[serde(skip)]
    score: u32,
}

/// Every page holding all the query's words, best first: a word in the title
/// counts for more than one in the text, the whole query as a phrase for
/// more again. Matching is ASCII case-insensitive, so byte positions in the
/// lowered text are positions in the original and the snippet can be cut
/// from it.
pub(crate) fn search(
    pages: &[(std::string::String, WikiPage)],
    query: &str,
    limit: usize,
) -> Vec<Hit> {
    let phrase = query.to_ascii_lowercase();
    let mut terms: Vec<&str> = Vec::new();
    for t in phrase.split_whitespace() {
        if t.len() <= 64 && !terms.contains(&t) && terms.len() < MAX_TERMS {
            terms.push(t);
        }
    }
    if terms.is_empty() {
        return Vec::new();
    }
    let phrase = terms.join(" ");
    let mut hits = Vec::new();
    for (slug, p) in pages {
        let title = p.title.to_ascii_lowercase();
        let text = plain_text(&p.content);
        let lower = text.to_ascii_lowercase();
        let mut score = 0u32;
        let mut all = true;
        for t in &terms {
            let in_title = title.contains(t);
            let n = lower.matches(t).take(5).count() as u32;
            if !in_title && n == 0 {
                all = false;
                break;
            }
            score += if in_title { 10 } else { 0 } + n;
        }
        if !all {
            continue;
        }
        if terms.len() > 1 {
            if title.contains(&phrase) {
                score += 20;
            }
            if lower.contains(&phrase) {
                score += 5;
            }
        }
        let at = lower.find(&phrase).or_else(|| lower.find(terms[0]));
        hits.push(Hit {
            slug: slug.clone(),
            title: p.title.clone(),
            section: p.section.clone(),
            snippet: snippet(&text, at),
            score,
        });
    }
    hits.sort_by(|a, b| b.score.cmp(&a.score).then(a.title.cmp(&b.title)));
    hits.truncate(limit);
    hits
}

/// About 140 characters of `text` around byte `at`, on word boundaries, with
/// "..." where it was cut.
fn snippet(text: &str, at: Option<usize>) -> std::string::String {
    const BEFORE: usize = 50;
    const LEN: usize = 140;
    let at = at.unwrap_or(0).min(text.len());
    let mut start = at.saturating_sub(BEFORE);
    while !text.is_char_boundary(start) {
        start -= 1;
    }
    if start > 0 {
        // back to the start of a word
        start = text[..start].rfind(' ').map(|i| i + 1).unwrap_or(start);
    }
    let mut end = (start + LEN).min(text.len());
    while !text.is_char_boundary(end) {
        end -= 1;
    }
    if end < text.len() {
        end = text[..end].rfind(' ').filter(|&i| i > at).unwrap_or(end);
    }
    let mut out = std::string::String::new();
    if start > 0 {
        out.push_str("...");
    }
    out.push_str(text[start..end].trim());
    if end < text.len() {
        out.push_str("...");
    }
    out
}

/// Markdown reduced to the words a reader sees: no image references, link
/// text without its address, `{{cfg:path|fallback}}` as its fallback, no
/// emphasis or heading marks, whitespace collapsed.
pub(crate) fn plain_text(md: &str) -> std::string::String {
    use std::sync::OnceLock;
    static RES: OnceLock<[regex::Regex; 4]> = OnceLock::new();
    let [image, link, cfg, marks] = RES.get_or_init(|| {
        [
            regex::Regex::new(r"!\[[^\]]*\]\([^)]*\)").unwrap(),
            regex::Regex::new(r"\[([^\]]*)\]\([^)]*\)").unwrap(),
            regex::Regex::new(r"\{\{\s*(?:cfg|list):([^|}]*?)\s*(?:\|([^}]*?))?\s*\}\}")
                .unwrap(),
            regex::Regex::new(r"[*_`#>|~]+").unwrap(),
        ]
    });
    let s = image.replace_all(md, " ");
    let s = link.replace_all(&s, "$1");
    let s = cfg.replace_all(&s, |c: &regex::Captures| {
        c.get(2).or_else(|| c.get(1)).map(|m| m.as_str().to_string()).unwrap_or_default()
    });
    let s = marks.replace_all(&s, " ");
    s.split_whitespace().collect::<Vec<_>>().join(" ")
}

// ── Pictures ────────────────────────────────────────────────────────────

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum Source {
    Png,
    Jpeg,
    Svg,
}

/// What a stored wiki image is, judged by its bytes (and, for SVG, which has
/// no magic number, the start of its text).
pub(crate) fn source_of(data: &[u8]) -> std::result::Result<Source, &'static str> {
    match websec::sniff_image(data) {
        Some("image/png") => return Ok(Source::Png),
        Some("image/jpeg") => return Ok(Source::Jpeg),
        Some("image/webp") => {
            return Err("this picture is WebP, which DCS can't show");
        }
        _ => (),
    }
    let head = &data[..data.len().min(2048)];
    let head = std::string::String::from_utf8_lossy(head);
    let t = head.trim_start_matches('\u{feff}').trim_start();
    if (t.starts_with("<svg") || t.starts_with("<?xml") || t.starts_with("<!--"))
        && head.contains("<svg")
    {
        return Ok(Source::Svg);
    }
    Err("this picture is in a format DCS can't show")
}

/// What `/image/<id>?w=` answers with: the file's format and pixel size.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) struct Plan {
    pub(crate) source: Source,
    pub(crate) jpeg: bool,
    pub(crate) width: u32,
    pub(crate) height: u32,
}

impl Plan {
    fn ext(&self) -> &'static str {
        if self.jpeg {
            "jpg"
        } else {
            "png"
        }
    }
    fn content_type(&self) -> &'static str {
        if self.jpeg {
            "image/jpeg"
        } else {
            "image/png"
        }
    }
}

pub(crate) fn want_width(q: &Q) -> u32 {
    q.get("w")
        .and_then(|s| s.trim().parse::<u32>().ok())
        .unwrap_or(DEFAULT_W)
        .clamp(MIN_W, MAX_W)
}

fn png_size(data: &[u8]) -> Option<(u32, u32)> {
    // signature, IHDR length and type, then width and height
    if data.len() < 24 || &data[12..16] != b"IHDR" {
        return None;
    }
    let w = u32::from_be_bytes(data[16..20].try_into().ok()?);
    let h = u32::from_be_bytes(data[20..24].try_into().ok()?);
    (w > 0 && h > 0).then_some((w, h))
}

/// A JPEG's size, from its first start-of-frame marker.
pub(crate) fn jpeg_size(data: &[u8]) -> Option<(u32, u32)> {
    let mut i = 2;
    while i + 4 <= data.len() {
        if data[i] != 0xFF {
            return None;
        }
        let marker = data[i + 1];
        if marker == 0xFF {
            // fill byte
            i += 1;
            continue;
        }
        if marker == 0xD8 || (0xD0..=0xD7).contains(&marker) || marker == 0x01 {
            i += 2;
            continue;
        }
        let len = u16::from_be_bytes([data[i + 2], data[i + 3]]) as usize;
        let sof =
            (0xC0..=0xCF).contains(&marker) && !matches!(marker, 0xC4 | 0xC8 | 0xCC);
        if sof {
            let s = data.get(i + 5..i + 9)?;
            let h = u16::from_be_bytes([s[0], s[1]]) as u32;
            let w = u16::from_be_bytes([s[2], s[3]]) as u32;
            return (w > 0 && h > 0).then_some((w, h));
        }
        if len < 2 {
            return None;
        }
        i += 2 + len;
    }
    None
}

/// A width, and the height that keeps `w0 x h0`'s shape, no taller than
/// `MAX_H` (a very tall drawing is made narrower instead).
fn fit(w0: u32, h0: u32, w: u32) -> (u32, u32) {
    let mut w = w.max(1);
    let mut h = ((h0 as f64) * (w as f64) / (w0.max(1) as f64)).round().max(1.) as u32;
    if h > MAX_H {
        w = ((w as f64) * (MAX_H as f64) / (h as f64)).round().max(1.) as u32;
        h = MAX_H;
    }
    (w, h)
}

fn svg_tree(data: &[u8]) -> Result<usvg::Tree> {
    let mut opt = usvg::Options::default();
    opt.fontdb = crate::range::cards::fontdb();
    opt.font_family = "Arial".into();
    // Nothing outside the document: an <image href="C:\..."> would otherwise
    // be read from the server's disk. Inline (data:) images stay allowed.
    opt.image_href_resolver.resolve_string = Box::new(|_, _| None);
    usvg::Tree::from_data(data, &opt)
        .map_err(|e| anyhow::anyhow!("the drawing is not valid SVG: {e}"))
}

/// What the picture for width `want` will be, without making it.
pub(crate) fn plan(data: &[u8], want: u32) -> Result<Plan> {
    let source = source_of(data).map_err(websec::unsupported_media)?;
    let bad = || anyhow::anyhow!("the stored picture is damaged");
    Ok(match source {
        Source::Png => {
            let (w0, h0) = png_size(data).ok_or_else(bad)?;
            let (width, height) = if w0 > want { fit(w0, h0, want) } else { (w0, h0) };
            Plan { source, jpeg: false, width, height }
        }
        Source::Jpeg => {
            let (width, height) = jpeg_size(data).ok_or_else(bad)?;
            Plan { source, jpeg: true, width, height }
        }
        Source::Svg => {
            let size = svg_tree(data)?.size();
            let (width, height) =
                fit(size.width().ceil() as u32, size.height().ceil() as u32, want);
            Plan { source, jpeg: false, width, height }
        }
    })
}

/// The picture for width `want`, and what it is.
pub(crate) fn render(data: &[u8], want: u32) -> Result<(Vec<u8>, Plan)> {
    let p = plan(data, want)?;
    let bytes = match p.source {
        Source::Jpeg => data.to_vec(),
        Source::Png => {
            let (w0, _) = png_size(data).unwrap_or((p.width, p.height));
            if w0 == p.width {
                data.to_vec()
            } else {
                let src = tiny_skia::Pixmap::decode_png(data)
                    .map_err(|e| anyhow::anyhow!("reading the PNG: {e}"))?;
                let mut out = tiny_skia::Pixmap::new(p.width, p.height)
                    .ok_or_else(|| anyhow::anyhow!("empty picture"))?;
                let sx = p.width as f32 / src.width() as f32;
                let sy = p.height as f32 / src.height() as f32;
                let paint = tiny_skia::PixmapPaint {
                    quality: tiny_skia::FilterQuality::Bicubic,
                    ..Default::default()
                };
                out.draw_pixmap(
                    0,
                    0,
                    src.as_ref(),
                    &paint,
                    tiny_skia::Transform::from_scale(sx, sy),
                    None,
                );
                out.encode_png().map_err(|e| anyhow::anyhow!("writing the PNG: {e}"))?
            }
        }
        Source::Svg => {
            let tree = svg_tree(data)?;
            let size = tree.size();
            let mut pm = tiny_skia::Pixmap::new(p.width, p.height)
                .ok_or_else(|| anyhow::anyhow!("the drawing has no size"))?;
            let t = tiny_skia::Transform::from_scale(
                p.width as f32 / size.width(),
                p.height as f32 / size.height(),
            );
            resvg::render(&tree, t, &mut pm.as_mut());
            pm.encode_png().map_err(|e| anyhow::anyhow!("writing the PNG: {e}"))?
        }
    };
    Ok((bytes, p))
}

fn image_data(db: &StatsDb, id: &Uuid) -> Result<Vec<u8>> {
    Ok(db.wiki_get_image(id)?.ok_or_else(|| websec::not_found("image not found"))?.data)
}

/// The last few pictures made. An id's picture never changes and every
/// player asks for the same few widths, so a PNG scaled or an SVG drawn once
/// serves everyone after.
const CACHE_ENTRIES: usize = 48;
type Made = Arc<(Vec<u8>, Plan)>;

fn render_cached(db: &StatsDb, id: Uuid, want: u32) -> Result<Made> {
    use std::sync::OnceLock;
    static CACHE: OnceLock<Mutex<VecDeque<((Uuid, u32), Made)>>> = OnceLock::new();
    let cache = CACHE.get_or_init(|| Mutex::new(VecDeque::new()));
    let lock = || cache.lock().unwrap_or_else(|e| e.into_inner());
    if let Some((_, hit)) = lock().iter().find(|(k, _)| *k == (id, want)) {
        return Ok(hit.clone());
    }
    let made: Made = Arc::new(render(&image_data(db, &id)?, want)?);
    let mut c = lock();
    c.retain(|(k, _)| *k != (id, want));
    c.push_back(((id, want), made.clone()));
    while c.len() > CACHE_ENTRIES {
        c.pop_front();
    }
    Ok(made)
}

/// `GET /api/cockpit/wiki/image/<uuid>/info?w=<px>`
pub(crate) async fn api_image_info(
    id: Uuid,
    q: Q,
    db: StatsDb,
) -> std::result::Result<impl warp::Reply, Error> {
    let want = want_width(&q);
    let p = task::block_in_place(|| plan(&image_data(&db, &id)?, want))?;
    Ok(warp::reply::json(&serde_json::json!({
        "id": id.to_string(),
        "format": p.ext(),
        "width": p.width,
        "height": p.height,
        "svg": p.source == Source::Svg,
        "url": format!("/api/cockpit/wiki/image/{id}?w={want}"),
    })))
}

/// `GET /api/cockpit/wiki/image/<uuid>?w=<px>`
pub(crate) async fn api_image(
    id: Uuid,
    q: Q,
    db: StatsDb,
) -> std::result::Result<warp::reply::Response, Error> {
    use warp::http::HeaderValue;
    let want = want_width(&q);
    let made = task::block_in_place(|| render_cached(&db, id, want))?;
    let (bytes, p) = (made.0.clone(), made.1);
    let mut r = warp::reply::Response::new(bytes.into());
    let h = r.headers_mut();
    h.insert("content-type", HeaderValue::from_static(p.content_type()));
    h.insert(
        "cache-control",
        HeaderValue::from_static("public, max-age=31536000, immutable"),
    );
    h.insert("x-content-type-options", HeaderValue::from_static("nosniff"));
    h.insert(
        "content-security-policy",
        HeaderValue::from_static("default-src 'none'; sandbox"),
    );
    h.insert("x-image-width", HeaderValue::from(p.width));
    h.insert("x-image-height", HeaderValue::from(p.height));
    Ok(r)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::route_tests::{body_json, ctype, harness};

    /// A seeded JPEG (bfdb/seed_wiki/images/server-browser.jpeg).
    const SEEDED_JPEG: &str = "cf08da53-e826-49b2-8b67-7ddded3cbb74";

    fn png(w: u32, h: u32) -> Vec<u8> {
        let mut pm = tiny_skia::Pixmap::new(w, h).unwrap();
        pm.fill(tiny_skia::Color::from_rgba8(200, 30, 30, 255));
        pm.encode_png().unwrap()
    }

    const SVG: &str = r##"<svg xmlns="http://www.w3.org/2000/svg" width="200" height="100" viewBox="0 0 200 100"><rect width="200" height="100" fill="#3a4a2e"/><circle cx="50" cy="50" r="40" fill="#8ec83f"/></svg>"##;

    fn store(h: &crate::route_tests::Harness, data: Vec<u8>, ctype: &str) -> Uuid {
        let id = Uuid::new_v4();
        h.db.wiki_save_image(
            id,
            WikiImage {
                content_type: ctype.into(),
                data,
                uploaded_at: chrono::Utc::now(),
                uploaded_by: "test".into(),
            },
        )
        .unwrap();
        id
    }

    fn header(r: &crate::route_tests::TestResp, name: &str) -> String {
        r.headers().get(name).and_then(|v| v.to_str().ok()).unwrap_or("").to_string()
    }

    #[test]
    fn jpeg_and_png_sizes() {
        let p = png(37, 11);
        assert_eq!(png_size(&p), Some((37, 11)));
        assert_eq!(source_of(&p), Ok(Source::Png));
        let jpeg = include_bytes!("../seed_wiki/images/server-browser.jpeg");
        let (w, h) = jpeg_size(jpeg).expect("seeded jpeg has a size");
        assert!(w > 100 && h > 100, "{w}x{h}");
        assert_eq!(jpeg_size(&[0xFF, 0xD8, 0xFF, 0xD9]), None);
        assert_eq!(jpeg_size(b"not a jpeg at all"), None);
    }

    #[test]
    fn sources_by_content() {
        assert_eq!(source_of(SVG.as_bytes()), Ok(Source::Svg));
        let xml = format!("<?xml version=\"1.0\"?>\n{SVG}");
        assert_eq!(source_of(xml.as_bytes()), Ok(Source::Svg));
        assert!(source_of(b"RIFF\0\0\0\0WEBPVP8 ").unwrap_err().contains("WebP"));
        assert!(source_of(b"<html><script>").is_err());
    }

    #[test]
    fn png_is_scaled_down_never_up() {
        let big = png(1000, 500);
        let (out, p) = render(&big, 400).unwrap();
        assert_eq!((p.width, p.height), (400, 200));
        assert_eq!(png_size(&out), Some((400, 200)));
        let small = png(120, 60);
        let (out, p) = render(&small, 400).unwrap();
        assert_eq!((p.width, p.height), (120, 60));
        assert_eq!(out, small, "a PNG that fits is passed on untouched");
    }

    #[test]
    fn svg_is_rasterised_at_the_asked_width() {
        let (out, p) = render(SVG.as_bytes(), 640).unwrap();
        assert_eq!(p.source, Source::Svg);
        assert_eq!((p.width, p.height), (640, 320));
        assert_eq!(png_size(&out), Some((640, 320)));
        let pm = tiny_skia::Pixmap::decode_png(&out).unwrap();
        // the middle of the circle is drawn, in its colour
        let px = pm.pixel(160, 160).unwrap().demultiply();
        assert_eq!((px.red(), px.green(), px.blue()), (0x8e, 0xc8, 0x3f));
    }

    #[test]
    fn svg_cannot_read_the_servers_disk() {
        let dir = std::env::temp_dir().join(format!("bfdb-svg-{}", Uuid::new_v4()));
        std::fs::create_dir_all(&dir).unwrap();
        let inner = dir.join("inner.svg");
        std::fs::write(
            &inner,
            r##"<svg xmlns="http://www.w3.org/2000/svg" width="10" height="10"><rect width="10" height="10" fill="#ff0000"/></svg>"##,
        )
        .unwrap();
        let svg = format!(
            r#"<svg xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink" width="10" height="10"><image width="10" height="10" xlink:href="{}"/></svg>"#,
            inner.display()
        );
        let (out, _) = render(svg.as_bytes(), 64).unwrap();
        let pm = tiny_skia::Pixmap::decode_png(&out).unwrap();
        assert_eq!(pm.pixel(32, 32).unwrap().alpha(), 0, "the file was not drawn");
        let _ = std::fs::remove_dir_all(&dir);
    }

    #[test]
    fn tall_drawings_are_kept_under_the_cap() {
        assert_eq!(
            fit(100, 10_000, 800),
            ((800.0 * 4096.0 / 80_000.0f64).round() as u32, MAX_H)
        );
        assert_eq!(fit(200, 100, 640), (640, 320));
    }

    #[test]
    fn plain_text_drops_markup() {
        let t = plain_text(
            "## Points\nAn **air kill** pays {{cfg:points.air_kill|350}} points. See [JTAC](../f10-menu/jtac.md).\n![alt](/api/wiki/images/x)",
        );
        assert_eq!(t, "Points An air kill pays 350 points. See JTAC.");
    }

    #[test]
    fn snippets_cut_on_words() {
        let text = "alpha bravo charlie delta echo foxtrot golf hotel india juliet kilo lima mike november oscar papa quebec romeo sierra tango uniform victor whiskey xray yankee zulu";
        let s = snippet(text, text.find("quebec"));
        assert!(s.starts_with("...") && s.contains("quebec"), "{s}");
        assert!(!s.contains("  "));
        assert_eq!(snippet("short", Some(0)), "short");
    }

    #[tokio::test(flavor = "multi_thread")]
    async fn index_lists_every_page_with_its_date() {
        let h = harness();
        let r = h.get("/api/cockpit/wiki/index", None).await;
        assert_eq!(r.status(), 200);
        let v = body_json(r);
        let pages = v["pages"].as_array().unwrap();
        assert!(pages.len() > 20, "seeded pages: {}", pages.len());
        for p in pages {
            assert!(p["slug"].is_string() && p["title"].is_string());
            assert!(p["updated_at"].is_string(), "{p}");
        }
        let sections: Vec<&str> =
            v["sections"].as_array().unwrap().iter().filter_map(|s| s.as_str()).collect();
        assert_eq!(sections.first(), Some(&"Introduction"));
        // every page's section is listed, once
        for p in pages {
            assert!(sections.contains(&p["section"].as_str().unwrap()));
        }
        let mut uniq = sections.clone();
        uniq.dedup();
        assert_eq!(uniq.len(), sections.len());
    }

    #[tokio::test(flavor = "multi_thread")]
    async fn search_finds_pages_by_title_and_text() {
        let h = harness();
        let v = body_json(h.get("/api/cockpit/wiki/search?q=objectives", None).await);
        let hits = v["hits"].as_array().unwrap();
        assert!(!hits.is_empty());
        assert!(hits[0]["title"].as_str().unwrap().to_lowercase().contains("objective"));
        assert!(hits.len() <= MAX_HITS);
        for hit in hits {
            assert!(hit["slug"].is_string() && hit["snippet"].is_string());
            assert!(hit.get("score").is_none());
        }
        let none =
            body_json(h.get("/api/cockpit/wiki/search?q=zzqqxxnotaword", None).await);
        assert_eq!(none["hits"].as_array().unwrap().len(), 0);
        let empty = body_json(h.get("/api/cockpit/wiki/search?q=", None).await);
        assert_eq!(empty["hits"].as_array().unwrap().len(), 0);
        let long = format!("/api/cockpit/wiki/search?q={}", "a".repeat(MAX_QUERY + 1));
        assert_eq!(h.get(&long, None).await.status(), 400);
    }

    #[tokio::test(flavor = "multi_thread")]
    async fn seeded_jpeg_is_passed_on_with_its_size() {
        crate::db::seed_images_in_this_test();
        let h = harness();
        let info = body_json(
            h.get(&format!("/api/cockpit/wiki/image/{SEEDED_JPEG}/info?w=480"), None)
                .await,
        );
        assert_eq!(info["format"], "jpg");
        let (w, hh) = (info["width"].as_u64().unwrap(), info["height"].as_u64().unwrap());
        assert_eq!(info["url"], format!("/api/cockpit/wiki/image/{SEEDED_JPEG}?w=480"));
        let r =
            h.get(&format!("/api/cockpit/wiki/image/{SEEDED_JPEG}?w=480"), None).await;
        assert_eq!(r.status(), 200);
        assert_eq!(ctype(&r), "image/jpeg");
        assert_eq!(header(&r, "x-image-width"), w.to_string());
        assert_eq!(header(&r, "x-image-height"), hh.to_string());
        assert!(r.body().starts_with(&[0xFF, 0xD8, 0xFF]));
    }

    #[tokio::test(flavor = "multi_thread")]
    async fn png_and_svg_come_back_as_png_at_the_width() {
        let h = harness();
        let pid = store(&h, png(1200, 600), "image/png");
        let r = h.get(&format!("/api/cockpit/wiki/image/{pid}?w=600"), None).await;
        assert_eq!(r.status(), 200);
        assert_eq!(ctype(&r), "image/png");
        assert_eq!(png_size(r.body()), Some((600, 300)));
        assert_eq!(header(&r, "x-image-height"), "300");

        let sid = store(&h, SVG.as_bytes().to_vec(), "image/svg+xml");
        let info = body_json(
            h.get(&format!("/api/cockpit/wiki/image/{sid}/info?w=320"), None).await,
        );
        assert_eq!(info["format"], "png");
        assert_eq!(info["svg"], true);
        assert_eq!(
            (info["width"].as_u64(), info["height"].as_u64()),
            (Some(320), Some(160))
        );
        let r = h.get(&format!("/api/cockpit/wiki/image/{sid}?w=320"), None).await;
        assert_eq!(ctype(&r), "image/png");
        assert_eq!(png_size(r.body()), Some((320, 160)));

        // widths are clamped, not refused
        let r = h.get(&format!("/api/cockpit/wiki/image/{sid}/info?w=99999"), None).await;
        assert_eq!(body_json(r)["width"].as_u64(), Some(MAX_W as u64));
    }

    #[tokio::test(flavor = "multi_thread")]
    async fn rendered_pictures_are_reused() {
        let h = harness();
        let sid = store(&h, SVG.as_bytes().to_vec(), "image/svg+xml");
        let a = render_cached(&h.db, sid, 320).unwrap();
        let b = render_cached(&h.db, sid, 320).unwrap();
        assert!(Arc::ptr_eq(&a, &b), "drawn once");
        let c = render_cached(&h.db, sid, 480).unwrap();
        assert_eq!((c.1.width, c.1.height), (480, 240));
        assert!(render_cached(&h.db, Uuid::new_v4(), 320).is_err());
    }

    #[tokio::test(flavor = "multi_thread")]
    async fn unknown_and_unshowable_pictures() {
        let h = harness();
        let missing = Uuid::new_v4();
        let r = h.get(&format!("/api/cockpit/wiki/image/{missing}/info"), None).await;
        assert_eq!(r.status(), 404);
        let wid = store(&h, b"RIFF\0\0\0\0WEBPVP8 xxxx".to_vec(), "image/webp");
        let r = h.get(&format!("/api/cockpit/wiki/image/{wid}/info"), None).await;
        assert_eq!(r.status(), 415);
        assert!(body_json(r)["error"].as_str().unwrap_or("").contains("WebP"));
        let r = h.get(&format!("/api/cockpit/wiki/image/{wid}"), None).await;
        assert_eq!(r.status(), 415);
    }
}
