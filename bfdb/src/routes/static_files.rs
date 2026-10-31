//! The embedded dashboard and site.

use crate::*;

pub(crate) fn serve_site_asset(path: &str) -> Response {
    let path = if path.is_empty() { "index.html" } else { path };
    match SiteAssets::get(path) {
        Some(content) => {
            let mime = mime_guess::from_path(path).first_or_octet_stream();
            warp::http::Response::builder()
                .header("content-type", mime.as_ref())
                .body(content.data.into_owned())
                .unwrap()
                .into_response()
        }
        None => {
            // SPA fallback
            let content = SiteAssets::get("index.html").unwrap();
            warp::http::Response::builder()
                .header("content-type", "text/html")
                .body(content.data.into_owned())
                .unwrap()
                .into_response()
        }
    }
}

pub(crate) fn serve_asset(path: &str) -> Response {
    let path = if path.is_empty() { "index.html" } else { path };
    match Assets::get(path) {
        Some(content) => {
            let mime = mime_guess::from_path(path).first_or_octet_stream();
            warp::http::Response::builder()
                .header("content-type", mime.as_ref())
                .body(content.data.into_owned())
                .unwrap()
                .into_response()
        }
        None => {
            // SPA fallback: unknown paths get index.html for client-side routing
            let content = Assets::get("index.html").unwrap();
            warp::http::Response::builder()
                .header("content-type", "text/html")
                .body(content.data.into_owned())
                .unwrap()
                .into_response()
        }
    }
}
