//! The HTTP handlers, one module per area. `build_routes` in main.rs puts
//! them together in the order they are matched.

pub(crate) mod admin;
pub(crate) mod auth;
pub(crate) mod campaign;
pub(crate) mod cockpit;
pub(crate) mod command;
pub(crate) mod intel;
pub(crate) mod live;
pub(crate) mod ops;
pub(crate) mod pilots;
pub(crate) mod static_files;
pub(crate) mod wiki;
