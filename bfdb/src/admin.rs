//! Who may do what in the admin API.

/// Admin role bits, as kept in `SessionData::roles`.
pub(crate) struct Role;

impl Role {
    /// Server operations: the OPS page, DCSServerBot actions, the engine
    /// config editor, perf and logs.
    pub(crate) const OPS: u8 = 1 << 0;
    /// Content: the wiki and the war diary's pictures.
    pub(crate) const CONTENT: u8 = 1 << 1;
    /// Moderation: bans, side switches, commanders, recon intel clean-up.
    pub(crate) const MODERATION: u8 = 1 << 2;
    /// Every role -- what a full admin has.
    pub(crate) const ALL: u8 = Self::OPS | Self::CONTENT | Self::MODERATION;
}
