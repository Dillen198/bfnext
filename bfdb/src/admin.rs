//! Who may do what in the admin API.
//!
//! One `is_admin` bit used to gate everything, from editing a wiki page to
//! restarting the DCS server. Roles split that up. A full admin (the Discord
//! admin role, or the local password login) has every role, so nothing
//! changed for them; the `--discord-ops-role-id`, `--discord-content-role-id`
//! and `--discord-moderation-role-id` flags hand out a single role to members
//! of another Discord role. Campaign resets, session revocation, shutdown and
//! the audit log itself stay full-admin only.

use crate::db::SessionData;

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

/// Whether `s` may act in `role`.
pub(crate) fn has_role(s: &SessionData, role: u8) -> bool {
    s.is_admin || (s.roles & role) == role
}

/// The role names, for `/api/auth/me`.
pub(crate) fn role_names(bits: u8) -> Vec<&'static str> {
    [(Role::OPS, "ops"), (Role::CONTENT, "content"), (Role::MODERATION, "moderation")]
        .into_iter()
        .filter(|(b, _)| bits & b != 0)
        .map(|(_, n)| n)
        .collect()
}

/// The Discord role ids that grant each role (`--discord-*-role-id`).
#[derive(Debug, Clone, Default)]
pub(crate) struct DiscordRoles {
    pub(crate) ops: Option<String>,
    pub(crate) content: Option<String>,
    pub(crate) moderation: Option<String>,
}

impl DiscordRoles {
    /// The roles a guild member with `member_roles` gets. The admin role
    /// grants all of them.
    pub(crate) fn roles_for(&self, admin_role: &str, member_roles: &[String]) -> u8 {
        let has = |id: &Option<String>| id.as_ref().map_or(false, |id| member_roles.iter().any(|r| r == id));
        if member_roles.iter().any(|r| r == admin_role) {
            return Role::ALL;
        }
        let mut bits = 0;
        if has(&self.ops) {
            bits |= Role::OPS;
        }
        if has(&self.content) {
            bits |= Role::CONTENT;
        }
        if has(&self.moderation) {
            bits |= Role::MODERATION;
        }
        bits
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn discord_roles() {
        let d = DiscordRoles { ops: Some("10".into()), content: Some("20".into()), moderation: None };
        assert_eq!(d.roles_for("1", &["1".into()]), Role::ALL);
        assert_eq!(d.roles_for("1", &["20".into(), "99".into()]), Role::CONTENT);
        assert_eq!(d.roles_for("1", &["10".into(), "20".into()]), Role::OPS | Role::CONTENT);
        assert_eq!(d.roles_for("1", &["99".into()]), 0);
        assert_eq!(DiscordRoles::default().roles_for("1", &["10".into()]), 0);
        assert_eq!(role_names(Role::OPS | Role::MODERATION), vec!["ops", "moderation"]);
    }
}
