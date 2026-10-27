//! Discord OAuth and local admin login, sessions.

use crate::*;

#[derive(Debug, Clone)]
pub(crate) struct AuthConfig {
    pub(crate) client_id: String,
    pub(crate) client_secret: String,
    pub(crate) redirect_uri: String,
    pub(crate) guild_id: String,
    pub(crate) admin_role_id: String,
    /// Discord roles granting a single admin role (`admin::Role`).
    pub(crate) roles: admin::DiscordRoles,
}

#[derive(Debug, Clone)]
pub(crate) struct LocalAdminConfig {
    pub(crate) username: String,
    pub(crate) password: String,
}

#[derive(Deserialize)]
pub(crate) struct LoginQuery {
    /// Where to send the browser after a successful login -- the initiating
    /// frontend's own origin (e.g. "https://dashboard.vectorstrike.org/" or
    /// "https://wiki.vectorstrike.org/"). Only trusted if it exactly matches
    /// one of the configured --cors-origin values; anything else is dropped
    /// silently rather than erroring, since an open redirect here (this
    /// endpoint has no auth gate) would be a phishing vector.
    pub(crate) return_to: Option<std::string::String>,
}

/// GET /api/auth/login  — redirect to Discord OAuth
pub(crate) async fn api_auth_login(
    q: LoginQuery,
    cfg: AuthConfig,
    db: StatsDb,
    allowed_origins: Arc<Vec<std::string::String>>,
    ip: std::net::IpAddr,
) -> std::result::Result<impl warp::Reply, Error> {
    // Each call writes a login-state row (swept after it expires, see
    // maint::spawn_sweeps). Nobody needs thirty logins in ten minutes, and the
    // tree as a whole is capped so a botnet cannot fill it either.
    static LOGINS: std::sync::LazyLock<websec::RateLimiter<std::net::IpAddr>> =
        std::sync::LazyLock::new(|| {
            websec::RateLimiter::new(
                30,
                std::time::Duration::from_secs(600),
                std::time::Duration::from_secs(600),
            )
        });
    const MAX_PENDING_LOGINS: usize = 20_000;
    if !LOGINS.hit(&ip) {
        return Err(websec::too_many(
            "too many login attempts -- try again in a few minutes",
        )
        .into());
    }
    if db.oauth_state_count() >= MAX_PENDING_LOGINS {
        log::warn!("refusing /api/auth/login: {MAX_PENDING_LOGINS} login attempts already pending");
        return Err(
            websec::unavailable("login is busy -- try again in a few minutes").into()
        );
    }
    let state = Uuid::new_v4();
    let return_to =
        q.return_to.filter(|rt| allowed_origins.iter().any(|o| rt == &format!("{o}/")));
    task::block_in_place(|| db.store_oauth_state(state, return_to))?;
    let url = format!(
        "https://discord.com/oauth2/authorize?client_id={}&redirect_uri={}&response_type=code&scope=identify+guilds.members.read&state={}",
        cfg.client_id,
        urlencoding::encode(&cfg.redirect_uri),
        state,
    );
    Ok(warp::redirect::temporary(url.parse::<warp::http::Uri>().unwrap()))
}

#[derive(Deserialize)]
pub(crate) struct CallbackQuery {
    code: String,
    state: String,
}

#[derive(Deserialize)]
pub(crate) struct DiscordUser {
    id: String,
    username: String,
    avatar: Option<String>,
}

#[derive(Deserialize)]
pub(crate) struct GuildMember {
    roles: Vec<String>,
}

#[derive(Deserialize)]
pub(crate) struct TokenResponse {
    access_token: String,
}

/// Session cookie attributes. SameSite=None (needed for cross-origin fetch/XHR
/// with credentials) requires Secure, which requires bfdb to be served over
/// TLS — so we only opt into it when --cors-origin was actually configured.
pub(crate) fn session_cookie_attrs(cross_origin: bool) -> &'static str {
    if cross_origin {
        "SameSite=None; Secure"
    } else {
        "SameSite=Lax"
    }
}

/// GET /api/auth/callback?code=&state=  — exchange code, create session
pub(crate) async fn api_auth_callback(
    q: CallbackQuery,
    cfg: AuthConfig,
    db: StatsDb,
    cross_origin: bool,
) -> std::result::Result<impl warp::Reply, Error> {
    // Validate state, recovering which frontend origin to send the browser
    // back to (falls back to bfdb's own "/" for embedded-mode deployments
    // that never passed a return_to at /api/auth/login).
    let state_uuid =
        q.state.parse::<Uuid>().map_err(|_| websec::bad_request("bad login state"))?;
    let Some(return_to) = task::block_in_place(|| db.take_oauth_state(state_uuid))?
    else {
        return Err(websec::bad_request(
            "invalid or expired login attempt -- start the login again",
        )
        .into());
    };
    let redirect_location = return_to.unwrap_or_else(|| "/".to_string());

    let http = http_client();

    // Exchange code for access token
    let token_res: TokenResponse = http
        .post("https://discord.com/api/oauth2/token")
        .form(&[
            ("client_id", cfg.client_id.as_str()),
            ("client_secret", cfg.client_secret.as_str()),
            ("grant_type", "authorization_code"),
            ("code", q.code.as_str()),
            ("redirect_uri", cfg.redirect_uri.as_str()),
        ])
        .send()
        .await
        .map_err(|e| anyhow::anyhow!("token exchange failed: {e}"))?
        .json()
        .await
        .map_err(|e| anyhow::anyhow!("token parse failed: {e}"))?;

    // Fetch Discord user
    let user: DiscordUser = http
        .get("https://discord.com/api/users/@me")
        .bearer_auth(&token_res.access_token)
        .send()
        .await
        .map_err(|e| anyhow::anyhow!("user fetch failed: {e}"))?
        .json()
        .await
        .map_err(|e| anyhow::anyhow!("user parse failed: {e}"))?;

    // Guild roles: the admin role makes a full admin, the optional
    // --discord-*-role-id ones a single admin role each.
    let member_roles: Vec<String> = match http
        .get(format!("https://discord.com/api/users/@me/guilds/{}/member", cfg.guild_id))
        .bearer_auth(&token_res.access_token)
        .send()
        .await
    {
        Ok(r) => r.json::<GuildMember>().await.ok().map(|m| m.roles).unwrap_or_default(),
        Err(_) => Vec::new(),
    };
    let is_admin = member_roles.contains(&cfg.admin_role_id);
    let roles = cfg.roles.roles_for(&cfg.admin_role_id, &member_roles);

    let session_id = Uuid::new_v4();
    // Admin rights are read from the Discord role once, here -- so an admin
    // session is kept short (see ADMIN_SESSION_SECS) to bound how long a
    // removed role keeps working.
    let lifetime = if roles != 0 { ADMIN_SESSION_SECS } else { PLAYER_SESSION_SECS };
    let session = SessionData {
        discord_id: user.id.clone(),
        username: user.username.clone(),
        avatar: user.avatar.clone(),
        is_admin,
        expires: chrono::Utc::now() + chrono::Duration::seconds(lifetime),
        roles,
    };
    task::block_in_place(|| db.create_session(session_id, session))?;

    let cookie = format!(
        "session={}; Path=/; HttpOnly; {}; Max-Age={lifetime}",
        session_id,
        session_cookie_attrs(cross_origin)
    );
    Ok(warp::http::Response::builder()
        .status(302)
        .header("location", redirect_location)
        .header("set-cookie", cookie)
        .body("")
        .unwrap())
}

/// GET /api/auth/me  — return current user (reads session cookie)
pub(crate) async fn api_auth_me(
    session_id: Option<Uuid>,
    db: StatsDb,
    bot_cfg: Arc<Option<BotLinkConfig>>,
    inst: Inst,
) -> std::result::Result<impl warp::Reply, Error> {
    // Return 200 with null user when not logged in — avoids browser console errors
    // since this endpoint is polled on every page load to check auth state
    let Some(id) = session_id else {
        return Ok(json_response(r#"{"user":null}"#.to_string()));
    };
    let session = task::block_in_place(|| db.get_session(id))?;
    let Some(s) = session else {
        return Ok(json_response(r#"{"user":null}"#.to_string()));
    };
    let ucid = resolve_ucid_via_bot(&bot_cfg, &s.discord_id).await;
    // Coalition in the active round -- gates access to the recon intel page.
    let side = match &ucid {
        Some(u) => task::block_in_place(|| db.pilot_current_side(&inst.id, u))?
            .map(|s| format!("{s:?}")),
        None => None,
    };
    Ok(json_response(serde_json::to_string(&serde_json::json!({
        "user": {
            "discord_id": s.discord_id,
            "username":   s.username,
            "avatar":     s.avatar,
            "is_admin":   s.is_admin,
            // The admin roles this session holds (see admin.rs); every one
            // for a full admin.
            "roles":      admin::role_names(if s.is_admin { admin::Role::ALL } else { s.roles }),
            "ucid":       ucid.map(|u| u.to_string()),
            "side":       side,
        }
    })).map_err(anyhow::Error::from)?))
}

/// POST /api/auth/logout  — end the session and clear its cookie
pub(crate) async fn api_auth_logout(
    session_id: Option<Uuid>,
    db: StatsDb,
    cross_origin: bool,
) -> std::result::Result<impl warp::Reply, Error> {
    if let Some(id) = session_id {
        task::block_in_place(|| db.delete_session(id))?;
    }
    let cookie = format!(
        "session=; Path=/; HttpOnly; {}; Max-Age=0",
        session_cookie_attrs(cross_origin)
    );
    Ok(warp::http::Response::builder()
        .status(200)
        .header("set-cookie", cookie)
        .body("")
        .unwrap())
}

#[derive(Deserialize)]
pub(crate) struct LocalLoginBody {
    username: String,
    password: String,
}

/// Failed local-login attempts per client IP: 5 in 15 minutes locks that
/// address out for 15 minutes.
pub(crate) static LOCAL_LOGIN_FAILS: std::sync::LazyLock<
    websec::RateLimiter<std::net::IpAddr>,
> = std::sync::LazyLock::new(|| {
    websec::RateLimiter::new(
        5,
        std::time::Duration::from_secs(900),
        std::time::Duration::from_secs(900),
    )
});

/// Admin sessions (Discord or local) last this long. A Discord admin's role
/// is only checked at login, so this is also how long a removed admin keeps
/// access at most -- 12 hours rather than the 7 days a player session gets.
/// `POST /api/admin/sessions/revoke` ends one sooner.
pub(crate) const ADMIN_SESSION_SECS: i64 = 12 * 3600;
pub(crate) const PLAYER_SESSION_SECS: i64 = 7 * 24 * 3600;

/// POST /api/auth/local-login  — username/password admin login (no Discord required)
pub(crate) async fn api_auth_local_login(
    body: LocalLoginBody,
    local_cfg: Option<LocalAdminConfig>,
    db: StatsDb,
    cross_origin: bool,
    ip: std::net::IpAddr,
    allowed_from: Arc<Vec<websec::IpNet>>,
) -> std::result::Result<impl warp::Reply, Error> {
    let cfg = local_cfg
        .ok_or_else(|| websec::not_found("Local login is not enabled on this server"))?;
    // A machine credential for DCSServerBot on this box, not a login form for
    // the internet: by default only loopback may even try (--local-login-from).
    let allowed = if allowed_from.is_empty() {
        ip.is_loopback()
            || matches!(ip, std::net::IpAddr::V6(v6) if v6.to_ipv4_mapped().map_or(false, |v4| v4.is_loopback()))
    } else {
        allowed_from.iter().any(|n| n.contains(ip))
    };
    if !allowed {
        log::warn!("local-login refused from {ip} (not in --local-login-from)");
        return Err(
            websec::forbidden("local login is not available from your address").into()
        );
    }
    if let Some(wait) = LOCAL_LOGIN_FAILS.blocked(&ip) {
        return Err(websec::too_many(format!(
            "too many failed logins -- try again in {} minute(s)",
            wait.as_secs() / 60 + 1
        ))
        .into());
    }
    // Both compared in full and in constant time, so neither the timing nor
    // which field was wrong gives anything away.
    let user_ok = websec::ct_eq(&body.username, &cfg.username);
    let pass_ok = websec::ct_eq(&body.password, &cfg.password);
    if !(user_ok & pass_ok) {
        LOCAL_LOGIN_FAILS.hit(&ip);
        log::warn!("local-login: bad credentials from {ip}");
        return Err(websec::unauthorized("Invalid username or password").into());
    }
    LOCAL_LOGIN_FAILS.clear(&ip);
    let session_id = Uuid::new_v4();
    let session = SessionData {
        discord_id: format!("local:{}", cfg.username),
        username: cfg.username.clone(),
        avatar: None,
        is_admin: true,
        expires: chrono::Utc::now() + chrono::Duration::seconds(ADMIN_SESSION_SECS),
        roles: admin::Role::ALL,
    };
    task::block_in_place(|| db.create_session(session_id, session))?;
    let cookie = format!(
        "session={}; Path=/; HttpOnly; {}; Max-Age={ADMIN_SESSION_SECS}",
        session_id,
        session_cookie_attrs(cross_origin)
    );
    Ok(warp::http::Response::builder()
        .status(200)
        .header("set-cookie", cookie)
        .header("content-type", "application/json")
        .body(r#"{"ok":true}"#)
        .unwrap())
}

/// GET /api/admin/sessions  — list active sessions
pub(crate) async fn api_admin_sessions(
    session_id: Option<Uuid>,
    db: StatsDb,
    q: Q,
) -> std::result::Result<impl warp::Reply, Error> {
    require_admin(session_id, db.clone()).await?;
    let data = task::block_in_place(|| -> Result<(String, usize)> {
        let sessions = db.list_sessions()?;
        let entries: Vec<_> = sessions
            .iter()
            .map(|s| {
                serde_json::json!({
                    "discord_id": s.discord_id,
                    "username":   s.username,
                    "avatar":     s.avatar,
                    "is_admin":   s.is_admin,
                    "expires":    s.expires.to_rfc3339(),
                })
            })
            .collect();
        page_json(entries, &q, ADMIN_LIST_CAP)
    })?;
    Ok(paged_response(data))
}

#[derive(Deserialize)]
pub(crate) struct RevokeBody {
    /// End only this user's sessions (a Discord id, or `local:<username>` for
    /// the password login). Omitted: end every session, including the
    /// caller's own.
    #[serde(default)]
    pub(crate) discord_id: Option<std::string::String>,
}

/// POST /api/admin/sessions/revoke — log people out now rather than waiting
/// for their sessions to expire (a compromised account, a removed admin).
pub(crate) async fn api_admin_revoke_sessions(
    session_id: Option<Uuid>,
    body: RevokeBody,
    db: StatsDb,
) -> std::result::Result<impl warp::Reply, Error> {
    let who = require_admin(session_id, db.clone()).await?;
    let target = body.discord_id.as_deref().filter(|s| !s.is_empty());
    let n = task::block_in_place(|| db.revoke_sessions(target))?;
    audit(
        &db,
        &who,
        &format!("revoke {n} session(s)"),
        Some(target.unwrap_or("all users")),
        None,
    );
    Ok(warp::reply::json(&serde_json::json!({ "ok": true, "revoked": n })))
}
