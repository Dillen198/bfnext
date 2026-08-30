# ==========================================================
# FOWL ENGINE SERVICE STARTER
# ==========================================================

# DCS instance's Saved Games folder -- the one DCS/bflib actually run under
# (bflib writes its Logs/state here at runtime). Everything below is a
# subfolder of this, not the old, unused "server_2" folder.
$dcsWriteDir = "C:\Users\ATPAdmin\Saved Games\DCS.vectorstrike_1"

# Path to the netidx resolver config (tracked in the repo root). Only used
# when $netidxBase below is non-empty -- see user-guide/src/server-setup for
# the one-time setup this depends on (installing netidx-tools, and creating
# %APPDATA%\netidx\client.json for both this account and whichever account
# runs DCS.exe/bflib.dll).
$netidxResolverConfig = Join-Path $PSScriptRoot "netidx-resolver.json"

# Path to bfdb.exe (copy it here from target\release\bfdb.exe)
$bfdbExe = Join-Path $dcsWriteDir "bfdb.exe"

# Path where the Sled database will be stored (dedicated folder)
$dbPath = Join-Path $dcsWriteDir "bfdb"

# Path to your campaign branding config JSON
$configPath = Join-Path $dcsWriteDir "campaign.json"

# Path to the campaign ENGINE config JSON that bflib actually loads at
# mission start (distinct from $configPath above, which is just dashboard
# branding). Enables the admin CFG editor at /api/admin/cfg -- without this
# set, GET/POST to that endpoint fails with "engine config not configured".
$engineConfigPath = Join-Path $dcsWriteDir "ODFv2_CFG"

# Path to the stats JSONL file written by bflib
# bflib writes this file as missions run — e.g. DCS\Logs\stats.jsonl
$statsJsonl = Join-Path $dcsWriteDir "Logs\stats.jsonl"

# Path to the netidx-archive stats directory bflib writes alongside
# stats.jsonl (Logs\stats). Enables /api/admin/perf-history (Engine
# Performance History) -- without this set, that endpoint has no data.
$statsDir = Join-Path $dcsWriteDir "Logs\stats"

# Path where bfdb.exe will natively write its plain-text logs
$logFile = Join-Path $dcsWriteDir "Logs\bfdb.log"

# Dashboard address (stats + API + map)
$listenAddress = "0.0.0.0:8880"

# Public website address (separate port)
$siteAddress = "0.0.0.0:8766"

# Admin panel local login (username + password — no Discord required)
# Leave blank to disable local login (Discord-only)
$adminUsername = "admin"

# Discord OAuth2 login. Client ID/secret live in bfsystem.local.ps1 (secrets,
# gitignored) as $discordClientId / $discordClientSecret -- leave those blank
# there to disable Discord login entirely.
#
# Redirect URI must match EXACTLY (byte-for-byte) the redirect URL registered
# under OAuth2 -> Redirects in the Discord Developer Portal for this app.
# Since bfdb is reverse-proxied behind Caddy for TLS (see deploy/README.md),
# this points at the public HTTPS hostname, not the local listen address.
$discordRedirectUri  = "https://api.vectorstrike.org/api/auth/callback"

# Right-click your Discord server icon -> Copy Server ID (needs Developer
# Mode enabled in Discord: User Settings -> Advanced).
$discordGuildId      = ""

# Right-click the role that should grant dashboard admin access -> Copy Role ID.
$discordAdminRoleId  = ""

# SRS radio panel — URL of the SRS server (e.g. "http://localhost:5002")
# Leave blank to disable the SRS panel on the dashboard (or set srsUrl in campaign.json instead)
$srsUrl = ""

# DCSServerBot's RestAPI plugin — bfdb has no Discord-link database of its
# own; it resolves a Discord user's ucid by querying the bot's own player
# database here (POST {url}/getuser), and sources the dashboard's restart
# countdown from here too (GET {url}/servers). MUST include whatever
# `prefix` is set to in the bot's restapi.yaml — e.g. if that file has
# `prefix: stats`, this needs to be "http://127.0.0.1:9876/stats", NOT
# just "http://127.0.0.1:9876" (the API doesn't live at the bare root).
# No trailing slash. Leave blank to disable linking entirely (dashboard
# "My Profile", the cockpit UI, and the restart countdown will all behave
# as if nothing is configured, rather than erroring).
$dcsServerBotUrl = ""

# X-API-Key from the bot's restapi.yaml (api_key). Secret -- lives in
# bfsystem.local.ps1 as $dcsServerBotApiKey, not here.

# Allow the dashboard/site to call this API cross-origin (they're on separate
# domains now: vectorstrike.org and dashboard.vectorstrike.org via Vercel).
# Also flips the session cookie to SameSite=None; Secure, required for that.
$corsOrigins = @(
    "https://vectorstrike.org",
    "https://www.vectorstrike.org",
    "https://dashboard.vectorstrike.org",
    "https://wiki.vectorstrike.org"
)

# Netidx base path bflib publishes under. bflib takes the "netidx_base"
# field from the engine config above and appends the mission's "Sortie" name
# to it (base.append(sortie)) -- so the real value bfdb needs to subscribe
# to is "<netidx_base>/<sortie>", not just netidx_base alone.
# "/local/fowl/campaign" below is netidx_base from the repo's tracked
# miz/ODFv2_CFG -- VERIFY this matches your live $engineConfigPath above,
# and confirm the full path (including the sortie suffix) against bflib's
# own startup log, which logs the exact base path it publishes to.
# Leave blank to run bfdb without netidx -- REST endpoints backed by
# --stats-jsonl still work, but live engine-side features (Engine Log,
# the Discord bot's engine log relay, and priority/commander-spawn RPCs)
# won't, since those need a live subscription to bflib.
$netidxBase = "/local/fowl/campaign"

# ==========================================================
# Secrets: bfsystem.ps1 is tracked in git (public repo). $adminPassword lives
# in bfsystem.local.ps1 instead, which is gitignored and never committed.
# Copy bfsystem.local.ps1.example to bfsystem.local.ps1 and fill in a real
# password to get started.
# Defaults here so an older bfsystem.local.ps1 that predates Discord login
# (and so doesn't set these) doesn't leave them $null -- $null -ne "" is
# true in PowerShell, which would otherwise pass a literal null through to
# bfdb's argument list.
$discordClientId     = ""
$discordClientSecret = ""
$dcsServerBotApiKey  = ""
$localSecrets = Join-Path $PSScriptRoot "bfsystem.local.ps1"
if (-not (Test-Path $localSecrets)) {
    Write-Host "Missing $localSecrets -- copy bfsystem.local.ps1.example and set a real `$adminPassword." -ForegroundColor Red
    exit 1
}
. $localSecrets
if ([string]::IsNullOrWhiteSpace($adminPassword)) {
    Write-Host "`$adminPassword is empty in $localSecrets -- set a real password." -ForegroundColor Red
    exit 1
}

# ==========================================================

function Start-VECTOR {
    Write-Host "Cleaning up existing processes..." -ForegroundColor Gray
    Stop-Process -Name "bfdb" -Force -ErrorAction SilentlyContinue
    Get-Job -Name "DBEngine" -ErrorAction SilentlyContinue | Remove-Job -Force -ErrorAction SilentlyContinue

    # Only recycle the netidx resolver if it's NOT already serving. Killing a
    # healthy resolver drops bflib's publisher inside DCS for ~60s (its
    # heartbeat TTL) -- so if the FowlEngine supervisor relaunches bfdb, we
    # don't want to also blind the engine for a minute. If 4564 is already
    # listening, leave the resolver (and its job) alone.
    $resolverUp = Test-NetConnection -ComputerName 127.0.0.1 -Port 4564 -InformationLevel Quiet -WarningAction SilentlyContinue
    if (-not $resolverUp) {
        Stop-Process -Name "netidx" -Force -ErrorAction SilentlyContinue
        Get-Job -Name "NetidxResolver" -ErrorAction SilentlyContinue | Remove-Job -Force -ErrorAction SilentlyContinue
    } else {
        Write-Host "netidx resolver already listening on 127.0.0.1:4564 -- leaving it running." -ForegroundColor Gray
    }

    # Ensure DB directory exists
    if (-not (Test-Path $dbPath)) {
        New-Item -ItemType Directory -Path $dbPath | Out-Null
        Write-Host "Created DB directory: $dbPath" -ForegroundColor Gray
    }

    # bfdb's --base needs a running netidx resolver to subscribe to, and
    # bflib (inside DCS) needs one to publish to -- start it first so it's
    # up before bfdb tries to connect. Requires netidx-tools installed
    # (cargo install netidx-tools) and %APPDATA%\netidx\client.json set up
    # for this account -- see user-guide/src/server-setup.
    if ($netidxBase -ne "" -and $resolverUp) {
        Write-Host "Skipping netidx resolver start -- already up on 127.0.0.1:4564." -ForegroundColor Gray
    }
    elseif ($netidxBase -ne "") {
        if (-not (Get-Command netidx -ErrorAction SilentlyContinue)) {
            Write-Host "netidx CLI not found on PATH -- run 'cargo install netidx-tools' first, or clear `$netidxBase to skip the resolver." -ForegroundColor Red
        } else {
            Write-Host "Starting netidx resolver..." -ForegroundColor Cyan
            Start-Job -Name "NetidxResolver" -ScriptBlock {
                netidx resolver-server -f -c $using:netidxResolverConfig
            } | Out-Null
            Start-Sleep -Seconds 2
            $resolverJob = Get-Job -Name "NetidxResolver"
            if ($resolverJob.State -eq "Failed" -or $resolverJob.State -eq "Completed") {
                Write-Host "Resolver job exited immediately (state: $($resolverJob.State)) -- it's not actually running. Output below:" -ForegroundColor Red
                Receive-Job -Name "NetidxResolver" -Keep
            } elseif (-not (Test-NetConnection -ComputerName 127.0.0.1 -Port 4564 -InformationLevel Quiet -WarningAction SilentlyContinue)) {
                Write-Host "Resolver job is running but nothing is listening on 127.0.0.1:4564 yet -- check netidx-resolver.json's addr and the resolver output in the status loop below." -ForegroundColor Yellow
            } else {
                Write-Host "Resolver is listening on 127.0.0.1:4564." -ForegroundColor Green
            }
        }
    }

    Write-Host "Starting bfdb..." -ForegroundColor Cyan

    Start-Job -Name "DBEngine" -ScriptBlock {
        $argList = @(
            "--db",             $using:dbPath,
            "--config",         $using:configPath,
            "--stats-jsonl",    $using:statsJsonl,
            "--listen-address", $using:listenAddress,
            "--site-address",   $using:siteAddress,
            "--admin-username", $using:adminUsername,
            "--admin-password", $using:adminPassword
        )
        if ($using:srsUrl -ne "") {
            $argList += "--srs-url", $using:srsUrl
        }
        if ($using:dcsServerBotUrl -ne "" -and $using:dcsServerBotApiKey -ne "") {
            $argList += "--dcsserverbot-url", $using:dcsServerBotUrl
            $argList += "--dcsserverbot-api-key", $using:dcsServerBotApiKey
        }
        if ($using:netidxBase -ne "") {
            $argList += "--base", $using:netidxBase
        }
        if ($using:engineConfigPath -ne "") {
            $argList += "--engine-config", $using:engineConfigPath
        }
        if ($using:statsDir -ne "") {
            $argList += "--stats-dir", $using:statsDir
        }
        foreach ($origin in $using:corsOrigins) {
            $argList += "--cors-origin", $origin
        }
        if ($using:discordClientId -ne "" -and $using:discordClientSecret -ne "") {
            $argList += "--discord-client-id",     $using:discordClientId
            $argList += "--discord-client-secret", $using:discordClientSecret
            $argList += "--discord-redirect-uri",  $using:discordRedirectUri
            $argList += "--discord-guild-id",      $using:discordGuildId
            $argList += "--discord-admin-role-id", $using:discordAdminRoleId
        }
        if ($using:logFile -ne "") {
            $argList += "--log-file", $using:logFile
        }
        & $using:bfdbExe @argList
    } | Out-Null

    $dashPort = $listenAddress.Split(':')[1]
    $sitePort  = $siteAddress.Split(':')[1]
    Write-Host "Dashboard : http://localhost:$dashPort/"  -ForegroundColor Green
    Write-Host "Website   : http://localhost:$sitePort/"  -ForegroundColor Green
    Write-Host "Press 'Q' to stop and exit.`n" -ForegroundColor Yellow

    while ($true) {
        if ([console]::KeyAvailable) {
            $key = [console]::ReadKey($true)
            if ($key.Key -eq 'Q') { Stop-VECTOR; break }
        }

        Clear-Host
        if (Get-Job -Name "NetidxResolver" -ErrorAction SilentlyContinue) {
            Write-Host "=========== NETIDX RESOLVER ===========" -ForegroundColor DarkCyan
            $resolverJob = Get-Job -Name "NetidxResolver"
            Write-Host "Job state: $($resolverJob.State)" -ForegroundColor Gray
            Receive-Job -Name "NetidxResolver" -Keep | Select-Object -Last 10
            Write-Host ""
        }
        Write-Host "=========== FOWL ENGINE DB ===========" -ForegroundColor Magenta
        $dbJob = Get-Job -Name "DBEngine" -ErrorAction SilentlyContinue
        if ($dbJob -and $dbJob.State -eq "Running") {
            Write-Host "Status: Running" -ForegroundColor Green
            Write-Host "Logs   : $logFile" -ForegroundColor Gray
        } else {
            $state = if ($dbJob) { $dbJob.State } else { "Not started" }
            Write-Host "Status: $state" -ForegroundColor Red
            Receive-Job -Name "DBEngine" -Keep | Select-Object -Last 20
        }

        Write-Host "`n======================================"
        Write-Host "Dashboard: http://localhost:$dashPort/  |  Website: http://localhost:$sitePort/  |  Press 'Q' to shutdown" -ForegroundColor Gray

        Start-Sleep -Seconds 2
    }
}

function Stop-VECTOR {
    Write-Host "`nShutting down..." -ForegroundColor Red
    Get-Job | Stop-Job  -ErrorAction SilentlyContinue
    Get-Job | Remove-Job -Force -ErrorAction SilentlyContinue
    Stop-Process -Name "bfdb" -Force -ErrorAction SilentlyContinue
    Stop-Process -Name "netidx" -Force -ErrorAction SilentlyContinue
    Write-Host "Stopped." -ForegroundColor Green
    Start-Sleep -Seconds 1
    exit
}

Start-VECTOR