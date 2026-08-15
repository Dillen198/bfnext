# ==========================================================
# FOWL ENGINE SERVICE STARTER
# ==========================================================

# Path to bfdb.exe (copy it here from target\release\bfdb.exe)
$bfdbExe = "C:\Users\ATPAdmin\Saved Games\DCS.vectorstrike_1\bfdb.exe" 

# Path where the Sled database will be stored (dedicated folder)
$dbPath = "C:\Users\Administrator\Saved Games\server_2\bfdb"

# Path to your campaign branding config JSON
$configPath = "C:\Users\Administrator\Saved Games\server_2\campaign.json"

# Path to the stats JSONL file written by bflib
# bflib writes this file as missions run — e.g. DCS\Logs\stats.jsonl
$statsJsonl = "C:\Users\Administrator\Saved Games\server_2\Logs\stats.jsonl"

# Path where bfdb.exe will natively write its plain-text logs
$logFile = "C:\Users\Administrator\Saved Games\server_2\Logs\bfdb.log"

# Dashboard address (stats + API + map)
$listenAddress = "0.0.0.0:8880"

# Public website address (separate port)
$siteAddress = "0.0.0.0:8766"

# Admin panel local login (username + password — no Discord required)
# Leave blank to disable local login (Discord-only)
$adminUsername = "admin"

# SRS radio panel — URL of the SRS server (e.g. "http://localhost:5002")
# Leave blank to disable the SRS panel on the dashboard (or set srsUrl in campaign.json instead)
$srsUrl = ""

# Allow the dashboard/site to call this API cross-origin (they're on separate
# domains now: vectorstrike.org and dashboard.vectorstrike.org via Vercel).
# Also flips the session cookie to SameSite=None; Secure, required for that.
$corsOrigins = @(
    "https://vectorstrike.org",
    "https://www.vectorstrike.org",
    "https://dashboard.vectorstrike.org"
)

# Netidx base path bflib publishes under (matches cfg.netidx_base + the
# mission's sortie name in the campaign engine config). Leave blank to run
# bfdb without netidx -- REST endpoints backed by --stats-jsonl still work,
# but live engine-side features (the Discord bot's engine log relay, and the
# priority/commander-spawn RPCs) won't, since those need a live subscription
# to bflib. Find the real value from bflib's own startup log (it logs the
# base path it publishes to), or from whatever config sets cfg.netidx_base.
$netidxBase = ""

# ==========================================================
# Secrets: bfsystem.ps1 is tracked in git (public repo). $adminPassword lives
# in bfsystem.local.ps1 instead, which is gitignored and never committed.
# Copy bfsystem.local.ps1.example to bfsystem.local.ps1 and fill in a real
# password to get started.
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

    # Ensure DB directory exists
    if (-not (Test-Path $dbPath)) {
        New-Item -ItemType Directory -Path $dbPath | Out-Null
        Write-Host "Created DB directory: $dbPath" -ForegroundColor Gray
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
        if ($using:netidxBase -ne "") {
            $argList += "--base", $using:netidxBase
        }
        foreach ($origin in $using:corsOrigins) {
            $argList += "--cors-origin", $origin
        }
        # if ($using:logFile -ne "") {
        #     $argList += "--log-file", $using:logFile
        # }
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
        Write-Host "=========== FOWL ENGINE DB ===========" -ForegroundColor Magenta
        Receive-Job -Name "DBEngine" -Keep | Select-Object -Last 20

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
    Write-Host "Stopped." -ForegroundColor Green
    Start-Sleep -Seconds 1
    exit
}

Start-VECTOR