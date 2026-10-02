# Copyright (c) 2026 Dillen Weerasinghe. All rights reserved.
# Proprietary and confidential. No license is granted to use, copy, modify, or
# distribute this file. See the repository NOTICE file.
#
# The MAIN PC half of "Range on a second PC" (deploy/range.md). Run it after
# setup-range-pc.ps1, from an ELEVATED PowerShell on the main PC, logged in as
# the account DCSServerBot runs as (ATPAdmin): the share password is saved
# for that account only.
#
# What it does:
#   1. checks the range PC answers (resolver, file share)
#   2. firewall: Export.lua positions and the bot database from the range PC
#   3. checks (and with -ConfigurePostgres, sets) PostgreSQL remote access for
#      the DCSServerBot agent on the range PC -- never restarts PostgreSQL
#   4. saves the share login (cmdkey) and checks bfdb can read the range logs
#   5. prints the fowlengine.yaml lines to paste
#
# Example:
#   .\setup-main-pc.ps1 -RangePcIp 192.168.1.60
#   .\setup-main-pc.ps1 -RangePcIp 192.168.1.60 -ConfigurePostgres

#Requires -RunAsAdministrator

param(
    [Parameter(Mandatory = $true)][string]$RangePcIp,
    [int]$ResolverPort = 4564,
    [int]$ExportPort   = 42004,
    [int]$PostgresPort = 5432,
    [string]$ShareUser = "fowlread",
    # Edit postgresql.conf / pg_hba.conf (backups are kept). Off by default:
    # the live bot uses this database.
    [switch]$ConfigurePostgres,
    [switch]$SkipFirewall,
    [switch]$SkipCredentials
)

$ErrorActionPreference = "Stop"

function Write-Step([string]$msg) { Write-Host "`n== $msg" -ForegroundColor Cyan }
function Write-Ok([string]$msg)   { Write-Host "   $msg" -ForegroundColor Green }
function Write-Warn([string]$msg) { Write-Host "   $msg" -ForegroundColor Yellow }

function Test-TcpPort([string]$ip, [int]$port) {
    $c = New-Object Net.Sockets.TcpClient
    try {
        $ar = $c.BeginConnect($ip, $port, $null, $null)
        if (-not $ar.AsyncWaitHandle.WaitOne(1500)) { return $false }
        $c.EndConnect($ar)
        return $true
    } catch {
        return $false
    } finally {
        $c.Close()
    }
}

function ConvertTo-UInt64Ip([string]$ip) {
    $b = ([Net.IPAddress]::Parse($ip)).GetAddressBytes()
    return ([uint64]$b[0] * 16777216) + ([uint64]$b[1] * 65536) + ([uint64]$b[2] * 256) + [uint64]$b[3]
}

function Get-NetworkBase([string]$ip, [int]$prefix) {
    $n = ConvertTo-UInt64Ip $ip
    $size = [uint64][math]::Pow(2, 32 - $prefix)
    return $n - ($n % $size)
}

[void][Net.IPAddress]::Parse($RangePcIp)
$mainIp = $null
foreach ($c in (Get-NetIPAddress -AddressFamily IPv4 | Where-Object { $_.IPAddress -notlike "127.*" })) {
    if ((Get-NetworkBase $c.IPAddress $c.PrefixLength) -eq (Get-NetworkBase $RangePcIp $c.PrefixLength)) {
        $mainIp = $c.IPAddress
        break
    }
}
if (-not $mainIp) { throw "This PC has no address on the range PC's subnet ($RangePcIp). Are they on the same network?" }

# ---- 1. can we see it -------------------------------------------------------

Write-Step "Range PC $RangePcIp (this PC: $mainIp)"
if (Test-TcpPort $RangePcIp $ResolverPort) {
    Write-Ok "netidx resolver answers on ${RangePcIp}:$ResolverPort"
} else {
    Write-Warn "no answer on ${RangePcIp}:$ResolverPort -- run setup-range-pc.ps1 there first (and check its 'FowlRange netidx resolver' task is running)."
}
if (Test-TcpPort $RangePcIp 445) {
    Write-Ok "file sharing answers on ${RangePcIp}:445"
} else {
    Write-Warn "no answer on ${RangePcIp}:445 -- the log shares are not reachable (setup-range-pc.ps1 opens it for $mainIp only)."
}

# ---- 2. firewall -------------------------------------------------------------

function Set-FowlRule([string]$name, [string]$proto, [string]$port, [string]$remote) {
    Get-NetFirewallRule -DisplayName $name -ErrorAction SilentlyContinue | Remove-NetFirewallRule
    New-NetFirewallRule -DisplayName $name -Group "Fowl Range" -Direction Inbound -Action Allow `
        -Protocol $proto -LocalPort $port -RemoteAddress $remote -Profile Any | Out-Null
    Write-Ok ("{0,-44} {1} {2} from {3}" -f $name, $proto, $port, $remote)
}

if ($SkipFirewall) {
    Write-Step "Firewall (skipped)"
} else {
    Write-Step "Firewall (rule group 'Fowl Range')"
    Set-FowlRule "Fowl Range - live positions (Export.lua)" "UDP" "$ExportPort" $RangePcIp
    Set-FowlRule "Fowl Range - bot database (PostgreSQL)"   "TCP" "$PostgresPort" $RangePcIp
}

# ---- 3. PostgreSQL -----------------------------------------------------------

Write-Step "PostgreSQL (the DCSServerBot agent on the range PC shares the bot's database)"
$svc = Get-CimInstance Win32_Service | Where-Object { $_.Name -like "postgresql*" } | Select-Object -First 1
$dataDir = $null
if ($svc -and $svc.PathName -match '-D\s+"([^"]+)"') { $dataDir = $Matches[1] }
elseif ($svc -and $svc.PathName -match '-D\s+(\S+)') { $dataDir = $Matches[1] }
if (-not $dataDir) {
    Write-Warn "PostgreSQL service not found on this PC. If the bot's database is elsewhere, allow $RangePcIp there."
} else {
    $conf = Join-Path $dataDir "postgresql.conf"
    $hba  = Join-Path $dataDir "pg_hba.conf"
    $confTxt = [IO.File]::ReadAllText($conf)
    $hbaTxt  = [IO.File]::ReadAllText($hba)
    $listen = "localhost"
    if ($confTxt -match "(?m)^\s*listen_addresses\s*=\s*'([^']*)'") { $listen = $Matches[1] }
    $listenOk = ($listen -eq "*") -or ($listen -split "\s*,\s*" -contains $mainIp)
    $hbaOk = $hbaTxt -match ("(?m)^\s*host\s+\S+\s+\S+\s+" + [regex]::Escape($RangePcIp) + "(/32)?\s")
    $method = "scram-sha-256"
    if ($hbaTxt -match "(?m)^\s*host\s+\S+\s+\S+\s+127\.0\.0\.1/32\s+(\S+)") { $method = $Matches[1] }
    $hbaLine = "host    all    all    $RangePcIp/32    $method    # Fowl Range: DCSServerBot agent on the range PC"
    $newListen = if ($listen -eq "localhost" -or -not $listen) { "localhost,$mainIp" } else { "$listen,$mainIp" }

    if ($listenOk) { Write-Ok "listen_addresses = '$listen' (reachable on $mainIp)" }
    else { Write-Warn "listen_addresses = '$listen' -- PostgreSQL does not listen on $mainIp yet" }
    if ($hbaOk) { Write-Ok "pg_hba.conf already lets $RangePcIp in" }
    else { Write-Warn "pg_hba.conf has no line for $RangePcIp" }

    if ((-not $listenOk -or -not $hbaOk) -and $ConfigurePostgres) {
        $tag = Get-Date -Format "yyyyMMdd-HHmmss"
        if (-not $listenOk) {
            Copy-Item $conf "$conf.bak-$tag"
            $line = "listen_addresses = '$newListen'`t# Fowl Range: + the LAN address for the range PC"
            if ($confTxt -match "(?m)^\s*#?\s*listen_addresses\s*=.*$") {
                $confTxt = ([regex]"(?m)^\s*#?\s*listen_addresses\s*=.*$").Replace($confTxt, $line, 1)
            } else {
                $confTxt = $confTxt + "`r`n" + $line + "`r`n"
            }
            [IO.File]::WriteAllText($conf, $confTxt)
            Write-Ok "set listen_addresses = '$newListen' (backup postgresql.conf.bak-$tag)"
        }
        if (-not $hbaOk) {
            Copy-Item $hba "$hba.bak-$tag"
            [IO.File]::AppendAllText($hba, "`r`n$hbaLine`r`n")
            Write-Ok "added to pg_hba.conf: $hbaLine (backup pg_hba.conf.bak-$tag)"
        }
        Write-Warn "PostgreSQL reads these on its next restart. Do it when the servers are quiet:"
        Write-Warn "  Restart-Service $($svc.Name)"
        Write-Warn "(the bot reconnects by itself; the DCS servers keep running)"
    } elseif (-not $listenOk -or -not $hbaOk) {
        Write-Warn "Run again with -ConfigurePostgres to make these two edits, or by hand:"
        if (-not $listenOk) { Write-Warn "  $conf :  listen_addresses = '$newListen'" }
        if (-not $hbaOk)    { Write-Warn "  $hba :  $hbaLine" }
        Write-Warn "then restart the '$($svc.Name)' service when the servers are quiet."
    }
}

# ---- 4. the share login ------------------------------------------------------

Write-Step "Share login for bfdb"
$bot = Get-CimInstance Win32_Service | Where-Object { $_.Name -like "DCSServerBot*" } | Select-Object -First 1
if ($bot -and $bot.StartName) {
    $runAs = ($bot.StartName -replace "^\.\\", "")
    if ($runAs -notlike "*$env:USERNAME") {
        Write-Warn "the DCSServerBot service runs as '$($bot.StartName)', not '$env:USERNAME'. Saved logins are per account:"
        Write-Warn "run this script again logged in as that account, or:  runas /user:$($bot.StartName) `"cmdkey /add:$RangePcIp /user:$RangePcIp\$ShareUser /pass`""
    }
}
if ($SkipCredentials) {
    Write-Warn "skipped (-SkipCredentials)"
} else {
    Write-Host "   Type the password you gave '$ShareUser' on the range PC:" -ForegroundColor Gray
    & cmdkey.exe /add:$RangePcIp /user:"$RangePcIp\$ShareUser" /pass
    $logs = "\\$RangePcIp\FowlRangeLogs"
    if (Test-Path $logs) {
        Write-Ok "$logs is readable as $env:USERNAME"
        foreach ($f in @("stats.jsonl", "range.jsonl")) {
            if (Test-Path (Join-Path $logs $f)) { Write-Ok "  $f is there" }
            else { Write-Warn "  $f not there yet (bfrange writes it when the range mission first runs)" }
        }
    } else {
        Write-Warn "$logs is NOT readable. Check the password (cmdkey /list), and that setup-range-pc.ps1 made the share."
    }
}

# ---- 5. what to paste --------------------------------------------------------

Write-Step "Paste into fowlengine.yaml, the range entry under bfdb.instances, then /feops bfdb_restart"
Write-Host @"

        netidx_resolver: "${RangePcIp}:$ResolverPort"
        stats_jsonl: "\\\\$RangePcIp\\FowlRangeLogs\\stats.jsonl"
        range_jsonl: "\\\\$RangePcIp\\FowlRangeLogs\\range.jsonl"
        tacview_dir: "\\\\$RangePcIp\\FowlRangeTacview"
        export_port: $ExportPort

"@
