<#
.SYNOPSIS
    Opens THIS server box's Fowl Engine OPS API to your tailnet, so Fowl
    Engine Manager on another PC can control it. Opt-in; -Undo reverses it.

.DESCRIPTION
    Run once on each server box (campaign, range), in an elevated PowerShell.
    See deploy/manager-remote.md for the whole picture. It:

      1. finds this box's Tailscale address (tailscale ip -4);
      2. adds two Windows Firewall rules for the bot's WebService port:
           allow  from 100.64.0.0/10 (the tailnet) on the Tailscale adapter
           block  from every other IPv4 address (a block beats any allow rule,
                  e.g. the "python.exe, all ports" one Windows offers to create)
         Loopback is not filtered, so bfdb and this box's own Manager keep
         working on 127.0.0.1;
      3. sets the WebService's listen: to 0.0.0.0 in
         <bot>\config\services\webservice.yaml (a copy of the old file is
         kept next to it);
      4. checks fowlengine.yaml has ops_api.api_key (32+ characters) and
         ops_api.require_own_key: true -- it does NOT edit that file: use the
         Manager's BOT CONFIG tab (checks -> Generate & set) or an editor;
      5. prints the OPS address to type into the controller PC's Manager.

    The WebService only picks up the new listen: when DCSServerBot restarts:
    -RestartBot asks the Manager service to do that now (DCS keeps running).

.PARAMETER BotDir
    DCSServerBot's folder. Default: bot_dir from Fowl Engine Manager's
    %ProgramData%\FowlEngine\manager.json.

.PARAMETER Undo
    Remove the firewall rules and set listen: back to 127.0.0.1.

.PARAMETER RestartBot
    Ask the FowlEngine service to restart DCSServerBot afterwards.

.EXAMPLE
    .\deploy\manager-remote-setup.ps1 -RestartBot
.EXAMPLE
    .\deploy\manager-remote-setup.ps1 -Undo -RestartBot
#>
[CmdletBinding()]
param(
    [string]$BotDir,
    [switch]$Undo,
    [switch]$RestartBot
)

$ErrorActionPreference = 'Stop'
$Group = 'Fowl Engine OPS (Tailscale)'
$AllowName = 'Fowl Engine OPS - allow tailnet'
$BlockName = 'Fowl Engine OPS - block everything else'
# every IPv4 address except the tailnet (100.64.0.0/10) and loopback (127/8)
$NotTailnet = @('0.0.0.0-100.63.255.255', '100.128.0.0-126.255.255.255', '128.0.0.0-255.255.255.255')

function Write-Step([string]$t) { Write-Host "`n== $t" -ForegroundColor Cyan }
function Write-Ok([string]$t) { Write-Host "  ok   $t" -ForegroundColor Green }
function Write-Warn([string]$t) { Write-Host "  !!   $t" -ForegroundColor Yellow }
function Fail([string]$t) { Write-Host "  FAIL $t" -ForegroundColor Red; exit 1 }

$admin = ([Security.Principal.WindowsPrincipal][Security.Principal.WindowsIdentity]::GetCurrent()).IsInRole(
    [Security.Principal.WindowsBuiltInRole]::Administrator)
if (-not $admin) { Fail 'run this from an elevated PowerShell (Run as administrator)' }

$data = $env:FOWL_MANAGER_DATA
if (-not $data) { $data = Join-Path $env:ProgramData 'FowlEngine' }

# ---- the bot's folder and its config ----------------------------------------------

if (-not $BotDir) {
    $mj = Join-Path $data 'manager.json'
    if (Test-Path $mj) {
        try { $BotDir = (Get-Content $mj -Raw | ConvertFrom-Json).bot_dir } catch { $BotDir = $null }
    }
}
if (-not $BotDir -or -not (Test-Path (Join-Path $BotDir 'run.py'))) {
    Fail "no DCSServerBot folder (pass -BotDir C:\path\to\DCSServerBot)"
}
$wsFile = Join-Path $BotDir 'config\services\webservice.yaml'
$feFile = Join-Path $BotDir 'config\plugins\fowlengine.yaml'
if (-not (Test-Path $wsFile)) { Fail "$wsFile not found -- configure DCSServerBot's WebService first" }

$ws = Get-Content $wsFile
$port = 9876
foreach ($l in $ws) { if ($l -match '^\s+port:\s*(\d+)') { $port = [int]$Matches[1]; break } }

function Set-Listen([string]$value) {
    $lines = [System.Collections.Generic.List[string]](Get-Content $wsFile)
    $i = -1
    for ($n = 0; $n -lt $lines.Count; $n++) { if ($lines[$n] -match '^(\s+)listen:') { $i = $n; break } }
    $current = $null
    if ($i -ge 0 -and $lines[$i] -match 'listen:\s*["'']?([^"''\s#]+)') { $current = $Matches[1] }
    if ($current -eq $value) { Write-Ok "webservice.yaml already says listen: $value"; return $false }
    $stamp = Get-Date -Format 'yyyyMMdd-HHmmss'
    Copy-Item $wsFile "$wsFile.before-manager-remote-$stamp"
    if ($i -ge 0) {
        $indent = ([regex]::Match($lines[$i], '^\s+')).Value
        $lines[$i] = "${indent}listen: $value"
    } else {
        $d = -1
        for ($n = 0; $n -lt $lines.Count; $n++) { if ($lines[$n] -match '^DEFAULT:') { $d = $n; break } }
        if ($d -lt 0) { Fail "$wsFile has no DEFAULT: section -- set listen: $value by hand" }
        $lines.Insert($d + 1, "  listen: $value")
    }
    [System.IO.File]::WriteAllLines($wsFile, $lines)
    $was = 'not set'
    if ($current) { $was = $current }
    Write-Ok "webservice.yaml: listen: $value (was $was; old copy kept as $wsFile.before-manager-remote-$stamp)"
    return $true
}

function Request-BotRestart {
    $cmd = Join-Path $data 'commands'
    if (-not (Test-Path $cmd)) { Write-Warn "no $cmd -- restart DCSServerBot yourself"; return }
    Set-Content -Path (Join-Path $cmd 'restart-bot') -Value 'manager-remote-setup'
    Write-Ok 'asked the FowlEngine service to restart DCSServerBot (DCS keeps running)'
}

# ---- undo ---------------------------------------------------------------------------

if ($Undo) {
    Write-Step 'Closing the OPS API to the tailnet'
    Get-NetFirewallRule -Group $Group -ErrorAction SilentlyContinue | Remove-NetFirewallRule
    Write-Ok "firewall rules in group '$Group' removed"
    $changed = Set-Listen '127.0.0.1'
    if ($changed -and $RestartBot) { Request-BotRestart }
    elseif ($changed) { Write-Warn 'restart DCSServerBot (Manager -> Restart bot) for it to take effect' }
    exit 0
}

# ---- 1. Tailscale -------------------------------------------------------------------

Write-Step 'Tailscale'
$ts = (Get-Command tailscale.exe -ErrorAction SilentlyContinue).Source
if (-not $ts) {
    $guess = Join-Path $env:ProgramFiles 'Tailscale\tailscale.exe'
    if (Test-Path $guess) { $ts = $guess }
}
if (-not $ts) { Fail 'tailscale.exe not found -- install Tailscale on this box and sign in first' }
$ip = (& $ts ip -4 2>$null | Select-Object -First 1)
if (-not $ip) { Fail 'tailscale ip -4 printed nothing -- is Tailscale signed in and connected?' }
$ip = $ip.Trim()
$o = $ip.Split('.')
if (-not ($o.Count -eq 4 -and [int]$o[0] -eq 100 -and [int]$o[1] -ge 64 -and [int]$o[1] -le 127)) {
    Fail "tailscale ip -4 says $ip, which is not in 100.64.0.0/10"
}
Write-Ok "this box is $ip on the tailnet"
$tsAdapter = Get-NetAdapter -ErrorAction SilentlyContinue | Where-Object { $_.InterfaceDescription -like 'Tailscale*' -or $_.Name -like 'Tailscale*' } |
    Select-Object -First 1

# ---- 2. firewall ----------------------------------------------------------------------

Write-Step "Windows Firewall (group '$Group', TCP $port)"
Get-NetFirewallRule -Group $Group -ErrorAction SilentlyContinue | Remove-NetFirewallRule
$allow = @{
    DisplayName = $AllowName; Group = $Group; Direction = 'Inbound'; Action = 'Allow'; Protocol = 'TCP'
    LocalPort = "$port"; RemoteAddress = '100.64.0.0/10'; Profile = 'Any'
    Description = "Fowl Engine Manager remote control: the bot's WebService (OPS API) from the tailnet only. deploy/manager-remote.md"
}
if ($tsAdapter) { $allow.InterfaceAlias = $tsAdapter.Name }
New-NetFirewallRule @allow | Out-Null
if ($tsAdapter) { Write-Ok "allow TCP $port from 100.64.0.0/10 on '$($tsAdapter.Name)'" }
else { Write-Warn "allow TCP $port from 100.64.0.0/10 (no Tailscale adapter found to pin it to)" }
New-NetFirewallRule -DisplayName $BlockName -Group $Group -Direction Inbound -Action Block -Protocol TCP `
    -LocalPort "$port" -RemoteAddress $NotTailnet -Profile Any `
    -Description "Fowl Engine Manager remote control: nobody outside the tailnet reaches the OPS API, whatever else allows python.exe. deploy/manager-remote.md" | Out-Null
Write-Ok "block TCP $port from every other IPv4 address (beats any allow rule)"

# ---- 3. listen on every interface (the rules above decide who gets in) ------------------

Write-Step 'WebService'
$changed = Set-Listen '0.0.0.0'

# ---- 4. the OPS key -------------------------------------------------------------------

Write-Step 'OPS API key (fowlengine.yaml)'
$prefix = $null
if (Test-Path $feFile) {
    $fe = Get-Content $feFile
    $inOps = $false; $key = $null; $req = $false; $dcsUrl = $null
    foreach ($l in $fe) {
        if ($l -match '^\s{2}ops_api:\s*$') { $inOps = $true; continue }
        if ($inOps -and $l -match '^\s{2}\S') { $inOps = $false }
        if ($inOps -and $l -match '^\s{4}api_key:\s*["'']?([^"''\s#]*)') { $key = $Matches[1] }
        if ($inOps -and $l -match '^\s{4}require_own_key:\s*true') { $req = $true }
        if ($inOps -and $l -match '^\s{4}prefix:\s*["'']?([^"''\s#]*)') { $prefix = $Matches[1] }
        if ($l -match '^\s+dcsserverbot_url:\s*["'']?([^"''\s#]+)') { $dcsUrl = $Matches[1] }
    }
    if ($key -and $key.Length -ge 32) { Write-Ok "ops_api.api_key is set ($($key.Length) characters)" }
    elseif ($key) { Write-Warn "ops_api.api_key is only $($key.Length) characters -- use 32 or more" }
    else { Write-Warn 'ops_api.api_key is not set -- Manager -> BOT CONFIG -> checks -> OPS API key -> Generate & set' }
    if ($req) { Write-Ok 'ops_api.require_own_key: true (the RestAPI key is not accepted)' }
    else { Write-Warn 'add ops_api.require_own_key: true (under ops_api:) so only that key opens the OPS API' }
    if ($null -eq $prefix -and $dcsUrl -and $dcsUrl -match '^[a-z]+://[^/]+(/.*)?$') { $prefix = $Matches[1] }
} else {
    Write-Warn "$feFile not found"
}
$prefix = ('/' + ("$prefix").Trim('/')).TrimEnd('/')

if ($changed) {
    if ($RestartBot) { Request-BotRestart }
    else { Write-Warn 'restart DCSServerBot (Manager -> OVERVIEW -> Restart bot) for listen: 0.0.0.0 to take effect' }
}

# ---- 5. what to type on the controller --------------------------------------------------

Write-Step 'On your PC: Fowl Engine Manager -> ALL SERVERS -> Add a server'
Write-Host "  OPS address  http://${ip}:$port$prefix/fowlengine/ops"
Write-Host "  OPS key      this box's ops_api.api_key (fowlengine.yaml)"
Write-Host "`n  Then limit who may reach port $port in your Tailscale policy (deploy/manager-remote.md, step 4)."
