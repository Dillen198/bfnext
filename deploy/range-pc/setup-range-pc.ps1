# Copyright (c) 2026 Dillen Weerasinghe. All rights reserved.
# Proprietary and confidential. No license is granted to use, copy, modify, or
# distribute this file. See the repository NOTICE file.
#
# Prepares the SECOND PC -- the one that runs the training range DCS server --
# so bfdb and DCSServerBot on the main PC can reach it over the LAN.
# Run it once from an ELEVATED PowerShell on the range PC. Safe to run again
# (after an IP change, a new DCS.range folder, ...): it replaces what it made.
#
# What it does (see deploy/range.md, "Range on a second PC"):
#   1. writes a netidx resolver config and client config for this PC
#   2. registers the "FowlRange netidx resolver" scheduled task (starts at
#      boot, restarts if it dies) and starts it
#   3. firewall: resolver + engine RPCs + SMB from the main PC only; the DCS
#      and SRS ports from anywhere
#   4. shares DCS.range\Logs and DCS.range\Tacview read-only to a local
#      account that bfdb on the main PC reads them with
#   5. points Export.lua at the main PC (Scripts\bf_export_host.lua / _port.lua)
#   6. sets netidx_config in the range config (<sortie>_RANGE)
#
# Example:
#   .\setup-range-pc.ps1 -MainPcIp 192.168.1.10
#   .\setup-range-pc.ps1 -MainPcIp 192.168.1.10 -SavedGames "C:\Users\ATPAdmin\Saved Games\DCS.range"

#Requires -RunAsAdministrator

param(
    # LAN address of the main PC (bfdb, the bot, the campaign servers).
    [Parameter(Mandatory = $true)][string]$MainPcIp,
    # LAN address of THIS PC. Default: its address on the main PC's subnet.
    [string]$RangePcIp,
    # The range DCS server's Saved Games folder.
    [string]$SavedGames   = (Join-Path $env:USERPROFILE "Saved Games\DCS.range"),
    # Where the resolver config, client config and log live.
    [string]$FowlHome     = "C:\fowl-range",
    # netidx.exe (netidx-tools). Default: <FowlHome>\netidx.exe, then PATH.
    [string]$NetidxExe,
    [int]$ResolverPort    = 4564,
    [int]$ExportPort      = 42004,
    [int]$DcsPort         = 10328,
    [int]$SrsPort         = 5005,
    # Local account the main PC's bfdb reads the shares with.
    [string]$ShareUser    = "fowlread",
    # The range mission's sortie; the config is <SavedGames>\<Sortie>_RANGE.
    [string]$Sortie       = "VSRANGE",
    [switch]$SkipShare,
    [switch]$SkipFirewall
)

$ErrorActionPreference = "Stop"

function Write-Step([string]$msg) { Write-Host "`n== $msg" -ForegroundColor Cyan }
function Write-Ok([string]$msg)   { Write-Host "   $msg" -ForegroundColor Green }
function Write-Warn([string]$msg) { Write-Host "   $msg" -ForegroundColor Yellow }

# netidx refuses to read a JSON file that starts with a byte-order mark, and
# Windows PowerShell's Set-Content/Out-File write one. Always write through this.
function Write-Utf8NoBom([string]$path, [string]$text) {
    [IO.File]::WriteAllText($path, $text, (New-Object Text.UTF8Encoding($false)))
}

function ConvertTo-UInt64Ip([string]$ip) {
    $b = ([Net.IPAddress]::Parse($ip)).GetAddressBytes()
    if ($b.Length -ne 4) { throw "$ip is not an IPv4 address" }
    return ([uint64]$b[0] * 16777216) + ([uint64]$b[1] * 65536) + ([uint64]$b[2] * 256) + [uint64]$b[3]
}

function Get-NetworkBase([string]$ip, [int]$prefix) {
    $n = ConvertTo-UInt64Ip $ip
    $size = [uint64][math]::Pow(2, 32 - $prefix)
    $base = $n - ($n % $size)
    $o = @()
    foreach ($div in @(16777216, 65536, 256, 1)) {
        $o += [uint64]([math]::Floor($base / $div) % 256)
    }
    return ($o -join ".")
}

function Test-PrivateIp([string]$ip) {
    $b = ([Net.IPAddress]::Parse($ip)).GetAddressBytes()
    return ($b[0] -eq 10) -or ($b[0] -eq 172 -and $b[1] -ge 16 -and $b[1] -le 31) -or ($b[0] -eq 192 -and $b[1] -eq 168)
}

function Test-TcpPort([string]$ip, [int]$port) {
    $c = New-Object Net.Sockets.TcpClient
    try {
        $ar = $c.BeginConnect($ip, $port, $null, $null)
        if (-not $ar.AsyncWaitHandle.WaitOne(1000)) { return $false }
        $c.EndConnect($ar)
        return $true
    } catch {
        return $false
    } finally {
        $c.Close()
    }
}

# ---- 0. addresses -----------------------------------------------------------

Write-Step "Addresses"
[void][Net.IPAddress]::Parse($MainPcIp)
$prefix = 24
$local = Get-NetIPAddress -AddressFamily IPv4 |
    Where-Object { $_.IPAddress -notlike "127.*" -and $_.IPAddress -notlike "169.254.*" }
if ($RangePcIp) {
    $iface = $local | Where-Object { $_.IPAddress -eq $RangePcIp } | Select-Object -First 1
    if (-not $iface) { throw "$RangePcIp is not an address of this PC. Its addresses: $(($local | ForEach-Object IPAddress) -join ', ')" }
    $prefix = [int]$iface.PrefixLength
} else {
    foreach ($c in $local) {
        if ((Get-NetworkBase $c.IPAddress $c.PrefixLength) -eq (Get-NetworkBase $MainPcIp $c.PrefixLength)) {
            $RangePcIp = $c.IPAddress
            $prefix = [int]$c.PrefixLength
            break
        }
    }
    if (-not $RangePcIp) {
        throw "No address of this PC is on the same subnet as $MainPcIp. Pass -RangePcIp. This PC has: $(($local | ForEach-Object IPAddress) -join ', ')"
    }
}
if (-not (Test-PrivateIp $MainPcIp) -or -not (Test-PrivateIp $RangePcIp)) {
    Write-Warn "netidx only lets a private (LAN) publisher register with a private resolver: use LAN addresses (10.x, 172.16-31.x, 192.168.x)."
}
$subnet = "{0}/{1}" -f (Get-NetworkBase $RangePcIp $prefix), $prefix
Write-Ok "main PC $MainPcIp, this PC $RangePcIp, subnet $subnet"
if ((Get-NetIPInterface -AddressFamily IPv4 -InterfaceIndex ($local | Where-Object { $_.IPAddress -eq $RangePcIp } | Select-Object -First 1).InterfaceIndex).Dhcp -eq "Enabled") {
    Write-Warn "this PC gets its address from DHCP. Give it a fixed address (a DHCP reservation on the router) -- the resolver and the main PC's config name it."
}

# ---- 1. netidx configs --------------------------------------------------------

Write-Step "netidx configs in $FowlHome"
New-Item -ItemType Directory -Force -Path $FowlHome | Out-Null
if (-not $NetidxExe) {
    $NetidxExe = Join-Path $FowlHome "netidx.exe"
    if (-not (Test-Path $NetidxExe)) {
        $cmd = Get-Command netidx -ErrorAction SilentlyContinue
        if ($cmd) { $NetidxExe = $cmd.Source }
    }
}
if (-not (Test-Path $NetidxExe)) {
    throw "netidx.exe not found. Copy it from the main PC (run  (Get-Command netidx).Source  there; usually %USERPROFILE%\.cargo\bin\netidx.exe) to $FowlHome\netidx.exe and run this again."
}
$resolverCfg = Join-Path $FowlHome "resolver.json"
$clientCfg   = Join-Path $FowlHome "client.json"
$resolverLog = Join-Path $FowlHome "resolver.log"
$addr = "{0}:{1}" -f $RangePcIp, $ResolverPort

Write-Utf8NoBom $resolverCfg @"
{
    "parent": null,
    "children": [],
    "member_servers": [
        {
            "pid_file": "",
            "addr": "$addr",
            "max_connections": 768,
            "hello_timeout": 10,
            "reader_ttl": 60,
            "writer_ttl": 120,
            "id_map_type": "DoNotMap",
            "auth": "Anonymous"
        }
    ],
    "perms": {
        "/": {"Anonymous": "swlpd"}
    }
}
"@
# default_bind_config makes the engine publish on this PC's LAN address: the
# default (127.0.0.1) is refused by a resolver on a LAN address, and the main
# PC could not connect to it anyway.
Write-Utf8NoBom $clientCfg @"
{
    "base": "/",
    "addrs": [["$addr", "Anonymous"]],
    "default_auth": "Anonymous",
    "default_bind_config": "$subnet"
}
"@
Write-Ok "resolver.json (listens on $addr), client.json (publish on $subnet)"

# ---- 2. the resolver task -----------------------------------------------------

Write-Step "Resolver scheduled task"
$taskName = "FowlRange netidx resolver"
if (Get-ScheduledTask -TaskName $taskName -ErrorAction SilentlyContinue) {
    Stop-ScheduledTask -TaskName $taskName -ErrorAction SilentlyContinue
}
# the task runs netidx under cmd.exe (for the log redirect); stopping the task
# does not always take the child with it
Get-CimInstance Win32_Process -Filter "Name='netidx.exe'" |
    Where-Object { $_.CommandLine -like "*$resolverCfg*" } |
    ForEach-Object { Stop-Process -Id $_.ProcessId -Force -ErrorAction SilentlyContinue }

$cmdLine = '/c ""{0}" resolver-server -f -c "{1}" >> "{2}" 2>&1"' -f $NetidxExe, $resolverCfg, $resolverLog
$action    = New-ScheduledTaskAction -Execute "cmd.exe" -Argument $cmdLine -WorkingDirectory $FowlHome
$trigger   = New-ScheduledTaskTrigger -AtStartup
$settings  = New-ScheduledTaskSettingsSet -AllowStartIfOnBatteries -DontStopIfGoingOnBatteries `
                -StartWhenAvailable -ExecutionTimeLimit ([TimeSpan]::Zero) `
                -RestartCount 999 -RestartInterval (New-TimeSpan -Minutes 1)
$principal = New-ScheduledTaskPrincipal -UserId "SYSTEM" -LogonType ServiceAccount -RunLevel Highest
Register-ScheduledTask -TaskName $taskName -Action $action -Trigger $trigger -Settings $settings `
    -Principal $principal -Description "netidx resolver for the Fowl Engine training range (bfrange). See deploy/range.md." -Force | Out-Null
Start-ScheduledTask -TaskName $taskName
$up = $false
for ($i = 0; $i -lt 20; $i++) {
    if (Test-TcpPort $RangePcIp $ResolverPort) { $up = $true; break }
    Start-Sleep -Milliseconds 500
}
if ($up) {
    Write-Ok "resolver listening on $addr"
} else {
    Write-Warn "resolver is NOT listening on $addr after 10 s. Look at $resolverLog, or run it by hand to see why:"
    Write-Warn "  & '$NetidxExe' resolver-server -f -c '$resolverCfg'"
}

# ---- 3. firewall --------------------------------------------------------------

function Set-FowlRule([string]$name, [string]$proto, [string]$port, [string]$remote) {
    Get-NetFirewallRule -DisplayName $name -ErrorAction SilentlyContinue | Remove-NetFirewallRule
    $p = @{
        DisplayName = $name; Group = "Fowl Range"; Direction = "Inbound"; Action = "Allow"
        Protocol = $proto; LocalPort = $port; Profile = "Any"
    }
    if ($remote) { $p.RemoteAddress = $remote }
    New-NetFirewallRule @p | Out-Null
    $from = "anywhere"
    if ($remote) { $from = $remote }
    Write-Ok ("{0,-44} {1} {2} from {3}" -f $name, $proto, $port, $from)
}

if ($SkipFirewall) {
    Write-Step "Firewall (skipped)"
} else {
    Write-Step "Firewall (rule group 'Fowl Range')"
    Set-FowlRule "Fowl Range - netidx resolver"          "TCP" "$ResolverPort" $MainPcIp
    # the engine's RPC publisher takes a free port in netidx's range
    Set-FowlRule "Fowl Range - engine RPCs (netidx)"     "TCP" "5000-32767" $MainPcIp
    if (-not $SkipShare) {
        Set-FowlRule "Fowl Range - log shares (SMB)"     "TCP" "445" $MainPcIp
    }
    Set-FowlRule "Fowl Range - DCS server (TCP)"         "TCP" "$DcsPort" $null
    Set-FowlRule "Fowl Range - DCS server (UDP)"         "UDP" "$DcsPort" $null
    Set-FowlRule "Fowl Range - SRS (TCP)"                "TCP" "$SrsPort" $null
    Set-FowlRule "Fowl Range - SRS (UDP)"                "UDP" "$SrsPort" $null
    $blocks = Get-NetFirewallApplicationFilter -ErrorAction SilentlyContinue |
        Where-Object { $_.Program -match "DCS(_server)?\.exe$" } |
        Get-NetFirewallRule -ErrorAction SilentlyContinue |
        Where-Object { $_.Action -eq "Block" -and $_.Enabled -eq "True" }
    foreach ($b in $blocks) {
        Write-Warn "BLOCK rule '$($b.DisplayName)' on DCS -- it overrides the allow rules above (usually left by a dismissed 'allow access' prompt). Delete it in wf.msc."
    }
}

# ---- 4. shares ----------------------------------------------------------------

$logs    = Join-Path $SavedGames "Logs"
$tacview = Join-Path $SavedGames "Tacview"
if ($SkipShare) {
    Write-Step "Shares (skipped)"
} else {
    Write-Step "Read-only shares for bfdb"
    $acct = "$env:COMPUTERNAME\$ShareUser"
    if (-not (Get-LocalUser -Name $ShareUser -ErrorAction SilentlyContinue)) {
        $pw = Read-Host -AsSecureString "Password for the new local account '$ShareUser' (bfdb on the main PC reads the range logs with it; you type it again there)"
        New-LocalUser -Name $ShareUser -Password $pw -PasswordNeverExpires -UserMayNotChangePassword `
            -Description "Fowl Range: read-only access to the range logs for bfdb on the main PC" | Out-Null
        Write-Ok "created local account $acct"
    } else {
        Write-Ok "local account $acct already exists (password unchanged)"
    }
    foreach ($s in @(@{ Name = "FowlRangeLogs"; Path = $logs }, @{ Name = "FowlRangeTacview"; Path = $tacview })) {
        New-Item -ItemType Directory -Force -Path $s.Path | Out-Null
        if (Get-SmbShare -Name $s.Name -ErrorAction SilentlyContinue) {
            Remove-SmbShare -Name $s.Name -Force
        }
        New-SmbShare -Name $s.Name -Path $s.Path -ReadAccess $acct | Out-Null
        & icacls.exe $s.Path /grant "${acct}:(OI)(CI)RX" | Out-Null
        Write-Ok ("\\{0}\{1}  ->  {2}" -f $RangePcIp, $s.Name, $s.Path)
    }
}

# ---- 5. Export.lua ------------------------------------------------------------

Write-Step "Export.lua target"
$scripts = Join-Path $SavedGames "Scripts"
New-Item -ItemType Directory -Force -Path $scripts | Out-Null
Write-Utf8NoBom (Join-Path $scripts "bf_export_host.lua") ("return `"{0}`"`r`n" -f $MainPcIp)
Write-Utf8NoBom (Join-Path $scripts "bf_export_port.lua") ("return {0}`r`n" -f $ExportPort)
Write-Ok "Scripts\bf_export_host.lua -> $MainPcIp, Scripts\bf_export_port.lua -> $ExportPort"
if (-not (Test-Path (Join-Path $scripts "Export.lua"))) {
    Write-Warn "no Scripts\Export.lua yet: copy scripts/Export.lua from the repo (it reads the two files above)."
} elseif (-not (Select-String -Path (Join-Path $scripts "Export.lua") -Pattern "bf_export_host" -Quiet)) {
    Write-Warn "Scripts\Export.lua is an old copy that ignores bf_export_host.lua: replace it with scripts/Export.lua from the repo."
}

# ---- 6. the range config ------------------------------------------------------

Write-Step "Range config"
$rangeCfg = Join-Path $SavedGames ("{0}_RANGE" -f $Sortie)
$clientFwd = $clientCfg -replace "\\", "/"
if (-not (Test-Path $rangeCfg)) {
    Write-Warn "$rangeCfg not found. When it is in place, add this line under netidx_base:"
    Write-Warn ("  `"netidx_config`": `"{0}`"," -f $clientFwd)
} else {
    $txt = [IO.File]::ReadAllText($rangeCfg)
    $line = '"netidx_config": "{0}"' -f $clientFwd
    if ($txt -match '"netidx_config"\s*:\s*("[^"]*"|null)') {
        $new = ([regex]'"netidx_config"\s*:\s*("[^"]*"|null)').Replace($txt, $line, 1)
    } elseif ($txt -match '"netidx_base"\s*:\s*"[^"]*"\s*,') {
        $nl = "`n"
        if ($txt.Contains("`r`n")) { $nl = "`r`n" }
        $new = ([regex]'("netidx_base"\s*:\s*"[^"]*"\s*,)').Replace($txt, ('$1' + $nl + '  ' + $line + ','), 1)
    } else {
        $new = $null
        Write-Warn "$rangeCfg has no netidx_base, so the engine publishes nothing: add both, e.g."
        Write-Warn '  "netidx_base": "/local/fowl/range",'
        Write-Warn ("  {0}," -f $line)
    }
    if ($new -and $new -ne $txt) {
        Copy-Item $rangeCfg ("{0}.bak-{1}" -f $rangeCfg, (Get-Date -Format "yyyyMMdd-HHmmss"))
        Write-Utf8NoBom $rangeCfg $new
        Write-Ok "set netidx_config in $rangeCfg (backup next to it); applies on the next mission start"
    } elseif ($new) {
        Write-Ok "netidx_config already set"
    }
}

# ---- summary ------------------------------------------------------------------

Write-Step "Done. On the MAIN PC now:"
Write-Host @"
   1. Run deploy\range-pc\setup-main-pc.ps1 -RangePcIp $RangePcIp
      (firewall for Export.lua and the bot database, the share login).
   2. In fowlengine.yaml, the range instance under bfdb.instances:

        netidx_resolver: "$addr"
        stats_jsonl: "\\\\$RangePcIp\\FowlRangeLogs\\stats.jsonl"
        range_jsonl: "\\\\$RangePcIp\\FowlRangeLogs\\range.jsonl"
        tacview_dir: "\\\\$RangePcIp\\FowlRangeTacview"

      then /feops bfdb_restart.
"@
