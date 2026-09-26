# Installs DCSServerBot as an auto-starting, auto-restarting Windows service
# via NSSM, so the Vector Strike stack survives a reboot, a blue screen or a
# crash with nobody logging in. Run from an ELEVATED PowerShell. See README.md
# in this folder, then run check-autostart.ps1 to confirm everything.

#Requires -RunAsAdministrator

param(
    [string]$ServiceName = "DCSServerBot",
    [string]$BotDir      = "E:\Github\DCSServerBot",
    [string]$Account     = ".\ATPAdmin",
    [string]$Password    = $(Read-Host -AsSecureString "Password for $Account" |
                            ForEach-Object { [Runtime.InteropServices.Marshal]::PtrToStringAuto(
                                [Runtime.InteropServices.Marshal]::SecureStringToBSTR($_)) }),
    # Wait for the network before starting (Discord + the netidx resolver need
    # it); a plain auto-start can race the NIC on boot.
    [switch]$NoDelayedStart
)

$ErrorActionPreference = "Stop"

if (-not (Get-Command nssm -ErrorAction SilentlyContinue)) {
    throw "nssm.exe not found on PATH. Install it (choco install nssm) and retry."
}
$runCmd = Join-Path $BotDir "run.cmd"
if (-not (Test-Path $runCmd)) { throw "run.cmd not found at $runCmd -- fix `$BotDir." }

$log = Join-Path $BotDir "service.log"

# Fowl Engine Manager already runs the bot as its own "FowlEngine" service; a
# second (NSSM) service would start a second bot and the two fight over DCS.
if ($ServiceName -ne "FowlEngine" -and (Get-Service FowlEngine -ErrorAction SilentlyContinue)) {
    throw "Fowl Engine Manager's FowlEngine service is installed and already keeps the bot up across reboots. Don't install this one as well; manage the service from Fowl Engine Manager instead."
}

if (Get-Service $ServiceName -ErrorAction SilentlyContinue) {
    Write-Host "Service $ServiceName already exists -- reconfiguring." -ForegroundColor Yellow
    nssm stop $ServiceName confirm 2>$null | Out-Null
} else {
    nssm install $ServiceName $runCmd
}

nssm set $ServiceName AppDirectory     $BotDir
nssm set $ServiceName ObjectName       $Account $Password
nssm set $ServiceName Start            $(if ($NoDelayedStart) { "SERVICE_AUTO_START" } else { "SERVICE_DELAYED_AUTO_START" })
nssm set $ServiceName AppExit Default  Restart
nssm set $ServiceName AppRestartDelay  15000
# A bot restart (crash, update, the OPS page's "Restart bot") must NOT take
# the DCS servers down with it: DCSServerBot re-attaches to running servers,
# and procman replaces an orphaned bfdb.exe on start.
nssm set $ServiceName AppKillProcessTree 0
# Give the bot time to shut down cleanly (Ctrl-C first) before NSSM gets rough.
nssm set $ServiceName AppStopMethodConsole 20000
nssm set $ServiceName AppStdout        $log
nssm set $ServiceName AppStderr        $log
nssm set $ServiceName AppRotateFiles   1
nssm set $ServiceName AppRotateOnline  1
nssm set $ServiceName AppRotateBytes   10485760
nssm set $ServiceName Description      "Vector Strike: DCSServerBot + DCS servers + bfdb (Fowl Engine). Auto-starts at boot."

# OS-level recovery on top of NSSM's own restart, in case the process manager dies.
sc.exe failure $ServiceName reset= 86400 actions= restart/15000/restart/30000/restart/60000 | Out-Null
sc.exe failureflag $ServiceName 1 | Out-Null

# "Log on as a service" for the account. NSSM normally grants it when
# ObjectName is set; this makes sure (read, patch, re-apply the local policy).
function Test-LogonRight([string]$acct) {
    $tmp = Join-Path $env:TEMP "fowl-secpol-$([guid]::NewGuid().ToString('N')).inf"
    secedit /export /cfg $tmp /areas USER_RIGHTS | Out-Null
    $line = (Get-Content $tmp | Where-Object { $_ -like "SeServiceLogonRight*" }) -join ""
    Remove-Item $tmp -ErrorAction SilentlyContinue
    $sid = (New-Object System.Security.Principal.NTAccount($acct.TrimStart('.', '\'))).Translate(
        [System.Security.Principal.SecurityIdentifier]).Value
    return ($line -match [regex]::Escape($sid)) -or ($line -match [regex]::Escape($acct.TrimStart('.', '\')))
}
try {
    if (-not (Test-LogonRight $Account)) {
        Write-Host "Granting 'Log on as a service' to $Account" -ForegroundColor Yellow
        $sid = (New-Object System.Security.Principal.NTAccount($Account.TrimStart('.', '\'))).Translate(
            [System.Security.Principal.SecurityIdentifier]).Value
        $exp = Join-Path $env:TEMP "fowl-secpol-export.inf"
        $imp = Join-Path $env:TEMP "fowl-secpol-import.inf"
        $db  = Join-Path $env:TEMP "fowl-secpol.sdb"
        secedit /export /cfg $exp /areas USER_RIGHTS | Out-Null
        $cur = (Get-Content $exp | Where-Object { $_ -like "SeServiceLogonRight*" }) -replace '^SeServiceLogonRight\s*=\s*', ''
        $new = if ($cur) { "$cur,*$sid" } else { "*$sid" }
        @("[Unicode]", "Unicode=yes", "[Version]", 'signature="$CHICAGO$"', "Revision=1",
          "[Privilege Rights]", "SeServiceLogonRight = $new") | Set-Content -Encoding Unicode $imp
        secedit /configure /db $db /cfg $imp /areas USER_RIGHTS | Out-Null
        Remove-Item $exp, $imp, $db -ErrorAction SilentlyContinue
    }
} catch {
    Write-Warning "Could not verify/grant 'Log on as a service' ($_). Do it by hand: secpol.msc -> Local Policies -> User Rights Assignment -> Log on as a service -> add $Account."
}

nssm start $ServiceName
Start-Sleep 5
Get-Service $ServiceName | Format-Table -AutoSize

Write-Host @"

Installed. Now run (elevated or not):
  .\check-autostart.ps1
It checks the service, the account's rights, restart-after-BSOD, pending
Windows Update reboots and that bfdb / netidx answer. Then Restart-Computer
and run it again with nobody logged in (e.g. over SSH or from another PC).
"@ -ForegroundColor Green
