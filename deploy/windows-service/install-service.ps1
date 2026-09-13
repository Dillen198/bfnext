# Installs DCSServerBot as an auto-starting, auto-restarting Windows service
# via NSSM, so the Vector Strike stack survives a reboot / crash with no login.
# Run from an ELEVATED PowerShell. See README.md in this folder.

#Requires -RunAsAdministrator

param(
    [string]$ServiceName = "DCSServerBot",
    [string]$BotDir      = "E:\Github\DCSServerBot",
    [string]$Account     = ".\ATPAdmin",
    [string]$Password    = $(Read-Host -AsSecureString "Password for $Account" |
                            ForEach-Object { [Runtime.InteropServices.Marshal]::PtrToStringAuto(
                                [Runtime.InteropServices.Marshal]::SecureStringToBSTR($_)) })
)

$ErrorActionPreference = "Stop"

if (-not (Get-Command nssm -ErrorAction SilentlyContinue)) {
    throw "nssm.exe not found on PATH. Install it (choco install nssm) and retry."
}
$runCmd = Join-Path $BotDir "run.cmd"
if (-not (Test-Path $runCmd)) { throw "run.cmd not found at $runCmd -- fix `$BotDir." }

$log = Join-Path $BotDir "service.log"

if (Get-Service $ServiceName -ErrorAction SilentlyContinue) {
    Write-Host "Service $ServiceName already exists -- reconfiguring." -ForegroundColor Yellow
    nssm stop $ServiceName confirm 2>$null | Out-Null
} else {
    nssm install $ServiceName $runCmd
}

nssm set $ServiceName AppDirectory     $BotDir
nssm set $ServiceName ObjectName       $Account $Password
nssm set $ServiceName Start            SERVICE_AUTO_START
nssm set $ServiceName AppExit Default  Restart
nssm set $ServiceName AppRestartDelay  15000
nssm set $ServiceName AppStdout        $log
nssm set $ServiceName AppStderr        $log
nssm set $ServiceName AppRotateFiles   1
nssm set $ServiceName AppRotateBytes   10485760

# OS-level recovery on top of NSSM's own restart, in case the process manager dies.
sc.exe failure $ServiceName reset= 86400 actions= restart/15000/restart/30000/restart/60000 | Out-Null

nssm start $ServiceName
Start-Sleep 5
Get-Service $ServiceName | Format-Table -AutoSize

Write-Host @"

Installed. Verify:
  Invoke-WebRequest http://localhost:8880/api/health
  Test-NetConnection 127.0.0.1 -Port 4564 -InformationLevel Quiet
Then Restart-Computer and confirm both still pass with nobody logged in.
"@ -ForegroundColor Green
