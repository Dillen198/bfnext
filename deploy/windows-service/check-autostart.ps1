# Read-only check that the Vector Strike box comes back by itself after a
# reboot, a blue screen or a power cut -- with nobody logging in. Changes
# nothing. Run it after install-service.ps1, then again after a test
# Restart-Computer (from another PC / SSH, without logging in on the box).
#
#   .\check-autostart.ps1
#   .\check-autostart.ps1 -BfdbUrl http://127.0.0.1:8880 -ServiceName DCSServerBot

param(
    [string]$ServiceName = "DCSServerBot",
    [string]$BfdbUrl     = "http://127.0.0.1:8880",
    [int]$NetidxPort     = 4564,
    [string]$BotDir      = "E:\Github\DCSServerBot"
)

$fail = 0; $warn = 0
function Ok([string]$m)   { Write-Host "  [ OK ] $m" -ForegroundColor Green }
function Bad([string]$m, [string]$fix)  { Write-Host "  [FAIL] $m" -ForegroundColor Red; if ($fix) { Write-Host "         fix: $fix" -ForegroundColor DarkGray }; $script:fail++ }
function Warn([string]$m, [string]$fix) { Write-Host "  [WARN] $m" -ForegroundColor Yellow; if ($fix) { Write-Host "         fix: $fix" -ForegroundColor DarkGray }; $script:warn++ }

Write-Host "`nService" -ForegroundColor Cyan
$svc = Get-CimInstance Win32_Service -Filter "Name='$ServiceName'" -ErrorAction SilentlyContinue
if (-not $svc) {
    Bad "service $ServiceName is not installed" "run install-service.ps1 (elevated)"
} else {
    if ($svc.StartMode -eq "Auto") { Ok "starts automatically at boot$(if ($svc.DelayedAutoStart) { ' (delayed, after the network)' })" }
    else { Bad "start mode is $($svc.StartMode)" "sc.exe config $ServiceName start= delayed-auto" }
    if ($svc.State -eq "Running") { Ok "running (pid $($svc.ProcessId)) as $($svc.StartName)" } else { Bad "state is $($svc.State)" "Start-Service $ServiceName; see $BotDir\service.log" }
    if ($svc.StartName -match "LocalSystem") { Warn "runs as LocalSystem -- DCS and netidx need the account that owns Saved Games\DCS.* and %APPDATA%\netidx" "re-run install-service.ps1 -Account .\<that user>" }
    $fl = sc.exe qfailure $ServiceName | Out-String
    if ($fl -match "RESTART") { Ok "Windows restarts it if it dies" } else { Warn "no restart-on-failure action" "re-run install-service.ps1" }
    $nssm = Get-Command nssm -ErrorAction SilentlyContinue
    if ($nssm) {
        $kill = (nssm get $ServiceName AppKillProcessTree 2>$null | Out-String).Trim()
        if ($kill -eq "0") { Ok "a bot restart leaves DCS running (AppKillProcessTree 0)" }
        else { Warn "a bot restart kills DCS too (AppKillProcessTree $kill)" "nssm set $ServiceName AppKillProcessTree 0" }
    }
}

Write-Host "`nCrash / reboot behaviour" -ForegroundColor Cyan
$ar = (Get-ItemProperty "HKLM:\SYSTEM\CurrentControlSet\Control\CrashControl" -ErrorAction SilentlyContinue).AutoReboot
if ($ar -eq 1) { Ok "Windows reboots by itself after a blue screen" }
else { Bad "Windows will sit on the blue screen instead of rebooting" "System Properties -> Advanced -> Startup and Recovery -> tick 'Automatically restart'" }
$pending = (Test-Path "HKLM:\SOFTWARE\Microsoft\Windows\CurrentVersion\WindowsUpdate\Auto Update\RebootRequired") -or
           (Test-Path "HKLM:\SOFTWARE\Microsoft\Windows\CurrentVersion\Component Based Servicing\RebootPending")
if ($pending) { Warn "a Windows Update reboot is pending -- it will happen on Windows' schedule" "set Active Hours around play time, or reboot now in a quiet window" }
else { Ok "no reboot pending" }
$ah = Get-ItemProperty "HKLM:\SOFTWARE\Microsoft\WindowsUpdate\UX\Settings" -ErrorAction SilentlyContinue
if ($ah -and $ah.ActiveHoursStart -ne $null) { Ok "Windows Update active hours: $($ah.ActiveHoursStart):00-$($ah.ActiveHoursEnd):00 (no forced reboots inside)" }
$boot = (Get-CimInstance Win32_OperatingSystem).LastBootUpTime
Ok "last boot $boot"
$ev = Get-WinEvent -FilterHashtable @{ LogName = "System"; Id = 41, 6008 } -MaxEvents 3 -ErrorAction SilentlyContinue
foreach ($e in $ev) { Warn "unexpected shutdown recorded $($e.TimeCreated) (event $($e.Id))" }
Write-Host "  [INFO] power loss: set the BIOS/UEFI 'Restore on AC power loss' to Power On -- no way to check that from Windows" -ForegroundColor DarkGray

Write-Host "`nStack" -ForegroundColor Cyan
try {
    $r = Invoke-WebRequest "$BfdbUrl/api/health" -UseBasicParsing -TimeoutSec 8
    if ($r.StatusCode -eq 200) { Ok "bfdb answers at $BfdbUrl" } else { Bad "bfdb returned $($r.StatusCode)" }
} catch { Bad "bfdb does not answer at $BfdbUrl ($($_.Exception.Message))" "see the OPS page / procman-bfdb-boot.log" }
try {
    $v = Invoke-RestMethod "$BfdbUrl/api/version" -TimeoutSec 8
    Ok "bfdb build $($v.git) built $($v.built)"
} catch { }
$tcp = New-Object Net.Sockets.TcpClient
try { $tcp.Connect("127.0.0.1", $NetidxPort); Ok "netidx resolver listening on $NetidxPort" }
catch { Bad "netidx resolver is not listening on $NetidxPort" "netidx-tools on PATH for the service account? see procman-netidx.log" }
finally { $tcp.Dispose() }
$dcs = Get-Process DCS, DCS_server -ErrorAction SilentlyContinue
if ($dcs) { Ok "$($dcs.Count) DCS process(es) running" } else { Warn "no DCS process running (fine if the schedule has them down right now)" }
$bf = Get-Process bfdb -ErrorAction SilentlyContinue
if (($bf | Measure-Object).Count -gt 1) { Warn "$($bf.Count) bfdb.exe processes -- only one should run" "is bfsystem.ps1 also running? stop it; the bot owns bfdb" }

Write-Host ""
if ($fail) { Write-Host "$fail problem(s), $warn warning(s)." -ForegroundColor Red; exit 1 }
elseif ($warn) { Write-Host "OK with $warn warning(s)." -ForegroundColor Yellow }
else { Write-Host "All good -- this box comes back by itself." -ForegroundColor Green }
