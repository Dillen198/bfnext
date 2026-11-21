<#
.SYNOPSIS
    Opt-in scheduled backup of DCSServerBot's PostgreSQL database and its
    config\ folder (live secrets), encrypted, to an off-box path.

.DESCRIPTION
    Fowl Engine Manager's BACKUP page makes a whole-box zip when someone asks
    for one. This script is the unattended part: run it from Task Scheduler
    (-Install does that for you) and every night it writes

        <Destination>\FowlBot-<PC>-<yyyyMMdd-HHmmss>.fowlbak

    holding
        db\<database>.dump      pg_dump -Fc of the bot's database
        config\...              DCSServerBot's config folder, .secret included
        backup.txt              what was taken, from where, with which pg_dump

    The .fowlbak is a zip encrypted with a passphrase (AES-256-CBC, keys from
    PBKDF2-SHA256, HMAC-SHA256 over the whole file -- format below), so it can
    go to a NAS, a synced folder or another PC without exposing the Discord
    token or database password. -Decrypt turns one back into the zip on any
    Windows PC with this script; nothing else is needed.

    The unencrypted staging copy lives in a fresh folder under the running
    account's TEMP and is deleted when the run ends, whether it worked or not.
    Old .fowlbak files in Destination are pruned after -KeepDays.

    Nothing runs until you install it: there is no default schedule.

    Database settings come from the bot's config\nodes.yaml (database.url), and
    the password from config\.secret\database.pkl when the URL says SECRET --
    the same places the bot and Fowl Engine Manager read them. PGPASSWORD or a
    pgpass file (PGPASSFILE) in the environment take precedence.

    File format (all integers little-endian):
        8   magic "FWLBAK1" + 0x00
        16  salt
        4   PBKDF2 iterations (uint32)
        16  IV
        n   AES-256-CBC (PKCS7) ciphertext of the zip
        32  HMAC-SHA256 of everything above
    PBKDF2-SHA256(passphrase as UTF-8, salt, iterations) gives 64 bytes:
    the first 32 are the AES key, the last 32 the HMAC key.

.PARAMETER BotDir
    DCSServerBot's folder (the one holding config\ and run.cmd).

.PARAMETER Destination
    Where the .fowlbak files go. Should be off this box: a share
    (\\nas\fowl-backups), a mapped/synced folder, or a removable drive.

.PARAMETER PassphraseFile
    A file whose first line is the passphrase. Keep it readable only by the
    account the task runs as (and Administrators), and keep a copy of the
    passphrase somewhere that is NOT this box -- without it the backups
    cannot be opened.

.PARAMETER Install
    Register a daily Task Scheduler task ("FowlEngine bot backup", as SYSTEM
    by default) that runs this script with the same parameters.

.PARAMETER Decrypt
    A .fowlbak to turn back into a zip (written to -OutFile, default: next to
    it with .zip). Checks the HMAC first: a wrong passphrase or a damaged file
    is refused before anything is written.

.EXAMPLE
    # one run by hand (as Administrator)
    .\backup-bot.ps1 -BotDir C:\DCSServerBot -Destination \\nas\fowl-backups -PassphraseFile C:\ProgramData\FowlEngine\backup-passphrase.txt

.EXAMPLE
    # every night at 04:30
    .\backup-bot.ps1 -Install -At 04:30 -BotDir C:\DCSServerBot -Destination \\nas\fowl-backups -PassphraseFile C:\ProgramData\FowlEngine\backup-passphrase.txt

.EXAMPLE
    # on the PC you restore to
    .\backup-bot.ps1 -Decrypt E:\FowlBot-VS1-20261025-043000.fowlbak -PassphraseFile .\passphrase.txt

.NOTES
    Restore steps: deploy/backups/README.md.
#>
[CmdletBinding(DefaultParameterSetName = 'Backup')]
param(
    [Parameter(ParameterSetName = 'Backup', Mandatory)]
    [Parameter(ParameterSetName = 'Install', Mandatory)]
    [string]$BotDir,

    [Parameter(ParameterSetName = 'Backup', Mandatory)]
    [Parameter(ParameterSetName = 'Install', Mandatory)]
    [string]$Destination,

    [Parameter(ParameterSetName = 'Backup', Mandatory)]
    [Parameter(ParameterSetName = 'Install', Mandatory)]
    [Parameter(ParameterSetName = 'Decrypt', Mandatory)]
    [string]$PassphraseFile,

    [Parameter(ParameterSetName = 'Backup')]
    [Parameter(ParameterSetName = 'Install')]
    [string]$PgDump,

    # nodes.yaml block to read database.url from; default: the first one with a database
    [Parameter(ParameterSetName = 'Backup')]
    [Parameter(ParameterSetName = 'Install')]
    [string]$Node,

    [Parameter(ParameterSetName = 'Backup')]
    [Parameter(ParameterSetName = 'Install')]
    [int]$KeepDays = 30,

    [Parameter(ParameterSetName = 'Backup')]
    [Parameter(ParameterSetName = 'Install')]
    [switch]$NoDatabase,

    [Parameter(ParameterSetName = 'Backup')]
    [Parameter(ParameterSetName = 'Install')]
    [switch]$NoConfig,

    [Parameter(ParameterSetName = 'Install', Mandatory)]
    [switch]$Install,

    [Parameter(ParameterSetName = 'Install')]
    [string]$At = '04:30',

    # Run the task as this account instead of SYSTEM (e.g. one that can write
    # the share). Task Scheduler asks for its password.
    [Parameter(ParameterSetName = 'Install')]
    [string]$TaskUser,

    [Parameter(ParameterSetName = 'Uninstall', Mandatory)]
    [switch]$Uninstall,

    [Parameter(ParameterSetName = 'Decrypt', Mandatory)]
    [string]$Decrypt,

    [Parameter(ParameterSetName = 'Decrypt')]
    [string]$OutFile
)

Set-StrictMode -Version 3
$ErrorActionPreference = 'Stop'

$TaskName = 'FowlEngine bot backup'
$Magic = [byte[]](0x46, 0x57, 0x4C, 0x42, 0x41, 0x4B, 0x31, 0x00)   # "FWLBAK1\0"
$Iterations = 600000
$HeaderLen = 8 + 16 + 4 + 16
$MacLen = 32

function Read-Passphrase([string]$Path) {
    if (-not (Test-Path -LiteralPath $Path -PathType Leaf)) { throw "passphrase file not found: $Path" }
    $p = (Get-Content -LiteralPath $Path -TotalCount 1 -Encoding UTF8)
    if ($null -eq $p -or $p.Trim().Length -lt 12) { throw "the passphrase in $Path is empty or shorter than 12 characters" }
    return $p.Trim()
}

function Get-Keys([string]$Passphrase, [byte[]]$Salt, [int]$Iter) {
    $kdf = New-Object System.Security.Cryptography.Rfc2898DeriveBytes(
        [Text.Encoding]::UTF8.GetBytes($Passphrase), $Salt, $Iter,
        [System.Security.Cryptography.HashAlgorithmName]::SHA256)
    try { $k = $kdf.GetBytes(64) } finally { $kdf.Dispose() }
    return @{ Enc = [byte[]]($k[0..31]); Mac = [byte[]]($k[32..63]) }
}

function New-Aes([byte[]]$Key, [byte[]]$IV) {
    $aes = [System.Security.Cryptography.Aes]::Create()
    $aes.KeySize = 256
    $aes.Mode = [System.Security.Cryptography.CipherMode]::CBC
    $aes.Padding = [System.Security.Cryptography.PaddingMode]::PKCS7
    $aes.Key = $Key
    $aes.IV = $IV
    return $aes
}

# HMAC-SHA256 of the first $Length bytes of a file, streamed.
function Get-FileMac([string]$Path, [long]$Length, [byte[]]$Key) {
    $h = New-Object System.Security.Cryptography.HMACSHA256(, $Key)
    $fs = [IO.File]::OpenRead($Path)
    try {
        $buf = New-Object byte[] (1MB)
        $left = $Length
        while ($left -gt 0) {
            $n = $fs.Read($buf, 0, [int][Math]::Min([long]$buf.Length, $left))
            if ($n -le 0) { throw "$Path ended early" }
            [void]$h.TransformBlock($buf, 0, $n, $null, 0)
            $left -= $n
        }
        [void]$h.TransformFinalBlock((New-Object byte[] 0), 0, 0)
        return $h.Hash
    } finally { $fs.Dispose(); $h.Dispose() }
}

function Protect-File([string]$In, [string]$Out, [string]$Passphrase) {
    $rng = [System.Security.Cryptography.RandomNumberGenerator]::Create()
    $salt = New-Object byte[] 16; $rng.GetBytes($salt)
    $iv = New-Object byte[] 16; $rng.GetBytes($iv)
    $rng.Dispose()
    $keys = Get-Keys $Passphrase $salt $Iterations
    $aes = New-Aes $keys.Enc $iv
    $fs = [IO.File]::Open($Out, [IO.FileMode]::CreateNew, [IO.FileAccess]::Write)
    try {
        $fs.Write($Magic, 0, 8)
        $fs.Write($salt, 0, 16)
        $fs.Write([BitConverter]::GetBytes([uint32]$Iterations), 0, 4)
        $fs.Write($iv, 0, 16)
        $cs = New-Object System.Security.Cryptography.CryptoStream($fs, $aes.CreateEncryptor(),
            [System.Security.Cryptography.CryptoStreamMode]::Write)
        $src = [IO.File]::OpenRead($In)
        try { $src.CopyTo($cs, 1MB) } finally { $src.Dispose() }
        $cs.FlushFinalBlock()
        $cs.Dispose()     # closes $fs too
    } finally { $fs.Dispose(); $aes.Dispose() }
    $len = (Get-Item -LiteralPath $Out).Length
    $mac = Get-FileMac $Out $len $keys.Mac
    $fs = [IO.File]::Open($Out, [IO.FileMode]::Append, [IO.FileAccess]::Write)
    try { $fs.Write($mac, 0, $MacLen) } finally { $fs.Dispose() }
}

function Unprotect-File([string]$In, [string]$Out, [string]$Passphrase) {
    $total = (Get-Item -LiteralPath $In).Length
    if ($total -lt $HeaderLen + 16 + $MacLen) { throw "$In is too short to be a .fowlbak" }
    $fs = [IO.File]::OpenRead($In)
    try {
        $hdr = New-Object byte[] $HeaderLen
        if ($fs.Read($hdr, 0, $HeaderLen) -ne $HeaderLen) { throw "$In ended early" }
        $stored = New-Object byte[] $MacLen
        [void]$fs.Seek($total - $MacLen, [IO.SeekOrigin]::Begin)
        if ($fs.Read($stored, 0, $MacLen) -ne $MacLen) { throw "$In ended early" }
    } finally { $fs.Dispose() }
    for ($i = 0; $i -lt 8; $i++) { if ($hdr[$i] -ne $Magic[$i]) { throw "$In is not a .fowlbak (bad magic)" } }
    $salt = [byte[]]($hdr[8..23])
    $iter = [BitConverter]::ToUInt32($hdr, 24)
    $iv = [byte[]]($hdr[28..43])
    $keys = Get-Keys $Passphrase $salt ([int]$iter)
    $got = Get-FileMac $In ($total - $MacLen) $keys.Mac
    $diff = 0
    for ($i = 0; $i -lt $MacLen; $i++) { $diff = $diff -bor ($got[$i] -bxor $stored[$i]) }
    if ($diff -ne 0) { throw "wrong passphrase, or $In is damaged (HMAC mismatch) -- nothing written" }

    $aes = New-Aes $keys.Enc $iv
    $dec = $aes.CreateDecryptor()
    $src = [IO.File]::OpenRead($In)
    $dst = [IO.File]::Open($Out, [IO.FileMode]::CreateNew, [IO.FileAccess]::Write)
    try {
        [void]$src.Seek($HeaderLen, [IO.SeekOrigin]::Begin)
        $left = $total - $HeaderLen - $MacLen
        $buf = New-Object byte[] (1MB)
        $obuf = New-Object byte[] (1MB + 32)
        while ($left -gt 0) {
            # fill the buffer completely (CBC wants whole 16-byte blocks)
            $want = [int][Math]::Min([long]$buf.Length, $left)
            $n = 0
            while ($n -lt $want) {
                $r = $src.Read($buf, $n, $want - $n)
                if ($r -le 0) { throw "$In ended early" }
                $n += $r
            }
            $left -= $n
            if ($left -eq 0) {
                $last = $dec.TransformFinalBlock($buf, 0, $n)
                $dst.Write($last, 0, $last.Length)
            } else {
                $w = $dec.TransformBlock($buf, 0, $n, $obuf, 0)
                $dst.Write($obuf, 0, $w)
            }
        }
    } finally { $src.Dispose(); $dst.Dispose(); $dec.Dispose(); $aes.Dispose() }
}

# ---- database settings, the way the bot reads them ------------------------------

function Get-BotDatabase([string]$Bot, [string]$NodeName) {
    $nodes = Join-Path $Bot 'config\nodes.yaml'
    if (-not (Test-Path -LiteralPath $nodes)) { throw "not found: $nodes" }
    $url = $null
    $current = $null
    $inDb = $false
    foreach ($line in Get-Content -LiteralPath $nodes -Encoding UTF8) {
        if ($line -match '^([^\s#][^:]*):\s*$') { $current = $Matches[1].Trim(); $inDb = $false; continue }
        if ($line -match '^\s+database:\s*$') { $inDb = $true; continue }
        if ($inDb -and $line -match '^\s+url:\s*[''"]?(postgres[^\s''"]+)') {
            if (-not $NodeName -or $NodeName -eq $current) { $url = $Matches[1]; break }
        }
    }
    if (-not $url) { throw "no database.url found in $nodes$(if ($NodeName) { " for node $NodeName" })" }
    $u = [Uri]$url
    $parts = @($u.UserInfo -split ':', 2)
    $user = $parts[0]
    $pw = if ($parts.Count -gt 1 -and $parts[1]) { [Uri]::UnescapeDataString($parts[1]) } else { $null }
    if (-not $pw -or $pw -eq 'SECRET') {
        $pw = Read-PickledString (Join-Path $Bot 'config\.secret\database.pkl')
    }
    return @{
        Host = $u.Host; Port = $(if ($u.Port -gt 0) { $u.Port } else { 5432 })
        Name = $u.AbsolutePath.TrimStart('/'); User = [Uri]::UnescapeDataString($user); Password = $pw
    }
}

# The one str in a Python pickle (what DCSServerBot's utils.set_password writes).
function Read-PickledString([string]$Path) {
    if (-not (Test-Path -LiteralPath $Path)) { return $null }
    $b = [IO.File]::ReadAllBytes($Path)
    $i = 0
    while ($i -lt $b.Length) {
        switch ($b[$i]) {
            0x80 { $i += 2; continue }                      # PROTO n
            0x95 { $i += 9; continue }                      # FRAME
            0x8c { $n = $b[$i + 1]; return [Text.Encoding]::UTF8.GetString($b, $i + 2, $n) }   # SHORT_BINUNICODE
            0x58 { $n = [BitConverter]::ToInt32($b, $i + 1); return [Text.Encoding]::UTF8.GetString($b, $i + 5, $n) }  # BINUNICODE
            default { return $null }
        }
    }
    return $null
}

function Find-PgDump {
    $c = Get-ChildItem -Path "$env:ProgramFiles\PostgreSQL\*\bin\pg_dump.exe" -ErrorAction SilentlyContinue |
        Sort-Object { [int]($_.Directory.Parent.Name -replace '\D.*$', '') } -Descending | Select-Object -First 1
    if ($c) { return $c.FullName }
    $cmd = Get-Command pg_dump.exe -ErrorAction SilentlyContinue
    if ($cmd) { return $cmd.Source }
    throw "pg_dump.exe not found -- pass -PgDump <path>"
}

# ---- the modes --------------------------------------------------------------------

if ($PSCmdlet.ParameterSetName -eq 'Uninstall') {
    Unregister-ScheduledTask -TaskName $TaskName -Confirm:$false
    Write-Host "removed task '$TaskName'"
    return
}

if ($PSCmdlet.ParameterSetName -eq 'Decrypt') {
    $pass = Read-Passphrase $PassphraseFile
    if (-not $OutFile) { $OutFile = [IO.Path]::ChangeExtension($Decrypt, '.zip') }
    if (Test-Path -LiteralPath $OutFile) { throw "$OutFile already exists" }
    Unprotect-File $Decrypt $OutFile $pass
    Write-Host "decrypted -> $OutFile"
    return
}

if ($PSCmdlet.ParameterSetName -eq 'Install') {
    [void](Read-Passphrase $PassphraseFile)    # fail now, not at 04:30
    $script = $MyInvocation.MyCommand.Path
    $a = @('-NoProfile', '-ExecutionPolicy', 'Bypass', '-File', "`"$script`"",
           '-BotDir', "`"$BotDir`"", '-Destination', "`"$Destination`"", '-PassphraseFile', "`"$PassphraseFile`"",
           '-KeepDays', $KeepDays)
    if ($PgDump) { $a += @('-PgDump', "`"$PgDump`"") }
    if ($Node) { $a += @('-Node', "`"$Node`"") }
    if ($NoDatabase) { $a += '-NoDatabase' }
    if ($NoConfig) { $a += '-NoConfig' }
    $action = New-ScheduledTaskAction -Execute 'powershell.exe' -Argument ($a -join ' ')
    $trigger = New-ScheduledTaskTrigger -Daily -At $At
    $settings = New-ScheduledTaskSettingsSet -StartWhenAvailable -ExecutionTimeLimit (New-TimeSpan -Hours 2)
    if ($TaskUser) {
        $cred = Get-Credential -UserName $TaskUser -Message "Password for $TaskUser (runs the backup task)"
        Register-ScheduledTask -TaskName $TaskName -Action $action -Trigger $trigger -Settings $settings `
            -User $cred.UserName -Password $cred.GetNetworkCredential().Password -RunLevel Highest -Force | Out-Null
    } else {
        $principal = New-ScheduledTaskPrincipal -UserId 'SYSTEM' -LogonType ServiceAccount -RunLevel Highest
        Register-ScheduledTask -TaskName $TaskName -Action $action -Trigger $trigger -Settings $settings `
            -Principal $principal -Force | Out-Null
    }
    Write-Host "task '$TaskName' runs daily at $At. Start it once now to check: Start-ScheduledTask -TaskName '$TaskName'"
    return
}

# Backup
$pass = Read-Passphrase $PassphraseFile
if (-not (Test-Path -LiteralPath (Join-Path $BotDir 'config'))) { throw "$BotDir has no config folder -- is it DCSServerBot's folder?" }
if (-not (Test-Path -LiteralPath $Destination)) { New-Item -ItemType Directory -Force -Path $Destination | Out-Null }

$stamp = Get-Date -Format 'yyyyMMdd-HHmmss'
$name = "FowlBot-$env:COMPUTERNAME-$stamp"
$work = Join-Path ([IO.Path]::GetTempPath()) "fowl-bot-backup-$([guid]::NewGuid().ToString('N'))"
$stage = Join-Path $work 'stage'
New-Item -ItemType Directory -Force -Path $stage | Out-Null
$log = New-Object System.Collections.Generic.List[string]
$log.Add("Fowl Engine bot backup $stamp on $env:COMPUTERNAME")
$log.Add("bot: $BotDir")
try {
    if (-not $NoDatabase) {
        $db = Get-BotDatabase $BotDir $Node
        if (-not $PgDump) { $PgDump = Find-PgDump }
        New-Item -ItemType Directory -Force -Path (Join-Path $stage 'db') | Out-Null
        $dump = Join-Path $stage "db\$($db.Name).dump"
        $hadPw = Test-Path Env:PGPASSWORD
        if (-not $hadPw -and $db.Password) { $env:PGPASSWORD = $db.Password }
        try {
            & $PgDump -Fc --no-owner --no-privileges -h $db.Host -p $db.Port -U $db.User -d $db.Name -f $dump
            if ($LASTEXITCODE) { throw "pg_dump exited with $LASTEXITCODE" }
        } finally {
            if (-not $hadPw) { Remove-Item Env:PGPASSWORD -ErrorAction SilentlyContinue }
        }
        $ver = (& $PgDump --version) -join ' '
        $log.Add("database: $($db.Name) on $($db.Host):$($db.Port) as $($db.User), $ver, $((Get-Item $dump).Length) bytes")
    }
    if (-not $NoConfig) {
        # robocopy: copies files the bot holds open, keeps hidden .secret
        $cfgOut = Join-Path $stage 'config'
        & robocopy (Join-Path $BotDir 'config') $cfgOut /E /R:2 /W:2 /NFL /NDL /NJH /NJS /NP | Out-Null
        if ($LASTEXITCODE -ge 8) { throw "robocopy of config\ failed ($LASTEXITCODE)" }
        $global:LASTEXITCODE = 0
        $log.Add("config: $((Get-ChildItem -LiteralPath $cfgOut -Recurse -Force -File).Count) files")
    }
    Set-Content -LiteralPath (Join-Path $stage 'backup.txt') -Value $log -Encoding UTF8

    Add-Type -AssemblyName System.IO.Compression.FileSystem
    $zip = Join-Path $work "$name.zip"
    [IO.Compression.ZipFile]::CreateFromDirectory($stage, $zip, [IO.Compression.CompressionLevel]::Optimal, $false)

    # encrypt locally, then copy: a half-written file never sits in Destination
    $enc = Join-Path $work "$name.fowlbak"
    Protect-File $zip $enc $pass
    $partial = Join-Path $Destination "$name.fowlbak.partial"
    Copy-Item -LiteralPath $enc -Destination $partial -Force
    Move-Item -LiteralPath $partial -Destination (Join-Path $Destination "$name.fowlbak") -Force
    Write-Host "backup written: $(Join-Path $Destination "$name.fowlbak") ($((Get-Item $enc).Length) bytes)"

    if ($KeepDays -gt 0) {
        $cut = (Get-Date).AddDays(-$KeepDays)
        Get-ChildItem -LiteralPath $Destination -Filter "FowlBot-$env:COMPUTERNAME-*.fowlbak" -File |
            Where-Object { $_.LastWriteTime -lt $cut } |
            ForEach-Object { Remove-Item -LiteralPath $_.FullName -Force; Write-Host "pruned $($_.Name)" }
    }
} finally {
    Remove-Item -LiteralPath $work -Recurse -Force -ErrorAction SilentlyContinue
}
