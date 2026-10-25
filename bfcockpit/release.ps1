# Builds the Vector Strike cockpit plugin and publishes it as GitHub release
# cockpit-v<version>, which installed copies update themselves from
# (bfcockpit_net.dll, `update_now`).
#
#   .\bfcockpit\release.ps1                 # build + sign + publish cockpit-v<version>
#   .\bfcockpit\release.ps1 -DryRun         # build + sign only; everything lands in -OutDir
#
# The three stages can also run separately, so CI keeps the signing key and the
# GitHub token away from the build (which runs cargo build scripts):
#   -DryRun -NoSign     build only
#   -DryRun -NoBuild    sign what the build left in -OutDir
#   -NoBuild -NoSign    publish what is in -OutDir (must be signed)
#
# Bump BFCOCKPIT_VERSION in Scripts/Hooks/bfcockpit.lua first: the tag is
# cockpit-v<that>, releases are never replaced, and the installer never
# installs an older version over a newer one.
#
# Assets: VectorStrike-Cockpit-Install.exe, VectorStrike-Cockpit-<version>.zip
# and a .minisig (a plain minisign signature) for each. bfcockpit_net.dll runs
# a downloaded installer only if its .minisig verifies against the public key
# compiled in from net/updater.pub.
#
# Signing: the COCKPIT release key, a minisign key made by `tauri signer
# generate` -- default %USERPROFILE%\.tauri\fowl-cockpit.key, separate from the
# engine's and the Manager's keys. Its password, if it has one (it should):
# $env:FOWL_COCKPIT_SIGNING_PASSWORD. Its public half goes in net/updater.pub
# BEFORE the build, or the DLL can't check any later update; this script
# refuses to build without it. See README.md, "Releasing".

[CmdletBinding()]
param(
    [string]$Notes = "",
    [string]$Repo = "Dillen198/bfnext",
    [string]$KeyPath = (Join-Path $env:USERPROFILE ".tauri\fowl-cockpit.key"),
    [string]$OutDir = (Join-Path $env:TEMP "fowl-cockpit-release"),
    [string]$TargetDir,
    [switch]$AllowDirty,
    # a key rotation: sign with the OLD key (what installed copies trust) a
    # build whose net/updater.pub already holds the NEW one
    [switch]$Rotating,
    [switch]$DryRun,
    [switch]$NoBuild,
    [switch]$NoSign
)

# "Continue", not "Stop": Windows PowerShell 5.1 turns any stderr line from a
# native tool (cargo's progress, the tauri CLI's "Info") into a terminating
# error under "Stop". Failures are caught by exit code ($LASTEXITCODE) and
# the cmdlets that matter say -ErrorAction Stop themselves.
$ErrorActionPreference = "Continue"
$here = $PSScriptRoot
$repoRoot = (Resolve-Path (Join-Path $here "..")).Path
function Say([string]$m, [string]$c = "Cyan") { Write-Host "==> $m" -ForegroundColor $c }
function Run([string]$what, [scriptblock]$cmd) {
    Say $what
    & $cmd
    if ($LASTEXITCODE -ne 0) { throw "$what failed (exit $LASTEXITCODE)" }
}

$installerName = "VectorStrike-Cockpit-Install.exe"
$pubPath = Join-Path $here "net\updater.pub"
$infoName = "release.json"   # what the build stage hands the later ones; not an asset

# A minisign key or signature file, as it is or base64-wrapped (tauri signer).
function Unwrap-Minisign([string]$s, [string]$what) {
    $s = "$s".Trim()
    if ($s.StartsWith("untrusted comment:")) { return $s }
    try { return [System.Text.Encoding]::UTF8.GetString([Convert]::FromBase64String($s)) }
    catch { throw "$what is neither a minisign file nor base64 of one" }
}
# The key id in a minisign public key or signature (bytes 2..9 of its second
# line), as minisign prints it.
function Get-KeyId([string]$text, [string]$what) {
    $line = ((Unwrap-Minisign $text $what) -split "`r?`n")[1]
    try { $b = [Convert]::FromBase64String("$line".Trim()) } catch { throw "$what has no key line" }
    if ($b.Length -lt 10) { throw "$what has no key id" }
    return (($b[9..2] | ForEach-Object { $_.ToString("X2") }) -join "")
}

# ---- build -------------------------------------------------------------------

if (-not $NoBuild) {
    $lua = Get-Content (Join-Path $here "Scripts\Hooks\bfcockpit.lua") -Raw -ErrorAction Stop
    $version = [regex]::Match($lua, '(?m)^\s*local BFCOCKPIT_VERSION\s*=\s*"([0-9]+(\.[0-9]+){1,3})"').Groups[1].Value
    if (-not $version) { throw "no BFCOCKPIT_VERSION in Scripts/Hooks/bfcockpit.lua" }
    $tag = "cockpit-v$version"
    if (-not $DryRun) {
        if (-not (Get-Command gh -ErrorAction SilentlyContinue)) { throw "gh (GitHub CLI) is not installed" }
        gh release view $tag --repo $Repo 2>$null | Out-Null
        if ($LASTEXITCODE -eq 0) { throw "$tag already exists on $Repo -- bump BFCOCKPIT_VERSION in Scripts/Hooks/bfcockpit.lua" }
    }
    # The DLL in this release checks every LATER update against this key
    $pub = if (Test-Path $pubPath) { (Get-Content $pubPath -Raw).Trim() } else { "" }
    if (-not $pub) {
        throw "bfcockpit/net/updater.pub is empty: put the cockpit release key's public half there and commit it first (README.md, 'Releasing')"
    }
    $keyId = Get-KeyId $pub "net/updater.pub"
    $dirty = git -C $repoRoot status --porcelain -- bfcockpit Cargo.toml Cargo.lock
    if ($dirty -and -not $AllowDirty -and -not $DryRun) {
        throw "uncommitted changes under bfcockpit/ or in Cargo.toml/Cargo.lock -- a release is one commit; commit them or pass -AllowDirty`n$($dirty -join "`n")"
    }
    $commit = (git -C $repoRoot rev-parse HEAD).Trim()
    Say "Vector Strike Cockpit $version from $($commit.Substring(0, 12))$(if ($dirty) { ' (dirty)' }), release key $keyId"

    if (-not $env:LUA_LIB) {   # what setup-build.ps1 sets; lua.lib is at the repo root
        $env:LUA_LIB = $repoRoot; $env:LUA_LINK = "dylib"; $env:LUA_LIB_NAME = "lua"
    }
    if (-not $TargetDir) { $TargetDir = if ($env:CARGO_TARGET_DIR) { $env:CARGO_TARGET_DIR } else { Join-Path $repoRoot "target" } }
    $env:CARGO_TARGET_DIR = $TargetDir
    $dll = Join-Path $TargetDir "release\bfcockpit_net.dll"
    Push-Location $repoRoot
    try {
        # the DLL first: the installer embeds it (installer/build.rs)
        Run "cargo build --release --locked -p bfcockpit-net" { cargo build --release --locked -p bfcockpit-net }
        $env:BFCOCKPIT_NET_DLL = $dll
        Run "cargo build --release --locked -p bfcockpit-installer" { cargo build --release --locked -p bfcockpit-installer }
    } finally {
        Pop-Location
        Remove-Item Env:\BFCOCKPIT_NET_DLL -ErrorAction SilentlyContinue
    }

    if (Test-Path $OutDir) { Remove-Item -Recurse -Force $OutDir }
    New-Item -ItemType Directory -Force $OutDir -ErrorAction Stop | Out-Null
    Copy-Item (Join-Path $TargetDir "release\bfcockpit-install.exe") (Join-Path $OutDir $installerName) -ErrorAction Stop

    # The same files for installing by hand: a DCS profile's layout, forward-
    # slash entry names (PowerShell 5's Compress-Archive writes '\')
    Add-Type -AssemblyName System.IO.Compression, System.IO.Compression.FileSystem
    $zipName = "VectorStrike-Cockpit-$version.zip"
    $zip = [System.IO.Compression.ZipFile]::Open((Join-Path $OutDir $zipName), "Create")
    try {
        $add = { param($from, $entry) [System.IO.Compression.ZipFileExtensions]::CreateEntryFromFile($zip, $from, $entry) | Out-Null }
        & $add (Join-Path $here "Scripts\Hooks\bfcockpit.lua") "Scripts/Hooks/bfcockpit.lua"
        $plugin = Join-Path $here "Mods\Services\BFCockpit"
        Get-ChildItem $plugin -Recurse -File | ForEach-Object {
            & $add $_.FullName ("Mods/Services/BFCockpit/" + $_.FullName.Substring($plugin.Length + 1).Replace("\", "/"))
        }
        & $add $dll "Mods/Services/BFCockpit/bin/bfcockpit_net.dll"
    } finally { $zip.Dispose() }

    $info = [ordered]@{ version = $version; tag = $tag; commit = $commit; dirty = [bool]$dirty; key_id = $keyId
                        assets = @($installerName, $zipName) }
    [System.IO.File]::WriteAllText((Join-Path $OutDir $infoName), ($info | ConvertTo-Json),
                                   (New-Object System.Text.UTF8Encoding $false))
    foreach ($a in $info.assets) {
        $f = Get-Item (Join-Path $OutDir $a)
        Say ("  {0,-36} {1,6:N1} MB  sha256 {2}" -f $a, ($f.Length / 1MB), (Get-FileHash $f.FullName).Hash.Substring(0, 12).ToLower()) "Gray"
    }
}

$infoPath = Join-Path $OutDir $infoName
if (-not (Test-Path $infoPath)) { throw "no $infoName in $OutDir -- run the build stage first (drop -NoBuild)" }
$info = Get-Content $infoPath -Raw -ErrorAction Stop | ConvertFrom-Json
$assets = @($info.assets | ForEach-Object { Join-Path $OutDir $_ })
foreach ($a in $assets) { if (-not (Test-Path $a)) { throw "$a is missing -- run the build stage again" } }
if ($NoSign -and $DryRun) { Say "built $($info.tag) in $OutDir (not signed: -NoSign)" "Green"; return }

# ---- sign ----------------------------------------------------------------------

if (-not $NoSign) {
    if (-not (Test-Path $KeyPath)) { throw "signing key not found at $KeyPath (pass -KeyPath; see the header of this script)" }
    if (Test-Path "$KeyPath.pub") {
        $id = Get-KeyId (Get-Content "$KeyPath.pub" -Raw) "$KeyPath.pub"
        if ($id -ne $info.key_id -and -not $Rotating) {
            throw "$KeyPath is key $id, but this build's DLL trusts $($info.key_id) (net/updater.pub) -- pass -Rotating if that is the point"
        }
    }
    # `tauri signer` from bfmanager's own dev dependencies: the same tool (and
    # key format) as the engine's and the Manager's releases.
    $mgr = Join-Path $repoRoot "bfmanager"
    if (-not (Test-Path (Join-Path $mgr "node_modules\.bin\tauri.cmd"))) {
        Push-Location $mgr
        try { Run "npm ci (bfmanager, for tauri signer)" { npm ci --no-audit --no-fund --ignore-scripts } } finally { Pop-Location }
    }
    # The password travels in the environment the signer reads, never on a
    # command line; a key without one gets "--password=" so the signer doesn't
    # sit on a hidden prompt.
    $pwArgs = @()
    if ("$env:FOWL_COCKPIT_SIGNING_PASSWORD") { $env:TAURI_SIGNING_PRIVATE_KEY_PASSWORD = $env:FOWL_COCKPIT_SIGNING_PASSWORD }
    else { $pwArgs = @("--password=") }
    Push-Location $mgr
    try {
        foreach ($a in $assets) {
            Remove-Item "$a.sig", "$a.minisig" -ErrorAction SilentlyContinue
            Run "sign $(Split-Path $a -Leaf)" { npx tauri signer sign --private-key-path "$KeyPath" @pwArgs "$a" }
            if (-not (Test-Path "$a.sig")) { throw "tauri signer wrote no $a.sig" }
            # tauri signer writes base64 of the signature file; publish it plain,
            # so stock `minisign -Vm` can check it too
            $plain = Unwrap-Minisign (Get-Content "$a.sig" -Raw) "$a.sig"
            [System.IO.File]::WriteAllText("$a.minisig", "$plain`n", (New-Object System.Text.UTF8Encoding $false))
            Remove-Item "$a.sig" -ErrorAction Stop
        }
    } finally {
        Pop-Location
        Remove-Item Env:\TAURI_SIGNING_PRIVATE_KEY_PASSWORD -ErrorAction SilentlyContinue
    }
}
foreach ($a in $assets) {
    if (-not (Test-Path "$a.minisig")) { throw "no $(Split-Path $a -Leaf).minisig -- sign it first (drop -NoSign)" }
    $id = Get-KeyId (Get-Content "$a.minisig" -Raw) "$a.minisig"
    if ($id -ne $info.key_id) {
        if (-not $Rotating) { throw "$(Split-Path $a -Leaf).minisig is by key $id, not $($info.key_id) (net/updater.pub) -- pass -Rotating if that is the point" }
        Say "$(Split-Path $a -Leaf): signed by $id; this build's DLL trusts $($info.key_id) from now on" "Yellow"
    }
    $signedBy = $id
}
Say "signed with key $signedBy" "Green"

if ($DryRun) { Say "dry run -- nothing published. Output: $OutDir" "Yellow"; return }

# ---- publish ---------------------------------------------------------------------

if ($info.dirty -and -not $AllowDirty) { throw "$OutDir was built from a dirty tree -- rebuild from a commit, or pass -AllowDirty" }
if (-not (Get-Command gh -ErrorAction SilentlyContinue)) { throw "gh (GitHub CLI) is not installed" }
if (-not $Notes) { $Notes = "Vector Strike Cockpit $($info.version)`n`nBuilt from $($info.commit.Substring(0, 12))." }
# The notes go through a file: Windows PowerShell 5.1 passes a native
# argument's embedded double quotes unescaped, and gh then takes the pieces for
# asset files.
$notesFile = Join-Path $OutDir "release-notes.md"
[System.IO.File]::WriteAllText($notesFile, $Notes, (New-Object System.Text.UTF8Encoding $false))
$files = @()
foreach ($a in $assets) { $files += $a, "$a.minisig" }
# --target: without it gh tags the default branch (main), not the commit this
# was built from. That commit has to be on the remote.
$ghArgs = @("release", "create", $info.tag) + $files + @("--repo", $Repo, "--target", $info.commit,
            "--title", "Vector Strike Cockpit $($info.version)", "--notes-file", $notesFile)
Run "gh release create $($info.tag)" { gh @ghArgs }
Say "published https://github.com/$Repo/releases/tag/$($info.tag)" "Green"
Say "players update once a server's bfdb ships this version (/api/cockpit/plugin/version)" "Green"
