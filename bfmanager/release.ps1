# Builds Fowl Engine Manager and publishes it as a GitHub release every
# installed copy updates itself from.
#
#   .\bfmanager\release.ps1                 # build + publish manager-v<version> (stable)
#   .\bfmanager\release.ps1 -Channel beta   # as a pre-release (only beta-channel servers take it)
#   .\bfmanager\release.ps1 -DryRun         # build + sign only; the installer lands in -OutDir
#
# The three stages can also run separately, so CI can keep the signing key
# and the GitHub token out of the build (which runs npm/cargo build scripts):
#   -DryRun -NoSign     build only
#   -DryRun -NoBuild    sign the installer the build left behind
#   -NoBuild -NoSign    publish the already signed installer
#
# Bump `version` in src-tauri/Cargo.toml first: servers only install a
# version NEWER than theirs.
#
# Signing: every installer is signed with the minisign key made by
# `tauri signer generate` (default %USERPROFILE%\.tauri\fowl-manager.key).
# Installed copies carry the matching public key (src-tauri/updater.pub) and
# refuse anything else -- so KEEP THAT KEY: without it no installed copy will
# ever accept an update again. Back it up somewhere safe. Give it a password
# (`tauri signer generate -p ...`) and put that in $env:FOWL_SIGNING_PASSWORD.

[CmdletBinding()]
param(
    [ValidateSet("stable", "beta")][string]$Channel = "stable",
    [string]$Notes = "",
    [string]$Repo = "Dillen198/bfnext",
    [string]$KeyPath = (Join-Path $env:USERPROFILE ".tauri\fowl-manager.key"),
    [string]$OutDir = (Join-Path $env:TEMP "fowl-manager-release"),
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

$cargo = Get-Content (Join-Path $here "src-tauri\Cargo.toml") -Raw -ErrorAction Stop
$version = [regex]::Match($cargo, '(?m)^version\s*=\s*"([^"]+)"').Groups[1].Value
if (-not $version) { throw "no version in src-tauri/Cargo.toml" }
$tag = "manager-v$version"
Say "Fowl Engine Manager $version ($Channel)"

if (-not $DryRun) {
    if (-not (Get-Command gh -ErrorAction SilentlyContinue)) { throw "gh (GitHub CLI) is not installed" }
    $existing = gh release view $tag --repo $Repo 2>$null
    if ($LASTEXITCODE -eq 0) { throw "$tag already exists on $Repo -- bump the version in src-tauri/Cargo.toml" }
}

if (-not $NoSign) {
    if (-not (Test-Path $KeyPath)) { throw "signing key not found at $KeyPath (see the header of this script)" }
    # the public key compiled into the app must match the key we sign with
    $pub = (Get-Content (Join-Path $here "src-tauri\updater.pub") -Raw -ErrorAction Stop).Trim()
    if (Test-Path "$KeyPath.pub") {
        $pubFromKey = (Get-Content "$KeyPath.pub" -Raw -ErrorAction Stop).Trim()
        if ($pubFromKey -ne $pub) { throw "src-tauri/updater.pub does not match $KeyPath.pub -- installed copies would reject this release" }
    }
}
# The key's password, if it has one: FOWL_SIGNING_PASSWORD (blank = none).
$keyPassword = "$env:FOWL_SIGNING_PASSWORD"

if (-not $NoBuild) {
    Push-Location $here
    try {
        if (-not (Test-Path "node_modules")) { npm ci --no-audit --no-fund; if ($LASTEXITCODE) { throw "npm ci failed" } }
        if (-not (Test-Path "..\bfweb\node_modules")) { npm --prefix ..\bfweb ci --no-audit --no-fund; if ($LASTEXITCODE) { throw "npm ci (bfweb) failed" } }
        Say "tauri build (frontend, bot bundle, release exe, NSIS installer)"
        npx tauri build
        if ($LASTEXITCODE) { throw "tauri build failed" }
    } finally { Pop-Location }
}

$nsis = Join-Path $here "src-tauri\target\release\bundle\nsis"
$setup = Get-ChildItem $nsis -Filter "*_$version*-setup.exe" | Select-Object -First 1
if (-not $setup) { throw "no installer for $version in $nsis" }
if ($NoSign -and $DryRun) { Say "built $($setup.FullName) (not signed: -NoSign)" "Green"; return }

if (-not $NoSign) {
    # Sign here rather than in the bundler: its env-var route can't carry an
    # empty password through PowerShell (setting $env:X = "" deletes X) and
    # then sits on a hidden prompt -- so a key WITHOUT a password gets
    # "--password=" (nothing secret on the command line), and a real password
    # goes through the environment variable the signer reads, never the
    # command line.
    Say "signing $($setup.Name)"
    Remove-Item "$($setup.FullName).sig" -ErrorAction SilentlyContinue
    $pwArgs = @()
    if ($keyPassword) { $env:TAURI_SIGNING_PRIVATE_KEY_PASSWORD = $keyPassword } else { $pwArgs = @("--password=") }
    Push-Location $here
    try { npx tauri signer sign --private-key-path "$KeyPath" @pwArgs "$($setup.FullName)" }
    finally {
        Pop-Location
        Remove-Item Env:\TAURI_SIGNING_PRIVATE_KEY_PASSWORD -ErrorAction SilentlyContinue
    }
    if ($LASTEXITCODE) { throw "signing failed" }
}
$sig = Get-Item "$($setup.FullName).sig" -ErrorAction SilentlyContinue
if (-not $sig) { throw "no .sig next to $($setup.Name) -- sign it first (drop -NoSign)" }

if (Test-Path $OutDir) { Remove-Item -Recurse -Force $OutDir }
New-Item -ItemType Directory -Force $OutDir -ErrorAction Stop | Out-Null
Copy-Item $setup.FullName, $sig.FullName $OutDir -ErrorAction Stop
$out = Get-Item (Join-Path $OutDir $setup.Name)
Say ("installer: {0} ({1:N1} MB, sha256 {2})" -f $out.FullName, ($out.Length / 1MB), (Get-FileHash $out.FullName).Hash.Substring(0, 12).ToLower()) "Green"

if ($DryRun) { Say "dry run -- nothing published" "Yellow"; return }

$commit = (git -C $repoRoot rev-parse HEAD).Trim()
if (-not $Notes) { $Notes = "Fowl Engine Manager $version`n`nBuilt from $($commit.Substring(0,12))." }
# --target: without it gh tags the repo's DEFAULT branch (main), not the
# commit this was built from. That commit has to be on the remote.
$ghArgs = @("release", "create", $tag, $out.FullName, (Join-Path $OutDir $sig.Name), "--repo", $Repo,
            "--target", $commit, "--title", "Fowl Engine Manager $version", "--notes", $Notes)
if ($Channel -eq "beta") { $ghArgs += "--prerelease" }
gh @ghArgs
if ($LASTEXITCODE) { throw "gh release create failed" }
Say "published $tag -- installed copies pick it up on their next check" "Green"
