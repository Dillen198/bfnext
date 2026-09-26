# Builds the Fowl Engine binaries and publishes them as an engine release that
# every server's auto-updater picks up (DCSServerBot/plugins/fowlengine/
# autoupdate.py). See deploy/auto-update.md.
#
#   .\deploy\publish-release.ps1                        # build HEAD, publish to GitHub
#   .\deploy\publish-release.ps1 -Channel beta -Notes "try the new CAP"
#   .\deploy\publish-release.ps1 -Folder \\server\fowl-releases   # LAN folder source
#   .\deploy\publish-release.ps1 -DryRun                # build + manifest only, publish nothing
#
# A release is always ONE COMMIT: by default the build runs in a fresh
# `git worktree` of -Ref (HEAD), so whatever else is half-edited in this tree
# can't leak into it -- commit (and push, for GitHub) what you want shipped
# first. -WorkingTree builds the tree as it is instead (the binaries then
# report "<rev>-dirty"); it needs -AllowDirty to publish.
#
# Needs: git, cargo (the MSVC toolchain), node/npm (bfdb embeds the dashboard
# and the site), and gh (GitHub CLI, logged in) unless -Folder is used.

[CmdletBinding()]
param(
    [string]$Ref = "HEAD",
    [string]$Tag,
    [ValidateSet("stable", "beta")][string]$Channel = "stable",
    [string]$Notes = "",
    [string[]]$Files = @("bflib.dll", "bfdb.exe", "bftools.exe", "bfrange.dll"),
    [switch]$WorkingTree,
    [switch]$AllowDirty,
    [string]$Folder,
    [string]$Repo = "Dillen198/bfnext",
    [string]$TagPrefix = "engine-",
    [switch]$BotPlugin,
    [switch]$SkipBuild,
    [switch]$DryRun,
    [string]$TargetDir = (Join-Path $env:LOCALAPPDATA "fowl-release\target"),
    [string]$OutDir
)

# "Continue", not "Stop": Windows PowerShell 5.1 turns any stderr line from a
# native tool (cargo's progress, the tauri CLI's "Info") into a terminating
# error under "Stop". Failures are caught by exit code ($LASTEXITCODE) and
# the cmdlets that matter say -ErrorAction Stop themselves.
$ErrorActionPreference = "Continue"
$repoRoot = (Resolve-Path (Join-Path $PSScriptRoot "..")).Path

function Say([string]$msg, [string]$color = "Cyan") { Write-Host "==> $msg" -ForegroundColor $color }
function Run([string]$what, [scriptblock]$cmd) {
    Say $what
    & $cmd
    if ($LASTEXITCODE -ne 0) { throw "$what failed (exit $LASTEXITCODE)" }
}

# ---- what are we building ------------------------------------------------------

$commit = (git -C $repoRoot rev-parse $Ref).Trim()
if ($LASTEXITCODE -ne 0 -or -not $commit) { throw "cannot resolve -Ref $Ref" }
$short = $commit.Substring(0, 12)
if (-not $Tag) { $Tag = "$TagPrefix$(Get-Date -Format 'yyyy.MM.dd-HHmm')-$($short.Substring(0, 7))" }
if ($TagPrefix -and -not $Tag.StartsWith($TagPrefix)) {
    throw "tag $Tag does not start with $TagPrefix -- the servers only look at $TagPrefix* tags"
}
$dirty = $false
if ($WorkingTree) {
    $dirty = [bool](git -C $repoRoot status --porcelain)
    if ($dirty -and -not $AllowDirty -and -not $DryRun) {
        throw "the working tree has uncommitted changes; commit them, drop -WorkingTree, or pass -AllowDirty"
    }
}
$gitLabel = if ($dirty) { "$short-dirty" } else { $short }
if (-not $OutDir) { $OutDir = Join-Path $env:TEMP "fowl-release-out\$Tag" }

Say "release $Tag  ($Channel)  from $gitLabel$(if ($WorkingTree) { ' [working tree]' } else { " [worktree of $Ref]" })"

# ---- build ---------------------------------------------------------------------------

$src = $repoRoot
$worktree = $null
try {
    if (-not $WorkingTree -and -not $SkipBuild) {
        $worktree = Join-Path $env:TEMP "fowl-release-src-$([guid]::NewGuid().ToString('N').Substring(0, 8))"
        Run "git worktree add $worktree ($short)" { git -C $repoRoot worktree add --detach $worktree $commit }
        $src = $worktree
        # Cargo.lock is gitignored: without these the worktree would resolve
        # every dependency afresh and ship something the dev tree never ran
        foreach ($lock in @("Cargo.lock", "bftools\Cargo.lock")) {
            if (Test-Path (Join-Path $repoRoot $lock)) { Copy-Item (Join-Path $repoRoot $lock) (Join-Path $worktree $lock) -ErrorAction Stop }
        }
    }

    $want = @{}
    foreach ($f in $Files) { $want[$f.ToLower()] = $true }
    $members = (Get-Content (Join-Path $src "Cargo.toml") -Raw -ErrorAction Stop)
    if ($want["bfrange.dll"] -and ($members -notmatch '"bfrange"' -or -not (Test-Path (Join-Path $src "bfrange\Cargo.toml")))) {
        Say "bfrange is not a workspace member at $short -- leaving bfrange.dll out" "Yellow"
        $want.Remove("bfrange.dll")
    }

    if (-not $SkipBuild) {
        $env:LUA_LIB = $src          # lua.lib is checked in at the repo root
        $env:LUA_LINK = "dylib"
        $env:LUA_LIB_NAME = "lua"
        $env:CARGO_TARGET_DIR = $TargetDir

        if ($want["bfdb.exe"]) {
            # bfdb embeds bfweb/dist and bfsite/dist at compile time
            foreach ($app in @("bfweb", "bfsite")) {
                Push-Location (Join-Path $src $app)
                try {
                    Run "npm ci ($app)" { npm ci --no-audit --no-fund }
                    Run "npm run build ($app)" { npm run build }
                } finally { Pop-Location }
            }
        }
        $pkgs = @()
        if ($want["bflib.dll"]) { $pkgs += "-p", "bflib" }
        if ($want["bfrange.dll"]) { $pkgs += "-p", "bfrange" }
        if ($want["bfdb.exe"]) { $pkgs += "-p", "bfdb" }
        if ($pkgs.Count) {
            Push-Location $src
            try { Run "cargo build --release $($pkgs -join ' ')" { cargo build --release @pkgs } } finally { Pop-Location }
        }
        if ($want["bftools.exe"]) {
            $env:CARGO_TARGET_DIR = "$TargetDir-bftools"
            Push-Location (Join-Path $src "bftools")
            try { Run "cargo build --release (bftools)" { cargo build --release } } finally { Pop-Location }
            $env:CARGO_TARGET_DIR = $TargetDir
        }
    }

    # ---- collect + manifest ---------------------------------------------------------

    if (Test-Path $OutDir) { Remove-Item -Recurse -Force $OutDir }
    New-Item -ItemType Directory -Force $OutDir -ErrorAction Stop | Out-Null
    $built = (Get-Date).ToUniversalTime().ToString("yyyy-MM-ddTHH:mm:ssZ")
    $manifestFiles = [ordered]@{}
    foreach ($name in @("bflib.dll", "bfrange.dll", "bfdb.exe", "bftools.exe")) {
        if (-not $want[$name]) { continue }
        $from = if ($name -eq "bftools.exe") {
            @("$TargetDir-bftools\release\bftools.exe", (Join-Path $src "bftools\target\release\bftools.exe")) |
                Where-Object { Test-Path $_ } | Select-Object -First 1
        } else { Join-Path $TargetDir "release\$name" }
        if (-not $from -or -not (Test-Path $from)) { throw "$name was not built (looked for $from)" }
        Copy-Item $from (Join-Path $OutDir $name) -ErrorAction Stop
        $item = Get-Item (Join-Path $OutDir $name)
        $manifestFiles[$name] = [ordered]@{
            sha256 = (Get-FileHash $item.FullName -Algorithm SHA256).Hash.ToLower()
            size   = $item.Length
            # with -SkipBuild nobody knows which commit produced the file
            git    = if ($SkipBuild) { $null } else { $gitLabel }
            built  = $item.LastWriteTimeUtc.ToString("yyyy-MM-ddTHH:mm:ssZ")
        }
        Say ("  {0,-12} {1,8:N1} MB  {2}" -f $name, ($item.Length / 1MB), $manifestFiles[$name].sha256.Substring(0, 12)) "Gray"
    }

    if ($BotPlugin) {
        # forward-slash entry names (PowerShell 5's Compress-Archive writes '\',
        # which the updater rightly refuses)
        Add-Type -AssemblyName System.IO.Compression, System.IO.Compression.FileSystem
        $zipPath = Join-Path $OutDir "fowlengine-bot.zip"
        $zip = [System.IO.Compression.ZipFile]::Open($zipPath, "Create")
        try {
            $botRoot = Join-Path $src "DCSServerBot"
            $dirs = @("plugins\fowlengine") + (Get-ChildItem (Join-Path $botRoot "extensions") -Directory -Filter "bf*" |
                ForEach-Object { "extensions\$($_.Name)" })
            foreach ($d in $dirs) {
                Get-ChildItem (Join-Path $botRoot $d) -Recurse -File |
                    Where-Object { $_.FullName -notmatch '\\__pycache__\\' -and $_.Name -ne "fowlengine.yaml" } |
                    ForEach-Object {
                        $rel = $_.FullName.Substring($botRoot.Length + 1).Replace("\", "/")
                        [System.IO.Compression.ZipFileExtensions]::CreateEntryFromFile($zip, $_.FullName, $rel) | Out-Null
                    }
            }
        } finally { $zip.Dispose() }
        $zi = Get-Item $zipPath
        $manifestFiles["fowlengine-bot.zip"] = [ordered]@{
            sha256 = (Get-FileHash $zipPath -Algorithm SHA256).Hash.ToLower(); size = $zi.Length; git = $gitLabel; built = $built
        }
        Say "  fowlengine-bot.zip (bot plugin)" "Gray"
    }
    if ($manifestFiles.Count -eq 0) { throw "nothing to release" }

    $subject = (git -C $repoRoot log -1 --format=%s $commit).Trim()
    if (-not $Notes) { $Notes = "$subject`n`nBuilt from $gitLabel." }
    $manifest = [ordered]@{
        schema  = 1
        tag     = $Tag
        git     = $gitLabel
        commit  = $commit
        built   = $built
        channel = $Channel
        notes   = $Notes
        files   = $manifestFiles
    }
    $manifestPath = Join-Path $OutDir "manifest.json"
    # UTF-8 without a BOM: a BOM makes some JSON readers choke
    [System.IO.File]::WriteAllText($manifestPath, ($manifest | ConvertTo-Json -Depth 5), (New-Object System.Text.UTF8Encoding $false))
    Say "manifest written: $manifestPath"

    # ---- publish -------------------------------------------------------------------

    if ($DryRun) {
        Say "dry run -- nothing published. Output: $OutDir" "Yellow"
        return
    }
    $assets = Get-ChildItem $OutDir -File | ForEach-Object { $_.FullName }
    if ($Folder) {
        $dest = Join-Path $Folder $Tag
        if (Test-Path $dest) { throw "$dest already exists -- releases are immutable, pick a new -Tag" }
        New-Item -ItemType Directory -Force $dest -ErrorAction Stop | Out-Null
        # manifest last, so a server scanning the folder never sees a half-copied release
        Get-ChildItem $OutDir -File | Where-Object Name -ne "manifest.json" | Copy-Item -Destination $dest -ErrorAction Stop
        Copy-Item $manifestPath $dest -ErrorAction Stop
        Say "published to $dest" "Green"
    } else {
        if (-not (Get-Command gh -ErrorAction SilentlyContinue)) { throw "gh (GitHub CLI) is not installed -- or use -Folder" }
        if (-not $WorkingTree -or -not $dirty) {
            $onRemote = git -C $repoRoot branch -r --contains $commit 2>$null
            if (-not $onRemote) { throw "commit $short is not pushed to any remote branch -- push it first (the release tag points at it)" }
        }
        $ghArgs = @("release", "create", $Tag) + $assets + @("--repo", $Repo, "--title", $Tag, "--notes", $Notes, "--target", $commit)
        if ($Channel -eq "beta") { $ghArgs += "--prerelease" }
        Run "gh release create $Tag" { gh @ghArgs }
        Say "published https://github.com/$Repo/releases/tag/$Tag" "Green"
    }
    Say "servers with autoupdate enabled pick it up on their next check (or: OPS page -> Check now, /feops update_check)" "Green"
}
finally {
    if ($worktree) {
        Pop-Location -ErrorAction SilentlyContinue
        git -C $repoRoot worktree remove --force $worktree 2>$null | Out-Null
    }
}
