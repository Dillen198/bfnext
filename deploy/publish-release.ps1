# Builds the Fowl Engine binaries and publishes them as an engine release that
# every server's auto-updater picks up (DCSServerBot/plugins/fowlengine/
# autoupdate.py). See deploy/auto-update.md.
#
#   .\deploy\publish-release.ps1                        # build HEAD, publish to GitHub
#   .\deploy\publish-release.ps1 -Channel beta -Notes "try the new CAP"
#   .\deploy\publish-release.ps1 -Folder \\server\fowl-releases   # LAN folder source
#   .\deploy\publish-release.ps1 -DryRun                # build + manifest only, publish nothing
#   .\deploy\publish-release.ps1 -SignOnly -OutDir <d>    # sign what a -DryRun left in <d>
#   .\deploy\publish-release.ps1 -PublishOnly -OutDir <d> -Channel <c>   # publish it (must be signed)
#   .\deploy\publish-release.ps1 -Campaigns deploy\campaigns.json        # + one campaign pack per server
#   .\deploy\publish-release.ps1 -Files @() -Campaigns <json> -SkipBuild # campaign packs only, no build
#
# Campaign packs: -Campaigns names a JSON on THIS PC (deploy/campaigns.sample.json)
# mapping each server's key -- its bfdb instance id, see autoupdate.py -- to its
# campaign config and mission file(s). Each becomes campaign-<key>.zip (flat
# file names) with every file's sha256 in the signed manifest; a cfg that isn't
# valid JSON fails the publish. Servers apply them only with
# autoupdate.campaigns: true (deploy/auto-update.md, "Campaign packs").
#
# A release is always ONE COMMIT: by default the build runs in a fresh
# `git worktree` of -Ref (HEAD), so whatever else is half-edited in this tree
# can't leak into it -- commit (and push, for GitHub) what you want shipped
# first. -WorkingTree builds the tree as it is instead (the binaries then
# report "<rev>-dirty"); it needs -AllowDirty to publish.
#
# Needs: git, cargo (the MSVC toolchain), node/npm (bfdb embeds the dashboard
# and the site), and gh (GitHub CLI, logged in) unless -Folder is used.
#
# Signing: manifest.json (which holds every file's sha256) is signed with the
# ENGINE release key -- a minisign key made by `tauri signer generate`, default
# %USERPROFILE%\.tauri\fowl-engine.key, SEPARATE from Fowl Engine Manager's
# fowl-manager.key -- and published as manifest.json.sig. Servers refuse any
# release that doesn't verify against the public half pinned in their
# fowlengine.yaml (autoupdate.public_key). The key's password, if it has one:
# $env:FOWL_ENGINE_SIGNING_PASSWORD. See deploy/auto-update.md, "Signing releases".

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
    [string]$Campaigns,
    [switch]$SkipBuild,
    [switch]$DryRun,
    [string]$TargetDir = (Join-Path $env:LOCALAPPDATA "fowl-release\target"),
    [string]$OutDir,
    [string]$SigningKey = (Join-Path $env:USERPROFILE ".tauri\fowl-engine.key"),
    [switch]$SignOnly,
    [switch]$PublishOnly
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

# Sign manifest.json -> manifest.json.sig with $SigningKey.
function Sign-Manifest([string]$manifestPath) {
    if (-not (Test-Path $SigningKey)) { throw "no signing key at $SigningKey (pass -SigningKey)" }
    # `tauri signer` from bfmanager's own dev dependencies: the same tool (and
    # key format) the manager's releases are signed with.
    $mgr = Join-Path $repoRoot "bfmanager"
    if (-not (Test-Path (Join-Path $mgr "node_modules\.bin\tauri.cmd"))) {
        Push-Location $mgr
        try { Run "npm ci (bfmanager, for tauri signer)" { npm ci --no-audit --no-fund --ignore-scripts } } finally { Pop-Location }
    }
    Remove-Item "$manifestPath.sig" -ErrorAction SilentlyContinue
    # The password travels in the environment the signer reads, never on a
    # command line. Unset/blank = a key made without one ("--password=" is
    # then passed so the signer doesn't sit on a hidden prompt).
    $pwArgs = @()
    if ("$env:FOWL_ENGINE_SIGNING_PASSWORD") { $env:TAURI_SIGNING_PRIVATE_KEY_PASSWORD = $env:FOWL_ENGINE_SIGNING_PASSWORD }
    else { $pwArgs = @("--password=") }
    Push-Location $mgr
    try { Run "sign manifest.json" { npx tauri signer sign --private-key-path "$SigningKey" @pwArgs "$manifestPath" } }
    finally {
        Pop-Location
        Remove-Item Env:\TAURI_SIGNING_PRIVATE_KEY_PASSWORD -ErrorAction SilentlyContinue
    }
    if (-not (Test-Path "$manifestPath.sig")) { throw "tauri signer wrote no $manifestPath.sig" }
    if (Test-Path "$SigningKey.pub") {
        Say ("  signed; servers need autoupdate.public_key = the key in {0}" -f "$SigningKey.pub") "Gray"
    }
}

# Publish what is in manifest.json's folder: to -Folder, or as a GitHub release.
function Publish-Out([string]$manifestPath, [string]$tag, [string]$notes, [string]$commit, [bool]$checkPushed) {
    $out = Split-Path $manifestPath
    if (-not (Test-Path "$manifestPath.sig")) { throw "$manifestPath is not signed -- servers would refuse it" }
    $assets = Get-ChildItem $out -File | ForEach-Object { $_.FullName }
    if ($Folder) {
        $dest = Join-Path $Folder $tag
        if (Test-Path $dest) { throw "$dest already exists -- releases are immutable, pick a new -Tag" }
        New-Item -ItemType Directory -Force $dest -ErrorAction Stop | Out-Null
        # manifest last, so a server scanning the folder never sees a half-copied release
        Get-ChildItem $out -File | Where-Object Name -ne "manifest.json" | Copy-Item -Destination $dest -ErrorAction Stop
        Copy-Item $manifestPath $dest -ErrorAction Stop
        Say "published to $dest" "Green"
    } else {
        if (-not (Get-Command gh -ErrorAction SilentlyContinue)) { throw "gh (GitHub CLI) is not installed -- or use -Folder" }
        if ($checkPushed) {
            $onRemote = git -C $repoRoot branch -r --contains $commit 2>$null
            if (-not $onRemote) { throw "commit $($commit.Substring(0, 12)) is not pushed to any remote branch -- push it first (the release tag points at it)" }
        }
        $ghArgs = @("release", "create", $tag) + $assets + @("--repo", $Repo, "--title", $tag, "--notes", $notes, "--target", $commit)
        if ($Channel -eq "beta") { $ghArgs += "--prerelease" }
        Run "gh release create $tag" { gh @ghArgs }
        Say "published https://github.com/$Repo/releases/tag/$tag" "Green"
    }
    Say "servers with autoupdate enabled pick it up on their next check (or: OPS page -> Check now, /feops update_check)" "Green"
}

# The engine reads a campaign cfg with serde_json: strict UTF-8 JSON, no BOM, an
# object. One that doesn't parse would stop the mission on the server, so the
# release is refused here instead.
function Assert-CfgJson([string]$path) {
    $bytes = [System.IO.File]::ReadAllBytes($path)
    if ($bytes.Length -ge 3 -and $bytes[0] -eq 0xEF -and $bytes[1] -eq 0xBB -and $bytes[2] -eq 0xBF) {
        throw "$path starts with a UTF-8 BOM -- the engine refuses that; save it as UTF-8 without a BOM"
    }
    try {
        $text = (New-Object System.Text.UTF8Encoding($false, $true)).GetString($bytes)
        if ($PSVersionTable.PSVersion.Major -ge 6) {
            $doc = ConvertFrom-Json -InputObject $text -AsHashtable -ErrorAction Stop
        } else {
            # Windows PowerShell's ConvertFrom-Json chokes on big documents and
            # on keys that differ only in case; the serializer under it doesn't
            Add-Type -AssemblyName System.Web.Extensions
            $js = New-Object System.Web.Script.Serialization.JavaScriptSerializer
            $js.MaxJsonLength = [int]::MaxValue
            $js.RecursionLimit = 1000
            $doc = $js.DeserializeObject($text)
        }
    } catch { throw "$path is not valid JSON: $($_.Exception.Message)" }
    if ($doc -isnot [System.Collections.IDictionary]) { throw "$path is not a JSON object" }
}

# campaign-<key>.zip for every entry of -Campaigns, into $outDir; returns the
# manifest entries. Relative paths in the JSON are relative to the JSON itself.
function Build-CampaignPacks([string]$jsonPath, [string]$outDir, [string]$git, [string]$built) {
    $base = Split-Path $jsonPath
    $at = { param($p) if ([System.IO.Path]::IsPathRooted($p)) { $p } else { Join-Path $base $p } }
    $doc = Get-Content $jsonPath -Raw -ErrorAction Stop | ConvertFrom-Json
    Add-Type -AssemblyName System.IO.Compression, System.IO.Compression.FileSystem
    $out = [ordered]@{}
    foreach ($prop in $doc.PSObject.Properties) {
        if ($prop.Name.StartsWith("_")) { continue }   # "_comment" and friends
        # the key is a file name on both ends; the server compares it lower-case
        $key = $prop.Name.ToLower()
        if ($key -notmatch '^[a-z0-9][a-z0-9_.-]{0,63}$') {
            throw "campaigns key '$($prop.Name)': use the server's bfdb instance id (letters, digits, . _ -)"
        }
        $zipName = "campaign-$key.zip"
        if ($out.Contains($zipName)) { throw "campaigns key '$key' is listed twice" }
        $e = $prop.Value
        $cfgPath = if ($e.cfg) { & $at ([string]$e.cfg) } else { $null }
        if (-not $cfgPath -or -not (Test-Path $cfgPath -PathType Leaf)) { throw "campaigns.$key.cfg: no file at '$cfgPath'" }
        $cfgName = if ($e.cfg_name) { [string]$e.cfg_name } else { Split-Path $cfgPath -Leaf }
        if ($cfgName -notmatch '^[^\\/:*?"<>|]+_CFG$') {
            throw "campaigns.$key.cfg_name '$cfgName' must be a file name ending in _CFG (the engine loads <sortie>_CFG)"
        }
        Assert-CfgJson $cfgPath
        $members = [ordered]@{}
        $members[$cfgName] = @{ path = (Resolve-Path $cfgPath).Path; role = "cfg" }
        foreach ($m in @($e.miz)) {
            if (-not $m) { continue }
            $p = & $at ([string]$m)
            if (-not (Test-Path $p -PathType Leaf)) { throw "campaigns.$key.miz: no file at '$p'" }
            $leaf = Split-Path $p -Leaf
            if ($leaf -notmatch '\.miz$') { throw "campaigns.$key.miz: $leaf is not a .miz" }
            if ($members.Contains($leaf)) { throw "campaigns.$key has two files named $leaf" }
            $members[$leaf] = @{ path = (Resolve-Path $p).Path; role = "miz" }
        }
        $contents = [ordered]@{}
        $zipPath = Join-Path $outDir $zipName
        $zip = [System.IO.Compression.ZipFile]::Open($zipPath, "Create")
        try {
            foreach ($n in $members.Keys) {
                $src = $members[$n].path
                $contents[$n] = [ordered]@{
                    sha256 = (Get-FileHash $src -Algorithm SHA256).Hash.ToLower()
                    size   = (Get-Item $src).Length
                    role   = $members[$n].role
                }
                # flat names: the server decides where each file goes
                [System.IO.Compression.ZipFileExtensions]::CreateEntryFromFile($zip, $src, $n) | Out-Null
            }
        } finally { $zip.Dispose() }
        $zi = Get-Item $zipPath
        $out[$zipName] = [ordered]@{
            sha256   = (Get-FileHash $zipPath -Algorithm SHA256).Hash.ToLower()
            size     = $zi.Length
            git      = $git
            built    = $built
            key      = $key
            cfg_name = $cfgName
            contents = $contents
        }
        Say ("  {0,-24} {1}" -f $zipName, (($members.Keys) -join ", ")) "Gray"
    }
    if ($out.Count -eq 0) { throw "$jsonPath lists no campaigns" }
    return $out
}

# -SignOnly / -PublishOnly: the later stages of a release a `-DryRun` build
# left in -OutDir, so CI can build without the signing key or the GitHub
# token in reach, sign with only the key, and publish with only the token.
if ($SignOnly -or $PublishOnly) {
    if (-not $OutDir) { throw "-SignOnly / -PublishOnly need -OutDir: the folder a -DryRun build left" }
    if ($Campaigns) { Say "-Campaigns is ignored here: the packs were made by the -DryRun that left $OutDir" "Yellow" }
    $manifestPath = Join-Path $OutDir "manifest.json"
    if (-not (Test-Path $manifestPath)) { throw "no manifest.json in $OutDir" }
    if ($SignOnly) { Sign-Manifest $manifestPath; return }
    $m = Get-Content $manifestPath -Raw -ErrorAction Stop | ConvertFrom-Json
    if ($m.channel -ne $Channel) { throw "manifest says channel $($m.channel), -Channel says $Channel" }
    Publish-Out $manifestPath $m.tag $m.notes $m.commit $false
    return
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
# When the commit was made, UTC: plugin stamps are ordered by it, so a Manager
# bundle and a release built from different commits compare the right way
# round whenever each was built.
$commitTime = ([DateTimeOffset]::Parse((git -C $repoRoot log -1 --format=%cI $commit).Trim(),
    [System.Globalization.CultureInfo]::InvariantCulture)).UtcDateTime.ToString("yyyy-MM-ddTHH:mm:ssZ")
if (-not $OutDir) { $OutDir = Join-Path $env:TEMP "fowl-release-out\$Tag" }
if ($Campaigns) {
    if (-not (Test-Path $Campaigns -PathType Leaf)) { throw "no campaigns file at $Campaigns" }
    $Campaigns = (Resolve-Path $Campaigns).Path   # before any Push-Location
}

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
            # The community plugins ride along (they use fowlengine's icons) --
            # their code only, never their *.yaml, which is the operator's settings.
            # Keep in step with COMMUNITY_PLUGINS in bfmanager/scripts/stage-bot.mjs.
            $community = @("about", "announcements", "faq", "radio", "rules", "smartmod", "tickets")
            $dirs = @("plugins\fowlengine") + ($community | ForEach-Object { "plugins\$_" }) +
                (Get-ChildItem (Join-Path $botRoot "extensions") -Directory -Filter "bf*" |
                ForEach-Object { "extensions\$($_.Name)" })
            foreach ($d in $dirs) {
                Get-ChildItem (Join-Path $botRoot $d) -Recurse -File |
                    Where-Object { $_.FullName -notmatch '\\__pycache__\\' -and $_.Name -ne "fowlengine.yaml" -and
                                   $_.Name -ne ".fowl-plugin.json" -and
                                   -not ($community -contains ($d -replace '^plugins\\', '') -and
                                         $_.Extension -in ".yaml", ".yml") } |
                    ForEach-Object {
                        $rel = $_.FullName.Substring($botRoot.Length + 1).Replace("\", "/")
                        [System.IO.Compression.ZipFileExtensions]::CreateEntryFromFile($zip, $_.FullName, $rel) | Out-Null
                    }
            }
            # The plugin stamp: which build this is. The server's updater won't
            # unpack it over a newer plugin (e.g. one Fowl Engine Manager
            # synced), and the Manager won't sync an older bundle over it.
            $stamp = [ordered]@{ schema = 1; source = "engine-release"; tag = $Tag; git = $gitLabel
                                 commit = $commit; commit_time = $commitTime; built = $built }
            $w = [System.IO.StreamWriter]::new($zip.CreateEntry("plugins/fowlengine/.fowl-plugin.json").Open(),
                                               (New-Object System.Text.UTF8Encoding $false))
            try { $w.Write(($stamp | ConvertTo-Json)) } finally { $w.Dispose() }
        } finally { $zip.Dispose() }
        $zi = Get-Item $zipPath
        $manifestFiles["fowlengine-bot.zip"] = [ordered]@{
            sha256 = (Get-FileHash $zipPath -Algorithm SHA256).Hash.ToLower(); size = $zi.Length; git = $gitLabel; built = $built
        }
        Say "  fowlengine-bot.zip (bot plugin)" "Gray"
    }
    if ($Campaigns) {
        $packs = Build-CampaignPacks $Campaigns $OutDir $gitLabel $built
        foreach ($k in $packs.Keys) { $manifestFiles[$k] = $packs[$k] }
    }
    if ($manifestFiles.Count -eq 0) { throw "nothing to release" }

    $subject = (git -C $repoRoot log -1 --format=%s $commit).Trim()
    if (-not $Notes) { $Notes = "$subject`n`nBuilt from $gitLabel." }
    $manifest = [ordered]@{
        schema  = 1
        tag     = $Tag
        git     = $gitLabel
        commit  = $commit
        commit_time = $commitTime
        built   = $built
        channel = $Channel
        notes   = $Notes
        files   = $manifestFiles
    }
    $manifestPath = Join-Path $OutDir "manifest.json"
    # UTF-8 without a BOM: a BOM makes some JSON readers choke. Depth: a
    # campaign pack's per-file listing sits five objects down.
    [System.IO.File]::WriteAllText($manifestPath, ($manifest | ConvertTo-Json -Depth 10), (New-Object System.Text.UTF8Encoding $false))
    Say "manifest written: $manifestPath"

    # ---- sign + publish --------------------------------------------------------------

    if (Test-Path $SigningKey) {
        Sign-Manifest $manifestPath
    } elseif ($DryRun) {
        Say "no signing key at $SigningKey -- dry run left manifest.json UNSIGNED (servers would refuse it)" "Yellow"
    } else {
        throw "no signing key at $SigningKey (pass -SigningKey). Servers refuse unsigned releases -- see deploy/auto-update.md, 'Signing releases'."
    }

    if ($DryRun) {
        Say "dry run -- nothing published. Output: $OutDir" "Yellow"
        return
    }
    Publish-Out $manifestPath $Tag $Notes $commit (-not $WorkingTree -or -not $dirty)
}
finally {
    if ($worktree) {
        Pop-Location -ErrorAction SilentlyContinue
        git -C $repoRoot worktree remove --force $worktree 2>$null | Out-Null
    }
}
