# Why?
# In order to safely interface with DCS we need to link to the same version of the lua runtime
# that DCS is using. If we link to a different version, very bad things like structures not being compatible,
# globals being in the wrong place, or in two places, the GC of one version running on the heap of the other
# version, etc might happen. There's a huge list, and you just don't want to go there.
#
# Setting the environment variables below tells the mlua build scripts that it should produce a dll linked
# dynamically to the lua runtime, meaning it will not include one of it's own, it will just assume that
# when our dll is loaded, lua is already linked, which DCS will ensure is true, because it loads lua.dll
# very early on.
#
# In order to link dynamically to lua, we need a .lib file that describes all the functions exported by the dll,
# so that the linker can check that the dynamic linkage will work at run time. That is why there is a lua.lib
# checked in at the root of the repo.
#
# When a new version of DCS is released, it *could* use a different version of lua (although ED has not upgraded
# their lua in aaaaaaages). As such every time DCS is updated we should copy lua.dll from the bin-mt folder of DCS
# to this directory and run dll2lib.bat on it to generate a new lua.lib file. 
# (you must have the windows sdk installed and the tools in your path for that to work).

$env:LUA_LIB=Get-Location
$env:LUA_LINK="dylib"
$env:LUA_LIB_NAME="lua"

# lua.lib only describes the lua.dll it was generated from. lua.sha256
# records that pair (dll2lib.bat rewrites it). A lua.dll swapped in without
# regenerating lua.lib, or a stale lua.lib, still links -- and then crashes or
# misbehaves inside DCS. So check the pair here, and say so loudly.
$luaRoot = if ($PSScriptRoot) { $PSScriptRoot } else { Get-Location }
$luaRecord = Join-Path $luaRoot "lua.sha256"
if (-not (Test-Path $luaRecord)) {
    Write-Warning "lua.sha256 is missing: cannot check that lua.dll and lua.lib are a matching pair."
} else {
    $luaBad = @()
    foreach ($line in Get-Content $luaRecord) {
        if ($line -match '^\s*([0-9a-fA-F]{64})\s+\*?(\S+)\s*$') {
            $want = $Matches[1].ToLower()
            $file = Join-Path $luaRoot $Matches[2]
            if (-not (Test-Path $file)) {
                $luaBad += "$($Matches[2]) is missing"
            } elseif ((Get-FileHash -Algorithm SHA256 $file).Hash.ToLower() -ne $want) {
                $luaBad += "$($Matches[2]) does not match lua.sha256"
            }
        }
    }
    if ($luaBad.Count -gt 0) {
        Write-Warning ("Lua link check FAILED: " + ($luaBad -join "; ") + ".")
        Write-Warning "If you just copied a new lua.dll from DCS's bin-mt folder, run dll2lib.bat on it: it rebuilds lua.lib and rewrites lua.sha256. Otherwise restore both files from git. A mismatched pair builds fine and fails inside DCS. See docs/dcs-update-checklist.md."
    }
    # Optional: compare against the installed DCS, to notice a DCS update that
    # changed its Lua before it bites. Set DCS_PATH to the install folder.
    if ($env:DCS_PATH) {
        $dcsLua = Join-Path $env:DCS_PATH "bin-mt\lua.dll"
        if (Test-Path $dcsLua) {
            $mine = (Get-FileHash -Algorithm SHA256 (Join-Path $luaRoot "lua.dll")).Hash
            if ((Get-FileHash -Algorithm SHA256 $dcsLua).Hash -ne $mine) {
                Write-Warning "DCS's $dcsLua differs from the repo's lua.dll: DCS has changed its Lua. Follow docs/dcs-update-checklist.md before building for this DCS."
            }
        } else {
            Write-Warning "DCS_PATH is set but $dcsLua does not exist."
        }
    }
}
