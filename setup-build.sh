export LUA_LIB=$(pwd)
export LUA_LIB_NAME="lua"
export LUA_LINK=dylib

# lua.lib only describes the lua.dll it was generated from; lua.sha256
# records that pair (dll2lib.bat rewrites it). A mismatched pair links fine
# and then fails inside DCS, so check it and warn loudly.
lua_root=$(pwd)
if [ ! -f "$lua_root/lua.sha256" ]; then
    echo "WARNING: lua.sha256 is missing: cannot check that lua.dll and lua.lib are a matching pair." >&2
else
    if command -v sha256sum >/dev/null 2>&1; then
        lua_check="sha256sum -c --quiet -"
    else
        lua_check="shasum -a 256 -c --quiet -"
    fi
    # tr: a Windows checkout (core.autocrlf) gives the file CRLF line ends
    if ! (cd "$lua_root" && tr -d '' < lua.sha256 | $lua_check >/dev/null 2>&1); then
        echo "WARNING: Lua link check FAILED: lua.dll / lua.lib do not match lua.sha256." >&2
        echo "WARNING: after copying a new lua.dll from DCS, run dll2lib.bat (it rebuilds lua.lib and lua.sha256); otherwise restore both from git. See docs/dcs-update-checklist.md." >&2
    fi
fi
