@echo off

setlocal enabledelayedexpansion
for /f "tokens=1-4" %%1 in ('dumpbin /exports %1') do (
    set /a ordinal=%%1 2>nul
    set /a hint=0x%%2 2>nul
    set /a rva=0x%%3 2>nul
    if !ordinal! equ %%1 if !hint! equ 0x%%2 if !rva! equ 0x%%3 set exports=!exports! /export:%%4
)

for /f %%i in ("%1") do set dllpath=%%~dpni
rem Not "start": lua.lib must exist before it is hashed below.
lib /out:%dllpath%.lib /machine:x86-64 /def: %exports%

rem Record the dll/lib pair; setup-build.ps1 and .sh warn when the files in
rem the checkout stop matching it. Commit lua.sha256 with the new files.
powershell -NoProfile -Command "$l = Get-ChildItem -LiteralPath '%~f1','%dllpath%.lib' | ForEach-Object { (Get-FileHash -Algorithm SHA256 $_.FullName).Hash.ToLower() + '  ' + $_.Name }; [IO.File]::WriteAllText('%~dp1lua.sha256', ($l -join [char]10) + [char]10)"
