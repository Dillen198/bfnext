; Fowl Engine Manager installer hooks (Tauri NSIS).
;
; The installed exe is also the "FowlEngine" Windows service, so an install or
; update has to stop the service before the files can be replaced, and start
; it again afterwards. PREINSTALL runs before Tauri's "is the app running"
; check, so the service gets a clean stop (not a kill the service manager
; would count as a crash and restart mid-copy). Stopping it stops
; DCSServerBot; DCS itself keeps running.
;
; Strings are `backtick`-delimited so the PowerShell inside can use both
; quote kinds; $$ is a literal $ for NSIS.

!macro NSIS_HOOK_PREINSTALL
  DetailPrint "Stopping the FowlEngine service (DCS keeps running)..."
  nsExec::Exec `powershell.exe -NoProfile -ExecutionPolicy Bypass -Command "$$s = Get-Service FowlEngine -ErrorAction SilentlyContinue; if ($$s -and $$s.Status -ne 'Stopped') { Stop-Service FowlEngine -Force -ErrorAction SilentlyContinue; $$s.WaitForStatus('Stopped', '00:01:00') }"`
  Pop $0
!macroend

!macro NSIS_HOOK_POSTINSTALL
  ; A service from 0.1.x runs as the server's user, in session 0 where DCS
  ; hangs: move it to LocalSystem, which starts the bot on that user's desktop.
  DetailPrint "Updating the FowlEngine service (if installed)..."
  nsExec::Exec `"$INSTDIR\FowlEngineManager.exe" --migrate-service`
  Pop $0
  ; Only if the app's setup has already created it -- a first install leaves
  ; that to the setup wizard.
  DetailPrint "Starting the FowlEngine service (if installed)..."
  nsExec::Exec `powershell.exe -NoProfile -ExecutionPolicy Bypass -Command "if (Get-Service FowlEngine -ErrorAction SilentlyContinue) { Start-Service FowlEngine -ErrorAction SilentlyContinue }"`
  Pop $0
!macroend

!macro NSIS_HOOK_PREUNINSTALL
  DetailPrint "Stopping the FowlEngine service..."
  nsExec::Exec `powershell.exe -NoProfile -ExecutionPolicy Bypass -Command "Stop-Service FowlEngine -Force -ErrorAction SilentlyContinue"`
  Pop $0
  ; An update (/UPDATE) keeps the service -- and its account + password. A
  ; real uninstall removes it, since its exe is about to be deleted.
  ${If} $UpdateMode <> 1
    DetailPrint "Removing the FowlEngine service..."
    nsExec::Exec `sc.exe delete FowlEngine`
    Pop $0
  ${EndIf}
!macroend
