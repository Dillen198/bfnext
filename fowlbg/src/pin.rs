//! Keep the mission DLL loaded until the process exits.
//!
//! DCS unloads a Lua C module when the Lua state that required it closes,
//! and the background thread and tokio's workers are still running the
//! DLL's code then: nothing stops them first. The process died in
//! `bflib.dll_unloaded` that way, on its way down. Pinned, the unload is a
//! no-op and the threads keep their code until the process ends.

/// Pin the DLL this crate is linked into. Once, at module load; a second
/// call is harmless.
#[cfg(windows)]
pub fn pin_module() -> anyhow::Result<()> {
    use std::ffi::c_void;
    const FLAG_PIN: u32 = 0x1;
    const FLAG_FROM_ADDRESS: u32 = 0x4;
    #[link(name = "kernel32")]
    unsafe extern "system" {
        fn GetModuleHandleExW(
            flags: u32,
            name: *const u16,
            module: *mut *mut c_void,
        ) -> i32;
    }
    let mut module = std::ptr::null_mut();
    // SAFETY: with FROM_ADDRESS `name` is read only as an address inside a
    // loaded module -- this function's, so the DLL it is linked into -- and
    // `module` is a valid out-pointer. Neither is kept after the call.
    let ok = unsafe {
        GetModuleHandleExW(
            FLAG_FROM_ADDRESS | FLAG_PIN,
            pin_module as *const () as *const u16,
            &mut module,
        )
    };
    if ok == 0 {
        anyhow::bail!("GetModuleHandleExW: {}", std::io::Error::last_os_error())
    }
    Ok(())
}

/// Nothing to do off Windows: DCS servers run there.
#[cfg(not(windows))]
pub fn pin_module() -> anyhow::Result<()> {
    Ok(())
}

#[cfg(test)]
mod tests {
    #[test]
    fn pins_its_own_image_twice() {
        // In a test it is the test executable that gets pinned: harmless.
        super::pin_module().unwrap();
        super::pin_module().unwrap();
    }
}
