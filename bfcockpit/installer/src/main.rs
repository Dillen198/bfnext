//! The windowed installer for the BFNext cockpit overlay.
//!
//! This is what a player runs. It opens a window, lists the DCS profiles it
//! found, and installs into the ones they tick — no console, no typing, and
//! nothing to read before it works.
//!
//! Built on the Win32 API directly (via `native-windows-gui`) rather than a
//! toolkit, so it stays a single small exe with no runtime to install — which
//! would be a strange thing to require of an installer.
//!
//! All the actual work lives in the library next door and is shared with
//! `bfcockpit-install-cli`, so the two can never disagree about what
//! "install" means.
#![windows_subsystem = "windows"]

use bfcockpit_installer as installer;
use installer::Profile;
use native_windows_gui as nwg;
use nwg::NativeUi;
use std::{cell::RefCell, path::PathBuf, rc::Rc};

const W: i32 = 700;
const H: i32 = 520;
const PAD: i32 = 14;

fn main() {
    if let Err(e) = nwg::init() {
        fatal(&format!("Could not start the installer window: {e}"));
        return;
    }
    // Without this the whole window renders in the 1995 system font.
    let _ = nwg::Font::set_global_family("Segoe UI");

    match Installer::build_ui(Default::default()) {
        Ok(ui) => {
            ui.rescan();
            nwg::dispatch_thread_events();
        }
        Err(e) => fatal(&format!("Could not build the installer window: {e}")),
    }
}

/// Last-resort error path: if the window itself cannot be created there is
/// nowhere to print to, so use the one UI primitive that needs no setup.
fn fatal(msg: &str) {
    nwg::modal_fatal_message(&nwg::Window::default(), "BFNext Cockpit Overlay", msg);
}

#[derive(Default)]
pub struct Installer {
    window: nwg::Window,
    heading: nwg::Label,
    subheading: nwg::Label,
    list: nwg::ListBox<String>,
    add_btn: nwg::Button,
    refresh_btn: nwg::Button,
    install_btn: nwg::Button,
    uninstall_btn: nwg::Button,
    close_btn: nwg::Button,
    log: nwg::TextBox,
    folder_dialog: nwg::FileDialog,

    /// Profiles currently listed, in list order.
    profiles: RefCell<Vec<Profile>>,
    /// Extra folders the user pointed us at this session.
    extra_roots: RefCell<Vec<PathBuf>>,
}

impl Installer {
    /// Re-find every profile and repaint the list.
    fn rescan(&self) {
        let extra = self.extra_roots.borrow().clone();
        let scan = installer::scan(&extra);

        let mut labels: Vec<String> = scan
            .profiles
            .iter()
            .map(|p| format!("{}          [{}]", p.path.display(), p.status()))
            .collect();

        if labels.is_empty() {
            labels.push("(no DCS profile found — see below)".into());
        }

        self.list.set_collection(labels);
        // Default to acting on everything: having several profiles and
        // installing into only one is the mistake this is here to prevent.
        if !scan.profiles.is_empty() {
            self.list.select_all();
        }

        let found = scan.profiles.len();
        *self.profiles.borrow_mut() = scan.profiles;

        let enabled = found > 0;
        self.install_btn.set_enabled(enabled);
        self.uninstall_btn.set_enabled(enabled);

        if found == 0 {
            let mut msg = String::from("No DCS profile was found.\r\n\r\nLooked in:\r\n");
            if scan.roots.is_empty() {
                msg.push_str("    (no Saved Games folder could be located)\r\n");
            }
            for r in &scan.roots {
                msg.push_str(&format!("    {}\r\n", r.display()));
            }
            if !scan.near_misses.is_empty() {
                msg.push_str(
                    "\r\nThese look like DCS folders but have no Config folder, so they are \
                     not profiles (usually a mod manager's, or a module's own data folder):\r\n",
                );
                for m in &scan.near_misses {
                    msg.push_str(&format!("    {}\r\n", m.display()));
                }
            }
            msg.push_str(
                "\r\nRun DCS once so it creates its profile, then press Refresh — or use \
                 \"Add profile...\" to point at it yourself. A DCS profile is the folder \
                 containing Config and Logs, not the folder DCS is installed in.",
            );
            self.set_log(&msg);
            self.subheading.set_text("Nothing found yet");
        } else {
            self.set_log(
                "Ready. Tick the profiles you want and press Install.\r\n\r\n\
                 All of them are selected by default — having more than one DCS profile and \
                 installing into only one is the usual reason the overlay doesn't appear.",
            );
            self.subheading.set_text(&format!(
                "{found} DCS profile{} found on this machine",
                if found == 1 { "" } else { "s" }
            ));
        }
    }

    /// Which profiles are ticked. Nothing ticked means the user meant all of
    /// them — the alternative is a button that silently does nothing.
    fn selected(&self) -> Vec<Profile> {
        let all = self.profiles.borrow();
        let picked = self.list.multi_selection();
        if picked.is_empty() {
            return all.clone();
        }
        picked.iter().filter_map(|i| all.get(*i).cloned()).collect()
    }

    fn apply(&self, uninstall: bool) {
        let targets = self.selected();
        if targets.is_empty() {
            return;
        }

        let mut out = String::new();
        let mut changed = 0usize;
        let mut failed = 0usize;

        for p in &targets {
            let res = if uninstall {
                installer::uninstall_one(&p.path)
            } else {
                installer::install_one(&p.path, installer::DEFAULT_URL)
            };
            match res {
                Ok(Some(msg)) => {
                    out.push_str(&format!("{}: {msg}\r\n", p.name()));
                    changed += 1;
                }
                Ok(None) => out.push_str(&format!("{}: nothing to do\r\n", p.name())),
                // One unwritable profile must not stop the others.
                Err(e) => {
                    out.push_str(&format!("{}: FAILED — {e:#}\r\n", p.name()));
                    failed += 1;
                }
            }
        }

        out.push_str("\r\n");
        if uninstall {
            out.push_str(if changed > 0 {
                "Removed. Your settings were left in place, so reinstalling keeps your keys, \
                 opacity and window position."
            } else {
                "Nothing was installed."
            });
        } else if changed > 0 {
            out.push_str(
                "Done.\r\n\r\n\
                 Restart DCS, join the server, and press your Comms / radio-menu key.\r\n\
                 Nothing else to set up — it knows who you are the moment you join.\r\n\r\n\
                 If it doesn't appear, open Saved Games\\DCS\\Logs\\dcs.log and search for \
                 BFCOCKPIT. Every step it takes is logged there.",
            );
        } else {
            out.push_str("Nothing was installed.");
        }
        if failed > 0 {
            out.push_str(
                "\r\n\r\nSome profiles could not be written to. If DCS is running, close it \
                 and try again.",
            );
        }

        self.set_log(&out);
        // Repaint the list so the version column reflects what just happened,
        // without wiping the report that is now on screen.
        let report = out;
        self.refresh_list_only();
        self.set_log(&report);
    }

    /// Re-read the list without touching the log area.
    fn refresh_list_only(&self) {
        let extra = self.extra_roots.borrow().clone();
        let scan = installer::scan(&extra);
        let labels: Vec<String> = scan
            .profiles
            .iter()
            .map(|p| format!("{}          [{}]", p.path.display(), p.status()))
            .collect();
        if labels.is_empty() {
            return;
        }
        self.list.set_collection(labels);
        self.list.select_all();
        *self.profiles.borrow_mut() = scan.profiles;
    }

    fn add_profile(&self) {
        if self.folder_dialog.run(Some(&self.window)) {
            if let Ok(path) = self.folder_dialog.get_selected_item() {
                let path = PathBuf::from(path);
                // Accept either the profile itself or the folder holding
                // several of them -- people reasonably pick either.
                let root = if path.join("Config").is_dir() {
                    path.parent().map(|p| p.to_path_buf()).unwrap_or(path)
                } else {
                    path
                };
                self.extra_roots.borrow_mut().push(root);
                self.rescan();
            }
        }
    }

    fn set_log(&self, text: &str) {
        self.log.set_text(text);
    }
}

// nwg's derive macro is convenient but hides the layout; with this many
// absolutely-positioned controls it is clearer, and easier to adjust, written
// out. This is the boilerplate the macro would have generated.
mod installer_ui {
    use super::*;
    use std::ops::Deref;

    pub struct InstallerUi {
        inner: Rc<Installer>,
        handler: nwg::EventHandler,
    }

    impl NativeUi<InstallerUi> for Installer {
        fn build_ui(mut data: Installer) -> Result<InstallerUi, nwg::NwgError> {
            nwg::Window::builder()
                .size((W, H))
                .position((350, 200))
                .title(&format!(
                    "BFNext Cockpit Overlay {} — Installer",
                    installer::plugin_version()
                ))
                // No resize/maximize: every control is absolutely positioned,
                // and a stretched window would just leave a field of grey.
                .flags(nwg::WindowFlags::WINDOW | nwg::WindowFlags::VISIBLE)
                .build(&mut data.window)?;

            nwg::Label::builder()
                .text("Install the cockpit overlay into DCS")
                .size((W - PAD * 2, 26))
                .position((PAD, PAD))
                .parent(&data.window)
                .build(&mut data.heading)?;

            nwg::Label::builder()
                .text("Searching...")
                .size((W - PAD * 2, 20))
                .position((PAD, PAD + 26))
                .parent(&data.window)
                .build(&mut data.subheading)?;

            nwg::ListBox::builder()
                .size((W - PAD * 2, 150))
                .position((PAD, PAD + 52))
                .flags(
                    nwg::ListBoxFlags::VISIBLE
                        | nwg::ListBoxFlags::TAB_STOP
                        | nwg::ListBoxFlags::MULTI_SELECT,
                )
                .parent(&data.window)
                .build(&mut data.list)?;

            let row = PAD + 52 + 150 + 10;
            nwg::Button::builder()
                .text("Add profile...")
                .size((120, 30))
                .position((PAD, row))
                .parent(&data.window)
                .build(&mut data.add_btn)?;

            nwg::Button::builder()
                .text("Refresh")
                .size((90, 30))
                .position((PAD + 128, row))
                .parent(&data.window)
                .build(&mut data.refresh_btn)?;

            nwg::Button::builder()
                .text("Uninstall")
                .size((110, 30))
                .position((W - PAD - 110 - 128, row))
                .parent(&data.window)
                .build(&mut data.uninstall_btn)?;

            nwg::Button::builder()
                .text("Install")
                .size((120, 30))
                .position((W - PAD - 120, row))
                .parent(&data.window)
                .build(&mut data.install_btn)?;

            let log_y = row + 40;
            nwg::TextBox::builder()
                .size((W - PAD * 2, H - log_y - 56))
                .position((PAD, log_y))
                .readonly(true)
                .flags(
                    nwg::TextBoxFlags::VISIBLE
                        | nwg::TextBoxFlags::VSCROLL
                        | nwg::TextBoxFlags::AUTOVSCROLL,
                )
                .parent(&data.window)
                .build(&mut data.log)?;

            nwg::Button::builder()
                .text("Close")
                .size((100, 30))
                .position((W - PAD - 100, H - 46))
                .parent(&data.window)
                .build(&mut data.close_btn)?;

            nwg::FileDialog::builder()
                .action(nwg::FileDialogAction::OpenDirectory)
                .title("Pick your DCS profile folder (the one with Config in it)")
                .build(&mut data.folder_dialog)?;

            let ui = Rc::new(data);
            let evt_ui = Rc::downgrade(&ui);
            let handler = nwg::full_bind_event_handler(
                &ui.window.handle,
                move |evt, _data, handle| {
                    let Some(ui) = evt_ui.upgrade() else { return };
                    match evt {
                        nwg::Event::OnButtonClick => {
                            if handle == ui.install_btn { ui.apply(false) }
                            else if handle == ui.uninstall_btn { ui.apply(true) }
                            else if handle == ui.refresh_btn { ui.rescan() }
                            else if handle == ui.add_btn { ui.add_profile() }
                            else if handle == ui.close_btn { nwg::stop_thread_dispatch() }
                        }
                        nwg::Event::OnWindowClose => {
                            if handle == ui.window {
                                nwg::stop_thread_dispatch()
                            }
                        }
                        _ => {}
                    }
                },
            );

            Ok(InstallerUi { inner: ui, handler })
        }
    }

    impl Drop for InstallerUi {
        fn drop(&mut self) {
            nwg::unbind_event_handler(&self.handler);
        }
    }

    impl Deref for InstallerUi {
        type Target = Installer;
        fn deref(&self) -> &Installer {
            &self.inner
        }
    }
}

