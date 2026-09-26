//! Command-line front end for the cockpit overlay installer.
//!
//! The windowed installer (`bfcockpit-install.exe`) is what players run. This
//! exists for anything scripted -- a server admin rolling it out, a CI check,
//! or diagnosing a machine where the window found nothing and you want to see
//! exactly where it looked.
//!
//! All the actual work is in the library, so the two front ends cannot
//! disagree about what "install" means.
use anyhow::{bail, Result};
use bfcockpit_installer as installer;
use clap::Parser;
use std::{
    io::{self, IsTerminal, Write},
    path::PathBuf,
};

#[derive(Parser, Debug)]
#[command(
    name = "bfcockpit-install-cli",
    about = "Install the BFNext cockpit overlay into your DCS profile(s)",
    version
)]
struct Args {
    /// Install into this DCS profile specifically, instead of searching.
    /// Repeatable. Use it for a profile in an unusual place -- a `--write-dir`
    /// launch option, or a relocated Saved Games folder.
    #[arg(long, value_name = "PATH")]
    write_dir: Vec<PathBuf>,

    /// Also search this folder for DCS profiles. Repeatable.
    #[arg(long, value_name = "PATH")]
    search: Vec<PathBuf>,

    /// Campaign URL to seed into a new settings file. Only needed if you run
    /// your own bfdb.
    #[arg(long, value_name = "URL", default_value = installer::DEFAULT_URL)]
    url: String,

    /// Remove the overlay. Your settings file is left alone, so reinstalling
    /// keeps your keys, opacity and window position.
    #[arg(long)]
    uninstall: bool,

    /// Show what would be found and stop.
    #[arg(long)]
    list: bool,

    /// Don't ask; act on everything found.
    #[arg(long, short)]
    yes: bool,
}

fn main() {
    let args = Args::parse();
    let interactive = io::stdin().is_terminal();
    let code = match run(&args, interactive) {
        Ok(()) => 0,
        Err(e) => {
            eprintln!();
            eprintln!("  Failed: {e:#}");
            1
        }
    };
    // Double-clicked from Explorer, the console closes the instant main
    // returns and takes every message with it.
    if interactive {
        eprintln!();
        eprint!("  Press Enter to close...");
        let _ = io::stderr().flush();
        let mut sink = String::new();
        let _ = io::stdin().read_line(&mut sink);
    }
    std::process::exit(code);
}

fn run(args: &Args, interactive: bool) -> Result<()> {
    println!();
    println!("BFNext cockpit overlay {}", installer::plugin_version());
    println!("-------------------------------");

    let (profiles, scan) = if args.write_dir.is_empty() {
        let scan = installer::scan(&args.search);
        (scan.profiles.clone(), Some(scan))
    } else {
        let (found, missing) = installer::profiles_at(&args.write_dir);
        for m in missing {
            println!("  Skipping '{}' -- no such folder.", m.display());
        }
        (found, None)
    };

    if profiles.is_empty() {
        println!();
        match &scan {
            Some(scan) => {
                println!("  No DCS profile found.");
                println!();
                println!("  Searched:");
                if scan.roots.is_empty() {
                    println!("    (no Saved Games folder could be located)");
                }
                for r in &scan.roots {
                    println!("    {}", r.display());
                }
                if !scan.near_misses.is_empty() {
                    println!();
                    println!("  These look like DCS folders but have no Config folder, so they");
                    println!("  are not profiles (usually a mod manager's, or a module's own");
                    println!("  data folder):");
                    for m in &scan.near_misses {
                        println!("    {}", m.display());
                    }
                }
                println!();
                println!("  Run DCS once so it creates its profile, then try again -- or point");
                println!("  this at the profile directly:");
            }
            None => {
                println!("  None of the folders you named exist.");
                println!();
                println!("  A DCS profile is the folder containing Config and Logs -- the one");
                println!("  DCS writes to, not the one it is installed in. For example:");
            }
        }
        println!();
        println!("      bfcockpit-install-cli --write-dir \"C:\\path\\to\\your\\DCS\"");
        bail!("nothing to install into");
    }

    println!();
    println!("  Found {} DCS profile(s):", profiles.len());
    for p in &profiles {
        println!("    {}   [{}]", p.path.display(), p.status());
    }

    if args.list {
        return Ok(());
    }

    if interactive && !args.yes {
        let verb = if args.uninstall { "Remove from" } else { "Install into" };
        print!("\n  {verb} all {} profile(s)? [Y/n] ", profiles.len());
        io::stdout().flush().ok();
        let mut answer = String::new();
        io::stdin().read_line(&mut answer)?;
        let answer = answer.trim().to_ascii_lowercase();
        if !(answer.is_empty() || answer == "y" || answer == "yes") {
            println!("\n  Cancelled. Nothing was changed.");
            return Ok(());
        }
    }

    println!();
    let mut changed = 0usize;
    for p in &profiles {
        let res = if args.uninstall {
            installer::uninstall_one(&p.path)
        } else {
            installer::install_one(&p.path, &args.url)
        };
        match res {
            Ok(Some(msg)) => {
                println!("  {}: {msg}", p.name());
                changed += 1;
            }
            Ok(None) => println!("  {}: nothing to do", p.name()),
            // One unwritable profile (a stale folder, a permissions oddity)
            // must not stop the others from being done.
            Err(e) => println!("  {}: FAILED -- {e:#}", p.name()),
        }
    }

    println!();
    if args.uninstall {
        if changed > 0 {
            println!("  Removed. Your settings file was left in place.");
        } else {
            println!("  Nothing was installed.");
        }
        return Ok(());
    }

    if changed == 0 {
        bail!("could not install into any profile");
    }

    println!("  Done.");
    println!();
    println!("  Restart DCS, join the server, and press your Comms / radio-menu key.");
    println!("  Nothing else to set up -- it knows who you are the moment you join.");
    Ok(())
}
