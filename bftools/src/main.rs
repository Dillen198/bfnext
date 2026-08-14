use anyhow::Result;
use clap::{Args, Parser, Subcommand};
use serde_derive::Serialize;
use std::path::PathBuf;

mod mission_edit;

#[derive(Args, Clone, Debug, Serialize)]
struct SpecialSamCmd {
    /// a mission editor template with special SAM site groups. Every
    /// vehicle/static group placed under the Red or Blue coalition whose
    /// name matches "<Location> - <Label>" (e.g. "Hayjanah - SA-1") becomes
    /// one site; all of that group's units belong to it. The site's starting
    /// coalition is whichever side (Red/Blue country tree) the group is
    /// actually placed under in the editor. The opposite side's unit list is
    /// synthesized as a mirror of the placed units so the site can flip on
    /// capture.
    #[clap(long)]
    template: PathBuf,
    /// the output json file: an array of special_sam_sites entries, ready to
    /// be spliced into the main campaign config
    #[clap(long)]
    output: PathBuf,
    /// optionally merge the generated sites directly into a campaign config
    /// file's "special_sam_sites" array, writing the result back in place
    #[clap(long)]
    merge_into: Option<PathBuf>,
}

#[derive(Args, Clone, Debug, Serialize)]
struct MizCmd {
    /// the final miz file to output
    #[clap(long)]
    output: PathBuf,
    /// the base mission file
    #[clap(long)]
    base: PathBuf,
    /// the weapon template
    #[clap(long)]
    weapon: PathBuf,
    /// the options template
    #[clap(long)]
    options: PathBuf,
    /// the warehouse template
    #[clap(long)]
    warehouse: Option<PathBuf>,
    #[clap(long, default_value = "BINVENTORY")]
    blue_production_template: String,
    #[clap(long, default_value = "RINVENTORY")]
    red_production_template: String
}

#[derive(Subcommand, Clone, Debug, Serialize)]
enum Tools {
    Miz(MizCmd),
    SpecialSam(SpecialSamCmd),
}

#[derive(Parser)]
struct BftoolsArgs {
    #[clap(subcommand)]
    tool: Tools,
}

fn main() -> Result<()> {
    let bftools_args = BftoolsArgs::parse();
    env_logger::init();

    match bftools_args.tool {
        Tools::Miz(cfg) => mission_edit::run(&cfg)?,
        Tools::SpecialSam(cfg) => mission_edit::run_special_sam(&cfg)?,
    };
    Ok(())
}