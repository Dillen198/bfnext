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
struct FixLogiCoverageCmd {
    /// source mission file to read (left untouched)
    #[clap(long)]
    input: PathBuf,
    /// fixed mission file to write
    #[clap(long)]
    output: PathBuf,
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
    red_production_template: String,
    /// override the mission's date and start time with the current real-world
    /// local date/time (of the machine running bftools)
    #[clap(long)]
    live_time: bool,
    /// override the mission's ground-level temperature, QNH, and wind with
    /// live real-world weather (fetched from open-meteo.com, no API key
    /// required). Upper winds and clouds are left as authored in the options
    /// template. Requires --live-weather-lat and --live-weather-lon
    #[clap(long)]
    live_weather: bool,
    /// latitude to fetch live weather for, required by --live-weather
    #[clap(long)]
    live_weather_lat: Option<f64>,
    /// longitude to fetch live weather for, required by --live-weather
    #[clap(long)]
    live_weather_lon: Option<f64>,
    /// optional JSON file of DCS client option overrides (e.g.
    /// {"miscellaneous": {"f10_awacs": true, "chat_window_at_start": true}})
    /// merged into the generated mission's options file. The options file's
    /// settings (miscellaneous, difficulty, graphics, etc.) are a snapshot
    /// of whoever's DCS client saved --options through the Mission Editor,
    /// not something editable in the ME itself -- this lets you override
    /// specific keys without requiring someone to change their local DCS
    /// settings and re-save the mission just to flip one flag. Keys not
    /// mentioned are left as copied from --options
    #[clap(long)]
    options_overrides: Option<PathBuf>,
}

#[derive(Subcommand, Clone, Debug, Serialize)]
enum Tools {
    Miz(MizCmd),
    SpecialSam(SpecialSamCmd),
    FixLogiCoverage(FixLogiCoverageCmd),
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
        Tools::FixLogiCoverage(cfg) => mission_edit::run_fix_logi_coverage(&cfg)?,
    };
    Ok(())
}