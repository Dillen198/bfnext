use anyhow::Result;
use clap::{Args, Parser, Subcommand};
use serde_derive::Serialize;
use std::path::PathBuf;

mod mission_edit;
mod unitdb;

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
    /// the weapon template: a miz whose Client-skill plane/helicopter groups
    /// carry the payload, radio, frequency and AddPropAircraft settings to
    /// stamp onto the base mission's static player slots. Optional -- a
    /// mission built entirely on DCS dynamic slots has no static player slots
    /// to stamp, and its loadouts come from the dynSpawnTemplate groups in the
    /// --warehouse miz instead, so there is nothing for this to do
    #[clap(long)]
    weapon: Option<PathBuf>,
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
    /// Optional per-coalition NAVAL inventory templates (Invisible FARP
    /// statics in the --warehouse miz). If present, their aircraft roster
    /// is copied onto every ship warehouse of that coalition, so carriers
    /// stock a carrier-appropriate airframe list. Absent => ships keep
    /// their editor roster.
    #[clap(long, default_value = "BINVENTORYNAVY")]
    blue_navy_production_template: String,
    #[clap(long, default_value = "RINVENTORYNAVY")]
    red_navy_production_template: String,
    /// override the mission's date and start time with the current real-world
    /// local date/time (of the machine running bftools)
    #[clap(long)]
    live_time: bool,
    /// override the mission's temperature, QNH, wind, clouds and obscurants
    /// with live real-world weather. The surface layer is the real decoded
    /// METAR from --metar-station; the winds aloft come from open-meteo's
    /// model at that station's position. Requires --metar-station and
    /// --checkwx-api-key
    #[clap(long)]
    live_weather: bool,
    /// checkwxapi.com API key, required by --live-weather
    #[clap(long)]
    checkwx_api_key: Option<String>,
    /// ICAO of the METAR station the weather is taken from (e.g. "UGKO"
    /// Kutaisi, "OSDI" Damascus). Required by --live-weather: it sets both
    /// the surface conditions and, from the station's own coordinates, where
    /// the winds aloft are sampled
    #[clap(long)]
    metar_station: Option<String>,
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
    /// F10 map view forced on players who are IN a slot: one of all, allies
    /// (fog of war), onlyallies, myaircraft, onlymap. Defaults to whatever
    /// --options forces, or "all" if it forces nothing -- i.e. leaving this
    /// alone does not change what pilots see
    #[clap(long)]
    map_view: Option<String>,
    /// F10 map view forced on spectators and observers -- the people who are
    /// not in a slot. Defaults to "onlymap": terrain and markup, no units.
    /// This is what stops players dropping to spectator to read the whole
    /// battlefield off the F10 map and then jumping back into a jet
    #[clap(long, default_value = "onlymap")]
    spectator_map_view: String,
    /// don't touch the F10 map view at all: take forcedOptions from --options
    /// exactly as it is
    #[clap(long)]
    no_force_map_view: bool,
}

#[derive(Subcommand, Clone, Debug, Serialize)]
enum Tools {
    Miz(MizCmd),
    SpecialSam(SpecialSamCmd),
    FixLogiCoverage(FixLogiCoverageCmd),
    /// Report on a harvested DCS unit range database (JSON or a markdown table)
    UnitDb(unitdb::UnitDbCmd),
    /// Print this build's identity as JSON (git rev + build time) and exit.
    Version,
}

/// Build identity, embedded at compile time by `build.rs`.
const BUILD_GIT: &str = env!("BFNEXT_BUILD_GIT");
const BUILD_EPOCH: &str = env!("BFNEXT_BUILD_EPOCH");
const BUILD_VERSION: &str = env!("CARGO_PKG_VERSION");

fn print_version() {
    let built = BUILD_EPOCH
        .parse::<i64>()
        .ok()
        .and_then(|s| chrono::DateTime::from_timestamp(s, 0))
        .map(|dt| dt.to_rfc3339_opts(chrono::SecondsFormat::Secs, true))
        .unwrap_or_else(|| "unknown".to_string());
    println!(
        r#"{{"name":"bftools","version":"{BUILD_VERSION}","git":"{BUILD_GIT}","built":"{built}"}}"#
    );
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
        Tools::UnitDb(cfg) => unitdb::run(&cfg)?,
        Tools::Version => print_version(),
    };
    Ok(())
}