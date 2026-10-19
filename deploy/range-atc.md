# Range ATC: military air traffic control over SRS

The training range has a full set of spoken controllers at Andersen AFB. Pilots
talk to them on SRS the way they would at a real base. Each controller hands
the pilot on to the next one on its own frequency.

| Position | Callsign | UHF | What it does |
|---|---|---|---|
| ATIS | Andersen ATIS | 273.5 | Loops wind, visibility, cloud, temperature, altimeter, runway in use and pattern altitude. The letter changes when any of them change. |
| Clearance Delivery | Andersen Clearance | 335.8 | Issues the departure clearance: limit, radar vectors, initial and expected altitudes, departure frequency and squawk. It checks the readback. |
| Ground | Andersen Ground | 275.8 | Start-up, then taxi on the real taxiways to a hold-short line ("taxi via Bravo, Echo, hold short runway 06R"). Runway crossings, and taxi to parking after landing. |
| Tower | Andersen Tower | 340.2 | Takeoff, line up and wait, the overhead (initial, break, base), straight-ins, landing clearances, go-arounds and closed traffic. |
| Departure | Andersen Departure | 363.8 | Radar contact, climb, and release at 25 nm or 12,000 ft to Overlord 251.5. |
| Approach | Andersen Approach | 319.2 | Radar contact and descent, then vectors for the overhead, a straight-in, a visual, ILS/TACAN or a PAR. |
| Final | Andersen Final | 385.4 | PAR talkdown: course and glidepath corrections, range from touchdown, landing clearance, decision height. |

The runway in use follows the wind: 06R/24L for departures and arrivals.
Frequencies, runway use and procedures are in
[bfdb/data/atc/andersen.json](../bfdb/data/atc/andersen.json). Override any of
them in the config below.

## How a sortie sounds

```
You:   Andersen Clearance, Viper 1-1, request clearance to the range.
CLR:   Viper 1-1, Andersen Clearance, cleared to the range complex via radar vectors,
       fly runway heading, climb and maintain 4,000, expect FL220 10 minutes after
       departure, departure frequency 363.8, squawk 4201.
You:   Cleared to the range, 4,000, 363.8, squawk 4201, Viper 1-1.
CLR:   Viper 1-1, readback correct, contact Andersen Ground 275.8 when ready to taxi.
You:   Andersen Ground, Viper 1-1, request taxi.
GND:   Viper 1-1, Andersen Ground, runway 06R, taxi via Bravo, Echo, hold short
       runway 06R, wind 070 at 12, altimeter 2992.
You:   Taxi via Bravo, Echo, hold short 06R, Viper 1-1.
  (you reach the hold line)
GND:   Viper 1-1, contact Andersen Tower 340.2.
You:   Andersen Tower, Viper 1-1, holding short 06R, ready for departure.
TWR:   Viper 1-1, Andersen Tower, hold short runway 06R, traffic 2 mile final.
  (the traffic lands and clears)
TWR:   Viper 1-1, wind 070 at 12, runway 06R, cleared for takeoff.
  (600 ft after takeoff)
TWR:   Viper 1-1, contact Andersen Departure 363.8.
You:   Andersen Departure, Viper 1-1, passing 2,000 for 4,000.
DEP:   Viper 1-1, Andersen Departure, radar contact 3 miles northeast of Andersen,
       climb and maintain FL220, resume own navigation.
  (25 nm out)
DEP:   Viper 1-1, Andersen Departure, radar service terminated, contact Overlord 251.5.
```

The way home is the same in reverse. Call Approach with your request ("request
the overhead", "request PAR full stop", "request ILS"). Approach turns you
toward the initial point or the final approach gate and hands you to Tower, or
to Final for a PAR. On the runway, Tower tells you which way to turn off and
sends you to Ground. Ground taxis you to the ramp.

Things the controllers do on their own:

- **Hold you at the line.** If an arrival is inside 3 nm or the runway is
  occupied, Tower holds you short. It clears you for takeoff without being
  asked once the runway is free.
- **Wait for the runway.** An arrival with someone on the runway hears
  "continue, traffic on the runway". It is cleared to land once the runway is
  clear, or sent around inside half a mile if it is not.
- **Catch mistakes.**
  - Rolling onto a runway without a clearance gets "stop! You are on runway 06R
    without a clearance".
  - Leaving the taxi route gets "verify taxiing via ...".
  - A readback that drops the hold short gets "read back hold short
    instruction".
- **Talk you down.** The PAR talkdown runs every 5 seconds from about 10 miles
  to the threshold.
- **Handle emergencies.** "Mayday" gets priority handling from whoever you are
  talking to.

You can say "say again" to any controller, and ask any of them for the
weather or a radio check.

## Sounding like people

- **They pause before answering.** A controller waits one to two and a half
  seconds after you unkey before answering, as a person reaching for the
  mic would. Calls to the same frequency queue one after another, and never
  start while someone is transmitting.
- **Every frequency has its own voice.**
  - With Piper and a multi-speaker model (`en_US-libritts_r-medium`), each
    position gets a different speaker.
  - With several Piper models in the model's folder, they are dealt out one
    per position.
  - With Windows SAPI, the installed voices are dealt out.
  - Each position also has its own pace: ATIS reads slowly, Clearance reads
    fast, Final is deliberate.
  - AI pilots get voices of their own.
  - Override any of it under `voices:`.
- **The phrasing varies** where real controllers vary it ("good day", "read
  you loud and clear"). Clearances always use the standard words.
- **Traffic calls name the aircraft type:** "hold short runway 06R, traffic,
  a C-17 on a 3 mile final".

## AI traffic

The range flies a steady trickle of single-ship AI traffic in and out of
Andersen for the controllers to work: C-17s (Reach), KC-135s (Shell), C-130s
(Herky), B-52s (Doom), F-16s (Venom) and Hornets (Ragin). Their pilots talk
on the frequency in their own voices. They request taxi, read back
clearances, call ready, check in with Departure and Approach, and ask to
taxi in after landing.

The controllers really control them:

- **Departures sit on their stands, engines running, until Ground releases
  them.** Ground keeps one holding ("hold position, traffic, an F-16 on a 4
  mile final, expect taxi in two minutes") while a player is on final or in
  the pattern, or the runway is busy. It clears the AI to taxi once the runway
  is free.
- **Arrivals can be put in a hold.** Approach holds an inbound AI over its
  present position while a player flies a PAR or the overhead in front of it,
  then clears it direct once the player is down.

Nothing spawns while nobody is flying. Arrivals are removed after they have
parked, departures once they are 45 nm out.

Tune it in the range config (`<sortie>_RANGE`):

```json
"atc_traffic": {
  "enabled": true,
  "field": "Andersen",
  "departures_per_hour": 4,
  "arrivals_per_hour": 4,
  "max_active": 4,
  "country": "USA",
  "types": [{"typ": "C-17A", "callsign": "Reach", "weight": 3}]
}
```

`"enabled": false` turns it off. `types` replaces the built-in mix.

## Transcripts

- **In the cockpit.** Every controller transmission addressed to you also
  appears as text in DCS for 12 seconds ("ANDERSEN TOWER: Viper 1-1, runway
  06R, cleared for takeoff..."). A missed word is never lost.
- **On the range site.** The **ATC radio** page (`/atc`) shows every
  transmission live, with a tab per frequency. It covers controllers, players
  (what whisper heard) and AI pilots. **All** shows every station side by
  side.

## How it understands you

Speech goes through whisper (speech to text). A phraseology parser then picks
the request out. Standard calls are answered in about two seconds, mostly
whisper's time.

Calls the parser cannot place go to a language model (Claude), if a key is
set. It only classifies the call: taxi request, readback, approach request,
and so on. Every clearance comes from the controller logic and the live
traffic picture, never from the model.

Without a key, an unrecognised call gets "say again" if it named Andersen or
your callsign, and silence otherwise.

## Setting it up

1. **SRS External AWACS Mode.** The controllers join SRS the way an AWACS
   client does. In the range's `Config\SRS.cfg` set:
   ```
   EXTERNAL_AWACS_MODE=true
   EXTERNAL_AWACS_MODE_BLUE_PASSWORD=<a password>
   EXTERNAL_AWACS_MODE_RED_PASSWORD=<another>
   ```
   Restart SRS. Players are not affected. The Manager's **New box** does this
   for a fresh box.
2. **whisper.cpp**, to listen. Run `whisper-server` with an English model;
   `ggml-base.en` is fine on a GPU, `ggml-small.en` is more accurate. Without
   whisper the controllers still broadcast ATIS but cannot hear anyone.
3. **Piper**, for natural voices. Optional: without it, Windows SAPI speaks.
   Give each position its own Piper voice so Ground and Tower sound like
   different people.
4. **The bot's YAML.** Use `fowlengine.yaml` → `bfdb.instances` → the
   `kind: range` instance:
   ```yaml
   gci:
     enabled: false                 # no GCI on the range; the file is still written
     srs_port: 5005
     blue_eam_password: "<the SRS.cfg password>"
     opus_dll_path: "C:\\Program Files\\DCS-SimpleRadio-Standalone\\opus.dll"
     whisper_server_url: "http://127.0.0.1:8178"
     piper_exe: "C:\\piper\\piper.exe"
     piper_model: "C:\\piper\\en_US-ryan-medium.onnx"
     range_atc:
       enabled: true
       handoff_out: {name: Overlord, mhz: 251.5}
       voices:
         ground: {piper_model: "C:\\piper\\en_US-joe-medium.onnx"}
         final: {piper_model: "C:\\piper\\en_US-kusal-medium.onnx"}
   ```
   For the language model, add `atc_llm_key: "<Anthropic API key>"` under
   `bfdb:`. It reaches bfdb as `BFDB_ATC_LLM_KEY` and is never written to a
   file.
5. Run `/feops bfdb_restart`. bfdb's log should show:
   ```
   [range] range ATC: Andersen (PGUA) on ATIS 273.500 UHF AM, Clearance 335.800 UHF AM, ...
   [range] range ATC: Andersen taxi network aligned to DCS (12 m shift)
   ```
   Andersen ATC (one SRS client) then appears in the SRS client list.

### Options (`range_atc:`)

| Key | Default | |
|---|---|---|
| `enabled` | true | |
| `coalition` | blue | The SRS coalition the controllers sit on. |
| `require_clearance` | false | If true, Ground sends you to Clearance Delivery before it gives taxi. |
| `handoff_out` | none | Who Departure releases traffic to. Without it, Departure says "frequency change approved". |
| `fields` | Andersen | `[{builtin: andersen}]` or `[{data: "path\\to\\field.json"}]`. `positions` overrides frequencies, and `final: []` switches a position off. |
| `voices` | cast automatically | Per position: `piper_model`, `speaker` (multi-speaker Piper), `tts_voice` (SAPI), `speed` (1 = normal). |
| `llm` | on when a key is set | `model` (claude-haiku-5-5), `timeout_ms` (4000), `url`, `enabled`. |
| `gca_interval_secs` | 5 | PAR talkdown cadence. |
| `atis_interval_secs` | 60 | ATIS repeat. |
| `poll_ms` | 1000 | How often the radar picture is read from the engine. |

## Another airfield

DCS does not expose the taxi network. It comes from OpenStreetMap:

```
python scripts/atc_field_from_osm.py --icao PGSN --name "Saipan Intl" --callsign Saipan \
    --bbox 15.10,145.70,15.14,145.75 --out bfdb/data/atc/saipan.json
```

Then hand-tune `positions`, `runwayUse`, `departure`, `arrival` and the ramp
names in the generated file. A re-run keeps those sections. Point a `fields`
entry at the file.

On startup the controller lines the network up with the DCS runway thresholds.
If they disagree by more than 400 m, it says so and gives taxi clearances
without taxiway names.

## Where the code is

- `bfrange/src/atc.rs` builds the `query-atc` picture: runways with
  thresholds, weather, and player and AI traffic.
- `bfrange/src/atctraffic.rs` flies the AI traffic. The `range-atc` RPC
  starts, holds or resumes a flight, or shows a player a subtitle.
- `bfdb/src/milatc/`:
  - `controller.rs`: positions, flight strips and every decision.
  - `airport.rs`: the airfield and taxi routing.
  - `intent.rs`: the phraseology parser.
  - `llm.rs`: the language-model fallback.
  - `speech.rs`: written and spoken forms.
  - `voices.rs`: voice casting.
  - `mod.rs`: SRS, whisper, Piper, the poll loop, and the transcript
    behind `/api/range/atc`.
- `bfrange-web/src/pages/AtcPage.tsx` is the transcript page.
