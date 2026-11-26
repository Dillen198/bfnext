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

Beyond the airfield, the range has its own controllers (see
[Range Control, the tankers and the carriers](#range-control-the-tankers-and-the-carriers)):

| Station | UHF | What it does |
|---|---|---|
| Range Control | 348.6 | Books the ranges in 20-minute slots, one flight per range at a time. Clears you hot and calls you off. |
| Each tanker ("Texaco 1-1") | the tanker's own frequency | Its crew runs the line: rejoin, pre-contact, contact, cleared to depart. |
| Each carrier's Marshal | the carrier's frequency | Case I/III check-in, a slot in the stack, push time. |
| Each carrier's Approach / Paddles | Marshal + 0.5 | Range calls on the way in, then the LSO in the groove. |

The runway in use follows the wind: 06R/24L for departures and arrivals.
Frequencies, runway use and procedures are in
[bfdb/data/atc/andersen.json](../bfdb/data/atc/andersen.json). Override any of
them in the config below.

## How a sortie sounds

```
You:   Andersen Clearance, Viper 1-1, request clearance to the range.
CLR:   Viper 1-1, Andersen Clearance, clearance on request, standby.
  (10 to 20 seconds later)
CLR:   Viper 1-1, Andersen Clearance, I have your clearance, advise ready to copy.
You:   Viper 1-1, ready to copy.
CLR:   Viper 1-1, Andersen Clearance, cleared to the range complex via radar vectors,
       fly runway heading, climb and maintain 4,000, expect FL220 10 minutes after
       departure, departure frequency 363.8, squawk 4201.
You:   Cleared to the range, 4,000, 363.8, squawk 4201, Viper 1-1.
CLR:   Viper 1-1, readback correct, contact Andersen Ground 275.8 when ready to taxi.
You:   Andersen Ground, Viper 1-1, request taxi.
GND:   Viper 1-1, Andersen Ground, runway 06R, taxi via Bravo, Echo, hold short
       runway 06R at Echo, wind 070 at 12, altimeter 2992.
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
- **They say "standby".** When a frequency is busy, a taxi request or a
  check-in gets "Viper 1-1, standby", and the answer comes when there is a
  gap. Now and then a quiet controller does the same, finishing something
  else first. Range Control sometimes checks the schedule before booking you.
- **Voices that sound like people.** See [Voices](#voices).
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

## Ground traffic

Ground works everyone on the taxiways, not just the one it is talking to:

- **Every taxi clearance names the route and the holding point**: "runway
  24L, taxi via Alpha, Kilo, hold short runway 24L at Kilo". From the north
  ramp it stops you short of 06L/24R first and clears you across when the
  runway is free.
- **Traffic crossing ahead, or coming the other way**: "hold position,
  traffic crossing left to right, a KC-135 on Bravo", then "traffic has
  passed, continue taxi".
- **Following**: "follow the C-17 ahead" when you end up behind someone
  going your way.
- **AI departures wait on their stands** while a player is taxiing where
  they would go ("Shell 1-1, hold position, traffic taxiing on Bravo, expect
  taxi in three minutes"). DCS gives no way to stop AI once it is rolling,
  so Ground holds it before it starts.

Tower keeps the order: departures go in the order they called ready ("hold
short runway 06R, number 2 for departure"), a second one is never cleared
while the first is still on the runway, and after a heavy goes there is
"caution wake turbulence, departing KC-135". An arrival behind a heavy on
final hears the same for the landing heavy, and a number 2 in the overhead
is told who to follow ("number 2, follow the C-17 on a 3 mile final").

Approach and Departure call traffic: "Viper 1-1, traffic, 2 o'clock, 3
miles, crossing left to right, a C-130, 4,000."

If the taxi network does not line up with DCS's runways, bfdb's log says
by how much and routes are still given from the network; if a stand is
too far from any taxiway for a route, the log says where it is.

## When things go wrong

- **Stopped on a runway.** After 25 seconds (two minutes if you were told
  to line up and wait), Tower asks "say status, you are stopped on runway
  06R". A minute later it tells you to vacate at the next taxiway. If you
  are still there two and a half minutes on, the runway is **closed**:
  "All aircraft, Andersen Tower, runway 06R is closed, disabled aircraft on
  the runway. Runway 06L in use." It reopens when you move off it.
- **A crash on the runway.** An aircraft lost at speed on a runway, or low
  over it, closes the runway for 10 minutes for the emergency vehicles.
- **While a runway is closed**, ATIS reads it out (with a new letter),
  arrivals and departures move to the parallel runway, and anyone on final
  for the closed one is sent around.
- **Traffic stopped on a taxiway.** A player stopped on a taxiway for 90
  seconds is asked their status. A player taxiing up behind gets new taxi
  instructions around it ("amend taxi instructions, traffic stopped on
  Echo, taxi via Charlie, Delta, hold short runway 06R"), or, when there is
  no way round, "hold position, traffic stopped on Echo ahead", and "traffic
  has moved, continue taxi" once it goes.
- **Taxiing without a clearance** gets "hold position and say request".
- **Landing without a clearance** gets "possible pilot deviation".

These two only go to pilots who have been working the controllers: a
player who never tunes in is not told off on a frequency they are not on.

## Voices

Out of the box the controllers speak with Piper (or Windows SAPI), which is
free and runs on the box but sounds synthetic. For voices that sound like
people, give them a speech service under `range_atc` → `tts`:

| `engine` | Sounds | Cost | Needs |
|---|---|---|---|
| `openai` | Very natural. `gpt-4o-mini-tts` is also told how each position talks (a brisk tower, a laid-back range officer, an urgent LSO). | About $0.015 a minute of speech | An OpenAI API key |
| `elevenlabs` | The most human. Uses the voices in your ElevenLabs account. | From $5 a month, by characters | An ElevenLabs API key |
| `kokoro` | Natural, much better than Piper. | Free | [Kokoro-FastAPI](https://github.com/remsky/Kokoro-FastAPI) running on the box (port 8880) |

```yaml
bfdb:
  atc_tts_key: "<the service's API key>"   # not needed for kokoro
  instances:
    - kind: range
      gci:
        range_atc:
          tts:
            engine: openai
            # when: audience           # the default for openai/elevenlabs
            # voices: [ash, onyx, echo] # cast from these, busiest positions first
            # pilots: true             # AI pilots get the service's voices too
```

- **Paying only when someone listens.** With `when: audience` (the default
  for the paid services), the service speaks only while a player is on the
  server or someone is listening live on the site. The rest of the time the
  AI traffic keeps talking in the local voices for the transcript, at no
  cost. `when: always` uses it all the time.
- **ATIS** repeats itself every minute: the last couple of dozen things said
  are cached, so it costs one synthesis per letter.
- **If the service fails** (no credit, no network), the local voice speaks
  instead and bfdb's log says why, at most every five minutes.
- Pick a voice per position under `voices:` with `voice:` ("onyx",
  "am_michael", or an ElevenLabs voice id).
- Numbers are said the way US military controllers say them: "three",
  "five" and "niner".

## Range Control, the tankers and the carriers

**Range Control (348.6)** gives everyone a slot, so two flights never work the
same range at once.

```
You:   Range Control, Viper 1-1, request R-7201 bomb circle.
RC:    Viper 1-1, Range Control, cleared into R-7201 FDM Bomb Circle, your slot
       until 2-0, range is cold, report IP inbound.
You:   Viper 1-1, IP inbound.
RC:    Viper 1-1, cleared hot, report off.
  (two minutes before the end)
RC:    Viper 1-1, Range Control, two minutes remaining in your slot.
You:   Viper 1-1, departing the range.
RC:    Viper 1-1, Range Control, cleared off the range, good hits today.
```

A flight asking for a range that is in use hears when its slot starts ("is in
use, expect it at 4-0, hold clear") and which similar range is free now. When
the flight before leaves, or its time runs out, the next one is called in
("R-7201 FDM Bomb Circle is yours, cleared in"). A flight dropping on a range
without a slot is booked into it if it is free, or told to cease if it is not.

**Tankers.** Every tanker on station answers on its own frequency, with its own
callsign ("Texaco 1-1"). Call it for fuel ("Texaco 1-1, Viper 1-1, request
fuel"). The first receiver hears "cleared to join, FL250, heading 090, 280
knots, cleared pre-contact"; the next is "number 2, cleared to the left
observation". Once you are stable astern it calls "cleared contact" (boom) or
"cleared to plug" (basket). When you back off it clears you to depart and calls
the next receiver astern.

**Carriers.** Each Supercarrier or Forrestal gets three controllers:

- **Marshal**, on the carrier's frequency: check in ("Marshal, Ragin 1-1,
  checking in, angels 20, 40 miles") and get a slot. Case I: "hold overhead at
  angels 2, expected Charlie time 0-2". Case III, at night or under cloud:
  "marshal on the 171 radial, 21 DME, angels 6, expected approach time 1-2".
  Slots are 90 seconds apart.
- **Approach**, 0.5 MHz above Marshal: range calls on the way in, then "three
  quarters of a mile, call the ball".
- **Paddles**, on the Approach frequency in a voice of its own: "Roger ball,
  27 knots", then the groove calls (power, right for lineup, wave off) and
  bolter calls.

Override the carrier frequencies under `carriers:` (below).

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

The traffic flies even when nobody is on the server, so the field is
already busy when someone joins. Arrivals are removed after they have
parked, departures once they are 45 nm out. A departure Ground never
releases (ATC not running, say) starts by itself after 8 minutes, so parked
traffic never blocks the stands.

The AI also does the range's own work, through the same controllers:

- **Range sorties.** Fighters take off from Andersen (released by Ground like
  any departure), call Range Control, hold at the IP until their slot, make
  dry passes over the range, and come home through Approach and Tower.
- **Tanker sorties.** Fighters take off and join a tanker that suits them:
  F-16s go to a boom tanker, Hornets to a basket. They take fuel when the
  crew clears them, then come home.
- **Carrier recoveries.** Hornets arrive 40 nm out, check in with Marshal,
  hold for their slot, call the ball and trap. Only Supercarriers and
  Forrestals get recoveries.

Tune it in the range config (`<sortie>_RANGE`):

```json
"atc_traffic": {
  "enabled": true,
  "field": "Andersen",
  "departures_per_hour": 4,
  "arrivals_per_hour": 4,
  "max_active": 4,
  "country": "USA",
  "fly_when_empty": true,
  "self_release_s": 480,
  "range_sorties_per_hour": 2,
  "tanker_sorties_per_hour": 2,
  "carrier_recoveries_per_hour": 2,
  "types": [{"typ": "C-17A", "callsign": "Reach", "weight": 3}]
}
```

`"enabled": false` turns it off. `types` replaces the built-in mix. A rate of
0 switches that kind of sortie off. Range and tanker sorties use the fighters
in `types` (F-16 and Hornet if there are none).

## Listening, and transcripts

- **Live, like LiveATC.** On the range site's **ATC radio** page, pick what
  to listen to under **Listen to**: **All**, one frequency, or several (tap
  each one you want), then press **Listen live**. You hear them as it
  happens: controllers, AI pilots and players, through a radio filter with
  squelch. It is what is on SRS: players' voices are streamed from SRS as
  they talk, and the controllers and AI pilots are the same audio they put
  on SRS. Only the ATC frequencies are on it. The page remembers your pick. Each transmission can
  also be replayed with ▶.
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

The Manager's **New box** wizard does all of this on a fresh range box (the
"Install the range ATC's speech" tick, on by default):

- It writes `SRS.cfg` with External AWACS Mode on.
- It installs whisper.cpp v1.9.4 with `ggml-small.en` and runs
  `whisper-server` on `127.0.0.1:8178`. A scheduled task, "FowlEngine
  whisper-server", starts it at boot as LOCAL SERVICE.
- It installs Piper 2023.11.14-2 with the multi-speaker voice
  `en_US-libritts_r-medium`, so every frequency gets its own speaker.
- It fills in the `gci:` keys below in `fowlengine.yaml`.

whisper.cpp and Piper go under `C:\ProgramData\FowlEngine\speech`. Every
download is a pinned release, checked by sha256. If one fails, the wizard
leaves that key out (the ATC then falls back to SAPI, or to broadcast only)
and lists it as a next step. Running New box again fetches only what is
missing. If it keeps an existing `fowlengine.yaml`, it lists the lines to add.

By hand:

1. **SRS External AWACS Mode.** The controllers join SRS the way an AWACS
   client does. In the range's `Config\SRS.cfg` set:
   ```
   EXTERNAL_AWACS_MODE=true
   EXTERNAL_AWACS_MODE_BLUE_PASSWORD=<a password>
   EXTERNAL_AWACS_MODE_RED_PASSWORD=<another>
   ```
   Restart SRS. Players are not affected.
2. **whisper.cpp**, to listen. Run `whisper-server` with an English model;
   `ggml-base.en` is fine on a GPU, `ggml-small.en` is more accurate. Without
   whisper the controllers still broadcast ATIS but cannot hear anyone.
3. **Piper**, for natural voices. Optional: without it, Windows SAPI speaks.
   With the multi-speaker `en_US-libritts_r-medium` every position gets its
   own speaker. Separate single-voice models in the same folder work too.
   Either way, Ground and Tower sound like different people.
4. **The bot's YAML.** Use `fowlengine.yaml` → `bfdb.instances` → the
   `kind: range` instance:
   ```yaml
   gci:
     enabled: false                 # no GCI on the range; the file is still written
     srs_port: 5005
     blue_eam_password: "<the SRS.cfg password>"
     # what New box writes:
     opus_dll_path: "C:\\Program Files\\DCS-SimpleRadio-Standalone\\ExternalAudio\\opus.dll"
     whisper_server_url: "http://127.0.0.1:8178"
     piper_exe: "C:\\ProgramData\\FowlEngine\\speech\\piper\\piper.exe"
     piper_model: "C:\\ProgramData\\FowlEngine\\speech\\voices\\en_US-libritts_r-medium.onnx"
     range_atc:
       enabled: true
       handoff_out: {name: Overlord, mhz: 251.5}
       voices:                      # optional: pick a speaker yourself
         ground: {speaker: 120}
         final: {speaker: 431, speed: 0.95}
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
| `publish_pilot_audio` | true | Put players' own transmissions on the site's live feed and replays. Off: text only. |
| `tts` | none (Piper/SAPI) | A speech service for the voices: `engine` (openai, elevenlabs, kokoro), `url`, `model`, `voices`, `when` (always, audience), `pilots`. See [Voices](#voices). |
| `range_control` | on, 348.6 | `enabled`, `name` ("Range Control"), `freqs` (`[{mhz: 348.6}]`), `slot_minutes` (20). |
| `tanker_atc` | true | A crew voice on every tanker's frequency. |
| `carrier_atc` | true | Marshal, Approach and Paddles on every carrier. |
| `carriers` | from the carrier | By the engine's carrier id: `{cvn71: {marshal: [{mhz: 305.0}], approach: [{mhz: 305.5}]}}`. |

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
  - `facility.rs`: what every controller has in common (the trait behind
    `airfield.rs`, `rangectl.rs`, `tanker.rs` and `carrier.rs`).
  - `controller.rs`: positions, flight strips and every decision.
  - `airport.rs`: the airfield and taxi routing.
  - `intent.rs`: the phraseology parser.
  - `llm.rs`: the language-model fallback.
  - `speech.rs`: written and spoken forms.
  - `voices.rs`: voice casting.
  - `live.rs`: the live audio feed behind `/api/range/atc/live`.
  - `mod.rs`: SRS, whisper, Piper, the poll loop, and the transcript
    behind `/api/range/atc`.
- `bfrange-web/src/pages/AtcPage.tsx` is the transcript page.
