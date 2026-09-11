# Comms Plan

One frequency card for the whole server. Blue and red share **no working
channel** — only guard. Your live card, with the stations that are actually up
marked, is on the dashboard under **Briefing → Situation → Comms Card**, on
**F10 → Info → Situation → 6. Comms**, and in the kneeboard PDF.

## Why the bands are split this way

Western jets have a 225–400 MHz AM radio and usually a 118–156 VHF set; the
A-10, Huey and Gazelle add a 30–76 MHz FM ground radio. The Russian R-862 /
R-863 in the MiG-29, Su-27, Su-25, MiG-21 and Mi-8 covers 100–150 and 220–400,
with a 20–60 FM set in the helicopters.

So blue lives high (UHF 251–270) and red lives low (VHF-AM 124–145), each with
one relay in the other's comfortable band and its own FM ground net. Nothing
overlaps, and neither side can sit on the other's control channel by accident.

## Blue

| CH | Freq | Mod | Station |
| --- | --- | --- | --- |
| 1 | 251.000 | AM | **AWACS / GCI — MAGIC**. Primary control: picture, bogey dope, commit, declare |
| 2 | 119.000 | AM | GCI VHF relay — MAGIC. Same controller, for VHF-only airframes |
| 3 | 252.000 | AM | AWACS alternate — DARKSTAR. Second controller / overflow |
| 4 | 253.000 | AM | Tanker **TEXACO** (boom) |
| 5 | 254.000 | AM | Tanker **ARCO** (boom, second track) |
| 6 | 255.000 | AM | Tanker **SHELL** (drogue: Hornet, Tomcat, Harrier, Viggen) |
| 7–10 | 256–259.000 | AM | **JTAC 1–4**. Nine-line, talk-on, laser |
| 11 | 260.000 | AM | **CSAR — SANDY**. On-scene commander and the pickup helo |
| 12 | 265.000 | AM | Package common — **STRIKE** |
| 13 | 266.000 | AM | Package common — **SEAD** |
| 14 | 267.000 | AM | Package common — **CAP** (sweep and escort) |
| 15 | 268.000 | AM | Package common — **CAS** (stack check-in, deconfliction) |
| 16 | 270.000 | AM | Carrier **MARSHAL** (approach, marshal stack, case II/III) |
| 17 | 127.500 | AM | Carrier **TOWER / LSO** (ball call, paddles, deck ops) |
| 18 | 243.000 | AM | **GUARD** — emergency only |
| 19 | 121.500 | AM | **GUARD** (VHF) — emergency only |
| 20 | 30.000 | FM | Ground / logistics net (convoys, crates, warehouse) |
| 21 | 31.000 | FM | Troop & crate ops (helo lift working channel) |

**Intra-flight:** FLIGHT 1–8 on **305.000 → 312.000 AM**, one MHz apart.

## Red

| CH | Freq | Mod | Station |
| --- | --- | --- | --- |
| 1 | 124.000 | AM | **GCI — OVERLORD**. Primary control |
| 2 | 228.000 | AM | GCI UHF relay — OVERLORD. Same controller |
| 3 | 125.000 | AM | AWACS A-50 — **DRAGNET** |
| 4 | 126.000 | AM | Tanker IL-78 — **KUZNETS** (drogue) |
| 5–8 | 133–136.000 | AM | **JTAC 1–4** |
| 9 | 137.000 | AM | **CSAR** — rescue |
| 10 | 142.000 | AM | Package common — **STRIKE** |
| 11 | 143.000 | AM | Package common — **SEAD** |
| 12 | 144.000 | AM | Package common — **CAP** |
| 13 | 145.000 | AM | Package common — **CAS** |
| 14 | 243.000 | AM | **GUARD** — emergency only |
| 15 | 121.500 | AM | **GUARD** (VHF) — emergency only |
| 16 | 40.000 | FM | Ground / logistics net |
| 17 | 41.000 | FM | Troop & crate ops |

**Intra-flight:** FLIGHT 1–8 on **230.000 → 237.000 AM**.

## Reserved — do not assign

| Band | Use |
| --- | --- |
| 243.000 / 121.500 | Guard, both sides. Never used for traffic. |
| 340.0–360.0 AM | Spoken ATC tower, one per field, assigned by field name |
| 370.0–390.0 AM | Spoken ATIS, one per field |

Airfield ATC and ATIS frequencies are **not** DCS's stock numbers — DCS does not
expose them to scripting, so the server assigns its own from the blocks above
and they are on your briefing.

## Etiquette

- **Say the callsign first.** "Magic, bogey dope" — the controller is talking to
  a dozen people.
- **Guard is for emergencies.** Not for "anyone on?"
- **Leave the control channel quiet.** GCI waits for a clear channel before it
  transmits, so a flight chatting on 251.0 delays everyone's threat calls. Move
  flight business to your package or intra-flight channel.
- **Check in on the JTAC's channel, not the JTAC.** The number you want is on
  your briefing, marked UP when one is actually working.

## For server admins

The plan lives in the mission config under `comms_plan` (`blue`, `red`,
`blue_flight_base_mhz`, `red_flight_base_mhz`, `flight_step_mhz`,
`flight_count`). Omit the block entirely to use the built-in defaults, which are
exactly the tables above.

Two things must be kept in step with it by hand, because they are configured
elsewhere:

- **`gci.json` → `blueFreqs` / `redFreqs`** — what the live GCI controller
  actually transmits on. These should match presets 1–2 plus the FM ground net.
- **`gci_briefing` in the mission config** — the slot-entry "GCI: Magic on …"
  note. Cosmetic, but it should say the same numbers.

Everything else — the AWACS, tanker and JTAC radios — the engine reads from the
running action specs and overlays onto the card automatically, so a channel
marked **UP** always shows the frequency the aircraft is really on.
