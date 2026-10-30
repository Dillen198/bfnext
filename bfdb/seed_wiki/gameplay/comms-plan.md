# Comms Plan

One frequency card per coalition. Blue and red share **no working channel**,
only guard. Your live card, with the stations that are actually up marked, is
on **F10 → Info → Situation → 6. Comms**, on the dashboard under
**BRIEFING → Situation → Comms Card**, and in the kneeboard PDF.

The tables below are the engine's built-in plan, with this server's numbers
filled in where it sets its own. If they ever disagree with the in-game Comms
page, the in-game page is right.

## Why the bands are split this way

Western jets have a 225–400 MHz AM radio and usually a 118–156 VHF set; the
A-10, Huey and Gazelle add a 30–76 MHz FM ground radio. The Russian R-862 /
R-863 in the MiG-29, Su-27, Su-25, MiG-21 and Mi-8 covers 100–150 and 220–400,
with a 20–60 FM set in the helicopters.

So blue lives high (UHF 251–270) and red lives low (VHF-AM 124–145), each with
one relay in the other's comfortable band and its own FM ground net. Nothing
overlaps, and neither side can sit on the other's control channel by accident.

## Blue

| CH | Freq (MHz) | Mod | Station |
| --- | --- | --- | --- |
| 1 | {{cfg:comms_plan.blue[0].freq_mhz|251.000}} | AM | **AWACS / GCI: MAGIC**. Primary control: picture, bogey dope, commit, declare |
| 2 | {{cfg:comms_plan.blue[1].freq_mhz|119.000}} | AM | GCI VHF relay: MAGIC. Same controller, for VHF-only airframes |
| 3 | {{cfg:comms_plan.blue[2].freq_mhz|252.000}} | AM | AWACS alternate: DARKSTAR. Second controller / overflow |
| 4 | {{cfg:comms_plan.blue[3].freq_mhz|253.000}} | AM | Tanker **TEXACO** (boom) |
| 5 | {{cfg:comms_plan.blue[4].freq_mhz|254.000}} | AM | Tanker **ARCO** (boom, second track) |
| 6 | {{cfg:comms_plan.blue[5].freq_mhz|255.000}} | AM | Tanker **SHELL** (drogue: Hornet, Tomcat, Harrier, Viggen) |
| 7 | {{cfg:comms_plan.blue[6].freq_mhz|256.000}} | AM | **JTAC 1**. Nine-line, talk-on, laser |
| 8 | {{cfg:comms_plan.blue[7].freq_mhz|257.000}} | AM | **JTAC 2** |
| 9 | {{cfg:comms_plan.blue[8].freq_mhz|258.000}} | AM | **JTAC 3** |
| 10 | {{cfg:comms_plan.blue[9].freq_mhz|259.000}} | AM | **JTAC 4** |
| 11 | {{cfg:comms_plan.blue[10].freq_mhz|260.000}} | AM | **CSAR: SANDY**. On-scene commander and the pickup helo |
| 12 | {{cfg:comms_plan.blue[11].freq_mhz|265.000}} | AM | Package common: **STRIKE** |
| 13 | {{cfg:comms_plan.blue[12].freq_mhz|266.000}} | AM | Package common: **SEAD** |
| 14 | {{cfg:comms_plan.blue[13].freq_mhz|267.000}} | AM | Package common: **CAP** (sweep and escort) |
| 15 | {{cfg:comms_plan.blue[14].freq_mhz|268.000}} | AM | Package common: **CAS** (stack check-in, deconfliction) |
| 16 | {{cfg:comms_plan.blue[15].freq_mhz|270.000}} | AM | Carrier **MARSHAL** (approach, marshal stack, case II/III) |
| 17 | {{cfg:comms_plan.blue[16].freq_mhz|127.500}} | AM | Carrier **TOWER / LSO** (ball call, paddles, deck ops) |
| 18 | {{cfg:comms_plan.blue[17].freq_mhz|243.000}} | AM | **GUARD** (UHF): emergency only |
| 19 | {{cfg:comms_plan.blue[18].freq_mhz|121.500}} | AM | **GUARD** (VHF): emergency only |
| 20 | {{cfg:comms_plan.blue[19].freq_mhz|30.000}} | FM | Ground / logistics net (convoys, crates, warehouse) |
| 21 | {{cfg:comms_plan.blue[20].freq_mhz|31.000}} | FM | Troop & crate ops (helo lift working channel) |

**Intra-flight:** FLIGHT 1 to FLIGHT {{cfg:comms_plan.flight_count|8}} start at
**{{cfg:comms_plan.blue_flight_base_mhz|305}} MHz AM**, one every
{{cfg:comms_plan.flight_step_mhz|1}} MHz.

## Red

| CH | Freq (MHz) | Mod | Station |
| --- | --- | --- | --- |
| 1 | {{cfg:comms_plan.red[0].freq_mhz|124.000}} | AM | **GCI: OVERLORD**. Primary control |
| 2 | {{cfg:comms_plan.red[1].freq_mhz|228.000}} | AM | GCI UHF relay: OVERLORD. Same controller |
| 3 | {{cfg:comms_plan.red[2].freq_mhz|125.000}} | AM | AWACS A-50: **DRAGNET** |
| 4 | {{cfg:comms_plan.red[3].freq_mhz|126.000}} | AM | Tanker IL-78: **KUZNETS** (drogue) |
| 5 | {{cfg:comms_plan.red[4].freq_mhz|133.000}} | AM | **JTAC 1** |
| 6 | {{cfg:comms_plan.red[5].freq_mhz|134.000}} | AM | **JTAC 2** |
| 7 | {{cfg:comms_plan.red[6].freq_mhz|135.000}} | AM | **JTAC 3** |
| 8 | {{cfg:comms_plan.red[7].freq_mhz|136.000}} | AM | **JTAC 4** |
| 9 | {{cfg:comms_plan.red[8].freq_mhz|137.000}} | AM | **CSAR**: rescue |
| 10 | {{cfg:comms_plan.red[9].freq_mhz|142.000}} | AM | Package common: **STRIKE** |
| 11 | {{cfg:comms_plan.red[10].freq_mhz|143.000}} | AM | Package common: **SEAD** |
| 12 | {{cfg:comms_plan.red[11].freq_mhz|144.000}} | AM | Package common: **CAP** |
| 13 | {{cfg:comms_plan.red[12].freq_mhz|145.000}} | AM | Package common: **CAS** |
| 14 | {{cfg:comms_plan.red[13].freq_mhz|243.000}} | AM | **GUARD** (UHF): emergency only |
| 15 | {{cfg:comms_plan.red[14].freq_mhz|121.500}} | AM | **GUARD** (VHF): emergency only |
| 16 | {{cfg:comms_plan.red[15].freq_mhz|40.000}} | FM | Ground / logistics net |
| 17 | {{cfg:comms_plan.red[16].freq_mhz|41.000}} | FM | Troop & crate ops |

**Intra-flight:** FLIGHT 1 to FLIGHT {{cfg:comms_plan.flight_count|8}} start at
**{{cfg:comms_plan.red_flight_base_mhz|230}} MHz AM**, one every
{{cfg:comms_plan.flight_step_mhz|1}} MHz.

## The live card in game

**F10 → Info → Situation → 6. Comms** is the card with the live picture laid
over it:

```dcsmessage
=== SITUATION 6/6 -- Comms (Blue 14:05) ===
PRESET  FREQ       WHO
   1   251.000 AM  AWACS / GCI -- MAGIC
         GCI: Magic on 251.0 AM (SRS)
   2   119.000 AM  GCI VHF relay -- MAGIC
   3   252.000 AM  AWACS alternate -- DARKSTAR
   4   253.000 AM  Tanker TEXACO (boom)  [UP]
         TANKER Texaco -- TACAN 51Y TEX
   5   254.000 AM  Tanker ARCO (boom)
   6   255.000 AM  Tanker SHELL (drogue)
   7   256.000 AM  JTAC 1  [UP]
         JTAC 12 -- laser 1688 near Senaki
   8   257.000 AM  JTAC 2
...
  20    30.000 FM  Ground / logistics net
  21    31.000 FM  Troop & crate ops

INTRA-FLIGHT:
  FLIGHT 1 305.000   FLIGHT 2 306.000   FLIGHT 3 307.000   FLIGHT 4 308.000   FLIGHT 5 309.000   FLIGHT 6 310.000   FLIGHT 7 311.000   FLIGHT 8 312.000
```

How the live stations are matched onto the plan:

- A channel whose name says **AWACS** or **GCI** gets the next AI AWACS your
  side has up, **TANKER** the next tanker, **JTAC** the next active JTAC, in
  the order they came up. A **relay** channel shows the same station as the
  line above it.
- A matched channel is marked **[UP]** and the line under it names the actual
  aircraft or JTAC: its TACAN, or its laser code and where it is.
- For an AWACS or tanker the line shows the **aircraft's real radio frequency**,
  which wins over the planned one. A JTAC keeps the planned frequency; the
  note gives its laser code.
- Tankers are handed out in order, not by type: the first tanker up sits on
  the TEXACO line even if it is a drogue tanker. Read the note under it.
- **[UP] on channel 1 means an AI AWACS is airborne**, not that the live GCI
  controller is on. If the server configures a GCI note and no AWACS is up,
  channel 1 carries that note instead (`GCI: Magic on 251.0 AM (SRS)`).

The slot-entry briefing panel ends with the first line of this card. See
[The Auto-Generated Briefing](./briefing.md).

## Reserved: do not assign

| Band | Use |
| --- | --- |
| 243.000 / 121.500 | Guard, both sides. Never used for traffic. |
| 340.0 and up, AM | Spoken ATC tower, one per field (when the server runs voice ATC) |
| 370.0 and up, AM | Spoken ATIS, one per field (when the server runs voice ATC) |

The spoken tower and ATIS frequencies are the server's own, handed out
alphabetically by field name in 0.5 MHz steps, so a field keeps its
frequencies whoever owns it. The server can set any field's frequencies by
hand. See [Weather, ATIS & Time](./weather.md).

The **text ATIS** you get on spawn lists something different: DCS's own
stock tower frequencies for the field, read from the terrain. Those are where
DCS's built-in ATC answers.

## Etiquette

- **Say the callsign first.** "Magic, bogey dope". The controller is talking
  to a dozen people.
- **Guard is for emergencies.** Not for "anyone on?"
- **Leave the control channel quiet.** The voice GCI waits for a clear channel
  before it transmits, so a flight chatting on the GCI frequency delays
  everyone's threat calls. Move flight business to your package or
  intra-flight channel.
- **Check in on the JTAC's channel, not the JTAC.** The number you want is on
  your briefing, marked [UP] when one is actually working.

## For server admins

The plan lives in the engine config under `comms_plan` (`blue`, `red`,
`blue_flight_base_mhz`, `red_flight_base_mhz`, `flight_step_mhz`,
`flight_count`). Omit the block entirely to use the built-in defaults, which
are exactly the tables above. Each channel has `preset`, `label`, `freq_mhz`,
`modulation` (`AM` or `FM`) and an optional `purpose`; the live-station
matching keys off the words AWACS, GCI, TANKER, JTAC and RELAY in the label.

Two things must be kept in step with it by hand, because they are configured
elsewhere:

- **`gci.json` → `blueFreqs` / `redFreqs`**: what the live GCI controller
  actually transmits on. These should match presets 1–2 plus the FM ground net.
- **`gci_briefing` in the engine config**: the slot-entry
  "GCI: Magic on …" line. Cosmetic, but it should say the same numbers.

The AWACS and tanker radios are read from the running action specs and laid
over the card automatically, so an [UP] AWACS or tanker line always shows the
frequency the aircraft is really on.
