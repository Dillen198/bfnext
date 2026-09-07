<!-- Copyright (c) 2026 Dillen Weerasinghe. All rights reserved. Proprietary — no license granted. See the repository NOTICE file. -->

# Live GCI (AWACS Radio Calls)

On servers that run it, a **live GCI controller** watches your coalition's
radar picture and calls threats to you **by your pilot name / flight callsign**
over SRS — like a human AWACS/CRC controller, not the F10
[EWR](../f10-menu/ewr.md) text report. It talks on its own; on many servers you
can also key up and ask it questions.

- **Blue** controller is usually **Magic**, **Red** is usually **Overlord**
  (the server sets the names and the frequency).
- Everything it says is **fog-of-war true** — if your coalition's EWR / AWACS /
  fighter radars are not painting a contact, GCI does not know about it either.
- **Players only.** GCI never controls or talks to AI flights.

---

## Getting set up

1. **Find the frequency.** When you take a slot a note appears on screen:
   *"GCI: Magic on 251.0 AM (SRS)"*. It is also on the
   [Briefing](../f10-menu/overview.md) kneeboard under **Radios**.
2. **Tune it in SRS.** Put the GCI frequency on a radio you can hear (COM2 /
   the guard radio / an SRS overlay preset). AM vs FM matters — match what the
   note says.
3. **Fly.** Once you are airborne GCI starts tracking your flight and will
   start calling contacts as they appear.

You do **not** have to check in or ask for anything — the proactive calls come
regardless. Checking in is just good manners and lets you confirm two-way.

---

## Talking to GCI

On servers with **speech recognition** enabled you can key up on the GCI
frequency and ask. Two rules make it work:

- **Say the controller's name first** — *"Magic, …"*. GCI ignores anything not
  addressed to it.
- **Speak clearly, plain brevity.** The recogniser is trained on English
  aircrew phraseology. Short sentences. Numbers as digits ("two seven zero",
  not "two hundred seventy").

GCI leaves a short quiet gap between its own transmissions so you have room to
talk, and it listens before it keys up so it won't step on you.

### What you can ask

| Say | You get back |
|---|---|
| *"Magic, **radio check**"* | *"loud and clear"* — confirms two-way. |
| *"Magic, **checking in**"* / *"…on station"* | *"radar contact, copy the picture, bullseye is …"* plus the current group count. |
| *"Magic, **alpha check**"* | your own position as bullseye bearing/range — a nav sanity check. |
| *"Magic, **bogey dope**"* | your nearest hostile group: bearing, range, altitude, aspect, and what it is. |
| *"Magic, **picture**"* | the whole picture — every group GCI holds for you, nearest first, with a group label. |
| *"Magic, **declare**"* / *"…declare bullseye 270 40"* | **hostile** or **clean** for the contact nearest you (or nearest the bullseye point you named). |
| *"Magic, **snaplock**"* | quick hostile / clean confirmation on the group you're pointing at. |
| *"Magic, **commit**"* | GCI takes you onto the nearest group and starts feeding **intercept vectors** — see below. |

### A worked intercept

```
You:    Magic, Viper 1-1, checking in, angels 25.
Magic:  Viper 1-1, Magic, radar contact, copy the picture, bullseye is DALLAS.
Magic:  Viper 1-1, single group, BRAA 040 for 38, 22 thousand, hot, hostile Flankers.
You:    Magic, Viper 1-1, bogey dope.
Magic:  Viper 1-1, single group, BRAA 042 for 30, 22 thousand, hot, hostile Flankers.
You:    Magic, Viper 1-1, commit.
Magic:  Viper 1-1, Magic, come right 055, single group, 25 miles, 22 thousand, hot.
Magic:  Viper 1-1, come left 040, single group, 14 miles, 22 thousand, hot.
Magic:  Viper 1-1, 6 miles.
Magic:  Viper 1-1, merged, merged.
   …after the fight…
Magic:  Viper 1-1, splash, 040 for 22.
Magic:  Viper 1-1, clean, resume CAP.
```

Under **commit**, GCI recomputes a lead-collision heading every few seconds and
gives you turn calls ("come left / come right / continue heading") until you're
inside 3 miles ("merged"), then hands control back when the group is dead or
runs away ("clean, resume CAP").

---

## Tuning your own calls

Set these in-game with chat commands, or via **F10 → EWR → GCI Voice**:

| Command | Effect |
|---|---|
| `-gci` | Show your current GCI settings |
| `-gci on` / `-gci off` | Unmute / mute GCI calls to you (proactive **and** answers) |
| `-gci imperial` / `-gci metric` | Range in nautical miles + altitude in thousands of feet, **or** kilometres + metres |
| `-gci braa` | Position given as bearing/range **from your jet** (default) |
| `-gci bulls` | Position given as bearing/range **from the bullseye** |
| `-gci clock` | Position given as a **clock code + high/low** relative to your nose |
| `-gci auto` | Follow the server defaults |

Your choice sticks with your pilot across slots and sessions.

---

## Calls you will hear (unprompted)

| Call | Meaning |
|------|---------|
| *"…, threat, single group, BRAA 340 for 12, 18 thousand, hot"* | A hostile is close and dangerous (≈10 nm hot, or ≈5 nm any aspect). Highest priority — it jumps the quiet timer. |
| *"…, bogey, BRAA 090 for 38, 22 thousand, flank"* | A fresh radar hit, no ID yet. It **ripens**: after ~15 s *"bandit Flanker"*, then *"hostile Flanker"* once the track is solid. |
| *"…, single group, BRAA 090 for 32, 24 thousand, flank, hostile fighters"* | A new group on your picture (outside threat range). |
| *"…, north group … / south group …"* | When several flights see the same raid, GCI names groups by direction ("north / south / center group") so the whole coalition shares one picture. |
| *"…, single group, … , hot"* re-call | A group you were told about turned hot, closed inside 10 nm, or changed altitude block. |
| *"…, single group cold, extending"* | A threat turned away. |
| *"…, lead group now two ship"* | The group's count changed. |
| *"…, groups splitting, two groups"* / *"groups converging, single group"* | One group broke into several, or several merged into one. |
| *"…, merged"* | A hostile is inside ~3 nm of you. |
| *"…, single group faded, last bullseye 270 for 50"* | A tracked group dropped off radar. |
| *"…, splash, 270 for 12"* | A hostile you were warned about went down. |
| *"…, Magic, SAM launch, 210 for 18, defend, defend"* | An enemy SAM missile is in the air near you. |
| *"…, Magic, medium range SAM threat, 210 for 24, defend"* | A live enemy SAM's engagement ring now covers you. |
| *"all players, Magic, chute observed, bullseye 270 for 30"* | A friendly ejected — CSAR cue. |
| *"all players, Magic, tumbleweed, negative radar"* | The coalition radar net is down. |
| *"all players, Magic, support. Texaco, bullseye 090 for 45, angels 22."* | Where your tanker / AWACS is (periodic). |
| *"…, picture, 2 groups. Lead group bullseye 270 for 45 …"* / *"…, picture clean"* | Periodic situation update. |

---

## Brevity glossary

| Term | Meaning |
|---|---|
| **BRAA** | Bearing, Range, Altitude, Aspect — the four numbers in a contact call. Bearing/range are **from your jet** unless you asked for bullseye. |
| **Bullseye** | A fixed reference point both sides share. "Bullseye 270 for 40" = 40 nm on the 270° radial from it. Your briefing may give it a **codeword** GCI will use instead of the word "bullseye". |
| **Hot / Flank / Beam / Cold** | The contact's aspect relative to you: nose-on / ~45° off / ~90° off (side-on) / tail-on and running. |
| **Angels / thousand** | Altitude in thousands of feet. "Angels 22" = 22,000 ft. |
| **Bogey → Bandit → Hostile** | Unknown → radar-identified threat → confirmed hostile. GCI ripens the ID as the track firms up. |
| **Single group / two ship / heavy** | How many aircraft are flying together as one group. |
| **Threat** | This contact can kill you *now* — react. |
| **Defend** | A weapon is in the air — go defensive immediately. |
| **Merged** | Inside ~3 nm — visual / WVR range. |
| **Faded** | Lost radar contact; last known position given. |
| **Splash** | Confirmed kill. |
| **Clean** | Nothing hostile where you're looking. |
| **Tumbleweed** | GCI has lost its own radar picture — you're on your own sensors. |
| **Picture** | The full tactical air situation, groups nearest-first. |

---

## Notes & limits

- **One flight is served per cycle** and there's a short cooldown between calls
  to any one flight, so the frequency doesn't flood. Threat / SAM / splash /
  vector calls always come through.
- Contact **type** ("Flankers", "Viper", "bomber") is only spoken when your
  network has identified it; otherwise it's just "hostile".
- **Red** GCI speaks the same English brevity in a Russian-accented voice.
- If speech recognition is off on your server, GCI is **broadcast-only** — the
  proactive calls still work, you just can't ask it anything.

## See Also

- [Early Warning Radar](../f10-menu/ewr.md) — the on-demand F10 text picture
- [Actions Menu](../f10-menu/actions.md) — deploy an AWACS to widen coverage
- [Chat Commands](chat-commands.md) — the full `-gci` command list
