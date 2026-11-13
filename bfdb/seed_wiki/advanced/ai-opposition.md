# AI Opposition: CAP and Helo Patrols

The campaign puts AI up against you on its own, without a commander spending a
point on it. Two systems do this, and they are deliberately separate: one
answers jets, one answers helicopters.

If you are flying and the other side is thin, expect company.

## Reactive CAP: jets answering jets

AI fighters scramble from the **nearest owned airbase** when either of these is
true for the defending side:

- **A threat.** Its radar network (EWR, AWACS and SAM search radars) is
  painting at least {{cfg:campaign_events.cap_min_threat_count|2}} enemy
  **fixed-wing players**. Contacts within
  {{cfg:campaign_events.cap_trigger_radius_m|90000}} m of each other count as
  one incursion, and one flight answers each incursion, biggest first. AI
  aircraft never trigger a scramble. A server can set
  `cap_trigger_on_known_players` to count every enemy jet in the air instead,
  painted or not.
- **An imbalance.** The other side has
  {{cfg:campaign_events.cap_balance_gap|2}} or more fixed-wing players airborne
  than it does. Four Blue jets against one Red jet gets Red a CAP, even with
  nothing specific detected.

Not every incursion is met on the same beat. The chance that the CAP launches
at once is {{cfg:campaign_events.cap_probability|0.35}} (where 1 means
always); otherwise it launches 2 to 8 minutes into your attack.

They start **cold**: parked with the engines off, AI running its own startup,
then taxi and take off for real. That is deliberate. The minutes it costs them
are the defending side's reaction delay and your warning. A flight that
appears in the air instead of taxiing out is scrapped.

Once up they climb to {{cfg:campaign_events.cap_altitude_m|8000}} m and hold a
station over the contact their side is actually painting, re-stationing as it
moves. They push no more than
{{cfg:campaign_events.cap_max_push_m|60000}} m from the base they defend and
chase contacts up to {{cfg:campaign_events.cap_engage_radius_m|45000}} m from
their station. A wave is up for
{{cfg:campaign_events.cap_duration_secs|600}} seconds. With nothing to work
for {{cfg:campaign_events.cap_idle_rtb_secs|240}} s they go home early rather
than loiter.

They also do not break off a fight because a timer expired: a flight with a
contact inside its engage radius keeps earning
{{cfg:campaign_events.cap_engaged_extension_secs|180}} s at a time, up to
double its normal time on station.

### What you are told

The side being scrambled against hears about it, with the direction measured
from the middle of the battlefield and the size of the incursion that
triggered it:

```dcsmessage
THREAT: Enemy CAP scrambled to the Northeast - 3 aircraft detected by EWR!
```

Your F10 map also gets a solid circle, about 25 km across, around the field
the flight launched from, labelled `Enemy CAP [Red] — ACTIVE`. It goes away
when the wave ends.

### How many, and how often

- One wave per airfield at a time, at most
  {{cfg:campaign_events.cap_max_per_side|2}} per side and
  {{cfg:campaign_events.cap_max_concurrent|3}} across both sides.
- After a wave ends (shot down **or** timed out and RTB'd) **the airfield it
  launched from** waits {{cfg:campaign_events.cap_respawn_cooldown_secs|1800}} s
  before launching another. That is per base, not per coalition, so the
  answer to a second push is the *next* field over rather than nothing at all.
- What limits the side as a whole is its sortie budget:
  {{cfg:campaign_events.cap_max_sorties_per_hour|6}} reactive scrambles per
  rolling hour (helicopter patrols have their own,
  {{cfg:campaign_events.helo_max_sorties_per_hour|4}}).

So the window is not "half an hour anywhere". It is: the field you just
beat is out of the fight, the coalition is out once it has burned its budget,
and the rest of the hour depends on how hard you keep pushing.

## Helicopter patrols: helos answering helos

CAP ignores helicopters on purpose. Fighters hunting a helo on the deck is
lopsided and not much fun for anyone, and SAMs are meant to cover that job. The
side effect was that a night where **only helo pilots showed up** got no AI
response at all, on either side. Helicopter patrols fix that.

They work like CAP (cold start included, so a patrol scrambled against you is
spooling up on its pad while you are still inbound) with rotary numbers:

| | CAP | Helo patrol |
|---|---|---|
| Answers | fixed-wing players | helicopter players |
| Triggered by | contacts the radar network paints | every enemy helo player in the air (radar rarely paints a helo on the deck) |
| Threat needed | {{cfg:campaign_events.cap_min_threat_count|2}} | {{cfg:campaign_events.helo_min_threat_count|2}} |
| Balance gap | {{cfg:campaign_events.cap_balance_gap|2}} | {{cfg:campaign_events.helo_balance_gap|2}} |
| Launches from | airbases only | **airbases, FARPs and FOBs** |
| Launch delay | random, see above | at once |
| Station altitude | {{cfg:campaign_events.cap_altitude_m|8000}} m above sea level | {{cfg:campaign_events.helo_altitude_agl_m|300}} m **above the terrain** |
| Engages | anything in the air | helicopters and ground units |
| Push limit | {{cfg:campaign_events.cap_max_push_m|60000}} m | {{cfg:campaign_events.helo_max_push_m|30000}} m |
| Engage radius | {{cfg:campaign_events.cap_engage_radius_m|45000}} m | {{cfg:campaign_events.helo_engage_radius_m|15000}} m |
| Time up | {{cfg:campaign_events.cap_duration_secs|600}} s | {{cfg:campaign_events.helo_duration_secs|1800}} s |
| Idle before RTB | {{cfg:campaign_events.cap_idle_rtb_secs|240}} s | {{cfg:campaign_events.helo_idle_rtb_secs|420}} s |
| Up at once (per side / total) | {{cfg:campaign_events.cap_max_per_side|2}} / {{cfg:campaign_events.cap_max_concurrent|3}} | {{cfg:campaign_events.helo_max_per_side|1}} / {{cfg:campaign_events.helo_max_concurrent|2}} |
| Between waves (per field) | {{cfg:campaign_events.cap_respawn_cooldown_secs|1800}} s | {{cfg:campaign_events.helo_respawn_cooldown_secs|900}} s |

Two differences matter to you in the air.

**They come from close.** A patrol does not need a runway, so it launches from
whichever owned Airbase, FARP or FOB is nearest the fight. That is often a FOB
a few minutes away, not a rear airbase half the map back.

**They will shoot at your troops.** An armed patrol engages ground units inside
{{cfg:campaign_events.helo_engage_radius_m|15000}} m of its station, so a
squad you just landed is a target, not just your helo. What they will *not* do
is chase a fast mover: jets are off their target list.

```dcsmessage
THREAT: Enemy attack helicopters inbound from the West - 2 contact(s)!
```

The F10 circle for a patrol is smaller, about 12 km, and reads
`Enemy helo patrol [Red] — ACTIVE`.

The two systems never block each other. A helicopter patrol up over a FOB does
not stop the same side scrambling CAP against jets, and it does not consume a
CAP slot.

## Commander CAP

Separate from the reactive scrambles, the AI commander can **buy** a CAP flight
for its own side when the enemy has more jets up. It uses the same templates
and the same cold start, launches from the friendly airbase nearest the most
threatened objective, and shows the same `Enemy CAP [...] — ACTIVE` circle to
the other side. See [Campaign Events](./campaign-events.md#commander-cap).

## Reading it in the log

Both systems log a heartbeat every ten minutes (or whenever the picture
changes) saying why a side is *not* scrambling: the count it can see, what it
needed, and the current balance. If you expect opposition and are not getting
any, that line names the reason: below the threshold, no free launch field
(every nearby base is either already flying or still on its post-wave
cooldown), or the side has spent its sortie budget for the hour.

## Server setup

Both need template groups in the mission file, and neither spawns without them:

- CAP: a **roster** (`cap_templates_red` / `cap_templates_blue`) listing
  **plane**-section groups, each entry carrying a `weight` and a `min_threat`.
- Helo patrols: `helo_templates_red` / `helo_templates_blue`, listing
  **helicopter**-section groups armed for the job you want them doing.

A scramble draws from the entries the incursion is big enough to qualify for,
so a two-ship probe and an eight-ship push stop getting the same flight, and
the same field stops sending the identical jets every time. Entries are tried
in pick order, so a name that is not in the mission is skipped in favour of the
next one instead of cancelling the scramble. You can list the roster you
intend to build and add the groups as you go. A roster of one entry is simply
the same flight every time.

An enabled system with an empty roster is refused at startup rather than
failing quietly at the first scramble.

Two things about the templates are easy to get wrong:

**The helicopter section matters.** The ground-start logic only picks helipads
(and only falls back to open ground at a padless FOB) when the template is
actually a helicopter group. A helo sitting in the plane section air-starts,
and the air-start watchdog then scraps the whole event.

**Copy the CAP templates otherwise.** Late activation on, uncontrolled off, and
a single waypoint is all a CAP template needs. The engine rewrites waypoint 0
into a ground start at whichever field the flight was scrambled from, and
issues the on-station orbit itself once the flight is airborne, so the
template's own route does not need to go anywhere.

**Every unit type must be classified.** A type missing from
`unit_classification` makes the spawn fail outright with `unit type not
classified`.

Turn each on with `enemy_cap_enabled` and `enemy_helo_enabled` under
`campaign_events` (which itself needs `enabled`). Both scramble cold by
default; `cap_cold_start` and `helo_cold_start` set to `false` put them on the
ramp with engines already running, which gets them airborne sooner and gives
the attacking side much less warning. `helo_trigger_on_known_players` is on by
default; `cap_trigger_on_known_players` is off.

The same rosters double as the fallback for [AI wingmen](./air-life.md) when
the server has no wingman templates of its own.
