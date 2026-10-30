# Theatre HQ: the AI Commander

Each side has an AI commander, the **Theatre HQ**. It reads the war the way
your side can see it and decides what to do about it. Then it runs the
missions and the logistics itself: air packages, artillery and missile fires,
supply convoys and helicopter supply runs, troop insertions and reinforcement
convoys. It also points the [ground formations](./ground-war.md) at its main
effort, and posts its asks on your side's
[tasking board](../gameplay/tasking-board.md).

It fills the gaps; it does not fly over you. The more of you there are, the
less it does itself: at {{cfg:smart_commander.hq.gap_fill.full_until_players|2}}
players on your side or fewer it runs at full strength, and by
{{cfg:smart_commander.hq.gap_fill.fade_out_players|16}} it is down to
{{cfg:smart_commander.hq.gap_fill.min_factor|0.25}} of it. It backs off
fastest on the jobs you are already doing: every fixed-wing pilot in the air
cuts what it spends on the air war, and every helicopter pilot what it spends
on supply runs and troops, by
{{cfg:smart_commander.hq.gap_fill.per_human_cut|0.35}} each.

The HQ is set up per server. Where it is off, there is no **HQ** entry under
F10 > Info, and `-hq` answers "there is no HQ on this server".

## Where to see it

In game, open **F10 → Info → HQ**:

```f10menu
{"root":"F10 Other","items":[
 {"label":"Info","items":[
  {"label":"HQ","note":"Only on servers whose HQ commands your side.","items":[
   {"label":"Commander's Intent","says":"HQ -- COMMANDER'S INTENT (OFFENSIVE)\nTake Gori. Defend Senaki. Resupply Kutaisi, Senaki first.\nMain effort: Gori\nHold: Senaki\nResupply first: Kutaisi, Senaki\n(HQ's own assessment)\n3 operation(s) under way, 1 request(s) waiting. Treasury 1840.","note":"The line in brackets says who set the plan: the HQ's own assessment, the strategist's directive, or 'orders from <commander>', with ', HQ planning paused' when a commander has paused it."},
   {"label":"Operations","says":"HQ -- OPERATIONS\n#31 strike on Gori: active (4m ago)\n#30 supply convoy on Kutaisi: succeeded, convoy dispatched (12m ago)\n#29 CAP on Senaki: failed, lost (25m ago)\nrequest #33 CAS at Gori by Viper: waiting","note":"The last twelve operations, newest first, then the requests still waiting. 'Nothing under way.' when there are none."},
   {"label":"Request Support (nearest objective)","items":[
    {"label":"CAS on the nearest enemy objective","says":"HQ copies: CAS at Gori. You will hear back when it is tasked (request #33)."},
    {"label":"CAP over the nearest friendly objective","says":"HQ copies: CAP at Senaki. You will hear back when it is tasked (request #34)."},
    {"label":"SEAD near the nearest enemy objective","says":"HQ copies: SEAD at Gori. You will hear back when it is tasked (request #35)."},
    {"label":"Recon of the nearest enemy objective","says":"HQ copies: recon at Gori. You will hear back when it is tasked (request #36)."},
    {"label":"Fires on the nearest enemy objective","says":"HQ copies: fires at Gori. You will hear back when it is tasked (request #37)."},
    {"label":"Resupply the nearest friendly objective","says":"HQ copies: resupply at Senaki. You will hear back when it is tasked (request #38)."},
    {"label":"Troops to the nearest enemy objective","says":"HQ copies: troops at Gori. You will hear back when it is tasked (request #39)."},
    {"label":"Tanker near the nearest friendly objective","says":"HQ copies: tanker at Senaki. You will hear back when it is tasked (request #40)."},
    {"label":"AWACS near the nearest friendly objective","says":"you can ask again in 212s","note":"One of the refusals: each pilot may ask once every {{cfg:smart_commander.hq.requests.cooldown_secs|300}} seconds."}
   ]}
  ]}
 ]}
]}
```

- **`-hq`** in chat shows the same intent, **`-hq ops`** the operations, and
  **`-hq help`** the list of HQ commands. Chat answers come back in chat.
- The dashboard's **THEATRE HQ** page shows all of it on a map: the
  commander's intent, the HQ's assessment, its resources, the operations your
  side **CAN RUN** with what each costs, its **RECORD** of which kinds have
  worked, the support requests, and the **HQ LOG**.

### Reading the HQ map

```mapsymbols
[
 {"icon": "hq-own", "name": "Diamond in our colour", "text": "One of our bases."},
 {"icon": "hq-enemy", "name": "Diamond in their colour", "text": "One of theirs."},
 {"icon": "hq-neutral", "name": "Grey diamond", "text": "Nobody's."},
 {"icon": "hq-effort", "name": "Orange ring: MAIN EFFORT", "text": "The HQ's main effort: the base it is putting most of its weight on, to take or to save. It pulses."},
 {"icon": "hq-hold", "name": "Halo in our colour: HOLD", "text": "A base the HQ has said must be held. RESUPPLY under a base means it gets supplied first; FALLING means it is being captured."},
 {"icon": "hq-capturable", "name": "Orange halo on an enemy base", "text": "Broken enough to capture now. Send troops."},
 {"icon": "hq-sam", "name": "SAM tag", "text": "Known enemy air defence, from intel. Hover for how many and how old the report is. The HQ sends SEAD with strikes near these."},
 {"icon": "hq-op", "name": "Labels stacked over a base", "text": "Operations the HQ has running against or over it: CAP, STRIKE, SEAD, BOMBER, ARTILLERY, CONVOY, HELO SUPPLY and the rest."}
]
```

When the main effort changes, your whole side is told, and so it is for
every operation the HQ launches (servers can turn these announcements off):

```dcsmessage
HQ: main effort is now Gori (OFFENSIVE).
```

```dcsmessage
HQ: strike tasked on Gori.
```

## How it decides

Every {{cfg:smart_commander.hq.think_secs|120}} seconds (every
{{cfg:smart_commander.hq.emergency_think_secs|30}} seconds while one of your
bases is being captured) the HQ does three things:

1. **Posture.** *Offensive*, *balanced* or *defensive*, based on how much
   ground you hold, how many of your bases are under threat or being taken,
   and how many enemy bases are within reach and weak.
2. **Main effort.** The one enemy objective to concentrate on. It favours
   bases that are worth taking, already weakened, close to ground you hold,
   and above all bases with **no logistics left**, which fall to the first
   troops that reach them. The main effort is sticky: the HQ only switches
   when another target is clearly better, so it doesn't throw half the war at
   one base and half at another.
3. **Operations.** Everything the side could do right now, each one valued
   against the plan. They are bought from your side's treasury, best value
   for money first. One pass starts at most
   {{cfg:smart_commander.hq.max_new_ops_per_think|2}} operations, one of each
   kind, and spends at most
   {{cfg:smart_commander.hq.max_spend_fraction|0.4}} of what the treasury
   holds above its reserve (twice that while a base is falling).

While nobody is on the server, the HQ keeps running what is under way but
plans nothing new, unless the server says otherwise.

### What it runs

| Operation | When the HQ reaches for it |
|---|---|
| **CAP** | Enemy aircraft on our radar near a base we are holding or staging from |
| **Strike** | The main effort, or enemy ground our intel sees closing on one of our bases |
| **SEAD** | Known air defence covering the main effort. A strike into known air defence is a bad bet without SEAD first |
| **Recon** | The main effort, when the side has no fresh intel on it |
| **Artillery** | Enemy bases and fresh contacts in range of our guns |
| **Missile strike** | Enemy logistics hubs, factories and airbases in range of our launchers |
| **Convoy ambush** | Enemy supply convoys on the roads near the front |
| **Supply convoy / helo supply** | Our bases below {{cfg:smart_commander.hq.resupply_below_pct|60}}% supply or fuel, emptiest and closest to the front first. A helo goes where the road is cut |
| **Helo troop insertion** | Enemy bases with no logistics left, or our own about to fall |
| **Reinforcement convoy** | Rebuilding the garrisons of the bases we are holding |
| **Bomber strike** | A friendly JTAC is lasing a target at the main effort, or one closing on a base we are holding |
| **AWACS / tanker** | Kept on station behind the front while there is an air war to support |
| **Naval strike** | Enemy logistics, factories and airbases in range of our carrier group |
| **Air logistics repair** | Our bases whose logistics are shot up |

AI air packages only go at targets within
{{cfg:smart_commander.hq.max_air_range_m|180000}} m of a friendly airbase,
and ground and helicopter operations within
{{cfg:smart_commander.hq.max_ground_range_m|60000}} m of ground your side
holds. A package that has flown
{{cfg:smart_commander.hq.package_lifetime_secs|2700}} seconds is sent home.

**Strikes fly as packages.** A bomber always goes with a fighter escort, and
an attack-aircraft strike gets one when enemy aircraft are about. Once both
are in the air the escort is tasked to stay with the bomber and fight off
anything that comes for it. When your intel holds air defence near the
target, a SEAD flight goes in first. Every aircraft starts on the ground at a
friendly airfield and takes off; nothing appears in the air.

**The right aircraft for the job.** A server can give the HQ several
aircraft for each job, and the HQ picks the one that fits the target: heavy
fighters when the radar shows a lot of enemy air and lighter ones when it
doesn't, attack helicopters against armour only where there is no known air
defence and only within their range, day-only aircraft never at night, and
dedicated strike jets for bases. The dashboard's operations list shows what
went.

**AWACS and tankers** orbit well back: an AWACS at least
{{cfg:smart_commander.hq.support.awacs_standoff_m|120000}} m from the front
and from every enemy base, a tanker at least
{{cfg:smart_commander.hq.support.tanker_standoff_m|90000}} m, and both clear
of known enemy air defence. A relief takes off before the one on station runs
out of time. AI flights low on fuel are sent to a friendly tanker that can
refuel them.

Which of these a server's HQ can run depends on how that server is set up: a
server with no attack-aircraft templates gets no AI strikes, one with no AI
helicopter missions gets no helo supply runs. The dashboard's THEATRE HQ
page lists the operations your side can run (CAN RUN), with what each costs.

The HQ keeps score. A kind of operation that keeps failing, such as strike
packages lost to air defence, is trusted less next time; one that keeps
working is trusted more.

It only ever uses what your side can see. Enemy aircraft count only if your
radars hold them, enemy ground only if your recon, JTACs or special forces
found it.

### Steering the ground war

On most servers the HQ also steers the [ground war](./ground-war.md): the
AI ground commander attacks only while the HQ's posture is not defensive, and
aims at the HQ's main effort.

## Asking for support

Any pilot can ask the HQ for something:

- **F10 → Info → HQ → Request Support (nearest objective)** asks for it at
  the nearest objective that fits: CAS, SEAD, recon, fires and troops at the
  nearest enemy objective, CAP, resupply, tanker and AWACS at the nearest
  friendly one. It has to be within 80 km of you, or you are asked to name
  one.
- **`-request <cas|cap|sead|recon|fires|supply|troops> [objective]`** in
  chat, e.g. `-request cas Gori` or `-request supply` (nearest friendly
  base). `tanker` and `awacs` work too. Part of a name is enough when only
  one objective matches.
- **`-request cancel <id>`** withdraws one of yours.
- The dashboard's THEATRE HQ page has the same requests under **SUPPORT
  REQUESTS** (pick one, pick an objective, **ASK HQ**), once your Discord is
  linked and you fly for that side.

A request doesn't buy anything by itself. It makes the operation that answers
it worth {{cfg:smart_commander.hq.requests.bonus|1.6}} times as much to the
HQ, and brings the next planning pass forward, so if your side can afford it,
it gets tasked and you are told. If something already under way covers it,
you are told that instead.

```dcsmessage
HQ: your CAS request at Gori: strike tasked (HQ strike 31 airborne, 1 escort flight(s))
```

You hear again when that operation ends:

```dcsmessage
HQ: the CAS you asked for at Gori has succeeded
```

If nothing can be done (no treasury, nothing in range), the request expires
after {{cfg:smart_commander.hq.requests.ttl_secs|1200}} seconds:

```dcsmessage
HQ: sorry, nothing available for your CAS request at Gori
```

Each pilot can ask once every
{{cfg:smart_commander.hq.requests.cooldown_secs|300}} seconds, and a side has
at most {{cfg:smart_commander.hq.requests.max_open_per_side|6}} requests
waiting. If someone has already asked for the same thing at the same place,
you are told who.

## Strategy from above

Two things can set the plan instead of the HQ's own judgement, field by field.
Whatever they leave out, the HQ still decides.

- **The strategist.** On servers that have one, a language model looks over
  the HQ's picture every quarter of an hour or so and sets the posture, the
  main effort, the priorities and the commander's intent you see in game. It
  is shown exactly what your side can see, nothing more. Its orders expire on
  their own (after {{cfg:smart_commander.hq.strategist.max_directive_secs|2400}}
  seconds without a fresh one), and if it is down the HQ simply carries on by
  its rules.
- **A human commander.** Commanders (pilots who have reached the server's
  commander rank, or been made one by an admin; see
  [Command](./command.md)) and admins can take command from the dashboard's
  THEATRE HQ page or from chat.

| Command | Effect |
|---|---|
| `-hq posture <offensive\|balanced\|defensive>` | Set the posture |
| `-hq effort <objective>` | Set the main effort (an enemy base) |
| `-hq defend <objective>` | Put one of your bases at the top of the hold list |
| `-hq supply <objective>` | Put one of your bases at the top of the resupply list |
| `-hq avoid <objective>` | Send nothing at it |
| `-hq pause` / `-hq resume` | Stop / restart the HQ's own planning (what's under way carries on) |
| `-hq cancel <op id>` | Call off an operation (air packages are sent home; fires and transports already on their way can't be recalled, the HQ just stops following them) |
| `-hq clear` | Hand command back to the HQ |

The op id is the `#` number in the operations list. The reply comes back in
chat: "orders set", "operation called off", "command handed back", or why
not ("you are not cleared to command this side's HQ"). Taking command and
handing it back are told to your whole side:

```dcsmessage
HQ: Viper has taken command, posture DEFENSIVE
```

```dcsmessage
HQ: Viper has handed command back to the HQ
```

On the dashboard, **TAKE COMMAND** does the same with buttons: a posture, a
main effort and a base to hold (each left on "HQ's call" if you don't pick
one), which kinds of operation the HQ may run, and how long the orders
**STANDS FOR** (1, 2 or 4 hours). Then **ISSUE ORDERS**, **PAUSE PLANNING**
/ **RESUME PLANNING**, or **HAND BACK TO HQ**.

Human orders outrank the strategist, and stand for up to
{{cfg:smart_commander.hq.override_max_secs|7200}} seconds unless handed back
sooner. Each order adds to the ones already standing, so `-hq posture
defensive` then `-hq effort Gori` keeps both. When they run out the HQ goes
back to its own judgement.

## The money

Everything the HQ does comes out of your side's **Smart Commander treasury**,
which earns income over time (see [War Economy](../gameplay/war-economy.md)).
The HQ's own planning never spends the last
{{cfg:smart_commander.action_reserve|300}} of it; a commander's orders from
the dashboard can spend it all. While the HQ is running, the passive point
drip into damaged bases only takes
{{cfg:smart_commander.hq.objective_funding_share|0.5}} of the income, so the
HQ always has something left to act with.

Related: [Tasking Board](../gameplay/tasking-board.md) (where the HQ posts
its asks), [AI Helo Missions](../advanced/helo-missions.md),
[Ground War](../advanced/ground-war.md) (the formations the HQ steers),
[Command](./command.md) (launching the HQ's operations yourself) and
[Logistics](../gameplay/logistics.md).
