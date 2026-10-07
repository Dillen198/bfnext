# Theatre HQ: the AI Commander

Each side has an AI commander, the **Theatre HQ**. It reads the war the way
your side can see it and decides what to do about it. Then it runs the
missions and the logistics itself: air packages, artillery and missile fires,
supply convoys and helicopter supply runs, troop insertions and reinforcement
convoys. It also points the ground formations at its main effort, and posts
its asks on your side's tasking board.

It fills the gaps; it does not fly over you. The more of you there are, the
less it does itself, and it backs off fastest on the jobs you are already
doing: fixed-wing pilots in the air mean fewer AI air packages, helicopter
pilots in the air mean fewer AI supply runs.

## Where to see it

- **F10 > Info > HQ > Commander's Intent**: the posture, the main effort,
  the bases to hold, what gets resupplied first, and who set the plan.
- **F10 > Info > HQ > Operations**: what the HQ has under way, and the
  support requests waiting.
- **`-hq`** in chat shows the same intent, and **`-hq ops`** the operations.
- The dashboard's **THEATRE HQ** page shows all of it on a map, with the
  HQ's record of which kinds of operation have worked.

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

When the main effort changes, the whole side gets a message. So does every
operation the HQ launches.

## How it decides

Every couple of minutes (every 30 seconds while one of your bases is being
captured) the HQ does three things:

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
   first, until the HQ's share of the treasury for that pass runs out.

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
| **Supply convoy / helo supply** | Our bases low on supply or fuel, emptiest and closest to the front first. A helo goes where the road is cut |
| **Helo troop insertion** | Enemy bases with no logistics left, or our own about to fall |
| **Reinforcement convoy** | Rebuilding the garrisons of the bases we are holding |
| **Bomber strike** | A friendly JTAC is lasing a target at the main effort, or one closing on a base we are holding |
| **AWACS / tanker** | Kept on station behind the front while there is an air war to support |
| **Naval strike** | Enemy logistics, factories and airbases in range of our carrier group |
| **Air logistics repair** | Our bases whose logistics are shot up |

**Strikes fly as packages.** A bomber always goes with a fighter escort, and
an attack-aircraft strike gets one when enemy aircraft are about. Once both
are in the air the escort is tasked to stay with the bomber and fight off
anything that comes for it. When our intel holds air defence near the target,
a SEAD flight goes in first. Every aircraft starts on the ground at a
friendly airfield and takes off; nothing appears in the air.

**The right aircraft for the job.** A server can give the HQ several
aircraft for each job, and the HQ picks the one that fits the target: heavy
fighters when the radar shows a lot of enemy air and lighter ones when it
doesn't, attack helicopters against armour only where there is no known air
defence and only within their range, day-only aircraft never at night, and
dedicated strike jets for bases. The dashboard's operations list shows what
went.

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

## Asking for support

Any pilot can ask the HQ for something:

- **F10 > Info > HQ > Request Support** asks for it at the nearest objective
  that fits. CAS, SEAD, fires, recon and troops go at the nearest enemy
  objective; CAP and resupply go to the nearest friendly one.
- **`-request <cas|cap|sead|recon|fires|supply|troops> [objective]`** in chat,
  e.g. `-request cas Gori` or `-request supply` (nearest friendly base).
- **`-request cancel <id>`** withdraws one.

A request doesn't buy anything by itself. It makes the operation that answers
it worth much more to the HQ, so if your side can afford it, it gets tasked at
the next planning pass and you are told. You hear again when that operation
ends. If nothing can be done (no treasury, nothing in range), the request
expires after {{cfg:smart_commander.hq.requests.ttl_secs|1200}} seconds and
you get a message saying so. Each pilot can ask once every
{{cfg:smart_commander.hq.requests.cooldown_secs|300}} seconds.

## Strategy from above

Two things can set the plan instead of the HQ's own judgement, field by field.
Whatever they leave out, the HQ still decides.

- **The strategist.** On servers that have one, a language model looks over
  the HQ's picture every quarter of an hour or so and sets the posture, the
  main effort, the priorities and the commander's intent you see in game. It
  is shown exactly what your side can see, nothing more. Its orders expire on
  their own, and if it is down the HQ simply carries on by its rules.
- **A human commander.** Commanders (pilots who have reached the server's
  commander rank, or been made one by an admin) and admins can take command
  from the dashboard's THEATRE HQ page or from chat:

| Command | Effect |
|---|---|
| `-hq posture <offensive\|balanced\|defensive>` | Set the posture |
| `-hq effort <objective>` | Set the main effort |
| `-hq defend <objective>` | Put a base at the top of the hold list |
| `-hq supply <objective>` | Put a base at the top of the resupply list |
| `-hq avoid <objective>` | Send nothing at it |
| `-hq pause` / `-hq resume` | Stop / restart the HQ's own planning (what's under way carries on) |
| `-hq cancel <op id>` | Call off an operation (air packages are sent home) |
| `-hq clear` | Hand command back to the HQ |

Human orders outrank the strategist, and stand for up to
{{cfg:smart_commander.hq.override_max_secs|7200}} seconds unless handed back
sooner. Each order adds to the ones already standing, so `-hq posture
defensive` then `-hq effort Gori` keeps both.

## The money

Everything the HQ does comes out of your side's **Smart Commander treasury**,
which earns income over time. A reserve of
{{cfg:smart_commander.action_reserve|300}} is never spent. While the HQ is
running, the passive point drip into damaged bases only takes
{{cfg:smart_commander.hq.objective_funding_share|0.5}} of the income, so the
HQ always has something left to act with.

Related: [Tasking Board](../gameplay/tasking-board.md) (where the HQ posts
its asks), [AI Helo Missions](../advanced/helo-missions.md),
[Ground War](../advanced/ground-war.md) (the formations the HQ steers) and
[Logistics](../gameplay/logistics.md).
