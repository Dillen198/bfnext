# XP, Levels & Squadrons

Points are what you spend. **XP** is what you keep: it never goes down, it
counts across rounds, and it shows on your pilot page. XP is only earned on
the public campaign servers, not on test or training servers.

## Earning XP

| Event | XP |
| --- | --- |
| Shooting down an enemy pilot | 500 |
| AI aircraft kill | 150 |
| AI ground kill | 60 |
| Capturing an objective | 1,000 |
| Repair | 200 |
| Supply run | 150 |
| Troop insertion | 120 |
| Deploying a unit | 100 |
| Building a FARP | 300 |
| Work inside [your task zone](./your-task.md) | +150 to +300 bonus |

Team kills earn nothing. **AI farming is capped:** after 20 AI kills in an
hour, each further AI kill that hour pays a quarter of the usual XP.

## Levels

There is no level cap. Level *L* needs 250 × *L* × (*L* − 1) XP in total, so
level 2 is at 500 XP, level 3 at 1,500 and level 10 at 22,500.

## Challenges

Three **daily** and three **weekly** challenges are active at a time, the same
for everyone. Each pays its XP once. Dailies reset at 00:00 UTC, and weeklies
reset on Monday. Each period the window of three moves one step along this
pool:

| Daily | XP | Weekly | XP |
| --- | --- | --- | --- |
| Shoot down 2 aircraft | 500 | Shoot down 15 aircraft | 2,500 |
| Destroy 5 ground targets | 400 | Destroy 40 ground targets | 2,000 |
| Capture an objective | 600 | Capture 5 objectives | 3,000 |
| Fly 3 logistics jobs | 500 | Fly 20 logistics jobs | 2,500 |
| Down an enemy pilot | 600 | Down 8 enemy pilots | 3,000 |

An enemy pilot shot down also counts toward "shoot down aircraft". Logistics
jobs are repairs, supply runs, troop insertions, deployments and FARPs.

## Achievements

There are eight tracks with ten tiers each (1, 5, 10, 25, 50, 100, 250, 500,
1,000 and 2,500):

- Fighter Ace
- Mud Mover
- Conqueror
- Lifeline
- Air Assault
- Builder
- Veteran (flight hours)
- Commander (actions called)

They are read from your all-time stats, so your whole history counts.

## Squadrons

You make or join a squadron on the dashboard's **SQUADRONS** page.

- A tag is 2–5 letters or digits. A squadron holds up to 50 pilots.
- You can be in one squadron at a time.
- **A squadron's XP is what its members earned while they were in it.** A
  pilot's history from before they joined doesn't count, so the squadron
  leaderboard can't be won by recruiting veterans.
- The owner can remove members. If the owner leaves, the squadron passes to
  its longest-serving member. When the last member leaves, the squadron is
  disbanded.

## Pilot cards

Your profile on the dashboard's **PILOTS** page has a **Progression** card: your
level, XP and how far to the next level, today's and this week's challenges,
your achievements, and a pilot card image with your squadron tag and headline
stats.
In Discord, `/fe_card` posts it, and `/fe_card @someone` posts another
pilot's. Link your Discord and game accounts first: `/linkme` in Discord, then
`-linkme <token>` in DCS chat (that one is DCSServerBot's command, not the
engine's).

## After you land

Every landing gets a debrief in the cockpit. It says whether the landing
counted, what the sortie did, and what it settled:

```dcsmessage
LANDED at Kutaisi -- landing registered, safe to leave the slot
Sortie: 42 min from Senaki-Kolkhi
Weapons fired: 4
Contributed: supplied Kutaisi, inserted Infantry Squad
Kills: 2 (+9 pts): 1x T-72B, 1x BMP-2
Airframe/stores: 160 charged at takeoff, 100 refunded for what's still aboard
Balance: 1840 pts
Lives: standard 3/3 (life returned)
```

The **Contributed:** line lists what you did on that sortie (captured,
repaired, supplied, inserted troops, deployed units, built FARPs). It lists up
to six, then "and N more". A landing away from a friendly objective starts
`LANDED outside a friendly objective -- no landing credit here` instead; see
[Points and Lives](./points-and-lives.md).

## See Also

- [Points and Lives](./points-and-lives.md)
- [Your Task & Rookie Mode](./your-task.md)
