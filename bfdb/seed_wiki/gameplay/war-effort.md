# The War Effort Bar

Each round has one **war-effort bar**: a tug of war from 0 to 100 that starts
at 50. You see it at the end of your slot briefing and on the dashboard's
Situation tab:

```dcsmessage
WAR EFFORT: 62/100 for us (perk: takeoff hold 25% shorter)
```

- **Every capture** moves it 4 points toward the side that took the base. A
  base that only falls to neutral doesn't move it.
- **A frozen front breaks.** When two hours or more have passed since the last
  capture by either side, the next capture moves it twice as far (8 points).
- A new round starts it at 50 again.

## The perks

The side the bar favours gets perks you can feel in play. Each side reads the
bar from its own end, so Blue at 70 is Red at 30.

| Your side's effort | Perk |
| --- | --- |
| 60+ | Takeoff hold after taking a slot is 25% shorter |
| 75+ | Actions (AWACS, CAP, strikes, artillery...) also cost 15% less |
| 90+ | Takeoff hold is halved (actions still 15% cheaper) |

The takeoff hold is the wait a server can put on a fresh ground-start slot
({{cfg:takeoff_delay_secs|0}} s here; 0 means none, and then the first perk
has nothing to shorten). The F10 menu still shows the full price of an action;
the 15% comes off when you buy it.

When a side climbs into a new tier, both sides hear about it:

```dcsmessage
WAR EFFORT 76/100: our push has earned shorter takeoff holds and cheaper actions.
```

and the other side gets:

```dcsmessage
The enemy's war effort is at 76/100 -- they now have shorter takeoff holds and cheaper actions. Take bases back to stop it.
```

Dropping back a tier is not announced.

## Last defenders

The last few infantry in a battered town are the slowest part of a capture.
A base is ready for this when all three hold:

- it is down to capture health (20 or less);
- it is under attack;
- it has **8 or fewer** infantry left alive.

When they do, the attacking side gets an F10 map mark on each survivor, reading:

```
Last defenders of Gori: infantry here (3 left)
```

The marks are rough, within about 150 m. Every minute they move to where the
survivors are now, and they disappear as soon as they aren't needed. You still
have to find the soldier, but not search the whole town.

## See Also

- [Capturing Objectives](./capturing-objectives.md)
- [Capturing a Base](../playbooks/capturing-a-base.md)
- [Rear Bases, Era Weapons & Round Length](./round-rules.md)
