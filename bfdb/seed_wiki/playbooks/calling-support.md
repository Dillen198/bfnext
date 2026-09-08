# Playbook: Calling AWACS, Tankers & CAP

Once you've earned points (kills, captures, transport, recon) you can spend them
on support missions from the **Actions menu**. This is how a coalition projects
airpower beyond what individual players bring. Full reference:
[Actions Menu](../f10-menu/actions.md),
[Points and Lives](../gameplay/points-and-lives.md).

## The universal workflow

Every Actions deployment works the same way:

1. **Open the F10 _map_** (not the radio menu).
2. Right-click where you want the unit → **Add Mark…** → short name, **≤ 24
   characters** (`AWACS1`, `TANKER`, `CAP`).
3. Slot into an aircraft, press **F10** for the radio menu.
4. `Actions >> → [the mission] → [your marker]`.
5. Unit spawns, points are deducted immediately, it appears on the F10 map.

Marker rules that bite people: name must be **your** mark, **≤ 24 chars**, and
**unique** — duplicate names don't show in the menu.

## What to call, and when

### AWACS — 50 pts (RTB refunds ~25%)

- ~400 km radar picture feeding your whole coalition's
  [EWR](../f10-menu/ewr.md) and [GCI](../gameplay/gci.md).
- Place it **behind your front line**, orbiting where it can see the contested
  airspace but stay out of enemy SAM and fighter reach.
- The single best-value action for a fighter-heavy coalition. Call one early.

### Tanker — check menu for cost (RTB refunds ~25%)

- Aerial refueling; extends everyone's time on station.
- Put it on the likely **transit route** between your main base and the AO, at a
  sensible altitude/speed for the receivers (fast jet vs. A-10).

### CAP (fighters) — 200 pts (RTB refunds 50)

- AI fighters holding a patrol where you mark them.
- Use to **cover an axis you can't personally be on** — a flank, the tanker,
  a transport corridor — not as your primary offense.

### SEAD — 200 pts (RTB refunds 50)

- AI package that hunts enemy SAM radars.
- Mark it on a **known threat area** ahead of a strike push to clear the path.

### Attack helicopters — 200–300 pts

- AI CAS on the marked area. Good for grinding a ground concentration when no
  player is in a CAS jet.

### Recon drone — 50 pts (small) / 100 pts (large)

- Surveillance **and** a [JTAC](./cas-with-jtac.md) in one. Small = 12 km JTAC
  range, large MQ-9 = 18 km, neither needs line of sight.
- **Cannot RTB for a refund** — it stays out for its full duration (12 h).

### Cruise missile platform (ALCM) — 25–150 pts

- Standoff strike platform. Task it through a JTAC against hardened or
  high-value targets. See [ALCM](../advanced/alcm.md).

## Moving and recalling deployed units

- **Waypoint actions:** mark a new spot, then
  `Actions → [Unit] Waypoint → [unit] → [new marker]` to reposition an AWACS,
  tanker, or CAP.
- **RTB:** `Actions → RTB` (or the chat form) recalls an aircraft for a **~25%
  refund**. Do this before a shift change or restart rather than letting it die.
- Recon drones and ground deployables don't RTB; delete ground deployables with
  `-delete <group-id>` for a **50% refund**.

## Spending priority for a new coalition

1. **AWACS** — cheap, helps everyone.
2. **Tanker** — keeps sorties long.
3. **Recon drone / JTAC** over the contested objective — turns your CAS players
   into a threat.
4. **SEAD** ahead of a strike push.
5. **CAP** only to cover a gap you genuinely can't fill with players.

## See also

- [Actions Menu](../f10-menu/actions.md)
- [Points and Lives](../gameplay/points-and-lives.md)
- [Action Types](../reference/action-types.md)
