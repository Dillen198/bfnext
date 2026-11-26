# Playbook: Calling AWACS, Tankers & CAP

Once you've earned points (kills, captures, transport, recon) you can spend them
on support missions from the **Actions menu**. This is how a coalition projects
airpower beyond what individual players bring. Full reference:
[Actions Menu](../f10-menu/actions.md),
[Action Types](../reference/action-types.md),
[Points and Lives](../gameplay/points-and-lives.md).

## The universal workflow

Every Actions purchase that needs a place works the same way:

```steps
[
 {"title":"Drop a map mark","text":"Open the F10 **map** (not the radio menu), right-click where you want the aircraft and choose **Add mark**. A short name helps (`AWACS1`, `TANKER`); a blank mark works too and shows up as `Mark 1`, `Mark 2` and so on. See [map marks](../f10-menu/overview.md#map-marks-are-your-targeting-system)."},
 {"title":"Build the Actions menu","text":"Back in the cockpit, press **F10** and select **`Actions>>`**. That builds the `Actions` submenu with your marks in it. Open F10 again."},
 {"title":"Pick the action","text":"Go into **`Actions`** and pick the entry, for example `AWACS(100 pts)`. The number in brackets is what it costs on this server."},
 {"title":"Pick your mark","text":"Your marks are listed nearest first with bearing and range from you: `AWACS1 045/12km`. Pick yours."},
 {"title":"Read the answer","text":"The top-right box says `action AWACS started` and a points line like `1150(-100) points perform action AWACS`. If it could not start, the box says `could not start AWACS, ...` and why, and you were not charged."},
 {"title":"Watch it take off","text":"The aircraft starts on the ground at the nearest friendly field more than 10 km from your mark, takes off and flies there. Its orbit marks appear on your side's F10 map, labelled with the group id and you as **responsible party**. Frequencies and TACAN are in `F10 → Info → Support & Radios`."}
]
```

If you place a new mark while the menu is open, it resets to `Actions>>`:
select it again to see the new mark.

## Or ask the HQ

On servers running the [Theatre HQ](../advanced/theatre-hq.md) you don't have to
buy everything yourself. **F10 → Info → HQ → Request Support (nearest objective)**, or `-request cas
Gori` in chat, asks your side's AI commander for CAS, CAP, SEAD, recon, fires,
resupply, troops, a tanker or AWACS. It costs you nothing: the HQ pays from the
side's treasury if it judges the request worth it, and tells you when it is
tasked and when it is over.

## What to call, and when

Prices and limits are per server; the table on the
[Actions Menu](../f10-menu/actions.md#what-this-server-offers) page shows this
server's.

### AWACS

- An early-warning radar in the air, feeding your whole coalition's
  [EWR](../f10-menu/ewr.md) and [GCI](../gameplay/gci.md) picture. It flies a
  race track across your mark, side-on to the nearest enemy objective.
- Place it **behind your front line**, where it can see the contested airspace
  but stays out of enemy SAM and fighter reach.
- The single best-value action for a fighter-heavy coalition. Call one early.

### Tanker

- Aerial refuelling; extends everyone's time on station. Boom and basket are
  usually separate entries.
- Put it on the likely **transit route** between your main base and the area of
  operations.

### Fighters (CAP)

- AI fighters orbiting your mark, engaging enemy aircraft within 30 km.
- Use them to **cover an axis you can't personally be on** (a flank, the tanker,
  a transport corridor), not as your main offence.

### SEAD

- AI aircraft orbiting your mark and engaging SAMs, radars and AAA within 15 km.
- Mark it on a **known threat area** ahead of a strike push. It stays until you
  RTB it or its time runs out.

### Attackers

- AI aircraft (often attack helicopters) engaging ground units and aircraft
  within 15 km of your mark. Good for grinding a ground concentration when no
  player is in a CAS jet.

### JTAC drone

- An orbiting drone that is also a [JTAC](./cas-with-jtac.md): it finds and
  lases targets for you, for bombers and for artillery. Its range is set per
  drone action by the server.

### Cruise missile platform (ALCM)

- A stand-off strike aircraft. You fire its missiles through a JTAC; see
  [ALCM](../advanced/alcm.md).

## Moving and recalling what you bought

- **Waypoint actions:** drop a new mark, then
  `Actions → [AWACS / Tanker / Fighters ...] Waypoint → [flight] → [mark]`.
  The flight list shows group ids, with `*` on yours:

```dcsmessage
action AWACS Waypoint started for 5012
```

- **RTB:** `Actions → RTB → [flight] → [mark]` sends it to the friendly
  airbase nearest that mark. When it is back, whoever paid gets **a quarter of
  the action's cost** back, and your side sees:

```dcsmessage
Viper's BAWACS-5012 has RTB'd. points refunded: 25
```

- A flight that is shot down costs its **responsible party** the action's
  penalty on top: `875(-100) points for the loss of action group 5012`.
  Recall it before it is lost, not after.
- Ground deployables don't RTB; delete them with `-delete <group-id>` for half
  their cost back.

## Spending priority for a new coalition

1. **AWACS**: helps everyone.
2. **Tanker**: keeps sorties long.
3. **JTAC drone** over the contested objective: turns your CAS players into a
   threat.
4. **SEAD** ahead of a strike push.
5. **CAP** only to cover a gap you genuinely can't fill with players.

## See also

- [Actions Menu](../f10-menu/actions.md)
- [Action Types](../reference/action-types.md)
- [Points and Lives](../gameplay/points-and-lives.md)
