# Reconnaissance (Recon Pass)

Fly a recon-capable aircraft near an enemy objective and hold station for a
timed **Recon Pass**. When it completes, the enemy units around that objective
(SAM and AAA sites included) appear on the F10 map for your whole coalition, and
in the [GCI/EWR](./ewr.md) **Ground Intel** report.

## Who can fly it

- The server has to enable player recon. If it is off, the Recon menu never
  appears and the commands answer `Recon is not enabled on this server`.
- Only **recon-capable airframes** get the menu. Which types count is a server
  setting (the airframe's classification carries the `Recon` tag).
- The pass can cost points: **{{cfg:player_recon.cost|0}}** on this server.

## The menu

The Recon menu sits at the top of your F10 menu, with three commands:

```f10menu
{"root":"F10 Other","items":[
 {"label":"Recon","items":[
  {"label":"Start Recon Pass","says":"Recon pass started on Senaki -- hold within 25km for 120s","note":"Starts on the nearest enemy objective, if it is within the server's recon range."},
  {"label":"Cancel Recon Pass","says":"Recon pass cancelled -- 50 of 100 points refunded","note":"With a free pass it just says: Recon pass cancelled"},
  {"label":"Recon Status","says":"Recon on Senaki: 45% (54/120s)","note":"Before a pass: Ready -- nearest enemy objective Senaki at 18km"}
 ]}
]}
```

When the server also offers [wingmen](../advanced/air-life.md), Recon and
Wingman share one **`Flight`** menu to save room on the F10 root, so the path is
**F10 → Flight → Recon**:

```f10menu
{"root":"F10 Other","items":[
 {"label":"Flight","items":[
  {"label":"Recon","items":[
   {"label":"Start Recon Pass","says":"Recon pass started on Senaki -- hold within 25km for 120s"},
   {"label":"Cancel Recon Pass","says":"No recon pass in progress"},
   {"label":"Recon Status","says":"Nearest enemy objective Senaki is 41km away (need 25km)"}
  ]},
  {"label":"Wingman","note":"See Air Life for the wingman commands."}
 ]}
]}
```

The numbers in these replies are the server's own: the pass must start within
**{{cfg:player_recon.range_m|25000}} m** of the objective and takes
**{{cfg:player_recon.dwell_secs|120}} s** of station time.

## Running a pass

1. Slot a recon-capable aircraft and fly toward an enemy objective (an airfield,
   a SAM belt, a FARP).
2. Check **Recon Status**. When it says `Ready -- nearest enemy objective
   <name> at <n>km`, you are close enough.
3. Select **Start Recon Pass**. It always targets the **nearest enemy-held
   objective** to you at that moment.
4. Hold station within range for the dwell time: orbit, or hover in a
   helicopter. Straying out of range ends the pass (see below).
5. If the server allows progressive reveals (the default), you get partial
   results at 25, 50 and 75 %:

```dcsmessage
Recon 50% -- 7 contact(s)
```

6. On completion:

```dcsmessage
Recon complete -- 11 contact(s) on the F10 map
```

The contact count is enemy units revealed in that sweep. After a pass you wait
**{{cfg:player_recon.cooldown_secs|300}} s** before the next one, whether it
completed, was cancelled or aborted.

![Diagram of a recon pass from above: the aircraft orbits inside the recon range ring around an enemy objective, the scan circle around the objective reveals units with line of sight, and a ridge hides units behind it](/api/wiki/images/ffcddaf8-6ac9-4e2a-85ee-489f2c984c20)

### What gets scanned

The scan is centred on the **objective**, not on your aircraft: every living enemy
ground unit within **{{cfg:player_recon.scan_radius_m|12000}} m** of the objective,
whether part of its garrison or a formation out in the field nearby. By default
each unit also needs **terrain line of sight from your aircraft**; units in dead
ground behind a ridge stay hidden. Passes from different angles fill in more.

### What aborts a pass

| Message | Why |
|---|---|
| `Recon aborted -- out of range of the target` | You flew more than 10 % past the recon range from the objective. |
| `Recon aborted -- above the sensor altitude ceiling` | The server sets a ceiling ({{cfg:player_recon.max_altitude_m|0}} m MSL; 0 means none) and you climbed above it. |
| `Recon aborted -- left the aircraft` | You left the slot, ejected or were shot down. |
| `Recon aborted -- switched airframe` | You are no longer in a recon-capable aircraft. |

### Points and refunds

If a pass costs points, they are taken when it starts. Cancelling or aborting
refunds the share of the pass **not yet revealed**: abort before the first
reveal and you get it all back; abort after the 50 % reveal and you get half.
A completed pass refunds nothing.

### Other replies

| Reply | Meaning |
|---|---|
| `Recon pass already in progress -- cancel it first` | One pass at a time. |
| `Recon on cooldown for 212s` | Wait for the cooldown. |
| `Recon pass costs 100 points (you have 40)` | Not enough points. |
| `Nearest enemy objective (Senaki) is 41km away -- must be within 25km` | Fly closer. |
| `No enemy objective nearby` | Nothing enemy-held to scan. |
| `This airframe is not equipped for reconnaissance` | Wrong aircraft. |
| `You must be in an aircraft to run a recon pass` / `Not in an aircraft` | Take a slot first. |

## Reading the results

On completion the scanned area is drawn for your side for two minutes: a
**dotted square** around the objective and a pin reading `RECON` and
`~11 enemy units`.

The lasting result is a set of intel markers, one per cluster of the same kind of
unit (units within {{cfg:elint.contact_cluster_radius_m|500}} m of each other are
merged):

```dcsmessage
[INTEL/RED] 4×ADS | 100% ±0.6km
```

- `RED` / `BLU` is the side the units belong to.
- `4×ADS` is the count and kind: **ADS** air defence (SAM or AAA), **Armor**
  (tanks and APCs), **ARTY**, **INF**, **NAVAL**, **UNK** (anything else). The
  server can hide the kind (`4×UNK`).
- The percentage is **confidence**. Recon intel halves every
  {{cfg:elint.half_life_recon_secs|600}} seconds and the marker is removed once
  it falls below {{cfg:elint.confidence_delete_threshold|0.05}} (5 % by default,
  about four half-lives). Fly another pass to refresh it.
- `±0.6km` is how precisely recon places the cluster.

The same contacts are read out by **GCI/EWR → Ground Intel**, nearest and most
confident first.

## Other ways contacts reach the map

- A **JTAC** feeds every unit it can see into the same intel picture,
  automatically, and keeps it marked for hours after losing sight of it. See
  [JTAC: F10 map intel](./jtac.md#f10-map-intel).
- The AI **Recon** action in the [Actions menu](./actions.md) sends a flight to
  scout a map mark and scans the area without a player flying it.
- F-14 TARPS photos uploaded to the dashboard build a separate photo map:
  [Recon Intel Map (TARPS)](../advanced/recon-intel-map.md).

## See Also

- [GCI / EWR](./ewr.md) - the **Ground Intel** report reads out the same contacts
- [JTAC System](./jtac.md) - JTAC eyes-on targets also mark the map
- [Actions Menu](./actions.md) - the AI recon flight
