# miz/: missions and engine configs

| File | What |
|---|---|
| `ODFv2_CFG` | the ODFv2 campaign's engine config (objectives, deployables, EWR, logistics, ...) |
| `Scenarios/Vector Strike/VC_SYRIA_CFG`, `VSTest_CFG` | the Syria and test-bed scenarios' configs |
| `Scenarios/Vector Strike/*.miz` | base mission, warehouse template and the Golan test bed |
| `SAMPLE_CFG.json`, `SAMPLE_options_overrides.json` | annotated samples |
| `MISSION_BUILD_GUIDE.md`, `Mission Design Notes` | how the missions are put together |

bflib loads `<sortie>_CFG` from the DCS instance's write folder
(`Saved Games\DCS.<instance>\`). **Nothing on a server reads the copies in
this folder.** They are the source you edit and test, and a server only
gets one when someone copies it there, either by hand or through a campaign
pack (`deploy/publish-release.ps1 -Campaigns`, see `deploy/auto-update.md`).

## Admins

The engine's `admins` block maps a player's UCID (32 hex characters) to a
name, and those players get the in-game admin commands. The tracked configs
hold **placeholders**, `00000000000000000000000000000000` and `...0001`, not
real UCIDs: player identifiers don't belong in a public repo. Before a
config goes to a server, put that server's real admins in:

```json
"admins": {
  "<32-hex UCID>": "Name",
  "<32-hex UCID>": "Name"
}
```

A player's UCID is in DCSServerBot (`/info`), in bfdb's player list, or in
the server's `dcs.log` when they connect.

`publish-release.ps1` refuses to pack a cfg that still has a placeholder
admin, so a campaign pack can't strip a live server's admins. A copy made
by hand has no such check, so keep the server's own `admins` block when you
copy one over.

The production snapshots that used to sit here (`ODFv2_CFG production*`,
`ODFv2_CFG.pre-defence`) were removed. The live config is on the server,
and git history has the old snapshots.
