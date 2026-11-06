# Levant Ablaze: the Syria campaign, rebuilt

The modern Syria campaign rebuilt around its story, the way Southern Flank was
built for Red Storm Rising. The story is `levant_ablaze.story.json` beside this
file (and on the public site and in the wiki, `shared/servers/syria.ts`). The
build lives outside the repo, in `Saved Games\DCS\Missions\Vector\levant\`, and
reads the 2.x Syria build (`..\Syria\`) only as a donor.

Decided with the user on 2 November 2026:

| | |
|---|---|
| Objectives | Redrawn around the story's three fronts, an even start, every place the story names |
| Ground truth | Syria is not installed on the build PC. Units go only on ground the donor proves; a terrain probe run on the server box confirms them before the campaign goes live |
| Spawning | Dynamic spawn only, as Southern Flank |
| Going live | A new sortie, `LEVANT` (`LEVANT_CFG`, save `LEVANT`). The ODFv2 round finishes untouched; the vs1 server switches at a round's end |

## The map: 26 objectives a side

Every objective keeps the donor's position and zone, so its ground is proven:
the live server has spawned on every one of them.

| Front | NATO (Blue) | RSII (Red) |
|---|---|---|
| **South**: the Galilee, the Golan, Lebanon | Kiryat Shmona, Rosh Pina, Ramat David, Haifa, King Hussein Air College (airbases); HS33, HI01, HI12, HJ02 (FOBs); HI03 (factory); HI22 (logistics hub); HI06 HQ (command centre); Factory Ashdod | Naqoura, HL06, HL04, HS42, HS40, Marj as Sultan North (FOBs); Mezzeh, Beirut-Rafic Hariri, Rayak (airbases); HL01, Damascus (logistics hubs); SyADF 159th Regiment (command centre) |
| **North**: Hatay and Gaziantep against the Aleppo belt | Hatay, Gaziantep, Kahramanmaras, Adana Sakirpasa (airbases); Incirlik (logistics hub); Incirlik HQ (command centre); Mersin (naval base); HT01 Factory; Sanliurfa Heliport (FOB) | Taftanaz, Minakh, Aleppo, Kuweires (airbases); Abu al-Duhur (logistics hub); AL Safira Military Base, Durayhim Military Base (factories) |
| **Centre** | | Air Defence Academy HQ (command centre); Shayrat (airbase); SAA 312th Brigade (factory) |
| **Sea**: Cyprus and the Syrian coast | Akrotiri, Larnaca (airbases); HC04 (logistics hub); HC05 (factory) | Latakia (naval base); Bassel Al Assad (airbase) |
| **Euphrates** | | Sharqiyah Mine (factory); Tabqa (airbase) |

| | NATO | RSII |
|---|---|---|
| Airbases | 11 | 10 |
| FOBs | 5 | 6 |
| Logistics hubs | 3 | 3 |
| Factories | 4 | 4 |
| Command centres | 2 | 2 |
| Naval bases | 1 | 1 |
| Carrier groups | CVBG | RSII naval group |

## Forces

- **NATO**: M1A2C, Leopard 2, Merkava Mk4, M2 Bradley, Gepard, M6 Linebacker,
  M1097 Avenger, HEMTT, infantry with Stingers.
- **RSII** (Russia, Syria, Iran and Iraq): T-90M, BMPT, BMP-3, BMP-2,
  2S6 Tunguska, ZSU-23-4 Shilka, Osa, Strela, infantry with Igla.
- **Fleets**: NATO's carrier group in the Gulf of Iskenderun (CVN-72, Tarawa,
  a Ticonderoga, two Arleigh Burkes); the RSII naval group off the Syrian
  coast (Kuznetsov, the cruiser Moskva, a Neustrashimy, a Ropucha). Both stay
  where the donor's are: proven water. No Type 052 (not an RSII ship).
- **Air defence**: 75 hidden SAM sites (49 RSII, 26 NATO) from the owner's
  Levant SAM template and the 2.x campaign's proven sites, strongest first,
  18 km apart, within 60 km of their side's objectives.

Garrisons are laid out unit by unit (`garrison_layouts`), each group in a
formation facing the nearest objective the enemy holds.

## The campaign config

The donor's `SYRIA_CFG` (with the 2.1 story and war will) plus what Southern
Flank runs: the theatre HQ for both sides (`smart_commander.hq`), `ai_fill`,
`ai_air_stock`, `garrison_layouts`. Strike, CAS and SEAD templates are added to
the mission with loads from DCS's own payload library.

## Status (2 November 2026)

Built, checked and probed (`levant\README.md` in the build folder): LEVANT.miz
and LEVANT_CFG, 171 garrison groups (about 709 vehicles a side), 75 SAM sites,
the HQ, ai_fill, ai_air_stock and the 14 story events. The config loads with
the engine's own loader and every cross-check passes. The terrain probe ran in
a local DCS with the Syria map: 78 garrison vehicles on water, in buildings or
by a runway were moved, and one SAM site with a unit in the sea was left out.
The three HQ zones, missing from every Syria base after 20 September, are copied
from the mission the live round runs. Not deployed.
