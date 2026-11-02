// Generated from miz/Scenarios/Vector Strike/levant_ablaze.story.json, the
// story events the Syria campaign config carries (Missions/Vector/Syria/
// tools/add_story.py). Regenerate rather than edit. {by} stands for the
// pilots the engine credits.

import type { StoryEvent } from './types'

export const LEVANT_STORY: StoryEvent[] = [
  {
    "side": "Red",
    "title": "The shield goes blind",
    "when": "Air Defence Academy HQ destroyed while RSII holds it",
    "text": "{by} have knocked out the Air Defence Academy. Batteries across the south lose their cueing and start fighting on their own.",
    "effects": [
      "RSII will −3",
      "NATO sees the defences round Air Defence Academy HQ",
      "200 points to the pilots credited"
    ]
  },
  {
    "side": "Red",
    "title": "The 159th falls silent",
    "when": "SyADF 159th Regiment destroyed while RSII holds it",
    "text": "The coalition's second command post is gone after a strike by {by}. What is left of the network fights blind.",
    "effects": [
      "RSII will −3",
      "NATO sees the defences round SyADF 159th Regiment",
      "200 points to the pilots credited"
    ]
  },
  {
    "side": "Blue",
    "title": "Incirlik under attack",
    "when": "Incirlik HQ destroyed while NATO holds it",
    "text": "NATO's headquarters at Incirlik is down after a strike led by {by}. The Alliance's batteries in Turkey lose their picture.",
    "effects": [
      "NATO will −3",
      "RSII sees the defences round Incirlik HQ",
      "200 points to the pilots credited"
    ]
  },
  {
    "side": "Red",
    "title": "Naqoura line broken",
    "when": "NATO takes Naqoura",
    "text": "{by} have broken the Naqoura line on the coast. The road north into Lebanon is open.",
    "effects": [
      "RSII will −2",
      "100 points to the pilots credited"
    ]
  },
  {
    "side": "Blue",
    "title": "The Galilee breached",
    "when": "RSII takes Kiryat Shmona",
    "text": "Coalition troops under {by} are in Kiryat Shmona. NATO's southern front falls back on Rosh Pina.",
    "effects": [
      "NATO will −3",
      "100 points to the pilots credited",
      "NATO told: “Kiryat Shmona has fallen. Hold Rosh Pina.”"
    ]
  },
  {
    "side": "Red",
    "title": "Taftanaz taken",
    "when": "NATO takes Taftanaz",
    "text": "{by} have taken Taftanaz. Aleppo is next.",
    "effects": [
      "RSII will −2",
      "100 points to the pilots credited",
      "RSII told: “Taftanaz has fallen. Cover Aleppo.”"
    ]
  },
  {
    "side": "Red",
    "title": "Aleppo falls",
    "when": "NATO takes Aleppo",
    "text": "Aleppo is in NATO hands, {by} among the crews credited. The factories of the northern belt are behind the line.",
    "effects": [
      "RSII will −5",
      "RSII production: equipment −20%",
      "150 points to the pilots credited"
    ]
  },
  {
    "side": "Blue",
    "title": "Hatay lost",
    "when": "RSII takes Hatay",
    "text": "The coalition has pushed into Hatay behind {by}. Gaziantep becomes NATO's front line in the north.",
    "effects": [
      "NATO will −3",
      "100 points to the pilots credited"
    ]
  },
  {
    "side": "Red",
    "title": "Sharqiyah Mine hit",
    "when": "Sharqiyah Mine destroyed while RSII holds it",
    "text": "{by} have put the Sharqiyah Mine out of action. The coalition's Euphrates industry is next.",
    "effects": [
      "RSII production: equipment −10%",
      "100 points to the pilots credited"
    ]
  },
  {
    "side": "Red",
    "title": "The Euphrates burns",
    "when": "Durayhim Military Base destroyed while RSII holds it, after “Sharqiyah Mine hit”",
    "text": "With Sharqiyah already down, strikes flown by {by} have put Durayhim out of action too. The coalition's Euphrates industry is finished.",
    "effects": [
      "RSII production: equipment −15%",
      "RSII will −3",
      "150 points to the pilots credited"
    ]
  },
  {
    "side": "Red",
    "title": "Latakia closed",
    "when": "Latakia destroyed while RSII holds it",
    "text": "The coalition's only naval base is out of the war after {by} struck it. The sea belongs to NATO.",
    "effects": [
      "RSII will −3",
      "RSII production: fuel −15%",
      "150 points to the pilots credited"
    ]
  },
  {
    "side": "Red",
    "title": "Damascus cut off",
    "when": "NATO takes Damascus",
    "text": "{by} have taken the Damascus hub. Every coalition unit south of it fights with what it carries.",
    "effects": [
      "RSII will −5",
      "every RSII hub loses 20% of its fuel",
      "150 points to the pilots credited"
    ]
  },
  {
    "side": "Red",
    "title": "The coalition asks for terms",
    "when": "RSII will below 12, after “The shield goes blind”",
    "text": "With its shield broken and its will to fight gone, the RSII asks for a ceasefire in the Levant.",
    "effects": [
      "the round ends: NATO wins"
    ]
  },
  {
    "side": "Blue",
    "title": "NATO stands down",
    "when": "NATO will below 12, after “Incirlik under attack”",
    "text": "With Incirlik hit and the Alliance's will to fight gone, NATO agrees a ceasefire in the Levant.",
    "effects": [
      "the round ends: RSII wins"
    ]
  }
]
