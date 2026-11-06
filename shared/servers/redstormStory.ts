// Generated once from the Southern Flank intro page, which was built from
// miz/Scenarios/Red Storm Rising/story.sample.json and RSR_SF_CFG (2 November
// 2026). {by} stands for the pilots the engine credits.

import type { StoryEvent } from './types'

export const STORY: StoryEvent[] = [
  {
    "side": "Red",
    "title": "Tuapse burns",
    "when": "Tuapse refinery destroyed while the Soviets hold it",
    "text": "The Tuapse refinery is burning after a strike by {by}. Soviet fuel output is cut and the Front air army is told to cover the refineries it has left.",
    "effects": [
      "Soviet production: fuel −40%",
      "Soviet will −6",
      "Soviet HQ: Cover the refineries",
      "150 points to the pilots credited"
    ]
  },
  {
    "side": "Red",
    "title": "Krasnodar refinery hit",
    "when": "Krasnodar refinery destroyed while the Soviets hold it",
    "text": "With Tuapse already burning, {by} have put the Krasnodar refinery out of action too.",
    "effects": [
      "Soviet production: fuel −40%",
      "Soviet will −6",
      "150 points to the pilots credited"
    ]
  },
  {
    "side": "Blue",
    "title": "The carrier is hit",
    "when": "CVBG below 75% health",
    "text": "The carrier group off Batumi has been hit by {by}. Damage control is under way; the battle group's escorts close up.",
    "effects": [
      "NATO will −10",
      "NATO HQ: new orders",
      "200 points to the pilots credited"
    ]
  },
  {
    "side": "Red",
    "title": "Backfire regiment broken",
    "when": "6 Tu-22M3 shot down in 60 minutes",
    "text": "Six Tu-22M3s have been shot down within the hour, {by} among the pilots credited. The Backfire regiment can no longer mount an anti-ship strike until it is rebuilt.",
    "effects": [
      "Soviet loses \"Backfire raid\"",
      "Soviet will −4",
      "100 points to the pilots credited"
    ]
  },
  {
    "side": "Red",
    "title": "Backfires from the reserve",
    "when": "Soviet Tu-22M3 stock below 2",
    "text": "Long Range Aviation has lost its Backfires to {by}. Replacements from the strategic reserve land at Mozdok and the anti-ship raids resume.",
    "effects": [
      "6 Tu-22M3 land at Mozdok",
      "Soviet regains \"Backfire raid\"",
      "Soviet will −2"
    ]
  },
  {
    "side": "Red",
    "title": "Surami tunnel cut",
    "when": "Surami tunnel destroyed while the Soviets hold it",
    "text": "{by} have brought down the Surami tunnel, the one rail line between east and west Georgia. Everything west of it now runs on what it carries.",
    "effects": [
      "Soviet production: fuel −15%, equipment −25%",
      "Gori loses 30% of its fuel",
      "Soviet will −5",
      "Soviet HQ: new orders"
    ]
  },
  {
    "side": "Blue",
    "title": "Through the Surami pass",
    "when": "NATO captures Surami tunnel",
    "text": "{by} have taken the Surami tunnel. Turkish F-4Es are cleared to operate over eastern Georgia.",
    "effects": [
      "NATO regains \"Turkish F-4E strike\"",
      "Soviet will −5",
      "100 points to the pilots credited"
    ]
  },
  {
    "side": "Red",
    "title": "Red takes Zestafoni",
    "when": "the Soviets capture Zestafoni",
    "text": "Soviet troops under {by} have taken Zestafoni, NATO's forward hub. The Front turns its main effort on Kutaisi.",
    "effects": [
      "NATO will −4",
      "Soviet HQ: new orders"
    ]
  },
  {
    "side": "Blue",
    "title": "Sochi falls",
    "when": "NATO captures Sochi-Adler",
    "text": "{by} have taken Sochi-Adler, Red's front airfield on the coast. Captured documents show the defences around the Tuapse refinery.",
    "effects": [
      "NATO sees the defences round Tuapse refinery",
      "Soviet will −5",
      "100 points to the pilots credited"
    ]
  },
  {
    "side": "Blue",
    "title": "Kutaisi in danger",
    "when": "Kutaisi below 50% health",
    "text": "Kutaisi is under heavy attack. The Soviet airborne forces are ready to drop behind it.",
    "effects": [
      "Soviet regains \"Airborne assault\"",
      "NATO HQ: new orders"
    ]
  },
  {
    "side": "Red",
    "title": "Front commander lost",
    "when": "1 Mi-8MT shot down in 60 minutes within 30 km of Tbilisi",
    "text": "A Soviet command helicopter has been shot down near Tbilisi by {by}. The Front's staff work slows.",
    "effects": [
      "Soviet HQ: new orders",
      "Soviet will −3",
      "150 points to the pilots credited"
    ]
  },
  {
    "side": "Red",
    "title": "Convoys burning on the coast road",
    "when": "3 Soviet supply columns destroyed in 4 hours",
    "text": "Three Soviet supply columns have been destroyed in four hours, {by} among the crews credited.",
    "effects": [
      "every Soviet hub loses 5% of its fuel",
      "Soviet will −2"
    ]
  },
  {
    "side": "Blue",
    "title": "Enguri dam struck",
    "when": "Enguri dam destroyed while NATO hold it",
    "text": "{by} have knocked out the power station at the Enguri dam. Western Georgia's grid is down.",
    "effects": [
      "NATO production: equipment −15%",
      "NATO will −3"
    ]
  },
  {
    "side": "Blue",
    "title": "Batumi oil terminal burns",
    "when": "Batumi oil terminal destroyed while NATO hold it",
    "text": "Soviet bombers led by {by} have set the Batumi oil terminal on fire. Allied fuel is short.",
    "effects": [
      "NATO production: fuel −40%",
      "Batumi loses 30% of its fuel",
      "NATO will −5",
      "150 points to the pilots credited"
    ]
  },
  {
    "side": "Red",
    "title": "Fuel crisis",
    "when": "Soviet fuel below 30%",
    "text": "Soviet fuel stocks are under 30%. Rationing at the front: the offensive halts while the Front waits for fuel. The strikes that did it were flown by {by}.",
    "effects": [
      "Soviet HQ: Rationing at the front. Hold what we have",
      "Soviet will −4"
    ]
  },
  {
    "side": "Blue",
    "title": "NATO fuel crisis",
    "when": "NATO fuel below 30%",
    "text": "Allied fuel stocks at the beachhead are under 30%. Sorties are cut back.",
    "effects": [
      "NATO HQ: new orders",
      "NATO will −4"
    ]
  },
  {
    "side": "Red",
    "title": "Moscow asks for terms",
    "when": "Soviet will below 12",
    "text": "Out of fuel and with its will to fight gone, Moscow asks for a ceasefire on the southern flank.",
    "effects": [
      "the round ends: NATO wins"
    ]
  },
  {
    "side": "Blue",
    "title": "NATO withdraws from the beachhead",
    "when": "NATO will below 12",
    "text": "With the carrier damaged and the beachhead failing, the Alliance agrees a ceasefire.",
    "effects": [
      "the round ends: the Soviet Union wins"
    ]
  }
]

/** What a loss costs the side that suffers it, in will to fight. */
export const WILL_COSTS: [string, string][] = [
  [
    "Its carrier group destroyed",
    "−25"
  ],
  [
    "Its command centre captured or destroyed",
    "−10"
  ],
  [
    "Its carrier group damaged below 75%",
    "−8"
  ],
  [
    "A factory captured or destroyed",
    "−5"
  ],
  [
    "An airbase captured",
    "−4"
  ],
  [
    "A logistics hub captured",
    "−3"
  ],
  [
    "Any other objective captured",
    "−2"
  ],
  [
    "A ground formation destroyed",
    "−2"
  ],
  [
    "A bomber, tanker, AWACS or transport shot down",
    "−1"
  ],
  [
    "A ship sunk",
    "−1"
  ],
  [
    "Any other aircraft shot down",
    "−0.3"
  ]
]
