import { getRange } from "../../util";
import { Caucasus } from "./caucasus";
import { Falklands } from "./falklands";
import { Germany } from "./germany";
import { Kola } from "./kola";
import { MarianaIslands } from "./marianaislands";
import { Nevada } from "./nevada";
import { Normandy } from "./normandy";
import { PersianGulf } from "./persiangulf";
import { Sinai } from "./sinai";
import { Syria } from "./syria";
import { TheChannel } from "./thechannel";

export interface Terrain {
  name: string;
  center: [number, number];
  airports: Airport[];
  projection: Projection;
}

export interface Airport {
  name: string;
  position: [number, number];
  /** no runway — a helipad / FARP pad rather than an airfield. */
  heli?: boolean;
}

export interface Projection {
  centralMeridian: number;
  falseEasting: number;
  falseNorthing: number;
  scaleFactor: number;
}

export const Terrains = [
  Caucasus,
  Nevada,
  Normandy,
  PersianGulf,
  TheChannel,
  Syria,
  MarianaIslands,
  Sinai,
  Kola,
  Falklands,
  Germany,
];

export function getTerrainFromReferencePoint(
  refLat: number,
  refLng: number
): Terrain | undefined {
  for (const terrain of Terrains) {
    if (getRange([refLat, refLng], terrain.center) < 500.0) {
      return terrain;
    }
  }
  return undefined;
}
