import { type ReactElement } from "react";
import { Marker } from "react-map-gl/maplibre";

import Symbol from "./Symbol";
import { type Airport } from "./dcs/terrain";
import { sidcToSymbol } from "./entity";

export interface AirportMarkerProps {
  airport: Airport;
  selected: boolean;
  onClick: () => void;
}

export default function AirportMarker(props: AirportMarkerProps): ReactElement {
  const { airport, selected, onClick } = props;

  // Helipad / FARP pad — a small "H" tile, no NATO symbol, label only when
  // selected (there are hundreds on some theatres).
  if (airport.heli) {
    return (
      <Marker
        latitude={airport.position[0]}
        longitude={airport.position[1]}
        anchor="center"
        onClick={onClick}
      >
        <div className="flex flex-col items-center">
          <div
            style={{
              width: 11,
              height: 11,
              display: "flex",
              alignItems: "center",
              justifyContent: "center",
              border: `1px solid ${selected ? "#fff" : "var(--text-dim, #6b7858)"}`,
              borderRadius: 2,
              background: "rgba(0,0,0,0.35)",
              color: selected ? "#fff" : "var(--text-dim, #6b7858)",
              fontFamily: "var(--font-mono)",
              fontSize: 8,
              lineHeight: 1,
            }}
          >
            H
          </div>
          {selected && (
            <div
              className="mt-0.5 whitespace-nowrap"
              style={{ fontFamily: "var(--font-mono)", fontSize: 9, color: "var(--text-muted)", textShadow: "0 0 3px #000" }}
            >
              {airport.name}
            </div>
          )}
        </div>
      </Marker>
    );
  }

  const symbol = sidcToSymbol("10012000001213010000");
  const symbolElement = <Symbol symbol={symbol} />;

  return (
    <>
      <Marker
        latitude={airport.position[0]}
        longitude={airport.position[1]}
        anchor="center"
        onClick={onClick}
      >
        {selected ? (
          <div className="rounded-full border-2 border-white p-2">
            {symbolElement}
          </div>
        ) : (
          symbolElement
        )}
      </Marker>
      <Marker
        latitude={airport.position[0]}
        longitude={airport.position[1]}
        anchor="bottom"
        onClick={onClick}
      >
        <div
          className="mb-3 whitespace-nowrap"
          style={{ fontFamily: "var(--font-mono)", fontSize: 10, color: "var(--text)", textShadow: "0 0 3px #000, 0 0 3px #000" }}
        >
          {airport.name}
        </div>
      </Marker>
    </>
  );
}
