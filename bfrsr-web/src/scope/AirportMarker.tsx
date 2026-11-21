import { type ReactElement } from "react";
import { Marker } from "react-map-gl/maplibre";

import { type Airport } from "./dcs/terrain";

export interface AirportMarkerProps {
  airport: Airport;
  selected: boolean;
  onClick: () => void;
}

// Airfield: a circle with a diagonal runway. Helipad: a circle with an "H".
// Both hand-drawn (no milsymbol) so they stay small, sharp and theme-coloured.
const HALO = "drop-shadow(0 0 1.5px var(--bg)) drop-shadow(0 0 1.5px var(--bg))";
function AirfieldIcon({ col, size }: { col: string; size: number }): ReactElement {
  const s = size;
  return (
    <svg width={s} height={s} viewBox="0 0 14 14" style={{ display: "block", filter: HALO }}>
      <circle cx="7" cy="7" r="6" fill="var(--bg)" fillOpacity="0.55" stroke={col} strokeWidth="1" />
      <line x1="3.4" y1="10.6" x2="10.6" y2="3.4" stroke={col} strokeWidth="1.7" strokeLinecap="round" />
    </svg>
  );
}
function HelipadIcon({ col, size }: { col: string; size: number }): ReactElement {
  const s = size;
  return (
    <svg width={s} height={s} viewBox="0 0 12 12" style={{ display: "block", filter: HALO }}>
      <circle cx="6" cy="6" r="5" fill="var(--bg)" fillOpacity="0.5" stroke={col} strokeWidth="1" />
      <path d="M4.2 3.6 V8.4 M7.8 3.6 V8.4 M4.2 6 H7.8" stroke={col} strokeWidth="1.15" strokeLinecap="round" />
    </svg>
  );
}

export default function AirportMarker(props: AirportMarkerProps): ReactElement {
  const { airport, selected, onClick } = props;
  const col = selected ? "var(--accent-bright, #8ec83f)" : "var(--text-muted, #7d8a6a)";
  const heli = !!airport.heli;

  return (
    <>
      <Marker
        latitude={airport.position[0]}
        longitude={airport.position[1]}
        anchor="center"
        onClick={onClick}
      >
        <div className="cursor-pointer" style={selected ? { padding: 3, border: "1.5px solid #fff", borderRadius: "50%" } : undefined}>
          {heli ? <HelipadIcon col={col} size={13} /> : <AirfieldIcon col={col} size={16} />}
        </div>
      </Marker>
      {/* Airfield labels always; helipad labels only when selected (hundreds
          of them on some theatres). */}
      {(!heli || selected) && (
        <Marker
          latitude={airport.position[0]}
          longitude={airport.position[1]}
          anchor="bottom"
          onClick={onClick}
        >
          <div
            className="whitespace-nowrap"
            style={{
              marginBottom: heli ? 12 : 14,
              fontFamily: "var(--font-mono)",
              fontSize: heli ? 9 : 10,
              color: selected ? "var(--accent-bright)" : "var(--text-muted)",
              textShadow: "0 0 3px var(--bg), 0 0 3px var(--bg)",
            }}
          >
            {airport.name}
          </div>
        </Marker>
      )}
    </>
  );
}
