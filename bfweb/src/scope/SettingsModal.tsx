import { type ReactElement } from "react";

import { type Settings } from "./settings";

export interface SettingsModalProps {
  settings: Settings;
  setSettings: (settings: Settings) => void;
}

const ROWS: [label: string, key: keyof Settings["view"], group?: boolean][] = [
  ["Magnetic heading", "useMagneticHeading"],
  ["Ground contacts", "showGround"],
  ["Air contacts under 25 kt", "showSlowAir"],
  ["Weapon tracks", "showWeapon"],
  ["Cursor coordinate readout", "showCursorCoords"],
  ["Objectives", "showObjectives", true],
  ["Frontline", "showFrontline"],
  ["Airfields", "showAirports"],
  ["Helipads", "showHelipads"],
];

export default function SettingsModal(props: SettingsModalProps): ReactElement {
  const { settings, setSettings } = props;

  const set = (key: keyof Settings["view"], v: boolean) => {
    setSettings({ ...settings, view: { ...settings.view, [key]: v } });
  };

  return (
    <dialog id="settingsModal" className="modal">
      <div
        className="modal-box"
        style={{
          width: 300,
          maxWidth: "90vw",
          padding: 0,
          background: "var(--bg-card)",
          border: "1px solid var(--accent-border)",
          color: "var(--text)",
          fontFamily: "var(--font-mono)",
        }}
      >
        <div
          style={{
            display: "flex",
            alignItems: "center",
            padding: "10px 12px",
            borderBottom: "1px solid var(--border-light)",
          }}
        >
          <span style={{ fontFamily: "var(--font-display)", letterSpacing: "0.14em", fontSize: "0.95rem" }}>
            DISPLAY
          </span>
          <form method="dialog" style={{ marginLeft: "auto" }}>
            <button
              style={{ background: "none", border: "none", color: "var(--text-dim)", cursor: "pointer", fontSize: 14, lineHeight: 1 }}
            >
              ✕
            </button>
          </form>
        </div>

        <div style={{ padding: "6px 12px 12px" }}>
          {ROWS.map(([label, key, group]) => (
            <div key={key}>
              {group && <div style={{ height: 1, background: "var(--border-light)", margin: "8px 0" }} />}
              <label
                style={{
                  display: "flex",
                  alignItems: "center",
                  justifyContent: "space-between",
                  gap: 12,
                  padding: "6px 0",
                  fontSize: "0.78rem",
                  color: "var(--text-muted)",
                  cursor: "pointer",
                }}
              >
                <span>{label}</span>
                <input
                  type="checkbox"
                  checked={settings.view[key]}
                  onChange={(e) => set(key, e.target.checked)}
                  style={{
                    appearance: "none",
                    WebkitAppearance: "none",
                    width: 16,
                    height: 16,
                    flexShrink: 0,
                    borderRadius: 2,
                    border: "1px solid var(--accent-border)",
                    background: settings.view[key] ? "var(--accent)" : "var(--bg-input)",
                    boxShadow: settings.view[key] ? "inset 0 0 0 2px var(--bg-card)" : "none",
                    cursor: "pointer",
                  }}
                />
              </label>
            </div>
          ))}
        </div>
      </div>
      <form method="dialog" className="modal-backdrop">
        <button>close</button>
      </form>
    </dialog>
  );
}
