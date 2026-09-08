import { type ReactElement } from "react";

import { type Settings } from "./settings";

export interface SettingsModalProps {
  settings: Settings;
  setSettings: (settings: Settings) => void;
}

export default function SettingsModal(props: SettingsModalProps): ReactElement {
  const { settings, setSettings } = props;

  const check = (
    label: string,
    key: keyof Settings["view"],
  ): ReactElement => (
    <label className="label cursor-pointer" key={key}>
      <span className="label-text">{label}</span>
      <input
        className="checkbox"
        type="checkbox"
        checked={settings.view[key]}
        onChange={(e) => {
          settings.view[key] = e.target.checked;
          setSettings(settings);
        }}
      />
    </label>
  );

  return (
    <dialog id="settingsModal" className="modal">
      <form
        method="dialog"
        className="modal-box border border-gray-500 bg-gray-200"
      >
        <div className="mb-2 flex w-full flex-row px-2">
          <span className="text-xl">Settings</span>
          <div className="ml-auto">
            <button className="btn-sm btn">
              <svg
                xmlns="http://www.w3.org/2000/svg"
                viewBox="0 0 20 20"
                fill="currentColor"
                className="h-5 w-5"
              >
                <path d="M6.28 5.22a.75.75 0 00-1.06 1.06L8.94 10l-3.72 3.72a.75.75 0 101.06 1.06L10 11.06l3.72 3.72a.75.75 0 101.06-1.06L11.06 10l3.72-3.72a.75.75 0 00-1.06-1.06L10 8.94 6.28 5.22z" />
              </svg>
            </button>
          </div>
        </div>
        <div className="form-control px-2 pb-2 pt-2">
          {check("Use magnetic heading", "useMagneticHeading")}
          {check("Show ground objects", "showGround")}
          {check("Show air objects slower than 25 knots", "showSlowAir")}
          {check("Show weapon objects", "showWeapon")}
          {check("Show cursor coordinates", "showCursorCoords")}
          <div className="divider my-1" />
          {check("Show objectives", "showObjectives")}
          {check("Show frontline", "showFrontline")}
          {check("Show airfields", "showAirports")}
          {check("Show helipads", "showHelipads")}
        </div>
      </form>
      <form method="dialog" className="modal-backdrop">
        <button>Close</button>
      </form>
    </dialog>
  );
}
