export function defaultSettings(): Settings {
  return {
    view: {
      useMagneticHeading: true,
      showGround: true,
      showSlowAir: false,
      showWeapon: false,
      showCursorCoords: false,
      showObjectives: true,
      showFrontline: true,
      showAirports: true,
      showHelipads: false,
    },
  };
}

export interface Settings {
  view: ViewSettings;
}

export interface ViewSettings {
  useMagneticHeading: boolean;
  showGround: boolean;
  showSlowAir: boolean;
  showWeapon: boolean;
  showCursorCoords: boolean;
  /** Campaign layers added on top of the peace-eye scope. */
  showObjectives: boolean;
  showFrontline: boolean;
  showAirports: boolean;
  showHelipads: boolean;
}
