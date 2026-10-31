// Inline raster style off the Esri canvas tiles (a hosted vector style is
// blocked by the dashboard's prod CSP and renders black). Follows the
// dashboard light/dark theme, same as the old /map page.
export const mapStyleFor = (theme: string) => ({
  version: 8 as const,
  sources: {
    esri: {
      type: 'raster' as const,
      tiles: [
        theme === 'light'
          ? 'https://server.arcgisonline.com/ArcGIS/rest/services/Canvas/World_Light_Gray_Base/MapServer/tile/{z}/{y}/{x}'
          : 'https://server.arcgisonline.com/ArcGIS/rest/services/Canvas/World_Dark_Gray_Base/MapServer/tile/{z}/{y}/{x}',
      ],
      tileSize: 256,
      attribution: 'Esri',
    },
  },
  layers: [
    // Dark is the plot's radar scope: the grey canvas laid thin over a green
    // tube, so the land reads in phosphor green. Light is a paper map.
    { id: 'bg', type: 'background' as const, paint: { 'background-color': theme === 'light' ? '#e4ddca' : '#03170b' } },
    { id: 'esri', type: 'raster' as const, source: 'esri', paint: { 'raster-opacity': theme === 'light' ? 0.85 : 0.62 } },
  ],
})
