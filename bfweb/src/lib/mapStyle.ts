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
    { id: 'bg', type: 'background' as const, paint: { 'background-color': theme === 'light' ? '#dfe0d8' : '#0a0d07' } },
    { id: 'esri', type: 'raster' as const, source: 'esri', paint: { 'raster-opacity': theme === 'light' ? 0.95 : 0.85 } },
  ],
})
