# Recon Intel Map (TARPS)

The **Recon Intel** page on the [Ops Dashboard](../reference/changelog.md) is a shared,
coalition-only reconnaissance picture built from **F-14 TARPS** photography. Fly a
TARPS sortie, upload the photos, and they drop onto a map — geo-referenced,
perspective-corrected, and visible only to your own side.

It is entirely a web feature: nothing to install, no F10 menu. It complements the
in-game [Recon Pass](../f10-menu/recon.md) and JTAC intel rather than replacing
them — TARPS gives you an actual photograph of what's on the ground, at the cost
of having to fly the recon run and process the imagery yourself.

## Who can see it

- You must be **signed in** to the dashboard (Discord login) **and** registered to
  a coalition this campaign. If the dashboard can't work out which side you're on,
  the page shows a "no coalition" notice instead — link your Discord with
  `-linkme` in DCS chat and make sure you've picked a side on the server.
- Intel is **locked to your coalition**. The other side cannot see your captures,
  and the photos themselves are only served to same-side viewers.
- Server admins who don't play the campaign can switch between sides to moderate;
  an admin who *is* a registered player sees only their own side, like everyone
  else.

## Flying the recon run

1. Take the **F-14B** with a **TARPS pod** loaded.
2. Fly over (or obliquely past) the area you want imaged and trigger the pod's
   camera. Lower and slower gives you more usable frames; steep dive or bank
   angles beyond ~45° can't be perspective-corrected and will show up as a plain
   rectangle at the aircraft position.
3. Each capture is written to your **DCS `Screenshots` folder** as a PNG whose
   filename carries the shot's position (DMS lat/long), altitude, heading, pitch
   and roll — for example:

   ```
   TARPS KS-87D 07-16-22L 01-02-2005 N25-14-13 E055-24-12 ALT+08780 DRIFT+00 HDG301 PITCH+01 ROLL+01.png
   ```

## Uploading

On the **Recon Intel** page, use **Upload TARPS Photos** and select one or more
PNGs. The position and camera attitude are read straight from each filename:

- Photos with readable coordinates appear on the map immediately.
- Photos whose filename can't be parsed land in a **Needs Placement** list — click
  **Drop** and then click the map to place one by hand.

There is a per-side cap on how many captures a coalition can hold in a round, so
prune stale imagery you no longer need.

## Reading the map

- Each capture is a **camera marker** pointing in the direction the photo was
  taken, with a translucent **footprint** showing the ground it covers.
- Where the camera attitude allows, the photo itself is **warped onto the
  ground** so it lines up with the terrain underneath — near-vertical shots sit
  flat, oblique shots are stretched into perspective.
- **Click** any marker, footprint, or photo to open the full-resolution image in a
  viewer (mouse wheel to zoom, drag to pan, `Esc` to close).
- The **PHOTOS** toggle (top-right) hides or shows the image overlays while
  keeping the markers.
- Objectives and the frontline are drawn underneath for reference.

## Aligning a photo

Automatic placement is only as good as the metadata and a flat-earth assumption —
over uneven terrain, or when you want a photo to sit exactly on a runway or a
revetment, use the **align editor**:

1. Click the **align** (move) icon on one of your captures in the list.
2. Drag the four corner handles onto recognisable features on the basemap. The
   image re-warps live as you drag.
3. Use the **opacity** slider to fade the photo so you can see the map through it,
   which is useful for peeling apart a stack of overlapping captures.
4. **Save** to keep the alignment, **Reset** to go back to the automatic
   projection, or **Cancel** to discard.

Aligned photos are marked as such in the list, and the alignment persists for
everyone on your side.

## Retention

Recon intel belongs to the **current campaign**. It is wiped when the campaign is
reset, and captures from an earlier mission are cleared once a new one starts —
the picture always reflects the round you're in.

## See also

- [Reconnaissance (Recon Pass)](../f10-menu/recon.md) — the in-game F10 recon feature
- [Early Warning Radar](../f10-menu/ewr.md) — the radio intel picture
- [JTAC System](../f10-menu/jtac.md)
