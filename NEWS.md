# frazil 0.1.0.9000 (development)

* `relief_preset()` gives the two settled relief looks, `"quiet"` and
  `"rema"`, so the project's sheets agree on a treatment rather than each
  carrying its own four numbers. `"rema"` is the look of the Reference
  Elevation Model of Antarctica's published poster: a shadow floor of 0.16
  against 0.42, which is what lets the ice streams read as flow lines, light
  concentrated on the northwest, and a near-neutral ramp.
* `build_priority_terrain()` gained `lights` and `ice_ramp`, which were
  hard-coded, and now writes a signature of every setting beside its cached
  rasters. The cache is named by the caller, so changing the exaggeration, the
  shadow floor, the lights or the ramp while keeping the name used to return
  the old image; a mismatched signature now rebuilds.

# frazil 0.1.0

First release. The package ports the sea ice figure scripts written for the
BRI shipboard talk of October 2026 into functions that take a date and a map
window:

* `nsidc_download()` mirrors the NSIDC Sea Ice Index, Version 4, in parallel
  batches and fetches only new or changed files.
* `record_year()` applies the AMSR2 record rule.
* `build_priority_terrain()` builds the Imhof-style terrain of the BRI
  priority map from REMA elevation and the SCAR Antarctic Digital Database.
* `check_alignment()` stops a map when the sea ice grid and the coastline do
  not line up.
* Eleven figure functions and `fig_animation()` render the slide set at
  13.333 by 7.5 inches and 600 dpi.
