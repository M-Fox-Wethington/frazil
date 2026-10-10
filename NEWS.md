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
