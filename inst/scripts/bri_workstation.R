# Path settings for the BRI workstation, where the NSIDC mirror, the ADD layers,
# the REMA mosaics and the terrain caches already sit in the drone-processing
# repository. Source this before drawing anything:
#
#   source(system.file("scripts/bri_workstation.R", package = "frazil"))
#
# The terrain cache points at the folder that already holds the terrain
# rasters built in October 2026, so no terrain is rebuilt. terra's temporary
# files go to the same drive, because C: is nearly full on this machine.

repo <- "G:/BRI-Antarctic-Drone-Processing"
frazil::frazil_paths(
  cache         = file.path(repo, "figures/cache/seaice"),
  spatial       = file.path(repo, "peninsula_priority_bundle/spatial_data"),
  dem_1km       = file.path(repo, "figures/cache/rema/rema_mosaic_1km_v2.0_filled_cop30_dem.tif"),
  terrain_cache = file.path(repo, "figures/cache"),
  output        = file.path(repo, "figures/output/seaice"),
  terra_tmp     = file.path(repo, "figures/cache/terra_tmp")
)
