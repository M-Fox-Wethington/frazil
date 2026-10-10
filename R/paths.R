#' Folders and files the package reads and writes
#'
#' The package keeps its settings in the R option `frazil.paths`. Call
#' `frazil_paths()` with arguments to set them for the session, and with no
#' arguments to read the resolved list. Only the data cache has a default. The
#' spatial layers and the elevation models have to point at copies on disk
#' before a map can be drawn, and `output` has to be set before a figure is
#' written.
#'
#' @details
#' The four SCAR Antarctic Digital Database (ADD) layers default to this layout
#' under `spatial`, which is the layout of the BRI priority-map bundle. Each can
#' also be set on its own.
#' * `Continent_ADD/Antarctica_Coastline_med_res_poly.shp`, the ADD v7.3
#'   medium-resolution coastline polygons.
#' * `iceshelves/medium_res_IceShelfs.shp`, the ADD ice-shelf polygons.
#' * `add_rock_outcrop/add_rock_outcrop_high_res_polygon_v7.3.shp`, the ADD
#'   high-resolution rock outcrops.
#' * `add_contours/add_contours_medium_res_v7.3.gpkg`, the ADD medium-resolution
#'   contours.
#'
#' The elevation models are the REMA v2.0 mosaics from the Polar Geospatial
#' Center. `dem_100m` is the 100 m mosaic
#' (`rema_mosaic_100m_v2.0_filled_cop30_dem.tif`), which the regional maps use,
#' and `dem_1km` is the 1 km mosaic, which the circumpolar maps use. `dem_300m`
#' is an optional fallback for the Peninsula window when the 100 m mosaic is not
#' on disk. When `spatial` is set, `dem_100m` and `dem_300m` default to
#' `rema_mosaic_100m_v2.0/rema_mosaic_100m_v2.0_filled_cop30_dem.tif` and
#' `derived_cache/peninsula_priority_dem.tif` under it.
#'
#' @param cache Folder where [nsidc_download()] mirrors the NSIDC files and
#'   where the readers look for them. Defaults to the package's cache folder
#'   from [tools::R_user_dir()].
#' @param spatial Folder holding the ADD layers in the layout described below.
#' @param coastline,iceshelves,rock,contours Paths to the four ADD layers.
#' @param dem_100m,dem_300m,dem_1km Paths to the REMA v2.0 elevation mosaics.
#' @param terrain_cache Folder for the cached terrain rasters and ice-day
#'   counts. Defaults to `file.path(cache, "terrain")`.
#' @param output Folder the figure functions write to. While it is `NULL`, the
#'   default, the figure functions return their plots and write nothing.
#' @param terra_tmp Folder for terra's temporary rasters. Set it when the
#'   system temporary folder is on a nearly full drive.
#' @return The resolved list of paths, invisibly when any argument was given.
#' @examples
#' \dontrun{
#' frazil_paths(
#'   cache   = "D:/seaice/nsidc",
#'   spatial = "D:/seaice/spatial_data",
#'   dem_1km = "D:/seaice/rema_mosaic_1km_v2.0_filled_cop30_dem.tif",
#'   output  = "D:/seaice/figures"
#' )
#' frazil_paths()$cache
#' }
#' @export
frazil_paths <- function(cache = NULL, spatial = NULL, coastline = NULL, iceshelves = NULL,
                         rock = NULL, contours = NULL, dem_100m = NULL, dem_300m = NULL,
                         dem_1km = NULL, terrain_cache = NULL, output = NULL,
                         terra_tmp = NULL) {
  given <- list(cache = cache, spatial = spatial, coastline = coastline,
                iceshelves = iceshelves, rock = rock, contours = contours,
                dem_100m = dem_100m, dem_300m = dem_300m, dem_1km = dem_1km,
                terrain_cache = terrain_cache, output = output, terra_tmp = terra_tmp)
  given <- given[!vapply(given, is.null, logical(1))]
  stored <- getOption("frazil.paths", list())
  setting <- length(given) > 0
  if (setting) {
    stored[names(given)] <- lapply(given, normalizePath, winslash = "/", mustWork = FALSE)
    options(frazil.paths = stored)
  }
  p <- stored
  if (is.null(p$cache)) p$cache <- tools::R_user_dir("frazil", "cache")
  if (is.null(p$terrain_cache)) p$terrain_cache <- file.path(p$cache, "terrain")
  if (!is.null(p$spatial)) {
    under <- function(rel) file.path(p$spatial, rel)
    if (is.null(p$coastline))  p$coastline  <- under("Continent_ADD/Antarctica_Coastline_med_res_poly.shp")
    if (is.null(p$iceshelves)) p$iceshelves <- under("iceshelves/medium_res_IceShelfs.shp")
    if (is.null(p$rock))       p$rock       <- under("add_rock_outcrop/add_rock_outcrop_high_res_polygon_v7.3.shp")
    if (is.null(p$contours))   p$contours   <- under("add_contours/add_contours_medium_res_v7.3.gpkg")
    if (is.null(p$dem_100m))   p$dem_100m   <- under("rema_mosaic_100m_v2.0/rema_mosaic_100m_v2.0_filled_cop30_dem.tif")
    if (is.null(p$dem_300m))   p$dem_300m   <- under("derived_cache/peninsula_priority_dem.tif")
  }
  if (setting && !is.null(p$terra_tmp)) {
    dir.create(p$terra_tmp, recursive = TRUE, showWarnings = FALSE)
    terra::terraOptions(tempdir = p$terra_tmp, progress = 0)
  }
  if (setting) invisible(p) else p
}

# A path that must be set and must exist, with a message that says what to set.
require_path <- function(paths, field, what) {
  p <- paths[[field]]
  if (is.null(p)) {
    stop(sprintf("%s is needed but frazil_paths()$%s is not set. See ?frazil_paths.",
                 what, field), call. = FALSE)
  }
  if (!file.exists(p)) stop(sprintf("%s not found at %s.", what, p), call. = FALSE)
  p
}
