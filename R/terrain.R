# --- Priority-map terrain -----------------------------------------------------
# The maps use the terrain of the BRI Peninsula priority-sites map (the
# imhof-peninsula-priority-map skill). REMA elevation is shaded by a
# four-direction Imhof hillshade and colored by two hypsometric ramps, an ice
# ramp and a warm rock ramp used wherever the ADD rock-outcrop layer marks
# exposed rock. Shadows are tinted channel by channel, warmer on rock. The ADD
# contours are drawn heavier and darker on shaded slopes, and the rock outcrops
# get faint outlines.

#' ADD rock outcrops within a window
#'
#' The layer is read with terra, because it holds rings of only two points,
#' which make sf's validity checks fail at Peninsula extent.
#'
#' @param window_bbox An sf bbox in the map CRS.
#' @param crs_wkt WKT of the map CRS.
#' @param paths The list from [frazil_paths()].
#' @return A SpatVector in the map CRS.
#' @export
load_rock_vect <- function(window_bbox, crs_wkt, paths = frazil_paths()) {
  rock_path <- require_path(paths, "rock", "The ADD rock-outcrop layer")
  win <- terra::vect(sf::st_transform(sf::st_as_sfc(window_bbox), 3031))
  v <- terra::vect(rock_path, extent = terra::ext(win))
  v <- terra::crop(v, win)
  terra::project(v, crs_wkt)
}

#' Build the priority-map terrain over a window
#'
#' Builds the shaded, colored terrain of the BRI priority map from a REMA
#' elevation mosaic and caches the RGB raster and the hillshade in the terrain
#' cache, so a second call with the same `cache_name` loads them from disk. Only
#' ADD land and ice shelves keep a color, so sea ice drawn beneath the terrain
#' shows everywhere else.
#'
#' @details
#' The recipe follows the priority map: the elevation is multiplied by
#' `z_factor`, shaded from the northwest (0.45), north (0.20), northeast (0.15)
#' and west (0.20), and scaled to its 99.5th percentile. The ice ramp runs from
#' `#b0c4d6` to white and the rock ramp from `#9e7e56` to `#f0e4c4`. Shadows
#' stop at `shadow_floor`, each channel is shaded with its own exponent, warmer
#' on rock, and sunlit slopes get a small warm lift. When the terrain grid is at
#' least twice as coarse as the mosaic, the mosaic is averaged onto it so every
#' source cell contributes.
#'
#' @param dem_path Path to a REMA elevation GeoTIFF.
#' @param window_bbox An sf bbox in the map CRS.
#' @param crs_wkt WKT of the map CRS.
#' @param grid_res Cell size of the terrain in meters.
#' @param land The list from [load_land()] in the map CRS.
#' @param cache_name File name stem for the cached rasters.
#' @param z_factor Vertical exaggeration. The priority map uses 3 at 300 m; the
#'   circumpolar terrain uses 12 at 1.5 km because the ice sheet's slopes are
#'   about five times gentler on cells that coarse.
#' @param shadow_floor Darkest shading, as a share of the full color.
#' @param terrain_cache Folder for the cached rasters.
#' @param paths The list from [frazil_paths()], for the rock-outcrop layer.
#' @return A list with `rgb`, a three-band SpatRaster, and `hs`, the hillshade.
#' @export
build_priority_terrain <- function(dem_path, window_bbox, crs_wkt, grid_res, land,
                                   cache_name, z_factor = 3, shadow_floor = 0.38,
                                   terrain_cache = frazil_paths()$terrain_cache,
                                   paths = frazil_paths()) {
  dir.create(terrain_cache, recursive = TRUE, showWarnings = FALSE)
  rgb_path <- file.path(terrain_cache, paste0(cache_name, "_rgb.tif"))
  hs_path  <- file.path(terrain_cache, paste0(cache_name, "_hillshade.tif"))
  if (file.exists(rgb_path) && file.exists(hs_path)) {
    message("Loading cached terrain: ", basename(rgb_path))
    return(list(rgb = terra::rast(rgb_path), hs = terra::rast(hs_path)))
  }
  if (!file.exists(dem_path)) stop("Elevation model not found at ", dem_path, call. = FALSE)
  message(sprintf("Building terrain at %g m from %s ...", grid_res, basename(dem_path)))
  t0 <- Sys.time()

  dem <- terra::rast(dem_path)
  win_src <- sf::st_transform(sf::st_buffer(sf::st_as_sfc(window_bbox), 20000),
                              terra::crs(dem))
  dem <- terra::crop(dem, terra::vect(win_src), snap = "out")
  template <- terra::rast(xmin = window_bbox[["xmin"]], xmax = window_bbox[["xmax"]],
                          ymin = window_bbox[["ymin"]], ymax = window_bbox[["ymax"]],
                          resolution = grid_res, crs = crs_wkt)
  coarser <- grid_res >= 2 * terra::res(dem)[1]
  dem <- terra::project(dem, template, method = if (coarser) "average" else "bilinear")
  dem <- terra::classify(dem, cbind(-Inf, 0, NA), right = FALSE)
  message(sprintf("  DEM on the map grid: %d x %d cells", ncol(dem), nrow(dem)))

  dem_x <- dem * z_factor
  slp <- terra::terrain(dem_x, "slope", unit = "radians")
  asp <- terra::terrain(dem_x, "aspect", unit = "radians")
  rm(dem_x)
  hs <- 0.45 * terra::shade(slp, asp, angle = 45, direction = 315) +
        0.20 * terra::shade(slp, asp, angle = 50, direction = 0) +
        0.15 * terra::shade(slp, asp, angle = 40, direction = 45) +
        0.20 * terra::shade(slp, asp, angle = 35, direction = 270)
  names(hs) <- "hillshade"
  hs <- terra::writeRaster(hs, hs_path, overwrite = TRUE, datatype = "FLT4S",
                           gdal = c("COMPRESS=LZW", "TILED=YES"))
  rm(slp, asp); gc(verbose = FALSE)

  rock <- load_rock_vect(window_bbox, crs_wkt, paths = paths)
  is_rock <- terra::values(terra::rasterize(rock, dem, field = 1, background = 0),
                           mat = FALSE) == 1
  message(sprintf("  %d rock outcrop polygons, %.2f%% of cells marked rock",
                  nrow(rock), 100 * mean(is_rock)))

  v_dem   <- terra::values(dem, mat = FALSE)
  na_mask <- is.na(v_dem)
  dem_max <- stats::quantile(v_dem, 0.995, na.rm = TRUE)
  idx <- as.integer(pmin(pmax(round(pmin(pmax(v_dem / dem_max, 0), 1) * 255) + 1, 1), 256))
  rm(v_dem); gc(verbose = FALSE)
  v_hs <- terra::values(hs, mat = FALSE)
  hs_range <- range(v_hs, na.rm = TRUE)
  hsn <- (v_hs - hs_range[1]) / diff(hs_range)
  hsn[is.na(hsn)] <- 0.5
  rm(v_hs); gc(verbose = FALSE)
  bright <- hsn > 0.72

  pal_ice  <- grDevices::col2rgb(grDevices::colorRampPalette(TERRAIN_ICE)(256)) / 255
  pal_rock <- grDevices::col2rgb(grDevices::colorRampPalette(TERRAIN_ROCK)(256)) / 255
  ice_exp  <- c(0.92, 0.97, 1.08); rock_exp <- c(0.90, 0.95, 1.02)
  ice_lift <- c(0.025, 0.012, 0);  rock_lift <- c(0.030, 0.018, 0)
  bands <- vector("list", 3)
  for (ch in 1:3) {
    out  <- pal_ice[ch, idx] * (shadow_floor + (1 - shadow_floor) * hsn^ice_exp[ch])
    out[bright] <- out[bright] + ice_lift[ch]
    rock_ch <- pal_rock[ch, idx[is_rock]] *
      (shadow_floor + (1 - shadow_floor) * hsn[is_rock]^rock_exp[ch])
    rock_ch <- rock_ch + ifelse(bright[is_rock], rock_lift[ch], 0)
    out[is_rock] <- rock_ch
    out <- round(pmin(pmax(out, 0), 1) * 255)
    out[na_mask] <- NA
    bands[[ch]] <- terra::setValues(terra::rast(dem), out)
    rm(out, rock_ch); gc(verbose = FALSE)
  }
  rgb <- terra::rast(bands)
  names(rgb) <- c("red", "green", "blue")
  rm(bands, idx, hsn, bright, is_rock, na_mask); gc(verbose = FALSE)

  rgb <- terra::mask(rgb, terra::vect(c(land$land, land$shelves)))
  rgb <- terra::writeRaster(rgb, rgb_path, overwrite = TRUE, datatype = "INT1U",
                            gdal = c("COMPRESS=LZW", "TILED=YES"))
  message(sprintf("  Terrain built in %.1f min",
                  as.numeric(difftime(Sys.time(), t0, units = "mins"))))
  list(rgb = rgb, hs = terra::rast(hs_path))
}

#' Draw an RGB raster as one prebuilt image
#'
#' Far faster than a raster layer for a large terrain. The raster is first
#' averaged down to at most `max_cols` columns, which is still finer than the
#' slide's pixels, and cells with no color are transparent.
#'
#' @param rgb A three-band SpatRaster with values 0 to 255.
#' @param max_cols Widest image drawn, in columns.
#' @return A [ggplot2::annotation_raster()] layer.
#' @export
rgb_annotation <- function(rgb, max_cols = 7000) {
  f <- ceiling(terra::ncol(rgb) / max_cols)
  if (f > 1) rgb <- terra::aggregate(rgb, fact = f, fun = "mean", na.rm = TRUE)
  a <- terra::as.array(rgb) / 255
  alpha <- ifelse(is.na(a[, , 1]), 0, 1)
  a[is.na(a)] <- 0
  tmp <- tempfile(fileext = ".png")
  png::writePNG(array(c(a, alpha), dim = c(dim(a)[1:2], 4)), tmp)
  img <- png::readPNG(tmp, native = TRUE)
  unlink(tmp)
  e <- as.vector(terra::ext(rgb))
  annotation_raster(img, xmin = e[["xmin"]], xmax = e[["xmax"]],
                    ymin = e[["ymin"]], ymax = e[["ymax"]], interpolate = TRUE)
}

#' Illuminated contours
#'
#' The ADD contours at 500 m, with an index line every 1000 m. Each line's
#' width and opacity follow the hillshade at its midpoint, so contours are
#' heavier and darker on shaded slopes, as on the priority map.
#'
#' @param window_bbox An sf bbox in the map CRS.
#' @param crs_display The map CRS.
#' @param hs The hillshade from [build_priority_terrain()].
#' @param paths The list from [frazil_paths()].
#' @return An sf object of lines with `level`, `is_index`, `lw` and `alpha`
#'   columns, for `geom_sf(aes(linewidth = lw, alpha = alpha))` with identity
#'   scales.
#' @export
illuminated_contours <- function(window_bbox, crs_display, hs, paths = frazil_paths()) {
  contour_path <- require_path(paths, "contours", "The ADD contour layer")
  wkt <- sf::st_as_text(sf::st_transform(sf::st_as_sfc(window_bbox), 3031))
  ct <- sf::st_read(contour_path, quiet = TRUE, wkt_filter = wkt) |>
    rename(level = contour) |>
    mutate(level = as.numeric(level)) |>
    filter(level %in% seq(500, 4000, by = 500)) |>
    sf::st_transform(crs_display)
  # Two casts: the features are multi-part, and a direct cast keeps only the
  # first line of each.
  ct <- sf::st_cast(sf::st_cast(ct, "MULTILINESTRING", warn = FALSE), "LINESTRING",
                    warn = FALSE)
  ct <- ct[as.numeric(sf::st_length(ct)) > 500, ]
  ct$is_index <- ct$level %in% seq(1000, 4000, by = 1000)
  h  <- terra::extract(hs, terra::vect(sf::st_centroid(sf::st_geometry(ct))))[[2]]
  hn <- (h - min(h, na.rm = TRUE)) / diff(range(h, na.rm = TRUE))
  hn[is.na(hn)] <- 0.5
  ct$lw    <- ifelse(ct$is_index, 0.30, 0.15) * (1.8 - 1.2 * hn)
  ct$alpha <- pmin(pmax(ifelse(ct$is_index, 0.50, 0.30) * (1.5 - 0.9 * hn), 0.05), 0.70)
  ct
}
