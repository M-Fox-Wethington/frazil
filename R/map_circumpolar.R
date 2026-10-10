# --- Circumpolar map, shared by the still map, the animation and the change maps

#' The static layers of a circumpolar map
#'
#' Everything a circumpolar map needs that does not change from day to day,
#' built once: the ADD coastline and ice shelves in the grid's own projection
#' (EPSG:3412, so the 25 km cells are never resampled), the alignment check,
#' the continental terrain from the REMA 1 km mosaic, the land outside
#' Antarctica, a 60S circle and the outline of a regional window.
#'
#' At 1.5 km the terrain is finer than the roughly 2.4 km each pixel covers on
#' a 600 dpi slide. At that scale rock outlines and contours would be specks,
#' so only the rock colors are kept.
#'
#' @param raw A grid from [read_conc_raw()].
#' @param frame A [map_window()] to outline, or `NULL`.
#' @param grid_res Cell size of the terrain in meters.
#' @param z_factor Vertical exaggeration; see [build_priority_terrain()].
#' @param paths The list from [frazil_paths()].
#' @return A list used by [circumpolar_map()] and [circumpolar_change_map()].
#' @export
circumpolar_base <- function(raw, frame = peninsula_window(), grid_res = 1500, z_factor = 12,
                             paths = frazil_paths()) {
  dem <- require_path(paths, "dem_1km", "The REMA 1 km mosaic")
  crs_wkt <- terra::crs(raw)
  crs_map <- sf::st_crs(crs_wkt)
  e <- as.vector(terra::ext(raw))    # named xmin, xmax, ymin, ymax
  bbox <- sf::st_bbox(e[c("xmin", "ymin", "xmax", "ymax")], crs = crs_map)
  land <- load_land(crs_map, simplify_m = 1000, paths = paths)
  check_alignment(raw, land)
  lab60 <- sf::st_coordinates(sf::st_transform(
    sf::st_sfc(sf::st_point(c(-148, -60)), crs = 4326), crs_map))
  list(
    crs = crs_map, ext = e, land = land, lat = grid_latitude(raw),
    terrain = build_priority_terrain(
      dem, bbox, crs_wkt, grid_res = grid_res, land = land, z_factor = z_factor,
      cache_name = terrain_cache_name("antarctica", grid_res, z_factor),
      terrain_cache = paths$terrain_cache, paths = paths),
    other_land = other_land_cells(raw),
    lat60 = parallel_line(-60, crs_map), lab60 = lab60,
    frame = if (is.null(frame)) NULL else window_polygon(frame, crs_map)
  )
}

#' A circumpolar map of one day's sea ice
#'
#' @param base The list from [circumpolar_base()].
#' @param raw The day's grid from [read_conc_raw()].
#' @param month_num Month of the grid, for the median ice edge.
#' @param show_frame Outline the regional window held in `base`.
#' @param terrain_layer A prebuilt [rgb_annotation()] of the terrain, which
#'   lets the animation build it once for all its frames.
#' @param cache The folder [nsidc_download()] filled, for the median ice edge.
#' @return A ggplot object.
#' @export
circumpolar_map <- function(base, raw, month_num, show_frame = TRUE, terrain_layer = NULL,
                            cache = frazil_paths()$cache) {
  conc   <- conc_percent(raw)
  nodata <- raster_polygons(terra::ifel(raw >= 2530 & raw <= 2550 & base$lat <= -60,
                                        1, NA))
  edge   <- read_median_edge(month_num, cache) |> sf::st_transform(base$crs)
  if (is.null(terrain_layer)) terrain_layer <- rgb_annotation(base$terrain$rgb)
  p <- ggplot() +
    geom_spatraster(data = conc, interpolate = FALSE, maxcell = 1e6) +
    scale_fill_conc() +
    geom_sf(data = nodata, fill = NODATA_FILL, color = NA) +
    geom_sf(data = base$other_land, fill = OTHER_LAND, color = NA) +
    terrain_layer +
    geom_sf(data = base$land$shelves, fill = alpha(SHELF_FILL, 0.50), color = SHELF_BORDER,
            linewidth = 0.15) +
    geom_sf(data = base$land$land, fill = NA, color = MAP_COAST, linewidth = 0.15) +
    geom_sf(data = base$lat60, color = alpha(MAP_COAST, 0.55), linewidth = 0.35,
            linetype = "13") +
    annotate("text", x = base$lab60[1], y = base$lab60[2], label = "60\u00b0S",
             color = MAP_COAST, size = pt(TXT$small), hjust = 1.15, vjust = -0.3) +
    geom_sf(data = edge, color = MAP_COAST, linewidth = 2.0) +
    geom_sf(data = edge, color = EDGE_COLOR, linewidth = 1.0)
  if (show_frame && !is.null(base$frame)) {
    p <- p + geom_sf(data = base$frame, fill = NA, color = SITE_COLOR, linewidth = 0.9)
  }
  p +
    coord_sf(crs = base$crs, xlim = unname(base$ext[c("xmin", "xmax")]),
             ylim = unname(base$ext[c("ymin", "ymax")]), expand = FALSE, datum = NA) +
    theme_map_slide(edge_labels = FALSE)
}

# The boundary meridians of a sector on the circumpolar map, from the coast (or
# the ice-shelf front, as the grid's land mask has it) out to 50S.
sector_boundary_lines <- function(sector, raw, crs, lat_to = -50) {
  r <- sector_row(sector)
  from <- c(coast_latitude(r$west, raw), coast_latitude(r$east, raw))
  sector_meridians(sector, lat_from = from, lat_to = lat_to, crs = crs)
}

#' A circumpolar map of a change raster
#'
#' Draws a raster of change on the grid in the circumpolar map's style, with
#' the boundaries of one NSIDC sector dashed.
#'
#' @inheritParams circumpolar_map
#' @param change A SpatRaster on the grid, as from [cover_change()] or
#'   [season_change()].
#' @param limits Range of the diverging color scale; values beyond it are
#'   squished.
#' @param sector The sector whose boundaries are drawn, or `NULL`.
#' @return A ggplot object.
#' @export
circumpolar_change_map <- function(base, raw, change, limits, terrain_layer = NULL,
                                   sector = "Weddell") {
  if (is.null(terrain_layer)) terrain_layer <- rgb_annotation(base$terrain$rgb)
  nodata <- raster_polygons(terra::ifel(raw >= 2530 & raw <= 2550 & base$lat <= -60, 1, NA))
  p <- ggplot() +
    geom_spatraster(data = change, interpolate = FALSE, maxcell = 1e6) +
    scale_fill_gradientn(colours = CHANGE_COLORS, limits = limits, oob = scales::squish,
                         na.value = NA, guide = "none") +
    geom_sf(data = nodata, fill = NODATA_FILL, color = NA) +
    geom_sf(data = base$other_land, fill = OTHER_LAND, color = NA) +
    terrain_layer +
    geom_sf(data = base$land$shelves, fill = alpha(SHELF_FILL, 0.50), color = SHELF_BORDER,
            linewidth = 0.15) +
    geom_sf(data = base$land$land, fill = NA, color = MAP_COAST, linewidth = 0.15) +
    geom_sf(data = base$lat60, color = alpha(MAP_COAST, 0.55), linewidth = 0.35,
            linetype = "13")
  if (!is.null(sector)) {
    p <- p + geom_sf(data = sector_boundary_lines(sector, raw, base$crs), color = MAP_COAST,
                     linewidth = 0.6, linetype = "42")
  }
  p +
    coord_sf(crs = base$crs, xlim = unname(base$ext[c("xmin", "xmax")]),
             ylim = unname(base$ext[c("ymin", "ymax")]), expand = FALSE, datum = NA) +
    theme_map_slide(edge_labels = FALSE)
}
