# --- Map windows --------------------------------------------------------------
# A window holds everything a regional map needs to know about where it is
# drawn: the projection, the frame in projected meters, and the resolution and
# shading settings for the terrain and the sea ice grid.

# A polar stereographic CRS turned to `lon_0`, true at `lat_ts`.
stere_crs <- function(lon_0, lat_ts = -71) {
  sf::st_crs(sprintf(paste("+proj=stere +lat_0=-90 +lat_ts=%g +lon_0=%g",
                           "+x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"), lat_ts, lon_0))
}

#' Define a map window
#'
#' The lower-level constructor behind [polar_window()] and [lonlat_window()].
#'
#' @param name Short name, used in file names and terrain cache names.
#' @param crs The map projection, as an sf `crs` or anything [sf::st_crs()]
#'   accepts.
#' @param xlim,ylim The frame in projected meters.
#' @param title The phrase that follows "Sea ice" in a map title, such as
#'   "around the Antarctic Peninsula".
#' @param terrain_res Cell size of the terrain in meters.
#' @param z_factor Vertical exaggeration of the hillshade. The priority map uses
#'   3 at 300 m; coarser grids need more.
#' @param ice_res Cell size in meters that the 25 km sea ice grid is moved onto
#'   by nearest neighbor. A divisor of 25,000 keeps every cell square.
#' @param x_breaks,y_breaks Graticule breaks in degrees.
#' @param label_axes Which edges carry graticule labels, as in
#'   [ggplot2::coord_sf()]. The Weddell map uses `"E--N"` so that meridians are
#'   labeled along the top, where they are spread out.
#' @param rock_outlines Draw faint outlines around the rock outcrops. Suits
#'   terrain at 300 m or finer.
#' @param contours Draw the ADD contours.
#' @param sector An NSIDC sector whose boundary meridians are drawn, or `NULL`.
#' @param north_arrow Draw a north arrow.
#' @param panel_width Share of the slide width given to the map panel, or
#'   `NULL` to size it from the frame's aspect ratio.
#' @param legend_x Left edge of the legend column as a share of the slide
#'   width, or `NULL` to place it just right of the map panel.
#' @param terrain_max_cols Widest terrain image drawn, in columns; see
#'   [rgb_annotation()].
#' @return An object of class `frazil_window`: a list with the arguments and a
#'   `bbox`.
#' @export
map_window <- function(name, crs, xlim, ylim, title = name, terrain_res = 500, z_factor = 3,
                       ice_res = 5000, x_breaks = waiver(), y_breaks = waiver(),
                       label_axes = waiver(), rock_outlines = FALSE, contours = TRUE,
                       sector = NULL, north_arrow = FALSE, panel_width = NULL,
                       legend_x = NULL, terrain_max_cols = 7000) {
  crs  <- sf::st_crs(crs)
  bbox <- sf::st_bbox(c(xmin = xlim[1], ymin = ylim[1], xmax = xlim[2], ymax = ylim[2]),
                      crs = crs)
  structure(list(name = name, title = title, crs = crs, xlim = xlim, ylim = ylim,
                 bbox = bbox, terrain_res = terrain_res, z_factor = z_factor,
                 ice_res = ice_res, x_breaks = x_breaks, y_breaks = y_breaks,
                 label_axes = label_axes, rock_outlines = rock_outlines,
                 contours = contours, sector = sector, north_arrow = north_arrow,
                 panel_width = panel_width, legend_x = legend_x,
                 terrain_max_cols = terrain_max_cols),
            class = "frazil_window")
}

# The map panel's share of the slide width: the window's own value, or one
# sized from the frame's aspect ratio plus room for the edge labels.
window_panel_width <- function(window) {
  if (!is.null(window$panel_width)) return(window$panel_width)
  diff(window$xlim) / diff(window$ylim) * 0.79 * SLIDE_H / SLIDE_W + 0.015
}

# The legend column's left edge: the window's own value, or just right of the
# map panel.
window_legend_x <- function(window) {
  if (!is.null(window$legend_x)) return(window$legend_x)
  0.0375 + window_panel_width(window) + 0.0225
}

#' @export
print.frazil_window <- function(x, ...) {
  cat(sprintf("Map window '%s': %.0f x %.0f km, terrain %g m (z %g), ice grid %g m\n",
              x$name, diff(x$xlim) / 1000, diff(x$ylim) / 1000, x$terrain_res,
              x$z_factor, x$ice_res))
  invisible(x)
}

#' A window centered on a point
#'
#' A polar stereographic window of a given half-size around a center, in a
#' projection turned to `lon_0`.
#'
#' @param center Longitude and latitude of the center.
#' @param half_size Half-width and half-height in meters.
#' @param lon_0 Central meridian of the projection. Defaults to the center's
#'   longitude.
#' @param lat_ts Latitude of true scale.
#' @param name Short name.
#' @param ... Passed to [map_window()].
#' @return A `frazil_window`.
#' @export
polar_window <- function(center, half_size, lon_0 = center[1], lat_ts = -71, name = "window",
                         ...) {
  crs <- stere_crs(lon_0, lat_ts)
  ctr <- sf::st_coordinates(sf::st_transform(
    sf::st_sfc(sf::st_point(center), crs = 4326), crs))
  map_window(name, crs, xlim = ctr[1] + c(-1, 1) * half_size[1],
             ylim = ctr[2] + c(-1, 1) * half_size[2], ...)
}

#' A window that holds a longitude-latitude box
#'
#' The frame is the bounding box of the box's edges in a polar stereographic
#' projection turned to `lon_0`, with the edges densified so the curved sides
#' are captured.
#'
#' @param box Named vector with `lon_min`, `lon_max`, `lat_min` and `lat_max`.
#' @param lon_0 Central meridian of the projection. Defaults to the middle of
#'   the box.
#' @param n Points along each edge.
#' @inheritParams polar_window
#' @return A `frazil_window`.
#' @export
lonlat_window <- function(box, lon_0 = mean(box[c("lon_min", "lon_max")]), lat_ts = -71,
                          name = "window", n = 200, ...) {
  crs  <- stere_crs(lon_0, lat_ts)
  edge <- rbind(
    cbind(seq(box[["lon_min"]], box[["lon_max"]], length.out = n), box[["lat_min"]]),
    cbind(box[["lon_max"]], seq(box[["lat_min"]], box[["lat_max"]], length.out = n)),
    cbind(seq(box[["lon_max"]], box[["lon_min"]], length.out = n), box[["lat_max"]]),
    cbind(box[["lon_min"]], seq(box[["lat_max"]], box[["lat_min"]], length.out = n)))
  bb <- sf::st_bbox(sf::st_transform(
    sf::st_sfc(sf::st_polygon(list(rbind(edge, edge[1, ]))), crs = 4326), crs))
  map_window(name, crs, xlim = unname(bb[c("xmin", "xmax")]),
             ylim = unname(bb[c("ymin", "ymax")]), ...)
}

#' The Antarctic Peninsula window
#'
#' Centered between the four BRI drone sites and sized to fill a 16:9 slide
#' beside its legend, in the rotated stereographic projection of the BRI
#' Peninsula maps (central meridian 60W, true scale at 71S). The terrain is
#' built at 125 m from the REMA 100 m mosaic with rock outlines and contours,
#' and the sea ice grid is moved onto 2.5 km cells.
#'
#' @param center Longitude and latitude of the center.
#' @param half_size Half-width and half-height in meters.
#' @param ... Overrides passed to [map_window()].
#' @return A `frazil_window`.
#' @export
peninsula_window <- function(center = c(-60.5, -64.4), half_size = c(820e3, 520e3), ...) {
  args <- list(center = center, half_size = half_size, lon_0 = -60, name = "peninsula",
               title = "around the Antarctic Peninsula", terrain_res = 125,
               z_factor = 3, ice_res = 2500, x_breaks = seq(-80, -40, by = 10),
               y_breaks = seq(-70, -58, by = 2), rock_outlines = TRUE,
               north_arrow = TRUE, panel_width = 0.715, legend_x = 0.775)
  dots <- list(...)
  args[names(dots)] <- dots
  do.call(polar_window, args)
}

#' The Weddell Sea window
#'
#' The box 62W to 22E and 81S to 55S in a polar stereographic projection turned
#' to 20W, the middle of NSIDC's Weddell Sea sector, so the sector stands
#' upright on the slide. The terrain is built at 500 m with an exaggeration of
#' 5, the sea ice grid is moved onto 5 km cells, the sector's boundary meridians
#' are drawn, and the meridians are labeled along the top edge because they
#' converge at the bottom.
#'
#' @param box Named vector with `lon_min`, `lon_max`, `lat_min` and `lat_max`.
#' @param ... Overrides passed to [map_window()].
#' @return A `frazil_window`.
#' @export
weddell_window <- function(box = c(lon_min = -62, lon_max = 22, lat_min = -81, lat_max = -55),
                           ...) {
  args <- list(box = box, lon_0 = -20, name = "weddell", title = "in the Weddell Sea",
               terrain_res = 500, z_factor = 5, ice_res = 5000,
               x_breaks = seq(-60, 20, by = 20), y_breaks = seq(-80, -55, by = 5),
               label_axes = "E--N", sector = "Weddell", panel_width = 0.73,
               legend_x = 0.785, terrain_max_cols = 11000)
  dots <- list(...)
  args[names(dots)] <- dots
  do.call(lonlat_window, args)
}

# The window's frame as a densified polygon in another CRS, for outlining it on
# the circumpolar map.
window_polygon <- function(window, crs) {
  w <- window
  box <- sf::st_sfc(sf::st_polygon(list(rbind(
    c(w$xlim[1], w$ylim[1]), c(w$xlim[2], w$ylim[1]), c(w$xlim[2], w$ylim[2]),
    c(w$xlim[1], w$ylim[2]), c(w$xlim[1], w$ylim[1])))), crs = w$crs)
  sf::st_transform(sf::st_segmentize(box, 10000), crs)
}

# The window, buffered, as WKT in EPSG:3031 for reading the ADD layers.
window_wkt_3031 <- function(window, buffer = 50000) {
  sf::st_as_text(sf::st_geometry(sf::st_transform(
    sf::st_buffer(sf::st_as_sfc(window$bbox), buffer), 3031)))
}

# Cache name for a window's terrain, for example seaice_peninsula_terrain_125m
# or seaice_weddell_terrain_500m_z5.
terrain_cache_name <- function(name, grid_res, z_factor) {
  sprintf("seaice_%s_terrain_%gm%s", name, grid_res,
          if (z_factor == 3) "" else sprintf("_z%g", z_factor))
}
