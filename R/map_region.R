# --- Regional maps ------------------------------------------------------------

#' Everything a regional map needs, built once
#'
#' Reads the day's grid, checks it against the coastline, reads the ADD layers
#' within the window, builds or loads the terrain, moves the sea ice grid into
#' the map projection by nearest neighbor (which keeps every 25 km cell square
#' and every flag intact), and gathers the month's median ice edge, the
#' contours, the rock outlines and the sector boundaries that the window asks
#' for.
#'
#' @param window A [map_window()], such as [peninsula_window()] or
#'   [weddell_window()].
#' @param date The day, or `NULL` for the latest grid in the cache.
#' @param dem Path to the elevation mosaic, or `NULL` to use `dem_100m` from
#'   [frazil_paths()], falling back to `dem_300m` at 300 m when the 100 m
#'   mosaic is not on disk.
#' @param terrain_res,z_factor,ice_res Overrides of the window's settings.
#' @param paths The list from [frazil_paths()].
#' @return A list with the `window`, `date`, `month`, `land`, `terrain`,
#'   `conc`, `nodata`, `other_land`, `edge`, `contours`, `rock`, `sectors`,
#'   `terrain_res` and the grid value at each site.
#' @export
region_layers <- function(window, date = NULL, dem = NULL, terrain_res = window$terrain_res,
                          z_factor = window$z_factor, ice_res = window$ice_res,
                          paths = frazil_paths()) {
  g   <- daily_grid(date, paths$cache)
  raw <- g$raw
  check_alignment(raw, load_land(sf::st_crs(terra::crs(raw)), simplify_m = 1500,
                                 paths = paths))
  wkt  <- window_wkt_3031(window, buffer = 50000)
  land <- load_land(window$crs, window_wkt_3031 = wkt, paths = paths)

  if (is.null(dem)) {
    if (!is.null(paths$dem_100m) && file.exists(paths$dem_100m)) {
      dem <- paths$dem_100m
    } else if (!is.null(paths$dem_300m) && file.exists(paths$dem_300m)) {
      dem <- paths$dem_300m
      terrain_res <- max(terrain_res, 300)
      message("The REMA 100 m mosaic is not on disk; building the terrain at ",
              terrain_res, " m from ", basename(dem), ".")
    } else {
      stop("No elevation model: set frazil_paths(dem_100m = ) to the REMA 100 m mosaic.",
           call. = FALSE)
    }
  }
  terrain <- build_priority_terrain(
    dem, window$bbox, window$crs$wkt, grid_res = terrain_res, land = land,
    z_factor = z_factor, cache_name = terrain_cache_name(window$name, terrain_res, z_factor),
    terrain_cache = paths$terrain_cache, paths = paths)

  template <- terra::rast(xmin = window$xlim[1], xmax = window$xlim[2],
                          ymin = window$ylim[1], ymax = window$ylim[2],
                          resolution = ice_res, crs = window$crs$wkt)
  raw_map <- terra::project(raw, template, method = "near")
  month_num <- as.integer(format(g$date, "%m"))

  rock <- NULL
  if (isTRUE(window$rock_outlines)) {
    rock <- sf::st_as_sf(load_rock_vect(window$bbox, window$crs$wkt, paths = paths))
    rock <- rock[!sf::st_is_empty(rock), ]
  }
  list(
    window = window, date = g$date, month = month_num, raw = raw, land = land,
    terrain = terrain, terrain_res = terrain_res,
    conc = conc_percent(raw_map),
    nodata = raster_polygons(no_data_cells(raw_map, north_of = -60)),
    other_land = other_land_cells(raw_map),
    edge = read_median_edge(month_num, paths$cache) |> sf::st_transform(window$crs),
    contours = if (isTRUE(window$contours))
      illuminated_contours(window$bbox, window$crs, terrain$hs, paths = paths) else NULL,
    rock = rock,
    sectors = if (!is.null(window$sector))
      sector_meridians(window$sector, lat_from = -81, lat_to = -54, crs = window$crs) else NULL
  )
}

#' The base map of a region
#'
#' Draws the layers from [region_layers()] in the style of the BRI priority
#' map: sea ice under the terrain, ice shelves tinted, contours and rock
#' outlines where the window asks for them, the coastline, the sector
#' boundaries, the month's median ice edge, the graticule and a scale bar.
#' Place labels, sites and colonies are added by the figure functions.
#'
#' @param L The list from [region_layers()].
#' @param layers Extra ggplot2 layers drawn above the ice edge and below the
#'   graticule, such as place labels and site markers. Any `geom_sf()` layer
#'   has to go here, because a `geom_sf()` added after the map's `coord_sf()`
#'   replaces that coordinate system with its own default one.
#' @param scale_bar Draw a scale bar at the bottom right.
#' @return A ggplot object.
#' @export
region_map <- function(L, layers = NULL, scale_bar = TRUE) {
  w <- L$window
  p <- ggplot() +
    geom_spatraster(data = L$conc, interpolate = FALSE, maxcell = 1e7) +
    scale_fill_conc() +
    geom_sf(data = L$nodata, fill = NODATA_FILL, color = NA) +
    geom_sf(data = L$other_land, fill = OTHER_LAND, color = NA) +
    rgb_annotation(L$terrain$rgb, max_cols = w$terrain_max_cols) +
    geom_sf(data = L$land$shelves, fill = alpha(SHELF_FILL, 0.50), color = SHELF_BORDER,
            linewidth = 0.15)
  if (!is.null(L$rock)) {
    p <- p + geom_sf(data = L$rock, fill = NA, color = alpha(ROCK_BORDER, 0.30),
                     linewidth = 0.10)
  }
  if (!is.null(L$contours)) {
    p <- p +
      geom_sf(data = L$contours, aes(linewidth = .data$lw, alpha = .data$alpha),
              color = CONTOUR_COLOR) +
      scale_linewidth_identity() +
      scale_alpha_identity()
  }
  p <- p + geom_sf(data = L$land$land, fill = NA, color = MAP_COAST, linewidth = 0.20)
  if (!is.null(L$sectors)) {
    p <- p + geom_sf(data = L$sectors, color = MAP_COAST, linewidth = 0.6, linetype = "42")
  }
  p <- p +
    geom_sf(data = L$edge, color = MAP_COAST, linewidth = 2.2) +
    geom_sf(data = L$edge, color = EDGE_COLOR, linewidth = 1.1)
  if (!is.null(layers)) p <- p + layers
  p <- p +
    coord_sf(crs = w$crs, xlim = w$xlim, ylim = w$ylim, expand = FALSE,
             datum = sf::st_crs(4326), label_axes = w$label_axes) +
    scale_x_continuous(breaks = w$x_breaks) +
    scale_y_continuous(breaks = w$y_breaks)
  if (scale_bar) {
    p <- p + ggspatial::annotation_scale(
      location = "br", width_hint = 0.2, style = "ticks", text_col = MAP_COAST,
      line_col = MAP_COAST, text_cex = 1.25, line_width = 1.2,
      pad_x = unit(0.35, "in"), pad_y = unit(0.3, "in"))
  }
  if (isTRUE(w$north_arrow)) {
    p <- p + ggspatial::annotation_north_arrow(
      location = "tl", which_north = "true",
      style = ggspatial::north_arrow_minimal(line_col = MAP_COAST, text_col = MAP_COAST,
                                             fill = MAP_COAST, text_size = 14),
      height = unit(0.55, "in"), width = unit(0.4, "in"),
      pad_x = unit(0.25, "in"), pad_y = unit(0.2, "in"))
  }
  p + theme_map_slide()
}

#' Place-name labels for a map
#'
#' Repelled text labels: italic for water, bold for land, on a faint parchment
#' halo.
#'
#' @param places A data frame with `name`, `lon`, `lat` and `type` (`"water"`
#'   or anything else).
#' @param crs The map CRS.
#' @return A ggrepel layer, or `NULL` when `places` is `NULL`.
#' @export
place_labels <- function(places, crs) {
  if (is.null(places) || !nrow(places)) return(NULL)
  places_sf <- sf::st_as_sf(places, coords = c("lon", "lat"), crs = 4326) |>
    sf::st_transform(crs)
  ggrepel::geom_text_repel(
    data = places_sf,
    aes(label = .data$name, geometry = .data$geometry,
        fontface = ifelse(.data$type == "water", "italic", "bold")),
    stat = "sf_coordinates", size = pt(TXT$small), color = MAP_TEXT, lineheight = 0.9,
    bg.color = alpha(MAP_PAPER, 0.8), bg.r = 0.12, seed = 7, force = 0.2,
    max.overlaps = Inf)
}

# Site markers and their boxed labels, pushed by the `dx` and `dy` columns.
site_layers <- function(sites, crs) {
  if (is.null(sites) || !nrow(sites)) return(NULL)
  s  <- sites_sf(sites, crs)
  dx <- if (!is.null(sites$dx)) sites$dx else 0
  dy <- if (!is.null(sites$dy)) sites$dy else 0
  list(
    geom_halo_points(s, color = SITE_COLOR, size = 4.2),
    ggrepel::geom_label_repel(
      data = s, aes(label = .data$label, geometry = .data$geometry),
      stat = "sf_coordinates", nudge_x = dx, nudge_y = dy,
      size = pt(TXT$small), fontface = "bold", color = COLORS$title,
      fill = alpha(MAP_PAPER, 0.92), label.size = 0.3,
      label.r = unit(0.10, "lines"), segment.color = COLORS$title,
      segment.size = 0.5, min.segment.length = 0, seed = 42, max.overlaps = Inf)
  )
}

#' Default place names for the Peninsula and Weddell windows
#'
#' @return A data frame with `name`, `lon`, `lat` and `type`.
#' @export
peninsula_places <- function() {
  data.frame(
    name = c("Drake Passage", "Bellingshausen Sea", "Weddell Sea", "Antarctic Peninsula"),
    lon  = c(-64.5, -73.0, -50.5, -63.6),
    lat  = c(-60.6, -67.8, -67.6, -67.3),
    type = c("water", "water", "water", "land"),
    stringsAsFactors = FALSE)
}

#' @rdname peninsula_places
#' @export
weddell_places <- function() {
  data.frame(
    name = c("Weddell Sea", "Antarctic\nPeninsula", "South Orkney\nIslands",
             "Filchner-Ronne\nIce Shelf", "Coats Land", "Dronning Maud Land"),
    lon  = c(-40, -64, -45, -55, -24, 3),
    lat  = c(-68.5, -68.5, -60.7, -79.3, -77.6, -72.6),
    type = c("water", "land", "island", "land", "land", "land"),
    stringsAsFactors = FALSE)
}
