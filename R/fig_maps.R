# --- Map figures --------------------------------------------------------------

# Captions share one template. The circumpolar map draws no contours; the
# regional maps name their terrain resolution.
terrain_caption <- function(prefix = SOURCE_NSIDC, res_m = NULL) {
  if (is.null(res_m)) {
    paste0(prefix, ". Terrain: REMA v2.0 (Howat et al.). ",
           "Coastline and rock: SCAR Antarctic Digital Database.")
  } else {
    paste0(prefix, ". Terrain: REMA v2.0 (Howat et al.), ", res_m, " m. ",
           "Coastline, rock and contours: SCAR ADD.")
  }
}

#' Figure: sea ice around Antarctica on one day
#'
#' The circumpolar map of [circumpolar_map()] with the day's total extent, the
#' concentration colorbar and a key beside it. The sea ice grid is drawn in its
#' own projection (EPSG:3412), so the 25 km cells are not resampled, and the
#' coastline is checked against the grid before anything is drawn. Cells below
#' 15% are drawn as open water, matching the definition of extent.
#'
#' @param date The day, or `NULL` for the latest grid in the cache.
#' @param frame A [map_window()] outlined on the map, or `NULL`.
#' @param title Slide title, or `NULL` for "Antarctic sea ice on <date>".
#' @param paths The list from [frazil_paths()].
#' @inheritParams fig_extent_daily
#' @return The cowplot object, invisibly, with attributes `date` and `extent`.
#' @export
fig_map_circumpolar <- function(date = NULL, frame = peninsula_window(), title = NULL,
                                out_dir = frazil_paths()$output, name = "map_circumpolar",
                                paths = frazil_paths()) {
  g <- daily_grid(date, paths$cache)
  month_num <- as.integer(format(g$date, "%m"))
  base  <- circumpolar_base(g$raw, frame = frame, paths = paths)
  p_map <- circumpolar_map(base, g$raw, month_num, show_frame = !is.null(frame),
                           cache = paths$cache)

  ext_day <- read_daily_extent(paths$cache) |> filter(date == g$date)
  extent  <- if (nrow(ext_day)) ext_day$extent else NA_real_
  message("Map date: ", format(g$date), "; extent that day: ", extent, " million km2")

  keys <- data.frame(
    type  = c("edge", "nodata", if (!is.null(frame)) "frame"),
    label = c(paste0("Usual ice edge in\n", month.name[month_num], ", 1981\u20132010"),
              "No ice data near\nthe coast",
              if (!is.null(frame))
                paste0("Area of the\n", tools::toTitleCase(frame$name), " map")))
  if (is.null(title)) title <- paste("Antarctic sea ice on", fmt_date(g$date))

  slide <- slide_canvas(title, terrain_caption()) +
    cowplot::draw_plot(p_map, x = 0.0375, y = 0.085, width = 0.47, height = 0.79) +
    cowplot::draw_label("Sea ice extent", x = 0.56, y = 0.835, hjust = 0, vjust = 1,
                        size = TXT$legend_title, fontface = "bold", color = COLORS$title) +
    cowplot::draw_label(if (is.na(extent)) "" else sprintf("%.1f million km\u00b2", extent),
                        x = 0.56, y = 0.785, hjust = 0, vjust = 1, size = 32,
                        fontface = "bold", color = COLORS$title) +
    cowplot::draw_plot(conc_colorbar(), x = 0.56, y = 0.30, width = 0.19, height = 0.36,
                       hjust = 0) +
    cowplot::draw_plot(key_panel(keys), x = 0.745, y = 0.30, width = 0.23, height = 0.36)

  save_slide(slide, name, out_dir, bg = MAP_PAPER)
  invisible(structure(slide, date = g$date, extent = extent))
}

#' Figure: sea ice in a region on one day
#'
#' The regional map of [region_map()] on a slide with the concentration
#' colorbar and a key. [fig_map_peninsula()] and [fig_map_weddell()] call it
#' with their windows, place names and sites.
#'
#' @param window A [map_window()].
#' @param date The day, or `NULL` for the latest grid in the cache.
#' @param places A data frame of place names for [place_labels()], or `NULL`.
#' @param sites A data frame of sites for [sites_sf()], with optional `dx` and
#'   `dy` label offsets, or `NULL`.
#' @param site_label Key label for the sites.
#' @param title Slide title, or `NULL` for "Sea ice <window title> on <date>".
#' @param dem Path to the elevation mosaic; see [region_layers()].
#' @inheritParams fig_map_circumpolar
#' @inheritParams fig_extent_daily
#' @return The cowplot object, invisibly, with attributes `date` and `layers`
#'   (the list from [region_layers()]).
#' @export
fig_map_region <- function(window, date = NULL, places = NULL, sites = NULL,
                           site_label = "Survey sites", title = NULL,
                           out_dir = frazil_paths()$output, name = paste0("map_", window$name),
                           dem = NULL, paths = frazil_paths()) {
  L <- region_layers(window, date, dem = dem, paths = paths)
  p_map <- region_map(L, layers = c(list(place_labels(places, window$crs)),
                                    site_layers(sites, window$crs)))

  if (!is.null(sites)) {
    vals <- terra::extract(L$raw, terra::vect(sf::st_transform(sites_sf(sites, window$crs),
                                                               terra::crs(L$raw))))[[2]]
    message("Grid value at each site (2530 = coast cell, no concentration): ",
            paste(sprintf("%s %s", sites$label, vals), collapse = "; "))
  }
  keys <- data.frame(
    type  = c("edge", "nodata", if (!is.null(sites)) "site", if (!is.null(window$sector)) "sector"),
    label = c(paste0("Usual ice edge in\n", month.name[L$month], ", 1981\u20132010"),
              "No ice data near\nthe coast",
              if (!is.null(sites)) site_label,
              if (!is.null(window$sector))
                paste0(sector_row(window$sector)$name, " sector,\n",
                       sector_bounds_text(window$sector))))
  if (is.null(title)) title <- paste("Sea ice", window$title, "on", fmt_date(L$date))

  panel_w  <- window_panel_width(window)
  legend_x <- window_legend_x(window)
  slide <- slide_canvas(title, terrain_caption(res_m = L$terrain_res)) +
    cowplot::draw_plot(p_map, x = 0.0375, y = 0.085, width = panel_w, height = 0.79) +
    cowplot::draw_plot(conc_colorbar(), x = legend_x, y = 0.50, width = 0.19, height = 0.36,
                       hjust = 0) +
    cowplot::draw_plot(key_panel(keys), x = legend_x, y = 0.10, width = 0.21, height = 0.36)

  save_slide(slide, name, out_dir, bg = MAP_PAPER)
  invisible(structure(slide, date = L$date, layers = L))
}

#' Figures: the Peninsula and Weddell Sea maps
#'
#' [fig_map_region()] with [peninsula_window()], the Peninsula place names and
#' the BRI drone sites, or with [weddell_window()] and the Weddell place
#' names.
#'
#' @inheritParams fig_map_region
#' @param ... Passed to [fig_map_region()].
#' @return The cowplot object, invisibly.
#' @export
fig_map_peninsula <- function(date = NULL, window = peninsula_window(),
                              places = peninsula_places(), sites = bri_sites,
                              site_label = "BRI drone\nsurvey sites", ...) {
  fig_map_region(window, date, places = places, sites = sites, site_label = site_label, ...)
}

#' @rdname fig_map_peninsula
#' @export
fig_map_weddell <- function(date = NULL, window = weddell_window(), places = weddell_places(),
                            ...) {
  fig_map_region(window, date, places = places, ...)
}
