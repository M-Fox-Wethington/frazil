#' The MAPPPD regions on the Weddell Sea's coasts
#'
#' A colony counts as on the Weddell Sea's coasts when its MAPPPD region is one
#' of these and it lies between 62W and 20E. The longitude bound is needed
#' because two regions reach beyond the Weddell: Palmer Land also covers the
#' Bellingshausen coast, and Queen Maud Land runs east past the sector edge at
#' 20E. The 62W bound keeps the emperor colonies on the Peninsula's east coast,
#' which sit just west of 60W.
#'
#' @return A character vector of region names.
#' @export
weddell_regions <- function() {
  c("Northeast Antarctic Peninsula", "Palmer Land", "Queen Elizabeth Land",
    "Coats Land", "Queen Maud Land", "South Orkney Islands")
}

#' Figure: penguin colonies around a sea
#'
#' The regional base map with every MAPPPD penguin colony in the frame.
#' Colonies on the chosen coasts are colored by species; the rest of the
#' frame's colonies are small gray dots for context. Where several species
#' share a site, each species is nudged `nudge_m` meters in its own direction,
#' so the site shows as a small cluster and no species hides another. Emperor
#' penguin colonies, the species that breeds on sea ice, are drawn on top and
#' named.
#'
#' A marker means MAPPPD holds at least one record of that species at the site;
#' it is not evidence the colony is occupied now. Needs the `mapppdr` package
#' from `CCheCastaldo/mapppdr` on GitHub.
#'
#' @param window A [map_window()].
#' @param date The day of sea ice drawn, or `NULL` for the latest.
#' @param regions MAPPPD regions whose colonies are colored.
#' @param lon_bounds Longitude bounds, east positive, that a colored colony
#'   must also fall within.
#' @param nudge_m Distance co-located species are pushed apart, in meters.
#' @param title Slide title.
#' @inheritParams fig_map_region
#' @return The cowplot object, invisibly, with attribute `colonies`, a data
#'   frame of the colored colony-species records.
#' @export
fig_map_colonies <- function(window = weddell_window(), date = NULL,
                             regions = weddell_regions(), lon_bounds = c(-62, 20),
                             nudge_m = 18000, title = "Penguin colonies around the Weddell Sea",
                             out_dir = frazil_paths()$output,
                             name = paste0("map_", window$name, "_colonies"), dem = NULL,
                             paths = frazil_paths()) {
  check_installed("mapppdr", reason = "for the colony map (CCheCastaldo/mapppdr on GitHub)")
  L <- region_layers(window, date, dem = dem, paths = paths)

  col <- mapppdr::sites |>
    inner_join(mapppdr::site_species, by = "site_id") |>
    filter(species_id %in% names(SPECIES_COLORS))
  col_sf <- sf::st_as_sf(col, coords = c("longitude", "latitude"), crs = 4326,
                         remove = FALSE) |>
    sf::st_transform(window$crs)
  xy <- sf::st_coordinates(col_sf)
  col_sf <- col_sf[xy[, 1] > window$xlim[1] & xy[, 1] < window$xlim[2] &
                   xy[, 2] > window$ylim[1] & xy[, 2] < window$ylim[2], ]
  col_sf$on_coast <- col_sf$region %in% regions &
    col_sf$longitude >= lon_bounds[1] & col_sf$longitude <= lon_bounds[2]

  # Nudge co-located species apart, each in a fixed direction for its species.
  spp_order <- c("CHPE", "GEPE", "ADPE", "MCPE", "KIPE", "EMPE")
  angle <- setNames(seq(0, 2 * pi, length.out = 7)[1:6], spp_order)
  xy <- sf::st_coordinates(col_sf)
  shared <- ave(seq_len(nrow(col_sf)), col_sf$site_id, FUN = length) > 1
  r <- ifelse(shared, nudge_m, 0)
  col_pts <- data.frame(site_id = col_sf$site_id, site_name = col_sf$site_name,
                        species_id = col_sf$species_id, on_coast = col_sf$on_coast,
                        x = xy[, 1] + r * cos(angle[col_sf$species_id]),
                        y = xy[, 2] + r * sin(angle[col_sf$species_id])) |>
    mutate(species_id = factor(species_id, levels = spp_order)) |>
    arrange(species_id)

  wed <- col_pts |> filter(on_coast)
  ctx <- col_pts |> filter(!on_coast)
  message("Colonies in the frame: ", length(unique(col_pts$site_id)),
          "; on the chosen coasts: ", length(unique(wed$site_id)))
  message(paste(utils::capture.output(print(as.data.frame(
    wed |> count(species_id, name = "sites")))), collapse = "\n"))
  emperors <- wed |> filter(species_id == "EMPE")
  message("Emperor colonies: ", paste(emperors$site_name, collapse = ", "))

  # Context colonies are light gray, so they cannot be mistaken for gentoo slate.
  context_color <- alpha(COLORS$aluminum, 0.9)
  p_map <- region_map(L) +
    geom_point(data = ctx, aes(x, y), color = context_color, size = 1.6) +
    geom_point(data = wed, aes(x, y), color = "white", size = 4.4) +
    geom_point(data = wed, aes(x, y, color = species_id), size = 3.0) +
    geom_point(data = wed, aes(x, y), shape = 1, stroke = 0.35, size = 3.0,
               color = alpha(COLORS$title, 0.45)) +
    scale_color_manual(values = SPECIES_COLORS, guide = "none") +
    ggrepel::geom_label_repel(
      data = emperors, aes(x, y, label = site_name), size = pt(14), fontface = "bold",
      color = COLORS$title, fill = alpha(MAP_PAPER, 0.88), label.size = 0.25,
      label.r = unit(0.08, "lines"), label.padding = unit(0.12, "lines"),
      segment.color = COLORS$title, segment.size = 0.4, min.segment.length = 0,
      box.padding = 0.35, point.padding = 0.3, force = 2, seed = 11, max.overlaps = Inf)

  # The legend lists only species that have a colony on the chosen coasts.
  present <- rev(spp_order)[rev(spp_order) %in% as.character(wed$species_id)]
  keys <- data.frame(
    type  = c(rep("species", length(present)), "context", "edge"),
    label = c(paste(SPECIES_NAMES[present], "penguin"), "Other coasts",
              paste("Usual", month.name[L$month], "edge")),
    color = c(SPECIES_COLORS[present], NA, NA))

  panel_w  <- window_panel_width(window)
  legend_x <- window_legend_x(window)
  caption  <- paste0("Sea ice on ", fmt_date(L$date), " from the NSIDC Sea Ice Index, ",
                     "Version 4. Colonies: MAPPPD v", mapppd_version(),
                     ". Terrain: REMA v2.0, ", L$terrain_res, " m.")
  slide <- slide_canvas(title, caption) +
    cowplot::draw_plot(p_map, x = 0.0375, y = 0.085, width = panel_w, height = 0.79) +
    cowplot::draw_plot(key_panel(keys, row_gap = 0.125, box_h = 0.04), x = legend_x, y = 0.33,
                       width = 0.21, height = 0.56) +
    cowplot::draw_plot(conc_colorbar(length_in = 2.0, horizontal = TRUE), x = legend_x,
                       y = 0.10, width = 0.19, height = 0.18)

  save_slide(slide, name, out_dir, bg = MAP_PAPER)
  invisible(structure(slide, date = L$date, colonies = wed))
}

# "2.4" from a package version such as 2.4 or 2.4.1
mapppd_version <- function() {
  v <- tryCatch(as.character(utils::packageVersion("mapppdr")), error = function(e) NA)
  if (is.na(v)) return("")
  paste(head(strsplit(v, ".", fixed = TRUE)[[1]], 2), collapse = ".")
}
