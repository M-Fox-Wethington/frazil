# ------------------------------------------------------------------------------
# BRI house style
#
# COLORS, theme_bri() and geom_halo_points() are copied from the design-library
# repository (templates/r/bri_figure_style.R, compiled from the bri-figure-style
# skill) on 2026-10-10, so the package installs without that repository. Keep
# them in step with the skill when either changes.
# ------------------------------------------------------------------------------

COLORS <- list(
  # Map structure
  parchment    = "#f5f2ed",
  ocean        = "#8facc0",
  land         = "#d8dce0",
  coastline    = "#3d5a6e",
  # Text and rules (Rams neutrals)
  braun_white  = "#F2F0EC",
  title        = "#1D1D1B",
  subtitle     = "#323733",
  muted        = "#8C8C84",
  grid         = "#DADCCF",
  # Data colors, in categorical order
  orange       = "#CC6324",
  slate        = "#3B4B59",
  yellow       = "#ECC046",
  red          = "#AF2E1B",
  sand         = "#BFA07A",
  navy         = "#003452",
  brown        = "#5E483E",
  print_blue   = "#285388",
  # Extras, kept out of the categorical order
  braun_orange = "#F3672B",
  lemon        = "#F8E538",
  teak         = "#AF6D40",
  dark_brown   = "#392F2D",
  olive        = "#545850",
  aluminum     = "#B1AFAD",
  cream        = "#D9C3B0",
  paper        = "#E2D9CE"
)

#' The BRI color palette
#'
#' The Dieter Rams (Braun) palette of the BRI figure style, as a named list of
#' hex colors: the map structure colors (`parchment`, `ocean`, `land`,
#' `coastline`), the text and rule neutrals (`braun_white`, `title`,
#' `subtitle`, `muted`, `grid`), the eight data colors in categorical order
#' (`orange`, `slate`, `yellow`, `red`, `sand`, `navy`, `brown`, `print_blue`)
#' and the extras.
#'
#' @format A named list of 25 hex color strings.
#' @export
bri_colors <- COLORS

#' The BRI ggplot2 theme
#'
#' The print-sized theme of the BRI figure style. [theme_slide()] enlarges its
#' text for a slide.
#'
#' @param base_size Base font size in points.
#' @param ground Background color: `"white"`, the parchment `"#f5f2ed"` of the
#'   terrain maps, or the Braun white `"#F2F0EC"` of standalone figures.
#' @return A ggplot2 theme.
#' @export
theme_bri <- function(base_size = 11, ground = "white") {
  ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(
      plot.background   = ggplot2::element_rect(fill = ground, color = NA),
      panel.background  = ggplot2::element_rect(fill = ground, color = NA),
      panel.grid.major  = ggplot2::element_line(color = COLORS$grid, linewidth = 0.2),
      panel.grid.minor  = ggplot2::element_blank(),
      plot.title        = ggplot2::element_text(size = 16, face = "bold",
                                                color = COLORS$title,
                                                margin = ggplot2::margin(b = 4)),
      plot.subtitle     = ggplot2::element_text(size = 10, color = COLORS$subtitle,
                                                margin = ggplot2::margin(b = 8)),
      plot.caption      = ggplot2::element_text(size = 7, color = COLORS$muted,
                                                hjust = 1, margin = ggplot2::margin(t = 8),
                                                lineheight = 1.3),
      axis.title        = ggplot2::element_text(size = 9, color = COLORS$subtitle),
      axis.text         = ggplot2::element_text(size = 8, color = COLORS$muted),
      legend.title      = ggplot2::element_text(size = 9, face = "bold",
                                                color = COLORS$title),
      legend.text       = ggplot2::element_text(size = 8, color = COLORS$subtitle),
      legend.background = ggplot2::element_rect(fill = ggplot2::alpha(ground, 0.85),
                                                color = NA),
      strip.text        = ggplot2::element_text(size = 10, face = "bold",
                                                color = COLORS$title),
      plot.margin       = ggplot2::margin(12, 16, 10, 12)
    )
}

#' Site markers with a white halo
#'
#' Three stacked `geom_sf()` point layers: a white halo, the colored point, and
#' a faint dark outline ring, as in the BRI figure style.
#'
#' @param data An sf object of points.
#' @param color Point color.
#' @param size Point size.
#' @return A list of three ggplot2 layers.
#' @export
geom_halo_points <- function(data, color = COLORS$orange, size = 3.0) {
  list(
    ggplot2::geom_sf(data = data, color = "white", size = size + 1.8, shape = 16),
    ggplot2::geom_sf(data = data, color = color, size = size, shape = 16),
    ggplot2::geom_sf(data = data, color = ggplot2::alpha(COLORS$title, 0.45),
                     shape = 1, size = size, stroke = 0.32)
  )
}

# --- Slide geometry, ground and text ------------------------------------------
# Every figure is a standalone 16:9 slide, 13.333 x 7.5 in, with text sized so
# the smallest label is at least 14 pt on the slide.
SLIDE_W <- 13.333
SLIDE_H <- 7.5
GROUND  <- COLORS$braun_white
SOURCE_NSIDC <- "Data: NSIDC Sea Ice Index, Version 4 (G02135)"

TXT <- list(title = 30, axis_title = 19, axis_text = 17, legend_title = 17,
            legend_text = 16, label = 17, small = 15, caption = 15)

# geom_text() size from a size in points.
pt <- function(x) x / ggplot2::.pt

# Captions use olive, darker than the print-grade muted gray, so the source line
# stays legible on a projector.
CAPTION_COLOR <- COLORS$olive

#' The slide theme
#'
#' [theme_bri()] with its text enlarged for a 13.333 by 7.5 inch slide: a 30 pt
#' title, 17 pt axis text and a 15 pt caption, so the smallest label is at least
#' 14 pt when the figure fills the slide.
#'
#' @param ground Background color. The charts use the Braun white
#'   `bri_colors$braun_white`; the maps use the parchment `bri_colors$parchment`.
#' @return A ggplot2 theme.
#' @export
theme_slide <- function(ground = GROUND) {
  theme_bri(ground = ground) +
    theme(
      plot.title    = element_text(size = TXT$title, face = "bold",
                                   color = COLORS$title, margin = margin(b = 14)),
      plot.title.position   = "plot",
      plot.caption  = element_text(size = TXT$caption, color = CAPTION_COLOR,
                                   hjust = 0, margin = margin(t = 14)),
      plot.caption.position = "plot",
      axis.title    = element_text(size = TXT$axis_title, color = COLORS$subtitle),
      axis.text     = element_text(size = TXT$axis_text, color = COLORS$subtitle),
      legend.title  = element_text(size = TXT$legend_title, face = "bold",
                                   color = COLORS$title),
      legend.text   = element_text(size = TXT$legend_text, color = COLORS$subtitle),
      panel.grid.major = element_line(color = COLORS$grid, linewidth = 0.6),
      plot.margin   = margin(0.32, 0.55, 0.28, 0.50, unit = "in")
    )
}

#' Write a figure as a slide-sized PNG and PDF
#'
#' Writes `name.png` through ragg at `dpi` (8000 by 4500 pixels at the default
#' 600 dpi) and `name.pdf` through `cairo_pdf`, both 13.333 by 7.5 inches, into
#' `out_dir`. The figure functions call this themselves.
#'
#' @param plot A ggplot or cowplot object.
#' @param name File name without extension.
#' @param out_dir Folder to write into. `NULL` writes nothing.
#' @param width,height Size in inches.
#' @param bg Background color.
#' @param dpi Resolution of the PNG.
#' @return The two paths, invisibly, or `NULL` when nothing was written.
#' @export
save_slide <- function(plot, name, out_dir = frazil_paths()$output, width = SLIDE_W,
                       height = SLIDE_H, bg = GROUND, dpi = 600) {
  if (is.null(out_dir)) return(invisible(NULL))
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  png_path <- file.path(out_dir, paste0(name, ".png"))
  pdf_path <- file.path(out_dir, paste0(name, ".pdf"))
  ggplot2::ggsave(png_path, plot, width = width, height = height, units = "in",
                  dpi = dpi, device = ragg::agg_png, bg = bg)
  ggplot2::ggsave(pdf_path, plot, width = width, height = height, units = "in",
                  device = grDevices::cairo_pdf, bg = bg)
  message("Saved ", png_path, " and ", basename(pdf_path))
  invisible(c(png = png_path, pdf = pdf_path))
}

# --- Map colors ---------------------------------------------------------------
# The maps use the terrain of the Peninsula priority-sites map (the
# imhof-peninsula-priority-map skill). Ocean, ice-shelf, coastline and parchment
# colors are the priority map's own.
MAP_PAPER     <- "#f5f2ed"
MAP_OCEAN     <- "#8facc0"
MAP_COAST     <- "#3d5a6e"
MAP_TEXT      <- "#2c3e50"
SHELF_FILL    <- "#d4e4ef"
SHELF_BORDER  <- "#a8c0d2"
ROCK_BORDER   <- "#8b6914"
CONTOUR_COLOR <- "#5a4a3a"
OTHER_LAND    <- "#d6d0c4"          # land outside Antarctica, where REMA has no data
NODATA_FILL   <- COLORS$aluminum    # grid cells with no ice value next to the coast
EDGE_COLOR    <- COLORS$yellow
SITE_COLOR    <- COLORS$orange

TERRAIN_ICE  <- c("#b0c4d6", "#bdd0df", "#cadce8", "#d7e6ef", "#e3edf4",
                  "#edf3f7", "#f4f7fa", "#fafbfd", "#ffffff")
TERRAIN_ROCK <- c("#9e7e56", "#b8956a", "#c5a47a", "#cda878", "#d8b888",
                  "#e0c898", "#e6d4a8", "#eee0bc", "#f0e4c4")

# Sea ice runs from a pale blue just lighter than the ocean at 15% to white at
# full cover, so the pack reads as white and the loose ice at the edge fades
# toward open water.
CONC_COLORS <- c("#B9CBD8", "#DCE6EE", "#F2F6F9", "#FFFFFF")
CONC_STOPS  <- c(15, 40, 70, 100)

#' The sea ice concentration fill scale
#'
#' A gradient from a pale blue at 15% cover, the threshold that defines extent,
#' to white at 100%.
#'
#' @param ... Passed to [ggplot2::scale_fill_gradientn()].
#' @return A ggplot2 scale.
#' @export
scale_fill_conc <- function(...) {
  scale_fill_gradientn(
    colours = CONC_COLORS, values = scales::rescale(CONC_STOPS, from = c(15, 100)),
    limits = c(15, 100), breaks = c(15, 50, 100), labels = c("15", "50", "100"),
    na.value = NA, name = "Ice cover (%)", ...
  )
}

# Rams diverging ramp, reversed so that less ice is warm and more ice cool.
CHANGE_COLORS <- c(COLORS$red, COLORS$orange, COLORS$grid, COLORS$print_blue, COLORS$navy)

# Penguin species colors for the colony map, in the Rams categorical order, with
# the focal orange on the emperor penguin, the species that breeds on sea ice.
SPECIES_COLORS <- c(EMPE = COLORS$orange, ADPE = COLORS$red, CHPE = COLORS$yellow,
                    GEPE = COLORS$slate, MCPE = COLORS$sand, KIPE = COLORS$navy)
SPECIES_NAMES  <- c(EMPE = "Emperor", ADPE = "Ad\u00e9lie", CHPE = "Chinstrap",
                    GEPE = "Gentoo", MCPE = "Macaroni", KIPE = "King")

# "7 October 2026"
fmt_date <- function(d) paste(as.integer(format(d, "%d")), format(d, "%B %Y"))

# A minus sign in place of a hyphen, for axis labels.
minus <- function(x) sub("-", "\u2212", x)
