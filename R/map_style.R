# --- Map slide furniture ------------------------------------------------------

#' The map theme for the slides
#'
#' The ocean panel, neatline and graticule of the BRI neighborhood maps, with
#' edge labels sized for a projector.
#'
#' @param edge_labels Draw graticule labels along the edges.
#' @return A ggplot2 theme.
#' @export
theme_map_slide <- function(edge_labels = TRUE) {
  theme_minimal() +
    theme(
      plot.background  = element_rect(fill = MAP_PAPER, color = NA),
      panel.background = element_rect(fill = MAP_OCEAN, color = NA),
      panel.border     = element_rect(color = MAP_COAST, fill = NA, linewidth = 0.7),
      panel.grid.major = element_line(color = alpha(MAP_COAST, 0.22), linewidth = 0.3),
      panel.grid.minor = element_blank(),
      axis.text  = if (edge_labels) element_text(size = 14, color = MAP_COAST)
                   else element_blank(),
      axis.ticks = element_line(color = alpha(MAP_COAST, 0.6), linewidth = 0.3),
      axis.ticks.length = unit(3, "pt"),
      axis.title = element_blank(),
      plot.margin = margin(0, 0, 0, 0),
      legend.position = "none"
    )
}

#' A hand-drawn legend panel for the map slides
#'
#' One row per key, so line, swatch, frame and marker keys can sit in one
#' column.
#'
#' @param keys A data frame with columns `type` and `label`, plus `color` for
#'   species keys. `type` is one of `"edge"` (the median ice edge), `"nodata"`
#'   (the gray no-data swatch), `"frame"` (the orange frame of the Peninsula
#'   map), `"site"` (a site marker), `"sector"` (a dashed sector boundary),
#'   `"species"` (a colored colony marker) or `"context"` (a gray colony
#'   marker).
#' @param row_gap Vertical spacing between rows, as a share of the panel.
#' @param box_h Half-height of the swatch box behind each key.
#' @return A ggplot object.
#' @export
key_panel <- function(keys, row_gap = 0.22, box_h = 0.07) {
  n <- nrow(keys)
  y <- 1 - row_gap * (seq_len(n) - 0.5)
  if (is.null(keys$color)) keys$color <- NA
  p <- ggplot() + xlim(0, 1) + ylim(0, 1) + theme_void()
  for (i in seq_len(n)) {
    yi <- y[i]
    p <- p + annotate("rect", xmin = 0.00, xmax = 0.13, ymin = yi - box_h, ymax = yi + box_h,
                      fill = MAP_OCEAN, color = NA)
    p <- p + switch(keys$type[i],
      sector  = annotate("segment", x = 0.01, xend = 0.12, y = yi, yend = yi,
                         color = MAP_COAST, linewidth = 0.8, linetype = "42"),
      species = list(annotate("point", x = 0.065, y = yi, size = 5.4, color = "white"),
                     annotate("point", x = 0.065, y = yi, size = 3.6, color = keys$color[i]),
                     annotate("point", x = 0.065, y = yi, size = 3.6, shape = 1,
                              stroke = 0.4, color = alpha(COLORS$title, 0.45))),
      context = annotate("point", x = 0.065, y = yi, size = 2.4,
                         color = alpha(COLORS$aluminum, 0.9)),
      edge   = list(annotate("segment", x = 0.01, xend = 0.12, y = yi, yend = yi,
                             color = MAP_COAST, linewidth = 2.2),
                    annotate("segment", x = 0.01, xend = 0.12, y = yi, yend = yi,
                             color = EDGE_COLOR, linewidth = 1.1)),
      nodata = annotate("rect", xmin = 0.025, xmax = 0.105, ymin = yi - 0.05,
                        ymax = yi + 0.05, fill = NODATA_FILL, color = NA),
      frame  = annotate("rect", xmin = 0.025, xmax = 0.105, ymin = yi - 0.045,
                        ymax = yi + 0.045, fill = NA, color = SITE_COLOR, linewidth = 1.0),
      site   = list(annotate("point", x = 0.065, y = yi, size = 6.2, color = "white"),
                    annotate("point", x = 0.065, y = yi, size = 4.2, color = SITE_COLOR),
                    annotate("point", x = 0.065, y = yi, size = 4.2, shape = 1,
                             stroke = 0.4, color = alpha(COLORS$title, 0.45))),
      stop("Unknown key type '", keys$type[i], "'", call. = FALSE))
    p <- p + annotate("text", x = 0.17, y = yi, label = keys$label[i], hjust = 0,
                      vjust = 0.5, size = pt(TXT$legend_text), color = COLORS$subtitle,
                      lineheight = 1.0)
  }
  p
}

#' The sea ice colorbar on its own
#'
#' For the hand-laid map slides. ggplot2 draws a colorbar five keys long, so
#' the key size is a fifth of `length_in`.
#'
#' @param length_in Length of the bar in inches.
#' @param horizontal Lay the bar horizontally.
#' @return A grob from [cowplot::get_legend()].
#' @export
conc_colorbar <- function(length_in = 2.1, horizontal = FALSE) {
  dummy <- data.frame(x = 1:2, y = 1, conc = c(15, 100))
  long  <- unit(length_in / 5, "in")
  thick <- unit(if (horizontal) 0.26 else 0.32, "in")
  p <- ggplot(dummy, aes(.data$x, .data$y, fill = .data$conc)) + geom_tile() +
    scale_fill_conc() +
    theme_void() +
    theme(legend.title = element_text(size = TXT$legend_title, face = "bold",
                                      color = COLORS$title, margin = margin(b = 8)),
          legend.text  = element_text(size = TXT$legend_text, color = COLORS$subtitle),
          legend.direction = if (horizontal) "horizontal" else "vertical",
          legend.title.position = "top",
          legend.key.height = if (horizontal) thick else long,
          legend.key.width  = if (horizontal) long else thick,
          legend.background = element_rect(fill = NA, color = NA))
  cowplot::get_legend(p)
}

#' The diverging colorbar for a change map on its own
#'
#' @param limits Range of the scale.
#' @param breaks,labels Tick positions and labels.
#' @param title Legend title.
#' @inheritParams conc_colorbar
#' @return A grob from [cowplot::get_legend()].
#' @export
change_colorbar <- function(limits, breaks, labels, title, length_in = 3,
                            horizontal = FALSE) {
  dummy <- data.frame(x = 1:2, y = 1, v = limits)
  long  <- unit(length_in / 5, "in")
  thick <- unit(if (horizontal) 0.26 else 0.32, "in")
  p <- ggplot(dummy, aes(.data$x, .data$y, fill = .data$v)) + geom_tile() +
    scale_fill_gradientn(colours = CHANGE_COLORS, limits = limits, breaks = breaks,
                         labels = labels, name = title) +
    theme_void() +
    theme(legend.title = element_text(size = TXT$legend_title, face = "bold",
                                      color = COLORS$title, margin = margin(b = 8)),
          legend.text  = element_text(size = TXT$legend_text, color = COLORS$subtitle,
                                      lineheight = 0.95),
          legend.direction = if (horizontal) "horizontal" else "vertical",
          legend.title.position = "top",
          legend.key.height = if (horizontal) thick else long,
          legend.key.width  = if (horizontal) long else thick,
          legend.background = element_rect(fill = NA, color = NA))
  cowplot::get_legend(p)
}

# A parchment slide with its title at the top left and its source line at the
# bottom left. Map panels and legends are drawn onto it.
slide_canvas <- function(title, caption) {
  cowplot::ggdraw() +
    theme(plot.background = element_rect(fill = MAP_PAPER, color = NA)) +
    cowplot::draw_label(title, x = 0.0375, y = 0.955, hjust = 0, vjust = 1,
                        size = TXT$title, fontface = "bold", color = COLORS$title) +
    cowplot::draw_label(caption, x = 0.0375, y = 0.035, hjust = 0, vjust = 0,
                        size = TXT$caption, color = CAPTION_COLOR)
}
