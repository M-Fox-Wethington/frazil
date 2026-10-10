#' Animation: a year of Antarctic sea ice
#'
#' One frame per daily grid over the last `days` days ending on `end_date`, in
#' the circumpolar map's style. Beside the map: the date, that day's extent,
#' and the period's extent curve drawn up to that day against the 1981-2010
#' median and its middle 80% of years. The yellow line is the usual ice edge
#' for each month, so it moves with the season.
#'
#' Frames are 1920 by 1080 pixels (13.333 by 7.5 inches at 144 dpi) and are
#' encoded with ffmpeg, which must be on the PATH, as H.264 in yuv420p at `fps`
#' frames per second, which PowerPoint plays. The last frame holds for
#' `hold_sec` seconds and is also written as a still slide through
#' [save_slide()]. Rendering takes about 10 to 15 minutes for a year of
#' frames.
#'
#' @param end_date The last day, or `NULL` for the latest grid in the cache.
#' @param days Length of the period in days.
#' @param fps Frames per second.
#' @param hold_sec Seconds the last frame holds.
#' @param out_dir Folder to write the MP4 and the still into. Required.
#' @param name File name stem for the MP4 and the still.
#' @param frame_dir Folder for the rendered frames, which stay on disk until
#'   the encode succeeds so a failed encode can be rerun without rendering
#'   again.
#' @param frame A [map_window()] outlined on each frame, or `NULL`.
#' @param test Render only the last frame, as `name_test_frame.png`, and skip
#'   the encode.
#' @param paths The list from [frazil_paths()].
#' @return The MP4 path, invisibly.
#' @export
fig_animation <- function(end_date = NULL, days = 366, fps = 12, hold_sec = 2,
                          out_dir = frazil_paths()$output, name = "sea_ice_12_months",
                          frame_dir = file.path(paths$terrain_cache, "frames"), frame = NULL,
                          test = FALSE, paths = frazil_paths()) {
  if (is.null(out_dir)) stop("fig_animation() needs an out_dir to write the video into.",
                             call. = FALSE)
  if (!test && !nzchar(Sys.which("ffmpeg"))) {
    stop("ffmpeg is not on the PATH; it is needed to encode the video.", call. = FALSE)
  }
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  frame_px  <- c(1920, 1080)   # set in pixels; 13.333 in at 144 dpi rounds to 1919
  frame_dpi <- 144
  mp4_path  <- file.path(out_dir, paste0(name, ".mp4"))

  files <- daily_conc_files(paths$cache)
  if (!is.null(end_date)) files <- files[files$date <= as.Date(end_date), ]
  files <- files[files$date > max(files$date) - days, ]
  message("Frames: ", nrow(files), " from ", format(min(files$date)), " to ",
          format(max(files$date)))

  base <- circumpolar_base(read_conc_raw(tail(files$path, 1)), frame = frame, paths = paths)
  base$land$land    <- sf::st_simplify(base$land$land, dTolerance = 5000,
                                       preserveTopology = TRUE)
  base$land$shelves <- sf::st_simplify(base$land$shelves, dTolerance = 5000,
                                       preserveTopology = TRUE)
  # The terrain as one prebuilt image at about the frame's own resolution,
  # built once and reused by every frame.
  terrain_layer <- rgb_annotation(base$terrain$rgb, max_cols = 1400)

  ext_all <- read_daily_extent(paths$cache)
  ext_win <- ext_all |> filter(date >= min(files$date), date <= max(files$date))
  clim <- read_climatology(paths$cache)
  band <- data.frame(date = seq(min(files$date), max(files$date), by = "day")) |>
    mutate(doy = pmin(as.integer(format(date, "%j")), 365)) |>
    left_join(clim, by = "doy")
  yr_label <- paste0(format(min(files$date), "%Y"), "\u2013", format(max(files$date), "%y"))
  lab_low  <- ext_win |> slice_min(extent, n = 1, with_ties = FALSE)

  chart_for <- function(day) {
    so_far <- ext_win |> filter(date <= day)
    ggplot() +
      geom_ribbon(data = band, aes(date, ymin = p10, ymax = p90), fill = MAP_COAST,
                  alpha = 0.14) +
      geom_line(data = band, aes(date, p50), color = MAP_COAST, linewidth = 0.7,
                linetype = "22") +
      geom_line(data = so_far, aes(date, extent), color = COLORS$orange, linewidth = 1.3) +
      geom_point(data = tail(so_far, 1), aes(date, extent), color = COLORS$orange,
                 size = 3) +
      annotate("text", x = min(band$date) + 4, y = 20.3, hjust = 0, vjust = 1,
               label = "1981\u20132010 median and middle 80% of years",
               size = pt(14), color = MAP_COAST) +
      annotate("text", x = lab_low$date, y = 0.7, hjust = 0.5, vjust = 0, label = yr_label,
               size = pt(15), fontface = "bold", color = COLORS$orange) +
      scale_x_date(date_breaks = "3 months", date_labels = "%b %Y", expand = c(0.01, 0)) +
      scale_y_continuous(limits = c(0, 21.5), breaks = c(0, 10, 20), expand = c(0, 0),
                         name = "Million km\u00b2") +
      labs(x = NULL) +
      theme_slide(ground = MAP_PAPER) +
      theme(axis.text  = element_text(size = 14, color = COLORS$subtitle),
            axis.title = element_text(size = 14, color = COLORS$subtitle),
            panel.grid.major = element_line(color = alpha(MAP_COAST, 0.15), linewidth = 0.4),
            plot.margin = margin(4, 10, 4, 4))
  }

  keys <- data.frame(type = c("edge", "nodata"),
                     label = c("Usual ice edge for\nthe month, 1981\u20132010",
                               "No ice data near\nthe coast"))
  colorbar <- conc_colorbar(length_in = 2.4, horizontal = TRUE)
  key_plot <- key_panel(keys, row_gap = 0.45)

  compose_frame <- function(i) {
    day <- files$date[i]
    raw <- read_conc_raw(files$path[i])
    p_map <- circumpolar_map(base, raw, as.integer(format(day, "%m")),
                             show_frame = !is.null(frame), terrain_layer = terrain_layer,
                             cache = paths$cache)
    ext_today <- ext_all$extent[ext_all$date == day]
    ext_text  <- if (length(ext_today)) sprintf("%.1f million km\u00b2", ext_today) else ""
    slide_canvas("Antarctic sea ice through the year",
                 paste0(SOURCE_NSIDC, ". Terrain: REMA v2.0. Coastline and rock: SCAR ADD.")) +
      cowplot::draw_plot(p_map, x = 0.0375, y = 0.085, width = 0.47, height = 0.79) +
      cowplot::draw_label(fmt_date(day), x = 0.56, y = 0.845, hjust = 0, vjust = 1, size = 28,
                          fontface = "bold", color = COLORS$title) +
      cowplot::draw_label(paste("Sea ice extent:", ext_text), x = 0.56, y = 0.775, hjust = 0,
                          vjust = 1, size = 20, color = COLORS$subtitle) +
      cowplot::draw_plot(chart_for(day), x = 0.555, y = 0.36, width = 0.42, height = 0.36) +
      cowplot::draw_plot(colorbar, x = 0.56, y = 0.12, width = 0.19, height = 0.18) +
      cowplot::draw_plot(key_plot, x = 0.77, y = 0.10, width = 0.22, height = 0.22)
  }

  unlink(frame_dir, recursive = TRUE)
  dir.create(frame_dir, recursive = TRUE)
  idx <- if (test) nrow(files) else seq_len(nrow(files))
  t0 <- Sys.time()
  for (k in seq_along(idx)) {
    i <- idx[k]
    png_i <- file.path(frame_dir, sprintf("frame_%04d.png", k))
    ragg::agg_png(png_i, width = frame_px[1], height = frame_px[2], units = "px",
                  res = frame_dpi, background = MAP_PAPER)
    print(compose_frame(i))
    invisible(grDevices::dev.off())
    if (k %% 25 == 0 || k == length(idx)) {
      message(sprintf("  %d / %d frames, %.1f s per frame", k, length(idx),
                      as.numeric(difftime(Sys.time(), t0, units = "secs")) / k))
    }
  }

  if (test) {
    test_png <- file.path(out_dir, paste0(name, "_test_frame.png"))
    file.copy(file.path(frame_dir, "frame_0001.png"), test_png, overwrite = TRUE)
    message("Test frame written to ", test_png)
    return(invisible(test_png))
  }

  # H.264 in yuv420p needs even dimensions; the scale step guarantees them.
  args <- c("-y", "-loglevel", "error", "-framerate", fps,
            "-i", shQuote(file.path(frame_dir, "frame_%04d.png")),
            "-vf", shQuote(sprintf("scale=trunc(iw/2)*2:trunc(ih/2)*2,tpad=stop_mode=clone:stop_duration=%d",
                                   hold_sec)),
            "-c:v", "libx264", "-pix_fmt", "yuv420p", "-crf", "18", "-movflags", "+faststart",
            shQuote(mp4_path))
  status <- system2("ffmpeg", args)
  if (status != 0) stop("ffmpeg failed with status ", status, call. = FALSE)
  message("Saved ", mp4_path, sprintf(" (%.1f MB)", file.size(mp4_path) / 1e6))

  save_slide(compose_frame(nrow(files)), paste0(name, "_last_frame"), out_dir, bg = MAP_PAPER)
  unlink(frame_dir, recursive = TRUE)
  invisible(mp4_path)
}
