#' Render the whole figure set
#'
#' Runs every figure function in turn for one day, so a slide set is rebuilt
#' with one call: the daily extent curves, the monthly stripes, the circumpolar
#' map, the Peninsula map, the sector extent and the sector trends, the Weddell
#' map, the colony map, the two change maps, the talking numbers and, when
#' asked, the animation.
#'
#' @param out_dir Folder to write into.
#' @param date The day, or `NULL` for the latest grid in the cache.
#' @param animation Render the animation too (about 15 minutes).
#' @param change_maps Render the two change maps, which need every monthly
#'   grid and the daily grids of both periods.
#' @param colonies Render the colony map, which needs the `mapppdr` package.
#' @param sector The sector for the sector-extent figure, the highlight of the
#'   trend figure and the change maps.
#' @param paths The list from [frazil_paths()].
#' @return A named list of the figure objects, invisibly.
#' @export
render_figure_set <- function(out_dir = frazil_paths()$output, date = NULL, animation = FALSE,
                              change_maps = TRUE, colonies = TRUE, sector = "Weddell",
                              paths = frazil_paths()) {
  if (is.null(out_dir)) stop("render_figure_set() needs an out_dir.", call. = FALSE)
  figs <- list()
  figs$extent_daily   <- fig_extent_daily(date, out_dir, cache = paths$cache)
  figs$extent_anomaly <- fig_extent_anomaly(date, out_dir, cache = paths$cache)
  figs$sector_extent  <- fig_sector_extent(sector, date, out_dir = out_dir, cache = paths$cache)
  figs$sector_trends  <- fig_sector_trends(highlight = sector, out_dir = out_dir,
                                           cache = paths$cache)
  figs$map_circumpolar <- fig_map_circumpolar(date, out_dir = out_dir, paths = paths)
  figs$map_peninsula   <- fig_map_peninsula(date, out_dir = out_dir, paths = paths)
  figs$map_weddell     <- fig_map_weddell(date, out_dir = out_dir, paths = paths)
  if (colonies) {
    figs$map_colonies <- fig_map_colonies(date = date, out_dir = out_dir, paths = paths)
  }
  if (change_maps) {
    figs$map_cover_change  <- fig_map_cover_change(sector = sector, out_dir = out_dir,
                                                   paths = paths)
    figs$map_season_change <- fig_map_season_change(sector = sector, out_dir = out_dir,
                                                    paths = paths)
  }
  figs$talking_numbers <- talking_numbers(date, file = file.path(out_dir, "talking_numbers.txt"),
                                          paths = paths)
  if (animation) {
    figs$animation <- fig_animation(end_date = date, out_dir = out_dir, paths = paths)
  }
  invisible(figs)
}
