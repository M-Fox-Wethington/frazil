#' Check that the sea ice grid and the coastline line up
#'
#' Before any map is drawn, the NSIDC land cells must sit on ADD land or ice
#' shelf, and the ocean cells next to the coast must sit off it. Only ocean
#' cells within `coastal_m` of a land or coast cell are tested, because those
#' are the ones a shifted or rescaled grid pushes onto land.
#'
#' Measured on 7 October 2026 with the Version 4 land mask: correctly
#' transformed, 99.2% of land cells and 92.6% of coastal ocean cells agree, and
#' the misses are small islands that 25 km cells cannot resolve. With the grid
#' read as EPSG:3031 the ocean share drops to 88.7%, and with the grid shifted
#' one cell east to 85.6%. The land mask is the same in every daily file, so the
#' shares do not vary by day.
#'
#' @param raw A grid from [read_conc_raw()].
#' @param land The list from [load_land()], already in the grid's CRS.
#' @param min_land Smallest acceptable share of grid land cells on ADD land or
#'   ice shelf.
#' @param min_ocean Smallest acceptable share of coastal ocean cells off it.
#' @param coastal_m Distance from a land or coast cell within which ocean cells
#'   are tested, in meters.
#' @return The two shares, invisibly, named `land` and `ocean`. The function
#'   stops when either is below its threshold.
#' @export
check_alignment <- function(raw, land, min_land = 0.97, min_ocean = 0.91, coastal_m = 60000) {
  continent <- c(land$land, land$shelves)
  v  <- terra::values(raw, mat = FALSE)
  xy <- terra::xyFromCell(raw, seq_along(v))
  on_land <- function(idx) {
    p <- sf::st_as_sf(data.frame(x = xy[idx, 1], y = xy[idx, 2]),
                      coords = c("x", "y"), crs = sf::st_crs(continent))
    lengths(sf::st_intersects(p, continent)) > 0
  }
  coastal <- terra::buffer(terra::ifel(raw == 2530 | raw == 2540, 1, NA), width = coastal_m)
  near    <- terra::values(coastal, mat = FALSE)
  land_idx  <- which(v == 2540)
  ocean_idx <- which(v <= 1000 & near == 1)
  if (!length(land_idx) || !length(ocean_idx)) {
    stop("The grid has no land cells or no coastal ocean cells to test.", call. = FALSE)
  }
  share_land  <- mean(on_land(land_idx))
  share_ocean <- mean(!on_land(ocean_idx))
  message(sprintf(paste0("Alignment: %.1f%% of %d grid land cells fall on ADD land or ",
                         "ice shelf; %.1f%% of %d coastal ocean cells fall off it."),
                  100 * share_land, length(land_idx), 100 * share_ocean, length(ocean_idx)))
  if (share_land < min_land || share_ocean < min_ocean) {
    stop("The sea ice grid and the coastline do not line up. Check the CRS.", call. = FALSE)
  }
  invisible(c(land = share_land, ocean = share_ocean))
}
