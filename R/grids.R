# --- Concentration grids ------------------------------------------------------
# The grids have 25 km cells, 316 columns by 332 rows, in NSIDC Sea Ice Polar
# Stereographic South (EPSG:3412). Values 0-1000 are concentration in tenths of
# a percent; 2530 marks coast cells, 2540 land and 2550 missing data.

#' The daily concentration grids in the cache
#'
#' @inheritParams read_daily_extent
#' @return A data frame with `date` and `path`, sorted by date.
#' @export
daily_conc_files <- function(cache = frazil_paths()$cache) {
  f <- list.files(file.path(cache, "daily/geotiff"),
                  pattern = "^S_\\d{8}_concentration_v4\\.0\\.tif$",
                  recursive = TRUE, full.names = TRUE)
  d <- as.Date(sub("^S_(\\d{8})_.*$", "\\1", basename(f)), "%Y%m%d")
  data.frame(date = d, path = f, stringsAsFactors = FALSE)[order(d), , drop = FALSE]
}

#' The monthly concentration grids in the cache
#'
#' @inheritParams read_daily_extent
#' @return A data frame with `year`, `month`, `date` and `path`, sorted by date.
#' @export
monthly_conc_files <- function(cache = frazil_paths()$cache) {
  f <- list.files(file.path(cache, "monthly/geotiff"),
                  pattern = "^S_\\d{6}_concentration_v4\\.0\\.tif$",
                  recursive = TRUE, full.names = TRUE)
  year  <- as.integer(substr(basename(f), 3, 6))
  month <- as.integer(substr(basename(f), 7, 8))
  d <- as.Date(sprintf("%04d-%02d-01", year, month))
  data.frame(year = year, month = month, date = d, path = f,
             stringsAsFactors = FALSE)[order(d), , drop = FALSE]
}

#' The daily grid for a date
#'
#' @param date The date, or `NULL` for the latest grid in the cache.
#' @inheritParams read_daily_extent
#' @return A list with `date`, `path` and `raw`, the grid from
#'   [read_conc_raw()].
#' @export
daily_grid <- function(date = NULL, cache = frazil_paths()$cache) {
  files <- daily_conc_files(cache)
  if (!nrow(files)) {
    stop("No daily concentration grids in ", cache, ". Run nsidc_download() first.",
         call. = FALSE)
  }
  if (is.null(date)) {
    f <- tail(files, 1)
  } else {
    date <- as.Date(date)
    f <- files[files$date == date, ]
    if (!nrow(f)) stop("No daily grid for ", format(date), " in the cache.", call. = FALSE)
  }
  list(date = f$date, path = f$path, raw = read_conc_raw(f$path))
}

#' Read a concentration grid without its color table
#'
#' The NSIDC GeoTIFFs carry a color table, which would make ggplot2 draw them
#' as categories. This reads the values alone, so the plotting scales apply.
#'
#' @param path Path to an NSIDC concentration GeoTIFF.
#' @return A one-layer SpatRaster named `raw`.
#' @export
read_conc_raw <- function(path) {
  r <- terra::rast(path)
  out <- terra::rast(r)
  terra::values(out) <- terra::values(r, mat = FALSE)
  names(out) <- "raw"
  out
}

#' Concentration in percent for cells at or above 15%
#'
#' 15% is the threshold that defines extent. Open water, lower concentrations
#' and all flags become `NA`.
#'
#' @param raw A grid from [read_conc_raw()].
#' @return A SpatRaster named `conc`, in percent.
#' @export
conc_percent <- function(raw) {
  x <- terra::classify(raw, rbind(c(-Inf, 150, NA), c(1000.5, Inf, NA)),
                       right = FALSE) / 10
  names(x) <- "conc"
  x
}

# Cells where the grid reports no concentration: coast (2530), land (2540) and
# missing (2550). The maps draw them in their own gray under the terrain, so the
# terrain covers real land and the gray shows only where the grid has no ice
# value over water: the coast cells, and the grid's land mask where it still
# holds ice shelves that have since broken up (Larsen A and B). Cells north of
# `north_of` are left out; on the circumpolar map they are South America and the
# sub-Antarctic islands, drawn as land.
no_data_cells <- function(raw, north_of = NULL) {
  x <- terra::ifel(raw >= 2530 & raw <= 2550, 1, NA)
  if (!is.null(north_of)) x <- terra::ifel(grid_latitude(raw) > north_of, NA, x)
  names(x) <- "nodata"
  x
}

# Grid land outside Antarctica (north of `north_of`), as polygons.
other_land_cells <- function(raw, north_of = -60) {
  x <- terra::ifel((raw == 2530 | raw == 2540) & grid_latitude(raw) > north_of, 1, NA)
  raster_polygons(x)
}

# Latitude and longitude of each cell, as rasters on the grid.
grid_lonlat <- function(raw) {
  xy <- terra::xyFromCell(raw, seq_len(terra::ncell(raw)))
  sf::sf_project(from = sf::st_crs(terra::crs(raw)), to = sf::st_crs(4326), pts = xy)
}

grid_latitude <- function(raw) {
  lat <- terra::rast(raw)
  terra::values(lat) <- grid_lonlat(raw)[, 2]
  lat
}

grid_longitude <- function(raw) {
  lon <- terra::rast(raw)
  terra::values(lon) <- grid_lonlat(raw)[, 1]
  lon
}

raster_polygons <- function(x) {
  terra::as.polygons(x) |> sf::st_as_sf() |> sf::st_geometry()
}

#' Which grid cells lie in an NSIDC sector
#'
#' @param raw A grid from [read_conc_raw()].
#' @param sector A sector key from [nsidc_sectors].
#' @return A logical vector with one value per cell, in cell order.
#' @export
in_sector <- function(raw, sector = "Weddell") {
  r <- sector_row(sector)
  lon <- terra::values(grid_longitude(raw), mat = FALSE)
  if (r$west < r$east) lon >= r$west & lon <= r$east else lon >= r$west | lon <= r$east
}
