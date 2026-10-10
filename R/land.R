# --- Coastline, ice shelves, ice edge and sites -------------------------------

#' ADD land and ice-shelf polygons
#'
#' Reads the ADD v7.3 coastline and ice-shelf layers (EPSG:3031), optionally
#' cut to a window given as WKT in EPSG:3031 and simplified by `simplify_m`
#' meters, and transforms them to `crs`.
#'
#' @param crs The target coordinate reference system, as an sf `crs`.
#' @param window_wkt_3031 A WKT polygon in EPSG:3031 to read within, or
#'   `character(0)` for the whole layer.
#' @param simplify_m Simplification tolerance in meters; 0 keeps every vertex.
#' @param paths The list from [frazil_paths()].
#' @return A list of two `sfc` geometries, `land` and `shelves`.
#' @export
load_land <- function(crs, window_wkt_3031 = character(0), simplify_m = 0,
                      paths = frazil_paths()) {
  coast <- require_path(paths, "coastline", "The ADD coastline layer")
  shelf <- require_path(paths, "iceshelves", "The ADD ice-shelf layer")
  read_one <- function(path) {
    x <- sf::st_read(path, quiet = TRUE, wkt_filter = window_wkt_3031)
    x <- x[!sf::st_is_empty(x), ]
    x <- sf::st_make_valid(sf::st_zm(x))
    if (simplify_m > 0) {
      x <- sf::st_simplify(x, dTolerance = simplify_m, preserveTopology = TRUE)
      x <- x[!sf::st_is_empty(x), ]
    }
    sf::st_transform(sf::st_geometry(x), crs)
  }
  list(land = read_one(coast), shelves = read_one(shelf))
}

#' The 1981 to 2010 median ice edge for a calendar month
#'
#' Reads NSIDC's median-extent polyline for the month from its zip file in the
#' cache, in the grid's own CRS (EPSG:3412).
#'
#' @param month Month number, 1 to 12.
#' @inheritParams read_daily_extent
#' @return An `sfc` of lines.
#' @export
read_median_edge <- function(month, cache = frazil_paths()$cache) {
  z <- cache_file(cache, file.path(
    "monthly/shapefiles/shp_median",
    sprintf("median_extent_S_%02d_1981-2010_polyline_v4.0.zip", month)))
  sf::st_read(paste0("/vsizip/", z), quiet = TRUE) |> sf::st_geometry()
}

#' The BRI drone survey sites
#'
#' The four sites of the BRI Antarctic drone surveys, for marking on the maps.
#' `dx` and `dy` are label offsets in meters for the Peninsula map, which push
#' the labels out over open water.
#'
#' @format A data frame with `code`, `label`, `lon`, `lat`, `dx` and `dy`.
#' @export
bri_sites <- data.frame(
  code  = c("HOVG", "JOUG", "DEVI", "NEDU"),
  label = c("Hovgaard Island", "Jougla Point", "Devil Island", "D'Urville Island"),
  lon   = c(-64.12, -63.49185, -57.29105, -56.17106),
  lat   = c(-65.13, -64.82842, -63.79720, -62.99438),
  dx    = c(-260e3, -250e3, 250e3, 250e3),
  dy    = c(-60e3, 110e3, -70e3, 90e3),
  stringsAsFactors = FALSE
)

#' Sites as an sf object
#'
#' @param sites A data frame with `lon` and `lat` columns, such as [bri_sites].
#' @param crs The target CRS.
#' @return An sf object of points.
#' @export
sites_sf <- function(sites, crs) {
  sf::st_as_sf(sites, coords = c("lon", "lat"), crs = 4326, remove = FALSE) |>
    sf::st_transform(crs)
}

# A line of latitude, for reference circles on the polar maps.
parallel_line <- function(lat, crs) {
  sf::st_sfc(sf::st_linestring(cbind(seq(-180, 180, by = 0.5), lat)), crs = 4326) |>
    sf::st_transform(crs)
}

# A meridian between two latitudes, for the sector boundaries.
meridian_line <- function(lon, lat_from, lat_to, crs) {
  sf::st_sfc(sf::st_linestring(cbind(lon, seq(lat_from, lat_to, length.out = 100))),
             crs = 4326) |> sf::st_transform(crs)
}

# The two boundary meridians of a sector, from `lat_from` (one value, or one per
# boundary) to `lat_to`.
sector_meridians <- function(sector, lat_from, lat_to, crs) {
  r <- sector_row(sector)
  lat_from <- rep_len(lat_from, 2)
  c(meridian_line(r$west, lat_from[1], lat_to, crs),
    meridian_line(r$east, lat_from[2], lat_to, crs))
}

# The southernmost open-water latitude along a meridian: where the meridian
# meets the coast or the ice-shelf front, coming from the pole. Measured on the
# sea ice grid's own land mask, which counts ice shelves as land and has no
# slivers between polygons.
coast_latitude <- function(lon, raw, lat_range = c(-85, -50), step = 0.1) {
  lats <- seq(lat_range[1], lat_range[2], by = step)
  pts  <- sf::st_transform(sf::st_as_sf(data.frame(lon = lon, lat = lats),
                                        coords = c("lon", "lat"), crs = 4326),
                           sf::st_crs(terra::crs(raw)))
  v <- terra::extract(raw, terra::vect(pts))[[2]]
  ocean <- !is.na(v) & v <= 1000
  if (!any(ocean)) return(lat_range[2])
  min(lats[ocean])
}
