# A grid with a square block of land cells, and polygons that match it exactly.
aligned_case <- function() {
  r <- terra::rast(nrows = 40, ncols = 40, xmin = -500000, xmax = 500000,
                   ymin = -500000, ymax = 500000, crs = "EPSG:3412")
  m <- matrix(0, 40, 40)
  m[15:25, 15:25] <- 2540
  m[14, 15:25] <- 2530
  m[26, 15:25] <- 2530
  terra::values(r) <- as.vector(t(m))
  names(r) <- "raw"
  land <- frazil:::raster_polygons(terra::ifel(r == 2540 | r == 2530, 1, NA))
  shelves <- sf::st_sfc(crs = sf::st_crs(3412))
  list(raw = r, land = list(land = land, shelves = shelves))
}

test_that("a correctly placed coastline passes the alignment check", {
  cs <- aligned_case()
  expect_message(res <- check_alignment(cs$raw, cs$land), "Alignment")
  expect_equal(unname(res[["land"]]), 1)
  expect_equal(unname(res[["ocean"]]), 1)
})

test_that("a coastline shifted by two cells fails the alignment check", {
  cs <- aligned_case()
  shifted <- cs$land$land + c(50000, 0)
  sf::st_crs(shifted) <- sf::st_crs(3412)
  land <- list(land = shifted, shelves = cs$land$shelves)
  expect_error(suppressMessages(check_alignment(cs$raw, land)), "do not line up")
})

test_that("a coastline shifted by one cell fails the alignment check", {
  cs <- aligned_case()
  shifted <- cs$land$land + c(0, 25000)
  sf::st_crs(shifted) <- sf::st_crs(3412)
  land <- list(land = shifted, shelves = cs$land$shelves)
  expect_error(suppressMessages(check_alignment(cs$raw, land)), "do not line up")
})

test_that("the thresholds can be relaxed", {
  cs <- aligned_case()
  shifted <- cs$land$land + c(25000, 0)
  sf::st_crs(shifted) <- sf::st_crs(3412)
  land <- list(land = shifted, shelves = cs$land$shelves)
  expect_error(suppressMessages(check_alignment(cs$raw, land)))
  res <- suppressMessages(check_alignment(cs$raw, land, min_land = 0.5, min_ocean = 0.5))
  expect_lt(res[["land"]], 1)
})
