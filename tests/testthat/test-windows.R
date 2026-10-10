test_that("the Peninsula window is where the sites are", {
  w <- peninsula_window()
  expect_s3_class(w, "frazil_window")
  expect_equal(diff(w$xlim), 2 * 820e3)
  expect_equal(diff(w$ylim), 2 * 520e3)
  expect_equal(w$terrain_res, 125)
  expect_true(w$rock_outlines)
  sites <- sites_sf(bri_sites, w$crs)
  xy <- sf::st_coordinates(sites)
  expect_true(all(xy[, 1] > w$xlim[1] & xy[, 1] < w$xlim[2]))
  expect_true(all(xy[, 2] > w$ylim[1] & xy[, 2] < w$ylim[2]))
})

test_that("the Weddell window holds its longitude-latitude box", {
  w <- weddell_window()
  expect_equal(w$sector, "Weddell")
  expect_equal(w$label_axes, "E--N")
  corners <- sf::st_as_sf(data.frame(lon = c(-62, 22, -62, 22), lat = c(-81, -81, -55, -55)),
                          coords = c("lon", "lat"), crs = 4326) |>
    sf::st_transform(w$crs) |> sf::st_coordinates()
  expect_true(all(corners[, 1] >= w$xlim[1] - 1 & corners[, 1] <= w$xlim[2] + 1))
  expect_true(all(corners[, 2] >= w$ylim[1] - 1 & corners[, 2] <= w$ylim[2] + 1))
  # About 5250 by 3200 km, as documented.
  expect_equal(diff(w$xlim) / 1000, 5250, tolerance = 0.05)
  expect_equal(diff(w$ylim) / 1000, 3200, tolerance = 0.05)
})

test_that("overrides reach the window", {
  w <- weddell_window(terrain_res = 1000, z_factor = 4)
  expect_equal(w$terrain_res, 1000)
  expect_equal(w$z_factor, 4)
  w2 <- peninsula_window(half_size = c(100e3, 50e3))
  expect_equal(diff(w2$xlim), 200e3)
})

test_that("terrain cache names match the ones already on disk", {
  expect_equal(frazil:::terrain_cache_name("peninsula", 125, 3), "seaice_peninsula_terrain_125m")
  expect_equal(frazil:::terrain_cache_name("weddell", 500, 5), "seaice_weddell_terrain_500m_z5")
  expect_equal(frazil:::terrain_cache_name("antarctica", 1500, 12),
               "seaice_antarctica_terrain_1500m_z12")
})

test_that("a window frame transforms to another projection as a closed polygon", {
  poly <- frazil:::window_polygon(peninsula_window(), sf::st_crs(3412))
  expect_equal(as.character(sf::st_geometry_type(poly)), "POLYGON")
  expect_equal(as.character(sf::st_crs(poly, parameters = TRUE)$epsg), "3412")
})

test_that("polar windows are centered where asked", {
  w <- polar_window(center = c(-40, -70), half_size = c(100e3, 100e3), name = "test")
  ctr <- sf::st_coordinates(sf::st_transform(
    sf::st_sfc(sf::st_point(c(-40, -70)), crs = 4326), w$crs))
  expect_equal(mean(w$xlim), ctr[1])
  expect_equal(mean(w$ylim), ctr[2])
  expect_output(print(w), "test")
})
