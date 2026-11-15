make_rasters <- function() {
  coarse <- terra::rast(
    nrows = 1, ncols = 2, xmin = 0, xmax = 2, ymin = 0, ymax = 1,
    crs = "EPSG:4326", vals = c(10, 20)
  )
  fine <- terra::rast(nrows = 10, ncols = 20, xmin = 0, xmax = 2, ymin = 0, ymax = 1, crs = "EPSG:4326")
  v <- matrix(0, nrow = 10, ncol = 20)
  v[1, 1] <- 10        # all of cell 1's population sits in its top-left corner
  v[, 11:20] <- 0.2    # cell 2 is uniform
  terra::values(fine) <- as.vector(t(v))
  list(coarse = coarse, fine = fine)
}

test_that("grid keeps every populated cell and preserves population", {
  r <- make_rasters()
  g <- build_population_grid(r$coarse)
  expect_equal(nrow(g), 2L)
  expect_equal(sum(g$population), 30)
  expect_equal(g$cell_id, c(1L, 2L))
  expect_true(all(g$centroid_method == "cell_centre"))
  expect_equal(g$lon, c(0.5, 1.5))
})

test_that("population-weighted centroids follow where people live", {
  r <- make_rasters()
  g <- build_population_grid(r$coarse, r$fine)
  expect_true(all(g$centroid_method == "population_weighted"))
  expect_equal(g$lon[g$col == 1], 0.05, tolerance = 1e-9)
  expect_equal(g$lat[g$col == 1], 0.95, tolerance = 1e-9)
  expect_equal(g$lon[g$col == 2], 1.5, tolerance = 1e-9)
  expect_equal(g$lat[g$col == 2], 0.5, tolerance = 1e-9)
})

test_that("empty and low-population cells are dropped", {
  r <- make_rasters()
  terra::values(r$coarse) <- c(NA, 0.3)
  expect_equal(nrow(build_population_grid(r$coarse)), 1L)
  expect_equal(nrow(build_population_grid(r$coarse, min_population = 0.5)), 0L)
})

test_that("cells are assigned to admin units, with nearest-unit fallback", {
  r <- make_rasters()
  g <- build_population_grid(r$coarse)
  left <- sf::st_polygon(list(rbind(c(0, 0), c(1, 0), c(1, 1), c(0, 1), c(0, 0))))
  adm <- sf::st_sf(id = "A", name = "West", geometry = sf::st_sfc(left, crs = 4326))
  g <- assign_admin(g, r$coarse, adm, "province")
  expect_equal(g$province, c("West", "West"))
  expect_equal(g$province_id, c("A", "A"))
})
