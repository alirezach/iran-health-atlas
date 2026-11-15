test_that("GeoParquet round-trips points with spec metadata", {
  x <- sf::st_sf(
    id = 1:3,
    name = c("a", "b", "c"),
    geometry = sf::st_sfc(lapply(1:3, function(i) sf::st_point(c(50 + i, 30 + i))), crs = 4326)
  )
  f <- tempfile(fileext = ".parquet")
  write_geoparquet(x, f)
  geo <- jsonlite::fromJSON(arrow::read_parquet(f, as_data_frame = FALSE)$metadata$geo)
  expect_equal(geo$version, "1.0.0")
  expect_equal(geo$primary_column, "geometry")
  expect_equal(geo$columns$geometry$encoding, "WKB")
  expect_equal(geo$columns$geometry$geometry_types, "Point")
  y <- read_geoparquet(f)
  expect_equal(unname(sf::st_coordinates(y)), unname(sf::st_coordinates(x)))
  expect_equal(y$name, x$name)
  expect_equal(nrow(check_geoparquet(f)), 1L)
  expect_equal(check_geoparquet(f)$status, "pass")
})

test_that("non-EPSG:4326 data is refused", {
  x <- sf::st_sf(id = 1, geometry = sf::st_sfc(sf::st_point(c(0, 0)), crs = 3857))
  expect_error(write_geoparquet(x, tempfile(fileext = ".parquet")), "EPSG:4326")
})
