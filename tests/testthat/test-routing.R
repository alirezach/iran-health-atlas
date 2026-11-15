road_lines <- function() {
  sf::st_sf(
    osm_id = c("1", "2", "3"),
    highway = c("primary", "primary", "tertiary"),
    oneway = c(NA, NA, "yes"),
    junction = NA_character_,
    geometry = sf::st_sfc(
      sf::st_linestring(rbind(c(50.0, 30.0), c(50.1, 30.0))),
      sf::st_linestring(rbind(c(50.1, 30.0), c(50.2, 30.0))),
      sf::st_linestring(rbind(c(50.2, 30.0), c(50.2, 30.1))),
      crs = 4326
    )
  )
}
speeds <- c(primary = 60, tertiary = 30)

test_that("oneway_code reads OSM conventions", {
  expect_equal(oneway_code(c("yes", "-1", "no", NA, NA), c(NA, NA, NA, "roundabout", NA), c("primary", "primary", "primary", "primary", "motorway")), c(1L, -1L, 0L, 1L, 1L))
})

test_that("graph is directed, time-weighted and honours one-way tags", {
  g <- build_road_graph(road_lines(), speeds)
  expect_equal(nrow(g), 5L)
  expect_true(all(g$d > 0 & g$d_weighted > 0))
  expect_equal(g$d_weighted, g$d / (g$speed_kmh / 3.6))
  rev <- reverse_graph(g)
  expect_equal(rev$from_id, g$to_id)
  expect_equal(rev$to_lon, g$from_lon)
})

test_that("routing finds the nearest hospital with the right time and distance", {
  g <- build_road_graph(road_lines(), speeds)
  origins <- data.table::data.table(cell_id = 1L, lon = 50.0, lat = 30.0)
  hosp <- data.table::data.table(hospital_id = c("A", "B"), lon = c(50.2, 50.1), lat = c(30.1, 30.0))
  res <- route_nearest("dodgr", graph = g, origins = origins, hospitals = hosp, chunk_size = 1L)
  leg_m <- haversine_m(50.0, 30.0, 50.1, 30.0)
  expect_equal(res$nearest_hospital, "B")
  expect_equal(res$travel_time_min, leg_m / (60 / 3.6) / 60, tolerance = 1e-6)
  expect_equal(res$distance_km, leg_m / 1000, tolerance = 1e-6)
  expect_false(is.null(attr(res, "hospital_snap")))
})

test_that("one-way streets cannot be driven against the flow", {
  g <- build_road_graph(road_lines(), speeds)
  origins <- data.table::data.table(cell_id = 7L, lon = 50.2, lat = 30.1)
  hosp <- data.table::data.table(hospital_id = "H", lon = 50.0, lat = 30.0)
  res <- route_nearest("dodgr", graph = g, origins = origins, hospitals = hosp)
  expect_true(is.na(res$travel_time_min))
  expect_true(is.na(res$distance_km))
})

test_that("off-network connectors add time at the connector speed", {
  g <- build_road_graph(road_lines(), speeds)
  origins <- data.table::data.table(cell_id = 1L, lon = 50.0, lat = 29.99)
  hosp <- data.table::data.table(hospital_id = "B", lon = 50.1, lat = 30.0)
  res <- route_nearest("dodgr", graph = g, origins = origins, hospitals = hosp, connector_speed_kmh = 15)
  conn_m <- haversine_m(50.0, 29.99, 50.0, 30.0)
  leg_m <- haversine_m(50.0, 30.0, 50.1, 30.0)
  expect_equal(res$origin_snap_m, conn_m, tolerance = 1e-6)
  expect_equal(res$travel_time_min, (leg_m / (60 / 3.6) + conn_m / (15 / 3.6)) / 60, tolerance = 1e-6)
})
