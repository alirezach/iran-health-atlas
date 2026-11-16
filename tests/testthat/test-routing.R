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


province_fixture <- function() {
  grid <- data.table::data.table(
    cell_id = 1:3, lon = c(50.0, 50.2, 50.2), lat = c(30.0, 30.0, 30.1),
    population = c(100, 200, 300), province_id = c("west", "east", "east"),
    province = c("West", "East", "East"), county_id = c("W1", "E1", "E1"), county = c("W", "E", "E")
  )
  hospitals <- data.frame(hospital_id = "border-hospital", lon = 50.1, lat = 30.0, province_id = "east")
  cfg <- list(routing = list(engine = "dodgr", hospital_chunk_size = 1L, connector_speed_kmh = 15),
              thresholds_min = c(15, 30, 60, 120), paths = list(raw = tempdir(), processed = tempdir()))
  list(grid = grid, hospitals = hospitals, cfg = cfg, graph = build_road_graph(road_lines(), speeds))
}

province_test_bundles <- function(f) {
  lapply(c("west", "east"), function(id) {
    list(snapshot_id = "test-snapshot", province_id = id,
         rows = route_province(f$graph, f$grid, f$hospitals, f$cfg, id))
  })
}

test_that("province plan is complete and picks the smallest province for the pilot", {
  f <- province_fixture()
  plan <- province_plan(f$grid)
  expect_equal(plan$province_id, c("west", "east"))
  expect_equal(plan$n_cells, c(1L, 2L))
  expect_true(all(grepl("^[a-f0-9]{32}$", plan$key)))
  bad <- data.table::copy(f$grid)
  bad$province_id[1] <- NA_character_
  expect_error(province_plan(bad), "province_id")
  bad <- data.table::rbindlist(list(f$grid, f$grid[1]))
  expect_error(province_plan(bad), "Duplicate")
})

test_that("provincial routes equal national routes and preserve cross-border hospitals", {
  f <- province_fixture()
  parts <- province_test_bundles(f)
  combined <- combine_province_bundles(parts, f$grid, "test-snapshot")
  national <- route_nearest("dodgr", graph = f$graph,
                            origins = f$grid[, list(cell_id, lon, lat)], hospitals = f$hospitals,
                            chunk_size = 1L, connector_speed_kmh = 15)
  expect_equal(combined$cell_id, national$cell_id)
  expect_equal(combined$travel_time_min, national$travel_time_min, tolerance = 1e-6)
  expect_equal(combined$distance_km, national$distance_km, tolerance = 1e-6)
  expect_equal(combined$nearest_hospital, national$nearest_hospital)
  expect_equal(combined$nearest_hospital[1], "border-hospital")
  expect_true(is.na(combined$travel_time_min[3]))
  expect_equal(sum(combined$population), sum(f$grid$population))
  expect_equal(attr(combined, "hospital_snap"), attr(national, "hospital_snap"))
})

test_that("national merge rejects partial, duplicate and stale provincial results", {
  f <- province_fixture()
  parts <- province_test_bundles(f)
  expect_error(combine_province_bundles(parts[1], f$grid, "test-snapshot"), "every province")
  expect_error(combine_province_bundles(c(parts, parts[1]), f$grid, "test-snapshot"), "every province")
  stale <- parts
  stale[[1]]$snapshot_id <- "other-inputs"
  expect_error(combine_province_bundles(stale, f$grid, "test-snapshot"), "Stale")
  missing <- parts
  missing[[2]]$rows <- missing[[2]]$rows[1]
  expect_error(combine_province_bundles(missing, f$grid, "test-snapshot"), "coverage")
})

test_that("completed provincial files survive a round trip and tampering is rejected", {
  f <- province_fixture()
  f$cfg$paths$processed <- tempfile("province-output-")
  on.exit(unlink(f$cfg$paths$processed, recursive = TRUE))
  m <- list(snapshot_id = "test-snapshot", provinces = province_plan(f$grid))
  rows <- route_province(f$graph, f$grid, f$hospitals, f$cfg, "west")
  paths <- write_province_bundle(rows, f$grid, "west", m, f$cfg)
  expect_true(all(file.exists(paths)))
  b <- read_province_bundle(dirname(paths[1]), f$grid, "west", m$snapshot_id)
  expect_equal(b$rows$cell_id, rows$cell_id)
  expect_equal(b$rows$travel_time_min, rows$travel_time_min)
  expect_error(write_province_bundle(rows, f$grid, "west", m, f$cfg), "overwrite")
  b$rows$population[1] <- b$rows$population[1] + 1
  expect_error(validate_province_bundle(b, f$grid, "west", m$snapshot_id), "population mismatch")
  cat("damaged", file = paths[2])
  expect_error(read_province_bundle(dirname(paths[1]), f$grid, "west", m$snapshot_id), "checksum")
})
