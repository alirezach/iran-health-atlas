pt <- function(lon, lat) sf::st_point(c(lon, lat))

test_that("graph_components groups transitive neighbours", {
  nb <- list(c(1L, 2L), c(1L, 2L, 3L), c(2L, 3L), 4L)
  expect_equal(graph_components(nb), c(1L, 1L, 1L, 2L))
})

test_that("layers are standardised with stable OSM ids", {
  pts <- sf::st_sf(
    osm_id = "42", name = "Sina", amenity = "hospital", `name:fa` = "سینا",
    geometry = sf::st_sfc(pt(51.4, 35.7), crs = 4326), check.names = FALSE
  )
  out <- standardise_hospital_layer(pts, "point")
  expect_equal(out$osm_id, "node/42")
  expect_equal(out$name_fa, "سینا")
  expect_true(all(hospital_fields %in% names(out)))
  expect_true(is.na(out$beds))
})

test_that("nearby duplicate nodes merge and distant hospitals stay separate", {
  pts <- sf::st_sf(
    osm_id = c("1", "2", "3"),
    name = c("Imam Khomeini Hospital", "Imam Khomeini Hospital", "Shahid Beheshti Hospital"),
    amenity = "hospital",
    geometry = sf::st_sfc(pt(51.4000, 35.7), pt(51.4005, 35.7), pt(52.0, 36.0), crs = 4326)
  )
  cand <- prepare_hospital_candidates(points = pts)
  h <- deduplicate_hospitals(cand$points, cand$polygons, dist_m = 150)
  expect_equal(nrow(h), 2L)
  expect_setequal(h$n_merged, c(2L, 1L))
  expect_true(all(c("hospital_id", "lon", "lat", "merged_osm_ids") %in% names(h)))
  expect_false(anyDuplicated(h$hospital_id) > 0)
})

test_that("a node inside a hospital footprint is absorbed by the footprint", {
  sq <- sf::st_polygon(list(rbind(
    c(52.999, 29.999), c(53.001, 29.999), c(53.001, 30.001), c(52.999, 30.001), c(52.999, 29.999)
  )))
  polys <- sf::st_sf(
    osm_id = NA_character_, osm_way_id = "99", name = NA_character_, amenity = "hospital",
    geometry = sf::st_sfc(sq, crs = 4326)
  )
  pts <- sf::st_sf(
    osm_id = "5", name = "Shahid Rajaei", amenity = "hospital",
    geometry = sf::st_sfc(pt(53, 30), crs = 4326)
  )
  cand <- prepare_hospital_candidates(pts, polys)
  h <- deduplicate_hospitals(cand$points, cand$polygons)
  expect_equal(nrow(h), 1L)
  expect_equal(h$hospital_id, "way/99")
  expect_equal(h$osm_type, "way")
  expect_equal(h$name, "Shahid Rajaei")
})

test_that("hospitals outside the study area are dropped", {
  sq <- sf::st_polygon(list(rbind(c(50, 30), c(51, 30), c(51, 31), c(50, 31), c(50, 30))))
  adm <- sf::st_sf(id = "P1", name = "Test", geometry = sf::st_sfc(sq, crs = 4326))
  h <- sf::st_sf(hospital_id = c("in", "out"), geometry = sf::st_sfc(pt(50.5, 30.5), pt(55, 35), crs = 4326))
  out <- assign_admin_points(h, adm, "province", drop_outside = TRUE)
  expect_equal(out$hospital_id, "in")
  expect_equal(out$province, "Test")
})
