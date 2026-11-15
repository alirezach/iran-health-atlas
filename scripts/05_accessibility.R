# Stage 05: accessibility -----------------------------------------------------
# Origin: population-weighted centroid of every populated 1 km cell.
# Destination: nearest deduplicated hospital. Mode: car (free flow).
# Engine: dodgr (config routing$engine); r5r follows the same contract.

accessibility_stage <- function(road_graph_file, population_grid, hospitals, cfg) {
  rc <- cfg$routing
  graph <- as.data.frame(arrow::read_parquet(road_graph_file))
  origins <- population_grid[, list(cell_id, lon, lat)]
  dests <- data.table::data.table(
    hospital_id = hospitals$hospital_id, lon = hospitals$lon, lat = hospitals$lat
  )
  res <- route_nearest(
    rc$engine,
    graph = graph, origins = origins, hospitals = dests,
    chunk_size = rc$hospital_chunk_size,
    connector_speed_kmh = rc$connector_speed_kmh,
    r5r_data_path = cfg$paths$raw
  )
  snap <- attr(res, "hospital_snap")
  rm(graph)
  gc()
  tt <- population_grid[, list(cell_id, lon, lat, population, province_id, province, county_id, county)][
    res, on = "cell_id"
  ]
  tt[, category_id := classify_travel_time(travel_time_min, cfg$thresholds_min)]
  data.table::setattr(tt, "hospital_snap", snap)
  tt
}

write_travel_times <- function(tt, cfg) {
  out <- cfg$paths$processed
  cols <- c(
    "cell_id", "population", "travel_time_min", "distance_km", "nearest_hospital",
    "category_id", "province_id", "province", "county_id", "county", "origin_snap_m", "lon", "lat"
  )
  df <- as.data.frame(tt)[, cols]
  pq <- write_geoparquet(
    sf::st_as_sf(df, coords = c("lon", "lat"), crs = 4326, remove = FALSE),
    file.path(out, "travel_times.parquet")
  )
  csv <- file.path(out, "travel_times.csv.gz")
  data.table::fwrite(df, csv)
  c(pq, csv)
}

if (sys.nframe() == 0L) {
  targets::tar_make(names = tidyselect::any_of(c("travel_times", "travel_times_file")))
}
