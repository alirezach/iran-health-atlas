# Stage 08: validation and QA report ------------------------------------------
# Checks CRS consistency, GeoParquet metadata, geometry validity, grid
# integrity, duplicate hospitals, network weights, population reconciliation
# (grid vs raster vs exactextractr) and impossible travel times. Writes
# data-processed/data-quality-report.html and qa_checks.csv. Any `fail`
# stops the pipeline (qa$stop_on_fail) before the website is updated.

validation_stage <- function(hospitals, population_grid, travel_times, adm1, adm2, zonal,
                             worldpop_1km, road_graph_file, parquet_files, osm_version, cfg) {
  q <- cfg$qa
  geo_names <- c("population_grid.parquet", "hospitals.parquet", "travel_times.parquet", "province_metrics.parquet")
  geo_files <- parquet_files[basename(parquet_files) %in% geo_names]
  snap <- attr(travel_times, "hospital_snap")
  far <- if (is.null(snap)) 0L else sum(snap$hospital_snap_m > q$max_hospital_snap_m)

  checks <- rbind(
    check_crs(list(
      hospitals = hospitals, provinces = adm1, counties = adm2,
      worldpop_1km = terra::rast(worldpop_1km)
    )),
    check_geoparquet(geo_files),
    check_geometry(adm1, "provinces"),
    check_geometry(adm2, "counties"),
    check_geometry(hospitals, "hospitals"),
    check_grid(population_grid, worldpop_1km),
    check_duplicate_hospitals(hospitals, q$duplicate_distance_m),
    qa_row(
      sprintf("Hospitals: more than %g m from the road network", q$max_hospital_snap_m),
      if (far == 0) "pass" else "warn", far, "Distance from hospital to its nearest drivable vertex"
    ),
    check_network(road_graph_file),
    check_population(population_grid, zonal, worldpop_1km, q$population_tolerance),
    check_travel_times(travel_times, hospitals, q$max_travel_time_min, q$max_plausible_speed_kmh)
  )

  tt <- data.table::as.data.table(travel_times)
  recon <- merge(
    population_grid[, list(population_grid = sum(population)), by = "province_id"],
    data.table::as.data.table(zonal)[, list(province_id = id, province = name, population_zonal)],
    by = "province_id"
  )
  recon[, diff_pct := round(100 * (population_grid - population_zonal) / population_zonal, 2)]
  reach <- tt[!is.na(travel_time_min)]
  probs <- c(0.1, 0.25, 0.5, 0.75, 0.9, 0.95, 0.99)
  pct <- data.frame(
    percentile = paste0("p", round(100 * probs)),
    travel_time_min = round(weighted_quantile(reach$travel_time_min, reach$population, probs), 1)
  )
  tables <- list(
    "Population reconciliation by province" = recon[, list(province, population_grid, population_zonal, diff_pct)],
    "Population-weighted drive-time percentiles" = pct
  )
  if (!is.null(snap) && far > 0) {
    named <- data.table::as.data.table(sf::st_drop_geometry(hospitals))[, list(hospital_id, name, province)]
    hd <- merge(snap[hospital_snap_m > q$max_hospital_snap_m], named, by = "hospital_id")
    tables[["Hospitals far from the drivable network"]] <- hd[order(-hospital_snap_m)][seq_len(min(.N, 50))]
  }

  report <- file.path(cfg$paths$processed, "data-quality-report.html")
  render_qa_report(checks, tables, report, meta = list(
    generated = format(Sys.time(), "%Y-%m-%d %H:%M UTC", tz = "UTC"),
    osm_md5 = as.character(osm_version),
    hospitals = nrow(hospitals),
    grid_cells = nrow(population_grid)
  ))
  checks_csv <- file.path(cfg$paths$processed, "qa_checks.csv")
  data.table::fwrite(checks, checks_csv)
  fails <- checks$check[checks$status == "fail"]
  if (isTRUE(q$stop_on_fail) && length(fails)) {
    stop("QA failed: ", paste(fails, collapse = "; "), ". See ", report)
  }
  c(report, checks_csv)
}

if (sys.nframe() == 0L) {
  targets::tar_make(names = tidyselect::any_of("qa_report"))
}
