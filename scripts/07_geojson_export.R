# Stage 07: web-ready exports -------------------------------------------------
# Everything the dashboard needs, written to dashboard/data/:
#   travel_time.geojson       dissolved accessibility categories per province
#   province_metrics.geojson  simplified provinces with indicators
#   hospitals.geojson         hospital points
#   travel_time.png, population.png + overlays.json  EPSG:3857 raster overlays
#   site-summary.json         headline numbers, palettes, categories
#   downloads/                Parquet / CSV copies + manifest for the Downloads page
# Size control: polygons simplified in metres, coordinates rounded, near-empty
# cells hidden, rasters shipped as PNG, the cell table shipped as gzip CSV.

export_web_stage <- function(travel_times, indicators, hospitals, adm1, worldpop_1km,
                             osm_version, data_files, qa_report, cfg) {
  web <- cfg$paths$web
  dl <- file.path(web, "downloads")
  unlink(web, recursive = TRUE)
  dir.create(dl, recursive = TRUE, showWarnings = FALSE)
  w <- cfg$web
  thr <- cfg$thresholds_min
  ref <- reference_threshold(thr)
  cats <- web_categories(cfg$categories)

  tt <- data.table::copy(data.table::as.data.table(travel_times))
  tt[, province_idx := match(province_id, adm1$id)]
  tt[, category_code := data.table::fifelse(is.na(category_id), max(cats$code), category_id)]

  polys <- category_polygons(
    worldpop_1km, tt, adm1, cats,
    factor = w$raster_factor, tolerance_m = w$simplify_tolerance_m,
    min_population = w$min_population_display
  )
  hosp_cols <- c("hospital_id", "name", "name_fa", "name_en", "province", "county", "n_merged")
  files <- c(
    write_geojson(polys, file.path(web, "travel_time.geojson"), w$coordinate_precision),
    write_geojson(
      province_web_layer(adm1, indicators$province, ref, w$simplify_tolerance_m),
      file.path(web, "province_metrics.geojson"), w$coordinate_precision
    ),
    write_geojson(hospitals[, hosp_cols], file.path(web, "hospitals.geojson"), 5)
  )

  shown <- tt[population >= w$min_population_display]
  minutes <- data.table::fifelse(is.na(shown$travel_time_min), 999, shown$travel_time_min)
  r_tt <- grid_to_raster(worldpop_1km, shown$cell_id, minutes)
  r_pop <- grid_to_raster(worldpop_1km, shown$cell_id, shown$population)
  if (w$raster_factor > 1) {
    r_tt <- terra::aggregate(r_tt, fact = w$raster_factor, fun = "mean", na.rm = TRUE)
    r_pop <- terra::aggregate(r_pop, fact = w$raster_factor, fun = "sum", na.rm = TRUE)
  }
  overlays <- list(
    travel = raster_overlay_png(r_tt, file.path(web, "travel_time.png"), atlas_palette(5), limits = c(0, 180)),
    population = raster_overlay_png(r_pop, file.path(web, "population.png"), population_palette(), limits = c(0, 4), fun = log10)
  )
  ov_file <- file.path(web, "overlays.json")
  jsonlite::write_json(overlays, ov_file, auto_unbox = TRUE, digits = NA)

  nat <- indicators$national
  ribbon <- merge(cats, as.data.frame(indicators$categories_national), by.x = "code", by.y = "category_id", all.x = TRUE)
  ribbon$share[is.na(ribbon$share)] <- 0
  ribbon$population[is.na(ribbon$population)] <- 0
  ribbon <- ribbon[order(ribbon$code), c("code", "label", "color", "share", "population")]
  site_summary <- list(
    title = cfg$project$name,
    version = cfg$project$version,
    generated_at = format(Sys.time(), "%Y-%m-%d %H:%M UTC", tz = "UTC"),
    osm_md5 = osm_version,
    worldpop_year = cfg$sources$worldpop_year,
    thresholds = thr,
    reference_threshold = ref,
    categories = cats,
    coverage_legend = coverage_legend(),
    ribbon = ribbon,
    palette_travel = atlas_palette(5),
    palette_population = population_palette(),
    provinces = sort(indicators$province$province),
    national = stats::setNames(as.list(nat$value), nat$metric)
  )
  sum_file <- file.path(web, "site-summary.json")
  jsonlite::write_json(site_summary, sum_file, auto_unbox = TRUE, digits = NA, na = "null", pretty = TRUE)

  copied <- file.path(dl, basename(data_files))
  file.copy(data_files, copied, overwrite = TRUE)
  qa_copy <- file.path(web, basename(qa_report))
  file.copy(qa_report, qa_copy, overwrite = TRUE)
  man_file <- file.path(web, "downloads-manifest.csv")
  data.table::fwrite(build_download_manifest(web), man_file)

  c(files, file.path(web, c("travel_time.png", "population.png")), ov_file, sum_file, copied, qa_copy, man_file)
}

if (sys.nframe() == 0L) {
  targets::tar_make(names = tidyselect::any_of("web_files"))
}
