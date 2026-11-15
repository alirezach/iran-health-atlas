# Automated validation --------------------------------------------------------
# Each check returns rows of (check, status, value, detail); status is one of
# pass / warn / fail. `fail` stops the pipeline when qa$stop_on_fail is TRUE.

qa_row <- function(check, status, value = NA, detail = "") {
  data.frame(check = check, status = status, value = as.character(value), detail = detail, stringsAsFactors = FALSE)
}

fmt_big <- function(x) format(round(x), big.mark = ",", scientific = FALSE, trim = TRUE)

check_crs <- function(layers) {
  rows <- lapply(names(layers), function(nm) {
    x <- layers[[nm]]
    code <- if (inherits(x, "SpatRaster")) terra::crs(x, describe = TRUE)$code else sf::st_crs(x)$epsg
    code <- as.character(code %||% NA)
    qa_row(paste0("CRS: ", nm), if (identical(code, "4326")) "pass" else "fail", paste0("EPSG:", code), "Analytical layers must be EPSG:4326")
  })
  do.call(rbind, rows)
}

check_geoparquet <- function(files) {
  rows <- lapply(files, function(f) {
    md <- arrow::read_parquet(f, as_data_frame = FALSE)$metadata$geo
    ok <- !is.null(md) && {
      g <- jsonlite::fromJSON(md, simplifyVector = FALSE)
      col <- g$columns[[g$primary_column]]
      identical(col$encoding, "WKB") && is.null(col$crs)
    }
    qa_row(
      paste0("GeoParquet metadata: ", basename(f)), if (isTRUE(ok)) "pass" else "fail",
      if (isTRUE(ok)) "1.0.0, WKB, OGC:CRS84" else "missing or invalid",
      "Geometry stored as WKB in longitude/latitude (EPSG:4326)"
    )
  })
  do.call(rbind, rows)
}

check_geometry <- function(x, name) {
  valid <- with_s2(FALSE, sf::st_is_valid(x))
  invalid <- sum(!valid, na.rm = TRUE) + sum(is.na(valid))
  empty <- sum(sf::st_is_empty(x))
  rbind(
    qa_row(paste0("Geometry validity: ", name), if (invalid == 0) "pass" else "fail", invalid, "Invalid geometries (GEOS)"),
    qa_row(paste0("Empty geometries: ", name), if (empty == 0) "pass" else "fail", empty, "Features without geometry")
  )
}

check_grid <- function(grid, template) {
  r <- as_spatraster(template)
  e <- as.vector(terra::ext(r))
  half <- terra::res(r) / 2 + 1e-9
  dup <- anyDuplicated(grid$cell_id) > 0
  outside <- sum(grid$lon < e[["xmin"]] | grid$lon > e[["xmax"]] | grid$lat < e[["ymin"]] | grid$lat > e[["ymax"]])
  drift <- sum(abs(grid$lon - grid$cell_lon) > half[1] | abs(grid$lat - grid$cell_lat) > half[2])
  neg <- sum(grid$population < 0)
  weighted <- mean(grid$centroid_method == "population_weighted")
  rbind(
    qa_row("Grid: unique cell ids", if (!dup) "pass" else "fail", if (dup) "duplicates" else "unique", "One row per 1 km cell"),
    qa_row("Grid: centroids inside study raster", if (outside == 0) "pass" else "fail", outside, "Centroids within the WorldPop extent"),
    qa_row("Grid: centroids inside their cell", if (drift == 0) "pass" else "fail", drift, "Weighted centroid must fall inside its 1 km cell"),
    qa_row("Grid: non-negative population", if (neg == 0) "pass" else "fail", neg, "Cells with negative population"),
    qa_row("Grid: population-weighted centroids", if (weighted > 0.5) "pass" else "warn", sprintf("%.1f%%", 100 * weighted), "Cells placed with 100 m weights (others use cell centres)")
  )
}

check_duplicate_hospitals <- function(h, dist_m = 100) {
  dup_id <- anyDuplicated(h$hospital_id) > 0
  nb <- sf::st_is_within_distance(h, h, dist = dist_m)
  pairs <- (sum(lengths(nb)) - nrow(h)) / 2
  rbind(
    qa_row("Hospitals: count", if (nrow(h) > 0) "pass" else "fail", nrow(h), "Deduplicated OSM hospitals inside Iran"),
    qa_row("Hospitals: unique ids", if (!dup_id) "pass" else "fail", if (dup_id) "duplicates" else "unique", "hospital_id = OSM type/id"),
    qa_row(sprintf("Hospitals: pairs closer than %g m", dist_m), if (pairs == 0) "pass" else "warn", pairs, "Should be zero after deduplication")
  )
}

check_network <- function(graph_file) {
  g <- arrow::read_parquet(graph_file, col_select = c("d", "d_weighted"))
  bad <- sum(!is.finite(g$d) | g$d <= 0 | !is.finite(g$d_weighted) | g$d_weighted <= 0)
  rbind(
    qa_row("Network: directed edges", if (nrow(g) > 0) "pass" else "fail", fmt_big(nrow(g)), "Drivable edges after component filtering"),
    qa_row("Network: invalid edge weights", if (bad == 0) "pass" else "fail", bad, "Edges with missing, zero or negative length or time")
  )
}

check_population <- function(grid, zonal, template, tolerance = 0.02) {
  r <- as_spatraster(template)
  raster_total <- terra::global(r, "sum", na.rm = TRUE)[1, 1]
  grid_total <- sum(grid$population)
  zonal_total <- sum(zonal$population_zonal, na.rm = TRUE)
  d1 <- abs(grid_total - raster_total) / raster_total
  d2 <- abs(zonal_total - raster_total) / raster_total
  rbind(
    qa_row("Population: grid vs WorldPop total", if (d1 <= tolerance) "pass" else "warn", sprintf("%.2f%%", 100 * d1), sprintf("Grid %s vs raster %s", fmt_big(grid_total), fmt_big(raster_total))),
    qa_row("Population: exactextractr provinces vs WorldPop", if (d2 <= tolerance) "pass" else "warn", sprintf("%.2f%%", 100 * d2), sprintf("Zonal sum %s", fmt_big(zonal_total)))
  )
}

check_travel_times <- function(tt, hospitals, max_minutes = 1440, max_speed_kmh = 130) {
  t <- tt$travel_time_min
  reach <- !is.na(t)
  n <- max(1, sum(reach))
  neg <- sum(t < 0, na.rm = TRUE)
  over <- sum(t > max_minutes, na.rm = TRUE)
  hi <- match(tt$nearest_hospital, hospitals$hospital_id)
  straight_km <- haversine_m(tt$lon, tt$lat, hospitals$lon[hi], hospitals$lat[hi]) / 1000
  pos <- reach & t > 0
  too_fast <- sum(straight_km[pos] / (t[pos] / 60) > max_speed_kmh, na.rm = TRUE)
  short <- sum(tt$distance_km[reach] < straight_km[reach] * 0.99 - 0.05, na.rm = TRUE)
  zero_far <- sum(reach & t == 0 & straight_km > 0.05, na.rm = TRUE)
  missing_h <- sum(reach & is.na(hi))
  unreach <- sum(tt$population[!reach]) / sum(tt$population)
  grade <- function(k) if (k == 0) "pass" else if (k / n < 0.001) "warn" else "fail"
  rbind(
    qa_row("Travel time: negative values", if (neg == 0) "pass" else "fail", neg, "Cells with negative drive time"),
    qa_row(sprintf("Travel time: above %g min", max_minutes), if (over == 0) "pass" else "fail", over, "Cells with implausibly long drive time"),
    qa_row(sprintf("Travel time: straight-line speed above %g km/h", max_speed_kmh), grade(too_fast), too_fast, "Crow-flies distance / drive time faster than any modelled road"),
    qa_row("Distance: route shorter than straight line", grade(short), short, "Network distance below great-circle distance"),
    qa_row("Travel time: zero time to a distant hospital", grade(zero_far), zero_far, "0 min while hospital is more than 50 m away"),
    qa_row("Travel time: nearest hospital assigned", if (missing_h == 0) "pass" else "fail", missing_h, "Reachable cells must reference a hospital in the register"),
    qa_row("Coverage: population without road route", if (unreach < 0.01) "pass" else "warn", sprintf("%.2f%%", 100 * unreach), "Share of people with no road route to any hospital")
  )
}

format_cell <- function(v) {
  if (is.numeric(v)) format(round(v, 3), big.mark = ",", scientific = FALSE, trim = TRUE) else as.character(v)
}

#' Self-contained HTML QA report
#' @noRd
render_qa_report <- function(checks, tables = list(), path, meta = list()) {
  h <- htmltools::tags
  counts <- table(factor(checks$status, levels = c("pass", "warn", "fail")))
  css <- paste(
    "body{font-family:system-ui,-apple-system,'Segoe UI',sans-serif;margin:0;background:#F4F6FA;color:#1B2233}",
    "main{max-width:1100px;margin:0 auto;padding:2rem 1rem}",
    "h1{font-size:2rem;margin:0 0 .25rem}h2{margin-top:2.25rem}",
    ".meta{color:#5B6478}",
    "table{border-collapse:collapse;width:100%;background:#fff;border-radius:8px;overflow:hidden;font-size:15px}",
    "th,td{padding:.55rem .7rem;text-align:left;border-bottom:1px solid #DDE2EC;vertical-align:top}",
    "th{background:#13204A;color:#F4E3B5;font-weight:600}",
    ".badge{display:inline-block;padding:.1rem .55rem;border-radius:999px;font-weight:600;font-size:13px;text-transform:uppercase}",
    ".pass{background:#DCE8F7;color:#1F3A93}.warn{background:#F6E7C1;color:#6B4A0F}.fail{background:#1F2452;color:#fff}",
    ".summary{display:flex;gap:.5rem;margin:1rem 0}",
    ".scroll{overflow-x:auto}"
  )
  check_rows <- lapply(seq_len(nrow(checks)), function(i) {
    h$tr(
      h$td(checks$check[i]),
      h$td(h$span(class = paste("badge", checks$status[i]), checks$status[i])),
      h$td(checks$value[i]),
      h$td(checks$detail[i])
    )
  })
  table_html <- function(df) {
    df <- as.data.frame(df)
    h$div(class = "scroll", h$table(
      h$thead(h$tr(lapply(names(df), h$th))),
      h$tbody(lapply(seq_len(nrow(df)), function(i) {
        h$tr(lapply(df[i, , drop = FALSE], function(v) h$td(format_cell(v))))
      }))
    ))
  }
  meta_txt <- paste(paste0(names(meta), ": ", unlist(meta)), collapse = " \u00b7 ")
  page <- h$html(
    lang = "en",
    h$head(
      h$meta(charset = "utf-8"),
      h$meta(name = "viewport", content = "width=device-width, initial-scale=1"),
      h$title("Data quality report \u00b7 Iran Health Accessibility Atlas"),
      h$style(htmltools::HTML(css))
    ),
    h$body(h$main(
      h$h1("Data quality report"),
      h$p(class = "meta", "Iran Health Accessibility Atlas \u00b7 ", meta_txt),
      h$div(
        class = "summary",
        h$span(class = "badge pass", sprintf("%d pass", counts[["pass"]])),
        h$span(class = "badge warn", sprintf("%d warn", counts[["warn"]])),
        h$span(class = "badge fail", sprintf("%d fail", counts[["fail"]]))
      ),
      h$h2("Automated checks"),
      h$div(class = "scroll", h$table(
        h$thead(h$tr(h$th("Check"), h$th("Status"), h$th("Value"), h$th("Detail"))),
        h$tbody(check_rows)
      )),
      lapply(names(tables), function(nm) htmltools::tagList(h$h2(nm), table_html(tables[[nm]])))
    ))
  )
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  writeLines(c("<!DOCTYPE html>", as.character(page)), path, useBytes = TRUE)
  path
}
