# Web-ready exports -----------------------------------------------------------

#' Categories plus an explicit "no road access" class (JSON-safe)
#' @noRd
web_categories <- function(categories) {
  n <- nrow(categories)
  upper <- categories$upper
  upper[!is.finite(upper)] <- NA_real_
  data.frame(
    code = c(categories$category_id, n + 1L),
    label = c(categories$label, "No road access"),
    lower = c(categories$lower, 9999),
    upper = c(upper, NA_real_),
    color = c(categories$color, atlas_unreachable_color()),
    stringsAsFactors = FALSE
  )
}

#' Choropleth classes for provincial coverage (dark = worse)
#' @noRd
coverage_legend <- function() {
  data.frame(
    lower = c(0, 0.5, 0.7, 0.85, 0.95),
    label = c("< 50%", "50\u201370%", "70\u201385%", "85\u201395%", "\u2265 95%"),
    color = c("#1F2452", "#5A5A8C", "#B8734A", "#E3B456", "#F4E3B5"),
    stringsAsFactors = FALSE
  )
}

#' Write RFC 7946 GeoJSON with limited coordinate precision
#' @noRd
write_geojson <- function(x, path, precision = 5) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  if (file.exists(path)) unlink(path)
  sf::st_write(
    x, path, driver = "GeoJSON", quiet = TRUE,
    layer_options = c("RFC7946=YES", paste0("COORDINATE_PRECISION=", precision))
  )
  path
}

#' Simplify polygons in metres (EPSG:3857) and return to EPSG:4326
#' @noRd
simplify_for_web <- function(x, tolerance_m = 750) {
  x <- sf::st_transform(x, 3857)
  x <- sf::st_simplify(x, preserveTopology = TRUE, dTolerance = tolerance_m)
  x <- sf::st_transform(x, 4326)
  x <- with_s2(FALSE, sf::st_make_valid(x))
  x[!sf::st_is_empty(x), ]
}

#' Put per-cell values back on the WorldPop 1 km grid
#' @noRd
grid_to_raster <- function(template, cells, values) {
  r <- terra::rast(as_spatraster(template), nlyrs = 1)
  v <- rep(NA_real_, terra::ncell(r))
  v[cells] <- values
  terra::values(r) <- v
  r
}

#' Render a raster as a transparent PNG in EPSG:3857 for web map overlays
#'
#' Returns the file name and its lat/lon bounds ([[south, west], [north, east]]).
#' @noRd
raster_overlay_png <- function(r, path, colors, limits, fun = NULL) {
  p <- terra::project(r, "EPSG:3857", method = "near")
  v <- terra::values(p, mat = FALSE)
  if (!is.null(fun)) v <- suppressWarnings(fun(v))
  s <- (pmin(pmax(v, limits[1]), limits[2]) - limits[1]) / diff(limits)
  ramp <- grDevices::colorRampPalette(colors)(256)
  idx <- floor(s * 255) + 1
  ok <- !is.na(idx)
  idx[!ok] <- 1
  rgb <- grDevices::col2rgb(ramp[idx]) / 255
  nr <- terra::nrow(p)
  nc <- terra::ncol(p)
  arr <- array(0, dim = c(nr, nc, 4))
  for (k in 1:3) arr[, , k] <- matrix(rgb[k, ], nrow = nr, ncol = nc, byrow = TRUE)
  arr[, , 4] <- matrix(as.numeric(ok), nrow = nr, ncol = nc, byrow = TRUE)
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  png::writePNG(arr, path)
  e <- as.vector(terra::ext(p))
  corners <- sf::st_transform(sf::st_sfc(
    sf::st_point(c(e[["xmin"]], e[["ymin"]])),
    sf::st_point(c(e[["xmax"]], e[["ymax"]])),
    crs = 3857
  ), 4326)
  cc <- sf::st_coordinates(corners)
  list(
    file = basename(path),
    bounds = list(c(cc[1, 2], cc[1, 1]), c(cc[2, 2], cc[2, 1])),
    crs = "EPSG:3857"
  )
}

#' Dissolved accessibility-category polygons, one feature per province x class
#' @noRd
category_polygons <- function(template, tt, adm1, categories, factor = 1L,
                              tolerance_m = 750, min_population = 1) {
  d <- tt[population >= min_population & !is.na(province_idx)]
  r <- grid_to_raster(template, d$cell_id, d$province_idx * 10L + d$category_code)
  if (factor > 1) r <- terra::aggregate(r, fact = factor, fun = "modal", na.rm = TRUE)
  p <- sf::st_as_sf(terra::as.polygons(r, dissolve = TRUE))
  names(p)[1] <- "code"
  p$province_idx <- as.integer(p$code %/% 10)
  p$category_id <- as.integer(p$code %% 10)
  p <- simplify_for_web(p, tolerance_m)
  pops <- d[, list(population = sum(population), cells = .N), by = list(province_idx, category_id = category_code)]
  p <- merge(p, as.data.frame(pops), by = c("province_idx", "category_id"), all.x = TRUE)
  i <- match(p$category_id, categories$code)
  p$label <- categories$label[i]
  p$lower <- categories$lower[i]
  p$upper <- categories$upper[i]
  p$color <- categories$color[i]
  p$province_id <- adm1$id[p$province_idx]
  p$province <- adm1$name[p$province_idx]
  p$population <- round(p$population)
  p[, c("province_id", "province", "category_id", "label", "lower", "upper", "color", "population", "cells")]
}

#' Province polygons with metrics and a pre-computed fill colour
#' @noRd
province_web_layer <- function(adm1, metrics, ref, tolerance_m = 750) {
  leg <- coverage_legend()
  x <- merge(adm1[, "id"], as.data.frame(metrics), by.x = "id", by.y = "province_id")
  names(x)[names(x) == "id"] <- "province_id"
  x <- simplify_for_web(x, tolerance_m)
  share <- x[[paste0("share_within_", ref)]]
  x$fill <- leg$color[findInterval(share, leg$lower, rightmost.closed = TRUE)]
  x$fill[is.na(x$fill)] <- atlas_unreachable_color()
  num <- vapply(sf::st_drop_geometry(x), is.numeric, logical(1))
  for (col in names(num)[num]) x[[col]] <- round(x[[col]], 4)
  x
}

#' Files offered on the Downloads page, with licences
#' @noRd
download_catalogue <- function() {
  odbl <- "ODbL 1.0 (derived from OpenStreetMap)"
  ccby <- "CC BY 4.0 (WorldPop, geoBoundaries)"
  files <- data.frame(
    file = c(
      "province_metrics.csv", "county_metrics.csv", "national_metrics.csv",
      "province_travel_time_distribution.csv", "hospitals.csv", "travel_times.csv.gz",
      "travel_time.geojson", "province_metrics.geojson", "hospitals.geojson",
      "population_grid.parquet", "hospitals.parquet", "travel_times.parquet",
      "province_metrics.parquet", "county_metrics.parquet", "national_metrics.parquet",
      "province_travel_time_distribution.parquet", "metadata.csv",
      "data-quality-report.html", "qa_checks.csv"
    ),
    description = c(
      "Coverage and drive-time indicators per province",
      "Coverage and drive-time indicators per county",
      "National indicators, inequality measures and percentiles",
      "Population by 10-minute drive-time bin per province",
      "Deduplicated hospital register with OSM ids",
      "Drive time, distance and nearest hospital for every populated 1 km cell",
      "Accessibility-category polygons by province (web map layer)",
      "Province boundaries with indicators (web map layer)",
      "Hospital points (web map layer)",
      "1 km population grid with population-weighted centroids (GeoParquet)",
      "Hospital register (GeoParquet)",
      "Cell-level accessibility results (GeoParquet)",
      "Province indicators with boundaries (GeoParquet)",
      "County indicators",
      "National indicators",
      "Province drive-time distribution",
      "Source provenance: URLs, versions, checksums, licences",
      "Automated data-quality report",
      "Machine-readable QA check results"
    ),
    license = c(
      odbl, odbl, odbl, odbl, odbl, odbl, odbl, odbl, odbl,
      ccby, odbl, odbl, odbl, odbl, odbl, odbl, "CC BY 4.0", "CC BY 4.0", "CC BY 4.0"
    ),
    stringsAsFactors = FALSE
  )
  ext <- tools::file_ext(sub(".gz", "", files$file, fixed = TRUE))
  files$format <- toupper(ifelse(endsWith(files$file, ".csv.gz"), "csv.gz", ext))
  files
}

#' Locate catalogue files under the web directory and record sizes
#' @noRd
build_download_manifest <- function(web_dir) {
  cat_df <- download_catalogue()
  in_dl <- file.exists(file.path(web_dir, "downloads", cat_df$file))
  loc <- ifelse(in_dl, file.path("downloads", cat_df$file), cat_df$file)
  full <- file.path(web_dir, loc)
  exists <- file.exists(full)
  out <- cat_df[exists, ]
  out$href <- file.path("data", loc[exists])
  out$bytes <- file.size(full[exists])
  out
}
