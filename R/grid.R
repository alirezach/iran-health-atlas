# Population grid -------------------------------------------------------------

#' Population-weighted centroids of coarse cells from a finer raster
#'
#' Sums population, population * x and population * y over each coarse cell
#' (aggregation by the integer resolution ratio, ~10 for WorldPop 100 m to
#' 1 km) and divides. Returns a two-layer SpatRaster (cx, cy) aligned to the
#' template.
#' @noRd
weighted_centroid_raster <- function(fine, template) {
  fine <- as_spatraster(fine)
  template <- as_spatraster(template)
  fine <- terra::crop(fine, template, snap = "out")
  fact <- round(terra::res(template) / terra::res(fine))
  wx <- fine * terra::init(fine, "x")
  wy <- fine * terra::init(fine, "y")
  s <- terra::aggregate(c(fine, wx, wy), fact = fact, fun = "sum", na.rm = TRUE)
  if (!terra::compareGeom(s, template, stopOnError = FALSE)) {
    s <- terra::resample(s, template, method = "near")
  }
  out <- c(s[[2]] / s[[1]], s[[3]] / s[[1]])
  names(out) <- c("cx", "cy")
  out
}

#' Build the national 1 km population grid
#'
#' One row per populated WorldPop 1 km cell. `cell_id` is the cell index in
#' the WorldPop raster, so outputs can be mapped back to the grid exactly.
#' `lon`/`lat` are population-weighted centroids when the 100 m raster is
#' available, otherwise cell centres (flagged in `centroid_method`).
#' @noRd
build_population_grid <- function(pop_1km, pop_fine = NULL, min_population = 0) {
  r <- as_spatraster(pop_1km)
  v <- terra::values(r, mat = FALSE)
  keep <- which(!is.na(v) & v > min_population)
  xy <- terra::xyFromCell(r, keep)
  rc <- terra::rowColFromCell(r, keep)
  dt <- data.table::data.table(
    cell_id = as.integer(keep),
    row = as.integer(rc[, 1]),
    col = as.integer(rc[, 2]),
    cell_lon = xy[, 1],
    cell_lat = xy[, 2],
    population = v[keep]
  )
  dt[, `:=`(lon = cell_lon, lat = cell_lat, centroid_method = "cell_centre")]
  if (!is.null(pop_fine)) {
    cxy <- weighted_centroid_raster(pop_fine, r)
    m <- terra::values(cxy, mat = TRUE)[keep, , drop = FALSE]
    half <- terra::res(r) / 2 + 1e-9
    ok <- is.finite(m[, 1]) & is.finite(m[, 2]) &
      abs(m[, 1] - dt$cell_lon) <= half[1] & abs(m[, 2] - dt$cell_lat) <= half[2]
    dt[ok, `:=`(lon = m[ok, 1], lat = m[ok, 2], centroid_method = "population_weighted")]
  }
  dt
}
