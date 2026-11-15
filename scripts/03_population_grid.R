# Stage 03: 1 km population grid ----------------------------------------------
# The analytical unit is the WorldPop 30 arc-second (~1 km) cell, EPSG:4326.
# Each populated cell gets a population-weighted centroid computed from the
# 100 m WorldPop raster (falls back to the cell centre), and province/county
# codes from geoBoundaries. exactextractr provides independent zonal totals
# for reconciliation in the QA report.

build_grid_stage <- function(worldpop_1km, worldpop_100m, adm1, adm2, cfg) {
  fine <- if (endsWith(worldpop_100m, ".tif")) worldpop_100m else NULL
  g <- build_population_grid(worldpop_1km, fine, min_population = cfg$population$min_population)
  g <- assign_admin(g, worldpop_1km, adm1, "province")
  assign_admin(g, worldpop_1km, adm2, "county")
}

write_population_grid <- function(grid, cfg) {
  x <- sf::st_as_sf(as.data.frame(grid), coords = c("lon", "lat"), crs = 4326, remove = FALSE)
  write_geoparquet(x, file.path(cfg$paths$processed, "population_grid.parquet"))
}

if (sys.nframe() == 0L) {
  targets::tar_make(names = tidyselect::any_of(c("population_grid", "population_grid_file", "zonal")))
}
