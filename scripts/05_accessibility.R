# Stage 05: independently checkpointed provincial accessibility.
# All provinces retain the full national road graph and hospital register.

accessibility_province_stage <- function(province_id, snapshot_files) {
  m <- read_routing_snapshot(snapshot_files)
  cfg <- readRDS(file.path(m$directory, "config.rds"))
  grid <- readRDS(file.path(m$directory, "grid.rds"))
  out <- province_result_dir(m, province_id, cfg)
  if (dir.exists(out)) {
    read_province_bundle(out, grid, province_id, m$snapshot_id)
    message(sprintf("province %s: reusing verified checkpoint", province_id))
    return(file.path(out, c("result.rds", "travel_times.parquet", "completion.json")))
  }
  if (identical(Sys.getenv("ATLAS_REQUIRE_PROVINCES"), "true")) {
    stop(sprintf("Missing completed province %s; national publication is blocked.", province_id))
  }
  hospitals <- readRDS(file.path(m$directory, "hospitals.rds"))
  graph <- as.data.frame(arrow::read_parquet(file.path(m$directory, "graph.parquet")))
  rows <- route_province(graph, grid, hospitals, cfg, province_id)
  rm(graph)
  gc()
  write_province_bundle(rows, grid, province_id, m, cfg)
}

combine_province_results <- function(province_files, population_grid, snapshot_files) {
  m <- read_routing_snapshot(snapshot_files)
  cfg <- readRDS(file.path(m$directory, "config.rds"))
  result_files <- province_files[basename(province_files) == "result.rds"]
  if (length(result_files) != nrow(m$provinces) || anyDuplicated(result_files)) {
    stop("Missing or duplicate provincial files; refusing national publication.")
  }
  bundles <- lapply(result_files, function(path) {
    b <- readRDS(path)
    read_province_bundle(dirname(path), population_grid, b$province_id, m$snapshot_id)
  })
  combine_province_bundles(bundles, population_grid, m$snapshot_id)
}

# Backward-compatible local entry point; targets uses explicit province branches.
accessibility_stage <- function(road_graph_file, population_grid, hospitals, cfg) {
  snapshot <- prepare_routing_snapshot(road_graph_file, population_grid, hospitals, cfg)
  ids <- province_plan(population_grid)$province_id
  outputs <- unlist(lapply(ids, accessibility_province_stage, snapshot_files = snapshot), use.names = FALSE)
  combine_province_results(outputs, population_grid, snapshot)
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
  for (f in list.files("R", pattern = "[.]R$", full.names = TRUE)) source(f)
  args <- commandArgs(trailingOnly = TRUE)
  action <- if (length(args)) args[1] else "local"
  if (identical(action, "prepare")) {
    targets::tar_make(
      names = c(routing_snapshot, population_grid_file, hospitals_file, zonal, source_metadata),
      reporter = "verbose"
    )
    snapshot <- targets::tar_read(routing_snapshot)
    m <- read_routing_snapshot(snapshot)
    mode <- Sys.getenv("ATLAS_MODE", "pilot")
    if (!mode %in% c("pilot", "full")) stop("ATLAS_MODE must be pilot or full.")
    selected <- m$provinces
    if (mode == "pilot") selected <- selected[1, , drop = FALSE]
    matrix <- jsonlite::toJSON(list(include = selected), dataframe = "rows", auto_unbox = TRUE)
    output <- Sys.getenv("GITHUB_OUTPUT")
    if (nzchar(output)) {
      cat(paste0("matrix=", matrix, "\nsnapshot=", m$snapshot_id, "\nmode=", mode, "\n"),
          file = output, append = TRUE)
    }
    message(sprintf("Prepared %s run: %d of %d provinces; snapshot %s",
                    mode, nrow(selected), nrow(m$provinces), m$snapshot_id))
    print(selected)
  } else if (identical(action, "province")) {
    id <- Sys.getenv("ATLAS_PROVINCE_ID")
    snapshot_id <- Sys.getenv("ATLAS_SNAPSHOT")
    if (!nzchar(id) || !grepl("^[a-f0-9]{32}$", snapshot_id)) stop("Missing province/snapshot environment.")
    manifest <- file.path("_atlas_inputs", snapshot_id, "manifest.rds")
    paths <- accessibility_province_stage(id, manifest)
    message(paste("Verified outputs:", paste(paths, collapse = ", ")))
  } else if (identical(action, "finalize")) {
    Sys.setenv(ATLAS_REQUIRE_PROVINCES = "true", ATLAS_FROZEN_INPUTS = "true")
    snapshot <- targets::tar_read(routing_snapshot)
    m <- read_routing_snapshot(snapshot)
    cfg <- readRDS(file.path(m$directory, "config.rds"))
    grid <- readRDS(file.path(m$directory, "grid.rds"))
    paths <- unlist(lapply(m$provinces$province_id, function(id) {
      file.path(province_result_dir(m, id, cfg), c("result.rds", "travel_times.parquet", "completion.json"))
    }), use.names = FALSE)
    # Verify complete national coverage BEFORE running any publication target.
    combine_province_results(paths, grid, snapshot)
    targets::tar_make(reporter = "verbose")
  } else if (identical(action, "local")) {
    targets::tar_make(names = tidyselect::any_of(c("travel_times", "travel_times_file")))
  } else stop("Usage: Rscript scripts/05_accessibility.R [prepare|province|finalize|local]")
}
