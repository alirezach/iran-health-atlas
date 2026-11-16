# Accessibility routing -------------------------------------------------------
# Engine contract (dodgr today, r5r tomorrow):
#   input  origins   data.table(cell_id, lon, lat)
#          hospitals data.table(hospital_id, lon, lat)
#   output data.table(cell_id, nearest_hospital, travel_time_min, distance_km,
#          origin_snap_m) with attr "hospital_snap" = data.table(hospital_id,
#          hospital_snap_m). Unreachable cells have NA travel time.

#' Dispatch to a routing engine
#' @noRd
route_nearest <- function(engine = c("dodgr", "r5r"), ...) {
  engine <- match.arg(engine)
  switch(engine,
    dodgr = route_nearest_dodgr(...),
    r5r = route_nearest_r5r(...)
  )
}

#' Snap points to the nearest graph vertex
#' @noRd
snap_to_vertices <- function(lon, lat, verts) {
  idx <- dodgr::match_points_to_verts(verts, data.frame(x = lon, y = lat))
  list(
    vertex = as.character(verts$id[idx]),
    snap_m = haversine_m(lon, lat, verts$x[idx], verts$y[idx])
  )
}

#' Nearest hospital by car with dodgr
#'
#' Dijkstra runs from each hospital on the *reversed* graph, which yields the
#' origin-to-hospital time on the real (directed) network. Hospitals are
#' processed in chunks and a running minimum is kept, so memory stays at
#' chunk_size x n_origin_vertices. Time and distance are computed on two copies
#' of the same contracted graph that share the time weighting, so both refer
#' to the fastest route. Off-network legs (centroid to road, road to hospital)
#' are added at `connector_speed_kmh`.
#' @noRd
route_nearest_dodgr <- function(graph, origins, hospitals, chunk_size = 50L,
                                connector_speed_kmh = 15, ...) {
  dodgr::dodgr_cache_off()
  graph <- as.data.frame(graph)
  verts <- dodgr::dodgr_vertices(graph)
  o <- data.table::as.data.table(origins)[, list(cell_id, lon, lat)]
  h <- data.table::as.data.table(hospitals)[, list(hospital_id, lon, lat)]
  message(sprintf("routing: snapping %d origins to %d vertices", nrow(o), nrow(verts)))
  so <- snap_to_vertices(o$lon, o$lat, verts)
  o[, `:=`(vertex = so$vertex, origin_snap_m = so$snap_m)]
  sh <- snap_to_vertices(h$lon, h$lat, verts)
  h[, `:=`(vertex = sh$vertex, hospital_snap_m = sh$snap_m)]
  conn <- connector_speed_kmh / 3.6

  rev_g <- as.data.frame(reverse_graph(graph))
  rm(graph)
  message("routing: contracting reversed national graph")
  rev_c <- dodgr::dodgr_contract_graph(rev_g, verts = unique(c(o$vertex, h$vertex)))
  if (!is.data.frame(rev_c) && is.list(rev_c)) rev_c <- rev_c$graph
  rm(rev_g)
  message(sprintf("routing: contraction complete (%d edges)", nrow(rev_c)))
  rev_t <- rev_c
  rev_t$d <- rev_t$d_weighted

  uo <- unique(o$vertex)
  best_t <- rep(Inf, length(uo))
  best_d <- rep(NA_real_, length(uo))
  best_h <- rep(NA_integer_, length(uo))
  h_conn_s <- h$hospital_snap_m / conn
  idx <- seq_len(nrow(h))
  chunks <- split(idx, ceiling(idx / chunk_size))
  for (k in seq_along(chunks)) {
    ch <- chunks[[k]]
    message(sprintf("routing: starting hospital chunk %d/%d (%d origin vertices)", k, length(chunks), length(uo)))
    tt <- dodgr::dodgr_dists(rev_t, from = h$vertex[ch], to = uo)
    dd <- dodgr::dodgr_dists(rev_c, from = h$vertex[ch], to = uo)
    tt <- tt + h_conn_s[ch]
    tt[is.na(tt)] <- Inf
    tt <- t(tt)
    dd <- t(dd)
    arg <- max.col(-tt, ties.method = "first")
    mn <- tt[cbind(seq_along(arg), arg)]
    better <- which(mn < best_t)
    if (length(better)) {
      best_t[better] <- mn[better]
      best_h[better] <- ch[arg[better]]
      best_d[better] <- dd[cbind(better, arg[better])] + h$hospital_snap_m[ch][arg[better]]
    }
    message(sprintf("routing: hospital chunk %d/%d", k, length(chunks)))
  }

  res <- data.table::data.table(vertex = uo, net_s = best_t, net_d = best_d, h_idx = best_h)
  o <- res[o, on = "vertex"]
  o[, travel_time_min := (net_s + origin_snap_m / conn) / 60]
  o[!is.finite(net_s), travel_time_min := NA_real_]
  o[, distance_km := (net_d + origin_snap_m) / 1000]
  o[is.na(travel_time_min), distance_km := NA_real_]
  o[, nearest_hospital := h$hospital_id[h_idx]]
  out <- o[, list(cell_id, nearest_hospital, travel_time_min, distance_km, origin_snap_m)]
  data.table::setattr(out, "hospital_snap", h[, list(hospital_id, hospital_snap_m)])
  out
}

#' Experimental r5r engine (same contract)
#'
#' Requires the r5r package, Java 21 and the .osm.pbf in `r5r_data_path`.
#' Distance is not returned by r5r's travel_time_matrix and is left NA.
#' @noRd
route_nearest_r5r <- function(origins, hospitals, r5r_data_path = "data-raw",
                              max_trip_duration = 300L, ...) {
  pkg <- "r5r"
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop("The r5r engine needs the r5r package and Java 21.")
  }
  setup_r5 <- getExportedValue(pkg, "setup_r5")
  ttm <- getExportedValue(pkg, "travel_time_matrix")
  core <- setup_r5(data_path = r5r_data_path, verbose = FALSE)
  o <- data.frame(id = as.character(origins$cell_id), lon = origins$lon, lat = origins$lat)
  d <- data.frame(id = hospitals$hospital_id, lon = hospitals$lon, lat = hospitals$lat)
  m <- data.table::as.data.table(ttm(
    core, origins = o, destinations = d, mode = "CAR",
    max_trip_duration = max_trip_duration, verbose = FALSE, progress = FALSE
  ))
  m <- m[m[, .I[which.min(travel_time_p50)], by = "from_id"]$V1]
  res <- data.table::data.table(
    cell_id = as.integer(m$from_id),
    nearest_hospital = as.character(m$to_id),
    travel_time_min = as.numeric(m$travel_time_p50),
    distance_km = NA_real_,
    origin_snap_m = NA_real_
  )
  out <- res[data.table::data.table(cell_id = as.integer(origins$cell_id)), on = "cell_id"]
  data.table::setattr(out, "hospital_snap", NULL)
  out
}


# Provincial checkpoints ----------------------------------------------------
# Only origins are partitioned. Every partition uses the entire national
# graph and hospital register, including hospitals across provincial borders.
atlas_hash <- function(x) {
  f <- tempfile()
  on.exit(unlink(f))
  saveRDS(x, f, compress = FALSE, version = 3)
  unname(tools::md5sum(f))
}

province_plan <- function(grid) {
  if (!nrow(grid) || anyNA(grid$province_id) || any(!nzchar(as.character(grid$province_id)))) {
    stop("Every populated cell must have a province_id.")
  }
  if (anyDuplicated(grid$cell_id)) stop("Duplicate cell_id in population grid.")
  ids <- sort(unique(as.character(grid$province_id)))
  plan <- data.frame(
    province_id = ids,
    province = vapply(ids, function(id) as.character(grid$province[match(id, grid$province_id)]), character(1)),
    n_cells = vapply(ids, function(id) sum(as.character(grid$province_id) == id), integer(1)),
    key = vapply(ids, atlas_hash, character(1)),
    stringsAsFactors = FALSE
  )
  plan[order(plan$n_cells, plan$province_id), , drop = FALSE]
}

routing_runtime <- function() {
  pkgs <- c("dodgr", "sf", "arrow", "data.table", "terra", "targets")
  list(R = as.character(getRversion()), packages = stats::setNames(
    vapply(pkgs, function(p) as.character(utils::packageVersion(p)), character(1)), pkgs
  ))
}

prepare_routing_snapshot <- function(graph_file, grid, hospitals, cfg) {
  plan <- province_plan(grid)
  source_files <- sort(c(list.files("R", pattern = "[.]R$", full.names = TRUE),
                         "scripts/05_accessibility.R", "_targets.R", "DESCRIPTION", "renv.lock"))
  identity <- list(
    graph = unname(tools::md5sum(graph_file)), grid = atlas_hash(as.data.frame(grid)),
    hospitals = atlas_hash(hospitals), config = cfg,
    code = tools::md5sum(source_files), runtime = routing_runtime()
  )
  snapshot_id <- atlas_hash(identity)
  out <- file.path("_atlas_inputs", snapshot_id)
  dir.create(out, recursive = TRUE, showWarnings = FALSE)
  paths <- file.path(out, c("graph.parquet", "grid.rds", "hospitals.rds", "config.rds"))
  if (!file.exists(paths[1]) && !file.copy(graph_file, paths[1])) stop("Cannot copy routing graph.")
  saveRDS(grid, paths[2], version = 3)
  saveRDS(hospitals, paths[3], version = 3)
  saveRDS(cfg, paths[4], version = 3)
  if (!identical(unname(tools::md5sum(paths[1])), identity$graph)) stop("Snapshot graph checksum mismatch.")
  manifest <- list(schema = 1L, snapshot_id = snapshot_id, identity = identity,
                   provinces = plan, checksums = stats::setNames(unname(tools::md5sum(paths)), basename(paths)))
  manifest_file <- file.path(out, "manifest.rds")
  saveRDS(manifest, manifest_file, version = 3)
  c(manifest_file, paths)
}

read_routing_snapshot <- function(snapshot_files, verify = TRUE) {
  path <- snapshot_files[basename(snapshot_files) == "manifest.rds"]
  if (length(path) != 1L || !file.exists(path)) stop("Missing routing snapshot manifest.")
  m <- readRDS(path)
  if (!identical(m$schema, 1L) || !identical(atlas_hash(m$identity), m$snapshot_id)) {
    stop("Invalid routing snapshot identity.")
  }
  m$directory <- dirname(path)
  if (verify) {
    paths <- file.path(m$directory, names(m$checksums))
    if (any(!file.exists(paths)) || !identical(unname(tools::md5sum(paths)), unname(m$checksums))) {
      stop("Routing input checksum mismatch.")
    }
    if (!identical(m$identity$runtime, routing_runtime())) {
      stop("R/package versions differ from the prepared snapshot; refusing mixed results.")
    }
    code <- names(m$identity$code)
    if (any(!file.exists(code)) || !identical(tools::md5sum(code), m$identity$code)) {
      stop("Routing source code differs from the prepared snapshot.")
    }
  }
  m
}

province_result_dir <- function(manifest, province_id, cfg) {
  i <- match(as.character(province_id), manifest$provinces$province_id)
  if (is.na(i)) stop("Unknown province_id.")
  file.path(cfg$paths$processed, "provinces", manifest$snapshot_id, manifest$provinces$key[i])
}

validate_province_bundle <- function(bundle, grid, province_id, snapshot_id) {
  if (!identical(bundle$snapshot_id, snapshot_id)) stop("Stale province checkpoint.")
  if (!identical(as.character(bundle$province_id), as.character(province_id))) stop("Wrong province checkpoint.")
  rows <- bundle$rows
  required <- c("cell_id", "province_id", "population", "travel_time_min", "distance_km", "nearest_hospital")
  if (!is.data.frame(rows) || !all(required %in% names(rows))) stop("Invalid province result schema.")
  expected <- grid$cell_id[as.character(grid$province_id) == as.character(province_id)]
  if (!length(expected) || nrow(rows) != length(expected) || anyDuplicated(rows$cell_id) ||
      !setequal(rows$cell_id, expected)) stop("Province cell coverage is incomplete or duplicated.")
  if (anyNA(rows$province_id) || any(as.character(rows$province_id) != as.character(province_id))) {
    stop("Province result contains cells from another province.")
  }
  original_pop <- grid$population[match(rows$cell_id, grid$cell_id)]
  if (!isTRUE(all.equal(as.numeric(rows$population), as.numeric(original_pop), tolerance = 0))) {
    stop("Province population mismatch.")
  }
  t <- rows$travel_time_min
  if (any(!is.na(t) & (!is.finite(t) | t < 0))) stop("Invalid province travel times.")
  if (any(!is.na(t) & is.na(rows$nearest_hospital))) stop("Reachable cells need a hospital.")
  if (!is.null(bundle$rows_hash) && !identical(bundle$rows_hash, atlas_hash(as.data.frame(rows)))) {
    stop("Province result checksum mismatch.")
  }
  invisible(TRUE)
}

route_province <- function(graph, grid, hospitals, cfg, province_id) {
  ids <- which(as.character(grid$province_id) == as.character(province_id))
  if (!length(ids)) stop("Unknown or empty province.")
  g <- data.table::as.data.table(grid)[ids]
  rc <- cfg$routing
  dests <- data.table::data.table(hospital_id = hospitals$hospital_id, lon = hospitals$lon, lat = hospitals$lat)
  message(sprintf("province %s: %d origins; all %d national hospitals", province_id, nrow(g), nrow(dests)))
  res <- route_nearest(
    rc$engine, graph = graph, origins = g[, list(cell_id, lon, lat)], hospitals = dests,
    chunk_size = rc$hospital_chunk_size, connector_speed_kmh = rc$connector_speed_kmh,
    r5r_data_path = cfg$paths$raw
  )
  snap <- attr(res, "hospital_snap")
  tt <- g[, list(cell_id, lon, lat, population, province_id, province, county_id, county)][res, on = "cell_id"]
  tt[, category_id := classify_travel_time(travel_time_min, cfg$thresholds_min)]
  data.table::setattr(tt, "hospital_snap", snap)
  tt
}

write_province_bundle <- function(rows, grid, province_id, manifest, cfg) {
  out <- province_result_dir(manifest, province_id, cfg)
  bundle <- list(snapshot_id = manifest$snapshot_id, province_id = as.character(province_id),
                 rows = rows, rows_hash = atlas_hash(as.data.frame(rows)))
  validate_province_bundle(bundle, grid, province_id, manifest$snapshot_id)
  dir.create(dirname(out), recursive = TRUE, showWarnings = FALSE)
  staging <- tempfile("province-", tmpdir = dirname(out))
  dir.create(staging)
  on.exit(unlink(staging, recursive = TRUE))
  saveRDS(bundle, file.path(staging, "result.rds"), version = 3)
  df <- as.data.frame(rows)
  write_geoparquet(sf::st_as_sf(df, coords = c("lon", "lat"), crs = 4326, remove = FALSE),
                   file.path(staging, "travel_times.parquet"))
  jsonlite::write_json(list(snapshot_id = manifest$snapshot_id, province_id = as.character(province_id),
                            cells = nrow(rows), population = sum(rows$population),
                            files_md5 = as.list(stats::setNames(
                              unname(tools::md5sum(file.path(staging, c("result.rds", "travel_times.parquet")))),
                              c("result.rds", "travel_times.parquet")))),
                       file.path(staging, "completion.json"), auto_unbox = TRUE, pretty = TRUE)
  if (dir.exists(out)) stop("Refusing to overwrite an existing province checkpoint.")
  if (!file.rename(staging, out)) stop("Cannot commit province checkpoint atomically.")
  message(sprintf("province %s: checkpoint committed (%d cells)", province_id, nrow(rows)))
  file.path(out, c("result.rds", "travel_times.parquet", "completion.json"))
}

read_province_bundle <- function(out, grid, province_id, snapshot_id) {
  paths <- file.path(out, c("result.rds", "travel_times.parquet", "completion.json"))
  if (any(!file.exists(paths))) stop("Incomplete province checkpoint files.")
  b <- readRDS(paths[1])
  validate_province_bundle(b, grid, province_id, snapshot_id)
  completion <- jsonlite::read_json(paths[3], simplifyVector = TRUE)
  if (!identical(completion$snapshot_id, snapshot_id) ||
      !identical(as.character(completion$province_id), as.character(province_id))) stop("Invalid completion record.")
  if (!identical(unname(tools::md5sum(paths[1:2])), unname(unlist(completion$files_md5)))) {
    stop("Province checkpoint file checksum mismatch.")
  }
  b
}

combine_province_bundles <- function(bundles, grid, snapshot_id) {
  expected <- as.character(province_plan(grid)$province_id)
  ids <- vapply(bundles, function(b) as.character(b$province_id), character(1))
  if (anyDuplicated(ids) || !setequal(ids, expected)) stop("National merge requires every province exactly once.")
  for (i in seq_along(bundles)) validate_province_bundle(bundles[[i]], grid, ids[i], snapshot_id)
  snaps <- lapply(bundles, function(b) attr(b$rows, "hospital_snap"))
  if (!all(vapply(snaps, identical, logical(1), snaps[[1]]))) stop("Inconsistent national hospital snapping.")
  tt <- data.table::rbindlist(lapply(bundles, function(b) b$rows), use.names = TRUE)
  if (anyDuplicated(tt$cell_id) || !setequal(tt$cell_id, grid$cell_id)) stop("National cell coverage mismatch.")
  tt <- tt[match(grid$cell_id, tt$cell_id)]
  data.table::setattr(tt, "hospital_snap", snaps[[1]])
  tt
}
