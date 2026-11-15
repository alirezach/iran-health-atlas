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
  so <- snap_to_vertices(o$lon, o$lat, verts)
  o[, `:=`(vertex = so$vertex, origin_snap_m = so$snap_m)]
  sh <- snap_to_vertices(h$lon, h$lat, verts)
  h[, `:=`(vertex = sh$vertex, hospital_snap_m = sh$snap_m)]
  conn <- connector_speed_kmh / 3.6

  rev_g <- as.data.frame(reverse_graph(graph))
  rm(graph)
  rev_c <- dodgr::dodgr_contract_graph(rev_g, verts = unique(c(o$vertex, h$vertex)))
  if (!is.data.frame(rev_c) && is.list(rev_c)) rev_c <- rev_c$graph
  rm(rev_g)
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
