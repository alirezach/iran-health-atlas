# Road network ----------------------------------------------------------------

#' Build a directed, time-weighted car graph from OSM linestrings
#'
#' Vertices are OSM nodes identified by their exact coordinates, so ways that
#' share a node are joined. Each consecutive coordinate pair becomes an edge
#' with `d` = great-circle length (m) and `d_weighted` = free-flow time (s).
#' One-way rules follow `oneway_code()`.
#' @noRd
build_road_graph <- function(roads, speeds_kmh) {
  roads <- roads[!sf::st_is_empty(roads) & roads$highway %in% names(speeds_kmh), ]
  if (!nrow(roads)) stop("No drivable roads found.")
  if (any(as.character(sf::st_geometry_type(roads)) != "LINESTRING")) {
    roads <- suppressWarnings(sf::st_cast(sf::st_cast(roads, "MULTILINESTRING"), "LINESTRING"))
  }
  n_roads <- nrow(roads)
  oneway <- if ("oneway" %in% names(roads)) roads$oneway else rep(NA_character_, n_roads)
  junction <- if ("junction" %in% names(roads)) roads$junction else rep(NA_character_, n_roads)
  ow <- oneway_code(oneway, junction, roads$highway)
  spd <- unname(speeds_kmh[roads$highway])
  way_ids <- as.character(roads$osm_id)

  co <- sf::st_coordinates(roads)
  n <- nrow(co)
  line <- co[, "L1"]
  key <- data.table::data.table(rx = round(co[, "X"], 7), ry = round(co[, "Y"], 7))
  node <- data.table::frank(key, ties.method = "dense")
  i <- which(line[-1] == line[-n])
  seg <- data.table::data.table(
    from = node[i], to = node[i + 1L],
    fx = co[i, "X"], fy = co[i, "Y"], tx = co[i + 1L, "X"], ty = co[i + 1L, "Y"],
    speed = spd[line[i]], ow = ow[line[i]], way = way_ids[line[i]]
  )
  rm(co, key, node)
  seg[, d := haversine_m(fx, fy, tx, ty)]
  seg <- seg[from != to & d > 0]
  seg[, d_weighted := d / (speed / 3.6)]
  fwd <- seg[ow >= 0L, list(
    from_id = from, to_id = to, from_lon = fx, from_lat = fy, to_lon = tx, to_lat = ty,
    d, d_weighted, speed_kmh = speed, way_id = way
  )]
  bwd <- seg[ow <= 0L, list(
    from_id = to, to_id = from, from_lon = tx, from_lat = ty, to_lon = fx, to_lat = fy,
    d, d_weighted, speed_kmh = speed, way_id = way
  )]
  g <- data.table::rbindlist(list(fwd, bwd))
  g[, `:=`(from_id = as.character(from_id), to_id = as.character(to_id))]
  g[, edge_id := seq_len(.N)]
  g
}

#' Keep connected components with at least `min_edges` edges
#'
#' Removes digitising fragments while keeping islands such as Qeshm and Kish
#' that carry their own hospitals.
#' @noRd
filter_graph_components <- function(graph, min_edges = 500) {
  g <- dodgr::dodgr_components(as.data.frame(graph))
  sizes <- table(g$component)
  keep <- as.integer(names(sizes)[sizes >= min_edges])
  if (!length(keep)) keep <- 1L
  data.table::as.data.table(g[g$component %in% keep, ])
}

#' Reverse every edge (used to route from hospitals back to origins)
#' @noRd
reverse_graph <- function(graph) {
  g <- data.table::copy(data.table::as.data.table(graph))
  data.table::setnames(
    g,
    c("from_id", "to_id", "from_lon", "from_lat", "to_lon", "to_lat"),
    c("to_id", "from_id", "to_lon", "to_lat", "from_lon", "from_lat")
  )
  data.table::setcolorder(g, c("from_id", "to_id", "from_lon", "from_lat", "to_lon", "to_lat"))
  g
}
