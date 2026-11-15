# Hospital candidates and deduplication ---------------------------------------

hospital_fields <- c(
  "osm_id", "osm_type", "name", "name_fa", "name_en", "amenity",
  "healthcare", "emergency", "operator", "beds"
)

#' Standardise an osmextract layer (points or multipolygons)
#' @noRd
standardise_hospital_layer <- function(x, kind = c("point", "polygon")) {
  kind <- match.arg(kind)
  if (is.null(x) || nrow(x) == 0) return(NULL)
  names(x) <- gsub(":", "_", names(x), fixed = TRUE)
  df <- sf::st_drop_geometry(x)
  geom <- sf::st_geometry(x)
  if (kind == "point") {
    type <- rep("node", nrow(df))
    id <- paste0("node/", df$osm_id)
  } else {
    way <- if ("osm_way_id" %in% names(df)) as.character(df$osm_way_id) else rep(NA_character_, nrow(df))
    rel <- if ("osm_id" %in% names(df)) as.character(df$osm_id) else rep(NA_character_, nrow(df))
    type <- ifelse(!is.na(way) & way != "", "way", "relation")
    id <- ifelse(type == "way", paste0("way/", way), paste0("relation/", rel))
  }
  out <- data.frame(osm_id = id, osm_type = type, stringsAsFactors = FALSE)
  for (f in setdiff(hospital_fields, c("osm_id", "osm_type"))) {
    out[[f]] <- if (f %in% names(df)) as.character(df[[f]]) else NA_character_
  }
  out$source_geom <- kind
  sf::st_sf(out, geometry = sf::st_transform(geom, 4326))
}

#' Standardise both OSM layers
#' @noRd
prepare_hospital_candidates <- function(points = NULL, polygons = NULL) {
  pts <- standardise_hospital_layer(points, "point")
  polys <- standardise_hospital_layer(polygons, "polygon")
  if (!is.null(polys)) {
    polys <- with_s2(FALSE, sf::st_make_valid(polys))
    polys <- polys[!sf::st_is_empty(polys), ]
  }
  list(points = pts, polygons = polys)
}

#' Connected components of a neighbour list (breadth-first search)
#' @noRd
graph_components <- function(neighbours) {
  n <- length(neighbours)
  comp <- integer(n)
  k <- 0L
  for (i in seq_len(n)) {
    if (comp[i] != 0L) next
    k <- k + 1L
    comp[i] <- k
    queue <- i
    while (length(queue)) {
      j <- queue[1]
      queue <- queue[-1]
      nb <- neighbours[[j]]
      nb <- nb[comp[nb] == 0L]
      comp[nb] <- k
      queue <- c(queue, nb)
    }
  }
  comp
}

#' Deduplicate OSM hospitals
#'
#' 1. A node that lies inside a hospital footprint is the same facility; its
#'    name fills a nameless footprint.
#' 2. Footprints become representative points (point on surface).
#' 3. Candidates within `dist_m` of each other (transitively) are merged; the
#'    representative prefers footprints, then named features, then the most
#'    complete tags.
#' @noRd
deduplicate_hospitals <- function(points = NULL, polygons = NULL, dist_m = 150) {
  has_pts <- !is.null(points) && nrow(points) > 0
  has_polys <- !is.null(polygons) && nrow(polygons) > 0
  if (has_pts && has_polys) {
    hit <- with_s2(FALSE, sf::st_intersects(points, polygons))
    inside <- lengths(hit) > 0
    if (any(inside)) {
      first_poly <- vapply(hit[inside], function(i) i[1], integer(1))
      donor <- points$name[inside]
      fill <- is.na(polygons$name[first_poly]) & !is.na(donor)
      polygons$name[first_poly[fill]] <- donor[fill]
      points <- points[!inside, ]
    }
  }
  reps <- list()
  if (has_polys) {
    reps[[length(reps) + 1]] <- with_s2(FALSE, suppressWarnings(sf::st_point_on_surface(polygons)))
  }
  if (has_pts && nrow(points) > 0) reps[[length(reps) + 1]] <- points
  if (!length(reps)) stop("No hospital candidates found.")
  cand <- do.call(rbind, reps)

  nb <- sf::st_is_within_distance(cand, cand, dist = dist_m)
  cluster <- graph_components(nb)
  attrs <- sf::st_drop_geometry(cand)
  completeness <- rowSums(!is.na(attrs[, c("name_fa", "name_en", "emergency", "operator", "beds"), drop = FALSE]))
  score <- (cand$source_geom == "polygon") * 4 + (!is.na(cand$name)) * 2 + completeness / 10
  ord <- order(cluster, -score)
  keep <- ord[!duplicated(cluster[ord])]

  out <- cand[keep, ]
  sizes <- tabulate(cluster)
  out$n_merged <- sizes[cluster[keep]]
  out$merged_osm_ids <- vapply(
    cluster[keep],
    function(k) paste(cand$osm_id[cluster == k], collapse = ";"),
    character(1)
  )
  out$hospital_id <- out$osm_id
  xy <- sf::st_coordinates(out)
  out$lon <- xy[, 1]
  out$lat <- xy[, 2]
  out <- out[order(out$hospital_id), ]
  rownames(out) <- NULL
  out
}
