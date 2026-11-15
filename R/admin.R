# Administrative boundaries ---------------------------------------------------

#' Read a geoBoundaries GeoJSON into a tidy sf (id, iso, name)
#' @noRd
read_boundaries <- function(files) {
  x <- sf::st_read(files[1], quiet = TRUE)
  x <- sf::st_transform(x, 4326)
  x <- with_s2(FALSE, sf::st_make_valid(x))
  out <- sf::st_sf(
    id = as.character(x$shapeID),
    iso = as.character(x$shapeISO %||% NA_character_),
    name = as.character(x$shapeName),
    geometry = sf::st_geometry(x)
  )
  out[order(out$name), ]
}

#' Attach the parent province to each county (point-on-surface lookup)
#' @noRd
add_parent_province <- function(adm2, adm1) {
  pts <- with_s2(FALSE, suppressWarnings(sf::st_point_on_surface(sf::st_geometry(adm2))))
  idx <- with_s2(FALSE, sf::st_nearest_feature(pts, adm1))
  adm2$province_id <- adm1$id[idx]
  adm2$province <- adm1$name[idx]
  adm2
}

#' Assign grid cells to admin units by rasterising the polygons on the grid
#'
#' Cells whose centre falls outside every polygon (coastline and border
#' slivers) take the nearest unit.
#' @noRd
assign_admin <- function(dt, template, adm, prefix) {
  r <- as_spatraster(template)
  adm$adm_idx <- seq_len(nrow(adm))
  ar <- terra::rasterize(terra::vect(adm), r, field = "adm_idx")
  idx <- terra::values(ar, mat = FALSE)[dt$cell_id]
  miss <- which(is.na(idx))
  if (length(miss)) {
    pts <- sf::st_as_sf(as.data.frame(dt[miss, list(lon, lat)]), coords = c("lon", "lat"), crs = 4326)
    idx[miss] <- with_s2(FALSE, sf::st_nearest_feature(pts, adm))
  }
  dt[, (paste0(prefix, "_id")) := adm$id[idx]]
  dt[, (prefix) := adm$name[idx]]
  dt
}

#' Assign point features (hospitals) to admin units
#'
#' With drop_outside = TRUE, points outside every polygon are removed (the
#' Geofabrik extract includes a buffer beyond the national border).
#' @noRd
assign_admin_points <- function(x, adm, prefix, drop_outside = FALSE) {
  hit <- with_s2(FALSE, sf::st_intersects(x, adm))
  idx <- vapply(hit, function(i) if (length(i)) i[1] else NA_integer_, integer(1))
  miss <- is.na(idx)
  if (drop_outside) {
    x <- x[!miss, ]
    idx <- idx[!miss]
  } else if (any(miss)) {
    idx[miss] <- with_s2(FALSE, sf::st_nearest_feature(x[miss, ], adm))
  }
  x[[paste0(prefix, "_id")]] <- adm$id[idx]
  x[[prefix]] <- adm$name[idx]
  x
}

#' Province population totals from the raster with exactextractr
#'
#' Used to reconcile the grid-based totals (coverage-fraction weighted sums).
#' @noRd
zonal_population <- function(pop_1km, adm) {
  r <- as_spatraster(pop_1km)
  data.table::data.table(
    id = adm$id,
    name = adm$name,
    population_zonal = exactextractr::exact_extract(r, adm, "sum", progress = FALSE)
  )
}
