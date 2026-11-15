# GeoParquet 1.0.0 writer/reader ----------------------------------------------
# Geometry is stored as WKB in a binary column with the standard "geo" file
# metadata. All outputs are EPSG:4326, which GeoParquet expresses by omitting
# the crs field (default OGC:CRS84, longitude/latitude axis order).

geoparquet_types <- function(geom) {
  lookup <- c(
    POINT = "Point", LINESTRING = "LineString", POLYGON = "Polygon",
    MULTIPOINT = "MultiPoint", MULTILINESTRING = "MultiLineString",
    MULTIPOLYGON = "MultiPolygon", GEOMETRYCOLLECTION = "GeometryCollection"
  )
  types <- unique(as.character(sf::st_geometry_type(geom)))
  unname(lookup[types])
}

#' Write an sf object to GeoParquet
#' @noRd
write_geoparquet <- function(x, path, compression = "zstd") {
  stopifnot(inherits(x, "sf"))
  geom <- sf::st_geometry(x)
  if (!isTRUE(sf::st_crs(geom) == sf::st_crs(4326))) {
    stop("GeoParquet outputs must be EPSG:4326; transform before writing.")
  }
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  df <- sf::st_drop_geometry(x)
  cols <- lapply(df, function(v) if (is.factor(v)) as.character(v) else v)
  wkb <- unclass(sf::st_as_binary(geom, EWKB = FALSE))
  cols[["geometry"]] <- arrow::Array$create(wkb, type = arrow::binary())
  tbl <- do.call(arrow::Table$create, cols)
  geo <- list(
    version = "1.0.0",
    primary_column = "geometry",
    columns = list(geometry = list(
      encoding = "WKB",
      geometry_types = as.list(geoparquet_types(geom)),
      bbox = as.numeric(sf::st_bbox(geom))
    ))
  )
  tbl$metadata$geo <- as.character(jsonlite::toJSON(geo, auto_unbox = TRUE, digits = NA))
  arrow::write_parquet(tbl, path, compression = compression)
  invisible(path)
}

#' Read a GeoParquet file into sf
#' @noRd
read_geoparquet <- function(path) {
  tbl <- arrow::read_parquet(path, as_data_frame = FALSE)
  geo <- jsonlite::fromJSON(tbl$metadata$geo, simplifyVector = FALSE)
  gcol <- geo$primary_column
  df <- as.data.frame(tbl)
  wkb <- structure(lapply(df[[gcol]], as.raw), class = "WKB")
  df[[gcol]] <- NULL
  sf::st_sf(df, geometry = sf::st_as_sfc(wkb, crs = 4326))
}

#' Write a non-spatial table as Parquet + CSV; returns both paths
#' @noRd
write_table <- function(x, stem) {
  dir.create(dirname(stem), recursive = TRUE, showWarnings = FALSE)
  pq <- paste0(stem, ".parquet")
  csv <- paste0(stem, ".csv")
  arrow::write_parquet(as.data.frame(x), pq, compression = "zstd")
  data.table::fwrite(x, csv)
  c(pq, csv)
}
