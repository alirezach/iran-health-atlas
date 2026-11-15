# Stage 02: extract hospitals from OSM ----------------------------------------
# Features tagged amenity=hospital OR healthcare=hospital, from both the
# points (nodes) and multipolygons (ways/relations) layers. Deduplication and
# QA rules live in R/hospitals.R; hospitals outside Iran's boundary are
# dropped (the Geofabrik extract includes a buffer across the border).

read_osm_hospitals <- function(pbf, cfg) {
  where <- "WHERE amenity = 'hospital' OR healthcare = 'hospital'"
  tags <- c("amenity", "healthcare", "emergency", "operator", "name:fa", "name:en", "beds")
  read_layer <- function(layer, extra) {
    osmextract::oe_read(
      pbf,
      layer = layer,
      extra_tags = extra,
      query = sprintf("SELECT * FROM \"%s\" %s", layer, where),
      force_vectortranslate = TRUE,
      quiet = TRUE
    )
  }
  list(
    points = read_layer("points", tags),
    # amenity is already a default multipolygon attribute in osmconf.ini
    polygons = read_layer("multipolygons", setdiff(tags, "amenity"))
  )
}

extract_hospitals <- function(raw, adm1, adm2, cfg) {
  cand <- prepare_hospital_candidates(raw$points, raw$polygons)
  h <- deduplicate_hospitals(cand$points, cand$polygons, dist_m = cfg$hospitals$dedup_distance_m)
  h <- assign_admin_points(h, adm1, "province", drop_outside = TRUE)
  assign_admin_points(h, adm2, "county")
}

write_hospitals <- function(h, cfg) {
  out <- cfg$paths$processed
  pq <- write_geoparquet(h, file.path(out, "hospitals.parquet"))
  csv <- file.path(out, "hospitals.csv")
  data.table::fwrite(sf::st_drop_geometry(h), csv)
  c(pq, csv)
}

if (sys.nframe() == 0L) {
  targets::tar_make(names = tidyselect::any_of(c("hospitals", "hospitals_file")))
}
