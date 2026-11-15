# Stage 04: drivable road network ---------------------------------------------
# OSM highway classes listed in config.yml (routing$speeds_kmh) become a
# directed graph weighted by free-flow time. Small disconnected fragments are
# removed. The graph is cached as Parquet in data-intermediate/.

read_osm_roads <- function(pbf, cfg) {
  classes <- names(cfg$routing$speeds_kmh)
  q <- sprintf(
    "SELECT * FROM \"lines\" WHERE highway IN (%s)",
    paste0("'", classes, "'", collapse = ", ")
  )
  roads <- osmextract::oe_read(
    pbf,
    layer = "lines",
    extra_tags = c("oneway", "junction", "maxspeed"),
    query = q,
    force_vectortranslate = TRUE,
    quiet = TRUE
  )
  for (col in c("oneway", "junction")) {
    if (!col %in% names(roads)) roads[[col]] <- NA_character_
  }
  roads[, c("osm_id", "highway", "oneway", "junction")]
}

build_network_stage <- function(pbf, cfg) {
  roads <- read_osm_roads(pbf, cfg)
  g <- build_road_graph(roads, cfg$routing$speeds_kmh)
  rm(roads)
  gc()
  g <- filter_graph_components(g, cfg$routing$min_component_edges)
  path <- file.path(cfg$paths$intermediate, "road_graph.parquet")
  arrow::write_parquet(g, path, compression = "zstd")
  path
}

if (sys.nframe() == 0L) {
  targets::tar_make(names = tidyselect::any_of("road_graph_file"))
}
