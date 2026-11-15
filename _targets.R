# Iran Health Accessibility Atlas: targets pipeline ---------------------------
#
#   targets::tar_make()                          build or update everything
#   targets::tar_make(names = travel_times_file)  rebuild one branch (partial)
#   targets::tar_outdated()                      what would rebuild
#   targets::tar_destroy(); targets::tar_make()  rebuild from scratch
#   targets::tar_visnetwork()                    dependency graph (visNetwork)
#
# Caching: every target is hashed; only stale targets rerun. `osm_version`
# always checks the published Geofabrik md5, so a new OSM extract (monthly
# refresh) invalidates the network, hospitals and everything downstream,
# while WorldPop and boundary downloads stay cached.

library(targets)

tar_option_set(
  packages = c("data.table", "sf", "terra", "arrow"),
  format = "rds",
  memory = "transient",
  error = "stop"
)

tar_source(c("R", "scripts"))

list(
  # Configuration ------------------------------------------------------------
  tar_target(config_file, "config.yml", format = "file"),
  tar_target(cfg, atlas_config(config_file)),

  # 01 Download --------------------------------------------------------------
  tar_target(osm_version, osm_remote_version(cfg), cue = tar_cue(mode = "always")),
  tar_target(osm_pbf, download_osm(cfg, osm_version), format = "file"),
  tar_target(worldpop_1km, download_worldpop(cfg, "1km"), format = "file"),
  tar_target(worldpop_100m, download_worldpop(cfg, "100m"), format = "file"),
  tar_target(adm1_files, download_boundaries(cfg, "ADM1"), format = "file"),
  tar_target(adm2_files, download_boundaries(cfg, "ADM2"), format = "file"),
  tar_target(adm1, read_boundaries(adm1_files)),
  tar_target(adm2, add_parent_province(read_boundaries(adm2_files), adm1)),
  tar_target(
    source_metadata,
    write_source_metadata(cfg, osm_pbf, osm_version, worldpop_1km, worldpop_100m, adm1_files, adm2_files),
    format = "file"
  ),

  # 02 Hospitals -------------------------------------------------------------
  tar_target(hospitals_raw, read_osm_hospitals(osm_pbf, cfg)),
  tar_target(hospitals, extract_hospitals(hospitals_raw, adm1, adm2, cfg)),
  tar_target(hospitals_file, write_hospitals(hospitals, cfg), format = "file"),

  # 03 Population grid -------------------------------------------------------
  tar_target(population_grid, build_grid_stage(worldpop_1km, worldpop_100m, adm1, adm2, cfg)),
  tar_target(population_grid_file, write_population_grid(population_grid, cfg), format = "file"),
  tar_target(zonal, zonal_population(worldpop_1km, adm1)),

  # 04 Road network ----------------------------------------------------------
  tar_target(road_graph_file, build_network_stage(osm_pbf, cfg), format = "file"),

  # 05 Accessibility ---------------------------------------------------------
  tar_target(travel_times, accessibility_stage(road_graph_file, population_grid, hospitals, cfg)),
  tar_target(travel_times_file, write_travel_times(travel_times, cfg), format = "file"),

  # 06 Indicators ------------------------------------------------------------
  tar_target(indicators, indicators_stage(travel_times, hospitals, adm2, cfg)),
  tar_target(indicator_files, write_indicators(indicators, adm1, cfg), format = "file"),

  # 08 Validation: runs before export so a failing QA never reaches the site --
  tar_target(
    qa_report,
    validation_stage(
      hospitals, population_grid, travel_times, adm1, adm2, zonal, worldpop_1km,
      road_graph_file, c(hospitals_file, population_grid_file, travel_times_file, indicator_files),
      osm_version, cfg
    ),
    format = "file"
  ),

  # 07 Web export ------------------------------------------------------------
  tar_target(
    web_files,
    export_web_stage(
      travel_times, indicators, hospitals, adm1, worldpop_1km, osm_version,
      c(hospitals_file, population_grid_file, travel_times_file, indicator_files, source_metadata),
      qa_report, cfg
    ),
    format = "file"
  )
)
