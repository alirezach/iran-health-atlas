# Stage 01: download raw data -------------------------------------------------
# Road network + hospitals  Geofabrik OSM extract for Iran (via osmextract)
# Population                WorldPop 2020, 1 km (+ 100 m for centroid weights)
# Boundaries                geoBoundaries gbOpen ADM1 (province), ADM2 (county)
#
# Functions are called from _targets.R. Running this file directly
# (Rscript scripts/01_download_data.R) builds only this stage.

osm_remote_version <- function(cfg) remote_md5(cfg$sources$osm_md5_url)

download_osm <- function(cfg, version) {
  dest <- file.path(cfg$paths$raw, paste0("geofabrik_", basename(cfg$sources$osm_pbf_url)))
  stale <- !file.exists(dest) || (!is.na(version) && !identical(file_md5(dest), version))
  old <- options(timeout = 7200)
  on.exit(options(old), add = TRUE)
  path <- osmextract::oe_download(
    file_url = cfg$sources$osm_pbf_url,
    provider = "geofabrik",
    download_directory = cfg$paths$raw,
    force_download = stale,
    max_file_size = Inf,
    quiet = FALSE
  )
  if (!is.na(version) && !identical(file_md5(path), version)) {
    warning("OSM extract checksum differs from the published md5 (extract updated mid-download?).")
  }
  path
}

download_worldpop <- function(cfg, resolution = c("1km", "100m")) {
  resolution <- match.arg(resolution)
  raw <- cfg$paths$raw
  if (resolution == "100m" && !isTRUE(cfg$population$weighted_centroids)) {
    f <- file.path(raw, "worldpop_100m_disabled.txt")
    writeLines("Population-weighted centroids disabled in config.yml", f)
    return(f)
  }
  url <- cfg$sources[[paste0("worldpop_", resolution, "_url")]]
  tryCatch(
    download_if_needed(url, file.path(raw, basename(url))),
    error = function(e) {
      if (resolution == "1km") stop(e)
      warning("WorldPop 100 m unavailable; using cell centres: ", conditionMessage(e))
      f <- file.path(raw, "worldpop_100m_unavailable.txt")
      writeLines(conditionMessage(e), f)
      f
    }
  )
}

download_boundaries <- function(cfg, level) {
  meta <- geoboundaries_meta(cfg$sources$boundaries_api, level)
  geo <- file.path(cfg$paths$raw, sprintf("geoBoundaries-IRN-%s.geojson", level))
  download_if_needed(meta$gjDownloadURL, geo)
  meta_file <- file.path(cfg$paths$raw, sprintf("geoBoundaries-IRN-%s.meta.json", level))
  jsonlite::write_json(meta, meta_file, auto_unbox = TRUE, pretty = TRUE)
  c(geo, meta_file)
}

write_source_metadata <- function(cfg, osm_pbf, osm_version, worldpop_1km, worldpop_100m,
                                  adm1_files, adm2_files) {
  gb_row <- function(files, level) {
    m <- jsonlite::read_json(files[2])
    data.frame(
      dataset = paste("Administrative boundaries", level),
      provider = paste("geoBoundaries gbOpen", m$boundarySource %||% ""),
      url = m$gjDownloadURL %||% NA_character_,
      license = m$boundaryLicense %||% "CC BY 4.0",
      version = paste(m$boundaryYearRepresented %||% "", m$buildDate %||% ""),
      file = files[1]
    )
  }
  wp <- function(label, url, file) {
    data.frame(
      dataset = label, provider = "WorldPop (University of Southampton)", url = url,
      license = "CC BY 4.0", version = as.character(cfg$sources$worldpop_year), file = file
    )
  }
  rows <- rbind(
    data.frame(
      dataset = "Road network and hospitals", provider = "OpenStreetMap contributors via Geofabrik",
      url = cfg$sources$osm_pbf_url, license = "ODbL 1.0", version = paste0("md5:", osm_version), file = osm_pbf
    ),
    wp("Population, 1 km (UN-adjusted)", cfg$sources$worldpop_1km_url, worldpop_1km),
    if (endsWith(worldpop_100m, ".tif")) wp("Population, 100 m (centroid weights)", cfg$sources$worldpop_100m_url, worldpop_100m),
    gb_row(adm1_files, "ADM1 (province)"),
    gb_row(adm2_files, "ADM2 (county)")
  )
  rows$retrieved_at <- format(file.mtime(rows$file), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  rows$bytes <- file.size(rows$file)
  rows$md5 <- unname(tools::md5sum(rows$file))
  rows$file <- basename(rows$file)
  path <- file.path(cfg$paths$processed, "metadata.csv")
  data.table::fwrite(rows, path)
  path
}

if (sys.nframe() == 0L) {
  targets::tar_make(names = tidyselect::any_of(c(
    "osm_pbf", "worldpop_1km", "worldpop_100m", "adm1_files", "adm2_files", "source_metadata"
  )))
}
