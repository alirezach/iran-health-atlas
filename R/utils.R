# Small shared helpers --------------------------------------------------------

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x

#' Great-circle distance in metres (vectorised)
#' @noRd
haversine_m <- function(lon1, lat1, lon2, lat2) {
  rad <- pi / 180
  dlat <- (lat2 - lat1) * rad
  dlon <- (lon2 - lon1) * rad
  a <- sin(dlat / 2)^2 + cos(lat1 * rad) * cos(lat2 * rad) * sin(dlon / 2)^2
  2 * 6371008.8 * asin(pmin(1, sqrt(a)))
}

#' Evaluate an expression with s2 switched on or off, restoring the setting
#' @noRd
with_s2 <- function(use_s2, expr) {
  old <- suppressMessages(sf::sf_use_s2(use_s2))
  on.exit(suppressMessages(sf::sf_use_s2(old)), add = TRUE)
  expr
}

#' Accept a file path or a SpatRaster
#' @noRd
as_spatraster <- function(x) {
  if (inherits(x, "SpatRaster")) x else terra::rast(x)
}

#' Encode OSM one-way rules: 1 forward only, -1 reverse only, 0 both ways
#'
#' Roundabouts and motorways are one-way unless tagged otherwise (OSM wiki).
#' @noRd
oneway_code <- function(oneway, junction = NULL, highway = NULL) {
  ow <- tolower(trimws(as.character(oneway)))
  code <- integer(length(ow))
  code[ow %in% c("yes", "true", "1")] <- 1L
  code[ow %in% c("-1", "reverse")] <- -1L
  unset <- is.na(ow) | ow == ""
  if (!is.null(junction)) {
    code[unset & tolower(as.character(junction)) %in% c("roundabout", "circular")] <- 1L
  }
  if (!is.null(highway)) {
    code[unset & highway %in% c("motorway", "motorway_link")] <- 1L
  }
  code
}
