# Configuration and shared classifications ------------------------------------

#' Read the pipeline configuration (config.yml)
#' @noRd
atlas_config <- function(path = "config.yml") {
  cfg <- yaml::read_yaml(path)
  if (!is.null(cfg$default)) cfg <- cfg$default
  cfg$thresholds_min <- sort(as.numeric(unlist(cfg$thresholds_min)))
  cfg$routing$speeds_kmh <- unlist(cfg$routing$speeds_kmh)
  cfg$categories <- atlas_categories(cfg$thresholds_min)
  for (p in cfg$paths) dir.create(p, recursive = TRUE, showWarnings = FALSE)
  cfg
}

#' Accessibility categories derived from the thresholds
#' @noRd
atlas_categories <- function(thresholds = c(15, 30, 60, 120)) {
  thresholds <- sort(as.numeric(thresholds))
  lower <- c(0, thresholds)
  upper <- c(thresholds, Inf)
  n <- length(lower)
  label <- c(
    sprintf("%g\u2013%g min", lower[-n], upper[-n]),
    sprintf("> %g min", lower[n])
  )
  data.frame(
    category_id = seq_len(n),
    lower = lower,
    upper = upper,
    label = label,
    color = atlas_palette(n),
    stringsAsFactors = FALSE
  )
}

#' Sequential, colour-blind-safe palette (saffron to lapis; light = fast)
#'
#' Lightness falls monotonically, so the ramp reads in greyscale and does not
#' rely on red-green contrast.
#' @noRd
atlas_palette <- function(n = 5L) {
  base <- c("#F4E3B5", "#E3B456", "#B8734A", "#5A5A8C", "#1F2452")
  if (n == length(base)) return(base)
  grDevices::colorRampPalette(base)(n)
}

atlas_unreachable_color <- function() "#9AA3AD"

#' Population density ramp for the population layer
#' @noRd
population_palette <- function() {
  c("#EEF0F7", "#C3C5E0", "#8C88BF", "#57509A", "#2A1F66")
}

#' Classify travel times: 1 = within the first threshold (inclusive), ...
#' @noRd
classify_travel_time <- function(minutes, thresholds = c(15, 30, 60, 120)) {
  out <- findInterval(minutes, sort(thresholds), left.open = TRUE) + 1L
  out[is.na(minutes)] <- NA_integer_
  as.integer(out)
}

#' Threshold used for rankings and choropleths (60 min when available)
#' @noRd
reference_threshold <- function(thresholds) {
  if (60 %in% thresholds) 60 else thresholds[ceiling(length(thresholds) / 2)]
}
