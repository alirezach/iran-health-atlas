# Indicators ------------------------------------------------------------------

#' Population-weighted quantiles (smallest x whose cumulative weight >= p)
#' @noRd
weighted_quantile <- function(x, w, probs = 0.5) {
  ok <- !is.na(x) & !is.na(w) & w > 0
  x <- x[ok]
  w <- w[ok]
  if (!length(x)) return(rep(NA_real_, length(probs)))
  o <- order(x)
  x <- x[o]
  cw <- cumsum(w[o]) / sum(w)
  vapply(probs, function(p) x[which(cw >= p - 1e-12)[1]], numeric(1))
}

#' Population-weighted Gini coefficient (trapezoidal Lorenz curve)
#' @noRd
weighted_gini <- function(x, w = rep(1, length(x))) {
  ok <- !is.na(x) & !is.na(w) & w > 0
  x <- x[ok]
  w <- w[ok]
  if (length(x) < 2 || sum(w * x) == 0) return(0)
  o <- order(x)
  x <- x[o]
  w <- w[o]
  p <- c(0, cumsum(w) / sum(w))
  l <- c(0, cumsum(w * x) / sum(w * x))
  1 - sum(diff(p) * (l[-1] + l[-length(l)]))
}

#' Population-weighted Theil T index (positive values only)
#' @noRd
weighted_theil <- function(x, w = rep(1, length(x))) {
  ok <- !is.na(x) & !is.na(w) & w > 0 & x > 0
  x <- x[ok]
  w <- w[ok]
  if (!length(x)) return(NA_real_)
  mu <- sum(w * x) / sum(w)
  sum(w * (x / mu) * log(x / mu)) / sum(w)
}

accessibility_stats <- function(population, minutes, thresholds) {
  reach <- !is.na(minutes)
  tot <- sum(population)
  pr <- sum(population[reach])
  out <- list(
    population = tot,
    population_reachable = pr,
    population_unreachable = tot - pr,
    mean_travel_time_min = if (pr > 0) sum(population[reach] * minutes[reach]) / pr else NA_real_,
    median_travel_time_min = weighted_quantile(minutes, population, 0.5)
  )
  for (t in thresholds) {
    within <- sum(population[reach & minutes <= t])
    out[[paste0("pop_within_", t)]] <- within
    out[[paste0("share_within_", t)]] <- if (tot > 0) within / tot else NA_real_
  }
  out
}

#' Coverage summary, optionally by group
#'
#' Shares use the *total* population as denominator, so people without any
#' road route count as not covered.
#' @noRd
summarise_accessibility <- function(dt, thresholds = c(15, 30, 60, 120), by = NULL) {
  dt <- data.table::as.data.table(dt)
  dt[, accessibility_stats(population, travel_time_min, thresholds), by = by]
}

metric_row <- function(metric, value, unit, description) {
  data.table::data.table(metric = metric, value = as.numeric(value), unit = unit, description = description)
}

#' National indicators in long format (metric, value, unit, description)
#' @noRd
national_indicators <- function(dt, thresholds, province_metrics = NULL, n_hospitals = NA_real_) {
  dt <- data.table::as.data.table(dt)
  s <- summarise_accessibility(dt, thresholds)
  r <- dt[!is.na(travel_time_min) & population > 0]
  probs <- c(0.10, 0.25, 0.50, 0.75, 0.90, 0.95, 0.99)
  q <- weighted_quantile(r$travel_time_min, r$population, probs)
  rows <- list(
    metric_row("total_population", s$population, "people", "Population of populated 1 km cells (WorldPop)"),
    metric_row("population_reachable", s$population_reachable, "people", "Population with a road route to at least one hospital"),
    metric_row("population_unreachable", s$population_unreachable, "people", "Population with no road route to any hospital"),
    metric_row("share_unreachable", s$population_unreachable / s$population, "proportion", "Share of population with no road route to any hospital"),
    metric_row("mean_travel_time_min", s$mean_travel_time_min, "minutes", "Population-weighted mean drive time (reachable population)"),
    metric_row("median_travel_time_min", s$median_travel_time_min, "minutes", "Population-weighted median drive time (reachable population)")
  )
  for (t in thresholds) {
    rows[[length(rows) + 1]] <- metric_row(paste0("pop_within_", t), s[[paste0("pop_within_", t)]], "people", sprintf("Population within %g minutes of a hospital", t))
    rows[[length(rows) + 1]] <- metric_row(paste0("share_within_", t), s[[paste0("share_within_", t)]], "proportion", sprintf("Share of population within %g minutes of a hospital", t))
  }
  for (i in seq_along(probs)) {
    rows[[length(rows) + 1]] <- metric_row(sprintf("p%02d_travel_time_min", round(100 * probs[i])), q[i], "minutes", sprintf("Population-weighted %g%% percentile of drive time", 100 * probs[i]))
  }
  rows[[length(rows) + 1]] <- metric_row("gini_travel_time", weighted_gini(r$travel_time_min, r$population), "index", "Population-weighted Gini coefficient of drive time (0 = equal)")
  rows[[length(rows) + 1]] <- metric_row("theil_travel_time", weighted_theil(r$travel_time_min, r$population), "index", "Population-weighted Theil T index of drive time")
  rows[[length(rows) + 1]] <- metric_row("p90_p10_ratio", if (isTRUE(q[1] > 0)) q[5] / q[1] else NA_real_, "ratio", "Ratio of the 90th to the 10th percentile of drive time")
  if (!is.null(province_metrics) && nrow(province_metrics)) {
    ref <- reference_threshold(thresholds)
    cov <- province_metrics[[paste0("share_within_", ref)]]
    rows[[length(rows) + 1]] <- metric_row("province_coverage_min", min(cov, na.rm = TRUE), "proportion", sprintf("Lowest provincial share within %g minutes", ref))
    rows[[length(rows) + 1]] <- metric_row("province_coverage_max", max(cov, na.rm = TRUE), "proportion", sprintf("Highest provincial share within %g minutes", ref))
    rows[[length(rows) + 1]] <- metric_row("province_coverage_range", diff(range(cov, na.rm = TRUE)), "proportion", sprintf("Gap between best and worst province (share within %g minutes)", ref))
    rows[[length(rows) + 1]] <- metric_row("province_coverage_gini", weighted_gini(cov, province_metrics$population), "index", sprintf("Population-weighted Gini of provincial %g-minute coverage", ref))
  }
  rows[[length(rows) + 1]] <- metric_row("n_hospitals", n_hospitals, "facilities", "Deduplicated OSM hospitals inside Iran")
  rows[[length(rows) + 1]] <- metric_row("hospitals_per_100k", 1e5 * n_hospitals / s$population, "per 100,000 people", "Mapped hospitals per 100,000 residents")
  rows[[length(rows) + 1]] <- metric_row("n_grid_cells", nrow(dt), "cells", "Populated 1 km grid cells routed")
  data.table::rbindlist(rows)
}

#' Population by drive-time bin (open-ended last bin has bin_end = NA)
#' @noRd
travel_time_distribution <- function(dt, by = NULL, bin_width = 10, max_minutes = 180) {
  d <- data.table::as.data.table(dt)[!is.na(travel_time_min)]
  d[, bin_start := pmin(floor(travel_time_min / bin_width) * bin_width, max_minutes)]
  out <- d[, list(population = sum(population)), by = c(by, "bin_start")]
  out[, bin_end := data.table::fifelse(bin_start >= max_minutes, NA_real_, bin_start + bin_width)]
  data.table::setorderv(out, c(by, "bin_start"))
  out
}

#' Population share by accessibility category; unreachable = n_thresholds + 2
#' @noRd
category_shares <- function(dt, thresholds, by = NULL) {
  d <- data.table::as.data.table(dt)
  d[, category_code := classify_travel_time(travel_time_min, thresholds)]
  d[is.na(category_code), category_code := length(thresholds) + 2L]
  out <- d[, list(population = sum(population)), by = c(by, "category_code")]
  out[, share := population / sum(population), by = by]
  data.table::setnames(out, "category_code", "category_id")
  data.table::setorderv(out, c(by, "category_id"))
  out
}
