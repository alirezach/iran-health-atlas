# Shared setup for dashboard pages (sourced with execute-dir: project) --------
suppressPackageStartupMessages(library(data.table))

web_dir <- "dashboard/data"
atlas_ready <- file.exists(file.path(web_dir, "site-summary.json"))
atlas_summary <- if (atlas_ready) {
  jsonlite::read_json(file.path(web_dir, "site-summary.json"), simplifyVector = TRUE)
} else {
  NULL
}

fmt_pct <- function(x, digits = 0) {
  ifelse(is.na(x), "-", paste0(formatC(100 * x, format = "f", digits = digits), "%"))
}
fmt_int <- function(x) ifelse(is.na(x), "-", formatC(round(x), format = "f", digits = 0, big.mark = ","))
fmt_min <- function(x) ifelse(is.na(x), "-", paste0(formatC(x, format = "f", digits = 0), " min"))
fmt_millions <- function(x) ifelse(is.na(x), "-", paste0(formatC(x / 1e6, format = "f", digits = 1), " million"))

# Readable text colour on a filled background
text_on <- function(hex) {
  rgb <- grDevices::col2rgb(hex) / 255
  lum <- 0.2126 * rgb[1, ] + 0.7152 * rgb[2, ] + 0.0722 * rgb[3, ]
  ifelse(lum > 0.5, "#13204A", "#FFFFFF")
}

not_built <- function() {
  cat(
    "::: {.callout-note}",
    "## Data not built yet",
    "",
    "This page fills in once the pipeline has run: `targets::tar_make()` locally, or the **Data refresh** GitHub Action.",
    ":::",
    sep = "\n"
  )
}

# Signature element: Iran's population as one ribbon, ordered by drive time
ribbon_html <- function(s) {
  rb <- s$ribbon
  segs <- sprintf(
    '<div class="seg" style="flex-grow:%.6f;background:%s;color:%s" title="%s: %s of people">%s</div>',
    rb$share, rb$color, text_on(rb$color), rb$label, fmt_pct(rb$share, 1),
    ifelse(rb$share >= 0.07, fmt_pct(rb$share), "")
  )
  legend <- sprintf('<span><i style="background:%s"></i>%s</span>', rb$color, rb$label)
  paste0(
    '<p class="atlas-kicker">', fmt_millions(s$national$total_population),
    ' people, lined up by drive time to the nearest hospital</p>',
    '<div class="atlas-ribbon" role="img" aria-label="Share of the population in each drive-time band">',
    paste(segs, collapse = ""), "</div>",
    '<div class="ribbon-legend">', paste(legend, collapse = ""), "</div>"
  )
}

ladder_html <- function(s) {
  thr <- s$thresholds
  rungs <- vapply(seq_along(thr), function(i) {
    sprintf(
      '<div class="rung" style="--rung-color:%s"><span class="t">Within %g min</span><span class="v">%s</span><span class="p">%s</span></div>',
      s$categories$color[i], thr[i],
      fmt_pct(s$national[[paste0("share_within_", thr[i])]]),
      fmt_millions(s$national[[paste0("pop_within_", thr[i])]])
    )
  }, character(1))
  paste0('<div class="ladder">', paste(rungs, collapse = ""), "</div>")
}

facts_html <- function(s) {
  n <- s$national
  items <- c(
    sprintf("Median drive <b>%s</b>", fmt_min(n$median_travel_time_min)),
    sprintf("Slowest 10%% drive over <b>%s</b>", fmt_min(n$p90_travel_time_min)),
    sprintf("Gini of drive time <b>%.2f</b>", n$gini_travel_time),
    sprintf("Hospitals mapped <b>%s</b>", fmt_int(n$n_hospitals)),
    sprintf("No road route <b>%s</b> people", fmt_int(n$population_unreachable))
  )
  paste0('<div class="facts">', paste0("<span>", items, "</span>", collapse = ""), "</div>")
}

map_toolbar_html <- function(s) {
  esc <- function(x) htmltools::htmlEscape(x, attribute = TRUE)
  provs <- paste0('<option value="', esc(s$provinces), '">', esc(s$provinces), "</option>", collapse = "")
  ref <- s$reference_threshold
  thr <- paste0(
    '<option value="', s$thresholds, '"', ifelse(s$thresholds == ref, " selected", ""), ">",
    s$thresholds, " min</option>",
    collapse = ""
  )
  paste0(
    '<div class="map-toolbar" role="group" aria-label="Map controls">',
    '<label>Layer<select id="atlas-layer">',
    '<option value="categories" selected>Accessibility categories</option>',
    '<option value="travel">Travel time</option>',
    '<option value="population">Population</option></select></label>',
    '<label>Province<select id="atlas-province"><option value="all" selected>All of Iran</option>', provs, "</select></label>",
    '<label>Threshold<select id="atlas-threshold">', thr, "</select></label>",
    '<label class="check"><input type="checkbox" id="atlas-hospitals" checked> Hospitals</label>',
    "</div>"
  )
}
