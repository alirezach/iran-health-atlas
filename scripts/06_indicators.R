# Stage 06: indicators --------------------------------------------------------
# Province and county: population, mean/median drive time, population and
# share within each threshold, hospitals per 100k, coverage rank.
# National: coverage, percentiles, Gini/Theil, provincial inequality.

indicators_stage <- function(travel_times, hospitals, adm2, cfg) {
  thr <- cfg$thresholds_min
  ref <- reference_threshold(thr)
  tt <- data.table::as.data.table(travel_times)
  hosp <- data.table::as.data.table(sf::st_drop_geometry(hospitals))

  prov <- summarise_accessibility(tt, thr, by = c("province_id", "province"))
  prov <- merge(prov, hosp[, list(n_hospitals = .N), by = "province_id"], by = "province_id", all.x = TRUE)
  prov[is.na(n_hospitals), n_hospitals := 0L]
  prov[, hospitals_per_100k := 1e5 * n_hospitals / population]
  ref_col <- paste0("share_within_", ref)
  prov[, rank_coverage := data.table::frank(-get(ref_col), ties.method = "min")]
  data.table::setorderv(prov, "rank_coverage")

  county <- summarise_accessibility(tt, thr, by = c("county_id", "county"))
  parents <- data.table::as.data.table(sf::st_drop_geometry(adm2))[, list(county_id = id, province_id, province)]
  county <- merge(county, parents, by = "county_id", all.x = TRUE)
  county <- merge(county, hosp[, list(n_hospitals = .N), by = "county_id"], by = "county_id", all.x = TRUE)
  county[is.na(n_hospitals), n_hospitals := 0L]
  county[, hospitals_per_100k := 1e5 * n_hospitals / population]

  list(
    province = prov,
    county = county,
    national = national_indicators(tt, thr, prov, nrow(hospitals)),
    distribution_province = travel_time_distribution(tt, by = c("province_id", "province")),
    distribution_national = travel_time_distribution(tt),
    categories_national = category_shares(tt, thr),
    categories_province = category_shares(tt, thr, by = c("province_id", "province"))
  )
}

write_indicators <- function(ind, adm1, cfg) {
  out <- cfg$paths$processed
  prov_sf <- merge(adm1[, "id"], as.data.frame(ind$province), by.x = "id", by.y = "province_id")
  names(prov_sf)[names(prov_sf) == "id"] <- "province_id"
  prov_csv <- file.path(out, "province_metrics.csv")
  data.table::fwrite(ind$province, prov_csv)
  c(
    write_geoparquet(prov_sf, file.path(out, "province_metrics.parquet")),
    prov_csv,
    write_table(ind$county, file.path(out, "county_metrics")),
    write_table(ind$national, file.path(out, "national_metrics")),
    write_table(ind$distribution_province, file.path(out, "province_travel_time_distribution"))
  )
}

if (sys.nframe() == 0L) {
  targets::tar_make(names = tidyselect::any_of(c("indicators", "indicator_files")))
}
