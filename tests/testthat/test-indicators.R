toy <- function() {
  data.table::data.table(
    province = c("A", "A", "B", "B"),
    population = c(100, 50, 50, 10),
    travel_time_min = c(10, 25, 90, NA)
  )
}

test_that("travel times are classified with inclusive upper bounds", {
  x <- c(0, 15, 15.01, 30, 61, 120, 121, NA)
  expect_equal(classify_travel_time(x), c(1L, 1L, 2L, 2L, 4L, 4L, 5L, NA))
  cats <- atlas_categories(c(15, 30, 60, 120))
  expect_equal(nrow(cats), 5L)
  expect_equal(cats$label[5], "> 120 min")
})

test_that("weighted quantiles and Gini behave at the extremes", {
  expect_equal(weighted_quantile(1:4, rep(1, 4), 0.5), 2)
  expect_equal(weighted_quantile(1:4, c(10, 1, 1, 1), 0.5), 1)
  expect_equal(weighted_gini(c(5, 5, 5, 5)), 0)
  expect_equal(weighted_gini(c(0, 0, 0, 1)), 0.75)
  expect_equal(weighted_gini(c(1, 2), c(2, 2)), weighted_gini(c(1, 1, 2, 2)))
  expect_equal(weighted_theil(c(3, 3, 3)), 0)
})

test_that("coverage counts unreachable people as not covered", {
  s <- summarise_accessibility(toy(), c(15, 30, 60, 120))
  expect_equal(s$population, 210)
  expect_equal(s$population_unreachable, 10)
  expect_equal(s$pop_within_15, 100)
  expect_equal(s$pop_within_60, 150)
  expect_equal(s$share_within_120, 200 / 210)
  expect_equal(s$mean_travel_time_min, (100 * 10 + 50 * 25 + 50 * 90) / 200)
  expect_equal(s$median_travel_time_min, 10)
})

test_that("province summaries split correctly", {
  s <- summarise_accessibility(toy(), c(15, 30, 60, 120), by = "province")
  b <- s[province == "B"]
  expect_equal(b$population, 60)
  expect_equal(b$share_within_120, 50 / 60)
  expect_equal(s[province == "A"]$share_within_30, 1)
})

test_that("national indicators include coverage, percentiles and inequality", {
  prov <- summarise_accessibility(toy(), c(15, 30, 60, 120), by = "province")
  nat <- national_indicators(toy(), c(15, 30, 60, 120), prov, n_hospitals = 3)
  need <- c("share_within_15", "share_within_120", "p50_travel_time_min", "p90_travel_time_min",
            "gini_travel_time", "theil_travel_time", "province_coverage_range", "hospitals_per_100k")
  expect_true(all(need %in% nat$metric))
  g <- nat[metric == "gini_travel_time"]$value
  expect_true(g >= 0 && g <= 1)
})

test_that("distribution and category shares sum to the population", {
  d <- travel_time_distribution(toy())
  expect_equal(sum(d$population), 200)
  expect_true(is.na(d$bin_end[d$bin_start == 90]) || d$bin_end[d$bin_start == 90] == 100)
  cs <- category_shares(toy(), c(15, 30, 60, 120))
  expect_equal(sum(cs$population), 210)
  expect_equal(sum(cs$share), 1)
  expect_equal(cs[category_id == 6L]$population, 10)
})
