test_that("fleet age reflects known build history", {
  skip_if_no_key()

  a <- analyze.fleetage("US", 2024)
  get <- function(r, col) a[[col]][a$resource == r]

  # Coal is the oldest fleet by a wide margin; solar the youngest.
  expect_gt(get("Coal", "mean_age"), 35)
  expect_lt(get("Solar", "mean_age"), 12)
  expect_gt(get("Coal", "mean_age"), get("Solar", "mean_age"))
  expect_gt(get("Nuclear", "mean_age"), 30)

  expect_true(all(a$oldest_year <= a$newest_year))
  expect_true(all(a$newest_year <= 2024))
})

test_that("capacity-weighted age differs from unweighted where sizes vary", {
  skip_if_no_key()

  a <- analyze.fleetage("US", 2024)
  h <- a[a$resource == "Hydro", ]

  # Which of the two is larger depends on whether the big units are older or
  # newer than the small ones, and EIA revises the inventory -- so pinning a
  # direction makes this test fail on data changes rather than on defects.
  # The invariant is that weighting by capacity actually changes the answer
  # for a fleet whose unit sizes vary this much.
  expect_gt(abs(h$weighted_mean_age - h$mean_age), 1)
  expect_true(all(a$mean_age > 0 & a$weighted_mean_age > 0))
  expect_true(all(is.finite(a$weighted_mean_age)))
})

test_that("additions capture the recent solar build-out", {
  skip_if_no_key()

  ad <- analyze.additions("US", 2024, years = 2020:2024)
  mw <- function(res, yr) sum(ad$mw[ad$resource == res & ad$year == yr])

  # Solar additions grew several-fold over this window and overtook wind.
  expect_gt(mw("Solar", 2024), mw("Solar", 2020))
  expect_gt(mw("Solar", 2024), mw("Wind", 2024))

  # Vogtle 3 and 4 are the only new US nuclear in this period, ~1.1 GW each.
  expect_equal(mw("Nuclear", 2023), 1114, tolerance = 5)
  expect_equal(mw("Nuclear", 2024), 1114, tolerance = 5)

  expect_true(all(ad$mw > 0))
  expect_true(all(ad$year %in% 2020:2024))
})

test_that("retirements exclude stale past-dated records", {
  skip_if_no_key()

  # Some operating units carry planned-retirement dates as far back as the
  # 1950s. Those are stale entries and must not appear as retirements.
  r <- analyze.retirements("US", 2024, through = 2032)
  expect_gte(min(r$year), 2024)
  expect_lte(max(r$year), 2032)

  # Coal dominates announced retirements.
  by_res <- stats::aggregate(mw ~ resource, data = r, sum)
  expect_equal(by_res$resource[which.max(by_res$mw)], "Coal")
  expect_gt(max(by_res$mw), 20000)
})

test_that("the retirement window is adjustable", {
  skip_if_no_key()

  wide   <- analyze.retirements("US", 2024, from = 1900, through = 2032)
  narrow <- analyze.retirements("US", 2024, through = 2032)

  # Widening the window can only add rows, never drop them. An earlier version
  # asserted that lowering `from` exposed stale pre-2024 retirement dates --
  # but EIA has since cleaned those records, so that test was asserting the
  # continued existence of a data defect rather than the filter working.
  expect_gte(nrow(wide), nrow(narrow))
  expect_lte(min(wide$year), min(narrow$year))

  # The upper bound is enforced in both cases.
  expect_lte(max(wide$year), 2032)
  expect_gte(min(narrow$year), 2024)
})

test_that("concentration metrics agree with each other", {
  skip_if_no_key()

  d <- analyze.diversity("US", 2024)

  # HHI runs 0-10,000; a single fuel supplying everything scores 10,000.
  expect_gt(d$hhi, 0)
  expect_lt(d$hhi, 10000)
  # Effective fuels cannot exceed the number of fuels present.
  expect_lte(d$effective_fuels, d$fuels)
  expect_gte(d$effective_fuels, 1)
  expect_equal(d$effective_fuels, exp(d$shannon))
})

test_that("a coal-dominated state scores as concentrated", {
  skip_if_no_key()

  wv <- analyze.diversity("WV", 2024)
  us <- analyze.diversity("US", 2024)

  # West Virginia generates overwhelmingly from coal; the nation does not.
  expect_gt(wv$top_share, 80)
  expect_gt(wv$hhi, us$hhi)
  expect_lt(wv$effective_fuels, us$effective_fuels)
  expect_lt(wv$effective_fuels, 2.5)
})
