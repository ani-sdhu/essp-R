test_that("storage splits charge from discharge by sign", {
  # EIA reports charging as negative net generation. Both columns come back as
  # positive magnitudes so they can be compared directly.
  skip_if_no_key()

  s <- analyze.storage("CAL", "2026-07-01T00", "2026-07-14T23")
  expect_equal(nrow(s), 24L)
  expect_equal(sort(s$hour), 0:23)
  expect_true(all(s$charge_mw >= 0))
  expect_true(all(s$discharge_mw >= 0))
  # An hour is one or the other, never both.
  expect_true(all(s$charge_mw == 0 | s$discharge_mw == 0))
  expect_equal(s$net_mw, s$discharge_mw - s$charge_mw)
})

test_that("the daily cycle matches how batteries are actually operated", {
  skip_if_no_key()

  s <- analyze.storage("CAL", "2026-07-01T00", "2026-07-14T23")
  charge_hour    <- s$hour[which.max(s$charge_mw)]
  discharge_hour <- s$hour[which.max(s$discharge_mw)]

  # Absorb surplus solar around midday, return it on the evening ramp.
  expect_true(charge_hour >= 8 && charge_hour <= 16)
  expect_true(discharge_hour >= 17 && discharge_hour <= 22)
  expect_gt(discharge_hour, charge_hour)
})

test_that("round-trip efficiency is derived, not assumed", {
  skip_if_no_key()

  s <- analyze.storage("CAL", "2026-07-01T00", "2026-07-14T23")
  rte <- attr(s, "essp_rte")

  # Physics: energy out is always less than energy in. The existing hand-built
  # scripts hardcoded 0.85; real data should land near it without being told.
  expect_gt(rte, 0.5)
  expect_lt(rte, 1.0)
  expect_gt(sum(s$charge_mw), sum(s$discharge_mw))
})

test_that("a respondent without a BAT series fails with guidance", {
  skip_if_no_key()
  # CISO folds battery output into OTH and reports no BAT series -- a silent
  # empty result here would look like "no batteries in California".
  expect_error(
    analyze.storage("CISO", "2026-07-01T00", "2026-07-02T23"),
    "regional respondent|No battery"
  )
})

test_that("chart.storagebands renders and validates input", {
  d <- tibble::tibble(hour = 0:23,
                      charge_mw = c(rep(0, 8), rep(100, 8), rep(0, 8)),
                      discharge_mw = c(rep(0, 18), rep(120, 6)))
  attr(d, "essp_rte") <- 0.84

  p <- chart.storagebands(d)
  expect_s3_class(p, "ggplot")
  expect_no_error(ggplot2::ggplot_build(p))

  expect_error(chart.storagebands(data.frame(hour = 1)), "charge_mw")
})
