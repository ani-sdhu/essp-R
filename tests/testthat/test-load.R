test_that("respondents separate balancing authorities from regions", {
  skip_if_no_key()

  r <- essp.respondents()
  expect_true(all(c("id", "name", "is_region") %in% names(r)))

  reg <- essp.respondents("region")
  # These aggregates contain their member BAs; summing across everything would
  # double-count, which is why the distinction is exposed at all.
  expect_true(all(c("US48", "CAL", "TEX", "NE", "NY") %in% reg$id))
  expect_true(all(reg$is_region))

  ba <- essp.respondents("ba")
  expect_true("CISO" %in% ba$id)
  expect_false(any(ba$is_region))
  expect_equal(nrow(ba) + nrow(reg), nrow(r))
})

test_that("local-hourly bounds get a UTC offset appended", {
  skip_if_no_key()

  # Without an offset suffix EIA returns HTTP 500 for local-hourly, so this is
  # load-bearing rather than cosmetic.
  off <- essp_ba_offset("CISO")
  expect_match(off, "^[+-][0-9]{2}$")

  l <- analyze.load("CISO", "2024-07-01T00", "2024-07-02T23")
  expect_match(l$peak_period, "[+-][0-9]{2}$")
})

test_that("load statistics are internally consistent", {
  skip_if_no_key()

  l <- analyze.load("CISO", "2024-07-01T00", "2024-07-31T23")

  expect_equal(l$hours, 744L)                 # 31 days x 24 hours
  expect_gt(l$peak_mw, l$mean_mw)
  expect_gt(l$mean_mw, l$min_mw)
  expect_true(l$load_factor > 0 && l$load_factor < 100)
  expect_equal(l$load_factor, l$mean_mw / l$peak_mw * 100)
  expect_gt(l$max_ramp_up, 0)
  expect_lt(l$max_ramp_down, 0)
})

test_that("the load shape is in local time, not UTC", {
  skip_if_no_key()

  s <- analyze.loadshape("CISO", "2024-07-01T00", "2024-07-31T23")

  expect_equal(nrow(s), 24L)
  expect_equal(sort(s$hour), 0:23)
  expect_true(all(s$n == 31))
  expect_true(all(s$max_mw >= s$mean_mw & s$mean_mw >= s$min_mw))

  # California's summer peak is in the evening after solar falls off, and the
  # trough is early morning. Under UTC the peak would land ~7 hours adrift, so
  # this is the check that local-time handling actually works.
  peak_hour <- s$hour[which.max(s$mean_mw)]
  min_hour  <- s$hour[which.min(s$mean_mw)]
  expect_true(peak_hour >= 17 && peak_hour <= 22)
  expect_true(min_hour >= 2 && min_hour <= 8)
})

test_that("load shape can be split by month or season", {
  skip_if_no_key()

  m <- analyze.loadshape("CISO", "2024-06-01T00", "2024-07-31T23", by = "month")
  expect_true("month" %in% names(m))
  expect_setequal(unique(m$month), c("Jun", "Jul"))
  expect_equal(nrow(m), 48L)
})

test_that("the duration curve is monotonically decreasing", {
  skip_if_no_key()

  dc <- analyze.durationcurve("CISO", "2024-07-01T00", "2024-07-31T23")

  expect_equal(nrow(dc), 744L)
  expect_true(all(diff(dc$mw) <= 0))
  expect_equal(dc$mw[1], max(dc$mw))
  expect_equal(max(dc$pct_hours), 100)

  # The defining shape: peak demand is far above the median, which is why
  # peaking capacity runs so few hours.
  expect_gt(dc$mw[1], stats::median(dc$mw) * 1.2)
})

test_that("reserve margin compares installed capacity against observed peak", {
  skip_if_no_key()

  rm <- analyze.reservemargin("CISO", 2024)

  expect_gt(rm$capacity_mw, rm$peak_mw)
  expect_equal(rm$margin_mw, rm$capacity_mw - rm$peak_mw)
  expect_equal(rm$margin_pct, rm$margin_mw / rm$peak_mw * 100)
  # CISO's annual peak falls in late summer, not midwinter.
  expect_match(rm$peak_period, "^2024-0[6-9]")
})

test_that("interchange is signed and reported as import dependence", {
  skip_if_no_key()

  i <- analyze.imports("CISO", "2024-07-01T00", "2024-07-31T23")

  # California imports power in summer, so net interchange is negative and
  # import share positive.
  expect_lt(i$net_interchange, 0)
  expect_gt(i$import_share, 0)
  expect_equal(i$direction, "net importer")
  expect_equal(i$hours, 744L)
})
