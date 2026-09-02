test_that("prime-mover groups follow EIA's coding, not intuition", {
  g <- essp.gasgroups()
  codes <- stats::setNames(g$codes, g$group)

  # CT is the combustion-turbine PART of a combined-cycle plant, not a peaker.
  # Reading it as a peaker inverts the chart's entire story, so pin it down.
  expect_true(grepl("CT", codes[["NGCC"]]))
  expect_true(grepl("CA", codes[["NGCC"]]))
  expect_true(grepl("CS", codes[["NGCC"]]))

  # The actual peaking fleet is simple-cycle GT plus reciprocating engines.
  expect_true(grepl("GT", codes[["NG Peakers"]]))
  expect_true(grepl("IC", codes[["NG Peakers"]]))
  expect_false(grepl("CT", codes[["NG Peakers"]]))

  # No code may appear in two groups, or generation is double-counted.
  all_codes <- unlist(strsplit(paste(g$codes, collapse = ","), ",", fixed = TRUE))
  expect_false(any(duplicated(trimws(all_codes))))
  # EIA's "ALL" aggregate must never be included alongside its components.
  expect_false("ALL" %in% trimws(all_codes))
})

test_that("gas fleet capacity factors are physically plausible", {
  skip_if_no_key()

  fl <- analyze.gasfleet(ba = "CISO", year = 2024)
  cf <- stats::setNames(fl$capacity_factor, fl$group)

  # facility-fuel reports MEGAWATTHOURS while electric-power-operational-data
  # reports THOUSAND megawatthours. Assuming the wrong one puts these off by a
  # factor of 1,000 -- which is exactly what happened before the unit was read
  # from the response.
  expect_true(all(fl$capacity_factor > 0 & fl$capacity_factor < 100, na.rm = TRUE))
  expect_gt(cf[["NGCC"]], 30)          # combined cycle carries the load
  expect_lt(cf[["NG Peakers"]], 30)    # peakers run rarely
  expect_gt(cf[["NGCC"]], cf[["NG Peakers"]])

  expect_match(attr(fl, "essp_generation_units"), "megawatthour")
})

test_that("peakers hold more capacity share than generation share", {
  skip_if_no_key()

  # This asymmetry is the point of the figure: a large peaking fleet that
  # produces very little energy.
  fl <- analyze.gasfleet(ba = "CISO", year = 2024)
  pk <- fl[fl$group == "NG Peakers", ]
  expect_gt(pk$capacity_share, pk$generation_share)

  cc <- fl[fl$group == "NGCC", ]
  expect_lt(cc$capacity_share, cc$generation_share)
})

test_that("splitting the gas band conserves the total exactly", {
  skip_if_no_key()

  fs <- analyze.fuelshape("CISO", "2024-07-01T00", "2024-07-31T23")
  sp <- suppressWarnings(analyze.gassplit(fs, ba = "CISO", year = 2024))

  before <- sum(fs$mean_mw[fs$fueltype == "NG"])
  after  <- sum(sp$mean_mw[sp$fueltype %in% c("NGCC", "NG Steam", "NG Peakers")])
  expect_equal(after, before, tolerance = 1e-6)

  # The single NG series is replaced, not duplicated alongside its parts.
  expect_false("NG" %in% sp$fueltype)
  expect_true(all(c("NGCC", "NG Steam", "NG Peakers") %in% sp$fueltype))

  # Non-gas fuels pass through untouched.
  expect_equal(sum(sp$mean_mw[sp$fueltype == "SUN"]),
               sum(fs$mean_mw[fs$fueltype == "SUN"]))
})

test_that("dispatch order puts combined cycle first and peakers last", {
  skip_if_no_key()

  fs <- analyze.fuelshape("CISO", "2024-07-01T00", "2024-07-31T23")
  sp <- suppressWarnings(analyze.gassplit(fs, ba = "CISO", year = 2024))

  get <- function(g) sp$mean_mw[sp$fueltype == g][order(sp$hour[sp$fueltype == g])]
  cc <- get("NGCC"); pk <- get("NG Peakers")

  # Combined cycle runs in every hour; peakers only in some.
  expect_true(all(cc > 0))
  expect_true(any(pk == 0))
  expect_gt(sum(cc), sum(pk))

  # Peakers, when they run, run at the daily peak rather than overnight.
  if (any(pk > 0)) {
    ng_total <- get("NGCC") + get("NG Steam") + pk
    expect_gt(stats::cor(pk, ng_total), 0.3)
  }
})

test_that("gassplit rejects data with no gas rows", {
  d <- data.frame(hour = 0:23, fueltype = "SUN", mean_mw = 1)
  expect_error(analyze.gassplit(d, ba = "CISO", year = 2024), "No `NG` rows")
  expect_error(analyze.gassplit(data.frame(a = 1), ba = "CISO", year = 2024),
               "fueltype")
})
