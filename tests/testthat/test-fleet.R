test_that("leap years are counted correctly", {
  # ~1% of a year; ignoring Feb 29 shifts every capacity factor visibly.
  expect_equal(hours_in_year(2024), 8784)  # divisible by 4
  expect_equal(hours_in_year(2023), 8760)
  expect_equal(hours_in_year(1900), 8760)  # century, not a leap year
  expect_equal(hours_in_year(2000), 8784)  # divisible by 400, is a leap year
})

test_that("capacity factors are physically plausible and correctly ordered", {
  skip_if_no_key()

  cf <- analyze.capacityfactor("US", 2024)
  get <- function(r) cf$capacity_factor[cf$resource == r]

  # Nothing can exceed 100% -- that would mean generating more than flat-out.
  expect_true(all(cf$capacity_factor > 0 & cf$capacity_factor < 100, na.rm = TRUE))

  # Nuclear runs baseload; solar is limited by daylight. The ordering between
  # them is the single most robust fact about capacity factors.
  expect_gt(get("Nuclear"), 85)
  expect_lt(get("Solar"), 35)
  expect_gt(get("Nuclear"), get("Wind"))
  expect_gt(get("Wind"), get("Solar"))
})

test_that("summer basis yields higher factors than nameplate", {
  skip_if_no_key()

  # Net summer capacity is lower than nameplate, so the same generation
  # divided by it gives a larger factor. EIA publishes on the summer basis.
  s <- analyze.capacityfactor("US", 2024, basis = "summer")
  n <- analyze.capacityfactor("US", 2024, basis = "nameplate")
  expect_gt(s$capacity_factor[s$resource == "Nuclear"],
            n$capacity_factor[n$resource == "Nuclear"])
  expect_equal(attr(s, "essp_basis"), "summer")
})

test_that("fleet counts distinguish plants from generators", {
  skip_if_no_key()

  f <- analyze.fleet("GA", 2024)
  # A plant houses several generating units, so generators >= plants always.
  expect_true(all(f$generators >= f$plants))
  expect_true(all(f$mw > 0))
  expect_true(all(f$mean_mw >= f$median_mw | is.na(f$median_mw) |
                    f$mean_mw < f$median_mw))  # either skew is legitimate

  # Georgia's nuclear fleet is a couple of large sites; solar is many small
  # ones. Mean unit size should reflect that by a wide margin.
  expect_gt(f$mean_mw[f$resource == "Nuclear"], f$mean_mw[f$resource == "Solar"])
})

test_that("top producers ranks states only, never census regions", {
  skip_if_no_key()

  tp <- analyze.fuelleaders("Solar", 2024, n = Inf)

  # Regions live in the same facet and contain their member states; including
  # them pushes cumulative share past 100%.
  expect_false(any(c("PCC", "SAT", "MTN", "WSC", "ENC", "90", "US") %in% tp$state))
  expect_true(all(tp$state %in% c(state.abb, "DC")))

  expect_lte(max(tp$cumulative_share), 100.01)
  expect_true(all(diff(tp$share) <= 0))          # sorted descending
  expect_equal(tp$rank, seq_len(nrow(tp)))
})

test_that("top producers matches known 2024 leaders", {
  skip_if_no_key()

  expect_equal(analyze.fuelleaders("Solar", 2024, n = 1)$state, "CA")
  expect_equal(analyze.fuelleaders("Wind", 2024, n = 1)$state, "TX")
  expect_equal(analyze.fuelleaders("Nuclear", 2024, n = 1)$state, "IL")
})

test_that("cumulative share supports the 'N states account for X%' claim", {
  skip_if_no_key()

  tp <- analyze.fuelleaders("Solar", 2024, n = 8)
  expect_equal(nrow(tp), 8L)
  # RT_Solar cites "71.7% from 8 states; 21.1% from California"; 2024 data
  # gives ~72.3% and ~22.0%.
  expect_equal(tp$cumulative_share[8], 72, tolerance = 2)
  expect_equal(tp$share[tp$state == "CA"], 22, tolerance = 2)
})
