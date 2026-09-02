# Routing behaviour of the essp.get() front door. The mapping and guards are
# offline; the equivalence-to-metric check needs a key.

test_that("catalog lists every routed topic", {
  cat <- essp.catalog()
  expect_true(all(c("what", "does", "where", "when") %in% names(cat)))
  expect_true(all(c("generation", "mix", "fuelmix", "demand", "fuelleaders") %in% cat$what))
})

test_that("an unknown topic names the valid options", {
  expect_error(essp.get("wattage", "US", "2024"), "Unknown topic")
})

test_that("geography is validated up front", {
  # A state topic given a BA code says so before any fetch.
  expect_error(essp.get("mix", "CISO", "2024"), "Not a state code")
  # An hourly topic with no time window is caught early.
  expect_error(essp.get("fuelmix", "CISO"), "needs a time window")
})

test_that("get forwards to the metric unchanged", {
  skip_if_no_key()

  g <- essp.get("generation", "US", "2024")
  d <- analyze.generation("US", "2024")
  expect_equal(g$share, d$share)
})

test_that("get stacks multiple states", {
  skip_if_no_key()

  ms <- essp.get("mix", "GA,MD", "2024")
  expect_true(all(c("state", "year") %in% names(ms)))
  expect_setequal(unique(ms$state), c("GA", "MD"))
})
