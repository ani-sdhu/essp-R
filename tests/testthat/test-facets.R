# EIA has no shared facet vocabulary across routes. These tests pin down the
# actual spellings, verified against the live API on 2026-08-26, so that a
# silent EIA rename shows up as a test failure rather than as empty results.

test_that("the geography facet is translated per route", {
  expect_equal(essp.facet("generators", "geo"),  "stateid")
  expect_equal(essp.facet("retail", "geo"),      "stateid")
  expect_equal(essp.facet("generation", "geo"),  "location")
  expect_equal(essp.facet("plants", "geo"),      "state")
  expect_equal(essp.facet("consumption", "geo"), "stateId")
  expect_equal(essp.facet("gasprices", "geo"),   "duoarea")
  expect_equal(essp.facet("international", "geo"), "countryRegionId")

  # The hourly routes are keyed by balancing authority, not state.
  expect_equal(essp.facet("hourly", "geo"), "respondent")
  expect_equal(essp.facet("demand", "geo"), "respondent")
})

test_that("sibling routes under one parent disagree on capitalization", {
  # Both live under electricity/state-electricity-profiles yet differ. This is
  # the sharpest evidence that the translation table cannot be replaced by a
  # naming convention.
  expect_equal(essp.facet("emissions", "geo"),  "stateid")
  expect_equal(essp.facet("capability", "geo"), "stateId")
  expect_false(essp.facet("emissions", "geo") == essp.facet("capability", "geo"))
})

test_that("the fuel facet is translated per route", {
  expect_equal(essp.facet("generation", "fuel"),  "fueltypeid")
  expect_equal(essp.facet("plants", "fuel"),      "fuelType")
  expect_equal(essp.facet("hourly", "fuel"),      "fueltype")
  expect_equal(essp.facet("generators", "fuel"),  "energy_source_code")
  expect_equal(essp.facet("emissions", "fuel"),   "fuelid")
})

test_that("a route with no facet for a role returns NA, not an error", {
  expect_true(is.na(essp.facet("plants", "sector")))
  expect_true(is.na(essp.facet("demand", "fuel")))
  expect_true(is.na(essp.facet("shortterm", "geo")))
})

test_that("an ambiguous form number will not silently pick a facet", {
  # 923 spans three routes whose geo facets are location / state / stateid.
  expect_error(essp.facet("923", "geo"), "ambiguous")
})

test_that("prime mover is available for the NGCC/NGCT split", {
  # The demand curve's baseload-vs-peaker distinction needs prime mover, which
  # only the generator inventory carries.
  gen <- essp.topics("generators")
  expect_true(grepl("prime mover", gen$description, ignore.case = TRUE))
})
