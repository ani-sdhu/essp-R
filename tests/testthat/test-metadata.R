# --- pure, no network ---------------------------------------------------------

test_that("period bounds are emitted in the route's own format", {
  # The API compares period stamps lexically, so bounds must match the route's
  # granularity. A day-precision bound on a monthly series drops January.
  expect_equal(essp_period_range(2023, "annual"),
               list(start = "2023", end = "2023"))
  expect_equal(essp_period_range(2023, "monthly"),
               list(start = "2023-01", end = "2023-12"))
  expect_equal(essp_period_range(2023, "quarterly"),
               list(start = "2023-Q1", end = "2023-Q4"))
  expect_equal(essp_period_range(2023, "hourly"),
               list(start = "2023-01-01T00", end = "2023-12-31T23"))
  expect_equal(essp_period_range(2023, "local-hourly"),
               list(start = "2023-01-01T00", end = "2023-12-31T23"))
})

test_that("period bounds span the full range of the years given", {
  r <- essp_period_range(2019:2024, "monthly")
  expect_equal(r$start, "2019-01")
  expect_equal(r$end, "2024-12")

  # Order must not matter -- only the range.
  expect_equal(essp_period_range(c(2024, 2019), "annual"),
               essp_period_range(2019:2024, "annual"))
})

test_that("a monthly start bound never lands mid-period", {
  # Regression guard for the off-by-one: "2023-01-01" would exclude 2023-01.
  r <- essp_period_range(2023, "monthly")
  expect_false(grepl("-01-01$", r$start))
  expect_match(r$start, "^[0-9]{4}-01$")
})

# --- live API -----------------------------------------------------------------

test_that("data columns are extracted from all three metadata shapes", {
  skip_if_no_key()

  # long: ids live in an `id` column
  expect_setequal(
    essp_data_columns("electricity/retail-sales"),
    c("revenue", "sales", "price", "customers")
  )

  # flat: a single column named "value.id"
  expect_equal(essp_data_columns("seds"), "value")

  # wide: the column NAMES are the ids. Reading $id here returns nothing, which
  # is what would silently produce valueless rows.
  wide <- essp_data_columns("electricity/operating-generator-capacity")
  expect_true("nameplate-capacity-mw" %in% wide)
  expect_gt(length(wide), 5L)
})

test_that("facet and frequency ids are read from live metadata", {
  skip_if_no_key()

  expect_setequal(essp_facet_ids("electricity/retail-sales"),
                  c("stateid", "sectorid"))
  expect_true("respondent" %in% essp_facet_ids("electricity/rto/fuel-type-data"))

  expect_true("annual" %in% essp_frequencies("electricity/retail-sales"))
  expect_true("hourly" %in% essp_frequencies("electricity/rto/fuel-type-data"))
})

test_that("the mapped facet names still match what the API reports", {
  skip_if_no_key()
  # If EIA renames a facet, this fails instead of queries silently returning
  # nothing.
  for (tp in c("retail", "generation", "plants", "generators", "hourly")) {
    row <- essp.topics(tp)
    live <- essp_facet_ids(row$route)
    expect_true(row$geo_facet %in% live,
                info = paste0(tp, ": geo facet '", row$geo_facet,
                              "' not among ", paste(live, collapse = ", ")))
  }
})
