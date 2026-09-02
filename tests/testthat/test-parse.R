# The parsers run entirely offline -- no API key, no network.

test_that("where splits, trims, and upper-cases states", {
  expect_identical(essp_parse_where("CA,MD,DE"), c("CA", "MD", "DE"))
  expect_identical(essp_parse_where(" ca , md "), c("CA", "MD"))
  expect_identical(essp_parse_where("US"), "US")
  expect_length(essp_parse_where("ALL"), 51L)          # 50 states + DC
})

test_that("where preserves fuel names and rejects bad states", {
  expect_identical(essp_parse_where("Solar", need = "fuel"), "Solar")
  expect_error(essp_parse_where("ZZ"), "Not a state code")
})

test_that("when reads a year, a list, and a range", {
  expect_identical(essp_parse_when("2024", "year")$years, 2024L)
  expect_identical(essp_parse_when("2023,2024", "year")$years, c(2023L, 2024L))
  expect_identical(essp_parse_when("2020-2024", "year")$years, 2020:2024)
  expect_identical(essp_parse_when(2024, "year")$years, 2024L)
  expect_error(essp_parse_when("soon", "year"), "Could not read a year")
})

test_that("when builds hourly windows for month, day, season, and year", {
  m <- essp_parse_when("2024-07", "window")
  expect_equal(c(m$start, m$end), c("2024-07-01T00", "2024-07-31T23"))

  expect_equal(essp_parse_when("July 2024", "window")$start, "2024-07-01T00")

  d <- essp_parse_when("2024-07-31", "window")
  expect_equal(c(d$start, d$end), c("2024-07-31T00", "2024-07-31T23"))

  s <- essp_parse_when("summer 2024", "window")
  expect_equal(c(s$start, s$end), c("2024-06-01T00", "2024-08-31T23"))

  # Winter wraps the year boundary.
  w <- essp_parse_when("winter 2024", "window")
  expect_equal(c(w$start, w$end), c("2024-12-01T00", "2025-02-28T23"))

  y <- essp_parse_when("2024", "window")
  expect_equal(c(y$start, y$end), c("2024-01-01T00", "2024-12-31T23"))

  expect_error(essp_parse_when("whenever", "window"), "Could not read a time window")
})

test_that("latest year is the prior calendar year", {
  expect_equal(essp_latest_year(), as.integer(format(Sys.Date(), "%Y")) - 1L)
})
