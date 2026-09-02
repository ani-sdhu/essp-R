# The chart facades. Season/storage helpers and type dispatch are offline; the
# fetch-and-draw equivalence needs a key.

test_that("season months are meteorological and winter wraps", {
  expect_identical(as.integer(essp_season_months("summer")), 6:8)
  expect_identical(as.integer(essp_season_months("fall")),   9:11)
  expect_identical(as.integer(essp_season_months("spring")), 3:5)
  expect_setequal(as.integer(essp_season_months("winter")), c(1L, 2L, 12L))
  expect_identical(as.integer(essp_season_months("annual")), 1:12)
  expect_error(essp_season_months("monsoon"), "Unknown season")
})

test_that("winter splits into two month blocks, contiguous seasons into one", {
  expect_length(essp_month_blocks(c(12, 1, 2)), 2L)
  expect_length(essp_month_blocks(6:8), 1L)
})

test_that("storage respondent maps known BAs and passes others through", {
  expect_equal(essp_storage_respondent("CISO"), "CAL")
  expect_equal(essp_storage_respondent("ciso"), "CAL")
  expect_equal(essp_storage_respondent("MISO"), "MISO")  # unmapped -> itself
})

test_that("chart.demandcurve still takes a data frame (engine path)", {
  # A data frame first argument must not be diverted to the string facade.
  hrs <- 0:23
  d <- rbind(
    data.frame(hour = hrs, fueltype = "NUC", fuel_name = "Nuclear", mean_mw = 2000),
    data.frame(hour = hrs, fueltype = "SUN", fuel_name = "Solar",
               mean_mw = pmax(0, 9000 * sin((hrs - 6) / 12 * pi)))
  )
  p <- chart.demandcurve(d, smooth = FALSE)
  expect_s3_class(p, "ggplot")
})

test_that("chart.generation equals the fleetmakeup data path", {
  skip_if_no_key()

  a <- chart.generation("Solar", "US", "2024")
  b <- chart.fleetmakeup(analyze.mix("US", 2024), highlight = "Solar")
  expect_equal(
    ggplot2::ggplot_build(a)$data[[1]]$fill,
    ggplot2::ggplot_build(b)$data[[1]]$fill)
})
