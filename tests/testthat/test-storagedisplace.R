shape <- function() {
  hrs <- 0:23
  rbind(
    data.frame(hour=hrs, fueltype="NUC", fuel_name="Nuclear", mean_mw=rep(2000,24)),
    data.frame(hour=hrs, fueltype="NGCC", fuel_name="NGCC", mean_mw=rep(8000,24)),
    data.frame(hour=hrs, fueltype="NG Peakers", fuel_name="NG Peakers",
               mean_mw=c(rep(0,17), rep(1500,5), 0, 0)),
    stringsAsFactors = FALSE)
}
stor <- function(peak = 1000) {
  hrs <- 0:23
  data.frame(hour = hrs,
             charge_mw = c(rep(0,9), rep(peak,6), rep(0,9)),
             discharge_mw = c(rep(0,18), rep(peak,4), 0, 0))
}

test_that("discharge displaces generation and conserves the total", {
  # Batteries substitute for other plant; they do not add supply on top of it.
  # If the total rises, the chart draws a taller peak than the grid served.
  d <- analyze.storagedisplace(shape(), stor())
  expect_equal(sum(d$mean_mw), sum(shape()$mean_mw), tolerance = 1e-6)
  expect_true("Storage" %in% d$fueltype)
})

test_that("peakers are displaced before combined cycle", {
  # Merit order: the most expensive plant comes off first.
  d <- analyze.storagedisplace(shape(), stor())
  pk <- d[d$fueltype == "NG Peakers" & d$hour == 19, "mean_mw"][[1]]
  cc <- d[d$fueltype == "NGCC" & d$hour == 19, "mean_mw"][[1]]
  expect_equal(pk, 500)    # 1500 peaker less 1000 discharge
  expect_equal(cc, 8000)   # combined cycle untouched
})

test_that("displacement spills to the next fuel once the first is exhausted", {
  d <- analyze.storagedisplace(shape(), stor(peak = 2000))
  pk <- d[d$fueltype == "NG Peakers" & d$hour == 19, "mean_mw"][[1]]
  cc <- d[d$fueltype == "NGCC" & d$hour == 19, "mean_mw"][[1]]
  expect_equal(pk, 0)      # peakers fully displaced
  expect_equal(cc, 7500)   # remainder taken from combined cycle
})

test_that("charging is carried separately, not stacked as generation", {
  # Charging is load. Putting it in the stack would overstate generation.
  d <- analyze.storagedisplace(shape(), stor())
  ch <- attr(d, "essp_charge")
  expect_false(is.null(ch))
  expect_equal(max(ch$charge_mw), 1000)
  expect_false(any(d$mean_mw < 0))
})

test_that("over-large discharge warns rather than going negative", {
  expect_warning(d <- analyze.storagedisplace(shape(), stor(peak = 50000)),
                 "exceeds displaceable")
  expect_true(all(d$mean_mw >= 0))
})

test_that("the chart accepts both palettes and draws charging below zero", {
  d <- analyze.storagedisplace(shape(), stor())

  house <- chart.demandcurve(d, palette = "house")
  expect_no_error(ggplot2::ggplot_build(house))
  # Charging must render below the axis.
  ylim <- ggplot2::ggplot_build(house)$layout$panel_params[[1]]$y.range
  expect_lt(min(ylim), 0)

  hl <- chart.demandcurve(d, palette = "highlight", highlight = "NGCC")
  expect_no_error(ggplot2::ggplot_build(hl))
})
