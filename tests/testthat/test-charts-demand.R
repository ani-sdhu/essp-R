mock_shape <- function() {
  hrs <- 0:23
  expand <- function(f, name, v) {
    data.frame(hour = hrs, fueltype = f, fuel_name = name, mean_mw = v,
               stringsAsFactors = FALSE)
  }
  rbind(
    expand("NUC", "Nuclear",     rep(2200, 24)),
    expand("NG",  "Natural Gas", 12000 + 4000 * sin((hrs - 6) / 24 * 2 * pi)),
    expand("SUN", "Solar",       pmax(0, 9000 * sin((hrs - 6) / 12 * pi))),
    expand("WND", "Wind",        2000 + 800 * cos(hrs / 24 * 2 * pi)),
    expand("WAT", "Hydro",       rep(1500, 24))
  )
}

test_that("chart.demandcurve builds and renders", {
  p <- chart.demandcurve(mock_shape())
  expect_s3_class(p, "ggplot")
  expect_no_error(ggplot2::ggplot_build(p))
})

test_that("missing columns are named", {
  expect_error(chart.demandcurve(data.frame(hour = 1)), "fueltype")
})

test_that("each label sits on the band it names", {
  # The original implementation computed midpoints in a different order than
  # ggplot stacked the areas, so "Solar" printed on the Natural Gas band.
  # Deriving bounds and label positions from one calculation is what fixes it.
  p <- chart.demandcurve(mock_shape(), smooth = FALSE, label_min = 1)
  b <- ggplot2::ggplot_build(p)

  ribbon <- b$data[[1]]
  text   <- Filter(function(d) "label" %in% names(d), b$data)[[1]]

  for (i in seq_len(nrow(text))) {
    x <- text$x[i]; y <- text$y[i]
    # The label's y must fall inside a band present at that x.
    near <- ribbon[abs(ribbon$x - x) < 0.6, ]
    inside <- any(y >= near$ymin - 1e-6 & y <= near$ymax + 1e-6)
    expect_true(inside, info = paste("label off its band:", text$label[i]))
  }
})

test_that("negative generation is clamped with a warning, not drawn below zero", {
  # Battery charging and net-metered solar come back negative. A stacked area
  # cannot express that; drawn raw it punches below the axis and displaces
  # every band above it.
  d <- mock_shape()
  d$mean_mw[d$fueltype == "SUN" & d$hour < 5] <- -40

  expect_warning(p <- chart.demandcurve(d, smooth = FALSE), "clamped to zero")

  ymin <- min(ggplot2::ggplot_build(p)$data[[1]]$ymin)
  expect_gte(ymin, 0)
})

test_that("annotations are optional and additive", {
  base <- chart.demandcurve(mock_shape())
  n_base <- length(ggplot2::ggplot_build(base)$data)

  full <- chart.demandcurve(
    mock_shape(),
    bands          = c(Baseload = 8000, Intermediate = 20000),
    reserve_margin = c(26000, 29000),
    mark_peak      = TRUE
  )
  expect_gt(length(ggplot2::ggplot_build(full)$data), n_base)
  expect_no_error(ggplot2::ggplot_build(full))
})

test_that("reserve_margin must be a pair", {
  expect_error(chart.demandcurve(mock_shape(), reserve_margin = 5000), "length 2")
})

test_that("smoothing changes resolution but not the peak much", {
  raw <- chart.demandcurve(mock_shape(), smooth = FALSE)
  sm  <- chart.demandcurve(mock_shape(), smooth = TRUE)

  raw_top <- max(ggplot2::ggplot_build(raw)$data[[1]]$ymax)
  sm_top  <- max(ggplot2::ggplot_build(sm)$data[[1]]$ymax)
  expect_equal(sm_top, raw_top, tolerance = 0.08)

  expect_gt(nrow(ggplot2::ggplot_build(sm)$data[[1]]),
            nrow(ggplot2::ggplot_build(raw)$data[[1]]))
})

test_that("duration curve and heatmap render", {
  dc <- data.frame(rank = 1:100, pct_hours = seq(1, 100),
                   mw = seq(500, 100, length.out = 100))
  expect_no_error(ggplot2::ggplot_build(chart.durationcurve(dc)))
  expect_no_error(ggplot2::ggplot_build(chart.durationcurve(dc, highlight_pct = 5)))
  expect_error(chart.durationcurve(data.frame(a = 1)), "pct_hours")

  hm <- expand.grid(hour = 0:23, month = month.abb[1:3])
  hm$mw <- runif(nrow(hm), 100, 900)
  expect_no_error(ggplot2::ggplot_build(chart.heatmap(hm, "hour", "month", "mw")))
})

test_that("palette and highlight compose rather than overriding each other", {
  d <- mock_shape()
  fills <- function(...) unique(ggplot2::ggplot_build(chart.demandcurve(d, ...))$data[[1]]$fill)

  # House alone: every fuel at its own full-saturation colour.
  house <- fills()
  expect_true(unname(essp.fuelcolors("NUC")) %in% house)
  expect_true(unname(essp.fuelcolors("SUN")) %in% house)

  # House plus a highlight: the named fuel keeps its colour, the others mute
  # but keep their hues, so the reader does not lose the colour coding.
  both <- fills(highlight = "SUN")
  expect_true(unname(essp.fuelcolors("SUN")) %in% both)
  expect_false(unname(essp.fuelcolors("NUC")) %in% both)
  expect_true(unname(essp.mute(essp.fuelcolors("NUC"))) %in% both)

  # The highlight palette is the other look entirely: flat greys, one accent.
  grey <- fills(highlight = "SUN", palette = "highlight")
  expect_true(essp.colors("ugared") %in% grey)
  expect_false(unname(essp.fuelcolors("SUN")) %in% grey)
})

test_that("muting keeps hue and lightness, drops saturation", {
  # Muting must not collapse colours onto each other, or the bands stop being
  # distinguishable at all.
  cols <- essp.fuelcolors(c("Nuclear", "Solar", "Wind", "Coal"))
  m <- essp.mute(cols)

  expect_length(m, length(cols))
  expect_true(all(grepl("^#[0-9A-Fa-f]{6}$", m)))
  expect_equal(length(unique(m)), length(unique(cols)))

  # Coal is #808080 -- already fully desaturated, so muting cannot take it
  # lower. The invariant is that nothing gains saturation and anything with
  # colour to lose, loses it.
  sat <- function(x) grDevices::rgb2hsv(grDevices::col2rgb(x))["s", ]
  expect_true(all(sat(m) <= sat(cols) + 1e-9))
  expect_true(all(sat(m)[sat(cols) > 0] < sat(cols)[sat(cols) > 0]))

  # amount = 1 is a no-op in saturation terms.
  expect_true(all(abs(sat(essp.mute(cols, 1)) - sat(cols)) < 1e-6))
})
