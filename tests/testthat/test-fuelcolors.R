test_that("house colors match the established figure palette", {
  # Taken from figure3_plot_storage.R, with Coal set to black per the
  # programme's standard. If any of these drift, a regenerated figure stops
  # matching the ones already in print.
  p <- essp.fuelcolors()
  expect_equal(unname(p["Nuclear"]), "#00B4D8")
  expect_equal(unname(p["Coal"]),    "#000000")
  expect_equal(unname(p["NGCC"]),    "#A8C5E2")
  expect_equal(unname(p["Solar"]),   "#70AD47")
  expect_equal(unname(p["Wind"]),    "#F4A743")
  expect_equal(unname(p["Hydro"]),   "#D6E6F4")
  expect_equal(unname(p["Storage"]), "#8E44AD")
  expect_equal(unname(p["NGCT"]),    "#3B6B9A")
})

test_that("EIA fuel codes resolve to the same house colors", {
  # A chart fed straight from essp.gather() must colour correctly without
  # renaming its fuel column first.
  expect_equal(unname(essp.fuelcolors("NUC")), "#00B4D8")
  expect_equal(unname(essp.fuelcolors("SUN")), "#70AD47")
  expect_equal(unname(essp.fuelcolors("BAT")), "#8E44AD")
  expect_equal(unname(essp.fuelcolors("NG")),  essp.fuelcolors("NGCC")[[1]])
})

test_that("unknown fuels fall back to Other rather than NA", {
  # An NA fill silently drops a band out of a stacked chart.
  out <- essp.fuelcolors(c("NUC", "ZZZ"))
  expect_equal(unname(out["ZZZ"]), "#C8C8C8")
  expect_false(any(is.na(out)))
  expect_equal(names(out), c("NUC", "ZZZ"))
})

test_that("every colour is a valid hex triplet", {
  expect_true(all(grepl("^#[0-9A-Fa-f]{6}$", essp.fuelcolors())))
})
