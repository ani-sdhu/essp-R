mock_mix <- function() {
  tibble::tibble(
    metric   = rep(c("Capacity", "Generation"), each = 7),
    resource = rep(c("Natural Gas", "Coal", "Wind", "Solar",
                     "Nuclear", "Hydro", "Other"), 2),
    value    = c(561783, 188382, 153135, 124299, 101789, 96032, 75865,
                 1869902, 652156, 451904, 219834, 781865, 242896, 90077),
    units    = rep(c("MW", "thousand megawatthours"), each = 7),
    share    = c(43.17, 14.48, 11.77, 9.55, 7.82, 7.38, 5.83,
                 43.40, 15.14, 10.49, 5.10, 18.15, 5.64, 2.09)
  )
}

test_that("chart.fleetmakeup builds a plot from analyze.mix output", {
  p <- chart.fleetmakeup(mock_mix(), highlight = "Solar")
  expect_s3_class(p, "ggplot")
  # It must render, not merely construct -- most errors surface at build time.
  expect_no_error(ggplot2::ggplot_build(p))
})

test_that("missing columns are reported by name", {
  expect_error(chart.fleetmakeup(data.frame(a = 1)), "metric")
  expect_error(chart.fleetmakeup(data.frame(metric = "Capacity")), "resource")
})

test_that("the highlighted resource gets the accent, others stay grey", {
  p <- chart.fleetmakeup(mock_mix(), highlight = "Solar", accent = "#BA0C2F")
  fills <- unique(ggplot2::ggplot_build(p)$data[[1]]$fill)
  expect_true("#BA0C2F" %in% fills)

  greys <- setdiff(fills, "#BA0C2F")
  for (col in greys) {
    rgb <- grDevices::col2rgb(col)
    expect_equal(rgb[1], rgb[2])
  }
})

test_that("label layout is decided from the resource's smallest segment", {
  # Solar is 9.55% of capacity but only 5.10% of generation. The smaller
  # segment cannot hold two stacked lines, so BOTH bars use the single-line
  # form -- a resource that switched layout between columns would read as an
  # inconsistency rather than a size cue.
  p <- chart.fleetmakeup(mock_mix(), highlight = "Solar", label_min = 3)
  labs <- unlist(lapply(ggplot2::ggplot_build(p)$data, function(d) d$label))
  labs <- labs[!is.na(labs)]

  expect_true(any(grepl("Solar  9.6%", labs, fixed = TRUE)))
  expect_true(any(grepl("Solar  5.1%", labs, fixed = TRUE)))
  expect_false(any(grepl("\n", labs, fixed = TRUE)))
})

test_that("two lines are used when both segments are roomy", {
  # Nuclear is 7.82% of capacity and 18.15% of generation; the smaller of the
  # two clears the two-line threshold, so both stack.
  p <- chart.fleetmakeup(mock_mix(), highlight = "Nuclear", label_min = 3)
  labs <- unlist(lapply(ggplot2::ggplot_build(p)$data, function(d) d$label))
  labs <- labs[!is.na(labs)]

  expect_true(any(grepl("Nuclear\n7.8%", labs, fixed = TRUE)))
  expect_true(any(grepl("Nuclear\n18.1%", labs, fixed = TRUE)))
})

test_that("segments too small to label are left unlabelled", {
  p <- chart.fleetmakeup(mock_mix(), highlight = "Solar", label_min = 6)
  labs <- unlist(lapply(ggplot2::ggplot_build(p)$data, function(d) d$label))
  labs <- labs[!is.na(labs) & nzchar(labs)]
  # Hydro generation is 5.64% and Other 2.09%; both fall below the threshold.
  expect_false(any(grepl("^Other", labs)))
})

test_that("every resource is labelled somewhere, since colour alone is grey", {
  # Identity in this chart comes from direct labels, not hue -- the grey steps
  # are deliberately close together. Each resource must appear in text.
  p <- chart.fleetmakeup(mock_mix(), highlight = "Solar", label_min = 3)
  labs <- unlist(lapply(ggplot2::ggplot_build(p)$data, function(d) d$label))
  labs <- paste(labs[!is.na(labs)], collapse = " ")

  for (r in c("Natural Gas", "Coal", "Wind", "Solar", "Nuclear", "Hydro")) {
    expect_true(grepl(r, labs, fixed = TRUE), info = paste("unlabelled:", r))
  }
})

# A PNG's pixel dimensions live in the IHDR chunk: after the 8-byte signature
# and an 8-byte chunk header, width and height are big-endian 4-byte integers.
# Reading them directly avoids a dependency just to assert image size.
png_dims <- function(path) {
  con <- file(path, "rb"); on.exit(close(con))
  readBin(con, "raw", n = 16)
  c(width  = readBin(con, "integer", n = 1, size = 4, endian = "big"),
    height = readBin(con, "integer", n = 1, size = 4, endian = "big"))
}

test_that("label colours follow their fill, one rule for all segments", {
  p <- chart.fleetmakeup(mock_mix(), highlight = "Solar", accent = "#BA0C2F")
  b <- ggplot2::ggplot_build(p)$data

  text_layers <- Filter(function(d) "label" %in% names(d), b)
  cols <- unlist(lapply(text_layers, function(d) d$colour))

  # Both colours are in play -- a single uniform colour would mean the rule
  # is not being applied.
  expect_setequal(unique(cols), c("#FFFFFF", "#1A1A1A"))

  # The accent is dark, so its label is white in every layer it appears in.
  hl <- do.call(rbind, lapply(text_layers, function(d) {
    d[grepl("Solar", d$label, fixed = TRUE), c("label", "colour")]
  }))
  expect_true(all(hl$colour == "#FFFFFF"))
})

test_that("essp.save writes both formats at the slot's dimensions", {
  dir <- withr::local_tempdir()
  p <- chart.fleetmakeup(mock_mix(), highlight = "Solar")
  out <- essp.save(p, file.path(dir, "fig.png"), slot = "profile")

  expect_length(out, 2L)
  expect_true(all(file.exists(out)))
  expect_true(all(file.size(out) > 0))

  # The profile slot is 3.2 x 4.2 inches at 300 dpi -- the size the template's
  # \includegraphics reserves, so a regenerated figure lands in the same space.
  d <- png_dims(out[grepl("\\.png$", out)])
  expect_equal(unname(d["width"]),  3.2 * 300, tolerance = 2)
  expect_equal(unname(d["height"]), 4.2 * 300, tolerance = 2)
})

test_that("essp.save needs either a slot or explicit dimensions", {
  dir <- withr::local_tempdir()
  p <- chart.fleetmakeup(mock_mix(), highlight = "Solar")
  expect_error(essp.save(p, file.path(dir, "f.png")), "either a .slot.")

  out <- essp.save(p, file.path(dir, "f.png"), width = 4, height = 3,
                   formats = "png")
  expect_true(file.exists(out))
})

test_that("slot sizes match the existing scripts' ggsave calls exactly", {
  # These are the dimensions the templates reserve. A regenerated figure that
  # is even slightly off drops into its slot at the wrong scale.
  s <- essp.slots()
  dim <- function(n) unlist(s[s$slot == n, c("width", "height")], use.names = FALSE)

  expect_equal(dim("profile"), c(3.2, 4.2))   # figure1_capacity_generation
  expect_equal(dim("wide"),    c(11, 6))      # figure3 demand curves
  expect_equal(dim("concept"), c(10, 6))      # DemandCurve figure1/figure2
  expect_equal(dim("column"),  c(9, 5))       # wallet share time series
  expect_equal(dim("facet"),   c(8, 9))       # wallet share small multiples
})

test_that("the stack follows the house figure's dispatch order", {
  # Order taken from the house figure3 script:
  #   Nuclear, Coal, NGCC, Solar, Wind, Hydro, Storage, NGCT
  # An earlier revision placed Solar and Wind above NGCT; the house script is
  # the authority and puts simple-cycle peakers on top as plant of last resort.
  fuels <- c("NUC", "COL", "NGCC", "NGCT", "SUN", "WND", "WAT", "Storage")
  hrs <- 0:23
  d <- do.call(rbind, lapply(fuels, function(f)
    data.frame(hour = hrs, fueltype = f, fuel_name = f,
               mean_mw = rep(1000, 24), stringsAsFactors = FALSE)))

  b <- ggplot2::ggplot_build(chart.demandcurve(d, palette = "house"))$data[[1]]
  b <- b[b$x == min(b$x), ]
  cols <- essp.fuelcolors(fuels)
  ord <- names(cols)[match(b$fill[order(b$ymin)], unname(cols))]
  pos <- stats::setNames(seq_along(ord), ord)

  # Baseload on the floor, peakers on top.
  expect_equal(ord[1], "NUC")
  expect_equal(ord[length(ord)], "NGCT")

  # Variable renewables sit above combined cycle, and storage above them.
  expect_gt(pos[["SUN"]], pos[["NGCC"]])
  expect_gt(pos[["WND"]], pos[["NGCC"]])
  expect_gt(pos[["Storage"]], pos[["WND"]])
  expect_gt(pos[["NGCT"]], pos[["Storage"]])
})
