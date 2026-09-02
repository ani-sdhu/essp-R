test_that("brand colors match the LaTeX preamble", {
  # These are \definecolor values from the briefs' preamble; charts and page
  # furniture must not drift apart.
  expect_equal(unname(essp.colors("ugared")), "#BA0C2F")     # RGB 186,12,47
  expect_equal(unname(essp.colors("ugagray")), "#808080")    # RGB 128,128,128
  expect_equal(unname(essp.colors("bannergray")), "#554F47") # RGB 85,79,71
  expect_equal(unname(essp.colors("stripgray")), "#9EA2A2")  # RGB 158,162,162

  expect_error(essp.colors("nope"), "Unknown color")
  expect_gt(length(essp.colors()), 5L)
})

test_that("semester accents rotate and are stable", {
  # Each semester-year pair gets a deterministic accent, so a cohort's figures
  # stay consistent across re-renders.
  a <- essp.accent("Fall", 2026)
  expect_equal(a, essp.accent("fall", 2026))       # case-insensitive
  expect_equal(a, essp.accent("Fall", 2026))       # deterministic

  # Consecutive semesters differ.
  expect_false(essp.accent("Spring", 2026) == essp.accent("Summer", 2026))
  expect_false(essp.accent("Summer", 2026) == essp.accent("Fall", 2026))
  # And the same semester differs year on year.
  expect_false(essp.accent("Fall", 2026) == essp.accent("Fall", 2027))

  expect_error(essp.accent("Winter", 2026), "must be one of")
})

test_that("the categorical palette refuses to exceed its validated size", {
  # Validated with a colorblind-safety checker: a fifth hue drops CVD
  # separation to 4.7 for protanopes, below the floor of 6. Silently returning
  # a fifth color would produce charts two readers in a hundred cannot decode.
  expect_length(essp.palette("categorical", 4), 4L)
  expect_length(essp.palette("categorical", 1), 1L)
  expect_length(essp.palette("categorical", 7), 7L)

  # The ceiling is a real limit, not an arbitrary one: an eighth hue cannot be
  # separated from the existing seven for colour-blind readers.
  expect_error(essp.palette("categorical", 8), "cannot be separated")

  # Hues are assigned in a fixed order and never cycled, so a chart that gains
  # or loses a series does not repaint the survivors.
  expect_equal(essp.palette("categorical", 3),
               essp.palette("categorical", 7)[1:3])
})

test_that("grey and sequential ramps are ordered light to dark", {
  g <- essp.palette("grey", 5)
  expect_length(g, 5L)
  expect_false(any(duplicated(g)))

  s <- essp.palette("sequential", 5)
  expect_length(s, 5L)
  # A sequential ramp is one hue varying in lightness, never a rainbow.
  lum <- vapply(s, function(h) sum(grDevices::col2rgb(h)), numeric(1))
  expect_true(all(diff(lum) < 0))
})

test_that("a highlighted resource keeps its own colour, others go grey", {
  res <- c("Natural Gas", "Coal", "Wind", "Solar", "Nuclear", "Hydro", "Other")
  h <- essp.highlight(res, highlight = "Solar")

  expect_named(h, res)

  # A resource carries the same colour wherever it appears, so highlighting
  # Solar on the fleet figure paints it the green it has on the demand curve
  # rather than a generic accent.
  expect_equal(unname(h["Solar"]), unname(essp.fuelcolors("Solar")))
  expect_equal(unname(essp.highlight(res, "Wind")["Wind"]),
               unname(essp.fuelcolors("Wind")))

  # Every other entry is a neutral grey: R == G == B.
  others <- h[names(h) != "Solar"]
  for (col in others) {
    rgb <- grDevices::col2rgb(col)
    expect_equal(rgb[1], rgb[2])
    expect_equal(rgb[2], rgb[3])
  }
  expect_false(any(duplicated(others)))
})

test_that("a highlighted resource stays distinguishable from the grey ramp", {
  res <- c("Natural Gas", "Coal", "Wind", "Solar", "Nuclear", "Hydro")

  # Coal is black precisely so it still reads when highlighted; a mid-grey
  # would have disappeared into the greyed-out bands around it.
  h <- essp.highlight(res, highlight = "Coal")
  expect_equal(unname(h["Coal"]), "#000000")
  expect_false(unname(h["Coal"]) %in% unname(h[names(h) != "Coal"]))

  lum <- function(x) sum(grDevices::col2rgb(x) * c(0.2126, 0.7152, 0.0722))
  greys <- vapply(unname(h[names(h) != "Coal"]), lum, numeric(1))
  expect_gt(min(abs(greys - lum("#000000"))), 40)
})

test_that("a resource with no house colour falls back to the accent", {
  h <- essp.highlight(c("Widgets", "Sprockets"), highlight = "Widgets")
  expect_equal(unname(h["Widgets"]), unname(essp.colors("ugared")))
})

test_that("highlight accepts a custom accent and rejects unknown resources", {
  res <- c("Coal", "Solar")
  expect_equal(unname(essp.highlight(res, "Coal", accent = "#123456")["Coal"]),
               "#123456")
  expect_error(essp.highlight(res, highlight = "Wind"), "not among resources")

  # No highlight is legitimate -- an all-grey context chart.
  expect_no_error(essp.highlight(res))
})

test_that("text colour is chosen by contrast, not by role", {
  # Dark fills take white text, light fills dark text -- one rule for every
  # segment. Hardcoding white on the accent and black elsewhere would flip the
  # accent's text colour as the semester rotates, and would leave dark grey
  # segments carrying black text they cannot support.
  expect_equal(essp.textcolor("#000000"), "#FFFFFF")
  expect_equal(essp.textcolor("#FFFFFF"), "#1A1A1A")

  # Every semester accent is dark enough to need white text.
  for (a in c("#BA0C2F", "#2E7D8F", "#4A7C2F", "#7A4E9E")) {
    expect_equal(essp.textcolor(a), "#FFFFFF", info = a)
  }

  # The grey ramp crosses the threshold, so it must not be uniform.
  greys <- essp.palette("grey", 7)
  chosen <- essp.textcolor(greys)
  expect_length(chosen, 7L)
  expect_gt(length(unique(chosen)), 1L)
  # Darkest grey gets white, lightest gets dark.
  expect_equal(chosen[1], "#FFFFFF")
  expect_equal(chosen[7], "#1A1A1A")
})

test_that("the chosen text colour actually wins on contrast ratio", {
  ratio <- function(a, b) {
    lum <- function(col) {
      v <- grDevices::col2rgb(col)[, 1] / 255
      v <- ifelse(v <= 0.03928, v / 12.92, ((v + 0.055) / 1.055)^2.4)
      sum(c(0.2126, 0.7152, 0.0722) * v)
    }
    l <- sort(c(lum(a), lum(b)), decreasing = TRUE)
    (l[1] + 0.05) / (l[2] + 0.05)
  }

  for (fill in c(essp.palette("grey", 7), "#BA0C2F", "#4A7C2F")) {
    picked <- essp.textcolor(fill)
    other  <- if (picked == "#FFFFFF") "#1A1A1A" else "#FFFFFF"
    expect_gte(ratio(fill, picked), ratio(fill, other), label = fill)
  }
})

test_that("the theme carries its semester accent", {
  th <- essp.theme("Fall", 2026)
  expect_s3_class(th, "theme")
  expect_equal(attr(th, "essp_accent"), essp.accent("Fall", 2026))
  expect_equal(attr(th, "essp_semester"), "Fall 2026")
})

test_that("slot dimensions match the template's figure slots", {
  s <- essp.slots()
  expect_setequal(s$slot, c("profile", "wide", "concept", "column", "facet"))

  # Taken from the ggsave() calls in the existing per-folder scripts.
  expect_equal(essp.slots("profile")$width, 3.2)
  expect_equal(essp.slots("profile")$height, 4.2)
  expect_equal(essp.slots("wide")$width, 11)

  expect_error(essp.slots("banner"), "Unknown slot")
})
