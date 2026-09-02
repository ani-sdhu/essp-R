test_that("essp.analyze coerces measurement columns and derives year", {
  skip_if_no_key()

  g <- essp.gather("retail", state = "GA", years = 2020:2024, freq = "annual",
                   sector = "RES", verbose = FALSE)
  # EIA returns measurements as character; every analysis otherwise repeats
  # this coercion.
  expect_type(g$price, "character")

  a <- essp.analyze(g, value = "price")
  expect_type(a$price, "double")
  expect_true("year" %in% names(a))
  expect_equal(sort(a$year), 2020:2024)

  # Units survive as an attribute even though the companion columns are gone.
  expect_false(any(grepl("-units$", names(a))))
  expect_match(unlist(attr(a, "essp_units")["price"]), "kilowatt")
})

test_that("the default derive argument does not error", {
  skip_if_no_key()
  # match.arg() throws on a zero-length argument, which is the default.
  g <- essp.gather("retail", state = "GA", years = 2023, freq = "annual",
                   verbose = FALSE)
  expect_no_error(essp.analyze(g))
})

test_that("growth is computed within groups and in period order", {
  skip_if_no_key()

  g <- essp.gather("retail", state = c("GA", "OH"), years = 2020:2024,
                   freq = "annual", sector = "RES", verbose = FALSE)
  a <- essp.analyze(g, value = "price", by = "stateid", derive = "growth")

  # The first year of each state has no prior period.
  first <- a[!duplicated(a$stateid), ]
  expect_true(all(is.na(first$yoy)))
  expect_equal(sum(is.na(a$yoy)), 2L)
})

test_that("essp.analyze rejects data that did not come from essp.gather", {
  expect_error(essp.analyze(data.frame(a = 1)), "No measurement columns")
  expect_error(essp.analyze("not a frame"), "must be a data frame")
})

test_that("essp.table aggregates, shares, and totals", {
  df <- data.frame(fuel = c("Gas", "Coal", "Gas", "Wind"), mw = c(10, 5, 15, 8))
  t <- essp.table(df, "fuel", "mw")

  expect_equal(nrow(t), 4L)                        # 3 fuels plus a total
  expect_equal(t$mw[t$fuel == "Gas"], 25)          # aggregated across rows
  expect_equal(t$mw[t$fuel == "Total"], 38)
  expect_equal(t$share[t$fuel == "Total"], 100)
  expect_equal(t$fuel[1], "Gas")                   # sorted descending

  # Shares of the non-total rows sum to 100.
  body <- t[t$fuel != "Total", ]
  expect_equal(sum(body$share), 100, tolerance = 1e-8)
})

test_that("essp.table honours stat, sort, and total switches", {
  df <- data.frame(fuel = c("Gas", "Coal", "Gas"), mw = c(10, 5, 20))

  expect_equal(essp.table(df, "fuel", "mw", stat = "mean",
                          total = FALSE, share = FALSE)$mw,
               c(15, 5))
  expect_equal(essp.table(df, "fuel", "mw", stat = "n",
                          total = FALSE, share = FALSE)$mw,
               c(2, 1))

  plain <- essp.table(df, "fuel", "mw", total = FALSE, share = FALSE)
  expect_equal(nrow(plain), 2L)
  expect_false("share" %in% names(plain))
})

test_that("export writes csv and latex, and refuses unknown formats", {
  df <- data.frame(fuel = c("Gas", "Coal"), mw = c(10.456, 5.123))

  csv <- withr::local_tempfile(fileext = ".csv")
  essp.export(df, csv, digits = 1)
  back <- utils::read.csv(csv)
  expect_equal(back$mw, c(10.5, 5.1))

  tex <- withr::local_tempfile(fileext = ".tex")
  essp.export(df, tex)
  lines <- readLines(tex)
  expect_true(any(grepl("\\\\begin\\{tabular\\}", lines)))
  expect_true(any(grepl("\\\\toprule", lines)))
  expect_true(any(grepl("Gas", lines)))

  bad <- withr::local_tempfile(fileext = ".docx")
  expect_error(essp.export(df, bad), "Unsupported format")
})

test_that("latex export escapes characters that would break the build", {
  df <- data.frame(label = "Cost & Fees 50%", v = 1)
  tex <- withr::local_tempfile(fileext = ".tex")
  essp.export(df, tex)
  lines <- readLines(tex)
  # A bare & or % in a LaTeX table is a compile error, not a typo.
  expect_true(any(grepl("Cost \\\\& Fees 50\\\\%", lines)))
})

test_that("emissions returns rates and tonnage", {
  skip_if_no_key()

  e <- analyze.emissions("GA", 2023)
  expect_gt(nrow(e), 0L)
  expect_true(any(grepl("^co2", names(e))))

  all_fuels <- e[e$fuelid == "ALL", ]
  expect_equal(nrow(all_fuels), 1L)
  # Georgia's grid runs a bit under the US average of ~800 lbs CO2/MWh.
  expect_gt(all_fuels[["co2-rate-lbs-mwh"]], 300)
  expect_lt(all_fuels[["co2-rate-lbs-mwh"]], 1500)
})

test_that("a metric stacks several states with a state column", {
  skip_if_no_key()

  # analyze.compare() was removed; every metric now takes multiple states.
  cmp <- analyze.diversity("GA,WV,CA", 2024)
  expect_equal(nrow(cmp), 3L)
  expect_setequal(cmp$state, c("GA", "WV", "CA"))

  # Coal-dominated West Virginia is the most concentrated of the three.
  expect_equal(cmp$state[which.max(cmp$hhi)], "WV")
  expect_equal(cmp$state[which.max(cmp$effective_fuels)], "CA")
})

test_that("an invalid state code is rejected up front, not silently dropped", {
  # The parser validates geography before any fetch, so a bad code names
  # itself rather than returning a short table.
  expect_error(analyze.diversity("GA,ZZ", 2024), "Not a state code: ZZ")
})
