test_that("an ambiguous form number is refused rather than guessed", {
  # 923 spans three routes; picking one silently would be worse than failing.
  expect_error(essp.gather("923"), "maps to 3 routes")
})

test_that("asking for a facet a route lacks fails with the real facet list", {
  skip_if_no_key()
  # facility-fuel has no sector facet.
  expect_error(essp.gather("plants", sector = "RES"), "no sector facet")
})

test_that("an unavailable frequency is refused with the available ones", {
  skip_if_no_key()
  # operating-generator-capacity is monthly only.
  expect_error(
    essp.gather("generators", freq = "annual"),
    "not available"
  )
})

test_that("measurement columns come back populated, not just labels", {
  skip_if_no_key()

  x <- essp.gather("retail", state = "GA", years = 2023, freq = "annual",
                   verbose = FALSE)
  prov <- essp.provenance(x)

  expect_gt(nrow(x), 0L)
  expect_setequal(prov$columns, c("revenue", "sales", "price", "customers"))

  # The core guard: every requested column is present AND not entirely NA.
  # Without data[], EIA returns period/facet labels only -- structurally fine,
  # completely empty.
  for (cl in prov$columns) {
    expect_true(cl %in% names(x), info = paste("missing column:", cl))
    expect_false(all(is.na(x[[cl]])), info = paste("all-NA column:", cl))
  }
})

test_that("the first period of a range is not dropped", {
  skip_if_no_key()
  m <- essp.gather("retail", state = "GA", years = 2023, freq = "monthly",
                   verbose = FALSE)
  expect_true(any(grepl("2023-01", m$period)))
  expect_true(any(grepl("2023-12", m$period)))
  expect_equal(length(unique(m$period)), 12L)
})

test_that("results beyond the 5,000-row cap are paged in full", {
  skip_if_no_key()

  h <- essp.gather("hourly", ba = "CISO",
                   start = "2024-01-01T00", end = "2024-03-31T23",
                   verbose = FALSE)
  prov <- essp.provenance(h)

  expect_gt(nrow(h), 5000L)
  # total is NA on a cache hit, so only assert it when it was recoverable.
  if (!is.na(prov$total)) expect_equal(nrow(h), prov$total)
  # Paging must not double-count across page boundaries.
  expect_equal(anyDuplicated(h[, c("period", "fueltype")]), 0L)
})

test_that("provenance records the request that produced the data", {
  skip_if_no_key()

  x <- essp.gather("retail", state = "GA", years = 2023, freq = "annual",
                   verbose = FALSE)
  p <- essp.provenance(x)

  expect_equal(p$route, "electricity/retail-sales")
  expect_equal(p$topic, "retail")
  expect_equal(p$frequency, "annual")
  expect_equal(p$facets$stateid, "GA")
  expect_equal(p$rows, nrow(x))
  expect_s3_class(p$fetched, "POSIXct")
})

test_that("the wide metadata shape still yields populated values", {
  skip_if_no_key()

  # This query exceeds max_rows on purpose, so truncation warnings are the
  # expected behavior rather than noise.
  g <- suppressWarnings(
    essp.gather("generators", state = "GA", years = 2024, freq = "monthly",
                max_rows = 5000, verbose = FALSE)
  )
  expect_gt(nrow(g), 0L)
  expect_true("nameplate-capacity-mw" %in% names(g))
  expect_false(all(is.na(g[["nameplate-capacity-mw"]])))
})

test_that("exceeding max_rows warns exactly once, reporting the true size", {
  skip_if_no_key()

  w <- character()
  g <- withCallingHandlers(
    essp.gather("generators", state = "GA", years = 2024, freq = "monthly",
                max_rows = 5000, verbose = FALSE),
    warning = function(x) {
      w <<- c(w, conditionMessage(x))
      invokeRestart("muffleWarning")
    }
  )

  # Exactly one warning, not two. Which one depends on cache state: eia_data()
  # reports the total only in a warning, and does not re-emit it on a cache
  # hit, so the up-front "rows available" message is best-effort and the
  # "stopped at" message is the fallback.
  expect_length(w, 1L)
  expect_match(w, "truncated|Stopped at")
  expect_lte(nrow(g), 10000L)
})

test_that("a filter matching nothing returns zero rows, not an error", {
  skip_if_no_key()

  # eia_data() throws "No data available - check temporal inputs" here, which
  # misattributes the cause: the period is fine, the prime mover code is not
  # one Georgia has. Zero rows is a legitimate answer and must stay composable.
  expect_warning(
    empty <- essp.gather("generators", state = "GA", years = 2024,
                         freq = "monthly",
                         facets = list(prime_mover_code = "NB"),
                         verbose = FALSE),
    "No data returned"
  )
  expect_equal(nrow(empty), 0L)
  # Provenance survives so the failed request is still inspectable.
  expect_equal(essp.provenance(empty)$facets$prime_mover_code, "NB")
})

test_that("prime mover is filterable, enabling the NGCC/NGCT split", {
  skip_if_no_key()

  ct <- essp.gather("generators", state = "GA", years = 2024, freq = "monthly",
                    facets = list(prime_mover_code = "CT"),
                    max_rows = 5000, verbose = FALSE)
  expect_gt(nrow(ct), 0L)
  expect_true(all(ct$prime_mover_code == "CT"))
})
