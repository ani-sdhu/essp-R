test_that("data without provenance cannot be cited", {
  expect_error(essp.cite(tibble::tibble(a = 1)), "no provenance")
})

test_that("a citation records route, form, filters, and retrieval date", {
  skip_if_no_key()

  x <- essp.gather("retail", state = "GA", years = 2023, freq = "annual",
                   verbose = FALSE)
  e <- essp.cite(x)

  expect_type(e, "character")
  expect_match(e, "^@online\\{eia_retail_[0-9]{4},")
  expect_match(e, "U.S. Energy Information Administration", fixed = TRUE)
  expect_match(e, "electricity/retail-sales", fixed = TRUE)
  expect_match(e, "EIA-826", fixed = TRUE)      # the forms behind the route
  expect_match(e, "stateid=GA", fixed = TRUE)   # the filter actually applied
  expect_match(e, "frequency: annual", fixed = TRUE)
  expect_match(e, "urldate", fixed = TRUE)
  expect_true(endsWith(e, "}\n"))
})

test_that("the citation key can be overridden", {
  skip_if_no_key()
  x <- essp.gather("retail", state = "GA", years = 2023, freq = "annual",
                   verbose = FALSE)
  expect_match(essp.cite(x, key = "my_key"), "^@online\\{my_key,")
})

test_that("appending to a .bib file is idempotent", {
  skip_if_no_key()

  bib <- withr::local_tempfile(fileext = ".bib")
  x <- essp.gather("retail", state = "GA", years = 2023, freq = "annual",
                   verbose = FALSE)

  essp.cite(x, key = "eia_test", file = bib)
  first <- readLines(bib, warn = FALSE)
  expect_true(any(grepl("eia_test", first)))

  # A second call must not duplicate the entry -- briefs get re-rendered often.
  expect_message(essp.cite(x, key = "eia_test", file = bib), "already present")
  expect_equal(readLines(bib, warn = FALSE), first)
})

test_that("two different extracts get distinguishable entries", {
  skip_if_no_key()

  ga <- essp.gather("retail", state = "GA", years = 2023, freq = "annual",
                    verbose = FALSE)
  oh <- essp.gather("retail", state = "OH", years = 2023, freq = "annual",
                    verbose = FALSE)

  # Same route, different filter -- the note must reflect that.
  expect_match(essp.cite(ga), "stateid=GA", fixed = TRUE)
  expect_match(essp.cite(oh), "stateid=OH", fixed = TRUE)
})
