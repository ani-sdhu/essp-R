fake <- function() {
  d <- tibble::tibble(period = c("2024", "2024"), location = c("US", "GA"),
                      generation = c(4308634.3, 140000.5))
  attr(d, "essp_provenance") <- list(
    route = "electricity/electric-power-operational-data", topic = "generation",
    forms = "923", frequency = "annual", start = "2024", end = "2024",
    facets = list(location = "US", sectorid = "99"),
    columns = c("generation", "cost"), rows = 2L, fetched = Sys.time())
  d
}

test_that("the first call writes a CSV and a sidecar", {
  dir <- withr::local_tempdir()
  p <- file.path(dir, "sub", "gen.csv")

  out <- essp.snapshot(p, fake())
  expect_true(file.exists(p))                                   # created nested dir
  expect_true(file.exists(file.path(dir, "sub", "gen.provenance")))
  expect_equal(nrow(out), 2L)
})

test_that("a later call reads the snapshot and never evaluates expr", {
  dir <- withr::local_tempdir()
  p <- file.path(dir, "gen.csv")
  essp.snapshot(p, fake())

  # If the promise were forced, this would error. A committed brief must render
  # with no network and no API key at all.
  called <- FALSE
  out <- essp.snapshot(p, { called <<- TRUE; stop("must not be evaluated") })
  expect_false(called)
  expect_equal(nrow(out), 2L)
})

test_that("provenance survives the round trip", {
  dir <- withr::local_tempdir()
  p <- file.path(dir, "gen.csv")
  essp.snapshot(p, fake())

  prov <- essp.provenance(essp.snapshot(p, stop("no")))
  expect_equal(prov$route, "electricity/electric-power-operational-data")
  expect_equal(prov$topic, "generation")
  expect_equal(prov$frequency, "annual")
  expect_equal(prov$from_snapshot, p)

  info <- essp.snapshotinfo(p)
  expect_match(info$Facets, "location=US")
  expect_match(info$Forms, "923")
  expect_true(nzchar(info$Retrieved))
})

test_that("values survive the round trip intact", {
  dir <- withr::local_tempdir()
  p <- file.path(dir, "gen.csv")
  orig <- fake()
  essp.snapshot(p, orig)

  back <- essp.snapshot(p, stop("no"))
  expect_equal(back$location, orig$location)
  # A written-then-read number must not drift; figures depend on it.
  expect_equal(back$generation, orig$generation, tolerance = 1e-9)
})

test_that("refresh overwrites, and hand edits otherwise persist", {
  dir <- withr::local_tempdir()
  p <- file.path(dir, "gen.csv")
  essp.snapshot(p, fake())

  # Editing the CSV by hand is a supported workflow -- correcting a number
  # must not be silently undone on the next render.
  edited <- utils::read.csv(p); edited$generation[1] <- 1
  utils::write.csv(edited, p, row.names = FALSE)
  expect_equal(essp.snapshot(p, stop("no"))$generation[1], 1)

  # refresh = TRUE deliberately discards the edit.
  expect_equal(essp.snapshot(p, fake(), refresh = TRUE)$generation[1],
               4308634.3, tolerance = 1e-6)
})

test_that("bad input and a missing sidecar are handled", {
  dir <- withr::local_tempdir()
  expect_error(essp.snapshot(file.path(dir, "x.csv"), "not a frame"),
               "must produce a data frame")

  p <- file.path(dir, "bare.csv")
  utils::write.csv(data.frame(a = 1), p, row.names = FALSE)
  expect_warning(info <- essp.snapshotinfo(p), "No provenance sidecar")
  expect_null(info)
  # A CSV without a sidecar still reads; it just carries no provenance.
  expect_equal(nrow(essp.snapshot(p, stop("no"))), 1L)
})
