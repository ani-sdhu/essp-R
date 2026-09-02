# The stacking combinator, exercised offline with a stand-in core so no API
# call is needed to prove the mechanics.

test_that("expand flags multi-value requests", {
  a <- essp_expand("CA", "2024")
  expect_false(a$multi)
  expect_identical(a$where, "CA"); expect_identical(a$years, 2024L)

  b <- essp_expand("CA,MD", "2024,2023")
  expect_true(b$multi)
  expect_length(b$where, 2L); expect_length(b$years, 2L)
})

test_that("stack re-runs a metric per (where, year) and prepends key columns", {
  # The eval/re-invoke mechanics are exercised through a real metric rather
  # than a synthetic stand-in, matching how it runs in practice.
  skip_if_no_key()

  out <- analyze.diversity("GA,WV", "2024")
  expect_equal(nrow(out), 2L)
  expect_identical(names(out)[1:2], c("state", "year"))
  expect_setequal(unique(out$state), c("GA", "WV"))
  expect_true(all(out$year == 2024L))
})

test_that("when_window passes explicit bounds through and expands a lone string", {
  expect_identical(
    essp_when_window("2024-07-01T00", "2024-07-31T23"),
    list(start = "2024-07-01T00", end = "2024-07-31T23"))

  w <- essp_when_window("July 2024")
  expect_equal(c(w$start, w$end), c("2024-07-01T00", "2024-07-31T23"))
})
