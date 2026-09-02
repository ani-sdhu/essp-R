test_that("CPI comes from EIA and matches published annual averages", {
  skip_if_no_key()

  cpi <- essp.cpi(2019:2024)
  expect_equal(nrow(cpi), 6L)
  expect_equal(cpi$year, 2019:2024)

  # BLS published CPI-U annual averages. Hardcoding these in the package would
  # go stale; asserting them in a test is how we notice if the series moves.
  expect_equal(cpi$cpi[cpi$year == 2019], 255.657, tolerance = 0.01)
  expect_equal(cpi$cpi[cpi$year == 2024], 313.689, tolerance = 0.01)

  # CPI is monotonically increasing over this stretch.
  expect_true(all(diff(cpi$cpi) > 0))
})

test_that("deflating to the base year is an identity", {
  skip_if_no_key()
  expect_equal(essp.deflate(100, 2024, base = 2024), 100)
})

test_that("deflation scales by the CPI ratio", {
  skip_if_no_key()

  idx <- essp.cpi(c(2000, 2024))
  ratio <- idx$cpi[idx$year == 2024] / idx$cpi[idx$year == 2000]

  expect_equal(essp.deflate(1, 2000, base = 2024), ratio)
  # Earlier dollars are worth more in later dollars.
  expect_gt(essp.deflate(1, 2000, base = 2024), 1)
  expect_lt(essp.deflate(1, 2024, base = 2000), 1)
})

test_that("deflate accepts a supplied index and vectorises", {
  idx <- c("2000" = 100, "2010" = 150, "2020" = 200)

  expect_equal(essp.deflate(10, 2000, base = 2020, index = idx), 20)
  expect_equal(essp.deflate(c(10, 10, 10), c(2000, 2010, 2020),
                            base = 2020, index = idx),
               c(20, 200/150 * 10, 10))

  # A data frame form is accepted too.
  df <- data.frame(year = c(2000, 2020), cpi = c(100, 200))
  expect_equal(essp.deflate(10, 2000, base = 2020, index = df), 20)
})

test_that("deflate rejects bad input and warns on missing years", {
  idx <- c("2000" = 100, "2020" = 200)

  expect_error(essp.deflate("a", 2000, 2020, idx), "must be numeric")
  expect_error(essp.deflate(c(1, 2), c(2000, 2010, 2020), 2020, idx),
               "length 1 or the same length")
  expect_error(essp.deflate(1, 2000, base = 1999, index = idx),
               "No index value for base year")

  # A year absent from the index yields NA with a warning, not a silent wrong
  # answer.
  expect_warning(out <- essp.deflate(1, 2011, base = 2020, index = idx),
                 "No index value")
  expect_true(is.na(out))
})

test_that("real and nominal price series diverge as expected", {
  skip_if_no_key()

  p <- analyze.prices("GA", 2010:2024)
  expect_equal(nrow(p), 15L)
  expect_true(all(p$state == "GA"))

  # The base year's two series coincide by definition.
  b <- p[p$year == 2024, ]
  expect_equal(b$price, b$price_real, tolerance = 1e-8)

  # Before the base year, real dollars exceed nominal.
  e <- p[p$year == 2010, ]
  expect_gt(e$price_real, e$price)

  # Nominal rose substantially while real was roughly flat -- the reason
  # deflating matters at all.
  nominal <- p$price[p$year == 2024] / p$price[p$year == 2010] - 1
  real    <- p$price_real[p$year == 2024] / p$price_real[p$year == 2010] - 1
  expect_gt(nominal, 0.25)
  expect_lt(abs(real), 0.15)
})

test_that("seasonal index centres on 100", {
  df <- data.frame(
    period = sprintf("2023-%02d", 1:12),
    mcf    = c(120, 110, 90, 60, 40, 30, 28, 29, 35, 55, 85, 115)
  )
  s <- analyze.seasonality(df, "mcf", "period")

  expect_equal(nrow(s), 12L)
  expect_equal(s$month, 1:12)
  expect_equal(s$month_name[1], "Jan")

  # A month at the series mean indexes to 100, so the mean of the indices is
  # 100 when every month is equally represented.
  expect_equal(mean(s$index), 100, tolerance = 1e-8)

  # Winter gas use dwarfs summer -- January should index well above July.
  expect_gt(s$index[s$month == 1], 150)
  expect_lt(s$index[s$month == 7], 60)
})

test_that("seasonality accepts Date columns as well as YYYY-MM strings", {
  df <- data.frame(
    when = as.Date(sprintf("2023-%02d-01", 1:12)),
    v    = c(120, 110, 90, 60, 40, 30, 28, 29, 35, 55, 85, 115)
  )
  s <- analyze.seasonality(df, "v", "when")
  expect_equal(s$month, 1:12)
  expect_gt(s$index[1], 150)
})
