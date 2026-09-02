test_that("fuel groups pick one non-overlapping generation code per resource", {
  g <- essp.fuelgroups()
  expect_false(any(duplicated(g$resource)))
  expect_false(any(duplicated(g$generation_code)))

  # Aggregates that contain other selected codes must never be picked --
  # FOS alone is ~59% of ALL, so including it would double-count massively.
  expect_false(any(c("ALL", "FOS", "REN", "AOR", "NGO") %in% g$generation_code))
})

test_that("the solar argument switches the generation series only", {
  u <- essp.fuelgroups("utility")
  t <- essp.fuelgroups("total")
  expect_equal(u$generation_code[u$resource == "Solar"], "SUN")
  expect_equal(t$generation_code[t$resource == "Solar"], "TSN")
  # Capacity has no distributed-PV equivalent, so it must not change.
  expect_equal(u$capacity_codes, t$capacity_codes)
})

test_that("capacity codes map to resources, unknown codes fall to Other", {
  g <- essp.fuelgroups()
  expect_equal(group_capacity_codes(c("NG", "WND", "NUC"), g),
               c("Natural Gas", "Wind", "Nuclear"))
  # All five coal codes roll up together.
  expect_true(all(group_capacity_codes(c("BIT","SUB","LIG","WC","RC"), g) == "Coal"))
  # Batteries and fuel oil are not among the seven headline resources.
  expect_equal(group_capacity_codes(c("MWH", "DFO", "ZZZ"), g),
               rep("Other", 3))
})

test_that("shares sum to 100 on both metrics", {
  skip_if_no_key()

  m <- analyze.mix("US", 2024)
  for (metric in c("Capacity", "Generation")) {
    expect_equal(sum(m$share[m$metric == metric]), 100, tolerance = 1e-6)
  }
})

test_that("analyze.capacity reads one snapshot, not twelve months summed", {
  skip_if_no_key()

  cap <- analyze.capacity("US", 2024)
  # US operating nameplate capacity is ~1.2-1.4 TW. Summing 12 monthly
  # snapshots would give ~15 TW, so this catches that class of mistake.
  expect_gt(sum(cap$mw), 1.0e6)
  expect_lt(sum(cap$mw), 2.0e6)
  expect_equal(attr(cap, "essp_period"), "2024-12")
})

test_that("mixing distributed solar into generation warns", {
  skip_if_no_key()
  expect_warning(analyze.mix("US", 2024, solar = "total"), "not like-for-like")
})

test_that("solar capacity share exceeds its generation share", {
  skip_if_no_key()

  # Physics, not preference: solar's capacity factor is well under 100%, so its
  # share of capacity must exceed its share of generation. The reverse holds
  # for nuclear. This is exactly the contrast the fleet-makeup figure exists to
  # show -- and the check that catches the two being swapped.
  m <- analyze.mix("US", 2024)
  s <- function(metric, res) m$share[m$metric == metric & m$resource == res]

  expect_gt(s("Capacity", "Solar"), s("Generation", "Solar"))
  expect_gt(s("Capacity", "Wind"),  s("Generation", "Wind"))
  expect_lt(s("Capacity", "Nuclear"), s("Generation", "Nuclear"))
})

test_that("the published 2024 figures are reproducible", {
  skip_if_no_key()

  # Regression against the numbers hardcoded at
  # RT_NaturalGas/figures/figure1_plot.R:18, which the program's own notes
  # flagged as uncited. They reproduce on the solar = "total" basis.
  m <- suppressWarnings(analyze.mix("US", 2024, solar = "total"))
  s <- function(metric, res) m$share[m$metric == metric & m$resource == res]

  expect_equal(s("Capacity", "Natural Gas"), 43.1, tolerance = 0.3)
  expect_equal(s("Capacity", "Coal"),        14.3, tolerance = 0.3)
  expect_equal(s("Capacity", "Wind"),        11.6, tolerance = 0.3)
  expect_equal(s("Capacity", "Solar"),        9.4, tolerance = 0.3)
  expect_equal(s("Capacity", "Nuclear"),      7.8, tolerance = 0.3)
  expect_equal(s("Capacity", "Hydro"),        7.7, tolerance = 0.4)

  expect_equal(s("Generation", "Natural Gas"), 43.4, tolerance = 0.3)
  expect_equal(s("Generation", "Coal"),        15.1, tolerance = 0.3)
  expect_equal(s("Generation", "Wind"),        10.5, tolerance = 0.3)
  expect_equal(s("Generation", "Solar"),        7.0, tolerance = 0.3)
  expect_equal(s("Generation", "Nuclear"),     18.1, tolerance = 0.3)
  expect_equal(s("Generation", "Hydro"),        5.5, tolerance = 0.3)
})

test_that("units are taken from EIA, not hardcoded", {
  skip_if_no_key()

  m <- analyze.mix("US", 2024)

  # Generation is reported in THOUSAND megawatthours. Reading this as plain MWh
  # understates it 1,000-fold, so the label must come from the API's own
  # "<column>-units" field rather than a string typed into the package.
  gu <- unique(m$units[m$metric == "Generation"])
  expect_length(gu, 1L)
  expect_match(gu, "thousand", ignore.case = TRUE)
  expect_match(gu, "megawatthour", ignore.case = TRUE)

  cu <- unique(m$units[m$metric == "Capacity"])
  expect_length(cu, 1L)
  expect_false(is.na(cu))

  # Sanity on magnitude: US annual generation is ~4.3 million thousand-MWh.
  # If the unit were plain MWh the total would be a thousand times larger.
  total_gen <- sum(m$value[m$metric == "Generation"])
  expect_gt(total_gen, 3e6)
  expect_lt(total_gen, 6e6)
})

test_that("a state with no wind fleet simply omits it", {
  skip_if_no_key()
  ga <- analyze.mix("GA", 2024)
  expect_false("Wind" %in% ga$resource[ga$metric == "Capacity"])
  expect_gt(sum(ga$share[ga$metric == "Capacity"]), 99.9)
})
