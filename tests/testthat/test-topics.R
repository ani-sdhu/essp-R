test_that("the topic map is internally well-formed", {
  m <- essp.topics()
  expect_s3_class(m, "tbl_df")
  expect_true(all(c("topic", "route", "forms", "geo_facet", "fuel_facet",
                    "sector_facet", "freqs", "period", "verified",
                    "description") %in% names(m)))
  expect_false(any(duplicated(m$topic)))
  expect_false(any(duplicated(m$route)))
  expect_false(any(is.na(m$topic)))
  expect_false(any(is.na(m$route)))
  expect_type(m$verified, "logical")
})

test_that("no mapped route is a known parent node or a deprecated route", {
  m <- essp.topics()
  expect_length(intersect(m$route, names(essp_parent_routes())), 0L)
  expect_length(intersect(m$route, names(essp_deprecated_routes())), 0L)
})

test_that("parent nodes and deprecated routes fail loudly, not silently", {
  # Both return a 200 from EIA -- empty metadata and stale data respectively --
  # so without these guards they look like they worked.
  expect_error(essp.resolve("aeo"), "parent node")
  expect_error(essp.resolve("electricity"), "parent node")
  expect_error(essp.resolve("natural-gas/pri"), "parent node")
  expect_error(essp.resolve("co2-emissions/co2-emissions-aggregates"), "deprecated")
})

test_that("essp.topics() filters by topic and rejects unknown ones", {
  expect_equal(nrow(essp.topics("hourly")), 1L)
  expect_equal(essp.topics("hourly")$route, "electricity/rto/fuel-type-data")
  expect_error(essp.topics("not-a-topic"), "Unknown topic")
})

test_that("essp.resolve() resolves topics, routes, and form numbers", {
  expect_equal(essp.resolve("hourly")$route, "electricity/rto/fuel-type-data")

  # A route path resolves to itself.
  expect_equal(
    essp.resolve("electricity/retail-sales")$route,
    "electricity/retail-sales"
  )

  # Form -> route is many-to-many: 923 spans several routes. This is the
  # behavior the whole topic-map design exists to handle, so pin it down.
  r923 <- essp.resolve("923")
  expect_gt(nrow(r923), 1L)
  expect_true("electricity/facility-fuel" %in% r923$route)
})

test_that("form matching is case-insensitive and does not match substrings", {
  expect_equal(essp.resolve("860m")$route, essp.resolve("860M")$route)

  # "86" must not match the "860" cell.
  expect_error(essp.resolve("86"), "Could not resolve")
})

test_that("an unmapped route path passes through rather than erroring", {
  out <- essp.resolve("electricity/some-new-route")
  expect_equal(out$route, "electricity/some-new-route")
  expect_true(is.na(out$topic))
  expect_false(out$verified)
})

test_that("a non-route unknown string errors with guidance", {
  expect_error(essp.resolve("nonsense"), "Could not resolve")
  expect_error(essp.resolve(c("a", "b")), "single non-missing")
  expect_error(essp.resolve(NA_character_), "single non-missing")
})
