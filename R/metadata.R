# Route metadata helpers -------------------------------------------------------
#
# eia::eia_metadata() returns a one-row tibble of list-columns whose internal
# shape is NOT consistent across routes. Observed against the live API on
# 2026-08-26, the `Data` field arrives in three different shapes:
#
#   long    retail-sales, electric-power-operational-data, rto/fuel-type-data
#           one row per column, ids in a column called `id`
#   flat    seds, natural-gas/pri/sum
#           a single column literally named `value.id` holding "value"
#   wide    operating-generator-capacity
#           the COLUMN NAMES are the data ids; every cell is a list
#
# Reading only `$id` -- the obvious implementation -- silently returns nothing
# for the wide shape. That matters more than it looks: `data[]` is what makes
# the API return values at all, so an empty column list yields rows of period
# and facet labels with no measurements, and no error. See essp.gather().

# Peel list-of-one wrappers until we reach something inspectable.
unwrap <- function(v) {
  while (is.list(v) && !is.data.frame(v) && length(v) == 1L) v <- v[[1]]
  v
}

#' Data column ids available on a route
#'
#' Reads a route's metadata and returns the names of its measurement columns --
#' the values that must be passed as `data[]` for the API to return anything but
#' labels.
#'
#' @param route An EIA route path, e.g. `"electricity/retail-sales"`.
#'
#' @return A character vector of column ids.
#'
#' @keywords internal
essp_data_columns <- function(route) {
  d <- unwrap(eia::eia_metadata(route)[["Data"]])

  if (is.data.frame(d)) {
    if ("id" %in% names(d)) return(as.character(d$id))
    # Flattened single-series routes: "value.id" -> "value".
    if (all(grepl("\\.id$", names(d)))) return(sub("\\.id$", "", names(d)))
    # Wide: the column names are the ids.
    return(names(d))
  }

  if (!is.null(names(d))) return(names(d))
  as.character(d)
}

#' Facet ids available on a route
#'
#' @param route An EIA route path.
#' @return A character vector of facet ids, empty if the route has none.
#' @keywords internal
essp_facet_ids <- function(route) {
  f <- unwrap(eia::eia_metadata(route)[["Facets"]])
  if (is.data.frame(f)) {
    if ("id" %in% names(f)) return(as.character(f$id))
    return(names(f))
  }
  if (length(f) == 0L) return(character(0))
  as.character(f)
}

#' Frequencies available on a route
#'
#' @param route An EIA route path.
#' @return A character vector of frequency ids, empty if the route declares none.
#' @keywords internal
essp_frequencies <- function(route) {
  f <- unwrap(eia::eia_metadata(route)[["Frequency"]])
  if (is.data.frame(f)) {
    if ("id" %in% names(f)) return(as.character(f$id))
    return(names(f))
  }
  if (length(f) == 0L) return(character(0))
  as.character(f)
}

#' Convert years to period strings at a route's own granularity
#'
#' The EIA date filter compares against period stamps, not calendar dates, and
#' the comparison is lexical. Passing `start = "2008-02-01"` to a monthly series
#' therefore EXCLUDES February 2008, because the stamp `"2008-02"` sorts before
#' it -- silently dropping the first period of any range.
#'
#' Emitting bounds already in the route's own format sidesteps the problem
#' rather than patching it: a monthly route gets `"2008-01"`, an annual route
#' `"2008"`, an hourly route `"2008-01-01T00"`.
#'
#' @param years Numeric vector of calendar years. Only the range is used.
#' @param freq A frequency id, e.g. `"monthly"`, `"annual"`, `"hourly"`.
#'
#' @return A list with `start` and `end` strings.
#'
#' @keywords internal
essp_period_range <- function(years, freq) {
  y0 <- min(years)
  y1 <- max(years)

  if (grepl("hour", freq)) {
    return(list(start = sprintf("%d-01-01T00", y0), end = sprintf("%d-12-31T23", y1)))
  }
  if (grepl("dai|day", freq)) {
    return(list(start = sprintf("%d-01-01", y0), end = sprintf("%d-12-31", y1)))
  }
  if (grepl("month", freq)) {
    return(list(start = sprintf("%d-01", y0), end = sprintf("%d-12", y1)))
  }
  if (grepl("quarter", freq)) {
    return(list(start = sprintf("%d-Q1", y0), end = sprintf("%d-Q4", y1)))
  }
  # Annual, and anything else that stamps a bare year.
  list(start = as.character(y0), end = as.character(y1))
}
