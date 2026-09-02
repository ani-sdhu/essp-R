# Prices, inflation adjustment, and seasonality ---------------------------------

#' Consumer Price Index (urban), annual
#'
#' Fetches CPI-U from EIA's own Monthly Energy Review (series `CPUCIUS`,
#' 1982-84 = 100) rather than carrying a hardcoded table that would silently go
#' stale. Values match the BLS published annual averages.
#'
#' @param years Calendar years to return. `NULL` (default) returns everything
#'   available.
#'
#' @return A tibble of `year` and `cpi`.
#'
#' @examples
#' \dontrun{
#' essp.cpi(2015:2024)
#' }
#'
#' @export
essp.cpi <- function(years = NULL) {
  args <- list("totalenergy", freq = "annual",
               facets = list(msn = "CPUCIUS"), verbose = FALSE)
  if (!is.null(years)) {
    args$start <- as.character(min(years))
    args$end   <- as.character(max(years))
  }
  raw <- do.call(essp.gather, args)

  out <- tibble::tibble(
    year = as.integer(raw$period),
    cpi  = suppressWarnings(as.numeric(raw$value))
  )
  out <- out[!is.na(out$cpi), , drop = FALSE]
  out[order(out$year), , drop = FALSE]
}

#' Convert nominal dollars to real dollars
#'
#' Rescales a nominal series into the dollars of a chosen base year:
#'
#' \deqn{real = nominal \times \frac{CPI_{base}}{CPI_{year}}}
#'
#' @param x Numeric vector of nominal values.
#' @param year Calendar year of each element of `x`. Recycled if length one.
#' @param base Base year to express results in.
#' @param index Optional named numeric vector of index values keyed by year, or
#'   a data frame with `year` and `cpi` columns. Defaults to [essp.cpi()].
#'
#' @return A numeric vector the same length as `x`, in `base`-year dollars.
#'
#' @examples
#' \dontrun{
#' # $1.00 in 2000 dollars, expressed in 2024 dollars
#' essp.deflate(1, year = 2000, base = 2024)
#' }
#'
#' @export
essp.deflate <- function(x, year, base, index = NULL) {
  if (!is.numeric(x)) rlang::abort("`x` must be numeric.")
  if (length(year) == 1L) year <- rep(year, length(x))
  if (length(year) != length(x)) {
    rlang::abort("`year` must be length 1 or the same length as `x`.")
  }

  if (is.null(index)) index <- essp.cpi(unique(c(year, base)))
  if (is.data.frame(index)) {
    index <- stats::setNames(index$cpi, as.character(index$year))
  } else if (is.null(names(index))) {
    rlang::abort("`index` must be named by year, or a data frame with year/cpi.")
  }

  # Single-bracket lookup: `[[` throws on a missing name, which would surface
  # as "subscript out of bounds" instead of the intended message.
  base_val <- unname(index[as.character(base)])
  if (length(base_val) != 1L || is.na(base_val)) {
    rlang::abort(paste0("No index value for base year ", base, "."))
  }

  vals <- unname(index[as.character(year)])
  missing <- unique(year[is.na(vals)])
  if (length(missing)) {
    warning("No index value for year(s): ", paste(missing, collapse = ", "),
            "; those elements return NA.", call. = FALSE)
  }

  x * base_val / vals
}

#' Retail electricity prices, nominal and real
#'
#' Average retail price by state and sector, with an inflation-adjusted series
#' alongside the nominal one. Comparing prices across years without deflating
#' overstates increases -- most of a twenty-year "rise" is usually inflation.
#'
#' @param state State code(s), or `"US"`.
#' @param years Calendar years.
#' @param sector EIA sector id; `"RES"` (residential) by default.
#' @param base Base year for the real series. Defaults to the latest year
#'   requested.
#'
#' @return A tibble of `year`, `state`, `sector`, `price` (nominal, cents/kWh),
#'   `price_real`, and `base_year`.
#'
#' @examples
#' \dontrun{
#' analyze.prices("GA", 2010:2024)
#' }
#'
#' @export
analyze.prices <- function(state = "US", years = NULL, sector = "RES", base = NULL) {
  state <- essp_parse_where(state, "state")
  if (is.null(years)) {
    years <- seq(essp_latest_year() - 9L, essp_latest_year())
  } else if (is.character(years)) {
    years <- essp_parse_when(years, "year")$years
  } else {
    years <- as.integer(years)
  }
  if (is.null(base)) base <- max(years)

  raw <- essp.gather("retail", state = state, years = years, freq = "annual",
                     sector = sector, verbose = FALSE)
  if (!nrow(raw)) rlang::abort("No retail price data returned.")

  out <- tibble::tibble(
    year   = as.integer(raw$period),
    state  = raw$stateid,
    sector = raw$sectorid,
    price  = suppressWarnings(as.numeric(raw$price))
  )
  out <- out[!is.na(out$price), , drop = FALSE]

  idx <- essp.cpi(unique(c(out$year, base)))
  out$price_real <- essp.deflate(out$price, out$year, base, index = idx)
  out$base_year  <- base

  out[order(out$state, out$year), , drop = FALSE]
}

#' Seasonal index of a monthly series
#'
#' Expresses each month as a percentage of the series mean, so a value of 150
#' means that month runs 50% above average. This is what turns "gas use peaks
#' in winter" into a number.
#'
#' @param data A data frame.
#' @param value Name of the numeric column, as a string.
#' @param period Name of the period column, as a string. Accepts `Date` or
#'   `"YYYY-MM"` strings.
#'
#' @return A tibble of `month`, `month_name`, `mean_value`, `index`, and `n`.
#'
#' @examples
#' df <- data.frame(
#'   period = sprintf("2023-%02d", 1:12),
#'   mcf    = c(120, 110, 90, 60, 40, 30, 28, 29, 35, 55, 85, 115)
#' )
#' analyze.seasonality(df, "mcf", "period")
#'
#' @export
analyze.seasonality <- function(data, value, period) {
  if (!is.data.frame(data)) rlang::abort("`data` must be a data frame.")
  for (col in c(value, period)) {
    if (!col %in% names(data)) {
      rlang::abort(paste0("Column \"", col, "\" not found in `data`."))
    }
  }

  v <- suppressWarnings(as.numeric(data[[value]]))
  p <- data[[period]]
  m <- if (inherits(p, "Date") || inherits(p, "POSIXt")) {
    as.integer(format(p, "%m"))
  } else {
    as.integer(substr(as.character(p), 6, 7))
  }

  keep <- !is.na(v) & !is.na(m)
  if (!any(keep)) rlang::abort("No usable month/value pairs found.")
  v <- v[keep]; m <- m[keep]

  overall <- mean(v)
  parts <- lapply(split(v, m), function(x) {
    data.frame(mean_value = mean(x), n = length(x))
  })

  out <- tibble::tibble(
    month      = as.integer(names(parts)),
    month_name = month.abb[as.integer(names(parts))],
    mean_value = vapply(parts, function(d) d$mean_value, numeric(1)),
    n          = vapply(parts, function(d) d$n, numeric(1)),
    index      = vapply(parts, function(d) d$mean_value, numeric(1)) / overall * 100
  )
  out[order(out$month), , drop = FALSE]
}
