# Hourly load analysis ---------------------------------------------------------
#
# Two quirks of the Form 930 routes shape everything here.
#
# 1. `local-hourly` REQUIRES a UTC offset in the period stamp
#    ("2024-07-01T00-07"). The plain "2024-07-01T00" form that works for
#    `hourly` returns HTTP 500. The offset differs per balancing authority and
#    shifts with daylight saving, so it cannot be hardcoded -- it is probed.
#
# 2. The `respondent` facet mixes individual balancing authorities (CISO, PJM)
#    with aggregate regions (US48, CAL, TEX, ...), exactly as `location` mixes
#    states with census divisions. Aggregating across respondents without
#    checking would double-count.

#' Balancing authorities and regions in the hourly data
#'
#' The `respondent` facet contains both individual balancing authorities and
#' aggregate regions that already contain them. Summing across all respondents
#' therefore double-counts; this lists which is which.
#'
#' @param type `"all"` (default), `"ba"` for individual balancing authorities,
#'   or `"region"` for the aggregates.
#'
#' @return A tibble of `id`, `name`, and `is_region`.
#'
#' @examples
#' \dontrun{
#' essp.respondents("region")
#' }
#'
#' @export
essp.respondents <- function(type = c("all", "ba", "region")) {
  type <- match.arg(type)
  r <- eia::eia_facets("electricity/rto/region-data", "respondent")

  # EIA marks aggregates by prefixing the alias with "Region: ".
  out <- tibble::tibble(
    id        = as.character(r$id),
    name      = as.character(r$name),
    is_region = grepl("^Region:", r$alias)
  )
  out <- out[order(out$is_region, out$id), , drop = FALSE]

  switch(type,
         all    = out,
         ba     = out[!out$is_region, , drop = FALSE],
         region = out[out$is_region, , drop = FALSE])
}

# Probe a respondent's current UTC offset by asking for one local-hourly row
# and reading the suffix off the period stamp it returns. Memoised by eia's
# own cache, so this costs one small request per session per BA.
essp_ba_offset <- function(ba) {
  one <- tryCatch(
    suppressWarnings(  # the "rows available" notice is noise for a 1-row probe
      eia::eia_data(dir = "electricity/rto/region-data", data = "value",
                    facets = list(respondent = ba, type = "D"),
                    freq = "local-hourly", length = 1)
    ),
    error = function(e) NULL
  )
  if (is.null(one) || !nrow(one)) return("-00")
  m <- regmatches(one$period[1], regexpr("[+-][0-9]{2}$", one$period[1]))
  if (length(m)) m else "-00"
}

# Fetch hourly demand (or another `type`) for a respondent, in local time.
fetch_hourly <- function(ba, start, end, type = "D", local = TRUE,
                         verbose = FALSE) {
  freq <- if (local) "local-hourly" else "hourly"
  if (local) {
    off <- essp_ba_offset(ba)
    # A DST change inside the window shifts its edges by an hour; the returned
    # stamps still carry the true offset, so derived hours stay correct.
    if (!grepl("[+-][0-9]{2}$", start)) start <- paste0(start, off)
    if (!grepl("[+-][0-9]{2}$", end))   end   <- paste0(end, off)
  }

  raw <- essp.gather("demand", ba = ba, freq = freq, start = start, end = end,
                     facets = list(type = type), max_rows = 1e6,
                     verbose = verbose)
  if (!nrow(raw)) {
    rlang::abort(paste0("No hourly data for ", ba, " between ", start, " and ", end, "."))
  }

  raw$mw <- suppressWarnings(as.numeric(raw$value))
  raw <- raw[!is.na(raw$mw), , drop = FALSE]
  # Hour-of-day comes from the stamp itself, which is authoritative for local
  # time regardless of the offset used to bound the request.
  raw$hour <- as.integer(sub("^.*T([0-9]{2}).*$", "\\1", raw$period))
  raw$date <- sub("T.*$", "", raw$period)
  raw[order(raw$period), , drop = FALSE]
}

#' Load statistics for a balancing authority
#'
#' Peak, minimum, and mean demand, load factor, when the peak fell, and the
#' steepest hour-to-hour ramp.
#'
#' Load factor is mean demand divided by peak demand. A high value means a flat
#' profile that baseload plants can serve; a low one means capacity sits idle
#' most of the year to cover a few hours.
#'
#' @param ba Balancing authority code, e.g. `"CISO"`. See [essp.respondents()].
#' @param start,end Period bounds, `"YYYY-MM-DDTHH"`. A UTC offset is appended
#'   automatically when `local = TRUE`.
#' @param local Use the respondent's local time (default) rather than UTC.
#'   Local time is what makes hour-of-day meaningful.
#'
#' @return A one-row tibble of load statistics.
#'
#' @examples
#' \dontrun{
#' analyze.load("CISO", "2024-01-01T00", "2024-12-31T23")
#' }
#'
#' @export
analyze.load <- function(ba, start, end = NULL, local = TRUE) {
  .w <- essp_when_window(start, end); start <- .w$start; end <- .w$end
  d <- fetch_hourly(ba, start, end, type = "D", local = local)

  ramp <- diff(d$mw)
  peak_i <- which.max(d$mw)

  tibble::tibble(
    respondent   = ba,
    hours        = nrow(d),
    peak_mw      = max(d$mw),
    peak_period  = d$period[peak_i],
    peak_hour    = d$hour[peak_i],
    min_mw       = min(d$mw),
    mean_mw      = mean(d$mw),
    load_factor  = mean(d$mw) / max(d$mw) * 100,
    max_ramp_up  = if (length(ramp)) max(ramp) else NA_real_,
    max_ramp_down = if (length(ramp)) min(ramp) else NA_real_,
    units        = "megawatthours"
  )
}

#' Average daily load shape
#'
#' Mean demand by hour of day, optionally split by month or season -- the
#' aggregation a demand-curve figure consumes.
#'
#' Hours are local to the respondent, which is what makes an evening peak land
#' in the evening. Under UTC the same data smears the peak across the day.
#'
#' @inheritParams analyze.load
#' @param by Optional grouping: `"month"` or `"season"`.
#'
#' @return A tibble of `hour`, mean/min/max demand, and the grouping column.
#'
#' @examples
#' \dontrun{
#' analyze.loadshape("CISO", "2024-07-01T00", "2024-07-31T23")
#' }
#'
#' @export
analyze.loadshape <- function(ba, start, end = NULL, by = c("none", "month", "season"),
                              local = TRUE) {
  by <- match.arg(by)
  .w <- essp_when_window(start, end); start <- .w$start; end <- .w$end
  d <- fetch_hourly(ba, start, end, type = "D", local = local)

  d$group <- switch(
    by,
    none   = "all",
    month  = month.abb[as.integer(substr(d$date, 6, 7))],
    season = c("Winter","Winter","Spring","Spring","Spring","Summer",
               "Summer","Summer","Fall","Fall","Fall","Winter")[
                 as.integer(substr(d$date, 6, 7))]
  )

  parts <- lapply(split(d, list(d$group, d$hour), drop = TRUE), function(x) {
    data.frame(group = x$group[1], hour = x$hour[1],
               mean_mw = mean(x$mw), min_mw = min(x$mw), max_mw = max(x$mw),
               n = nrow(x), stringsAsFactors = FALSE)
  })

  out <- do.call(rbind, parts)
  out <- out[order(out$group, out$hour), , drop = FALSE]
  rownames(out) <- NULL
  if (by == "none") out$group <- NULL else names(out)[names(out) == "group"] <- by
  tibble::as_tibble(out)
}

#' Average daily generation shape by fuel
#'
#' Mean generation by hour of day and fuel type -- the stacked profile a demand
#' curve figure is built from.
#'
#' Hours are local to the respondent. Under UTC the solar peak lands hours away
#' from midday and the whole shape becomes meaningless.
#'
#' @inheritParams analyze.load
#' @param fuels Optional fuel codes to keep, e.g. `c("NG", "NUC", "SUN")`.
#'   `NULL` keeps all.
#'
#' @return A tibble of `hour`, `fueltype`, `fuel_name`, `mean_mw`, and `n`.
#'
#' @examples
#' \dontrun{
#' analyze.fuelshape("CISO", "2024-07-01T00", "2024-07-31T23")
#' }
#'
#' @export
analyze.fuelshape <- function(ba, start, end = NULL, fuels = NULL, local = TRUE) {
  .w <- essp_when_window(start, end); start <- .w$start; end <- .w$end
  freq <- if (local) "local-hourly" else "hourly"
  if (local) {
    off <- essp_ba_offset(ba)
    if (!grepl("[+-][0-9]{2}$", start)) start <- paste0(start, off)
    if (!grepl("[+-][0-9]{2}$", end))   end   <- paste0(end, off)
  }

  raw <- essp.gather("hourly", ba = ba, freq = freq, start = start, end = end,
                     fuel = fuels, max_rows = 1e6, verbose = FALSE)
  if (!nrow(raw)) rlang::abort(paste0("No hourly fuel data for ", ba, "."))

  raw$mw <- suppressWarnings(as.numeric(raw$value))
  raw <- raw[!is.na(raw$mw), , drop = FALSE]
  raw$hour <- as.integer(sub("^.*T([0-9]{2}).*$", "\\1", raw$period))

  parts <- lapply(split(raw, list(raw$hour, raw$fueltype), drop = TRUE), function(d) {
    data.frame(hour = d$hour[1], fueltype = d$fueltype[1],
               fuel_name = d[["type-name"]][1],
               mean_mw = mean(d$mw), n = nrow(d), stringsAsFactors = FALSE)
  })

  out <- do.call(rbind, parts)
  out <- out[order(out$fueltype, out$hour), , drop = FALSE]
  rownames(out) <- NULL
  tibble::as_tibble(out)
}

#' Load duration curve
#'
#' Demand sorted from highest to lowest against the percentage of hours at or
#' above each level. The steepness of the left edge is what justifies peaking
#' capacity: a few hundred hours a year may set the entire peak requirement.
#'
#' @inheritParams analyze.load
#'
#' @return A tibble of `rank`, `pct_hours`, and `mw`.
#'
#' @examples
#' \dontrun{
#' analyze.durationcurve("CISO", "2024-01-01T00", "2024-12-31T23")
#' }
#'
#' @export
analyze.durationcurve <- function(ba, start, end = NULL, local = TRUE) {
  .w <- essp_when_window(start, end); start <- .w$start; end <- .w$end
  d <- fetch_hourly(ba, start, end, type = "D", local = local)
  mw <- sort(d$mw, decreasing = TRUE)

  tibble::tibble(
    rank      = seq_along(mw),
    pct_hours = seq_along(mw) / length(mw) * 100,
    mw        = mw
  )
}

#' Reserve margin against peak demand
#'
#' Installed capacity in a balancing authority compared with its observed peak
#' demand. The margin is the headroom above peak, as a percentage of peak.
#'
#' Capacity comes from the Form 860 inventory filtered by
#' `balancing_authority_code`, and peak demand from Form 930, so this is an
#' observed margin rather than a planning-reserve figure.
#'
#' Read it as installed nameplate headroom, not as firm capacity. Every
#' operating generator counts at its rated output, including solar and wind
#' whose contribution at the moment of peak may be near zero -- CISO's peak
#' falls after sunset, so a large share of its solar fleet counts here but
#' delivers nothing then. A planning reserve margin instead credits each
#' resource at an accredited capacity value, and is correspondingly lower.
#'
#' @param ba Balancing authority code.
#' @param year Calendar year.
#' @param basis Capacity basis; `"summer"` by default, since peaks in most
#'   regions fall in summer.
#' @param status Generator status codes to count. Defaults to `"OP"`.
#'
#' @return A one-row tibble with capacity, peak demand, and the margin.
#'
#' @examples
#' \dontrun{
#' analyze.reservemargin("CISO", 2024)
#' }
#'
#' @export
analyze.reservemargin <- function(ba, year = NULL,
                                  basis  = c("summer", "nameplate", "winter"),
                                  status = "OP") {
  if (is.null(year)) year <- essp_latest_year()
  year <- essp_parse_when(year, "year")$years[1]
  basis <- match.arg(basis)
  col <- switch(basis,
                summer    = "net-summer-capacity-mw",
                nameplate = "nameplate-capacity-mw",
                winter    = "net-winter-capacity-mw")

  period <- sprintf("%d-12", year)
  cap <- essp.gather("generators", freq = "monthly", start = period, end = period,
                     facets = list(balancing_authority_code = ba),
                     max_rows = 1e6, verbose = FALSE)
  if (!nrow(cap)) rlang::abort(paste0("No generators found for BA \"", ba, "\"."))
  if (!is.null(status)) cap <- cap[cap$status %in% status, , drop = FALSE]

  mw <- sum(suppressWarnings(as.numeric(cap[[col]])), na.rm = TRUE)

  ld <- analyze.load(ba, sprintf("%d-01-01T00", year), sprintf("%d-12-31T23", year))

  tibble::tibble(
    respondent    = ba,
    year          = year,
    capacity_mw   = mw,
    peak_mw       = ld$peak_mw,
    peak_period   = ld$peak_period,
    margin_mw     = mw - ld$peak_mw,
    margin_pct    = (mw - ld$peak_mw) / ld$peak_mw * 100,
    basis         = basis
  )
}

#' Net interchange as a share of demand
#'
#' How much of a balancing authority's load is met by imports from neighbours.
#' Positive net interchange means a net exporter, negative a net importer.
#'
#' @inheritParams analyze.load
#'
#' @return A one-row tibble of demand, net interchange, and import dependence.
#'
#' @examples
#' \dontrun{
#' analyze.imports("CISO", "2024-01-01T00", "2024-12-31T23")
#' }
#'
#' @export
analyze.imports <- function(ba, start, end = NULL, local = TRUE) {
  .w <- essp_when_window(start, end); start <- .w$start; end <- .w$end
  d  <- fetch_hourly(ba, start, end, type = "D",  local = local)
  ti <- fetch_hourly(ba, start, end, type = "TI", local = local)

  demand <- sum(d$mw)
  net    <- sum(ti$mw)

  tibble::tibble(
    respondent        = ba,
    demand_mwh        = demand,
    net_interchange   = net,
    # Negative interchange is inbound, so flip the sign to read as dependence.
    import_share      = -net / demand * 100,
    direction         = if (net < 0) "net importer" else "net exporter",
    hours             = nrow(d)
  )
}
