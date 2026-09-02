# Fleet age, additions, retirements, and diversity ------------------------------

# Pull the snapshot the turnover analyses share, with resource labels attached.
fleet_snapshot <- function(state, year, month, status, groups) {
  period <- sprintf("%d-%02d", year, month)
  raw <- essp.gather("generators",
                     state = if (identical(state, "US")) NULL else state,
                     freq = "monthly", start = period, end = period,
                     max_rows = 1e6, verbose = FALSE)
  if (!nrow(raw)) rlang::abort(paste0("No generator inventory for ", period, "."))
  if (!is.null(status)) raw <- raw[raw$status %in% status, , drop = FALSE]

  raw$resource <- group_capacity_codes(raw$energy_source_code, groups)
  raw$mw <- suppressWarnings(as.numeric(raw[["nameplate-capacity-mw"]]))
  raw
}

# "YYYY-MM" -> numeric year. Returns NA for blanks, which is the normal state
# of the planned-retirement column for most units.
ym_year <- function(x) suppressWarnings(as.integer(substr(as.character(x), 1, 4)))

#' Age of the generating fleet
#'
#' Age distribution by resource, weighted by capacity as well as counted by
#' unit. The two differ sharply where a few large old units sit alongside many
#' small new ones, which is exactly the coal-versus-solar contrast.
#'
#' @inheritParams analyze.capacity
#' @param as_of Year to measure age against. Defaults to `year`.
#'
#' @return A tibble of `resource`, `generators`, `mw`, `mean_age`,
#'   `weighted_mean_age`, `median_age`, `oldest_year`, and `newest_year`.
#'
#' @examples
#' \dontrun{
#' analyze.fleetage("US", 2024)
#' }
#'
#' @export
analyze.fleetage <- function(state  = "US",
                             year   = NULL,
                             as_of  = NULL,
                             month  = 12,
                             status = "OP",
                             solar  = c("utility", "total"),
                             groups = NULL) {
  if (is.null(year)) year <- essp_latest_year()
  .exp <- essp_expand(state, year)
  if (.exp$multi) return(essp_stack(match.call(), .exp, "state", parent.frame()))
  state <- .exp$where; year <- .exp$years

  solar <- match.arg(solar)
  if (is.null(groups)) groups <- essp.fuelgroups(solar = solar)
  if (is.null(as_of)) as_of <- year

  raw <- fleet_snapshot(state, year, month, status, groups)
  raw$online <- ym_year(raw[["operating-year-month"]])
  raw$age <- as_of - raw$online
  raw <- raw[!is.na(raw$age) & !is.na(raw$mw) & raw$age >= 0, , drop = FALSE]

  parts <- lapply(split(raw, raw$resource), function(d) {
    data.frame(
      resource          = d$resource[1],
      generators        = nrow(d),
      mw                = sum(d$mw),
      mean_age          = mean(d$age),
      # Capacity-weighted age answers "how old is the fleet's output",
      # which is the question that matters for retirement exposure.
      weighted_mean_age = sum(d$age * d$mw) / sum(d$mw),
      median_age        = stats::median(d$age),
      oldest_year       = min(d$online),
      newest_year       = max(d$online),
      stringsAsFactors  = FALSE
    )
  })

  tibble::as_tibble(order_resources(do.call(rbind, parts), groups))
}

#' Capacity additions by year
#'
#' Nameplate capacity brought online each year, by resource, read from the
#' operating dates in the current inventory.
#'
#' This is a survivorship view: it counts units that are *still operating* at
#' the snapshot date, so historical years understate what was actually built --
#' anything since retired has vanished from the inventory. Recent years are
#' reliable; distant ones are a floor.
#'
#' @inheritParams analyze.capacity
#' @param years Years of interest. Defaults to the ten years ending at `year`.
#'
#' @return A tibble of `year`, `resource`, `mw`, and `generators`.
#'
#' @examples
#' \dontrun{
#' analyze.additions("US", 2024, years = 2015:2024)
#' }
#'
#' @export
analyze.additions <- function(state  = "US",
                              year   = NULL,
                              years  = NULL,
                              month  = 12,
                              status = "OP",
                              solar  = c("utility", "total"),
                              groups = NULL) {
  if (is.null(year)) year <- essp_latest_year()
  .exp <- essp_expand(state, year)
  if (.exp$multi) return(essp_stack(match.call(), .exp, "state", parent.frame()))
  state <- .exp$where; year <- .exp$years

  solar <- match.arg(solar)
  if (is.null(groups)) groups <- essp.fuelgroups(solar = solar)
  if (is.null(years)) years <- seq(year - 9, year)

  raw <- fleet_snapshot(state, year, month, status, groups)
  raw$online <- ym_year(raw[["operating-year-month"]])
  raw <- raw[!is.na(raw$online) & raw$online %in% years & !is.na(raw$mw), , drop = FALSE]
  if (!nrow(raw)) rlang::abort("No units came online in the requested years.")

  parts <- lapply(split(raw, list(raw$online, raw$resource), drop = TRUE), function(d) {
    data.frame(year = d$online[1], resource = d$resource[1],
               mw = sum(d$mw), generators = nrow(d), stringsAsFactors = FALSE)
  })

  out <- do.call(rbind, parts)
  out <- out[order(out$year, match(out$resource, c(groups$resource, "Other"))), ]
  rownames(out) <- NULL
  tibble::as_tibble(out)
}

#' Announced capacity retirements
#'
#' Capacity with a planned retirement date on file, by year and resource.
#'
#' These are only the retirements operators have *announced* on Form 860. Units
#' with no planned date are excluded, so this is a floor on future retirements,
#' not a forecast.
#'
#' A handful of still-operating units carry planned-retirement dates in the
#' distant past (some as early as the 1950s) -- stale entries rather than real
#' plans. Years before `from` are dropped for that reason; set `from` lower to
#' inspect them.
#'
#' @inheritParams analyze.capacity
#' @param from Earliest retirement year to include. Defaults to `year`, so only
#'   retirements still ahead of the snapshot are counted.
#' @param through Latest retirement year to include. Defaults to ten years out.
#'
#' @return A tibble of `year`, `resource`, `mw`, and `generators`.
#'
#' @examples
#' \dontrun{
#' analyze.retirements("US", 2024)
#' }
#'
#' @export
analyze.retirements <- function(state   = "US",
                                year    = NULL,
                                from    = NULL,
                                through = NULL,
                                month   = 12,
                                status  = "OP",
                                solar   = c("utility", "total"),
                                groups  = NULL) {
  if (is.null(year)) year <- essp_latest_year()
  .exp <- essp_expand(state, year)
  if (.exp$multi) return(essp_stack(match.call(), .exp, "state", parent.frame()))
  state <- .exp$where; year <- .exp$years

  solar <- match.arg(solar)
  if (is.null(groups)) groups <- essp.fuelgroups(solar = solar)
  if (is.null(from))    from    <- year
  if (is.null(through)) through <- year + 10

  raw <- fleet_snapshot(state, year, month, status, groups)
  raw$retire <- ym_year(raw[["planned-retirement-year-month"]])

  # Some operating units carry planned-retirement dates decades in the past;
  # those are stale records, not plans, and would otherwise appear as
  # "retirements" in years long gone.
  raw <- raw[!is.na(raw$retire) & raw$retire >= from &
               raw$retire <= through & !is.na(raw$mw), , drop = FALSE]
  if (!nrow(raw)) {
    return(tibble::tibble(year = integer(0), resource = character(0),
                          mw = numeric(0), generators = integer(0)))
  }

  parts <- lapply(split(raw, list(raw$retire, raw$resource), drop = TRUE), function(d) {
    data.frame(year = d$retire[1], resource = d$resource[1],
               mw = sum(d$mw), generators = nrow(d), stringsAsFactors = FALSE)
  })

  out <- do.call(rbind, parts)
  out <- out[order(out$year, match(out$resource, c(groups$resource, "Other"))), ]
  rownames(out) <- NULL
  tibble::as_tibble(out)
}

#' Concentration of the generation mix
#'
#' How dependent a state is on a small number of fuels, via two standard
#' concentration measures.
#'
#' The Herfindahl-Hirschman Index sums squared percentage shares: 10,000 means
#' a single fuel supplies everything, and lower values mean a more even spread.
#' Shannon entropy rises with diversity instead, and `effective_fuels` converts
#' it back to an intuitive scale -- the number of equally-sized fuels that would
#' give the same entropy.
#'
#' @param state State code, or `"US"`.
#' @param year Calendar year.
#' @param metric `"generation"` (default) or `"capacity"`.
#' @param solar Passed to [essp.fuelgroups()].
#'
#' @return A one-row tibble of `hhi`, `shannon`, `effective_fuels`, and
#'   `top_share`.
#'
#' @examples
#' \dontrun{
#' analyze.diversity("GA", 2024)
#' }
#'
#' @export
analyze.diversity <- function(state = "US",
                              year  = NULL,
                              metric = c("generation", "capacity"),
                              solar  = c("utility", "total")) {
  if (is.null(year)) year <- essp_latest_year()
  .exp <- essp_expand(state, year)
  if (.exp$multi) return(essp_stack(match.call(), .exp, "state", parent.frame()))
  state <- .exp$where; year <- .exp$years

  metric <- match.arg(metric)
  solar  <- match.arg(solar)

  d <- if (metric == "generation") {
    analyze.generation(state = state, year = year, solar = solar)
  } else {
    analyze.capacity(state = state, year = year, solar = solar)
  }

  p <- d$share / 100
  p <- p[!is.na(p) & p > 0]

  tibble::tibble(
    state           = state,
    year            = year,
    metric          = metric,
    hhi             = sum((p * 100)^2),
    shannon         = -sum(p * log(p)),
    effective_fuels = exp(-sum(p * log(p))),
    top_share       = max(p) * 100,
    fuels           = length(p)
  )
}
