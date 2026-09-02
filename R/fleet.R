# Capacity factor, fleet composition, and rankings -----------------------------

# Hours in a calendar year, leap years included. At ~1% of the year, ignoring
# February 29 would shift every capacity factor by a visible amount.
hours_in_year <- function(year) {
  leap <- (year %% 4 == 0 & year %% 100 != 0) | (year %% 400 == 0)
  ifelse(leap, 8784, 8760)
}

#' Capacity factor by resource
#'
#' Capacity factor is not published by EIA -- it is derived. This divides
#' actual generation by the maximum a fleet could have produced running flat
#' out for the whole year:
#'
#' \deqn{CF = \frac{generation\ (MWh)}{capacity\ (MW) \times hours}}
#'
#' Two details decide whether the answer is credible. Generation arrives in
#' *thousand* megawatthours and must be scaled. And the capacity basis matters:
#' EIA's own published capacity factors use **net summer** capacity, which is
#' lower than nameplate and therefore yields a higher factor. `basis` defaults
#' to `"summer"` to match that convention -- switching to `"nameplate"` will
#' report visibly lower factors than EIA's published tables.
#'
#' @inheritParams analyze.mix
#'
#' @return A tibble of `resource`, `mw`, `mwh`, and `capacity_factor` (percent).
#'
#' @examples
#' \dontrun{
#' analyze.capacityfactor("US", 2024)
#' }
#'
#' @export
analyze.capacityfactor <- function(state  = "US",
                                   year   = NULL,
                                   month  = 12,
                                   basis  = c("summer", "nameplate", "winter"),
                                   status = "OP",
                                   sector = "99",
                                   solar  = c("utility", "total"),
                                   groups = NULL) {
  if (is.null(year)) year <- essp_latest_year()
  .exp <- essp_expand(state, year)
  if (.exp$multi) return(essp_stack(match.call(), .exp, "state", parent.frame()))
  state <- .exp$where; year <- .exp$years

  basis <- match.arg(basis)
  solar <- match.arg(solar)

  cap <- analyze.capacity(state = state, year = year, month = month,
                          basis = basis, status = status, solar = solar,
                          groups = groups)
  gen <- analyze.generation(state = state, year = year, sector = sector,
                            solar = solar, groups = groups)

  df <- merge(cap[, c("resource", "mw")], gen[, c("resource", "mwh")],
              by = "resource", all = FALSE)

  # Generation is in thousand MWh; capacity in MW. Scale before dividing.
  df$capacity_factor <- (df$mwh * 1000) / (df$mw * hours_in_year(year)) * 100
  df$capacity_factor[!is.finite(df$capacity_factor)] <- NA_real_

  df <- order_resources(df, groups %||% essp.fuelgroups(solar = solar))
  attr(df, "essp_basis") <- basis
  tibble::as_tibble(df)
}

#' Fleet composition from the generator inventory
#'
#' Counts plants and generating units and summarises unit size by resource,
#' from a single Form 860 monthly snapshot.
#'
#' @inheritParams analyze.capacity
#'
#' @return A tibble of `resource`, `plants`, `generators`, `mw`, `mean_mw`, and
#'   `median_mw`.
#'
#' @examples
#' \dontrun{
#' analyze.fleet("GA", 2024)
#' }
#'
#' @export
analyze.fleet <- function(state  = "US",
                          year   = NULL,
                          month  = 12,
                          basis  = c("nameplate", "summer", "winter"),
                          status = "OP",
                          solar  = c("utility", "total"),
                          groups = NULL) {
  if (is.null(year)) year <- essp_latest_year()
  .exp <- essp_expand(state, year)
  if (.exp$multi) return(essp_stack(match.call(), .exp, "state", parent.frame()))
  state <- .exp$where; year <- .exp$years

  basis <- match.arg(basis)
  solar <- match.arg(solar)
  if (is.null(groups)) groups <- essp.fuelgroups(solar = solar)

  period <- sprintf("%d-%02d", year, month)
  col <- switch(basis,
                nameplate = "nameplate-capacity-mw",
                summer    = "net-summer-capacity-mw",
                winter    = "net-winter-capacity-mw")

  raw <- essp.gather("generators",
                     state = if (identical(state, "US")) NULL else state,
                     freq = "monthly", start = period, end = period,
                     max_rows = 1e6, verbose = FALSE)
  if (!nrow(raw)) rlang::abort(paste0("No generator inventory for ", period, "."))
  if (!is.null(status)) raw <- raw[raw$status %in% status, , drop = FALSE]

  raw$resource <- group_capacity_codes(raw$energy_source_code, groups)
  raw$mw <- suppressWarnings(as.numeric(raw[[col]]))

  parts <- lapply(split(raw, raw$resource), function(d) {
    data.frame(
      resource   = d$resource[1],
      # One plant carries many generators, so plants must be counted distinctly.
      plants     = length(unique(d$plantid)),
      generators = nrow(d),
      mw         = sum(d$mw, na.rm = TRUE),
      mean_mw    = mean(d$mw, na.rm = TRUE),
      median_mw  = stats::median(d$mw, na.rm = TRUE),
      stringsAsFactors = FALSE
    )
  })

  out <- order_resources(do.call(rbind, parts), groups)
  attr(out, "essp_period") <- period
  tibble::as_tibble(out)
}

#' Rank states by production of a resource
#'
#' Produces the "N states account for X% of output" claim directly, with the
#' cumulative share carried alongside each rank so the sentence can be written
#' from the table rather than by hand.
#'
#' The `location` facet mixes true states with census regions (`PCC` Pacific
#' Contiguous, `SAT` South Atlantic, ...), the `US` total, and Puerto Rico.
#' Ranking them together double-counts, since a region already contains its
#' member states -- cumulative share then runs past 100%. Only the 50 states
#' and DC are ranked here.
#'
#' @param fuel A resource name as used by [essp.fuelgroups()] (e.g. `"Solar"`),
#'   or a raw EIA `fueltypeid`.
#' @param year Calendar year.
#' @param n Number of states to return. `Inf` returns all.
#' @param sector EIA sector id. Defaults to `"99"` (all sectors).
#' @param solar Passed to [essp.fuelgroups()].
#' @param include_pr Include Puerto Rico, which EIA reports alongside the
#'   states but excludes from its US total. Defaults to `FALSE`.
#'
#' @return A tibble of `rank`, `state`, `mwh`, `share`, and `cumulative_share`.
#'
#' @examples
#' \dontrun{
#' analyze.fuelleaders("Solar", 2024, n = 8)
#' }
#'
#' @export
analyze.fuelleaders <- function(fuel,
                                year       = NULL,
                                n          = 10,
                                sector     = "99",
                                solar      = c("utility", "total"),
                                include_pr = FALSE) {
  if (is.null(year)) year <- essp_latest_year()
  .exp <- essp_expand(fuel, year, need = "fuel")
  if (.exp$multi) return(essp_stack(match.call(), .exp, "fuel", parent.frame()))
  fuel <- .exp$where; year <- .exp$years

  solar  <- match.arg(solar)
  groups <- essp.fuelgroups(solar = solar)

  code <- if (fuel %in% groups$resource) {
    groups$generation_code[groups$resource == fuel]
  } else {
    fuel
  }

  raw <- essp.gather("generation", years = year, freq = "annual",
                     sector = sector, fuel = code, verbose = FALSE)
  if (!nrow(raw)) {
    rlang::abort(paste0("No generation returned for fuel code \"", code, "\"."))
  }

  raw$mwh <- suppressWarnings(as.numeric(raw$generation))
  raw <- raw[!is.na(raw$mwh), , drop = FALSE]

  # The national total is a row in the same response, not the sum of states.
  national <- sum(raw$mwh[raw$location == "US"])

  # Keep only true states. Census regions live in this same facet and already
  # contain their member states, so ranking them together sends cumulative
  # share past 100%.
  keep <- c(datasets::state.abb, "DC", if (isTRUE(include_pr)) "PR")
  st <- raw[raw$location %in% keep, , drop = FALSE]
  if (!nrow(st)) rlang::abort("No state-level rows returned.")

  st <- st[order(-st$mwh), , drop = FALSE]
  denom <- if (length(national) && national > 0) national else sum(st$mwh)

  out <- tibble::tibble(
    rank             = seq_len(nrow(st)),
    state            = st$location,
    state_name       = st$stateDescription,
    mwh              = st$mwh,
    share            = st$mwh / denom * 100,
    cumulative_share = cumsum(st$mwh) / denom * 100
  )

  if (is.finite(n)) out <- utils::head(out, n)
  attr(out, "essp_fuel_code") <- code
  attr(out, "essp_national")  <- denom
  out
}

#' @rdname analyze.fuelleaders
#' @export
analyze.topproducers <- function(...) {
  .Deprecated("analyze.fuelleaders")
  analyze.fuelleaders(...)
}
