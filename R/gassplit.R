# Splitting the natural gas band ------------------------------------------------
#
# Form 930 reports natural gas as a single hourly series, so a demand curve
# built from it alone cannot show the combined-cycle / peaker distinction the
# briefs rely on. Two other official sources carry that split, and combining
# them reconstructs it without inventing anything:
#
#   Form 860  operating-generator-capacity, prime_mover_code + balancing
#             authority -> installed MW of each gas plant type
#   Form 923  facility-fuel, primeMover -> generation actually produced by
#             each plant type
#
# The prime-mover codes are easy to get backwards, and doing so inverts the
# chart's meaning:
#
#   CA, CT, CS   combined cycle. CT is the combustion-turbine PART of a
#                combined-cycle plant, not a peaker.
#   GT, IC       simple-cycle turbines and reciprocating engines -- the actual
#                peaking fleet.
#   ST           gas-fired steam turbines, mostly older units.

#' Natural gas prime-mover groups
#'
#' The mapping from EIA prime-mover codes to the plant types a demand curve
#' distinguishes.
#'
#' @return A tibble of `group`, `codes`, and `role`.
#'
#' @examples
#' essp.gasgroups()
#'
#' @export
essp.gasgroups <- function() {
  tibble::tribble(
    ~group,       ~codes,       ~role,
    "NGCC",       "CA,CT,CS",   "Combined cycle: efficient, runs baseload and intermediate",
    "NG Steam",   "ST",         "Gas-fired steam turbine, generally older units",
    "NGCT",       "GT,IC",      "Simple-cycle turbines and engines, run at peak"
  )
}

# Sum a gathered tibble's numeric column over a set of prime-mover codes,
# skipping EIA's "ALL" aggregate row so components are not double-counted.
sum_by_codes <- function(data, col, pm_col, codes) {
  v <- suppressWarnings(as.numeric(data[[col]]))
  keep <- data[[pm_col]] %in% codes & !is.na(v)
  sum(v[keep])
}

#' Gas fleet capacity and generation by plant type
#'
#' Installed capacity from Form 860 and generation from Form 923, grouped into
#' combined cycle, steam, and peakers.
#'
#' @param state State code(s), or `NULL` for the whole country.
#' @param ba Balancing authority code, as an alternative to `state`.
#' @param year Calendar year.
#'
#' @return A tibble of `group`, `capacity_mw`, `generation_mwh`, and each as a
#'   share of the gas fleet, plus an implied capacity factor.
#'
#' @examples
#' \dontrun{
#' analyze.gasfleet(ba = "CISO", year = 2024)
#' }
#'
#' @export
analyze.gasfleet <- function(state = NULL, ba = NULL, year) {
  if (is.null(state) && is.null(ba)) state <- "US"
  groups <- essp.gasgroups()

  cap_args <- list("generators", freq = "monthly",
                   start = sprintf("%d-12", year), end = sprintf("%d-12", year),
                   fuel = "NG", max_rows = 1e6, verbose = FALSE)
  if (!is.null(ba)) {
    cap_args$facets <- list(balancing_authority_code = ba)
  } else if (!identical(state, "US")) {
    cap_args$state <- state
  }
  cap <- do.call(essp.gather, cap_args)
  cap <- cap[cap$status == "OP", , drop = FALSE]

  gen_args <- list("plants", years = year, freq = "annual", fuel = "NG",
                   max_rows = 5e5, verbose = FALSE)
  # facility-fuel has no balancing-authority facet, so a BA request is served
  # by the states its generators sit in.
  gen_state <- if (!is.null(ba)) unique(cap$stateid) else state
  if (!identical(gen_state, "US")) gen_args$state <- gen_state
  gen <- do.call(essp.gather, gen_args)

  rows <- lapply(seq_len(nrow(groups)), function(i) {
    codes <- trimws(strsplit(groups$codes[i], ",", fixed = TRUE)[[1]])
    data.frame(
      group          = groups$group[i],
      capacity_mw    = sum_by_codes(cap, "nameplate-capacity-mw", "prime_mover_code", codes),
      generation_mwh = sum_by_codes(gen, "generation", "primeMover", codes),
      stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, rows)

  out$capacity_share   <- out$capacity_mw / sum(out$capacity_mw) * 100
  out$generation_share <- out$generation_mwh / sum(out$generation_mwh) * 100

  # Routes do not agree on units: electric-power-operational-data reports
  # generation in THOUSAND megawatthours while facility-fuel reports plain
  # megawatthours. Assuming either one produces a capacity factor wrong by a
  # factor of 1,000, so read the unit the response declares.
  units <- api_units(gen, "generation")
  scale <- if (!is.na(units) && grepl("thousand", units, ignore.case = TRUE)) 1000 else 1
  out$capacity_factor <- (out$generation_mwh * scale) /
    (out$capacity_mw * hours_in_year(year)) * 100
  out$capacity_factor[!is.finite(out$capacity_factor)] <- NA_real_

  attr(out, "essp_generation_units") <- units
  tibble::as_tibble(out)
}

#' Split the hourly gas band into combined cycle, steam, and peakers
#'
#' Form 930 reports natural gas hourly as one series. This allocates it across
#' plant types using installed capacity from Form 860 and generation from
#' Form 923, so the combined-cycle / peaker distinction rests on published data
#' rather than assumption.
#'
#' Allocation follows dispatch order rather than a flat proportion. Efficient
#' combined-cycle plant runs first and fills the bottom of the gas band; steam
#' units take what is left; simple-cycle peakers pick up only the remainder at
#' the top, which is why they appear as a wedge under the evening peak instead
#' of a constant sliver all day.
#'
#' Each type's ceiling is its installed capacity scaled by `availability`, since
#' no fleet runs every unit at nameplate simultaneously. Where hourly gas
#' exceeds every ceiling combined, the excess is assigned to peakers and a
#' warning is issued rather than the total being quietly changed.
#'
#' @param data Output of [analyze.fuelshape()] containing an `NG` row.
#' @param ba Balancing authority, used to scope the capacity lookup.
#' @param year Calendar year for the capacity and generation vintages.
#' @param availability Fraction of nameplate treated as available. Defaults to
#'   0.55. This is an editorial heuristic, not a measured availability: it sets
#'   the combined-cycle ceiling low enough that simple-cycle peakers pick up a
#'   visible wedge under the evening ramp, matching how the reference figure
#'   reads. Raise it toward a fleet's true summer availability (~0.85) if you
#'   would rather all gas sit in NGCC until CC capacity is genuinely exhausted.
#' @param fleet Optional precomputed [analyze.gasfleet()] result, to avoid
#'   re-fetching.
#'
#' @return `data` with the `NG` row replaced by `NGCC`, `NG Steam`, and
#'   `NGCT` rows.
#'
#' @examples
#' \dontrun{
#' analyze.fuelshape("CISO", "2024-07-01T00", "2024-07-31T23") |>
#'   analyze.gassplit(ba = "CISO", year = 2024) |>
#'   chart.demandcurve(highlight = "NGCC")
#' }
#'
#' @export
analyze.gassplit <- function(data, ba, year, availability = 0.55, fleet = NULL) {
  if (!"fueltype" %in% names(data)) rlang::abort("`data` needs a `fueltype` column.")
  ng <- data[data$fueltype == "NG", , drop = FALSE]
  if (!nrow(ng)) rlang::abort("No `NG` rows found to split.")

  if (is.null(fleet)) fleet <- analyze.gasfleet(ba = ba, year = year)
  if (!sum(fleet$capacity_mw)) {
    rlang::abort(paste0("No gas capacity found for balancing authority \"", ba, "\"."))
  }

  # `availability` is an editorial knob, not a physical constant: it scales the
  # combined-cycle ceiling so peakers surface as a visible band at the evening
  # peak, the way the reference figure reads. See the parameter docs.
  ceilings <- stats::setNames(fleet$capacity_mw * availability, fleet$group)
  # Dispatch order: cheapest and most efficient first, peakers last.
  order_groups <- c("NGCC", "NG Steam", "NGCT")
  ceilings <- ceilings[order_groups]
  ceilings[is.na(ceilings)] <- 0

  ng <- ng[order(ng$hour), , drop = FALSE]
  remaining <- ng$mean_mw
  parts <- list()

  for (g in order_groups) {
    take <- pmin(remaining, ceilings[[g]])
    remaining <- remaining - take
    row <- ng
    row$fueltype  <- g
    row$fuel_name <- g
    row$mean_mw   <- take
    parts[[g]] <- row
  }

  if (any(remaining > 1)) {
    warning(sprintf(
      "Hourly gas exceeds available capacity in %d hour(s) by up to %.0f MW; ",
      sum(remaining > 1), max(remaining)),
      "the excess is assigned to peakers. Consider raising `availability`.",
      call. = FALSE)
    parts[["NGCT"]]$mean_mw <- parts[["NGCT"]]$mean_mw + remaining
  }

  out <- rbind(data[data$fueltype != "NG", , drop = FALSE], do.call(rbind, parts))
  attr(out, "essp_gasfleet") <- fleet
  tibble::as_tibble(out)
}
