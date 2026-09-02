# Fuel grouping ----------------------------------------------------------------
#
# EIA reports fuels at far finer grain than a two-pager can show, and the two
# sides of the fleet-makeup chart use DIFFERENT code sets: capacity comes from
# Form 860 `energy_source_code`, generation from Form 923 `fueltypeid`. Rolling
# both up to the same handful of resources is what makes them comparable.
#
# The generation code set is the trickier one: it mixes aggregates (ALL, FOS,
# REN, COW, TSN) with their own components (BIT, SUB, LIG, SPV, DPV). Summing
# indiscriminately double-counts badly -- FOS alone is 59% of ALL. The map
# below therefore picks ONE non-overlapping code per resource and derives
# "Other" as the residual against ALL, rather than adding up leftovers.

#' Resource groupings used by the mix analyses
#'
#' The default seven resources shown on the fleet-makeup figure, and the EIA
#' codes that roll up into each on the capacity and generation sides.
#'
#' @param solar Which solar series the generation side should use.
#'   `"utility"` uses `SUN` (utility-scale only, matching the coverage of the
#'   Form 860 capacity data). `"total"` uses `TSN`, which adds estimated
#'   small-scale distributed PV.
#'
#' @return A tibble with columns `resource`, `capacity_codes` (Form 860
#'   `energy_source_code` values) and `generation_code` (a single Form 923
#'   `fueltypeid`).
#'
#' @examples
#' essp.fuelgroups()
#' essp.fuelgroups(solar = "total")
#'
#' @export
essp.fuelgroups <- function(solar = c("utility", "total")) {
  solar <- match.arg(solar)

  tibble::tribble(
    ~resource,      ~capacity_codes,                          ~generation_code,
    "Natural Gas",  "NG",                                     "NG",
    "Coal",         "BIT,SUB,LIG,ANT,WC,RC,SGC,SC",           "COW",
    "Wind",         "WND",                                    "WND",
    "Solar",        "SUN",                                    if (solar == "total") "TSN" else "SUN",
    "Nuclear",      "NUC",                                    "NUC",
    "Hydro",        "WAT",                                    "HYC"
  )
}

# Map a vector of Form 860 energy source codes onto resource names, with
# anything unlisted falling into "Other".
group_capacity_codes <- function(codes, groups) {
  lookup <- stats::setNames(
    rep(groups$resource, lengths(strsplit(groups$capacity_codes, ",", fixed = TRUE))),
    unlist(strsplit(groups$capacity_codes, ",", fixed = TRUE))
  )
  out <- unname(lookup[codes])
  out[is.na(out)] <- "Other"
  out
}
