# The single-verb front door. Because every metric command now parses plain
# `where`/`when` strings itself (see stack.R / parse.R), essp.get() is a thin
# router: it maps a plain-English `what` to the right metric, validates the
# geography kind up front for a helpful error, and forwards the two strings.

#' The routing table behind [essp.get()]
#'
#' One row per `what`: the metric it calls, whether `where` means a state, a
#' balancing authority, or a fuel, and the time grain. Edit this table -- same
#' pattern as `essp_topic_map()` -- to add or rename a `what`.
#'
#' @return A named list keyed by `what`; each element is
#'   `list(fn=, geo=, time=, desc=)`.
#' @keywords internal
essp_get_map <- function() {
  row <- function(fn, geo, time, desc) list(fn = fn, geo = geo, time = time, desc = desc)
  list(
    # --- state, one year ---------------------------------------------------
    generation     = row(analyze.generation,     "state", "year",   "Net generation by resource"),
    capacity       = row(analyze.capacity,       "state", "year",   "Nameplate/summer capacity by resource"),
    mix            = row(analyze.mix,            "state", "year",   "Capacity and generation aligned"),
    capacityfactor = row(analyze.capacityfactor, "state", "year",   "Capacity factor by resource"),
    diversity      = row(analyze.diversity,      "state", "year",   "Fuel-mix concentration (HHI, Shannon)"),
    emissions      = row(analyze.emissions,      "state", "year",   "Emissions tonnage and rate"),
    fleet          = row(analyze.fleet,          "state", "year",   "Plant/generator counts and unit size"),
    fleetage       = row(analyze.fleetage,       "state", "year",   "Fleet age distribution"),
    retirements    = row(analyze.retirements,    "state", "year",   "Retired capacity by year"),
    # --- state, multi-year -------------------------------------------------
    additions      = row(analyze.additions,      "state", "years",  "Added capacity by year"),
    prices         = row(analyze.prices,         "state", "years",  "Retail prices, nominal and real"),
    # --- fuel, one year ----------------------------------------------------
    fuelleaders    = row(analyze.fuelleaders,    "fuel",  "year",   "States ranked by a fuel's output"),
    # --- balancing authority, hourly window --------------------------------
    demand         = row(analyze.load,           "ba",    "window", "Load statistics (peak, min, ramp)"),
    loadshape      = row(analyze.loadshape,      "ba",    "window", "Average demand profile by hour"),
    fuelmix        = row(analyze.fuelshape,      "ba",    "window", "Hourly generation by fuel"),
    duration       = row(analyze.durationcurve,  "ba",    "window", "Load-duration curve"),
    imports        = row(analyze.imports,        "ba",    "window", "Net interchange as a share of load"),
    storage        = row(analyze.storage,        "ba",    "window", "Battery charge/discharge by hour"),
    # --- balancing authority, one year -------------------------------------
    reserve        = row(analyze.reservemargin,  "ba",    "year",   "Capacity vs peak-demand headroom")
  )
}

#' Get analysis-ready data with one verb
#'
#' `essp.get("generation", "CA,MD,DE", "2024,2023")`. `what` decides whether
#' `where` is a state, a balancing authority, or a fuel, so you never have to.
#' `where` and `when` take the same plain strings the metric commands do:
#' `"CA,MD,DE"`, `"ALL"`, `"US"`; `"2024"`, `"2023,2024"`, `"2020-2024"`,
#' `"July 2024"`, `"summer 2024"`, `"2024-07-31"`.
#'
#' @param what A topic from [essp.catalog()].
#' @param where A place string (state(s) or a BA code), or a fuel for
#'   `"fuelleaders"`.
#' @param when A time string. Optional for annual topics (defaults to the
#'   latest complete year); required for the hourly ones.
#'
#' @return The analysis-ready tibble the underlying metric returns.
#'
#' @examples
#' \dontrun{
#' essp.get("mix", "CA,MD,DE", "2024")
#' essp.get("fuelmix", "CISO", "July 2024")
#' }
#'
#' @export
essp.get <- function(what, where = "US", when = NULL) {
  m <- essp_get_map()
  key <- tolower(trimws(what))
  if (!key %in% names(m)) {
    rlang::abort(paste0(
      "Unknown topic \"", what, "\". Options: ",
      paste(names(m), collapse = ", "), ". See essp.catalog()."))
  }
  spec <- m[[key]]

  # Validate the geography kind up front so the error names the problem,
  # rather than letting an empty fetch fail deeper in.
  if (spec$geo == "state") essp_parse_where(where, "state")
  if (spec$geo == "ba")    essp_parse_where(where, "ba")

  if (spec$geo == "ba" && spec$time == "window" && is.null(when)) {
    rlang::abort(paste0(
      "\"", key, "\" needs a time window, e.g. \"July 2024\" or \"summer 2024\"."))
  }

  # The metric parses `where`/`when` itself; forward positionally. A NULL
  # `when` lets an annual metric fall back to its latest-year default.
  if (is.null(when)) spec$fn(where) else spec$fn(where, when)
}

#' The essp.get() vocabulary
#'
#' Lists every `what` [essp.get()] understands, with its geography kind and
#' time grain -- the discoverable, curated command surface.
#'
#' @return A tibble of `what`, `does`, `where`, and `when`.
#' @examples
#' essp.catalog()
#' @export
essp.catalog <- function() {
  m <- essp_get_map()
  tibble::tibble(
    what  = names(m),
    does  = vapply(m, `[[`, character(1), "desc"),
    where = vapply(m, `[[`, character(1), "geo"),
    when  = vapply(m, `[[`, character(1), "time")
  )
}
