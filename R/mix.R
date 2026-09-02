# Capacity and generation mix --------------------------------------------------

#' Installed capacity by resource
#'
#' Aggregates the Form 860 generator inventory to resource-level capacity for a
#' single point in time.
#'
#' The inventory is a monthly *snapshot*, not a flow, so this reads one month
#' (December of `year` by default) rather than averaging twelve. Summing all
#' twelve months would multiply the fleet by twelve.
#'
#' @param state Two-letter state code, or `"US"` for the national fleet
#'   (default). Note the inventory has no `"US"` facet value -- a national
#'   figure is the sum over all states, which is what omitting the filter does.
#' @param year Calendar year.
#' @param month Month of the snapshot, `1`-`12`. Defaults to December, or the
#'   latest month available if the year is incomplete.
#' @param basis `"nameplate"` (default), `"summer"`, or `"winter"` capacity.
#' @param status Operating status codes to include. Defaults to `"OP"`
#'   (operating), which excludes planned, standby, and retired units.
#' @param solar Passed to [essp.fuelgroups()].
#' @param groups Resource groupings; defaults to [essp.fuelgroups()].
#'
#' @return A tibble of `resource`, `mw`, and `share` (percent).
#'
#' @examples
#' \dontrun{
#' analyze.capacity("US", 2024)
#' analyze.capacity("GA", 2024, basis = "summer")
#' }
#'
#' @export
analyze.capacity <- function(state  = "US",
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
  col    <- switch(basis,
                   nameplate = "nameplate-capacity-mw",
                   summer    = "net-summer-capacity-mw",
                   winter    = "net-winter-capacity-mw")

  raw <- essp.gather(
    "generators",
    state    = if (identical(state, "US")) NULL else state,
    freq     = "monthly",
    start    = period, end = period,
    max_rows = 1e6,
    verbose  = FALSE
  )

  if (!nrow(raw)) {
    rlang::abort(paste0("No generator inventory returned for ", period, "."))
  }

  if (!is.null(status)) raw <- raw[raw$status %in% status, , drop = FALSE]

  mw <- suppressWarnings(as.numeric(raw[[col]]))
  ok <- !is.na(mw)
  res <- group_capacity_codes(raw$energy_source_code[ok], groups)

  agg <- stats::aggregate(list(mw = mw[ok]), by = list(resource = res), FUN = sum)
  agg <- order_resources(agg, groups)
  agg$share <- agg$mw / sum(agg$mw) * 100

  attr(agg, "essp_basis")  <- basis
  attr(agg, "essp_period") <- period
  attr(agg, "essp_units")  <- api_units(raw, col)
  tibble::as_tibble(agg)
}

#' Net generation by resource
#'
#' Aggregates Form 923 operational data to resource-level generation.
#'
#' EIA's fuel codes mix aggregates with their own components, so this selects
#' one non-overlapping code per resource and derives `"Other"` as the residual
#' against the `ALL` total rather than summing leftovers -- adding codes
#' indiscriminately double-counts heavily.
#'
#' @param state Two-letter state code, or `"US"` (default).
#' @param year Calendar year.
#' @param sector EIA sector id. Defaults to `"99"` (all sectors).
#' @param solar `"utility"` (default) counts only utility-scale solar (`SUN`),
#'   matching the coverage of the Form 860 capacity inventory. `"total"` uses
#'   `TSN`, which adds estimated small-scale distributed PV -- larger, but no
#'   longer comparable like-for-like against capacity.
#' @param groups Resource groupings; defaults to [essp.fuelgroups()].
#'
#' @return A tibble of `resource`, `mwh` (thousand megawatthours), and `share`.
#'
#' @examples
#' \dontrun{
#' analyze.generation("US", 2024)
#' }
#'
#' @export
analyze.generation <- function(state  = "US",
                               year   = NULL,
                               sector = "99",
                               solar  = c("utility", "total"),
                               groups = NULL) {
  if (is.null(year)) year <- essp_latest_year()
  .exp <- essp_expand(state, year)
  if (.exp$multi) return(essp_stack(match.call(), .exp, "state", parent.frame()))
  state <- .exp$where; year <- .exp$years

  solar <- match.arg(solar)
  if (is.null(groups)) groups <- essp.fuelgroups(solar = solar)

  raw <- essp.gather("generation", state = state, years = year,
                     freq = "annual", sector = sector, verbose = FALSE)
  if (!nrow(raw)) {
    rlang::abort(paste0("No generation data returned for ", state, " ", year, "."))
  }

  val <- function(code) {
    v <- suppressWarnings(as.numeric(raw$generation[raw$fueltypeid == code]))
    v <- v[!is.na(v)]
    if (length(v)) sum(v) else NA_real_
  }

  total <- val("ALL")
  if (is.na(total)) {
    rlang::abort("The 'ALL' total is missing, so shares cannot be computed.")
  }

  picked <- vapply(groups$generation_code, val, numeric(1))
  agg <- data.frame(resource = groups$resource, mwh = unname(picked),
                    stringsAsFactors = FALSE)
  agg <- agg[!is.na(agg$mwh), , drop = FALSE]

  # Residual, not a sum of leftovers -- see the note above about overlap.
  agg <- rbind(agg, data.frame(resource = "Other",
                               mwh = total - sum(agg$mwh),
                               stringsAsFactors = FALSE))
  agg <- order_resources(agg, groups)
  agg$share <- agg$mwh / total * 100

  attr(agg, "essp_total") <- total
  attr(agg, "essp_units") <- api_units(raw, "generation")
  tibble::as_tibble(agg)
}

#' Capacity and generation mix, aligned
#'
#' Combines [analyze.capacity()] and [analyze.generation()] into the long
#' two-metric shape the fleet-makeup figure consumes: one row per resource per
#' metric, with each metric's shares summing to 100.
#'
#' The contrast between the two columns is the point of the figure -- resources
#' with low capacity factors (solar, wind) occupy more of the capacity bar than
#' the generation bar, and baseload resources (nuclear) the reverse.
#'
#' @inheritParams analyze.capacity
#' @param sector EIA sector id for the generation side. Defaults to `"99"`.
#' @param solar `"utility"` (default) keeps both metrics on utility-scale
#'   coverage, so the comparison is like-for-like. `"total"` adds estimated
#'   small-scale distributed PV to *generation only* -- Form 860 does not
#'   inventory sub-1 MW systems, so capacity cannot match it. That asymmetry
#'   inflates solar's generation share relative to its capacity share, which
#'   works against what the figure is meant to show; a warning is issued.
#'
#' @return A tibble of `metric` (`"Capacity"`/`"Generation"`), `resource`,
#'   `value`, `units`, and `share`.
#'
#' @examples
#' \dontrun{
#' analyze.mix("US", 2024)
#' analyze.mix("US", 2024, solar = "total")   # reproduces the older figures
#' }
#'
#' @export
analyze.mix <- function(state  = "US",
                        year   = NULL,
                        month  = 12,
                        basis  = c("nameplate", "summer", "winter"),
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

  if (solar == "total") {
    warning(
      "solar = \"total\" counts small-scale distributed PV in generation but ",
      "not in capacity (Form 860 does not inventory sub-1 MW systems), so the ",
      "two bars are not like-for-like.",
      call. = FALSE
    )
  }

  cap <- analyze.capacity(state = state, year = year, month = month,
                          basis = basis, status = status, solar = solar,
                          groups = groups)
  gen <- analyze.generation(state = state, year = year, sector = sector,
                            solar = solar, groups = groups)

  # Units come from EIA's own `<column>-units` field, never from a literal
  # typed here. Generation is reported in THOUSAND megawatthours, which is easy
  # to misread as MWh and understate by a factor of 1,000.
  out <- dplyr::bind_rows(
    tibble::tibble(metric = "Capacity",   resource = cap$resource,
                   value = cap$mw,  units = attr(cap, "essp_units"),
                   share = cap$share),
    tibble::tibble(metric = "Generation", resource = gen$resource,
                   value = gen$mwh, units = attr(gen, "essp_units"),
                   share = gen$share)
  )

  attr(out, "essp_solar") <- solar
  out
}

# Read the unit EIA declares for a column. Every measurement column arrives
# alongside a "<column>-units" companion; trusting that rather than hardcoding
# a string is what keeps "thousand megawatthours" from silently becoming "MWh".
api_units <- function(raw, col) {
  u <- paste0(col, "-units")
  if (!u %in% names(raw)) return(NA_character_)
  vals <- unique(stats::na.omit(raw[[u]]))
  if (!length(vals)) return(NA_character_)
  if (length(vals) > 1L) {
    warning("Column '", col, "' reports multiple units: ",
            paste(vals, collapse = ", "), call. = FALSE)
  }
  vals[[1]]
}

# Keep resources in the map's order, with Other last, so charts are stable.
order_resources <- function(df, groups) {
  lv <- c(groups$resource, "Other")
  df <- df[order(match(df$resource, lv)), , drop = FALSE]
  rownames(df) <- NULL
  df
}
