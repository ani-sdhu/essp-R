# Topic vocabulary and facet translation ---------------------------------------
#
# The EIA API organizes data by ROUTE, and has no concept of a "form" -- form
# numbers appear only inside route descriptions. Form -> route is therefore
# many-to-many and cannot be derived from the API, so this map is maintained by
# hand. It is the one part of ESSP that rots if EIA reorganizes its tree, which
# is why `essp.topics()` exists: the map is meant to be read and edited, not
# hidden.
#
# The facet columns are the reason this table has to exist at all. There is no
# shared vocabulary across routes -- the geography facet alone is spelled six
# different ways, and two SIBLING routes under state-electricity-profiles
# disagree on the capitalization of it:
#
#   stateid          operating-generator-capacity, retail-sales, emissions
#   stateId          seds, capability
#   location         electric-power-operational-data
#   state            facility-fuel
#   respondent       rto/* (a balancing authority, not a state)
#   duoarea          natural-gas/*
#   countryRegionId  international
#
# `verified` records whether the row was checked against the live API with
# eia::eia_metadata(). Rows marked FALSE are hypotheses and must not be trusted.
# Everything currently TRUE was walked on 2026-08-26.

essp_topic_map <- function() {
  tibble::tribble(
    ~topic,          ~route,                                                        ~forms,           ~geo_facet,        ~fuel_facet,          ~sector_facet,     ~freqs,                        ~period,               ~verified, ~description,
    "generators",    "electricity/operating-generator-capacity",                    "860, 860M",      "stateid",         "energy_source_code", "sector",          "monthly",                     "2008-01..2026-06",     TRUE,     "Generator-level capacity, technology, prime mover, status, online and retirement dates, lat/long",
    "generation",    "electricity/electric-power-operational-data",                 "923",            "location",        "fueltypeid",         "sectorid",        "monthly,quarterly,annual",    "2001-01..2026-06",     TRUE,     "Net generation, fuel consumption, stocks, receipts, and fuel cost by state, sector, and energy source",
    "plants",        "electricity/facility-fuel",                                   "923",            "state",           "fuelType",           NA,                "monthly,quarterly,annual",    "2001-01..2026-06",     TRUE,     "Plant-level net and gross generation and fuel consumption, by prime mover",
    "retail",        "electricity/retail-sales",                                    "826, 861, 861M", "stateid",         NA,                   "sectorid",        "monthly,quarterly,annual",    "2001-01..2026-06",     TRUE,     "Retail sales, revenue, average price, and customer counts by state and sector",
    "hourly",        "electricity/rto/fuel-type-data",                              "930",            "respondent",      "fueltype",           NA,                "hourly,local-hourly",         "2019-01-01..present",  TRUE,     "Hourly net generation by fuel type per balancing authority",
    "demand",        "electricity/rto/region-data",                                 "930",            "respondent",      NA,                   NA,                "hourly,local-hourly",         "2019-01-01..present",  TRUE,     "Hourly demand, day-ahead forecast, net generation, and interchange per balancing authority",
    "interchange",   "electricity/rto/interchange-data",                            "930",            "fromba",          NA,                   NA,                "hourly,local-hourly",         "2019-01-01..present",  TRUE,     "Hourly interchange between neighboring balancing authorities (also uses the 'toba' facet)",
    "emissions",     "electricity/state-electricity-profiles/emissions-by-state-by-fuel", "923",      "stateid",         "fuelid",             NA,                "annual",                      "1990..2024",           TRUE,     "SO2, NOx, and CO2 tonnage and lbs/MWh rates by state and fuel",
    "capability",    "electricity/state-electricity-profiles/capability",           "860",            "stateId",         "energysourceid",     "producertypeid",  "annual",                      "1990..2024",           TRUE,     "Annual generating capability by state, producer type, and energy source",
    "consumption",   "seds",                                                        NA,               "stateId",         NA,                   NA,                "annual",                      "1960..2024",           TRUE,     "State Energy Data System: production, consumption, price, and expenditure for all fuels (series encoded in seriesId)",
    "gasprices",     "natural-gas/pri/sum",                                         "176",            "duoarea",         NA,                   NA,                "monthly,annual",              "1973-01..2026-05",     TRUE,     "Natural gas prices by area, product, and process",
    "outlook",       "aeo/2026",                                                    NA,               "regionId",        NA,                   NA,                "annual",                      "2025..2050",           TRUE,     "Annual Energy Outlook projections. NOTE: aeo is year-versioned -- 'aeo' alone is a parent node, so the release year is part of the route",
    "shortterm",     "steo",                                                        NA,               NA,                NA,                   NA,                "monthly,quarterly,annual",    "1974-01..2027-12",     TRUE,     "Short-Term Energy Outlook 18-month projections (series encoded in seriesId)",
    "totalenergy",   "total-energy",                                                NA,               NA,                NA,                   NA,                "monthly,annual",              "1949-01..present",     TRUE,     "Monthly Energy Review national aggregates (series encoded in msn), including the CPI-U series CPUCIUS used by essp.deflate()",
    "international", "international",                                               NA,               "countryRegionId", "productId",          NA,                "monthly,quarterly,annual",    "1949-01..2026-05",     TRUE,     "Country-level production, consumption, imports, and exports by energy source"
  )
}

# Routes that are parent nodes, not data endpoints. Requesting data from these
# returns empty metadata rather than an error, which is a confusing failure --
# so name them and fail early with something actionable.
essp_parent_routes <- function() {
  c(
    "electricity"                          = "Use a child route: retail-sales, electric-power-operational-data, rto, state-electricity-profiles, operating-generator-capacity, facility-fuel",
    "electricity/state-electricity-profiles" = "Use a child route: emissions-by-state-by-fuel, source-disposition, capability, energy-efficiency, net-metering, meters, summary",
    "electricity/rto"                      = "Use a child route: fuel-type-data, region-data, interchange-data",
    "aeo"                                  = "AEO is year-versioned. Use aeo/<year>, e.g. aeo/2026",
    "natural-gas"                          = "Use a child route: sum, pri, enr, prod, move, stor, cons",
    "natural-gas/pri"                      = "Use a child route: sum, fut, rescom"
  )
}

# Deprecated routes still answer requests but serve stale data, which is worse
# than an error because nothing looks wrong.
essp_deprecated_routes <- function() {
  c(
    "co2-emissions/co2-emissions-aggregates" =
      "Deprecated by EIA and frozen at 2022. Use topic \"emissions\" (state-electricity-profiles/emissions-by-state-by-fuel, current through 2024) or \"consumption\" (seds).",
    "co2-emissions" =
      "Deprecated by EIA. Use topic \"emissions\" or \"consumption\" (seds)."
  )
}

#' List the topics ESSP can gather
#'
#' Prints the topic vocabulary used by [essp.gather()], showing the EIA route
#' and form(s) each topic resolves to, the route's own facet names, its
#' available frequencies, and its period coverage.
#'
#' The EIA API is organized by route, not by form number -- form numbers appear
#' only in route descriptions, and one form's data is often split across several
#' routes. Nor is there a shared facet vocabulary: the geography facet is
#' spelled `stateid`, `stateId`, `location`, `state`, `respondent`, `duoarea`,
#' or `countryRegionId` depending on the route. Neither mapping can be derived
#' from the API, so ESSP maintains both by hand and exposes them here rather
#' than burying them.
#'
#' @param topic Optional topic name to filter to. If `NULL` (default), all
#'   topics are returned.
#' @param verified_only If `TRUE`, return only rows checked against the live
#'   API. Defaults to `FALSE`.
#'
#' @return A tibble with one row per topic.
#'
#' @examples
#' essp.topics()
#' essp.topics("hourly")
#'
#' @export
essp.topics <- function(topic = NULL, verified_only = FALSE) {
  out <- essp_topic_map()

  if (!is.null(topic)) {
    if (!is.character(topic)) {
      rlang::abort("`topic` must be a character vector or NULL.")
    }
    unknown <- setdiff(topic, out$topic)
    if (length(unknown)) {
      rlang::abort(c(
        "Unknown topic.",
        x = paste0("Not in the topic map: ", paste(unknown, collapse = ", ")),
        i = paste0("Available: ", paste(out$topic, collapse = ", "))
      ))
    }
    out <- out[out$topic %in% topic, , drop = FALSE]
  }

  if (isTRUE(verified_only)) out <- out[out$verified, , drop = FALSE]

  out
}

#' Resolve a topic, form number, or route path to an API route
#'
#' Accepts ESSP's plain-English topic vocabulary, a bare EIA form number, or a
#' raw route path, and returns matching rows of the topic map. Raw route paths
#' pass through unchanged so a dataset the map does not yet cover is never
#' unreachable.
#'
#' Errors early on routes known to be parent nodes (which return empty metadata
#' rather than failing) and on routes EIA has deprecated (which return stale
#' data rather than failing) -- both are silent problems otherwise.
#'
#' @param x A topic name (`"hourly"`), an EIA form number (`"860"`), or a route
#'   path (`"electricity/retail-sales"`).
#'
#' @return A tibble of matching topic-map rows. A pass-through route yields a
#'   single row with `topic` and the facet columns set to `NA`.
#'
#' @examples
#' essp.resolve("hourly")
#' # A form number can map to several routes:
#' essp.resolve("923")
#'
#' @export
essp.resolve <- function(x) {
  if (!is.character(x) || length(x) != 1L || is.na(x)) {
    rlang::abort("`x` must be a single non-missing character string.")
  }

  dep <- essp_deprecated_routes()
  if (x %in% names(dep)) {
    rlang::abort(c(paste0("Route \"", x, "\" is deprecated."), i = dep[[x]]))
  }

  par <- essp_parent_routes()
  if (x %in% names(par)) {
    rlang::abort(c(
      paste0("\"", x, "\" is a parent node, not a data route."),
      i = par[[x]]
    ))
  }

  map <- essp_topic_map()

  hit <- map[map$topic == x, , drop = FALSE]
  if (nrow(hit)) return(hit)

  hit <- map[map$route == x, , drop = FALSE]
  if (nrow(hit)) return(hit)

  # Form numbers are many-to-many with routes, so this can match several rows.
  hit <- map[vapply(map$forms, form_matches, logical(1), form = x), , drop = FALSE]
  if (nrow(hit)) return(hit)

  # A path-shaped string we don't know is assumed to be a real route. Trusting
  # it is what keeps unmapped datasets reachable; the API will reject it if it
  # isn't real.
  if (grepl("/", x, fixed = TRUE)) {
    tmpl <- map[0, , drop = FALSE]
    row <- tmpl[NA_integer_, , drop = FALSE]
    row$topic <- NA_character_
    row$route <- x
    row$verified <- FALSE
    row$description <- "Unmapped route, passed through as given"
    return(row)
  }

  rlang::abort(c(
    paste0("Could not resolve \"", x, "\" to an EIA route."),
    i = paste0("Known topics: ", paste(map$topic, collapse = ", ")),
    i = "Pass a route path such as \"electricity/retail-sales\" to bypass the map."
  ))
}

#' Look up a route's name for a facet role
#'
#' Translates a generic role -- `"geo"`, `"fuel"`, or `"sector"` -- into the
#' facet name that a particular route actually uses. EIA has no shared facet
#' vocabulary across routes, so this indirection is what lets callers say
#' `state = "GA"` without knowing whether the route calls it `stateid`,
#' `stateId`, `location`, or `state`.
#'
#' @param x A topic name, form number, or route path, as accepted by
#'   [essp.resolve()].
#' @param role One of `"geo"`, `"fuel"`, or `"sector"`.
#'
#' @return The route's facet name as a string, or `NA_character_` if the route
#'   has no facet for that role.
#'
#' @examples
#' essp.facet("generation", "geo")   # "location"
#' essp.facet("plants", "geo")       # "state"
#' essp.facet("retail", "geo")       # "stateid"
#'
#' @export
essp.facet <- function(x, role = c("geo", "fuel", "sector")) {
  role <- match.arg(role)
  hit <- essp.resolve(x)

  if (nrow(hit) > 1L) {
    rlang::abort(c(
      paste0("\"", x, "\" resolves to ", nrow(hit), " routes, so its facet name is ambiguous."),
      i = paste0("Matches: ", paste(hit$route, collapse = ", ")),
      i = "Pass a single topic or route path instead."
    ))
  }

  hit[[paste0(role, "_facet")]][[1]]
}

# Does a comma-separated `forms` cell contain `form`? Compared case-insensitively
# so "860m" and "860M" both match, and trimmed because the cells are hand-edited.
form_matches <- function(forms, form) {
  if (is.na(forms)) return(FALSE)
  parts <- trimws(strsplit(forms, ",", fixed = TRUE)[[1]])
  tolower(form) %in% tolower(parts)
}
