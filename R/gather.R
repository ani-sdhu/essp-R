# The gatherer -----------------------------------------------------------------
#
# essp.gather() wraps eia::eia_data() with the four guards that package leaves
# to the caller. Each exists because of a specific, observed failure mode:
#
#   1. data[]        eia_data(data = NULL) returns rows of period and facet
#                    labels with NO measurements and NO error. Auto-filling the
#                    column list from route metadata is what makes results
#                    contain values at all.
#   2. pagination    The API caps a response at 5,000 rows and reports the true
#                    row count separately. eia_data() exposes length/offset but
#                    does not loop, so a large query silently truncates.
#   3. throttling    EIA suspends API keys that exceed per-second/per-hour
#                    limits. Paging without pauses is exactly the access
#                    pattern that trips it.
#   4. dates         Period filters compare lexically against period stamps, so
#                    a day-precision bound drops the first period of a monthly
#                    or annual series.

#' Gather energy data from the EIA API
#'
#' The single entry point for data acquisition. Resolves a plain-English topic
#' to an EIA route, reads that route's metadata, requests every available
#' measurement column, translates `state`/`fuel`/`sector` into the facet names
#' that route actually uses, pages through results beyond the API's 5,000-row
#' cap, throttles between requests, and records where the data came from.
#'
#' @param topic A topic name (`"generation"`), an EIA form number (`"923"`), or
#'   a raw route path (`"electricity/retail-sales"`) for datasets the topic map
#'   does not cover. See [essp.topics()].
#' @param state State or region code(s), e.g. `"GA"` or `c("GA", "OH")`. Mapped
#'   to whichever facet the route uses for geography.
#' @param years Calendar years to fetch, e.g. `2023` or `2019:2024`. Converted
#'   to the route's own period format.
#' @param freq Frequency id such as `"monthly"` or `"annual"`. Defaults to the
#'   route's own default.
#' @param ba Balancing authority code(s) for hourly routes, e.g. `"CISO"`.
#' @param fuel Fuel or energy source code(s).
#' @param sector Sector code(s), e.g. `"RES"`.
#' @param columns Measurement columns to request. Defaults to every column the
#'   route offers, which is almost always what you want.
#' @param facets Additional facets as a named list, for anything without a
#'   dedicated argument (e.g. `list(prime_mover_code = "CT")`).
#' @param start,end Explicit period bounds in the route's own format. Override
#'   `years` when supplied.
#' @param max_rows Stop after roughly this many rows. Guards against
#'   accidentally pulling millions.
#' @param throttle Seconds to pause between paged requests.
#' @param cache Passed to [eia::eia_data()].
#' @param verbose Report progress while paging.
#'
#' @return A tibble. Source metadata is attached as an `essp_provenance`
#'   attribute; see [essp.provenance()]. Its `total` element is best-effort:
#'   EIA reports the full row count only inside a warning, which is not
#'   re-emitted on a cache hit, so `total` is `NA` for cached queries.
#'
#' @examples
#' \dontrun{
#' essp.gather("retail", state = "GA", years = 2023, freq = "annual")
#' essp.gather("hourly", ba = "CISO", years = 2024)
#' }
#'
#' @export
essp.gather <- function(topic,
                        state    = NULL,
                        years    = NULL,
                        freq     = NULL,
                        ba       = NULL,
                        fuel     = NULL,
                        sector   = NULL,
                        columns  = NULL,
                        facets   = NULL,
                        start    = NULL,
                        end      = NULL,
                        max_rows = 250000,
                        throttle = 0.3,
                        cache    = TRUE,
                        verbose  = TRUE) {

  # Accept a bare `all` as well as "ALL"/"all": essp.gather(all) is the natural
  # thing to type, but unquoted `all` is base::all(), so catch the symbol here
  # before the promise is forced. A variable literally named `all` is the only
  # thing this could surprise, and there is no meaningful essp.gather(all)
  # otherwise, so treating it as the bulk loader matches intent.
  .te <- substitute(topic)
  if (is.symbol(.te) && tolower(as.character(.te)) == "all") topic <- "ALL"

  # Bulk working-set loader. "ALL" is a convenience: it caches the national
  # annual snapshots the analyses build on, so later calls read from disk.
  # Per-state detail and hourly data stay fetch-on-demand (cached on first use)
  # -- pulling every state's hourly series here would be gigabytes and risks
  # the key suspension the throttle guard exists to avoid.
  if (length(topic) == 1L && toupper(topic) == "ALL") {
    return(essp_gather_all(years = years, throttle = throttle,
                           verbose = verbose, cache = cache))
  }

  hit <- essp.resolve(topic)
  if (nrow(hit) > 1L) {
    rlang::abort(c(
      paste0("\"", topic, "\" maps to ", nrow(hit), " routes."),
      i = paste0("Matches: ", paste(hit$route, collapse = ", ")),
      i = "Gather them one at a time, or pass a route path directly."
    ))
  }
  route <- hit$route[[1]]

  # --- guard 1: never let data[] default to empty ----------------------------
  if (is.null(columns)) {
    columns <- essp_data_columns(route)
    if (!length(columns)) {
      rlang::abort(c(
        paste0("Route \"", route, "\" reports no data columns."),
        i = "It is probably a parent node rather than a data endpoint.",
        i = "Call eia::eia_dir() on it to list its children."
      ))
    }
  }

  # --- frequency -------------------------------------------------------------
  available <- essp_frequencies(route)
  if (is.null(freq)) {
    freq <- if (length(available)) available[[1]] else NULL
  } else if (length(available) && !freq %in% available) {
    rlang::abort(c(
      paste0("Frequency \"", freq, "\" is not available on \"", route, "\"."),
      i = paste0("Available: ", paste(available, collapse = ", "))
    ))
  }

  # --- guard 4: bounds in the route's own period format ----------------------
  if (is.null(start) && is.null(end) && !is.null(years)) {
    rng <- essp_period_range(years, freq %||% "annual")
    start <- rng$start
    end   <- rng$end
  }

  # --- facet translation -----------------------------------------------------
  f <- list()
  add_facet <- function(f, role, value) {
    if (is.null(value)) return(f)
    name <- hit[[paste0(role, "_facet")]][[1]]
    if (is.na(name)) {
      rlang::abort(c(
        paste0("Route \"", route, "\" has no ", role, " facet."),
        i = paste0("Its facets are: ", paste(essp_facet_ids(route), collapse = ", "))
      ))
    }
    f[[name]] <- value
    f
  }

  f <- add_facet(f, "geo",    state)
  f <- add_facet(f, "fuel",   fuel)
  f <- add_facet(f, "sector", sector)
  # Hourly routes key geography by balancing authority, which occupies the same
  # facet slot as state does elsewhere.
  if (!is.null(ba)) f <- add_facet(f, "geo", ba)
  if (!is.null(facets)) f <- utils::modifyList(f, facets)
  if (!length(f)) f <- NULL

  # --- guards 2 and 3: page, with pauses -------------------------------------
  page_size <- 5000L
  offset    <- 0L
  pages     <- list()
  total     <- NA_integer_
  warned_truncation <- FALSE

  repeat {
    # eia_data() attaches no attributes, so the only place the true row count
    # appears is inside a warning ("Rows returned: N / Rows available: M").
    # Capture it -- it is what makes max_rows and the size report meaningful --
    # and muffle it, since otherwise every page emits one.
    #
    # Best-effort by nature: the warning is not re-emitted on a cache hit, so
    # `total` stays NA for cached queries. Paging does not depend on it (a
    # short page ends the loop), and truncation falls back to a plain
    # "stopped at" warning.
    page_total <- NA_integer_
    chunk <- withCallingHandlers(
      tryCatch(
        eia::eia_data(
          dir = route, data = columns, facets = f, freq = freq,
          start = start, end = end, length = page_size, offset = offset,
          tidy = TRUE, cache = cache
        ),
        error = function(e) {
          # A filter matching nothing makes eia_data() throw, and its message
          # blames "temporal inputs" regardless of the real cause -- which is
          # usually an unmatched facet value, not the date range. An empty
          # result is a legitimate outcome, so report what was actually asked
          # for and let the caller carry on with zero rows.
          if (grepl("No data available", conditionMessage(e), fixed = TRUE)) {
            warning(sprintf(
              "No data returned for %s%s. Check the filter values and period.",
              route,
              if (length(f)) {
                paste0(" with ", paste(names(f),
                  vapply(f, function(v) paste(v, collapse = "/"), character(1)),
                  sep = "=", collapse = ", "))
              } else ""
            ), call. = FALSE)
            return(NULL)
          }
          stop(e)
        }
      ),
      warning = function(w) {
        msg <- conditionMessage(w)
        hit <- regmatches(msg, regexpr("Rows available:\\s*[0-9]+", msg))
        if (length(hit)) {
          page_total <<- as.integer(gsub("[^0-9]", "", hit))
          invokeRestart("muffleWarning")
        }
      }
    )

    n <- if (is.null(chunk)) 0L else nrow(chunk)
    if (is.na(total) && !is.na(page_total)) {
      total <- page_total
      if (verbose) {
        message(sprintf("%s: %s rows available", route, format(total, big.mark = ",")))
      }
      if (total > max_rows) {
        # Warn once, up front, where the true size is known -- that is the
        # number the caller needs in order to set max_rows correctly. The
        # later "stopped early" warning would just restate this event.
        warned_truncation <- TRUE
        warning(sprintf(
          "%s rows available but max_rows is %s; the result will be truncated.",
          format(total, big.mark = ","), format(max_rows, big.mark = ",")
        ), call. = FALSE)
      }
    }
    if (n == 0L) break

    pages[[length(pages) + 1L]] <- chunk
    offset <- offset + n

    if (n < page_size) break
    if (offset >= max_rows) {
      # Only reachable without a prior warning when the total was unavailable.
      if (!warned_truncation) {
        warning(sprintf(
          "Stopped at %s rows (max_rows). Narrow the query or raise max_rows.",
          format(offset, big.mark = ",")
        ), call. = FALSE)
      }
      break
    }
    if (verbose) message(sprintf("  fetched %s rows...", format(offset, big.mark = ",")))
    Sys.sleep(throttle)
  }

  out <- if (length(pages)) dplyr::bind_rows(pages) else tibble::tibble()

  attr(out, "essp_provenance") <- list(
    route     = route,
    topic     = hit$topic[[1]],
    forms     = hit$forms[[1]],
    facets    = f,
    columns   = columns,
    frequency = freq,
    start     = start,
    end       = end,
    rows      = nrow(out),
    total     = total,
    fetched   = Sys.time()
  )

  out
}

#' Read the source metadata attached to a gathered tibble
#'
#' @param data A tibble returned by [essp.gather()].
#' @return A list describing the request that produced `data`, or `NULL`.
#' @export
essp.provenance <- function(data) {
  attr(data, "essp_provenance", exact = TRUE)
}

# Bounded working-set loader behind essp.gather("ALL"). Caches the national
# annual snapshots the analyses build on; everything else loads on demand.
essp_gather_all <- function(years = NULL, throttle = 0.5, verbose = TRUE,
                            cache = TRUE) {
  if (is.null(years)) {
    y1 <- essp_latest_year()
    years <- (y1 - 4L):y1
  }
  dir <- tools::R_user_dir("ESSP", "cache")
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)

  # topic -> frequency for the national working set.
  jobs <- list(
    generation = "annual",
    generators = "monthly",
    retail     = "annual",
    emissions  = "annual"
  )

  if (verbose) {
    message("Caching the national working set (", min(years), "-", max(years),
            ") to:\n  ", dir,
            "\nPer-state and hourly data load on demand via essp.get().")
  }

  written <- character()
  for (t in names(jobs)) {
    path <- file.path(dir, sprintf("%s_%d_%d.csv", t, min(years), max(years)))
    if (verbose) message("  ", t, " (", jobs[[t]], ") ...", appendLF = FALSE)
    ok <- tryCatch({
      essp.snapshot(path, essp.gather(
        t, years = years, freq = jobs[[t]],
        max_rows = 1e6, throttle = throttle, cache = cache, verbose = FALSE))
      TRUE
    }, error = function(e) {
      if (verbose) message(" failed: ", conditionMessage(e))
      FALSE
    })
    if (ok) {
      if (verbose) message(" done")
      written <- c(written, path)
    }
  }
  if (verbose) message("Wrote ", length(written), " of ", length(jobs), " snapshots.")
  invisible(written)
}

`%||%` <- function(x, y) if (is.null(x)) y else x
