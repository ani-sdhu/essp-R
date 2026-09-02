# Plain-string parsers shared by essp.get() and the simplified metric commands.
# Keeping the two doors on one parser is what stops them drifting apart.

#' Most recent complete data year
#'
#' Annual EIA series lag the calendar, so the current year is rarely complete.
#' The default `year` for the metric commands is the prior calendar year.
#'
#' @return An integer year.
#' @keywords internal
essp_latest_year <- function() {
  as.integer(format(Sys.Date(), "%Y")) - 1L
}

#' Parse a plain "where" string into a vector of places
#'
#' `"CA,MD,DE"` becomes `c("CA","MD","DE")`; `"ALL"` becomes every state plus
#' DC; `"US"` stays national. For `need = "ba"` the codes are returned
#' upper-cased and, when the respondent list is reachable, validated with a
#' "did you mean" hint on a miss. For `need = "fuel"` the tokens are returned
#' as-is (title-preserving) and `"ALL"` is not expanded.
#'
#' @param x A string, or a character/factor vector (returned split+trimmed).
#' @param need One of `"state"`, `"ba"`, `"fuel"`.
#'
#' @return A character vector.
#' @keywords internal
essp_parse_where <- function(x, need = c("state", "ba", "fuel")) {
  need <- match.arg(need)
  toks <- unlist(strsplit(as.character(x), ","), use.names = FALSE)
  toks <- trimws(toks)
  toks <- toks[nzchar(toks)]
  if (!length(toks)) rlang::abort("`where` is empty; give a place like \"CA\" or \"US\".")

  if (need == "fuel") return(toks)

  up <- toupper(toks)

  if (need == "state") {
    if (length(up) == 1L && up == "ALL") return(c(datasets::state.abb, "DC"))
    if (length(up) == 1L && up == "US")  return("US")
    bad <- setdiff(up, c(datasets::state.abb, "DC", "US"))
    if (length(bad)) {
      rlang::abort(paste0(
        "Not a state code: ", paste(bad, collapse = ", "),
        ". Use two-letter codes (\"CA\"), \"US\", or \"ALL\"."))
    }
    return(up)
  }

  # need == "ba": validate against the live respondent list when we can reach
  # it; offline (no key / no network) just return the upper-cased codes.
  valid <- tryCatch(essp.respondents()$id, error = function(e) NULL)
  if (!is.null(valid)) {
    bad <- setdiff(up, valid)
    if (length(bad)) {
      hint <- utils::head(grep(substr(bad[1], 1, 2), valid, value = TRUE, ignore.case = TRUE), 5)
      msg <- paste0("Unknown balancing authority: ", paste(bad, collapse = ", "), ".")
      if (length(hint)) msg <- paste0(msg, " Did you mean: ", paste(hint, collapse = ", "), "?")
      rlang::abort(msg)
    }
  }
  up
}

# Month name/abbrev -> number, case-insensitive. Returns NA on no match.
essp_month_num <- function(s) {
  s <- tolower(trimws(s))
  m <- match(s, tolower(month.name))
  if (is.na(m)) m <- match(s, tolower(month.abb))
  m
}

#' Parse a plain "when" string into a period
#'
#' Understands, for `grain = "year"`: `"2024"`, `"2023,2024"`, `"2020-2024"`,
#' or a numeric year. For `grain = "window"` it additionally understands a
#' month (`"2024-07"` or `"July 2024"`), a single day (`"2024-07-31"`), and a
#' season (`"summer 2024"`), returning hourly `start`/`end` stamps.
#'
#' @param x A string (or number) describing the period.
#' @param grain `"year"` for annual metrics, `"window"` for hourly ones.
#'
#' @return For `"year"`: `list(years=)`. For `"window"`:
#'   `list(start=, end=, years=)` with hourly `"YYYY-MM-DDTHH"` bounds.
#' @keywords internal
essp_parse_when <- function(x, grain = c("year", "window")) {
  grain <- match.arg(grain)
  x <- trimws(as.character(x))
  if (!nzchar(x)) rlang::abort("`when` is empty; give a year like \"2024\".")

  # --- year grain: one, list, or range -------------------------------------
  parse_years <- function(s) {
    s <- trimws(s)
    if (grepl(",", s)) {
      y <- suppressWarnings(as.integer(trimws(strsplit(s, ",")[[1]])))
    } else if (grepl("^[0-9]{4}[[:space:]]*-[[:space:]]*[0-9]{4}$", s)) {
      ends <- suppressWarnings(as.integer(trimws(strsplit(s, "-")[[1]])))
      y <- seq(ends[1], ends[2])
    } else if (grepl("^[0-9]{4}$", s)) {
      y <- suppressWarnings(as.integer(s))
    } else {
      return(NULL)
    }
    if (anyNA(y)) NULL else y
  }

  if (grain == "year") {
    y <- parse_years(x)
    if (is.null(y)) {
      rlang::abort(paste0("Could not read a year from \"", x,
                          "\". Use \"2024\", \"2023,2024\", or \"2020-2024\"."))
    }
    return(list(years = y))
  }

  # --- window grain: day, month, season, or full year(s) -------------------
  hr <- function(d, h) sprintf("%sT%02d", d, h)
  month_end <- function(yr, mo) {
    format(seq(as.Date(sprintf("%04d-%02d-01", yr, mo)),
               by = "month", length.out = 2)[2] - 1, "%Y-%m-%d")
  }

  # single day  YYYY-MM-DD
  if (grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", x)) {
    return(list(start = hr(x, 0), end = hr(x, 23), years = as.integer(substr(x, 1, 4))))
  }
  # month  YYYY-MM
  if (grepl("^[0-9]{4}-[0-9]{2}$", x)) {
    yr <- as.integer(substr(x, 1, 4)); mo <- as.integer(substr(x, 6, 7))
    return(list(start = hr(sprintf("%04d-%02d-01", yr, mo), 0),
                end = hr(month_end(yr, mo), 23), years = yr))
  }
  # month name + year  "July 2024"  /  season + year  "summer 2024"
  toks <- strsplit(x, "[[:space:]]+")[[1]]
  if (length(toks) == 2L) {
    mo <- essp_month_num(toks[1]); yr <- suppressWarnings(as.integer(toks[2]))
    if (!is.na(mo) && !is.na(yr)) {
      return(list(start = hr(sprintf("%04d-%02d-01", yr, mo), 0),
                  end = hr(month_end(yr, mo), 23), years = yr))
    }
    mos <- tryCatch(essp_season_months(toks[1]), error = function(e) NULL)
    if (!is.null(mos) && !is.na(yr)) {
      # A continuous window from the first month's start to the last month's
      # end. Winter (Dec,Jan,Feb) wraps, so it runs Dec 1 (yr) -> end Feb (yr+1).
      if (identical(sort(as.integer(mos)), c(1L, 2L, 12L))) {
        start <- sprintf("%04d-12-01", yr)
        end_day <- format(as.Date(sprintf("%04d-03-01", yr + 1)) - 1, "%Y-%m-%d")
      } else {
        start <- sprintf("%04d-%02d-01", yr, min(mos))
        end_day <- month_end(yr, max(mos))
      }
      return(list(start = hr(start, 0), end = hr(end_day, 23), years = yr))
    }
  }
  # bare year(s) -> full-year hourly window
  y <- parse_years(x)
  if (!is.null(y)) {
    return(list(start = hr(sprintf("%04d-01-01", min(y)), 0),
                end   = hr(sprintf("%04d-12-31", max(y)), 23), years = y))
  }
  rlang::abort(paste0("Could not read a time window from \"", x,
                      "\". Use \"2024\", \"2024-07\", \"July 2024\", ",
                      "\"summer 2024\", or \"2024-07-31\"."))
}
