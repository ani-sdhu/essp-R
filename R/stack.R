# Multi-value stacking shared by every simplified metric command.
#
# A metric's body only ever handles a single place and a single year. These
# helpers sit at the top of each public function: they parse the plain-string
# `where`/`when` arguments, and when the request expands to more than one
# combination they re-invoke the same function once per combination and stack
# the pieces with `state`/`year` columns.

#' Resolve a start/end window, allowing a single normative `when` string
#'
#' The hourly commands historically took `start` and `end` ISO timestamps. They
#' now also accept a single plain string in `start` (with `end` left `NULL`) --
#' `"July 2024"`, `"summer 2024"`, `"2024-07-31"`, `"2024"` -- which is expanded
#' via [essp_parse_when()]. Two explicit bounds are passed through unchanged.
#'
#' @param start A timestamp, or a normative `when` string when `end` is `NULL`.
#' @param end The upper bound, or `NULL` to expand `start` as a window.
#' @return `list(start=, end=)`.
#' @keywords internal
essp_when_window <- function(start, end = NULL) {
  if (is.null(end)) {
    w <- essp_parse_when(start, "window")
    return(list(start = w$start, end = w$end))
  }
  list(start = start, end = end)
}

#' Decide whether a metric call needs stacking
#'
#' @param where A place string (`"CA"`, `"CA,MD,DE"`, `"ALL"`, `"US"`) or a
#'   fuel string when `need = "fuel"`.
#' @param when A year string/number (`"2024"`, `"2023,2024"`, `"2020-2024"`).
#' @param need Passed through to [essp_parse_where()].
#'
#' @return `list(where=, years=, multi=)` -- `multi` is `TRUE` when more than
#'   one `(where, year)` combination was requested.
#' @keywords internal
essp_expand <- function(where, when, need = "state") {
  wv <- essp_parse_where(where, need = need)
  yv <- essp_parse_when(when, "year")$years
  list(where = wv, years = as.integer(yv),
       multi = length(wv) > 1L || length(yv) > 1L)
}

#' Re-run a metric once per (where, year) and row-bind the results
#'
#' Rewrites the captured call with each scalar `where`/`year` in turn, evaluates
#' it in the caller's environment (so the metric's body runs on a single
#' combination), tags the result with `where`/`year` columns, and stacks.
#'
#' @param mc The metric's own `match.call()`.
#' @param expand The list from [essp_expand()].
#' @param where_arg The metric's place argument name (`"state"` or `"fuel"`).
#' @param env The caller environment the other arguments must evaluate in.
#'
#' @return One stacked tibble.
#' @keywords internal
essp_stack <- function(mc, expand, where_arg = "state", env = parent.frame()) {
  pieces <- vector("list", length(expand$where) * length(expand$years))
  i <- 0L
  for (w in expand$where) {
    for (y in expand$years) {
      cl <- mc
      cl[[where_arg]] <- w
      cl[["year"]]    <- y
      one <- eval(cl, env)
      # Tag provenance so a stacked frame stays self-describing. Put the keys
      # first for readability.
      one[[where_arg]] <- w
      one[["year"]]    <- as.integer(y)
      front <- c(where_arg, "year")
      one <- one[, c(front, setdiff(names(one), front)), drop = FALSE]
      pieces[[i <- i + 1L]] <- one
    }
  }
  out <- dplyr::bind_rows(pieces)
  tibble::as_tibble(out)
}
