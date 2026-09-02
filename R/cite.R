# Citation generation ----------------------------------------------------------
#
# Every EIA reference in the briefs' .bib files is currently a hand-typed
# webpage URL, and at least one figure's numbers are uncited entirely
# (RT_NaturalGas/figures/figure1_plot.R:18, flagged as an open problem in the
# program's own planning notes). Deriving the citation from the request that
# produced the data means a chart and its source cannot drift apart.

#' Build a bibliography entry for gathered data
#'
#' Turns the provenance recorded by [essp.gather()] into a biblatex entry ready
#' to paste into a brief's `references.bib`, capturing the route, the EIA
#' form(s) behind it, the filters applied, and the date retrieved.
#'
#' @param data A tibble returned by [essp.gather()].
#' @param key Citation key. Defaults to one derived from the topic and the year
#'   the data was retrieved, e.g. `eia_retail_2026`.
#' @param type Entry type. `"online"` (default) is what the existing briefs use
#'   for EIA web sources; `"dataset"` is more precise but needs a biblatex
#'   version that supports it.
#' @param file Optional path to append the entry to, typically a
#'   `references.bib`. The entry is not appended if its key is already present.
#'
#' @return The entry as a single string, invisibly when `file` is given.
#'
#' @examples
#' \dontrun{
#' x <- essp.gather("retail", state = "GA", years = 2023, freq = "annual")
#' cat(essp.cite(x))
#' essp.cite(x, file = "references.bib")
#' }
#'
#' @export
essp.cite <- function(data,
                      key  = NULL,
                      type = c("online", "dataset"),
                      file = NULL) {
  type <- match.arg(type)

  p <- essp.provenance(data)
  if (is.null(p)) {
    rlang::abort(c(
      "`data` carries no provenance.",
      i = "Only tibbles returned by essp.gather() can be cited automatically."
    ))
  }

  year <- format(p$fetched, "%Y")
  slug <- p$topic %||% gsub("[^a-z0-9]+", "_", tolower(p$route))
  if (is.null(key)) key <- paste0("eia_", slug, "_", year)

  # Prefer the route's own published name; fall back to the map description so
  # this still works offline.
  title <- tryCatch({
    nm <- unwrap(eia::eia_metadata(p$route)[["Name"]])
    nm <- paste(as.character(nm), collapse = " ")
    if (nzchar(nm)) nm else NA_character_
  }, error = function(e) NA_character_)

  if (is.na(title)) {
    row <- tryCatch(essp.resolve(p$route), error = function(e) NULL)
    title <- if (!is.null(row) && nrow(row)) row$description[[1]] else p$route
  }

  # Record what actually shaped the request, so the citation describes this
  # extract rather than the dataset in general.
  bits <- character(0)
  if (!is.null(p$forms) && !is.na(p$forms)) {
    bits <- c(bits, paste0("Source: Form(s) EIA-", gsub(", ", ", EIA-", p$forms)))
  }
  bits <- c(bits, paste0("API route: ", p$route))
  if (!is.null(p$frequency)) bits <- c(bits, paste0("frequency: ", p$frequency))
  if (!is.null(p$start) || !is.null(p$end)) {
    bits <- c(bits, paste0("period: ", p$start %||% "", "--", p$end %||% ""))
  }
  if (length(p$facets)) {
    bits <- c(bits, paste0(
      "filters: ",
      paste(names(p$facets), vapply(p$facets, function(v) paste(v, collapse = "/"),
                                    character(1)),
            sep = "=", collapse = ", ")
    ))
  }

  fields <- c(
    author  = "{U.S. Energy Information Administration}",
    title   = paste0("{", title, "}"),
    year    = paste0("{", year, "}"),
    url     = paste0("{https://www.eia.gov/opendata/browser/", p$route, "}"),
    urldate = paste0("{", format(p$fetched, "%Y-%m-%d"), "}"),
    note    = paste0("{", paste(bits, collapse = ". "), "}")
  )

  entry <- paste0(
    "@", type, "{", key, ",\n",
    paste0("  ", format(names(fields), width = 7), " = ", fields, collapse = ",\n"),
    "\n}\n"
  )

  if (is.null(file)) return(entry)

  existing <- if (file.exists(file)) readLines(file, warn = FALSE) else character(0)
  if (any(grepl(paste0("\\{", key, ","), existing, fixed = FALSE))) {
    message("Key '", key, "' already present in ", file, "; not appended.")
    return(invisible(entry))
  }

  # Keep a blank line between entries without accumulating them.
  prefix <- if (length(existing) && nzchar(utils::tail(existing, 1))) "\n" else ""
  cat(prefix, entry, sep = "", file = file, append = TRUE)
  message("Appended '", key, "' to ", file)
  invisible(entry)
}
