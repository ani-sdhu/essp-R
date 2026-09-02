# Frozen data snapshots ---------------------------------------------------------
#
# EIA revises published data: monthly figures are preliminary and get restated
# later. A brief that re-fetches at render time can therefore produce different
# numbers than the PDF already in circulation, with nothing to show why.
#
# A snapshot freezes the data next to the brief that uses it. The CSV is
# committed, human-readable, and editable -- a number can be corrected by hand
# without touching R. Provenance travels in a sidecar written in DCF, the same
# plain key-value format DESCRIPTION uses, so it stays readable and needs no
# extra dependency.

snapshot_sidecar <- function(path) {
  paste0(tools::file_path_sans_ext(path), ".provenance")
}

write_snapshot <- function(data, path) {
  dir <- dirname(path)
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)
  utils::write.csv(data, path, row.names = FALSE)

  p <- essp.provenance(data)
  fields <- list(
    Route     = p$route     %||% NA_character_,
    Topic     = p$topic     %||% NA_character_,
    Forms     = p$forms     %||% NA_character_,
    Frequency = p$frequency %||% NA_character_,
    Start     = p$start     %||% NA_character_,
    End       = p$end       %||% NA_character_,
    Facets    = if (length(p$facets)) {
      paste(names(p$facets),
            vapply(p$facets, function(v) paste(v, collapse = "/"), character(1)),
            sep = "=", collapse = "; ")
    } else NA_character_,
    Columns   = if (length(p$columns)) paste(p$columns, collapse = ", ") else NA_character_,
    Rows      = as.character(nrow(data)),
    Retrieved = format(p$fetched %||% Sys.time(), "%Y-%m-%d %H:%M:%S"),
    Written   = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  )
  fields <- fields[!vapply(fields, function(v) is.na(v[1]), logical(1))]
  write.dcf(as.data.frame(fields, stringsAsFactors = FALSE),
            snapshot_sidecar(path))
  invisible(path)
}

read_snapshot <- function(path) {
  data <- tibble::as_tibble(utils::read.csv(path, stringsAsFactors = FALSE,
                                            check.names = FALSE))
  side <- snapshot_sidecar(path)
  if (file.exists(side)) {
    d <- as.list(as.data.frame(read.dcf(side), stringsAsFactors = FALSE))
    attr(data, "essp_provenance") <- list(
      route = d$Route, topic = d$Topic, forms = d$Forms,
      frequency = d$Frequency, start = d$Start, end = d$End,
      columns = if (!is.null(d$Columns)) trimws(strsplit(d$Columns, ",")[[1]]),
      rows = nrow(data),
      fetched = as.POSIXct(d$Retrieved %||% NA, tz = ""),
      from_snapshot = path
    )
  }
  data
}

#' Read a frozen snapshot, or create it on first use
#'
#' Returns the CSV at `path` if it exists. If it does not, `expr` is evaluated,
#' its result written to `path` with a provenance sidecar, and returned.
#'
#' This is the form a brief should use. On the first render it fetches and
#' freezes; on every render after that it reads the committed CSV and makes no
#' network request at all. The figure in a published PDF then cannot drift when
#' EIA restates its data, and the numbers behind it stay open to inspection and
#' correction.
#'
#' `expr` is only evaluated when the snapshot is missing, so a cached brief
#' needs no API key to render.
#'
#' @param path Path to the CSV, typically inside the brief's own folder.
#' @param expr Expression producing the data, normally an [essp.gather()] call.
#'   Not evaluated if the snapshot already exists.
#' @param refresh Force re-evaluation and overwrite the snapshot.
#'
#' @return A tibble. Provenance is restored from the sidecar when read back.
#'
#' @examples
#' \dontrun{
#' gen <- essp.snapshot(
#'   "data/us_generation_2024.csv",
#'   essp.gather("generation", state = "US", years = 2024, freq = "annual")
#' )
#' }
#'
#' @export
essp.snapshot <- function(path, expr, refresh = FALSE) {
  if (!isTRUE(refresh) && file.exists(path)) return(read_snapshot(path))

  data <- expr   # the promise is forced here, and only here
  if (!is.data.frame(data)) {
    rlang::abort("`expr` must produce a data frame.")
  }
  write_snapshot(data, path)
  data
}

#' Describe a snapshot without reading its data
#'
#' Reports where a snapshot came from and when, so a committed CSV can be
#' traced back to the request that produced it.
#'
#' @param path Path to the snapshot CSV.
#'
#' @return A named list of provenance fields, or `NULL` if no sidecar exists.
#'
#' @examples
#' \dontrun{
#' essp.snapshotinfo("data/us_generation_2024.csv")
#' }
#'
#' @export
essp.snapshotinfo <- function(path) {
  side <- snapshot_sidecar(path)
  if (!file.exists(side)) {
    warning("No provenance sidecar beside ", path, call. = FALSE)
    return(NULL)
  }
  as.list(as.data.frame(read.dcf(side), stringsAsFactors = FALSE))
}
