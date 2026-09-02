# Layer A parsing, tables, export, emissions, and comparison --------------------

# EIA returns every measurement as a character column paired with a
# "<column>-units" companion. Those companions are how measurement columns can
# be told apart from facet labels without hardcoding column names per route.
measurement_columns <- function(data) {
  u <- grep("-units$", names(data), value = TRUE)
  intersect(sub("-units$", "", u), names(data))
}

# Turn a period stamp into a year, whatever its granularity.
period_year <- function(x) suppressWarnings(as.integer(substr(as.character(x), 1, 4)))

#' Make gathered data analysis-ready
#'
#' Takes raw [essp.gather()] output and does the cleanup every analysis
#' otherwise repeats: coerces measurement columns from character to numeric,
#' derives a `year` column from the period stamp, and optionally adds shares
#' and growth rates.
#'
#' Measurement columns are found via their `-units` companions rather than by
#' name, so this works across routes whose columns differ.
#'
#' @param data A tibble from [essp.gather()].
#' @param value Measurement column to derive from. Defaults to the first one
#'   found.
#' @param by Optional grouping column(s) for shares, e.g. `"fueltypeid"`.
#'   Shares are computed within each period.
#' @param derive Any of `"share"` and `"growth"`. Defaults to neither.
#'
#'   `"share"` assumes the rows in each period are mutually exclusive. EIA fuel
#'   data is not: a single response carries `ALL`, `FOS`, and `REN` alongside
#'   the very codes they aggregate, so sharing it raw is meaningless. Filter to
#'   a non-overlapping set first, or use [analyze.generation()], which selects
#'   one code per resource and derives the residual.
#' @param drop_units Remove the `-units` companion columns, keeping the units
#'   on the result as an attribute. Defaults to `TRUE`.
#'
#' @return A tibble with numeric measurement columns and a `year` column.
#'
#' @examples
#' \dontrun{
#' essp.gather("retail", state = "GA", years = 2020:2024, freq = "annual") |>
#'   essp.analyze(value = "price", derive = "growth")
#' }
#'
#' @export
essp.analyze <- function(data,
                         value      = NULL,
                         by         = NULL,
                         derive     = character(0),
                         drop_units = TRUE) {
  if (!is.data.frame(data)) rlang::abort("`data` must be a data frame.")
  # match.arg() errors on a zero-length argument, which is the default here.
  derive <- if (length(derive)) {
    match.arg(derive, c("share", "growth"), several.ok = TRUE)
  } else {
    character(0)
  }

  cols <- measurement_columns(data)
  if (!length(cols)) {
    rlang::abort(c(
      "No measurement columns found.",
      i = "Expected columns paired with a '<column>-units' companion.",
      i = "Did this come from essp.gather()?"
    ))
  }

  units <- stats::setNames(
    lapply(cols, function(cl) unique(stats::na.omit(data[[paste0(cl, "-units")]]))),
    cols
  )
  for (cl in cols) data[[cl]] <- suppressWarnings(as.numeric(data[[cl]]))

  if ("period" %in% names(data)) data$year <- period_year(data$period)

  if (is.null(value)) value <- cols[[1]]
  if (!value %in% cols) {
    rlang::abort(paste0("`value` must be one of: ", paste(cols, collapse = ", ")))
  }

  if ("share" %in% derive) {
    key <- if (is.null(by)) rep("all", nrow(data)) else
      interaction(data[, by, drop = FALSE], drop = TRUE)
    per <- if ("period" %in% names(data)) data$period else rep("all", nrow(data))
    denom <- stats::ave(data[[value]], per, FUN = function(x) sum(x, na.rm = TRUE))
    data$share <- data[[value]] / denom * 100
    # `key` participates only in ordering, not the denominator; shares are
    # within a period so they sum to 100 per period.
    invisible(key)
  }

  if ("growth" %in% derive) {
    if (!"year" %in% names(data)) {
      rlang::abort("`derive = \"growth\"` needs a period column to order by.")
    }
    grp <- if (is.null(by)) rep("all", nrow(data)) else
      as.character(interaction(data[, by, drop = FALSE], drop = TRUE))
    data <- data[order(grp, data$year), , drop = FALSE]
    grp <- grp[order(grp, data$year)]
    data$yoy <- stats::ave(data[[value]], grp, FUN = function(x) {
      prev <- c(NA_real_, utils::head(x, -1L))
      ifelse(is.na(prev) | prev == 0, NA_real_, (x - prev) / prev * 100)
    })
  }

  if (isTRUE(drop_units)) {
    data <- data[, !grepl("-units$", names(data)), drop = FALSE]
  }

  attr(data, "essp_units") <- units
  tibble::as_tibble(data)
}

#' Summary table from any tibble
#'
#' Grouped aggregation with optional share and totals row -- the plain research
#' table, as opposed to a publication-styled one.
#'
#' @param data A data frame.
#' @param rows Column(s) to group by.
#' @param value Numeric column to aggregate.
#' @param stat Aggregation function name: `"sum"` (default), `"mean"`,
#'   `"median"`, `"min"`, `"max"`, or `"n"`.
#' @param share Add each row's percentage of the total. Defaults to `TRUE`.
#' @param total Append a totals row. Defaults to `TRUE`.
#' @param sort Sort descending by the aggregated value. Defaults to `TRUE`.
#'
#' @return A tibble.
#'
#' @examples
#' df <- data.frame(fuel = c("Gas","Coal","Gas","Wind"), mw = c(10, 5, 15, 8))
#' essp.table(df, "fuel", "mw")
#'
#' @export
essp.table <- function(data, rows, value,
                       stat  = c("sum", "mean", "median", "min", "max", "n"),
                       share = TRUE, total = TRUE, sort = TRUE) {
  stat <- match.arg(stat)
  if (!is.data.frame(data)) rlang::abort("`data` must be a data frame.")
  for (cl in c(rows, value)) {
    if (!cl %in% names(data)) rlang::abort(paste0("Column \"", cl, "\" not found."))
  }

  f <- switch(stat,
              sum = sum, mean = mean, median = stats::median,
              min = min, max = max, n = length)

  key <- interaction(data[, rows, drop = FALSE], drop = TRUE, sep = " | ")
  parts <- lapply(split(seq_len(nrow(data)), key), function(i) {
    d <- data[i, , drop = FALSE]
    out <- d[1, rows, drop = FALSE]
    out[[value]] <- f(stats::na.omit(d[[value]]))
    out
  })
  out <- do.call(rbind, parts)
  rownames(out) <- NULL

  if (isTRUE(sort)) out <- out[order(-out[[value]]), , drop = FALSE]
  if (isTRUE(share)) out$share <- out[[value]] / sum(out[[value]], na.rm = TRUE) * 100

  if (isTRUE(total)) {
    tot <- out[1, , drop = FALSE]
    for (cl in rows) tot[[cl]] <- "Total"
    tot[[value]] <- sum(out[[value]], na.rm = TRUE)
    if (isTRUE(share)) tot$share <- 100
    out <- rbind(out, tot)
  }

  rownames(out) <- NULL
  tibble::as_tibble(out)
}

#' Write a table to CSV, Excel, or LaTeX
#'
#' @param data A data frame.
#' @param path Output path. The format is inferred from the extension unless
#'   `format` is given.
#' @param format `"csv"`, `"xlsx"`, or `"tex"`.
#' @param digits Rounding applied to numeric columns. `NULL` leaves them alone.
#'
#' @return `path`, invisibly.
#'
#' @examples
#' \dontrun{
#' essp.export(analyze.mix("US", 2024), "mix.csv")
#' }
#'
#' @export
essp.export <- function(data, path, format = NULL, digits = NULL) {
  if (!is.data.frame(data)) rlang::abort("`data` must be a data frame.")
  if (is.null(format)) format <- tolower(tools::file_ext(path))
  if (!nzchar(format)) rlang::abort("Cannot infer format; pass `format`.")

  if (!is.null(digits)) {
    num <- vapply(data, is.numeric, logical(1))
    data[num] <- lapply(data[num], round, digits = digits)
  }

  switch(format,
    csv = utils::write.csv(data, path, row.names = FALSE),
    xlsx = {
      if (!requireNamespace("writexl", quietly = TRUE)) {
        rlang::abort(c("Writing .xlsx needs the writexl package.",
                       i = "install.packages(\"writexl\"), or export to .csv."))
      }
      writexl::write_xlsx(data, path)
    },
    tex = writeLines(latex_table(data), path),
    rlang::abort(paste0("Unsupported format \"", format, "\"; use csv, xlsx, or tex."))
  )

  invisible(path)
}

# Minimal booktabs table. Publication styling (alternating row shading, the
# brief's rule weights) belongs in the visualization set, not here.
latex_table <- function(data) {
  esc <- function(x) gsub("([&%$#_{}])", "\\\\\\1", as.character(x))
  align <- paste(vapply(data, function(cl) if (is.numeric(cl)) "r" else "l",
                        character(1)), collapse = "")
  body <- apply(data, 1, function(r) paste(esc(r), collapse = " & "))

  c(paste0("\\begin{tabular}{", align, "}"),
    "\\toprule",
    paste(paste0("\\textbf{", esc(names(data)), "}", collapse = " & "), "\\\\"),
    "\\midrule",
    paste(body, "\\\\"),
    "\\bottomrule",
    "\\end{tabular}")
}

#' Emissions intensity by state
#'
#' CO2, SO2, and NOx from power generation, both as tonnage and as rates per
#' megawatthour. Rates are what make states comparable regardless of size.
#'
#' @param state State code(s), or `"US"`.
#' @param year Calendar year.
#' @param fuel Optional fuel id filter; defaults to all fuels combined.
#'
#' @return A tibble of emissions columns by state and fuel.
#'
#' @examples
#' \dontrun{
#' analyze.emissions("GA", 2023)
#' }
#'
#' @export
analyze.emissions <- function(state = "US", year = NULL, fuel = NULL) {
  if (is.null(year)) year <- essp_latest_year()
  .exp <- essp_expand(state, year)
  if (.exp$multi) return(essp_stack(match.call(), .exp, "state", parent.frame()))
  state <- .exp$where; year <- .exp$years

  raw <- essp.gather("emissions", state = state, years = year, freq = "annual",
                     fuel = fuel, verbose = FALSE)
  if (!nrow(raw)) rlang::abort("No emissions data returned.")

  out <- essp.analyze(raw)
  keep <- c("year", "stateid", "fuelid",
            grep("^(co2|so2|nox)", names(out), value = TRUE))
  out[, intersect(keep, names(out)), drop = FALSE]
}

# analyze.compare() was removed in the usability layer: every metric now
# accepts multiple states directly (e.g. analyze.diversity("GA,OH,WV", 2024))
# and stacks them itself via essp_stack(), so a separate comparison verb is
# redundant.
