# General-purpose charts ---------------------------------------------------------

#' Multi-series line chart
#'
#' One line per group, with the series under discussion in the accent color and
#' the rest greyed. Series are labelled at their right-hand end rather than in a
#' legend, so the eye never has to travel between a key and the lines.
#'
#' @param data A data frame.
#' @param x,y Column names, as strings.
#' @param group Optional grouping column for multiple series.
#' @param highlight Value of `group` to emphasise.
#' @param direct_label Label each line at its end instead of using a legend.
#' @param accent,semester,year As in [chart.fleetmakeup()].
#'
#' @return A ggplot object.
#'
#' @examples
#' \dontrun{
#' analyze.prices(c("GA","OH"), 2010:2024) |>
#'   chart.timeseries("year", "price_real", group = "state", highlight = "GA")
#' }
#'
#' @export
chart.timeseries <- function(data, x, y, group = NULL, highlight = NULL,
                             direct_label = TRUE, accent = NULL,
                             semester = "Fall",
                             year = as.integer(format(Sys.Date(), "%Y"))) {
  for (cl in c(x, y, group)) {
    if (!is.null(cl) && !cl %in% names(data)) {
      rlang::abort(paste0("Column \"", cl, "\" not found."))
    }
  }

  th <- essp.theme(semester = semester, year = year)
  if (is.null(accent)) accent <- attr(th, "essp_accent")

  if (is.null(group)) {
    p <- ggplot2::ggplot(data, ggplot2::aes(.data[[x]], .data[[y]])) +
      ggplot2::geom_line(colour = accent, linewidth = 0.9)
    return(p + ggplot2::labs(x = NULL, y = NULL) + th)
  }

  levs <- unique(as.character(data[[group]]))
  cols <- essp.highlight(levs, highlight = highlight, accent = accent)
  data[[group]] <- factor(data[[group]], levels = levs)

  # Draw the highlighted series last so it sits above the greyed ones.
  ord <- if (is.null(highlight)) levs else c(setdiff(levs, highlight), highlight)
  data <- data[order(match(as.character(data[[group]]), ord)), , drop = FALSE]

  p <- ggplot2::ggplot(data, ggplot2::aes(.data[[x]], .data[[y]],
                                          colour = .data[[group]],
                                          group = .data[[group]])) +
    ggplot2::geom_line(linewidth = 0.9, lineend = "round") +
    ggplot2::scale_colour_manual(values = cols,
                                 guide = if (direct_label) "none" else ggplot2::guide_legend())

  if (isTRUE(direct_label)) {
    ends <- do.call(rbind, lapply(split(data, data[[group]]), function(d) {
      d[which.max(d[[x]]), , drop = FALSE]
    }))
    p <- p +
      ggplot2::geom_text(
        data = ends,
        ggplot2::aes(label = .data[[group]]),
        hjust = -0.15, size = 3, show.legend = FALSE
      ) +
      ggplot2::scale_x_continuous(
        expand = ggplot2::expansion(mult = c(0.02, 0.14))
      )
  }

  p + ggplot2::labs(x = NULL, y = NULL) + th
}

#' Small multiples
#'
#' One panel per group on a shared scale, for when several series overlap too
#' heavily to read as lines on one plot.
#'
#' @inheritParams chart.timeseries
#' @param facet Column to facet by.
#' @param ncol Number of columns.
#' @param scales Passed to [ggplot2::facet_wrap()]. `"fixed"` (the default)
#'   keeps panels comparable; `"free_y"` makes them individually legible at the
#'   cost of comparability.
#'
#' @return A ggplot object.
#' @export
chart.multiples <- function(data, x, y, facet, ncol = 1, scales = "fixed",
                            accent = NULL, semester = "Fall",
                            year = as.integer(format(Sys.Date(), "%Y"))) {
  th <- essp.theme(semester = semester, year = year)
  if (is.null(accent)) accent <- attr(th, "essp_accent")

  ggplot2::ggplot(data, ggplot2::aes(.data[[x]], .data[[y]])) +
    ggplot2::geom_line(colour = accent, linewidth = 0.7, lineend = "round") +
    ggplot2::facet_wrap(stats::as.formula(paste("~", facet)),
                        ncol = ncol, scales = scales) +
    ggplot2::labs(x = NULL, y = NULL) +
    th +
    ggplot2::theme(panel.spacing.y = ggplot2::unit(8, "pt"))
}

#' Composition over time
#'
#' A 100% stacked area showing how a mix shifts, with one component
#' highlighted. Because every period sums to 100, this shows relative change
#' only -- a component can rise here while falling in absolute terms.
#'
#' @param data A data frame.
#' @param x,fill,value Column names, as strings.
#' @param highlight Value of `fill` to emphasise.
#' @param accent,semester,year As in [chart.fleetmakeup()].
#'
#' @return A ggplot object.
#' @export
chart.share <- function(data, x, fill, value, highlight = NULL, accent = NULL,
                        semester = "Fall",
                        year = as.integer(format(Sys.Date(), "%Y"))) {
  th <- essp.theme(semester = semester, year = year)
  if (is.null(accent)) accent <- attr(th, "essp_accent")

  levs <- unique(as.character(data[[fill]]))
  cols <- essp.highlight(levs, highlight = highlight, accent = accent)
  data[[fill]] <- factor(data[[fill]], levels = rev(levs))

  ggplot2::ggplot(data, ggplot2::aes(.data[[x]], .data[[value]],
                                     fill = .data[[fill]])) +
    ggplot2::geom_area(position = "fill", colour = "#FFFFFF", linewidth = 0.2) +
    ggplot2::scale_fill_manual(values = cols) +
    ggplot2::scale_y_continuous(labels = scales::percent, expand = c(0, 0)) +
    ggplot2::scale_x_continuous(expand = c(0, 0)) +
    ggplot2::labs(x = NULL, y = NULL) +
    th
}

#' Ranked bar chart
#'
#' Top-N categories as horizontal bars, ordered by value, with an optional
#' cumulative-share line. Horizontal bars let category names sit as readable
#' text rather than rotated axis labels.
#'
#' @param data Output of [analyze.fuelleaders()], or any data frame.
#' @param label,value Column names, as strings.
#' @param n Number of rows to show.
#' @param cumulative Annotate the cumulative share of the bars shown.
#' @param highlight Value of `label` to emphasise.
#' @param accent,semester,year As in [chart.fleetmakeup()].
#'
#' @return A ggplot object.
#'
#' @examples
#' \dontrun{
#' analyze.fuelleaders("Solar", 2024, n = 8) |>
#'   chart.ranked("state_name", "share", highlight = "California")
#' }
#'
#' @export
chart.ranked <- function(data, label, value, n = 10, cumulative = TRUE,
                         highlight = NULL, accent = NULL, semester = "Fall",
                         year = as.integer(format(Sys.Date(), "%Y"))) {
  for (cl in c(label, value)) {
    if (!cl %in% names(data)) rlang::abort(paste0("Column \"", cl, "\" not found."))
  }

  th <- essp.theme(semester = semester, year = year)
  if (is.null(accent)) accent <- attr(th, "essp_accent")

  d <- data[order(-data[[value]]), , drop = FALSE]
  d <- utils::head(d, n)
  d[[label]] <- factor(d[[label]], levels = rev(d[[label]]))

  cols <- essp.highlight(as.character(rev(levels(d[[label]]))),
                         highlight = highlight, accent = accent)

  p <- ggplot2::ggplot(d, ggplot2::aes(x = .data[[value]], y = .data[[label]],
                                       fill = .data[[label]])) +
    ggplot2::geom_col(width = 0.75) +
    ggplot2::geom_text(ggplot2::aes(label = sprintf("%.1f", .data[[value]])),
                       hjust = -0.2, size = 2.9, colour = essp.colors("ink")) +
    ggplot2::scale_fill_manual(values = cols, guide = "none") +
    ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = c(0, 0.12))) +
    ggplot2::labs(x = NULL, y = NULL) +
    th +
    ggplot2::theme(panel.grid.major.y = ggplot2::element_blank(),
                   panel.grid.major.x = ggplot2::element_line(
                     colour = essp.colors("rule"), linewidth = 0.3))

  if (isTRUE(cumulative)) {
    p <- p + ggplot2::labs(
      caption = sprintf("Top %d account for %.1f%% of the national total.",
                        nrow(d), sum(d[[value]]))
    )
  }
  p
}

#' Distribution by group
#'
#' Boxplots with the underlying points overlaid, so the reader sees the spread
#' and the sample size rather than five summary statistics alone.
#'
#' @param data A data frame.
#' @param group,value Column names, as strings.
#' @param points Overlay individual observations.
#' @param accent,semester,year As in [chart.fleetmakeup()].
#'
#' @return A ggplot object.
#' @export
chart.box <- function(data, group, value, points = TRUE, accent = NULL,
                      semester = "Fall",
                      year = as.integer(format(Sys.Date(), "%Y"))) {
  th <- essp.theme(semester = semester, year = year)
  if (is.null(accent)) accent <- attr(th, "essp_accent")

  p <- ggplot2::ggplot(data, ggplot2::aes(x = .data[[group]], y = .data[[value]])) +
    ggplot2::geom_boxplot(fill = essp.colors("ugacreamery"),
                          colour = essp.colors("termgray"),
                          outlier.shape = if (points) NA else 19,
                          linewidth = 0.4, width = 0.6)
  if (isTRUE(points)) {
    p <- p + ggplot2::geom_jitter(width = 0.15, height = 0, size = 1.1,
                                  colour = accent, alpha = 0.6)
  }
  p + ggplot2::labs(x = NULL, y = NULL) + th
}

#' Two-metric scatter
#'
#' @param data A data frame.
#' @param x,y Column names, as strings.
#' @param label Optional column to label points with.
#' @param size Optional column to scale point area by.
#' @param accent,semester,year As in [chart.fleetmakeup()].
#'
#' @return A ggplot object.
#' @export
chart.scatter <- function(data, x, y, label = NULL, size = NULL, accent = NULL,
                          semester = "Fall",
                          year = as.integer(format(Sys.Date(), "%Y"))) {
  th <- essp.theme(semester = semester, year = year)
  if (is.null(accent)) accent <- attr(th, "essp_accent")

  aes_args <- list(x = rlang::sym(x), y = rlang::sym(y))
  if (!is.null(size)) aes_args$size <- rlang::sym(size)

  p <- ggplot2::ggplot(data, do.call(ggplot2::aes, aes_args)) +
    ggplot2::geom_point(colour = accent, alpha = 0.75)

  if (!is.null(label)) {
    p <- p + ggplot2::geom_text(ggplot2::aes(label = .data[[label]]),
                                vjust = -0.8, size = 2.7,
                                colour = essp.colors("muted"))
  }
  p + ggplot2::labs(x = x, y = y) + th
}

#' Change between two periods
#'
#' A dumbbell chart: one row per category, a line joining its start and end
#' values. Reads change directly, which a pair of grouped bars does not.
#'
#' @param data A data frame.
#' @param group,start,end Column names, as strings.
#' @param accent,semester,year As in [chart.fleetmakeup()].
#'
#' @return A ggplot object.
#' @export
chart.dumbbell <- function(data, group, start, end, accent = NULL,
                           semester = "Fall",
                           year = as.integer(format(Sys.Date(), "%Y"))) {
  th <- essp.theme(semester = semester, year = year)
  if (is.null(accent)) accent <- attr(th, "essp_accent")

  d <- data[order(data[[end]] - data[[start]]), , drop = FALSE]
  d[[group]] <- factor(d[[group]], levels = d[[group]])

  ggplot2::ggplot(d) +
    ggplot2::geom_segment(
      ggplot2::aes(x = .data[[start]], xend = .data[[end]],
                   y = .data[[group]], yend = .data[[group]]),
      colour = essp.colors("rule"), linewidth = 1.4
    ) +
    ggplot2::geom_point(ggplot2::aes(x = .data[[start]], y = .data[[group]]),
                        colour = essp.colors("ugagray"), size = 2.4) +
    ggplot2::geom_point(ggplot2::aes(x = .data[[end]], y = .data[[group]]),
                        colour = accent, size = 2.4) +
    ggplot2::labs(x = NULL, y = NULL) +
    th +
    ggplot2::theme(panel.grid.major.y = ggplot2::element_blank())
}

#' Additions and retirements bridge
#'
#' Capacity added and retired per year as opposing bars around zero, so net
#' change is the visible balance rather than something the reader computes.
#'
#' @param additions Output of [analyze.additions()].
#' @param retirements Output of [analyze.retirements()]. Optional.
#' @param highlight Resource to emphasise.
#' @param accent,semester,year As in [chart.fleetmakeup()].
#'
#' @return A ggplot object.
#'
#' @examples
#' \dontrun{
#' chart.waterfall(analyze.additions("US", 2024, years = 2015:2024))
#' }
#'
#' @export
chart.waterfall <- function(additions, retirements = NULL, highlight = NULL,
                            accent = NULL, semester = "Fall",
                            year = as.integer(format(Sys.Date(), "%Y"))) {
  for (cl in c("year", "resource", "mw")) {
    if (!cl %in% names(additions)) {
      rlang::abort(paste0("`additions` needs a \"", cl, "\" column."))
    }
  }

  th <- essp.theme(semester = semester, year = year)
  if (is.null(accent)) accent <- attr(th, "essp_accent")

  d <- additions[, c("year", "resource", "mw")]
  d$direction <- "Added"
  if (!is.null(retirements) && nrow(retirements)) {
    r <- retirements[, c("year", "resource", "mw")]
    # Retirements point downward so the net balance is read off the axis.
    r$mw <- -r$mw
    r$direction <- "Retired"
    d <- rbind(d, r)
  }

  levs <- unique(d$resource)
  cols <- essp.highlight(levs, highlight = highlight, accent = accent)
  d$resource <- factor(d$resource, levels = rev(levs))

  ggplot2::ggplot(d, ggplot2::aes(x = .data$year, y = .data$mw,
                                  fill = .data$resource)) +
    ggplot2::geom_col(width = 0.75, colour = "#FFFFFF", linewidth = 0.2) +
    ggplot2::geom_hline(yintercept = 0, colour = essp.colors("ink"), linewidth = 0.4) +
    ggplot2::scale_fill_manual(values = cols) +
    ggplot2::scale_y_continuous(labels = scales::comma) +
    ggplot2::labs(x = NULL, y = "Capacity (MW)") +
    th
}

# Tile positions for the 50 states plus DC, laid out to echo the map's rough
# geography. A tile grid rather than true boundaries: at a 3.2-inch figure
# width, Rhode Island and Delaware are a pixel or two on a real map and vanish,
# while every tile here is equally readable.
essp_state_grid <- function() {
  g <- c(
    "AK"="1,1",  "ME"="1,11",
    "VT"="2,10", "NH"="2,11",
    "WA"="3,2",  "ID"="3,3",  "MT"="3,4",  "ND"="3,5",  "MN"="3,6",
    "IL"="3,7",  "WI"="3,8",  "MI"="3,9",  "NY"="3,10", "MA"="3,11", "RI"="3,12",
    "OR"="4,2",  "NV"="4,3",  "WY"="4,4",  "SD"="4,5",  "IA"="4,6",
    "IN"="4,7",  "OH"="4,8",  "PA"="4,9",  "NJ"="4,10", "CT"="4,11",
    "CA"="5,2",  "UT"="5,3",  "CO"="5,4",  "NE"="5,5",  "MO"="5,6",
    "KY"="5,7",  "WV"="5,8",  "VA"="5,9",  "MD"="5,10", "DE"="5,11",
    "AZ"="6,3",  "NM"="6,4",  "KS"="6,5",  "AR"="6,6",  "TN"="6,7",
    "NC"="6,8",  "SC"="6,9",  "DC"="6,10",
    "OK"="7,5",  "LA"="7,6",  "MS"="7,7",  "AL"="7,8",  "GA"="7,9",
    "HI"="8,1",  "TX"="8,5",  "FL"="8,10"
  )
  parts <- do.call(rbind, strsplit(unname(g), ",", fixed = TRUE))
  tibble::tibble(state = names(g),
                 row = as.integer(parts[, 1]),
                 col = as.integer(parts[, 2]))
}

#' State tile map
#'
#' A choropleth on a tile grid rather than true state boundaries. Each state is
#' an equal square, positioned to echo the country's shape.
#'
#' At the width of a two-pager figure slot, a geographic map renders Rhode
#' Island and Delaware as a couple of pixels, so the states with the most
#' distinctive energy profiles become the hardest to see. Equal tiles trade
#' geographic fidelity for the ability to read every state.
#'
#' @param data A data frame with a state column.
#' @param state Name of the two-letter state-code column.
#' @param value Name of the numeric column to shade by.
#' @param label Print the state code in each tile.
#' @param semester,year As in [chart.fleetmakeup()].
#'
#' @return A ggplot object.
#'
#' @examples
#' \dontrun{
#' analyze.diversity("ALL", 2024) |>
#'   chart.map("state", "hhi")
#' }
#'
#' @export
chart.map <- function(data, state, value, label = TRUE, semester = "Fall",
                      year = as.integer(format(Sys.Date(), "%Y"))) {
  for (cl in c(state, value)) {
    if (!cl %in% names(data)) rlang::abort(paste0("Column \"", cl, "\" not found."))
  }

  grid <- essp_state_grid()
  d <- merge(grid, data, by.x = "state", by.y = state, all.x = TRUE)

  missing <- sum(is.na(d[[value]]))
  if (missing == nrow(d)) {
    rlang::abort("No rows matched the state grid; are these two-letter codes?")
  }

  ramp <- essp.palette("sequential", 9)
  # Row 1 at the top: the grid is written north-to-south.
  d$row <- -d$row

  p <- ggplot2::ggplot(d, ggplot2::aes(x = .data$col, y = .data$row,
                                       fill = .data[[value]])) +
    ggplot2::geom_tile(colour = "#FFFFFF", linewidth = 0.8, width = 0.92, height = 0.92)

  if (isTRUE(label)) {
    shades <- grDevices::colorRampPalette(ramp)(100)
    idx <- pmax(1, ceiling(scales::rescale(d[[value]], to = c(1, 100))))
    d$.txt <- ifelse(is.na(idx), essp.colors("muted"), essp.textcolor(shades[idx]))
    p <- p + ggplot2::geom_text(data = d,
                                ggplot2::aes(label = .data$state, colour = .data$.txt),
                                size = 2.4, fontface = "bold") +
      ggplot2::scale_colour_identity(guide = "none")
  }

  p +
    ggplot2::scale_fill_gradientn(colours = ramp, na.value = essp.colors("rule"),
                                  labels = scales::comma) +
    ggplot2::coord_equal() +
    ggplot2::labs(x = NULL, y = NULL, fill = NULL) +
    essp.theme(semester = semester, year = year) +
    ggplot2::theme(
      panel.grid   = ggplot2::element_blank(),
      axis.text    = ggplot2::element_blank(),
      legend.position  = "right",
      legend.key.width = ggplot2::unit(8, "pt")
    )
}

#' Publication LaTeX table
#'
#' Emits a booktabs table styled to match the two-pagers -- alternating row
#' shading, the same rule weights -- so a generated table is indistinguishable
#' from the hand-written ones already in the briefs.
#'
#' The plain research table is [essp.table()]; this is its publication form.
#'
#' @param data A data frame.
#' @param align Column alignment string, e.g. `"lrr"`. Defaults to left for
#'   text and right for numbers.
#' @param digits Rounding for numeric columns.
#' @param striped Shade alternate rows with `gray!15`, as the briefs do.
#' @param caption Optional caption.
#' @param file Optional path to write to instead of returning.
#'
#' @return The LaTeX as a character vector, invisibly when `file` is given.
#'
#' @examples
#' chart.table(data.frame(Fuel = c("Gas", "Coal"), MW = c(561783, 188382)))
#'
#' @export
chart.table <- function(data, align = NULL, digits = 1, striped = TRUE,
                        caption = NULL, file = NULL) {
  if (!is.data.frame(data)) rlang::abort("`data` must be a data frame.")

  num <- vapply(data, is.numeric, logical(1))
  if (!is.null(digits)) {
    data[num] <- lapply(data[num], function(v) formatC(round(v, digits),
                                                       format = "f", digits = digits,
                                                       big.mark = ","))
  }
  if (is.null(align)) align <- paste(ifelse(num, "r", "l"), collapse = "")

  esc <- function(x) gsub("([&%$#_{}])", "\\\\\\1", as.character(x))
  rows <- vapply(seq_len(nrow(data)), function(i) {
    cells <- paste(esc(unlist(data[i, ], use.names = FALSE)), collapse = " & ")
    prefix <- if (isTRUE(striped) && i %% 2 == 1) "\\rowcolor{gray!15} " else ""
    paste0(prefix, cells, " \\\\")
  }, character(1))

  out <- c(
    "\\begin{tabular}{@{} " %+% align %+% " @{}}",
    "\\toprule[1.3pt]",
    paste(paste0("\\textbf{", esc(names(data)), "}", collapse = " & "), "\\\\"),
    "\\midrule[1pt]",
    rows,
    "\\bottomrule[1.5pt]",
    "\\end{tabular}"
  )
  if (!is.null(caption)) {
    out <- c("\\begin{table}[H]", "\\centering", out,
             paste0("\\caption{", esc(caption), "}"), "\\end{table}")
  }

  if (is.null(file)) return(out)
  writeLines(out, file)
  invisible(out)
}

`%+%` <- function(a, b) paste0(a, b)
