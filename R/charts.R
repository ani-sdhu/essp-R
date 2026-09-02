# Charts and output -------------------------------------------------------------

#' Figure slot dimensions
#'
#' Named sizes matching the slots the two-pager templates reserve, taken from
#' the `ggsave()` calls in the existing per-folder scripts so regenerated
#' figures drop into the same space.
#'
#' @param slot Optional slot name. `NULL` (default) returns all.
#'
#' @return A tibble of `slot`, `width`, and `height`, in inches.
#'
#' @examples
#' essp.slots()
#'
#' @export
essp.slots <- function(slot = NULL) {
  s <- tibble::tribble(
    ~slot,      ~width, ~height, ~description,
    "profile",     3.2,     4.2, "Small profile graphic beside the characteristics table",
    "wide",       11.0,     6.0, "Full-width figure, e.g. the demand curve",
    "concept",    10.0,     6.0, "Full-width figure in the concept briefs (DemandCurve figure1/figure2)",
    "column",      9.0,     5.0, "Single-column time series",
    "facet",       8.0,     9.0, "Small multiples, one panel per group"
  )
  if (is.null(slot)) return(s)

  unknown <- setdiff(slot, s$slot)
  if (length(unknown)) {
    rlang::abort(c(
      paste0("Unknown slot: ", paste(unknown, collapse = ", ")),
      i = paste0("Available: ", paste(s$slot, collapse = ", "))
    ))
  }
  s[s$slot %in% slot, , drop = FALSE]
}

#' Capacity versus generation, as a 0-100% stacked comparison
#'
#' The signature figure: two bars, one for capacity and one for generation, each
#' summing to 100%, with a single resource highlighted. The gap between a
#' resource's two segments is the point -- solar and wind occupy more of the
#' capacity bar than the generation bar because their capacity factors are low,
#' while nuclear does the reverse.
#'
#' Every segment is labelled directly. That is a requirement rather than a
#' flourish: the non-highlighted resources are distinguished only by grey steps,
#' which are close together on purpose, so labels carry identity instead of
#' color.
#'
#' @param data Output of [analyze.mix()], or any tibble with `metric`,
#'   `resource`, and `share`.
#' @param highlight Resource to emphasise, e.g. `"Solar"`.
#' @param accent Accent color. Defaults to the theme's semester accent.
#' @param show_pct Print the percentage inside the highlighted segments.
#' @param label_min Smallest share, in percent, that still gets a label. Below
#'   this the text will not fit and is dropped.
#' @param semester,year Passed to [essp.theme()].
#'
#' @return A ggplot object.
#'
#' @examples
#' \dontrun{
#' analyze.mix("US", 2024) |> chart.fleetmakeup(highlight = "Solar")
#' }
#'
#' @export
chart.fleetmakeup <- function(data,
                              highlight = NULL,
                              accent    = NULL,
                              show_pct  = TRUE,
                              label_min = 3,
                              palette   = c("highlight", "house"),
                              semester  = "Fall",
                              year      = as.integer(format(Sys.Date(), "%Y"))) {
  for (cl in c("metric", "resource", "share")) {
    if (!cl %in% names(data)) {
      rlang::abort(paste0("`data` needs a \"", cl, "\" column; use analyze.mix()."))
    }
  }
  palette <- match.arg(palette)

  th <- essp.theme(semester = semester, year = year)
  # Deliberately do NOT default `accent` to the semester colour here: a NULL
  # accent lets essp.highlight() paint the highlighted resource its own house
  # colour (Solar green, etc.), the same colour it carries on the demand curve,
  # rather than the rotating red accent. An explicit `accent` still wins.

  # Stack bottom-to-top in the order resources appear, so both bars agree.
  levs <- unique(data$resource)
  data$resource <- factor(data$resource, levels = rev(levs))
  data$metric   <- factor(data$metric, levels = c("Capacity", "Generation"))

  # "house" paints every resource its own colour, exactly like the demand
  # curve, so the two figures read as one system; "highlight" is the older
  # brief look -- greys behind a single accent. A named highlight still bolds
  # its label and shows its share in both modes.
  # "highlight" (default): grey every resource, then paint each highlighted one
  # its own house colour -- supports one or many highlights. "house": every
  # resource keeps its own colour.
  fills <- if (palette == "house") {
    essp.fuelcolors(as.character(levs))
  } else if (is.null(accent)) {
    greys <- essp.palette("grey", n = max(length(levs), 2L))
    f <- stats::setNames(greys[seq_along(levs)], as.character(levs))
    for (h in intersect(highlight, as.character(levs))) f[h] <- unname(essp.fuelcolors(h))
    f
  } else {
    essp.highlight(levs, highlight = highlight, accent = accent)
  }

  # Midpoint of each segment, for placing its label.
  data <- data[order(data$metric, match(as.character(data$resource), levs)), ]
  mid <- do.call(rbind, lapply(split(data, data$metric), function(d) {
    d$ymid <- cumsum(d$share) - d$share / 2
    d
  }))

  mid$label <- as.character(mid$resource)
  if (isTRUE(show_pct) && !is.null(highlight)) {
    hl <- mid$resource %in% highlight
    # A two-line label needs roughly twice the vertical room, and the format is
    # chosen from the resource's SMALLEST segment so both bars label it the
    # same way -- a resource switching layout between the two columns reads as
    # an inconsistency rather than a size cue.
    smallest <- min(mid$share[hl])
    if (smallest >= label_min * 2.4) {
      mid$label[hl] <- sprintf("%s\n%.1f%%", mid$resource[hl], mid$share[hl])
    } else {
      mid$label[hl] <- sprintf("%s  %.1f%%", mid$resource[hl], mid$share[hl])
    }
  }
  # A segment shorter than its text is worse than an unlabelled one.
  mid$label[mid$share < label_min] <- ""
  mid$bold <- if (is.null(highlight)) FALSE else mid$resource %in% highlight

  # One rule for every segment: the label takes whichever of black or white
  # contrasts better with the fill beneath it. Special-casing the accent would
  # make text colour flip as the semester accent rotates, and would leave dark
  # grey segments carrying unreadable black text.
  mid$label_colour <- essp.textcolor(unname(fills[as.character(mid$resource)]))

  ggplot2::ggplot(mid, ggplot2::aes(x = .data$metric, y = .data$share,
                                    fill = .data$resource)) +
    # A thin surface-colored border is the 2px gap between stacked segments.
    ggplot2::geom_col(width = 0.6, colour = "#FFFFFF", linewidth = 0.6) +
    ggplot2::geom_text(
      data = mid[!mid$bold & nzchar(mid$label), ],
      ggplot2::aes(y = .data$ymid, label = .data$label,
                   colour = .data$label_colour),
      size = 3.2, lineheight = 0.9
    ) +
    ggplot2::geom_text(
      data = mid[mid$bold & nzchar(mid$label), ],
      ggplot2::aes(y = .data$ymid, label = .data$label,
                   colour = .data$label_colour),
      size = 3.5, fontface = "bold", lineheight = 0.9
    ) +
    ggplot2::scale_colour_identity(guide = "none") +
    ggplot2::scale_fill_manual(values = fills, guide = "none") +
    ggplot2::scale_y_continuous(expand = c(0, 0)) +
    ggplot2::labs(x = NULL, y = NULL) +
    th +
    ggplot2::theme(
      axis.text.y        = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_blank(),
      axis.text.x        = ggplot2::element_text(size = ggplot2::rel(1.05),
                                                 colour = essp.colors("ink"))
    )
}

#' Save a figure at a template slot size
#'
#' Writes PNG and PDF at 300 dpi using the dimensions of a named slot, so a
#' regenerated figure lands exactly where the LaTeX expects it.
#'
#' Uses the `ragg` device when available for cleaner text rendering, falling
#' back to the default device otherwise.
#'
#' @param plot A ggplot object.
#' @param path Output path. The extension is replaced per format.
#' @param slot A slot name from [essp.slots()].
#' @param width,height Explicit dimensions in inches, overriding `slot`.
#' @param formats Which formats to write; `c("png", "pdf")` by default.
#' @param dpi Resolution for raster output.
#'
#' @return The paths written, invisibly.
#'
#' @examples
#' \dontrun{
#' p <- chart.fleetmakeup(analyze.mix("US", 2024), highlight = "Solar")
#' essp.save(p, "figures/figure1_capacity_generation.png", slot = "profile")
#' }
#'
#' @export
essp.save <- function(plot, path, slot = NULL, width = NULL, height = NULL,
                      formats = c("png", "pdf"), dpi = 300) {
  if (is.null(width) || is.null(height)) {
    if (is.null(slot)) {
      rlang::abort(c("Give either a `slot` or both `width` and `height`.",
                     i = "See essp.slots() for the available slots."))
    }
    s <- essp.slots(slot)
    width  <- width  %||% s$width[[1]]
    height <- height %||% s$height[[1]]
  }

  stem <- sub("\\.[A-Za-z0-9]+$", "", path)
  out <- character(0)

  for (fmt in formats) {
    f <- paste0(stem, ".", fmt)
    if (fmt == "png" && requireNamespace("ragg", quietly = TRUE)) {
      ggplot2::ggsave(f, plot, device = ragg::agg_png, width = width,
                      height = height, units = "in", dpi = dpi, bg = "white")
    } else {
      ggplot2::ggsave(f, plot, width = width, height = height,
                      units = "in", dpi = dpi, bg = "white")
    }
    out <- c(out, f)
  }

  invisible(out)
}
