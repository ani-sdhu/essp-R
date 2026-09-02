# Demand-side charts ------------------------------------------------------------

# Spline-smooth a band across the day so the stack reads as a curve rather than
# 24 straight segments. The existing figures do this; without it an hourly
# series looks like a polygon.
smooth_band <- function(hour, value, out_hours, spar = 0.35) {
  ok <- !is.na(hour) & !is.na(value)
  if (sum(ok) < 4) return(stats::approx(hour, value, xout = out_hours, rule = 2)$y)
  fit <- stats::smooth.spline(hour[ok], value[ok], spar = spar)
  v <- stats::predict(fit, out_hours)$y
  pmax(v, 0)
}

#' Daily demand curve, stacked by fuel
#'
#' Average generation by hour of day, stacked bottom-to-top in dispatch order,
#' with the resource under discussion highlighted and every band labelled.
#'
#' Bands are ordered baseload-first so the stack reads the way the grid
#' dispatches: nuclear and coal at the bottom running flat, gas filling the
#' middle, and peaking resources riding the evening ramp on top.
#'
#' The optional annotations reproduce the house demand-curve figure: dotted
#' threshold lines with labelled brackets, a shaded reserve margin, and a marked
#' peak.
#'
#' @param data Output of [analyze.fuelshape()], or any tibble with `hour`,
#'   `fueltype`, and `mean_mw`.
#' @param highlight Fuel code or name to emphasise, e.g. `"NG"`.
#' @param order Fuel codes bottom-to-top. Defaults to a dispatch-like order;
#'   anything not listed is appended.
#' @param bands Named numeric vector of threshold levels in MW, e.g.
#'   `c(Baseload = 22500, Intermediate = 38000)`. Drawn as dotted lines with
#'   labelled brackets down the right-hand side.
#' @param reserve_margin Length-2 numeric giving the lower and upper bounds of a
#'   shaded reserve band, e.g. `c(44000, 48000)`.
#' @param mark_peak Mark the peak of the stack with a dot and a leader.
#' @param palette `"house"` (default) gives every fuel its own established
#'   color, matching the concept-brief figures. `"highlight"` uses flat greys
#'   behind a single accent, matching the resource-brief figures.
#'
#'   The two compose with `highlight`: under `"house"` the named fuel keeps
#'   full saturation while the others are muted but keep their hues, so the
#'   colour coding survives; under `"highlight"` the others go grey.
#' @param smooth Spline-smooth the bands. `FALSE` draws the raw hourly steps.
#' @param bold Fuel codes whose labels are drawn bold.
#' @param italic Fuel codes whose labels are drawn italic.
#' @param accent,semester,year As in [chart.fleetmakeup()].
#' @param label_min Smallest share of peak, in percent, that still gets a label.
#'
#' @return A ggplot object.
#'
#' @examples
#' \dontrun{
#' analyze.fuelshape("CISO", "2024-07-01T00", "2024-07-31T23") |>
#'   chart.demandcurve(
#'     highlight      = "NG",
#'     bands          = c(Baseload = 22500, Intermediate = 38000),
#'     reserve_margin = c(44000, 48000)
#'   )
#' }
#'
#' @export
chart.demandcurve <- function(data,
                              highlight      = NULL,
                              order          = NULL,
                              bands          = NULL,
                              reserve_margin = NULL,
                              mark_peak      = FALSE,
                              smooth         = TRUE,
                              palette        = c("highlight", "house"),
                              bold           = highlight,
                              italic         = NULL,
                              accent         = NULL,
                              label_min      = 4,
                              semester       = "Fall",
                              storage_ba     = NULL,
                              year           = as.integer(format(Sys.Date(), "%Y"))) {
  # String front door: chart.demandcurve("Solar", "CISO", "2024", "summer").
  # A character first argument is the highlight tech, and the next positionals
  # are read as (ba, year, season); a data frame runs the engine below.
  if (is.character(data) && !is.data.frame(data)) {
    return(essp_demandcurve_strings(
      tech = data, ba = highlight, year = order, season = bands %||% "summer",
      storage_ba = storage_ba, smooth = smooth, palette = palette,
      semester = semester))
  }

  for (cl in c("hour", "fueltype", "mean_mw")) {
    if (!cl %in% names(data)) {
      rlang::abort(paste0("`data` needs a \"", cl, "\" column; use analyze.fuelshape()."))
    }
  }

  palette <- match.arg(palette, c("highlight", "house"))

  th <- essp.theme(semester = semester, year = year)
  if (is.null(accent)) accent <- attr(th, "essp_accent")

  # Stack order taken from the house figure3 script, bottom to top:
  #   Nuclear, Coal, NGCC, Solar, Wind, Hydro, Storage, NGCT
  # Baseload sits on the floor, combined cycle above it, the variable
  # renewables in the middle, and storage discharge plus simple-cycle peakers
  # riding the evening peak on top -- peakers last because they are the plant
  # of last resort.
  if (is.null(order)) {
    order <- c("NUC", "COL",
               "NGCC", "NG", "NG Steam",
               "SUN", "WND", "WAT", "GEO", "OTH", "OIL",
               "Storage", "BAT",
               "NGCT")
  }
  present <- unique(data$fueltype)
  levs <- c(intersect(order, present), setdiff(present, order))

  # Hours where a fuel is a net consumer -- batteries charging, or solar netted
  # against station load -- come back negative. A stacked area cannot express a
  # negative band: it would draw below the axis and misplace everything above
  # it. Clamp and say which fuels were affected rather than silently producing
  # a chart whose bands do not sum to the total.
  neg <- unique(data$fueltype[data$mean_mw < 0])
  if (length(neg)) {
    warning("Negative generation clamped to zero for: ", paste(neg, collapse = ", "),
            ". These hours are net consumption, which a stacked area cannot show.",
            call. = FALSE)
    data$mean_mw <- pmax(data$mean_mw, 0)
  }

  names_by_code <- stats::setNames(
    if ("fuel_name" %in% names(data)) data$fuel_name else as.character(data$fueltype),
    as.character(data$fueltype)
  )
  # "highlight" (default) is the resource-brief look: grey every band, then
  # paint each highlighted fuel its own house colour (Solar green, Wind orange,
  # ...), so one or more named resources pop while the rest recede. `highlight`
  # arrives as EIA codes ("SUN","WND"); essp.fuelcolors() accepts codes.
  # "house" keeps every fuel's own colour and merely mutes the others.
  fills <- if (palette == "house") {
    f <- essp.fuelcolors(levs)
    if (!is.null(highlight)) {
      others <- setdiff(levs, highlight)
      f[others] <- essp.mute(f[others])
    }
    f
  } else {
    greys <- essp.palette("grey", n = max(length(levs), 2L))
    f <- stats::setNames(greys[seq_along(levs)], levs)
    for (h in intersect(highlight, levs)) f[h] <- unname(essp.fuelcolors(h))
    f
  }

  # Build the stack explicitly rather than leaving it to position = "stack".
  # Labels must sit on the band they name, and the only way to guarantee that
  # is to derive band bounds and label positions from one calculation.
  # The house figure runs the smoothed curve to the data's own maximum and
  # leaves the last sliver of the day clear, which is where the tier brackets
  # sit. 500 points matches its spline resolution.
  hmax <- max(data$hour, na.rm = TRUE)
  hours_out <- if (isTRUE(smooth)) seq(0, hmax, length.out = 500) else sort(unique(data$hour))
  mat <- vapply(levs, function(f) {
    d <- data[data$fueltype == f, , drop = FALSE]
    d <- d[order(d$hour), , drop = FALSE]
    if (isTRUE(smooth)) smooth_band(d$hour, d$mean_mw, hours_out)
    else stats::approx(d$hour, d$mean_mw, xout = hours_out, rule = 2)$y
  }, numeric(length(hours_out)))
  if (is.null(dim(mat))) mat <- matrix(mat, nrow = length(hours_out))
  cum <- t(apply(mat, 1, cumsum))
  if (is.null(dim(cum))) cum <- matrix(cum, nrow = length(hours_out))

  stacked <- do.call(rbind, lapply(seq_along(levs), function(j) {
    data.frame(
      hour     = hours_out,
      fueltype = levs[j],
      value    = mat[, j],
      ymax     = cum[, j],
      ymin     = cum[, j] - mat[, j],
      stringsAsFactors = FALSE
    )
  }))
  stacked$ymid <- stacked$ymin + stacked$value / 2
  stacked$fueltype <- factor(stacked$fueltype, levels = levs)

  # The dot marks the top of what is actually drawn, so it cannot float above
  # or below the visible curve.
  total <- data.frame(hour = hours_out, mw = cum[, ncol(cum)])
  peak_i <- which.max(total$mw)

  # Label each band at its thickest point, where the text has the most room.
  # Two bands need their anchor overridden: Nuclear is near-constant baseload,
  # so its thickest point is an arbitrary hour (usually the far left) -- pin it
  # to mid-day so the label sits centred over the band. Hydro is a thin band
  # that peaks at the right edge, where its label clips off the panel -- keep
  # its anchor inside the interior so it stays fully drawn.
  anchor_hour <- c(NUC = 12)   # centre the label at this hour
  interior    <- c(WAT = 20.5) # cap the anchor hour at this, pulling it inward
  labs <- do.call(rbind, lapply(split(stacked, stacked$fueltype), function(d) {
    if (!nrow(d) || all(d$value <= 0)) return(NULL)
    ft <- as.character(d$fueltype[1])
    if (ft %in% names(anchor_hour)) {
      d[which.min(abs(d$hour - anchor_hour[[ft]])), , drop = FALSE]
    } else if (ft %in% names(interior) && d$hour[which.max(d$value)] > interior[[ft]]) {
      cand <- d[d$hour <= interior[[ft]], , drop = FALSE]
      cand[which.max(cand$value), , drop = FALSE]
    } else {
      d[which.max(d$value), , drop = FALSE]
    }
  }))
  labs$label <- unname(names_by_code[as.character(labs$fueltype)])
  # Every band that is visible at all gets named. A band too thin to hold text
  # is labelled outside with a leader rather than dropped -- an unlabelled
  # colour is unreadable, and silently omitting it hides real generation.
  # The peak marker's label sits above the peak, so the axis has to leave room
  # for it. Computed here because the label placement below depends on it.
  headroom  <- if (isTRUE(mark_peak)) 1.16 else 1.06
  ymax_axis <- max(c(total$mw * headroom, reserve_margin))
  # When a reserve band sits above the peak, the "Peak Demand" label is lifted
  # clear of it -- so the axis has to reach past the label, or ggplot drops it
  # and the annotation silently disappears.
  if (isTRUE(mark_peak) && !is.null(reserve_margin)) {
    ymax_axis <- max(ymax_axis, max(reserve_margin) * 1.075)
  }

  labs <- labs[labs$value > 0, , drop = FALSE]
  # A size-4.5 label occupies roughly 4.5% of the panel height.
  text_h <- ymax_axis * 0.045
  labs$outside <- labs$value < text_h

  # Several very thin bands stack up near the peak, so their leader labels land
  # on top of one another. Walk them in vertical order and push each clear of
  # the one below.
  if (any(labs$outside)) {
    oi <- which(labs$outside)
    oi <- oi[order(labs$ymid[oi])]
    for (k in seq_along(oi)[-1]) {
      gap <- labs$ymid[oi[k]] - labs$ymid[oi[k - 1]]
      if (gap < text_h) labs$ymid[oi[k]] <- labs$ymid[oi[k - 1]] + text_h
    }
  }
  # Nudge any inside label that would land on a threshold rule.
  if (!is.null(bands)) {
    for (b in unname(bands)) {
      hit <- !labs$outside & abs(labs$ymid - b) < text_h * 0.6
      labs$ymid[hit] <- labs$ymid[hit] +
        ifelse(labs$ymid[hit] >= b, 1, -1) * text_h * 0.75
    }
  }
  labs$colour <- essp.textcolor(unname(fills[as.character(labs$fueltype)]))
  labs$face <- ifelse(as.character(labs$fueltype) %in% bold, "bold",
                      ifelse(as.character(labs$fueltype) %in% italic, "italic", "plain"))
  # Keep labels near the day's edges inside the panel.
  labs$hjust <- ifelse(labs$hour <= 1.5, 0, ifelse(labs$hour >= 21.0, 1, 0.5))

  # The peak marker's label sits above the peak, so the axis has to leave
  # room for it -- otherwise ggplot drops the annotation outside the scale
  # and warns about a removed row instead of drawing it.

  # Charging is the battery acting as load. It belongs below the axis: drawing
  # it as a band in the stack would overstate generation by the charging energy.
  charge <- attr(data, "essp_charge")
  ymin_axis <- if (!is.null(charge)) -max(charge$charge_mw) * 1.25 else 0

  p <- ggplot2::ggplot()

  # Reserve margin sits behind the stack so the bands stay readable over it.
  if (!is.null(reserve_margin)) {
    if (length(reserve_margin) != 2) rlang::abort("`reserve_margin` must be length 2.")
    p <- p +
      ggplot2::annotate("rect", xmin = -Inf, xmax = Inf,
                        ymin = min(reserve_margin), ymax = max(reserve_margin),
                        fill = "#FFCCCC", alpha = 0.7) +
      ggplot2::annotate("segment", x = -Inf, xend = Inf,
                        y = reserve_margin, yend = reserve_margin,
                        linetype = "dashed", colour = "#C0392B", linewidth = 0.6) +
      ggplot2::annotate("text", x = 12, y = mean(reserve_margin),
                        label = "Reserve Margin", colour = "#C0392B",
                        fontface = "italic", size = 4.5)
  }

  if (!is.null(charge)) {
    p <- p +
      ggplot2::geom_ribbon(
        data = charge,
        ggplot2::aes(x = .data$hour, ymin = -.data$charge_mw, ymax = 0),
        fill = unname(essp.fuelcolors("Storage")), alpha = 0.92) +
      ggplot2::geom_hline(yintercept = 0, colour = essp.colors("ugablack"),
                          linewidth = 0.5) +
      ggplot2::annotate("text", x = charge$hour[which.max(charge$charge_mw)],
                        y = -max(charge$charge_mw) / 2, label = "Storage charging",
                        size = 2.9, fontface = "bold", colour = "#FFFFFF")
  }

  p <- p +
    ggplot2::geom_ribbon(
      data = stacked,
      ggplot2::aes(x = .data$hour, ymin = .data$ymin, ymax = .data$ymax,
                   fill = .data$fueltype),
      colour = "#FFFFFF", linewidth = 0.2
    )

  if (!is.null(bands)) {
    p <- p + ggplot2::geom_hline(yintercept = unname(bands), linetype = "dotted",
                                 colour = "black", linewidth = 0.6)

    # Brackets down the right margin, spanning each tier.
    edges <- c(0, sort(unname(bands)))
    labels <- names(bands)[order(unname(bands))]
    # The bracket sits just outside the panel; its label sits outside that
    # again. Both positions and the right margin below have to agree, or the
    # labels are silently clipped at the device edge.
    for (i in seq_along(labels)) {
      p <- p +
        ggplot2::annotate("segment", x = hmax + 0.45, xend = hmax + 0.45,
                          y = edges[i] + ymax_axis * 0.012,
                          yend = edges[i + 1] - ymax_axis * 0.012,
                          arrow = ggplot2::arrow(length = ggplot2::unit(0.12, "cm"),
                                                 ends = "both", type = "open"),
                          colour = "black", linewidth = 0.4) +
        ggplot2::annotate("text", x = hmax + 0.85, y = mean(edges[i:(i + 1)]),
                          label = labels[i], hjust = 0, size = 4.5,
                          lineheight = 0.9, colour = "black")
    }
  }

  # The storage discharge band is a thin slab at the evening peak -- too thin
  # to hold text -- so it gets an outside label with a leader, as in the house
  # figure. Charging keeps its label inside, where there is room.
  st <- labs[as.character(labs$fueltype) %in% c("Storage", "BAT"), , drop = FALSE]
  if (nrow(st)) {
    labs <- labs[!as.character(labs$fueltype) %in% c("Storage", "BAT"), , drop = FALSE]
    sx <- st$hour[1]; sy <- st$ymid[1]
    p <- p +
      ggplot2::annotate("text", x = min(sx + 2.0, 23.4), y = sy + ymax_axis * 0.11,
                        label = "Storage", size = 3.2, hjust = 0.5,
                        colour = essp.colors("ink")) +
      ggplot2::annotate("segment",
                        x = min(sx + 1.8, 23.2), xend = sx + 0.15,
                        y = sy + ymax_axis * 0.085, yend = sy + ymax_axis * 0.008,
                        arrow = ggplot2::arrow(length = ggplot2::unit(0.2, "cm"),
                                               type = "closed"),
                        colour = "black", linewidth = 0.5)
  }

  if (isTRUE(mark_peak)) {
    px <- total$hour[peak_i]; py <- total$mw[peak_i]
    # The peak usually falls in the evening, so a label offset to the right
    # runs off the panel. Flip it to the left whenever there is not room.
    side <- if (px > 16) -1 else 1
    # Lift the label above the reserve band when there is one; otherwise it
    # lands on the lower dashed bound, which happened on every chart.
    lift <- if (!is.null(reserve_margin)) {
      (max(reserve_margin) - py) + ymax_axis * 0.035
    } else {
      ymax_axis * 0.095
    }
    p <- p +
      ggplot2::annotate("point", x = px, y = py, colour = "#C00000", size = 2.4) +
      ggplot2::annotate("segment",
                        x = px + side * 2.2, xend = px + side * 0.25,
                        y = py + lift * 0.88, yend = py + ymax_axis * 0.010,
                        arrow = ggplot2::arrow(length = ggplot2::unit(0.2, "cm"),
                                               type = "closed"),
                        colour = "black", linewidth = 0.5) +
      ggplot2::annotate("text", x = px + side * 2.3, y = py + lift,
                        label = "Peak Demand", hjust = if (side < 0) 1 else 0,
                        size = 3, colour = "black")
  }

  p +
    ggplot2::geom_text(
      data = labs[!labs$outside, , drop = FALSE],
      ggplot2::aes(x = .data$hour, y = .data$ymid, label = .data$label,
                   colour = .data$colour, hjust = .data$hjust,
                   fontface = .data$face),
      size = 4.5
    ) +
    ggplot2::geom_text(
      data = labs[labs$outside, , drop = FALSE],
      ggplot2::aes(x = .data$hour + 0.35, y = .data$ymid, label = .data$label,
                   fontface = .data$face),
      colour = "black", hjust = 0, size = 3.4
    ) +
    ggplot2::geom_segment(
      data = labs[labs$outside, , drop = FALSE],
      ggplot2::aes(x = .data$hour + 0.30, xend = .data$hour,
                   y = .data$ymid, yend = .data$ymid),
      colour = "black", linewidth = 0.3
    ) +
    ggplot2::scale_colour_identity(guide = "none") +
    ggplot2::scale_fill_manual(values = fills, guide = "none") +
    ggplot2::scale_x_continuous(
      breaks = seq(0, 24, 2),
      labels = c(sprintf("%d:00", seq(0, 22, 2)), "0:00"),
      expand = c(0, 0)
    ) +
    ggplot2::scale_y_continuous(
      # A fixed 5,000 step gives 9 labels on a 45 GW grid and 33 on PJM's
      # 160 GW one. Pick a round step that lands near ten labels instead.
      breaks = {
        span <- ymax_axis - ymin_axis
        step <- 10^floor(log10(span / 8))
        step <- step * c(1, 2, 5, 10)[which.min(abs(span / (step * c(1, 2, 5, 10)) - 8))]
        seq(floor(ymin_axis / step) * step, ceiling(ymax_axis / step) * step, by = step)
      },
      labels = scales::comma, expand = c(0, 0),
      limits = c(ymin_axis, ymax_axis)) +
    ggplot2::coord_cartesian(xlim = c(0, 24), clip = "off") +
    ggplot2::labs(x = NULL, y = "Demand (MW)") +
    th +
    # The house figure is drawn on a bare canvas: no panel grid, no axis lines,
    # just tick labels and a rotated y title. Gridlines behind a filled stack
    # are invisible anyway, and the horizontal rules here carry meaning
    # (baseload and intermediate thresholds), so a decorative grid would
    # compete with them.
    ggplot2::theme(
      panel.grid           = ggplot2::element_blank(),
      panel.grid.major     = ggplot2::element_blank(),
      panel.grid.major.x   = ggplot2::element_blank(),
      panel.grid.major.y   = ggplot2::element_blank(),
      panel.grid.minor     = ggplot2::element_blank(),
      panel.background = ggplot2::element_blank(),
      axis.line        = ggplot2::element_blank(),
      axis.ticks       = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(size = 8, colour = "black",
                                          margin = ggplot2::margin(t = 5)),
      axis.text.y = ggplot2::element_text(size = 9, colour = "black", hjust = 1,
                                          margin = ggplot2::margin(r = 5)),
      axis.title.y = ggplot2::element_text(size = 11, colour = "black", angle = 90,
                                           margin = ggplot2::margin(r = 10)),
      legend.position = "none",
      # Exactly the house script's margin: 60pt on the right leaves room for
      # the tier bracket labels.
      plot.margin = ggplot2::margin(10, if (is.null(bands)) 20 else 95, 10, 10)
    )
}

#' Load duration curve
#'
#' Demand sorted highest to lowest against the share of hours at or above each
#' level. The steep left edge is the argument for peaking capacity: the highest
#' few percent of hours can set a requirement that sits idle the rest of the
#' year.
#'
#' @param data Output of [analyze.durationcurve()].
#' @param highlight_pct Optional percentage of hours to mark with a reference
#'   line, e.g. `5` to show the level exceeded only 5% of the time.
#' @param accent,semester,year As in [chart.fleetmakeup()].
#'
#' @return A ggplot object.
#'
#' @examples
#' \dontrun{
#' analyze.durationcurve("CISO", "2024-01-01T00", "2024-12-31T23") |>
#'   chart.durationcurve(highlight_pct = 5)
#' }
#'
#' @export
chart.durationcurve <- function(data,
                                highlight_pct = NULL,
                                accent        = NULL,
                                semester      = "Fall",
                                year          = as.integer(format(Sys.Date(), "%Y"))) {
  for (cl in c("pct_hours", "mw")) {
    if (!cl %in% names(data)) {
      rlang::abort(paste0("`data` needs a \"", cl, "\" column; use analyze.durationcurve()."))
    }
  }

  th <- essp.theme(semester = semester, year = year)
  if (is.null(accent)) accent <- attr(th, "essp_accent")

  p <- ggplot2::ggplot(data, ggplot2::aes(x = .data$pct_hours, y = .data$mw)) +
    ggplot2::geom_area(fill = essp.colors("ugacreamery"), alpha = 0.6) +
    ggplot2::geom_line(colour = accent, linewidth = 0.9)

  if (!is.null(highlight_pct)) {
    lvl <- stats::approx(data$pct_hours, data$mw, xout = highlight_pct)$y
    p <- p +
      ggplot2::geom_segment(x = highlight_pct, xend = highlight_pct,
                            y = -Inf, yend = lvl, linetype = "dashed",
                            colour = essp.colors("termgray"), linewidth = 0.4) +
      ggplot2::annotate("text", x = highlight_pct + 2, y = lvl,
                        label = sprintf("%.0f%% of hours above\n%s MW",
                                        highlight_pct, format(round(lvl), big.mark = ",")),
                        hjust = 0, vjust = 1, size = 3,
                        colour = essp.colors("ink"), lineheight = 0.95)
  }

  p +
    ggplot2::scale_x_continuous(labels = function(x) paste0(x, "%"), expand = c(0, 0)) +
    ggplot2::scale_y_continuous(labels = scales::comma,
                                expand = ggplot2::expansion(c(0, 0.05))) +
    ggplot2::labs(x = "Share of hours", y = "Demand (MW)") +
    th
}

#' Intensity grid
#'
#' A heatmap of one value across two discrete dimensions -- most often hour of
#' day against month, which shows seasonal and daily patterns at once.
#'
#' Uses a single-hue sequential ramp. A rainbow scale would imply category
#' boundaries where the data is continuous.
#'
#' @param data A data frame.
#' @param x,y,value Column names, as strings.
#' @param label Print values in each cell. Off by default; a dense grid becomes
#'   unreadable with a number in every cell.
#' @param semester,year As in [chart.fleetmakeup()].
#'
#' @return A ggplot object.
#'
#' @examples
#' \dontrun{
#' analyze.loadshape("CISO", "2024-01-01T00", "2024-12-31T23", by = "month") |>
#'   chart.heatmap("hour", "month", "mean_mw")
#' }
#'
#' @export
chart.heatmap <- function(data, x, y, value, label = FALSE,
                          semester = "Fall",
                          year = as.integer(format(Sys.Date(), "%Y"))) {
  for (cl in c(x, y, value)) {
    if (!cl %in% names(data)) rlang::abort(paste0("Column \"", cl, "\" not found."))
  }

  ramp <- essp.palette("sequential", 9)

  p <- ggplot2::ggplot(data, ggplot2::aes(x = .data[[x]], y = .data[[y]],
                                          fill = .data[[value]])) +
    ggplot2::geom_tile(colour = "#FFFFFF", linewidth = 0.3)

  if (isTRUE(label)) {
    data$.txt <- essp.textcolor(
      grDevices::colorRampPalette(ramp)(100)[
        pmax(1, ceiling(scales::rescale(data[[value]], to = c(1, 100))))]
    )
    p <- p + ggplot2::geom_text(
      data = data,
      ggplot2::aes(label = round(.data[[value]]), colour = .data$.txt),
      size = 2.6
    ) + ggplot2::scale_colour_identity(guide = "none")
  }

  p +
    ggplot2::scale_fill_gradientn(colours = ramp, labels = scales::comma) +
    ggplot2::scale_x_continuous(expand = c(0, 0)) +
    ggplot2::labs(x = NULL, y = NULL, fill = NULL) +
    essp.theme(semester = semester, year = year) +
    ggplot2::theme(panel.grid = ggplot2::element_blank(),
                   legend.position = "right",
                   legend.key.width = ggplot2::unit(8, "pt"))
}
