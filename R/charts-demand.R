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

# Label layout ----------------------------------------------------------------
#
# Labels are sized in points but positioned in data units (hours x MW), so every
# collision check converts through the panel's size. These are the measured
# panel extents at the wide slot (11 x 6 in) the demand curve is published at;
# without the tier brackets the right margin shrinks and the panel widens.
dc_panel_pt <- function(has_bands, has_title = FALSE) {
  c(w = if (has_bands) 630 else 705, h = if (has_title) 366 else 401)
}

# Approximate text box, in points, for a Merriweather label at ggplot size `s`
# (mm). Widths were measured per glyph class: capitals run ~0.74 em, lower case
# ~0.54 em, and bold ~8% wider. Height is one em, which covers ascenders and
# descenders.
dc_text_pt <- function(label, s, face = "plain") {
  em <- s * ggplot2::.pt
  ch <- strsplit(label, "")[[1]]
  w  <- sum(ifelse(grepl("[A-Z]", ch), 0.74, 0.54)) * em
  if (face == "bold") w <- w * 1.08
  c(w = w, h = em)
}

# Which rows of the box matrix `m` (columns x0, x1, y0, y1) does `b` overlap?
dc_overlaps <- function(m, b) {
  m[, "x0"] < b[["x1"]] & b[["x0"]] < m[, "x1"] & m[, "y0"] < b[["y1"]] & b[["y0"]] < m[, "y1"]
}

# Does the segment (x0, y0)-(x1, y1) pass through any box in `m`? Sampled along
# its length, which is exact enough at label scale and handles any angle.
dc_seg_hits <- function(x0, y0, x1, y1, m) {
  if (!nrow(m)) return(FALSE)
  t <- seq(0, 1, length.out = 30)
  x <- x0 + (x1 - x0) * t; y <- y0 + (y1 - y0) * t
  any(outer(x, m[, "x0"], ">") & outer(x, m[, "x1"], "<") &
        outer(y, m[, "y0"], ">") & outer(y, m[, "y1"], "<"))
}

# Does segment p1-p2 properly cross any segment in the rows of `m`
# (columns x0, y0, x1, y1)?
dc_seg_cross <- function(p1, p2, m) {
  if (!nrow(m)) return(FALSE)
  orient <- function(ax, ay, bx, by, cx, cy) sign((bx - ax) * (cy - ay) - (by - ay) * (cx - ax))
  o1 <- orient(p1[1], p1[2], p2[1], p2[2], m[, 1], m[, 2])
  o2 <- orient(p1[1], p1[2], p2[1], p2[2], m[, 3], m[, 4])
  o3 <- orient(m[, 1], m[, 2], m[, 3], m[, 4], p1[1], p1[2])
  o4 <- orient(m[, 1], m[, 2], m[, 3], m[, 4], p2[1], p2[2])
  any(o1 * o2 < 0 & o3 * o4 < 0)
}

# Place every band label. Each band is first offered a spot ON the band, at the
# widest stretch that holds the text clear of its edges, the threshold rules and
# every label already placed; big bands choose first, so the large fuel-coloured
# names land where the reader expects them. A band too thin to hold even the
# small type anywhere is labelled in the white space above the curve, with a
# leader that reaches it without crossing another label, leader or annotation.
# Those few labels compete for the same scarce room under the reserve band, so
# every order of placing them is tried and the arrangement with the shortest,
# clearest leaders overall wins -- a greedy pass lets the first label take the
# only open spot and pushes the rest across the chart. Returns one row per
# label: text position and style, plus the leader (NA when on its band).
dc_place_labels <- function(stacked, levs, names_by_code, fills, bold, italic,
                            hmax, ymin_axis, ymax_axis, bands, obstacles,
                            segments, ceiling_y, panel) {
  ux <- 24 / panel[["w"]]                       # hours per point
  uy <- (ymax_axis - ymin_axis) / panel[["h"]]  # MW per point
  pad_x <- 4 * ux                               # breathing room, in data units
  pad_y <- 2.5 * uy
  edge  <- 12 * ux                              # keep text off the panel's ends
  rule  <- 5 * uy                               # and off the dotted threshold rules

  # `stacked` holds one block of rows per band, in `levs` order, over the same
  # hours -- so each bound reshapes into an hours x bands matrix.
  hours <- stacked$hour[stacked$fueltype == levs[1]]
  lo_m  <- matrix(stacked$ymin,  nrow = length(hours), dimnames = list(NULL, levs))
  hi_m  <- matrix(stacked$ymax,  nrow = length(hours), dimnames = list(NULL, levs))
  val_m <- matrix(stacked$value, nrow = length(hours), dimnames = list(NULL, levs))
  top   <- hi_m[, ncol(hi_m)]

  box_m <- function(l) {
    if (!length(l)) return(matrix(numeric(0), 0, 4, dimnames = list(NULL, c("x0", "x1", "y0", "y1"))))
    do.call(rbind, lapply(l, function(b) b[c("x0", "x1", "y0", "y1")]))
  }
  seg_m <- function(l) if (length(l)) do.call(rbind, l) else matrix(numeric(0), 0, 4)

  thick <- apply(val_m, 2, max)
  todo  <- levs[thick > 0]
  todo  <- todo[order(-thick[todo])]
  face_of <- function(f) if (f %in% bold) "bold" else if (f %in% italic) "italic" else "plain"
  clear_of_rules <- function(y0, y1) is.null(bands) || !any(unname(bands) > y0 - rule & unname(bands) < y1 + rule)
  label_row <- function(f, x, y, size, colour, inside, ax = NA_real_, ay = NA_real_,
                        lx = NA_real_, ly = NA_real_) {
    data.frame(fueltype = f, label = unname(names_by_code[f]), x = x, y = y, size = size,
               face = face_of(f), colour = colour, inside = inside,
               ax = ax, ay = ay, lx = lx, ly = ly, stringsAsFactors = FALSE)
  }

  inside <- function(f, s, placed) {
    lab <- unname(names_by_code[f])
    tb <- dc_text_pt(lab, s, face_of(f)); w <- tb[["w"]] * ux; h <- tb[["h"]] * uy
    # The em box already carries some internal leading, so the small type
    # needs less margin to the band's edges than the large.
    pin <- if (s < 4) 1.5 * uy else pad_y
    # The label's centre of gravity: where the band carries most of its energy.
    xc <- sum(hours * val_m[, f]) / sum(val_m[, f])
    best <- NULL
    for (x in seq(w / 2 + edge, hmax - w / 2 - edge, by = 0.05)) {
      span <- hours >= x - w / 2 - pad_x & hours <= x + w / 2 + pad_x
      lo <- max(lo_m[span, f]); hi <- min(hi_m[span, f])
      room <- (hi - lo) - h - 2 * pin
      if (room < 0) next
      # Centre on the band, stepping off a threshold rule if one runs through.
      for (y in unique(c((lo + hi) / 2, seq(lo + h / 2 + pin, hi - h / 2 - pin, length.out = 9)))) {
        box <- c(x0 = x - w / 2, x1 = x + w / 2, y0 = y - h / 2, y1 = y + h / 2)
        if (!clear_of_rules(box[["y0"]], box[["y1"]]) || any(dc_overlaps(placed, box))) next
        # Favour roomy spots (capped, so a huge band does not drag its label to
        # an edge), then spots near the band's centre of gravity.
        score <- min(room / h, 1.5) - 0.04 * abs(x - xc) - 0.2 * abs(y - (lo + hi) / 2) / h
        if (is.null(best) || score > best$score) best <- list(x = x, y = y, box = box, score = score)
        break
      }
    }
    if (is.null(best)) return(NULL)
    row <- label_row(f, best$x, best$y, s, essp.textcolor(unname(fills[f])), TRUE)
    # Registered with a margin, so no later leader grazes the text.
    attr(row, "box") <- best$box + c(-pad_x, pad_x, -pad_y, pad_y)
    row
  }

  # Candidate spots for a band's leadered label, best first: up to `k` spots
  # at least half a label apart, so the arrangement search below has real
  # alternatives to choose between.
  outside <- function(f, placed, leaders, k = 4) {
    lab <- unname(names_by_code[f])
    tb <- dc_text_pt(lab, 3.6, face_of(f)); w <- tb[["w"]] * ux; h <- tb[["h"]] * uy
    # Every free spot for the label in the white space above the curve, below
    # the ceiling, off the threshold rules and clear of what is placed. Past
    # the curve's end the strip up to the panel edge is open too, but only
    # above the tier bracket, which fills that strip below the top rule.
    xs <- seq(w / 2 + edge, 24 - w / 2 - pad_x, by = 0.2)
    bracket_top <- if (is.null(bands)) -Inf else max(unname(bands)) + rule
    spots <- do.call(rbind, lapply(xs, function(x) {
      span <- hours >= x - w / 2 - pad_x & hours <= x + w / 2 + pad_x
      y0 <- max(top[span]) + h / 2 + 2 * pad_y
      if (x + w / 2 > hmax - edge) y0 <- max(y0, bracket_top + h / 2 + pad_y)
      y1 <- min(y0 + 80 * uy, ceiling_y - pad_y - h / 2)
      if (y1 < y0) return(NULL)
      ys <- seq(y0, y1, by = h / 4)
      cbind(x = x, y = ys)
    }))
    if (is.null(spots)) return(list())
    # Side padding keeps two floated names from running together as one phrase.
    bx <- cbind(x0 = spots[, "x"] - w / 2 - 1.5 * pad_x, x1 = spots[, "x"] + w / 2 + 1.5 * pad_x,
                y0 = spots[, "y"] - h / 2 - pad_y,     y1 = spots[, "y"] + h / 2 + pad_y)
    keep <- rep(TRUE, nrow(bx))
    if (!is.null(bands)) {
      for (r in unname(bands)) keep <- keep & !(r > bx[, "y0"] - rule & r < bx[, "y1"] + rule)
    }
    for (j in seq_len(nrow(placed))) {
      keep <- keep & !(bx[, "x0"] < placed[j, "x1"] & placed[j, "x0"] < bx[, "x1"] &
                         bx[, "y0"] < placed[j, "y1"] & placed[j, "y0"] < bx[, "y1"])
    }
    spots <- spots[keep, , drop = FALSE]; bx <- bx[keep, , drop = FALSE]
    if (!nrow(spots)) return(list())

    # Candidate tips: the band's midline wherever it is at least half as thick
    # as it ever gets, so the arrowhead lands unmistakably inside it.
    v <- val_m[, f]
    ok <- which(v >= max(v) * 0.5 & hours >= 0.3 & hours <= hmax - 0.3)
    # An arrowhead on a threshold rule is half hidden by it; keep tips off the
    # rules wherever the band allows.
    if (!is.null(bands)) {
      mid <- (lo_m[ok, f] + hi_m[ok, f]) / 2
      off <- vapply(mid, function(m) all(abs(unname(bands) - m) > rule), logical(1))
      if (any(off)) ok <- ok[off]
    }
    ok <- ok[unique(round(seq(1, length(ok), length.out = min(length(ok), 24))))]
    clear <- placed
    if (nrow(clear)) clear <- clear + rep(c(-6 * ux, 6 * ux, -4 * uy, 4 * uy), each = nrow(clear))
    pr <- expand.grid(s = seq_len(nrow(spots)), t = ok)
    ax <- hours[pr$t]; ay <- (lo_m[cbind(pr$t, match(f, levs))] + hi_m[cbind(pr$t, match(f, levs))]) / 2
    # The leader leaves the point of the label's box nearest its tip: the
    # lower edge when the tip is below, the facing side when it is beside.
    lx <- pmin(pmax(ax, bx[pr$s, "x0"]), bx[pr$s, "x1"])
    ly <- pmin(pmax(ay, bx[pr$s, "y0"]), bx[pr$s, "y1"])
    len <- sqrt(((lx - ax) / ux)^2 + ((ly - ay) / uy)^2)   # points
    # Short leaders read best: past 60 pt every extra point costs triple. A
    # tip on a thinner stretch of the band costs more, since the arrowhead
    # must land unmistakably inside it.
    cheap <- len + 2 * pmax(0, len - 60) + 40 * (1 - v[pr$t] / max(v))
    cand <- order(cheap)
    cand <- cand[len[cand] <= 200]

    found <- list()
    for (i in cand) {
      if (length(found) >= k) break
      b <- bx[pr$s[i], ]
      # Distinct alternatives only: skip spots within half a label of one kept.
      sx <- spots[pr$s[i], "x"]; sy <- spots[pr$s[i], "y"]
      if (length(found) && any(vapply(found, function(r) abs(r$x - sx) < w / 2 && abs(r$y - sy) < h / 2,
                                      logical(1)))) next
      # Leaders keep a clear margin from every other label, so none reads as
      # belonging to the name it passes.
      if (dc_seg_hits(lx[i], ly[i], ax[i], ay[i], clear)) next
      if (dc_seg_cross(c(lx[i], ly[i]), c(ax[i], ay[i]), leaders)) next
      # A leader should travel through white space and cross other bands only
      # briefly on its way in -- one that runs along inside a neighbour reads
      # as pointing at it -- so distance spent inside other bands costs extra,
      # as does each band crossed.
      ts <- seq(0.02, 0.98, length.out = 25)
      hi_i <- findInterval(lx[i] + (ax[i] - lx[i]) * ts, hours, all.inside = TRUE)
      py <- ly[i] + (ay[i] - ly[i]) * ts
      hit <- lo_m[hi_i, , drop = FALSE] <= py & hi_m[hi_i, , drop = FALSE] >= py &
        val_m[hi_i, , drop = FALSE] > 0
      hit[, f] <- FALSE
      cost <- cheap[i] + 1.5 * len[i] * mean(rowSums(hit) > 0) + 8 * sum(colSums(hit) > 0)
      row <- label_row(f, spots[pr$s[i], "x"], spots[pr$s[i], "y"], 3.6, essp.colors("ink"),
                       FALSE, ax[i], ay[i], lx[i], ly[i])
      attr(row, "box") <- b
      attr(row, "cost") <- cost
      found[[length(found) + 1L]] <- row
    }
    found[order(vapply(found, attr, numeric(1), "cost"))]
  }

  # Pass 1: on-band labels, big type then small, thickest bands first.
  placed <- obstacles
  out <- list()
  pending <- character(0)
  for (f in todo) {
    pm <- box_m(placed)
    row <- inside(f, 4.5, pm)
    if (is.null(row)) row <- inside(f, 3.6, pm)
    if (is.null(row)) {
      pending <- c(pending, f)
    } else {
      placed[[length(placed) + 1L]] <- attr(row, "box")
      out[[length(out) + 1L]] <- row
    }
  }

  # Pass 2: leadered labels. Search orders and each label's alternative spots
  # depth-first, pruning any branch already dearer than the best complete
  # arrangement found.
  best_cost <- Inf
  arrange <- function(remaining, placed, leaders, so_far) {
    if (!length(remaining)) {
      best_cost <<- min(best_cost, so_far)
      return(list(rows = list(), cost = 0))
    }
    best <- NULL
    # Beyond a handful of labels, keep the thickest-first order and one spot
    # each, so the search stays quick.
    many <- length(remaining) > 4
    for (f in if (many) remaining[1] else remaining) {
      for (row in outside(f, box_m(placed), seg_m(leaders), k = if (many) 1 else 4)) {
        c1 <- so_far + attr(row, "cost")
        if (c1 >= best_cost) next
        rest <- arrange(setdiff(remaining, f),
                        c(placed, list(attr(row, "box"))),
                        c(leaders, list(c(row$lx, row$ly, row$ax, row$ay))), c1)
        if (is.null(rest)) next
        cost <- attr(row, "cost") + rest$cost
        if (is.null(best) || cost < best$cost) best <- list(rows = c(list(row), rest$rows), cost = cost)
      }
    }
    best
  }
  if (length(pending)) {
    res <- arrange(pending, placed, segments, 0)
    if (!is.null(res)) {
      out <- c(out, res$rows)
    } else {
      # Never drop a name: as a last resort, label the band where it is thickest.
      for (f in pending) {
        i <- which.max(val_m[, f])
        out[[length(out) + 1L]] <- label_row(f, hours[i], (lo_m[i, f] + hi_m[i, f]) / 2, 3.6,
                                             essp.colors("ink"), TRUE)
      }
    }
  }
  do.call(rbind, lapply(out, function(r) { attributes(r)[c("box", "cost")] <- NULL; r }))
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
#' @param title,subtitle Optional plot title and subtitle. Pass them here rather
#'   than adding `labs()` afterwards: the label layout needs to know the panel
#'   they leave.
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
                              storage        = "auto",
                              title          = NULL,
                              subtitle       = NULL,
                              year           = as.integer(format(Sys.Date(), "%Y"))) {
  # String front door: chart.demandcurve("Solar", "CISO", "2024", "summer").
  # A character first argument is the highlight tech, and the next positionals
  # are read as (ba, year, season); a data frame runs the engine below.
  if (is.character(data) && !is.data.frame(data)) {
    return(essp_demandcurve_strings(
      tech = data, ba = highlight, year = order, season = bands %||% "summer",
      storage_ba = storage_ba, storage = storage, smooth = smooth,
      title = title, subtitle = subtitle,
      palette = palette, semester = semester))
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

  # The peak marker's label sits above the peak, so the axis has to leave room
  # for it -- otherwise ggplot drops the annotation outside the scale and warns
  # about a removed row instead of drawing it.
  headroom  <- if (isTRUE(mark_peak)) 1.16 else 1.06
  ymax_axis <- max(c(total$mw * headroom, reserve_margin))
  # When a reserve band sits above the peak, the "Peak Generation" label is lifted
  # clear of it -- so the axis has to reach past the label, or ggplot drops it
  # and the annotation silently disappears.
  if (isTRUE(mark_peak) && !is.null(reserve_margin)) {
    ymax_axis <- max(ymax_axis, max(reserve_margin) * 1.075)
  }

  # Charging is the battery acting as load. It belongs below the axis: drawing
  # it as a band in the stack would overstate generation by the charging energy.
  charge <- attr(data, "essp_charge")
  ymin_axis <- 0
  if (!is.null(charge)) {
    # Its label sits inside the ribbon when the ribbon is deep enough to hold
    # it (about 11 pt with margins). A shallow one -- a small battery fleet --
    # gets the label just beneath it instead, and the axis reaches down far
    # enough to hold it, rather than white text spilling over the axis.
    cmax <- max(charge$charge_mw)
    lab_mw <- (ymax_axis + cmax * 1.25) * 11 / dc_panel_pt(!is.null(bands), !is.null(title))[["h"]]
    charge_inside <- cmax >= 1.6 * lab_mw
    ymin_axis <- if (charge_inside) -cmax * 1.25 else -(cmax + 2.2 * lab_mw)
  }

  p <- ggplot2::ggplot()

  # Reserve margin sits behind the stack so the bands stay readable over it.
  if (!is.null(reserve_margin)) {
    if (length(reserve_margin) != 2) rlang::abort("`reserve_margin` must be length 2.")
    # The label centres on noon unless the curve rises into the band there and
    # would paint over it; then it moves to the nearest stretch the curve
    # leaves clear.
    rw <- dc_text_pt("Reserve Margin", 4.5)[["w"]] * 24 / dc_panel_pt(!is.null(bands), !is.null(title))[["w"]]
    rx <- seq(rw / 2 + 0.3, 24 - rw / 2 - 0.3, by = 0.1)
    clear_x <- rx[vapply(rx, function(x) {
      span <- total$hour >= x - rw / 2 - 0.2 & total$hour <= x + rw / 2 + 0.2
      !any(total$mw[span] > min(reserve_margin))
    }, logical(1))]
    rm_x <- if (length(clear_x)) clear_x[which.min(abs(clear_x - 12))] else 12
    p <- p +
      ggplot2::annotate("rect", xmin = -Inf, xmax = Inf,
                        ymin = min(reserve_margin), ymax = max(reserve_margin),
                        fill = "#FFCCCC", alpha = 0.7) +
      ggplot2::annotate("segment", x = -Inf, xend = Inf,
                        y = reserve_margin, yend = reserve_margin,
                        linetype = "dashed", colour = "#C0392B", linewidth = 0.6) +
      ggplot2::annotate("text", x = rm_x, y = mean(reserve_margin),
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
                        y = if (charge_inside) -cmax / 2 else -cmax - 1.1 * lab_mw,
                        label = "Storage charging", size = 2.9, fontface = "bold",
                        colour = if (charge_inside) "#FFFFFF" else essp.colors("ink"))
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

  # Everything the band labels must keep clear of, as boxes in data units: the
  # reserve band (labels stay below it), the peak dot, its callout and arrow.
  panel <- dc_panel_pt(!is.null(bands), !is.null(title))
  ux <- 24 / panel[["w"]]; uy <- (ymax_axis - ymin_axis) / panel[["h"]]
  obstacles <- list()
  segments  <- list()
  ceiling_y <- if (!is.null(reserve_margin)) min(reserve_margin) else ymax_axis

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
    ax0 <- px + side * 2.2; ay0 <- py + lift * 0.88
    ax1 <- px + side * 0.25; ay1 <- py + ymax_axis * 0.010
    p <- p +
      ggplot2::annotate("point", x = px, y = py, colour = "#C00000", size = 2.4) +
      ggplot2::annotate("segment", x = ax0, xend = ax1, y = ay0, yend = ay1,
                        arrow = ggplot2::arrow(length = ggplot2::unit(0.2, "cm"),
                                               type = "closed"),
                        colour = "black", linewidth = 0.5) +
      ggplot2::annotate("text", x = px + side * 2.3, y = py + lift,
                        label = "Peak Generation", hjust = if (side < 0) 1 else 0,
                        size = 3, colour = "black")
    obstacles[[length(obstacles) + 1L]] <- c(x0 = px - 6 * ux, x1 = px + 6 * ux,
                                             y0 = py - 6 * uy, y1 = py + 6 * uy)
    pk <- dc_text_pt("Peak Generation", 3)
    lx0 <- if (side < 0) px + side * 2.3 - pk[["w"]] * ux else px + side * 2.3
    obstacles[[length(obstacles) + 1L]] <- c(x0 = lx0, x1 = lx0 + pk[["w"]] * ux,
                                             y0 = py + lift - pk[["h"]] * uy / 2,
                                             y1 = py + lift + pk[["h"]] * uy / 2)
    # The callout arrow, as a chain of small boxes, so no label sits on it.
    for (t in seq(0, 1, length.out = 12)) {
      x <- ax0 + (ax1 - ax0) * t; y <- ay0 + (ay1 - ay0) * t
      obstacles[[length(obstacles) + 1L]] <- c(x0 = x - 3 * ux, x1 = x + 3 * ux,
                                               y0 = y - 3 * uy, y1 = y + 3 * uy)
    }
    segments[[length(segments) + 1L]] <- c(ax0, ay0, ax1, ay1)
  }

  labs <- dc_place_labels(stacked, levs, names_by_code, fills, bold, italic,
                          hmax, ymin_axis, ymax_axis, bands, obstacles,
                          segments, ceiling_y, panel)
  on_band <- labs[labs$inside, , drop = FALSE]
  led     <- labs[!labs$inside, , drop = FALSE]

  p <- p +
    ggplot2::geom_text(
      data = on_band,
      ggplot2::aes(x = .data$x, y = .data$y, label = .data$label,
                   colour = .data$colour, fontface = .data$face, size = .data$size)
    ) +
    ggplot2::scale_size_identity()
  if (nrow(led)) {
    p <- p +
      ggplot2::geom_segment(
        data = led,
        ggplot2::aes(x = .data$lx, xend = .data$ax, y = .data$ly, yend = .data$ay),
        arrow = ggplot2::arrow(length = ggplot2::unit(0.14, "cm"), type = "closed"),
        colour = "black", linewidth = 0.35
      ) +
      ggplot2::geom_text(
        data = led,
        ggplot2::aes(x = .data$x, y = .data$y, label = .data$label,
                     fontface = .data$face),
        colour = essp.colors("ink"), size = 3.6
      )
  }

  p +
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
    # The bands are generation. A grid's own demand differs from it by net
    # interchange (PJM exports, California imports), so the axis says what is
    # actually stacked.
    ggplot2::labs(x = NULL, y = "Generation (MW)", title = title, subtitle = subtitle) +
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
