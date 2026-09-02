# Battery storage --------------------------------------------------------------
#
# EIA-930 carries a BAT fuel type, but NOT for every respondent: individual
# balancing authorities such as CISO report no BAT series and fold battery
# output into OTH. The regional respondents (US48, CAL, ERCO, ...) do report it.
# analyze.storage() checks and says so rather than returning an empty frame.
#
# Sign convention follows EIA's: negative net generation is charging (the
# fleet is consuming), positive is discharging.

#' Hourly battery charge and discharge
#'
#' Splits the EIA `BAT` series into charging and discharging by hour of day, and
#' derives the implied round-trip efficiency from the energy that went in versus
#' the energy that came back out.
#'
#' @param ba Respondent code. Must be one that reports `BAT` -- regional codes
#'   such as `"CAL"`, `"US48"`, or `"ERCO"` do; most individual balancing
#'   authorities do not.
#' @param start,end Period bounds, `"YYYY-MM-DDTHH"`.
#' @param local Use the respondent's local time. Defaults to `TRUE`.
#'
#' @return A tibble of `hour`, `charge_mw` (positive magnitude), `discharge_mw`,
#'   and `net_mw`. The implied round-trip efficiency is attached as the
#'   `essp_rte` attribute.
#'
#' @examples
#' \dontrun{
#' analyze.storage("CAL", "2026-07-01T00", "2026-07-28T23")
#' }
#'
#' @export
analyze.storage <- function(ba, start, end = NULL, local = TRUE) {
  .w <- essp_when_window(start, end); start <- .w$start; end <- .w$end
  # The upstream "no data returned" warning is replaced below by an error that
  # explains WHY there is none, so muffle it rather than emitting both.
  b <- withCallingHandlers(
    tryCatch(analyze.fuelshape(ba, start, end, fuels = "BAT", local = local),
             error = function(e) NULL),
    warning = function(w) {
      if (grepl("No data returned", conditionMessage(w))) invokeRestart("muffleWarning")
    }
  )
  if (is.null(b) || !nrow(b)) {
    rlang::abort(c(
      paste0("No battery (BAT) series for respondent \"", ba, "\"."),
      i = "Most individual balancing authorities do not report BAT; their battery output is folded into OTH.",
      i = "Try a regional respondent such as \"CAL\", \"ERCO\", or \"US48\"."
    ))
  }

  b <- b[order(b$hour), , drop = FALSE]
  out <- tibble::tibble(
    hour         = b$hour,
    # Charging is reported as negative generation; report it as a magnitude so
    # the two columns can be compared and plotted directly.
    charge_mw    = pmax(-b$mean_mw, 0),
    discharge_mw = pmax(b$mean_mw, 0),
    net_mw       = b$mean_mw
  )

  ch <- sum(out$charge_mw)
  di <- sum(out$discharge_mw)
  attr(out, "essp_rte") <- if (ch > 0) di / ch else NA_real_
  attr(out, "essp_respondent") <- ba
  out
}

#' Battery charge and discharge by hour
#'
#' Discharging is drawn above the axis, charging below it, so the daily cycle
#' reads as one shape: batteries absorb surplus midday solar and return it
#' during the evening ramp.
#'
#' The two areas are deliberately not forced to balance. Charging energy always
#' exceeds discharging energy by the round-trip loss, and showing that gap is
#' the point -- the existing hand-built figures inflated charging to fit a
#' label, which is the one distortion this chart exists to avoid.
#'
#' @param data Output of [analyze.storage()].
#' @param show_rte Annotate the implied round-trip efficiency.
#' @param accent,semester,year As in [chart.fleetmakeup()].
#'
#' @return A ggplot object.
#'
#' @examples
#' \dontrun{
#' analyze.storage("CAL", "2026-07-01T00", "2026-07-28T23") |>
#'   chart.storagebands()
#' }
#'
#' @export
chart.storagebands <- function(data, show_rte = TRUE, accent = NULL,
                               semester = "Fall",
                               year = as.integer(format(Sys.Date(), "%Y"))) {
  for (cl in c("hour", "charge_mw", "discharge_mw")) {
    if (!cl %in% names(data)) {
      rlang::abort(paste0("`data` needs a \"", cl, "\" column; use analyze.storage()."))
    }
  }

  th <- essp.theme(semester = semester, year = year)
  if (is.null(accent)) accent <- attr(th, "essp_accent")

  d <- data[order(data$hour), , drop = FALSE]
  span <- max(c(d$charge_mw, d$discharge_mw))

  p <- ggplot2::ggplot(d, ggplot2::aes(x = .data$hour)) +
    ggplot2::geom_area(ggplot2::aes(y = .data$discharge_mw),
                       fill = accent, alpha = 0.9) +
    ggplot2::geom_area(ggplot2::aes(y = -.data$charge_mw),
                       fill = essp.colors("ugagray"), alpha = 0.9) +
    ggplot2::geom_hline(yintercept = 0, colour = essp.colors("ink"), linewidth = 0.4) +
    ggplot2::annotate("text", x = 0.4, y = span * 0.72, label = "Discharging",
                      hjust = 0, size = 3, fontface = "bold",
                      colour = essp.colors("ink")) +
    ggplot2::annotate("text", x = 0.4, y = -span * 0.72, label = "Charging",
                      hjust = 0, size = 3, fontface = "bold",
                      colour = essp.colors("ink"))

  if (isTRUE(show_rte)) {
    rte <- attr(data, "essp_rte")
    if (!is.null(rte) && !is.na(rte)) {
      p <- p + ggplot2::labs(caption = sprintf(
        "Energy returned is %.0f%% of energy absorbed \u2014 the round-trip loss.",
        rte * 100))
    }
  }

  p +
    ggplot2::scale_x_continuous(
      breaks = seq(0, 24, 4),
      labels = c("12a", "4a", "8a", "12p", "4p", "8p", "12a"),
      expand = c(0, 0)
    ) +
    ggplot2::scale_y_continuous(labels = function(v) scales::comma(abs(v))) +
    ggplot2::labs(x = NULL, y = "Storage (MW)") +
    th
}
