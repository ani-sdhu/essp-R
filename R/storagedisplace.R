#' Insert battery discharge into the dispatch stack
#'
#' Adds a `Storage` band to an hourly fuel shape and subtracts an equal amount
#' from the plant it displaces, so total generation is unchanged.
#'
#' This is what makes the storage figure honest. Batteries do not add supply on
#' top of demand -- they substitute for whatever would otherwise have run.
#' Discharge therefore displaces the most expensive plant first (simple-cycle
#' peakers), then the next (combined cycle), leaving the demand envelope exactly
#' where it was. Stacking storage on top instead would draw a taller peak than
#' the grid actually served.
#'
#' @param data Hourly fuel shape, as from [analyze.fuelshape()].
#' @param storage Output of [analyze.storage()], or any frame with `hour`,
#'   `charge_mw`, and `discharge_mw`.
#' @param displaces Fuel codes to displace, in order. Defaults to peakers then
#'   combined cycle.
#'
#' @return `data` with a `Storage` band added and the displaced fuels reduced.
#'   Charging is carried on the result as the `essp_charge` attribute, since it
#'   sits below the axis rather than in the stack.
#'
#' @examples
#' \dontrun{
#' fs <- analyze.fuelshape("CISO", "2024-07-01T00", "2024-07-31T23")
#' st <- analyze.storage("CAL", "2024-07-01T00", "2024-07-31T23")
#' analyze.storagedisplace(fs, st)
#' }
#'
#' @export
analyze.storagedisplace <- function(data, storage,
                                    displaces = c("NGCT", "NGCC", "NG")) {
  for (cl in c("hour", "fueltype", "mean_mw")) {
    if (!cl %in% names(data)) rlang::abort(paste0("`data` needs a \"", cl, "\" column."))
  }
  for (cl in c("hour", "discharge_mw")) {
    if (!cl %in% names(storage)) {
      rlang::abort(paste0("`storage` needs a \"", cl, "\" column; use analyze.storage()."))
    }
  }

  hours <- sort(unique(data$hour))
  disch <- stats::approx(storage$hour, storage$discharge_mw, xout = hours, rule = 2)$y
  names(disch) <- as.character(hours)

  # Take the displaced energy from each fuel in turn, in merit order.
  remaining <- disch
  for (f in displaces) {
    idx <- which(data$fueltype == f)
    if (!length(idx)) next
    for (i in idx) {
      h <- as.character(data$hour[i])
      take <- min(data$mean_mw[i], remaining[[h]])
      data$mean_mw[i] <- data$mean_mw[i] - take
      remaining[[h]] <- remaining[[h]] - take
    }
    if (all(remaining <= 1e-6)) break
  }

  short <- disch - remaining
  if (any(remaining > 1)) {
    warning(sprintf(
      "Storage discharge exceeds displaceable generation in %d hour(s); only the displaceable part is shown.",
      sum(remaining > 1)), call. = FALSE)
  }

  tmpl <- data[data$hour %in% hours, , drop = FALSE]
  tmpl <- tmpl[!duplicated(tmpl$hour), , drop = FALSE]
  tmpl <- tmpl[order(tmpl$hour), , drop = FALSE]
  tmpl$fueltype  <- "Storage"
  tmpl$mean_mw   <- unname(short[as.character(tmpl$hour)])
  if ("fuel_name" %in% names(tmpl)) tmpl$fuel_name <- "Storage"

  out <- rbind(data, tmpl)
  if ("charge_mw" %in% names(storage)) {
    attr(out, "essp_charge") <- data.frame(
      hour = hours,
      charge_mw = stats::approx(storage$hour, storage$charge_mw, xout = hours, rule = 2)$y
    )
  }
  tibble::as_tibble(out)
}
