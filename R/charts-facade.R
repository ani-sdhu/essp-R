# Friendly, string-argument front doors over the two charts the program
# actually reaches for. Both are additive: the underlying data-path charts
# (chart.fleetmakeup, chart.demandcurve) are unchanged and still take a frame.

#' Months feeding a season
#'
#' Meteorological seasons. `"annual"` returns all twelve. Winter deliberately
#' returns `c(12, 1, 2)` -- non-contiguous within a calendar year, which the
#' callers handle by fetching Dec separately from Jan-Feb.
#'
#' @param season One of `summer`, `fall`/`autumn`, `winter`, `spring`, `annual`.
#' @return An integer vector of month numbers.
#' @keywords internal
essp_season_months <- function(season) {
  s <- tolower(trimws(season))
  switch(s,
         summer = 6:8,
         fall   = 9:11, autumn = 9:11,
         winter = c(12L, 1L, 2L),
         spring = 3:5,
         annual = 1:12, year = 1:12, all = 1:12,
         rlang::abort(paste0("Unknown season \"", season,
                             "\". Use summer, fall, winter, spring, or annual.")))
}

#' The respondent that actually carries a battery series for a BA
#'
#' Most individual balancing authorities fold battery output into `OTH`; the
#' regional respondent they report into carries the `BAT` series. This is fixed
#' grid topology (CISO reports into CAL), not a state-to-BA guess.
#'
#' @param ba A balancing-authority / respondent code.
#' @return The respondent to pull storage from -- the mapped region, or `ba`
#'   itself when no mapping is known.
#' @keywords internal
essp_storage_respondent <- function(ba) {
  map <- c(
    CISO = "CAL", BANC = "CAL", IID = "CAL", LDWP = "CAL", TIDC = "CAL",
    ERCO = "TEX",
    PJM  = "MIDA"
  )
  out <- map[[toupper(ba)]]
  if (is.null(out)) toupper(ba) else out
}

# Contiguous runs of months, e.g. c(12,1,2) -> list(1:2, 12).
essp_month_blocks <- function(mos) {
  m <- sort(unique(as.integer(mos)))
  gaps <- which(diff(m) > 1L)
  starts <- c(1L, gaps + 1L)
  ends   <- c(gaps, length(m))
  Map(function(a, b) c(m[a], m[b]), starts, ends)
}

# Last day of a given month, as "YYYY-MM-DD".
essp_month_end <- function(year, month) {
  format(seq(as.Date(sprintf("%04d-%02d-01", year, month)),
             by = "month", length.out = 2)[2] - 1, "%Y-%m-%d")
}

# Average hourly fuel shape over a season's months (Dec fetched apart from
# Jan-Feb), re-averaged by hour and fuel across blocks.
essp_fuelshape_season <- function(ba, year, mos) {
  parts <- lapply(essp_month_blocks(mos), function(b) {
    start <- sprintf("%04d-%02d-01T00", year, b[1])
    end   <- paste0(essp_month_end(year, b[2]), "T23")
    analyze.fuelshape(ba, start, end)
  })
  fs <- do.call(rbind, parts)
  ag <- stats::aggregate(mean_mw ~ hour + fueltype + fuel_name, data = fs, FUN = mean)
  tibble::as_tibble(ag[order(ag$fueltype, ag$hour), , drop = FALSE])
}

# Average hourly storage over a season's months, by hour across blocks.
essp_storage_season <- function(ba, year, mos) {
  parts <- lapply(essp_month_blocks(mos), function(b) {
    start <- sprintf("%04d-%02d-01T00", year, b[1])
    end   <- paste0(essp_month_end(year, b[2]), "T23")
    analyze.storage(ba, start, end)
  })
  st <- do.call(rbind, parts)
  ag <- stats::aggregate(cbind(charge_mw, discharge_mw, net_mw) ~ hour,
                         data = st, FUN = mean)
  tibble::as_tibble(ag[order(ag$hour), , drop = FALSE])
}

# Modelled battery profile -- the reference figure's raised-cosine wave, scaled
# to this grid's peak. Discharge is concentrated in the evening ramp; charging
# spreads across the solar-rich midday and carries ~1/RTE more energy. EIA's
# hourly feed has no battery fuel type, so the house figure was always drawn
# this way. Returned in the shape analyze.storagedisplace() expects.
essp_storage_model <- function(data) {
  tot  <- tapply(pmax(data$mean_mw, 0), data$hour, sum)
  peak <- max(tot, na.rm = TRUE)
  hrs  <- sort(unique(data$hour))
  bump <- function(h, center, halfwidth, pk) {
    o <- numeric(length(h)); w <- abs(h - center) < halfwidth
    o[w] <- pk * 0.5 * (1 + cos(pi * (h[w] - center) / halfwidth)); o
  }
  dis <- 0.09 * peak
  tibble::tibble(
    hour         = hrs,
    discharge_mw = bump(hrs, 19.5, 2.4, dis),
    charge_mw    = bump(hrs, 12.3, 4.2, dis / 0.85))
}

#' Capacity-vs-generation bar from plain strings
#'
#' The signature figure, fetched and drawn in one call:
#' `chart.generation("Solar", "CA", "2024")`. A shortcut for
#' `chart.fleetmakeup(analyze.mix(state, year), highlight = resource)`.
#'
#' @param resource Fuel to highlight (an [essp.fuelcolors()] name), or `NULL`.
#' @param state State code or `"US"` -- forwarded to [analyze.mix()].
#' @param year A year string or number; defaults to the latest complete year.
#' @param ... Passed to [chart.fleetmakeup()].
#'
#' @return A ggplot object.
#' @examples
#' \dontrun{
#' chart.generation("Solar", "CA", "2024")
#' }
#' @export
chart.generation <- function(resource = NULL, state = "US", year = NULL, ...) {
  if (is.null(year)) year <- essp_latest_year()
  yr <- essp_parse_when(year, "year")$years
  if (length(yr) != 1L) rlang::abort("Give a single year for a capacity-vs-generation bar.")
  # Greyscale bar with the highlighted resource(s) in their own house colour
  # (the same colour they carry on the demand curve), not the semester accent.
  # `resource` may name one ("Solar") or several ("Solar,Wind").
  if (!is.null(resource)) resource <- trimws(strsplit(resource, ",")[[1]])
  args <- utils::modifyList(list(highlight = resource), list(...))
  do.call(chart.fleetmakeup, c(list(analyze.mix(state, yr)), args))
}

# The string-argument path for chart.demandcurve(), dispatched from its guard.
essp_demandcurve_strings <- function(tech, ba, year, season = "summer",
                                     storage_ba = NULL,
                                     storage = c("auto", "model", "none"), ...) {
  storage <- match.arg(storage)
  ba <- essp_parse_where(ba, need = "ba")
  if (length(ba) != 1L) rlang::abort("Give a single balancing-authority code, e.g. \"CISO\".")
  yr <- essp_parse_when(year, "year")$years
  if (length(yr) != 1L) rlang::abort("Give a single year for a demand curve.")
  mos <- essp_season_months(season)

  fs <- essp_fuelshape_season(ba, yr, mos)
  gs <- analyze.gassplit(fs, ba = ba, year = yr)

  # Storage. "auto" tries the live BAT series (rare); "model" overlays the
  # reference figure's raised-cosine battery profile (EIA's hourly feed has no
  # battery fuel type, so the house figure was always modelled this way);
  # "none" draws no storage.
  st <- switch(storage,
    none  = NULL,
    model = essp_storage_model(gs),
    auto  = tryCatch(essp_storage_season(storage_ba %||% essp_storage_respondent(ba), yr, mos),
                     error = function(e) NULL))
  if (is.null(st) || !nrow(st)) {
    if (storage == "auto") {
      message("No live battery series; drawing the curve without storage bands ",
              "(pass storage = \"model\" for the modelled overlay).")
    }
    d <- gs
  } else {
    d <- analyze.storagedisplace(gs, st)
  }

  # Show only the reportable resources; fold everything else into one "Other"
  # band. NG steam is non-peaker gas, so it joins NGCC rather than Other.
  keep <- c("NUC", "COL", "NGCC", "NGCT", "SUN", "WND", "WAT", "Storage", "BAT")
  d$fueltype[d$fueltype == "NG Steam"] <- "NGCC"
  d$fueltype[!d$fueltype %in% keep] <- "OTH"
  name_of <- c(NUC = "Nuclear", COL = "Coal", NGCC = "NGCC", NGCT = "NGCT",
               SUN = "Solar", WND = "Wind", WAT = "Hydro",
               Storage = "Storage", BAT = "Storage", OTH = "Other")
  d$fuel_name <- unname(name_of[d$fueltype])
  charge <- attr(d, "essp_charge")            # below-zero charging survives the fold
  d <- stats::aggregate(mean_mw ~ hour + fueltype + fuel_name, data = d, FUN = sum)
  attr(d, "essp_charge") <- charge

  # Reproduce the full house figure3 look, not a stripped stack: reserve-margin
  # band, baseload/intermediate tier lines, and every fuel in its own colour.
  # Peak total demand across the day, from the stacked generation.
  tot  <- tapply(pmax(d$mean_mw, 0), d$hour, sum)
  peak <- max(tot, na.rm = TRUE)

  # Editorial tier boundaries in the reference figure's proportions -- at its
  # ~43 GW peak these land on 18,500 / 38,000 and a 44,000-48,000 reserve band.
  # ponytail: fraction-of-peak heuristic, not a true baseload calc; pass
  # `bands=`/`reserve_margin=` to override for a specific grid.
  bands   <- c("Baseload\n(24/7)" = round(peak * 0.43, -2),
               "Intermediate"      = round(peak * 0.88, -2))
  reserve <- c(round(peak * 1.02, -2), round(peak * 1.12, -2))

  # Grey every band and highlight the named resource(s) in their own house
  # colour. `tech` is one or more comma-separated resource NAMES ("Solar" or
  # "Solar,Wind"); map each to the EIA CODE the engine stacks by.
  # tech = "ALL"/"none" means no highlight -- every fuel in its own house
  # colour (the reference figure3 look). Otherwise map each named resource to
  # the EIA code the engine stacks by.
  code_of <- c(Nuclear = "NUC", Coal = "COL", Solar = "SUN", Wind = "WND",
               Hydro = "WAT", "Natural Gas" = "NG", NGCC = "NGCC",
               NGCT = "NGCT", Storage = "Storage")
  if (toupper(trimws(tech)) %in% c("ALL", "NONE", "")) {
    tcodes <- NULL
  } else {
    techs  <- trimws(strsplit(tech, ",")[[1]])
    tcodes <- unname(code_of[techs])
    tcodes[is.na(tcodes)] <- techs[is.na(tcodes)]
  }

  # Caller's ... (season/palette/etc.) can override any of these defaults.
  # `highlight` drives both the fill (colour vs grey) and the bold labels. In
  # the full-colour case (no highlight) match the reference figure's emphasis:
  # Nuclear/Coal/Solar/Peakers bold, Wind italic.
  args <- utils::modifyList(
    list(mark_peak = TRUE, bands = bands, reserve_margin = reserve,
         highlight = tcodes,
         bold   = if (is.null(tcodes)) c("NUC", "COL", "SUN", "NGCT") else tcodes,
         italic = if (is.null(tcodes)) "WND" else NULL),
    list(...))
  # Title the chart with the balancing authority whose data it draws, by the
  # name it is registered under in EIA's hourly data, so a reader knows exactly
  # which entity's grid this is.
  who <- tryCatch({
    r <- essp.respondents()
    r$name[r$id == ba][1]
  }, error = function(e) NA_character_)
  who <- if (is.na(who) || !nzchar(who)) ba else paste0(who, " (", ba, ")")
  span <- paste(month.abb[c(mos[1], mos[length(mos)])], collapse = " to ")
  args <- utils::modifyList(
    list(title = who,
         subtitle = paste0("Average hourly generation by source, ", tolower(season), " ",
                           yr, " (", span, ")")),
    args)
  do.call(chart.demandcurve, c(list(d), args))
}
