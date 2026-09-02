# House fuel colors -------------------------------------------------------------
#
# These are the program's established figure colors, taken verbatim from the
# existing figure3 scripts so a regenerated chart matches the ones already in
# print. They are the standard; do not substitute a different palette without
# a deliberate decision.
#
# Coal is black. That also keeps the highlight idiom working: essp.highlight()
# greys the non-highlighted bands, so a mid-grey coal would have been
# invisible when highlighted, while black separates cleanly from the ramp.
#
# One accessibility note, recorded rather than silently "fixed": Storage
# (#8E44AD) and NGCT (#3B6B9A) are adjacent in the dispatch stack and separate
# by only DeltaE 2.8 under deuteranopia -- close to indistinguishable for
# red-green colorblind readers. The demand curve direct-labels every band,
# which is the secondary encoding that makes that acceptable. If a future chart
# drops those labels, the pair needs revisiting.

#' House fuel colors
#'
#' The program's standard fuel-to-color map, matching the existing figures.
#'
#' @param fuels Optional fuel names to return, in order. `NULL` returns all.
#'   Unknown names fall back to the `Other` grey.
#'
#' @return A named character vector of hex colors.
#'
#' @examples
#' essp.fuelcolors()
#' essp.fuelcolors(c("Nuclear", "Coal", "NGCC"))
#'
#' @export
essp.fuelcolors <- function(fuels = NULL) {
  pal <- c(
    "Nuclear"     = "#00B4D8",  # bright cyan
    "Coal"        = "#000000",  # black
    "NGCC"        = "#A8C5E2",  # light steel blue
    "Solar"       = "#70AD47",  # green
    "Wind"        = "#F4A743",  # orange
    "Hydro"       = "#D6E6F4",  # very pale blue
    "Storage"     = "#8E44AD",  # violet
    "NGCT"        = "#3B6B9A",  # medium-dark blue
    "Natural Gas" = "#A8C5E2",  # unsplit gas reads as combined cycle
    "NG Steam"    = "#7F9DB9",
    "NG Peakers"  = "#3B6B9A",
    "Petroleum"   = "#6E5A47",
    "Geothermal"  = "#B07AA1",
    "Other"       = "#C8C8C8"
  )

  # EIA fuel codes map onto the same house colors, so a chart fed straight from
  # essp.gather() colors correctly without renaming anything first.
  codes <- c(
    NUC = "Nuclear", COL = "Coal", NG = "Natural Gas", SUN = "Solar",
    WND = "Wind", WAT = "Hydro", BAT = "Storage", OIL = "Petroleum",
    GEO = "Geothermal", OTH = "Other", PS = "Storage"
  )

  if (is.null(fuels)) return(pal)

  resolved <- ifelse(fuels %in% names(pal), fuels,
                     ifelse(fuels %in% names(codes), codes[fuels], NA))
  out <- unname(pal[resolved])
  out[is.na(out)] <- unname(pal[["Other"]])
  stats::setNames(out, fuels)
}

#' Mute a colour so it recedes without losing its identity
#'
#' Drops most of a colour's saturation while keeping its hue and lightness, so
#' a band stays recognisably itself but sits behind a fully saturated one.
#'
#' This is what lets the house palette and `highlight` work together: greying
#' the other fuels outright would throw away the colour coding the reader has
#' already learned, while muting keeps it and still makes one band dominant.
#'
#' @param col Character vector of colours.
#' @param amount Fraction of the original saturation to keep, 0 to 1.
#'
#' @return A character vector of hex colours.
#'
#' @examples
#' essp.mute(essp.fuelcolors(c("Solar", "Wind")))
#'
#' @export
essp.mute <- function(col, amount = 0.25) {
  hsv <- grDevices::rgb2hsv(grDevices::col2rgb(col))
  grDevices::hsv(h = hsv["h", ], s = hsv["s", ] * amount, v = hsv["v", ])
}
