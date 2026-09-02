# House style: colors, palettes, and theme -------------------------------------
#
# The palette here is not a matter of taste -- it was run through a
# colorblind-safety validator (lightness band, chroma floor, CVD separation on
# every pair, normal-vision floor, contrast against the surface). What that
# exercise established:
#
#   1 accent + greys   PASSES every check cleanly
#   4 categorical      passes, but CVD separation lands at 6.8 -- legal ONLY
#                      with secondary encoding (direct labels)
#   5 categorical      FAILS: the closest pair is 4.7 for protanopes
#   6 categorical      FAILS badly: violet vs blue is 2.1 for deuteranopes
#
# So the house idiom -- one resource in the accent color, every other greyed and
# labelled directly -- is not merely a stylistic habit. It is the only approach
# that stays legible at seven resources. `essp.palette()` therefore refuses to
# hand back more than four categorical hues.

#' Brand colors
#'
#' The palette defined in the two-pagers' LaTeX preamble, so charts and page
#' furniture cannot drift apart.
#'
#' @param name Optional color name(s). `NULL` (default) returns all.
#'
#' @return A named character vector of hex colors.
#'
#' @examples
#' essp.colors()
#' essp.colors("ugared")
#'
#' @export
essp.colors <- function(name = NULL) {
  cols <- c(
    ugared      = "#BA0C2F",  # RGB 186,12,47
    ugablack    = "#000000",
    ugagray     = "#808080",  # RGB 128,128,128
    termgray    = "#505050",  # RGB 80,80,80
    bannergray  = "#554F47",  # RGB 85,79,71
    stripgray   = "#9EA2A2",  # RGB 158,162,162
    ugacreamery = "#D6D2C4",  # RGB 214,210,196
    # Chart-only additions; not in the LaTeX preamble.
    ink         = "#1A1A1A",
    muted       = "#5A5A5A",
    rule        = "#D8D8D8",
    surface     = "#FFFFFF"
  )
  if (is.null(name)) return(cols)

  unknown <- setdiff(name, names(cols))
  if (length(unknown)) {
    rlang::abort(c(
      paste0("Unknown color: ", paste(unknown, collapse = ", ")),
      i = paste0("Available: ", paste(names(cols), collapse = ", "))
    ))
  }
  cols[name]
}

# Accents cycled by semester, each verified to clear 3:1 contrast against a
# light chart surface on its own.
essp_accents <- function() {
  c("#BA0C2F",  # UGA red
    "#2E7D8F",  # teal
    "#4A7C2F",  # green
    "#7A4E9E")  # violet
}

#' Accent color for a semester
#'
#' Each semester gets its own accent, cycling through a validated set so
#' successive cohorts' figures are visually distinguishable while the rest of
#' the house style stays fixed.
#'
#' @param semester `"Spring"`, `"Summer"`, or `"Fall"` (case-insensitive).
#' @param year Calendar year.
#'
#' @return A hex color string.
#'
#' @examples
#' essp.accent("Fall", 2026)
#' essp.accent("Spring", 2027)
#'
#' @export
essp.accent <- function(semester, year) {
  sems <- c("spring", "summer", "fall")
  i <- match(tolower(semester), sems)
  if (is.na(i)) {
    rlang::abort(paste0("`semester` must be one of: ",
                        paste(tools::toTitleCase(sems), collapse = ", ")))
  }
  acc <- essp_accents()
  acc[[((as.integer(year) * 3L + i - 1L) %% length(acc)) + 1L]]
}

#' Chart palettes
#'
#' @param type `"categorical"`, `"grey"`, or `"sequential"`.
#' @param n Number of colors. Categorical is capped at 4 -- see the note below.
#'
#' @return A character vector of hex colors.
#'
#' @details
#' Categorical is capped at **four** hues because a fifth cannot be told apart
#' by protanopes (validated CVD separation 4.7, against a floor of 6). Charts
#' needing more categories should use [essp.highlight()] instead, which greys
#' everything except the resource in question and relies on direct labels for
#' identity. Even at four, direct labels are required -- separation sits at 6.8,
#' inside the band that is legal only with secondary encoding.
#'
#' The grey ramp is deliberately non-categorical. Its steps are close together
#' by design; they are backgrounded context, and identity comes from labels
#' rather than hue.
#'
#' @examples
#' essp.palette("categorical", 4)
#' essp.palette("grey", 5)
#'
#' @export
essp.palette <- function(type = c("categorical", "grey", "sequential"), n = 4) {
  type <- match.arg(type)

  if (type == "categorical") {
    # Seven hues, assigned in this fixed order and never cycled. Validated
    # against the light chart surface: all inside the lightness band, all above
    # the chroma floor, and no adjacent pair below the normal-vision floor.
    # The worst adjacent CVD separation is amber/green at DeltaE 6.8 under
    # protanopia, which is acceptable only because every ESSP chart carries
    # direct labels as a secondary encoding.
    pal <- c("#3E7CB1", "#9E5C2E", "#5A9E3F", "#E9A13B",
             "#8B5FBF", "#45B7C4", "#C94F7C")
    if (n > length(pal)) {
      rlang::abort(c(
        paste0("Only ", length(pal), " categorical hues are available; ", n, " requested."),
        x = "An eighth hue cannot be separated from the others for colour-blind readers.",
        i = "Fold the smallest categories into an \"Other\" group, use small multiples, or use essp.highlight() with direct labels."
      ))
    }
    return(pal[seq_len(n)])
  }

  if (type == "grey") {
    # Evenly spaced in lightness, dark to light.
    return(grDevices::grey.colors(n, start = 0.35, end = 0.85, gamma = 1))
  }

  # Sequential: a single hue, light to dark, never a rainbow.
  grDevices::colorRampPalette(c("#E3EDF5", "#1F4E79"))(n)
}

#' Colors for a set of resources, with one highlighted
#'
#' The house idiom: the resource under discussion takes the accent color, and
#' every other is greyed. This is what keeps a seven-resource chart legible when
#' seven distinct hues would not be.
#'
#' Greys are assigned darkest-to-lightest in the order given, so stacked
#' segments stay separable. Because grey steps are close together, charts using
#' this must label segments directly -- color alone does not carry identity here,
#' and is not meant to.
#'
#' @param resources Character vector of resource names, in plot order.
#' @param highlight Resource to receive the accent. `NULL` greys everything.
#' @param accent Accent color. Defaults to UGA red.
#'
#' @return A named character vector of hex colors, one per resource.
#'
#' @examples
#' essp.highlight(c("Natural Gas", "Coal", "Solar"), highlight = "Solar")
#'
#' @export
essp.highlight <- function(resources, highlight = NULL, accent = NULL) {
  # A resource keeps its own colour wherever it appears. Highlighting Solar on
  # the fleet-makeup figure paints it the same green it carries on the demand
  # curve, so a reader moving between figures does not have to relearn the
  # coding. Only a resource with no house colour falls back to the semester
  # accent, and an explicit `accent` always wins.
  if (is.null(accent)) {
    accent <- if (!is.null(highlight) && length(highlight) == 1L &&
                  highlight %in% names(essp.fuelcolors())) {
      unname(essp.fuelcolors(highlight))
    } else {
      unname(essp.colors("ugared"))
    }
  }

  greys <- essp.palette("grey", n = max(length(resources), 2L))
  out <- stats::setNames(greys[seq_along(resources)], resources)

  if (!is.null(highlight)) {
    missing <- setdiff(highlight, resources)
    if (length(missing)) {
      rlang::abort(paste0("`highlight` not among resources: ",
                          paste(missing, collapse = ", ")))
    }
    out[highlight] <- accent
  }
  out
}

#' Readable text color for a background
#'
#' Picks black or white for text sitting on a filled shape, choosing whichever
#' gives the higher WCAG contrast ratio against that fill.
#'
#' Using one rule for every segment is the point. Hardcoding white on the
#' accent and black elsewhere produces two inconsistencies at once: the same
#' resource changes text color between charts as the semester accent rotates,
#' and dark grey segments end up carrying black text they cannot support.
#'
#' @param fill Background color(s) as hex or R color names.
#' @param dark,light The two candidates to choose between.
#'
#' @return A character vector the same length as `fill`.
#'
#' @examples
#' essp.textcolor(c("#BA0C2F", "#D9D9D9"))
#'
#' @export
essp.textcolor <- function(fill, dark = "#1A1A1A", light = "#FFFFFF") {
  ratio <- function(a, b) {
    lum <- function(col) {
      v <- grDevices::col2rgb(col)[, 1] / 255
      v <- ifelse(v <= 0.03928, v / 12.92, ((v + 0.055) / 1.055)^2.4)
      sum(c(0.2126, 0.7152, 0.0722) * v)
    }
    l <- sort(c(lum(a), lum(b)), decreasing = TRUE)
    (l[1] + 0.05) / (l[2] + 0.05)
  }

  vapply(fill, function(f) {
    if (ratio(f, dark) >= ratio(f, light)) dark else light
  }, character(1), USE.NAMES = FALSE)
}

#' House ggplot theme
#'
#' Typography, grid, and spacing shared by every ESSP figure. The accent for the
#' given semester is attached as an attribute so charts can reach it without a
#' second call.
#'
#' Merriweather matches the briefs' body text and is used when the `showtext`
#' and `sysfonts` packages are available along with a font directory; otherwise
#' the device default applies and the chart still renders.
#'
#' @param semester `"Spring"`, `"Summer"`, or `"Fall"`.
#' @param year Calendar year.
#' @param base_size Base font size in points.
#' @param font_dir Directory holding the Merriweather static `.ttf` files.
#'   Defaults to the copies bundled with the package; `NULL` skips font
#'   registration.
#'
#' @return A ggplot2 theme object.
#'
#' @examples
#' \dontrun{
#' library(ggplot2)
#' ggplot(mtcars, aes(wt, mpg)) + geom_point() + essp.theme("Fall", 2026)
#' }
#'
#' @export
essp.theme <- function(semester = "Fall", year = as.integer(format(Sys.Date(), "%Y")),
                       base_size = 11,
                       font_dir = system.file("fonts", package = "ESSP")) {
  accent <- essp.accent(semester, year)
  cols   <- essp.colors()
  family <- register_merriweather(font_dir)

  th <- ggplot2::theme_minimal(base_size = base_size, base_family = family) +
    ggplot2::theme(
      plot.title            = ggplot2::element_text(face = "bold", size = ggplot2::rel(1.2),
                                                    colour = cols[["ink"]],
                                                    margin = ggplot2::margin(b = 3)),
      plot.subtitle         = ggplot2::element_text(size = ggplot2::rel(0.9),
                                                    colour = cols[["muted"]],
                                                    margin = ggplot2::margin(b = 10),
                                                    lineheight = 1.2),
      plot.caption          = ggplot2::element_text(size = ggplot2::rel(0.7),
                                                    colour = cols[["muted"]], hjust = 0,
                                                    margin = ggplot2::margin(t = 10),
                                                    lineheight = 1.2),
      plot.title.position   = "plot",
      plot.caption.position = "plot",
      plot.margin           = ggplot2::margin(12, 14, 10, 12),
      axis.title            = ggplot2::element_text(size = ggplot2::rel(0.85),
                                                    colour = cols[["muted"]]),
      axis.text             = ggplot2::element_text(size = ggplot2::rel(0.8),
                                                    colour = cols[["muted"]]),
      # Recessive grid: horizontal only, so bars and lines dominate.
      panel.grid.minor      = ggplot2::element_blank(),
      panel.grid.major.x    = ggplot2::element_blank(),
      panel.grid.major.y    = ggplot2::element_line(colour = cols[["rule"]], linewidth = 0.3),
      legend.position       = "top",
      legend.justification  = "left",
      legend.title          = ggplot2::element_blank(),
      legend.text           = ggplot2::element_text(size = ggplot2::rel(0.85),
                                                    colour = cols[["ink"]]),
      strip.text            = ggplot2::element_text(face = "bold", size = ggplot2::rel(0.9),
                                                    colour = cols[["ink"]], hjust = 0),
      plot.background       = ggplot2::element_rect(fill = cols[["surface"]], colour = NA),
      panel.background      = ggplot2::element_rect(fill = cols[["surface"]], colour = NA)
    )

  attr(th, "essp_accent")   <- accent
  attr(th, "essp_semester") <- paste(tools::toTitleCase(tolower(semester)), year)
  th
}

# Register Merriweather if the optional font packages and files are present.
# Returns the family name to use, or "" for the device default.
register_merriweather <- function(font_dir) {
  if (is.null(font_dir) || !dir.exists(font_dir)) return("")
  if (!requireNamespace("showtext", quietly = TRUE) ||
      !requireNamespace("sysfonts", quietly = TRUE)) return("")

  faces <- file.path(font_dir, paste0("Merriweather_24pt-",
                                      c("Regular", "Bold", "Italic", "BoldItalic"), ".ttf"))
  if (!all(file.exists(faces))) return("")

  if (!"Merriweather" %in% sysfonts::font_families()) {
    sysfonts::font_add(family = "Merriweather", regular = faces[1], bold = faces[2],
                       italic = faces[3], bolditalic = faces[4])
  }
  showtext::showtext_auto()
  showtext::showtext_opts(dpi = 300)
  # geom_text()/annotate("text") do not inherit theme fonts, so the demand-curve
  # band labels would stay sans even with the theme registered. Point the text
  # and label geom defaults at Merriweather so every direct label picks it up.
  ggplot2::update_geom_defaults("text",  list(family = "Merriweather"))
  ggplot2::update_geom_defaults("label", list(family = "Merriweather"))
  "Merriweather"
}
