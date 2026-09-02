# Set the EIA API key ----------------------------------------------------------

#' Set or check your EIA API key
#'
#' The gathering commands need a (free) EIA API key from
#' <https://www.eia.gov/opendata/>. `essp.key("your-key")` sets it for the
#' current session and, by default, saves it to `~/.Renviron` so you only ever
#' do this once. `essp.key()` with no argument reports whether a key is set,
#' without ever printing it.
#'
#' @param key Your EIA API key (a ~40-character letters-and-digits string).
#'   Omit to check the current status instead of setting one.
#' @param persist Also write the key to `~/.Renviron` so later R sessions pick
#'   it up automatically. `TRUE` by default; set `FALSE` to keep it to this
#'   session only.
#'
#' @return Invisibly `TRUE` if a key is now set, `FALSE` otherwise. Called for
#'   its side effect.
#'
#' @examples
#' \dontrun{
#' essp.key("abcdef0123456789abcdef0123456789abcdef01")
#' essp.key()   # is a key set?
#' }
#'
#' @export
essp.key <- function(key = NULL, persist = TRUE) {
  # No argument: report status without ever revealing the key itself.
  if (is.null(key)) {
    cur <- Sys.getenv("EIA_KEY")
    if (nzchar(cur)) {
      message("An EIA key is set (", nchar(cur), " characters).")
    } else {
      message("No EIA key set. Call essp.key(\"your-key\") to add one.")
    }
    return(invisible(nzchar(cur)))
  }

  key <- trimws(key)
  if (!nzchar(key)) {
    rlang::abort("Give your EIA API key, e.g. essp.key(\"abcdef...\").")
  }
  # Catch an obvious mistake -- a pasted URL, a placeholder, whitespace -- without
  # being fussy about the exact format in case EIA changes it. A real key is a
  # ~40-character alphanumeric string.
  if (grepl("[^A-Za-z0-9]", key) || nchar(key) < 20) {
    rlang::abort(c(
      "That does not look like an EIA API key.",
      i = "It should be a ~40-character letters-and-digits string.",
      i = "Get one free at https://www.eia.gov/opendata/."))
  }

  # Set for this session so the very next essp.gather()/essp.get() works.
  Sys.setenv(EIA_KEY = key)

  if (isTRUE(persist)) {
    path  <- file.path(Sys.getenv("HOME"), ".Renviron")
    lines <- if (file.exists(path)) readLines(path, warn = FALSE) else character(0)
    lines <- lines[!grepl("^\\s*EIA_KEY\\s*=", lines)]   # replace, never duplicate
    lines <- c(lines, paste0("EIA_KEY=", key))
    writeLines(lines, path)
    message("EIA key set for this session and saved to ", path,
            " for future ones.")
  } else {
    message("EIA key set for this session only.")
  }
  invisible(TRUE)
}
