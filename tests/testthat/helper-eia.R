# Tests that hit the live EIA API skip when no key is configured, so the suite
# still runs on a machine without credentials.
skip_if_no_key <- function() {
  key <- tryCatch(suppressWarnings(eia::eia_get_key()), error = function(e) NULL)
  testthat::skip_if(is.null(key) || !nzchar(key), "No EIA API key configured")
}
