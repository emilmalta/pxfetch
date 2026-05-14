# API URL helpers and version detection.
#
# The API version is always inferred from the URL — never passed as an
# explicit argument. Users set their base URL once via options(px.api_url).

.px_default_api_url <- NULL

#' Get or set the active PXWeb API URL
#'
#' Reads `getOption("px.api_url")`. Set it once in your `.Rprofile` or at the
#' top of a script:
#'
#' ```r
#' options(px.api_url = "https://bank.stat.gl/api/v1/en/Greenland/")
#' ```
#'
#' @return A length-1 character string.
#' @export
px_api_url <- function() {
  getOption("px.api_url", default = .px_default_api_url)
}

# Infer the integer API version (1 or 2) from a URL.
# Looks for /api/vN/ in the URL string.
#
# @param api_url Character. A PXWeb API base URL.
# @return Integer, e.g. 1L or 2L.
px_api_version <- function(api_url) {
  m <- regmatches(api_url, regexpr("/api/v(\\d+)/", api_url))
  if (length(m) == 0L) {
    rlang::abort(
      "Cannot determine API version from URL. Expected a path segment like /api/v1/ or /api/v2/.",
      class = "px_error_bad_url",
      api_url = api_url
    )
  }
  as.integer(sub("/api/v(\\d+)/", "\\1", m))
}
