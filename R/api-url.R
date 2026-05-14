# API URL helpers and version detection.
#
# The API version is always inferred from the URL — never passed as an
# explicit argument. Users set their base URL once via options(px.api_url).

#' Get the active PXWeb API URL
#'
#' Reads `getOption("px.api_url")`. Set it once in your `.Rprofile` or at the
#' top of a script:
#'
#' ```r
#' options(px.api_url = "https://bank.stat.gl/api/v1/en/Greenland/")
#' ```
#'
#' Used as the default for the `.api_url` argument in all pxfetch functions.
#' Pass `.api_url` explicitly to override for a single call.
#'
#' @return A length-1 character string.
#' @export
px_api_url <- function() {
  url <- getOption("px.api_url")
  if (is.null(url)) {
    rlang::abort(
      c(
        "No PXWeb API URL set.",
        i = "Set one in your .Rprofile or at the top of your script:",
        i = 'options(px.api_url = "https://your.stat.org/api/v1/en/")'
      ),
      class = "px_error_no_url"
    )
  }
  url
}

# Infer the integer API version (1 or 2) from a URL.
# Looks for /api/vN/ in the URL string.
#
# v1: POST with JSON body
# v2: GET with valueCodes[x]=y query parameters
#
# @param api_url Character. A PXWeb API base URL.
# @return Integer, e.g. 1L or 2L.
px_api_version <- function(api_url) {
  if (is.null(api_url) || !nzchar(api_url)) {
    rlang::abort(
      "api_url must be a non-empty string.",
      class = "px_error_bad_url",
      api_url = api_url
    )
  }
  # Match /v1/ or /v2/ anywhere in the URL — handles both standard PXWeb paths
  # (/api/v1/...) and non-standard ones like SSB's (/api/pxwebapi/v2/...).
  # Base URLs should include a trailing slash, e.g. ".../v2/".
  m <- regmatches(api_url, regexpr("/v(\\d+)/", api_url))
  if (length(m) == 0L) {
    rlang::abort(
      c(
        "Cannot determine API version from URL.",
        i = 'Expected a path segment like "/v1/" or "/v2/".',
        i = paste0("Got: ", api_url)
      ),
      class = "px_error_bad_url",
      api_url = api_url
    )
  }
  as.integer(sub("/v(\\d+)/", "\\1", m))
}
