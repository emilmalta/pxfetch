# Internal httr2 helpers.
# All HTTP calls go through here — never call httr2 directly from other files.

# Build a base httr2 request with standard headers. We disable httr2's
# built-in error handling (is_error = FALSE) so we can inspect the status
# ourselves and throw classed conditions.
px_req <- function(url) {
  httr2::request(url) |>
    httr2::req_user_agent(
      "pxfetch (https://github.com/emilmalta/pxfetch)"
    ) |>
    httr2::req_timeout(30) |>
    httr2::req_error(is_error = \(resp) FALSE)
}

# Perform a GET request. `query` is a named list of query parameters;
# it is spliced into the URL via req_url_query() when non-empty.
px_get <- function(url, query = list()) {
  req <- px_req(url)
  if (length(query) > 0L) {
    req <- do.call(httr2::req_url_query, c(list(req), query))
  }
  resp <- httr2::req_perform(req)
  px_stop_for_status(resp, url)
  resp
}

# Perform a POST request with a JSON body. `body` should be a list;
# httr2 will serialise it via jsonlite.
px_post_json <- function(url, body) {
  req <- px_req(url) |>
    httr2::req_body_json(body, auto_unbox = FALSE)
  resp <- httr2::req_perform(req)
  px_stop_for_status(resp, url)
  resp
}

# Throw a classed error for any HTTP 4xx/5xx response. The primary class
# is px_error_http_<status> (e.g. px_error_http_403) so callers can catch
# specific status codes, with px_error_http as the parent class.
px_stop_for_status <- function(resp, url) {
  status <- httr2::resp_status(resp)
  if (status < 400L) return(invisible(resp))

  body <- tryCatch(
    httr2::resp_body_string(resp),
    error = function(e) "<no body>"
  )

  rlang::abort(
    c(
      paste0("HTTP ", status, " from <", url, ">."),
      i = paste0("Response body: ", body)
    ),
    class   = c(paste0("px_error_http_", status), "px_error_http"),
    status  = status,
    url     = url,
    body    = body
  )
}
