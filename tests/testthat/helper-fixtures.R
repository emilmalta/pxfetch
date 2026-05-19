# Shared test helpers — sourced automatically by testthat before all test files.

fake_json_resp <- function(x) {
  structure(
    list(
      status_code = 200L,
      headers     = list("content-type" = "application/json; charset=utf-8"),
      body        = charToRaw(jsonlite::toJSON(x, auto_unbox = TRUE, null = "null")),
      cache       = new.env(parent = emptyenv())
    ),
    class = "httr2_response"
  )
}
