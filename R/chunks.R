# Large-query chunking.
#
# chunk_large_query() — called by px_fetch() on HTTP 403 (query too large).
# Splits the query into smaller pieces and reassembles the results.
#
# Not yet implemented (targeted for 0.6.0). Currently raises an informative
# error so users know what happened and how to work around it.

chunk_large_query <- function(
    table_id,
    selections,
    .column_codes,
    .value_codes,
    .lang,
    .api_url
) {
  rlang::abort(
    c(
      "Query too large: the API returned HTTP 403.",
      i = "Narrow your selection to reduce the number of cells requested.",
      i = paste0(
        "Use `px_meta(\"", table_id, "\")` to see available variable values ",
        "and select a subset."
      )
    ),
    class      = "px_error_query_too_large",
    table_id   = table_id,
    selections = selections
  )
}
