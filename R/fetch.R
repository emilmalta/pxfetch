# Main data fetching function.
#
# px_fetch() — fetches a table from a PXWeb API and returns a tibble.
#
# Signature:
#   px_fetch(
#     table_id,
#     ...,
#     .column_labels = TRUE,   # TRUE = variable names as labels (default, matches web UI)
#     .value_labels  = TRUE,   # TRUE = cell values as labels (default, matches web UI)
#     .expand_rest   = FALSE,  # TRUE = fetch all values for unspecified variables
#     .dry_run       = FALSE,  # TRUE = return resolved URL + query body without sending
#     .api_url       = px_api_url()
#   )
##
# Key behaviours:
#   - ... DSL: named arguments select variable values inline
#   - px_all(), px_top(n), px_agg() as DSL helpers
#   - .expand_rest = TRUE fetches all values for variables not mentioned in ...
#   - .dry_run = TRUE returns resolved URL + query body without sending
#   - Automatic 403 -> chunked retry via chunk_large_query()
#   - On-error 400 diagnosis using px_meta()
#   - v1/v2 dispatch via px_api_version()
#
# Known bugs from statgl to fix during implementation:
#   - Missing Content-Type: application/json header on POST (v1)
#   - fetch_jsonstat() parses response body twice — parse once
#   - message() calls should be cli::cli_inform()
