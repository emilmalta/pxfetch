# Table search.
#
# px_search() — searches the API for tables matching a keyword.
#
# Known issues carried over from statgl_search (to fix during implementation):
#   - Remove any lingering browser() calls before first commit
#   - Remove commented-out dead code
#   - Use && not & in compound if() conditions
#   - Call httr2::resp_check_status() before parsing response body
#   - Zero results should return empty tibble, not abort()
#   - Validate returnclass at top, before any network call
#   - Use cli::cli_inform(), never message()
#   - Detect Greenland API via is_greenland_api() helper, not inline regex
