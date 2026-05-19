# pxfetch (development version)

# pxfetch 0.2.0

* `px_fetch()`: `.column_codes` and `.value_codes` replaced by a single `.codes`
  argument. Pass a string (`"none"`, `"both"`, `"columns"`, `"values"`) for a
  global setting, or a named character vector (with optional `.default` slot) for
  per-variable control. Default is `"none"` — labels everywhere, matching the
  browser UI. Passing `"all"` gives a hint suggesting `"both"`.
* `px_fetch()`: `.expand_rest` renamed to `.fetch_all`.
* Tibbles returned by `px_fetch()` and `px_tag()` now have class `tbl_px`
  (was `px_ball`).
* `every()` alias removed. Use `px_all()` or bare `"*"`.
* `px_meta()` results are now cached in memory for the R session, keyed on
  table ID, API URL, and language. Use `px_meta_cache_clear()` to reset.
* `px_fetch()` gains a `.meta` argument: pass a pre-fetched `px_meta()`
  result to skip the internal metadata lookup entirely.
* `px_fetch()` now falls back to chunked retrieval when the API signals a
  query is too large: HTTP 403 on v1 APIs, HTTP 400 ("Too many cells
  selected") on v2 APIs. Chunks are fetched one variable at a time with a
  progress bar; chunk size is determined automatically.

# pxfetch 0.1.0

* Added `px_fetch()` to query and download data from PXWeb APIs (v1 and v2). Supports named variable selections using variable codes from `px_meta()`, language switching via `.lang`, selective code/label display via `.column_codes` and `.value_codes`, fetching all unspecified eliminable variables via `.expand_rest`, and a dry-run mode via `.dry_run` for inspecting queries before sending.
* Added `px_meta()` to retrieve variable and value metadata for a table as a flat tibble. The table title is stored as `attr(result, "px_title")`.
* Added `px_top()`, `px_all()`, and `px_agg()` as query DSL helpers for use inside `px_fetch()`, with short aliases `top()`, `every()`, and `agg()`. Plain `"*"` wildcard strings are automatically coerced to `px_all("*")`.
* Added `px_api_url()` to get and set the default API URL via `options(px.api_url)`.
* Added `px_url()` to construct the canonical table URL from a table ID and base API URL.
* Added `px_tag()` to attach PXWeb metadata attributes to any data frame, enabling `px_label()` (coming in 0.2.0) to work on data from any source.
* Tibbles returned by `px_fetch()` have class `px_ball` and print with the table title in the header and a `px_meta()` hint in the footer.
