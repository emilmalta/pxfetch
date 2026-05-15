# pxfetch 0.1.0

* Added `px_fetch()` to query and download data from PXWeb APIs (v1 and v2).
  Supports named variable selections, DSL helpers, language switching,
  selective code/label display, and a dry-run mode for debugging queries.
* Added `px_meta()` to retrieve variable and value metadata for a table as a
  flat tibble, with the table title stored as `attr(result, "px_title")`.
* Added selection helpers `px_top()`, `px_all()`, and `px_agg()` with short
  aliases `top()`, `every()`, and `agg()` for use inside `px_fetch()`.
* Added `px_api_url()` to get and set the default API URL via
  `options(px.api_url)`.
* Added `px_tag()` to attach PXWeb metadata attributes to any data frame,
  enabling `px_label()` (coming in 0.2.0) to work on data from any source.
