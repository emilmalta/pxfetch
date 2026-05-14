#' Build the PXWeb API URL for a table
#'
#' Constructs the canonical URL for a table from a base API URL and a table ID.
#' No network call is made.
#'
#' The URL structure depends on API version:
#'
#' - **v1**: `{.api_url}/{table_id}` — e.g.
#'   `https://bank.stat.gl/api/v1/en/Greenland/BEXSTA`
#' - **v2**: `{.api_url}/tables/{table_id}` — e.g.
#'   `https://data.ssb.no/api/pxwebapi/v2/tables/05279`
#'
#' The base URL should include the version segment and end with a trailing
#' slash, e.g. `"https://bank.stat.gl/api/v1/en/Greenland/"`.
#'
#' Some v1 APIs (e.g. Statistics Finland) require a `.px` suffix on the table
#' ID. If your API does, include it in `table_id`.
#'
#' @param table_id Table ID, e.g. `"BEXSTA"` or `"05279"`. Case-sensitive.
#' @param .api_url Base URL of the PXWeb API. Defaults to [px_api_url()].
#'
#' @return A length-1 character string.
#' @export
#'
#' @examples
#' \dontrun{
#' options(px.api_url = "https://bank.stat.gl/api/v1/en/Greenland/")
#' px_url("BEXSTA")
#' #> [1] "https://bank.stat.gl/api/v1/en/Greenland/BEXSTA"
#'
#' px_url("05279", .api_url = "https://data.ssb.no/api/pxwebapi/v2/")
#' #> [1] "https://data.ssb.no/api/pxwebapi/v2/tables/05279"
#' }
px_url <- function(table_id, .api_url = px_api_url()) {
  if (!is.character(table_id) || length(table_id) != 1L || !nzchar(table_id)) {
    rlang::abort(
      "table_id must be a non-empty character string.",
      class = "px_error_bad_table_id",
      table_id = table_id
    )
  }

  base    <- sub("/$", "", .api_url)        # normalise: strip trailing slash
  base    <- sub("/tables$", "", base)      # v2: strip /tables if user included it
  version <- px_api_version(.api_url)

  switch(version,
    `1` = paste0(base, "/", table_id),
    `2` = paste0(base, "/tables/", table_id),
    rlang::abort(
      c(
        paste0("Unsupported API version: ", version, "."),
        i = "pxfetch supports PXWeb API v1 and v2."
      ),
      class  = "px_error_unsupported_version",
      version = version,
      api_url = .api_url
    )
  )
}
