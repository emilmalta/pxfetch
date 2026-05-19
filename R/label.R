# Label joining and tibble tagging.
#
# px_tag()   — attaches px metadata as attributes and sets class "tbl_px".
#              Called internally by px_fetch(). Can also be called directly
#              to tag a tibble from any source (e.g. read from CSV) so that
#              px_label() can use it.
#
# px_label() — joins human-readable value labels onto a tagged tibble.
#              Calls px_meta() using the stored table_id and api_url.
#              Implemented in 0.2.0.

#' Tag a tibble with PXWeb metadata
#'
#' Attaches the table ID, table title, and API URL as attributes, and sets the
#' `"tbl_px"` class so that pxfetch's print methods apply. [px_fetch()] calls
#' this automatically; use `px_tag()` directly when working with data that was
#' not fetched via [px_fetch()] but still needs `px_label()` support.
#'
#' @param data A data frame or tibble.
#' @param table_id Table ID, e.g. `"BEXSTA"`.
#' @param title Table title string, typically from the API response. If `NULL`
#'   (default) the `px_title` attribute will be `NA`.
#' @param .api_url Base URL of the API the data came from. Defaults to
#'   [px_api_url()].
#'
#' @return `data` with class `c("tbl_px", <original classes>)` and attributes
#'   `px_class`, `px_table_id`, `px_title`, and `px_api_url`.
#' @export
#'
#' @examples
#' \dontrun{
#' options(px.api_url = "https://bank.stat.gl/api/v1/en/Greenland/")
#'
#' df <- read.csv("bexsta.csv")
#' df <- px_tag(df, "BEXSTA")
#' }
px_tag <- function(data, table_id, title = NULL, .api_url = px_api_url()) {
  if (!is.data.frame(data)) {
    rlang::abort(
      "`data` must be a data frame or tibble.",
      class = "px_error_bad_data"
    )
  }

  structure(
    data,
    px_class    = "tbl_px",
    px_table_id = table_id,
    px_title    = title %||% NA_character_,
    px_api_url  = .api_url,
    class       = c("tbl_px", class(data))
  )
}
