#' Retrieve variable metadata for a PXWeb table
#'
#' Returns a flat tibble with one row per variable–value combination,
#' describing everything you need to build a query. The table title is
#' stored as `attr(result, "px_title")`.
#'
#' @param table_id Table ID, e.g. `"BEXSTA"` or `"04861"`.
#' @param .lang Language code, e.g. `"en"`, `"da"`, `"no"`. For v1 APIs
#'   this rewrites the language segment of the URL; for v2 APIs it is
#'   passed as `?lang=`. If `NULL` (default), the language in `.api_url`
#'   is used as-is.
#' @param .api_url Base URL of the PXWeb API. Defaults to [px_api_url()].
#'
#' @return A tibble with columns `variable`, `label`, `eliminable`,
#'   `is_time`, `value`, `value_label`. The table title is available via
#'   `attr(result, "px_title")`.
#' @export
#'
#' @examples
#' \dontrun{
#' options(px.api_url = "https://bank.stat.gl/api/v1/en/Greenland/")
#' pxw_meta("BEXSTA")
#'
#' pxw_meta("04861", .lang = "en",
#'         .api_url = "https://data.ssb.no/api/pxwebapi/v2/")
#' }
pxw_meta <- function(table_id, .lang = NULL, .api_url = px_api_url()) {
  version <- px_api_version(.api_url)
  url     <- px_url(table_id, .api_url)
  query   <- list()

  if (version == 1L) {
    if (!is.null(.lang)) {
      # Rewrite /v1/da/ -> /v1/en/ (or whatever .lang is)
      url <- sub("(/v\\d+/)[^/]+/", paste0("\\1", .lang, "/"), url)
    }
  } else {
    url <- paste0(url, "/metadata")
    if (!is.null(.lang)) query <- list(lang = .lang)
  }

  resp <- px_get(url, query = query)

  if (version == 1L) parse_meta_v1(resp) else parse_meta_v2(resp)
}

# Internal parsers -------------------------------------------------------------

parse_meta_v1 <- function(resp) {
  raw <- httr2::resp_body_json(resp)

  rows <- lapply(raw$variables, function(v) {
    tibble::tibble(
      variable    = v$code,
      label       = v$text,
      eliminable  = isTRUE(v$elimination),
      is_time     = isTRUE(v$time),
      value       = as.character(unlist(v$values)),
      value_label = as.character(unlist(v$valueTexts))
    )
  })

  structure(dplyr::bind_rows(rows), px_title = raw$title)
}

parse_meta_v2 <- function(resp) {
  raw       <- httr2::resp_body_json(resp)
  time_vars <- raw$role$time %||% character(0L)

  rows <- lapply(raw$id, function(var_code) {
    dim    <- raw$dimension[[var_code]]
    idx    <- dim$category$index  # value_code -> integer position
    lbl    <- dim$category$label  # value_code -> value label

    # Preserve API order via index positions
    values <- names(idx)[order(unlist(idx))]
    labels <- unlist(lbl)[values]

    tibble::tibble(
      variable    = var_code,
      label       = dim$label,
      eliminable  = isTRUE(dim$extension$elimination),
      is_time     = var_code %in% time_vars,
      value       = values,
      value_label = as.character(labels)
    )
  })

  structure(dplyr::bind_rows(rows), px_title = raw$label)
}
