# Query builders and DSL helpers -----------------------------------------------

# DSL helpers ------------------------------------------------------------------

#' Selection helpers for PXWeb queries
#'
#' Use these inside `px_fetch()` to control how values are selected for each
#' variable. Plain character vectors select specific values by code.
#'
#' @param n Number of values to request. For time variables this is typically
#'   the most recent `n` periods.
#' @param pattern Wildcard pattern, e.g. `"*"` (all), `"*0"` (ending in 0),
#'   `"???"` (three characters). Defaults to `"*"`.
#' @param agg_file Aggregation file name as defined in the API.
#' @param ... Values to select from the aggregation.
#'
#' @return A modified vector with a `.px_filter` attribute consumed by the
#'   query builders. Not intended for direct use.
#'
#' @export
#' @name px_helpers
px_top <- function(n = 1L) {
  structure(as.integer(n), .px_filter = "Top")
}

#' @export
#' @rdname px_helpers
px_all <- function(pattern = "*") {
  structure(as.character(pattern), .px_filter = "all")
}

#' @export
#' @rdname px_helpers
px_agg <- function(agg_file, ...) {
  structure(c(...), .px_filter = paste0("agg:", agg_file))
}

#' @export
#' @rdname px_helpers
top <- function(n = 1L) px_top(n)

#' @export
#' @rdname px_helpers
every <- function(pattern = "*") px_all(pattern)

#' @export
#' @rdname px_helpers
agg <- function(agg_file, ...) px_agg(agg_file, ...)

# Query builders ---------------------------------------------------------------

# Build the JSON body for a v1 POST request.
# `selections` is a named list from `...` in px_fetch().
# Returns a list ready for httr2::req_body_json().
build_query_v1 <- function(selections) {
  nms <- names(selections)

  query_items <- lapply(seq_along(selections), function(i) {
    v      <- selections[[i]]
    filter <- attr(v, ".px_filter") %||% "item"

    list(
      code      = jsonlite::unbox(nms[[i]]),
      selection = list(
        filter = jsonlite::unbox(filter),
        values = as.character(v)
      )
    )
  })

  list(
    query    = query_items,
    response = list(format = jsonlite::unbox("json-stat2"))
  )
}

# Build the query string for a v2 GET request.
# `selections` is a named list from `...` in px_fetch().
# Returns a length-1 character string (the raw query string, without "?"),
# ready to be appended to the URL. We build it manually to avoid bracket
# encoding issues with req_url_query().
build_query_v2 <- function(selections) {
  if (length(selections) == 0L) return("")

  parts <- mapply(
    FUN = function(nm, v) {
      filter <- attr(v, ".px_filter")
      value_str <- if (is.null(filter)) {
        # Plain selection: comma-separated codes
        paste(as.character(v), collapse = ",")
      } else if (filter == "all") {
        as.character(v)           # "*", "324*", "???", etc.
      } else if (filter == "Top") {
        paste0("top(", as.integer(v), ")")
      } else if (startsWith(filter, "agg:")) {
        agg_file <- sub("^agg:", "", filter)
        paste0("agg(", agg_file, ",", paste(as.character(v), collapse = ","), ")")
      } else {
        paste(as.character(v), collapse = ",")
      }
      paste0("valueCodes[", nm, "]=", value_str)
    },
    nm = names(selections),
    v  = selections,
    SIMPLIFY = TRUE,
    USE.NAMES = FALSE
  )

  paste(parts, collapse = "&")
}
