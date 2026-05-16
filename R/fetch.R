#' Fetch data from a PXWeb table
#'
#' Sends a query to a PXWeb API and returns the result as a tibble. Variable
#' selections are passed as named arguments in `...`, using the variable codes
#' reported by [px_meta()]. DSL helpers [px_top()], [px_all()], and [px_agg()]
#' can be used as values.
#'
#' @param table_id Table ID, e.g. `"BEXSTA"` or `"04861"`.
#' @param ... Named selections: `variable_code = values`. Values can be a
#'   character vector of codes, or a DSL helper such as [px_top()],
#'   [px_all()], or [px_agg()]. Unspecified eliminable variables use the
#'   API default (typically a single value) unless `.expand_rest = TRUE`.
#' @param .column_codes Controls whether dimension column names are shown as
#'   variable codes or labels. `FALSE` (default) uses labels for all columns;
#'   `TRUE` keeps codes for all; a character vector of variable codes keeps
#'   codes only for those columns.
#' @param .value_codes Controls whether cell values are shown as codes or
#'   labels. `FALSE` (default) uses labels for all columns; `TRUE` keeps codes
#'   for all; a character vector of variable codes keeps codes only for those
#'   columns.
#' @param .expand_rest If `TRUE`, all unspecified variables are requested with
#'   `px_all("*")` even when `...` is empty (i.e. fetch everything). When any
#'   selection is provided in `...`, unspecified variables are always expanded
#'   automatically regardless of this argument.
#' @param .dry_run If `TRUE`, returns the resolved URL and query without
#'   sending a request. Useful for debugging. Default `FALSE`.
#' @param .lang Language code, e.g. `"en"`, `"da"`. For v1 APIs this rewrites
#'   the language segment of the URL; for v2 it is passed as `?lang=`. If
#'   `NULL` (default), the language in `.api_url` is used as-is.
#' @param .api_url Base URL of the PXWeb API. Defaults to [px_api_url()].
#'
#' @return A tibble with one column per dimension and a `value` column
#'   containing the observations. The table title is available via
#'   `attr(result, "px_title")`, and the table ID via
#'   `attr(result, "px_table_id")`. See also [px_meta()] and [px_tag()].
#' @export
#'
#' @examples
#' \dontrun{
#' options(px.api_url = "https://bank.stat.gl/api/v1/en/Greenland/")
#'
#' # Fetch all time values for capital city
#' px_fetch("BEXSTA", `residence type` = "A", time = px_all())
#'
#' # Most recent 5 periods, all residence types
#' px_fetch("BEXSTA", `residence type` = px_all(), time = px_top(5))
#'
#' #' # Keep raw codes instead of labels
#' px_fetch("BEXSTA", residence type` = "A", .column_codes = TRUE, .value_codes = TRUE)
#'
#' # Inspect the query without sending it
#' px_fetch("BEXSTA", residence type` = "A", .dry_run = TRUE)
#' }
px_fetch <- function(
    table_id,
    ...,
    .column_codes = FALSE,
    .value_codes  = FALSE,
    .expand_rest  = FALSE,
    .dry_run       = FALSE,
    .lang          = NULL,
    .api_url       = px_api_url()
) {
  selections <- list(...)

  if (...length() > 0L) {
    nms <- names(selections)
    if (is.null(nms) || any(!nzchar(nms))) {
      rlang::abort(
        c(
          "All arguments in `...` must be named.",
          i = "Use the variable code as the name, e.g. `Region = c(\"0301\", \"0322\")`.",
          i = "See `px_meta()` for available variable codes."
        ),
        class = "px_error_unnamed_selection"
      )
    }
  }

  version <- px_api_version(.api_url)
  url     <- px_url(table_id, .api_url)

  # Language rewrite: v1 encodes it in the URL path, v2 as a query parameter
  if (version == 1L && !is.null(.lang)) {
    url <- sub("(/v\\d+/)[^/]+/", paste0("\\1", .lang, "/"), url)
  }

  # When any selection is provided, mandatory (non-eliminable) variables must be
  # explicitly included in the query — v2 APIs reject partial queries otherwise.
  # Eliminable variables are left unspecified so the API applies its own default
  # (the eliminationValue, typically the aggregate/total category).
  # .expand_rest = TRUE overrides this and expands ALL unspecified variables.
  if (!.dry_run && (length(selections) > 0L || .expand_rest)) {
    meta <- px_meta(table_id, .lang = .lang, .api_url = .api_url)
    target_vars <- if (.expand_rest) {
      unique(meta$variable)
    } else {
      unique(meta$variable[!meta$eliminable])
    }
    unspec <- setdiff(target_vars, names(selections))
    for (v in unspec) selections[[v]] <- px_all("*")
  }

  # Build version-specific query representation
  if (version == 1L) {
    body <- build_query_v1(selections)
    qs   <- NULL
  } else {
    body <- NULL
    qs   <- build_query_v2(selections)
  }

  if (.dry_run) {
    out <- list(url = url, version = version, selections = selections)
    if (version == 1L) out$body <- body else out$query <- qs
    return(out)
  }

  # Send request, falling back to chunked retrieval on 403
  if (version == 1L) {
    resp <- tryCatch(
      px_post_json(url, body),
      px_error_http_403 = function(e) {
        chunk_large_query(
          table_id, selections, .column_codes, .value_codes, .lang, .api_url
        )
      }
    )
  } else {
    data_url <- build_v2_data_url(url, qs, .lang)
    resp     <- tryCatch(
      px_get(data_url),
      px_error_http_403 = function(e) {
        chunk_large_query(
          table_id, selections, .column_codes, .value_codes, .lang, .api_url
        )
      }
    )
  }

  # chunk_large_query returns a data frame directly; tag and return early
  if (inherits(resp, "data.frame")) {
    return(px_tag(resp, table_id = table_id, .api_url = .api_url))
  }

  parsed <- parse_jsonstat(resp, .column_codes = .column_codes, .value_codes = .value_codes)

  px_tag(parsed$data, table_id = table_id, title = parsed$title, .api_url = .api_url)
}

# Build the full v2 data URL with query string appended manually.
# Brackets in valueCodes[x] keys must not be URL-encoded, so we build by hand
# rather than using req_url_query().
build_v2_data_url <- function(table_url, qs, .lang = NULL) {
  data_url <- paste0(table_url, "/data")

  parts <- character(0)
  if (!is.null(.lang)) parts <- c(parts, paste0("lang=", .lang))
  parts <- c(parts, "outputFormat=json-stat2")
  if (!is.null(qs) && nzchar(qs)) parts <- c(parts, qs)

  paste0(data_url, "?", paste(parts, collapse = "&"))
}

# Resolve a .column_codes / .value_codes argument to the set of variable codes
# that should retain their raw codes rather than be labelled.
#   FALSE           -> label everything (empty set to skip)
#   TRUE            -> keep codes for all variables
#   character vector -> keep codes only for the named variable codes
resolve_code_selection <- function(sel, all_ids) {
  if (isFALSE(sel))       character(0)
  else if (isTRUE(sel))   all_ids
  else                    as.character(sel)
}

# Parse a json-stat or json-stat2 HTTP response into a flat tibble.
#
# json-stat (v1): data wrapped in $dataset; id and size live inside $dimension.
# json-stat2 (v2): no wrapper; id and size are top-level fields.
#
# Both encode values as a flat array in row-major order (first dimension varies
# slowest). We reconstruct the full cartesian grid via expand.grid().
parse_jsonstat <- function(resp, .column_codes = FALSE, .value_codes = FALSE) {
  raw <- httr2::resp_body_json(resp, simplifyVector = FALSE)

  # Normalise to a common shape regardless of format version
  if (!is.null(raw$dataset)) {
    # json-stat: unpack the dataset wrapper
    ds     <- raw$dataset
    ids    <- as.character(unlist(ds$dimension$id))
    dim_fn <- function(var_id) ds$dimension[[var_id]]
    title  <- ds$label %||% ""
    vals   <- ds$value
  } else {
    # json-stat2: fields are at the top level
    ids    <- as.character(unlist(raw$id))
    dim_fn <- function(var_id) raw$dimension[[var_id]]
    title  <- raw$label %||% ""
    vals   <- raw$value
  }

  # Ordered value codes for each dimension (API returns 0-indexed positions)
  codes_list <- lapply(ids, function(var_id) {
    cat_idx <- dim_fn(var_id)$category$index
    nms     <- names(cat_idx)
    pos     <- as.integer(unlist(cat_idx))
    nms[order(pos)]
  })
  names(codes_list) <- ids

  # Full cartesian grid in JSON-stat2 row-major order: first dimension varies
  # slowest, last varies fastest. expand.grid() is the opposite (first varies
  # fastest), so we reverse the dimension list, expand, then reverse columns.
  rev_codes <- rev(codes_list)
  grid_rev  <- do.call(expand.grid, c(rev_codes, list(stringsAsFactors = FALSE)))
  grid      <- grid_rev[, rev(seq_along(rev_codes)), drop = FALSE]
  names(grid) <- ids

  # Observations: NULL entries in the value array represent missing data
  grid$value <- vapply(
    vals,
    function(v) if (is.null(v)) NA_real_ else as.double(v),
    double(1L)
  )

  result <- tibble::as_tibble(grid)

  # Resolve which variable codes should keep their codes (vs. get labels).
  # FALSE = label everything, TRUE = code everything, character = selective.
  keep_val_codes <- resolve_code_selection(.value_codes,  ids)
  keep_col_codes <- resolve_code_selection(.column_codes, ids)

  # Replace value codes with human-readable labels for non-selected columns
  for (var_id in ids) {
    if (var_id %in% keep_val_codes) next
    lbl     <- dim_fn(var_id)$category$label
    lbl_vec <- stats::setNames(as.character(unlist(lbl)), names(lbl))
    result[[var_id]] <- lbl_vec[result[[var_id]]]
  }

  # Rename dimension columns from variable code to variable label
  for (var_id in ids) {
    if (var_id %in% keep_col_codes) next
    var_label <- dim_fn(var_id)$label %||% var_id
    if (nzchar(var_label) && var_label != var_id) {
      names(result)[names(result) == var_id] <- var_label
    }
  }

  list(data = result, title = title)
}
