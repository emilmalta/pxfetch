# Miscellaneous internal helpers.
# Nothing in this file should be exported.

# Null-coalescing operator: x %||% y returns x if not NULL, otherwise y.
`%||%` <- function(x, y) if (!is.null(x)) x else y

# Returns TRUE if x looks like a plausible HTTP(S) URL.
is_valid_url <- function(x) {
  grepl("^https?://", x, ignore.case = TRUE)
}

# .codes helpers ---------------------------------------------------------------

.px_codes_valid <- c("none", "both", "columns", "values")

# Validate the .codes argument (type and values only — no var_ids needed).
# Called early in px_fetch() so the user gets an error before any HTTP call.
validate_codes_arg <- function(.codes, call = rlang::caller_env()) {
  if (is.character(.codes) && length(.codes) == 1L && is.null(names(.codes))) {
    .check_single_code(.codes, call = call)
    return(invisible(NULL))
  }
  if (!is.character(.codes) || is.null(names(.codes))) {
    rlang::abort(
      c(
        "`.codes` must be a string or a named character vector.",
        i = 'Use `.codes = "none"` for a global setting.',
        i = 'Use `.codes = c(Region = "both")` for per-variable control.'
      ),
      class = "px_error_bad_codes",
      call  = call
    )
  }
  for (v in .codes) .check_single_code(v, call = call)
  invisible(NULL)
}

.check_single_code <- function(x, call = rlang::caller_env()) {
  if (x %in% .px_codes_valid) return(invisible(NULL))
  bullets <- character(0)
  if (identical(x, "all")) bullets <- c(bullets, i = 'Did you mean "both"?')
  bullets <- c(
    bullets,
    i = paste0('Must be one of ', paste0('"', .px_codes_valid, '"', collapse = ", "), ".")
  )
  rlang::abort(
    c(paste0('`.codes` value "', x, '" is not valid.'), bullets),
    class = "px_error_bad_codes",
    call  = call
  )
}

# Resolve the .codes argument to a named character vector: var_id -> setting.
# Called inside parse_jsonstat() once var_ids are known from the response.
#
# .codes can be:
#   - a single string ("none", "both", "columns", "values") -> applied to all vars
#   - a named character vector, optionally with a ".default" slot for the fallback
#
# Variables not named in the vector use ".default" if present, otherwise "none".
resolve_codes_per_var <- function(.codes, var_ids) {
  if (is.character(.codes) && length(.codes) == 1L && is.null(names(.codes))) {
    return(stats::setNames(rep(.codes, length(var_ids)), var_ids))
  }
  default  <- if (".default" %in% names(.codes)) .codes[[".default"]] else "none"
  override <- .codes[names(.codes) != ".default"]
  result   <- stats::setNames(rep(default, length(var_ids)), var_ids)
  common   <- intersect(names(override), var_ids)
  result[common] <- override[common]
  result
}
