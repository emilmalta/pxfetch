# Miscellaneous internal helpers.
# Nothing in this file should be exported.

# Null-coalescing operator: x %||% y returns x if not NULL, otherwise y.
`%||%` <- function(x, y) if (!is.null(x)) x else y

# Returns TRUE if x looks like a plausible HTTP(S) URL.
is_valid_url <- function(x) {
  grepl("^https?://", x, ignore.case = TRUE)
}
