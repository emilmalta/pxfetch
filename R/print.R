# Print methods for px_ball tibbles.
#
# px_ball is a subclass of tbl_df, so pillar/tibble handle most of the
# formatting. We only override:
#
#   tbl_sum.px_ball           -- appends "Title:" to the standard tibble header
#   tbl_format_footer.px_ball -- appends a px_meta() hint to the footer

#' @importFrom pillar tbl_sum tbl_format_footer style_subtle
NULL

#' @export
tbl_sum.px_ball <- function(x, ...) {
  parent <- NextMethod()
  title  <- attr(x, "px_title")

  if (!is.null(title) && !is.na(title) && nzchar(title)) {
    c(parent, "Title" = title)
  } else {
    parent
  }
}

#' @export
tbl_format_footer.px_ball <- function(x, setup, ...) {
  default  <- NextMethod()
  table_id <- attr(x, "px_table_id")

  if (!is.null(table_id) && nzchar(table_id)) {
    hint <- pillar::style_subtle(
      paste0("# i Use `px_meta(\"", table_id, "\")` to see all available values")
    )
    c(default, hint)
  } else {
    default
  }
}
