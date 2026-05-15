# Label joining and tibble tagging.
#
# px_tag() — attaches px_table_id and px_api_url as attributes to any tibble.
#            Lazy: no API call. Works on data from any source (px_fetch,
#            read_csv, etc). px_fetch() calls this internally.
#
#   px_tag(data, table_id, .api_url = px_api_url())
#
# px_label() — joins human-readable value labels onto a tibble. Requires
#              px_table_id attribute (set by px_tag / px_fetch). Calls
#              px_meta() internally with the stored table_id and api_url.
#
# Signature:
#   px_label(
#     data,
#     ...,             # tidyselect: which columns to label; default = all labelable
#     .lang   = NULL,  # language passed to px_meta(); NULL = API default
#     .codes  = c("drop", "keep"),  # "drop" replaces code col; "keep" adds label col alongside
#     suffix  = "_label"            # suffix for added label cols when .codes = "keep"
#   )
#
# Dot-prefixed parameters avoid collision with tidyselect column names in ...
#
# Rules:
#   - ... uses tidyselect::eval_select(rlang::expr(c(...)), data[labelable])
#   - Use ...length() == 0L for empty ... check, not length(list(...))
#   - .codes = "drop"  replaces code column with label column (default)
#   - .codes = "keep"  retains code column and inserts label column after it
#   - When variable == label in meta (API echoes code as label):
#       suffix always applied, e.g. age -> age + age_label
#   - When variable != label in meta:
#       label column takes the label name, suffix only added on collision
#       e.g. citydistrict -> citydistrict + bydel (no suffix needed)
#   - Coerce code column to character before joining —
#       px_fetch() may return numeric codes (e.g. age as <dbl>)
#       but px_meta() always returns character values
#   - In "keep" mode: relocate label column immediately after code column
