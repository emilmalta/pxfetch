# Label joining.
#
# px_label() — joins human-readable value labels onto a px_fetch() result.
#
# Signature:
#   px_label(data, meta, ..., .keep = c("label", "both"), suffix = "_label")
#
# Rules:
#   - ... selects columns via tidyselect::eval_select(rlang::expr(c(...)), data[labelable])
#   - Use ...length() == 0L for empty ... check, not length(list(...))
#   - .keep = "label"  replaces code column with label column
#   - .keep = "both"   keeps code column, adds label column alongside it
#   - When variable == label in meta (API echoes code as label):
#       use suffix to disambiguate, e.g. age -> age + age_label
#   - When variable != label in meta:
#       label column takes the label name, no suffix needed
#       e.g. citydistrict -> citydistrict + bydel (no "_label")
#   - Coerce code column to character before joining —
#       px_fetch() may return numeric codes (e.g. age as <dbl>)
#       but px_meta() always returns character values
#   - In "both" mode: relocate label column immediately after code column
#   - meta argument accepts a table ID or URL; px_meta() is called automatically
