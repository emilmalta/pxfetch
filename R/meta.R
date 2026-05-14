# Table metadata.
#
# px_meta() — returns a flat tibble describing all variables and their values
#             for a given table. Columns:
#
#   variable | label | eliminable | is_time | value | value_label
#
# No returnclass argument. No intermediate list object.
# The raw HTTP fetch used internally by px_fetch() chunking is a separate
# unexported helper — not px_meta().
