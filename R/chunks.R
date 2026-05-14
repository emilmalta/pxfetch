# Large-query chunking.
#
# chunk_large_query() — splits an oversized query into smaller pieces and
#                       reassembles the results.
#
# Used internally by px_fetch() when the API returns 403 (query too large).
#
# Known bugs from statgl to fix during implementation:
#   - .column_labels/.value_labels argument order was swapped between
#     px_fetch -> chunk_large_query -> try_chunk — verify both pass correctly
#   - Use cli::cli_inform() in the chunking loop, never message()
