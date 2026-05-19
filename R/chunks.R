# Large-query chunking.
#
# chunk_large_query() is called by px_fetch() on HTTP 403 (query too large).
# It picks the dimension with the most values to split on, probes with a
# small first chunk to find a safe size, then fetches all chunks and binds
# the results. The user never calls any of this directly.

chunk_large_query <- function(
    table_id,
    selections,
    .codes,
    .lang,
    .api_url
) {
  # px_meta() is a cache hit here: px_fetch() already called it when
  # auto-expanding non-eliminable variables before the failed request.
  meta <- px_meta(table_id, .lang = .lang, .api_url = .api_url)

  chunk_var  <- pick_chunk_variable(selections, meta)
  all_values <- chunk_variable_values(chunk_var, selections, meta)
  chunk_size <- max(1L, min(10L, length(all_values) %/% 4L))

  version <- px_api_version(.api_url)
  url     <- px_url(table_id, .api_url)
  if (version == 1L && !is.null(.lang)) {
    url <- sub("(/v\\d+/)[^/]+/", paste0("\\1", .lang, "/"), url)
  }

  while (chunk_size >= 1L) {
    chunks <- split(all_values, ceiling(seq_along(all_values) / chunk_size))

    # Probe with the first chunk to find a safe chunk size.
    first <- try_chunk(url, selections, chunk_var, chunks[[1L]], .codes, .lang, version)

    if (is.null(first)) {
      chunk_size <- chunk_size %/% 2L
      next
    }

    # Probe succeeded — fetch the rest with a progress bar.
    results      <- vector("list", length(chunks))
    results[[1L]] <- first

    if (length(chunks) > 1L) {
      pb <- cli::cli_progress_bar(
        total  = length(chunks),
        format = "{cli::pb_bar} chunk {cli::pb_current}/{cli::pb_total} [{cli::pb_elapsed}]"
      )
      cli::cli_progress_update(id = pb)  # chunk 1 already done

      for (i in seq_along(chunks)[-1L]) {
        r <- try_chunk(url, selections, chunk_var, chunks[[i]], .codes, .lang, version)
        if (is.null(r)) {
          cli::cli_progress_done(id = pb, result = "failed")
          rlang::abort(
            paste0("Chunk ", i, " failed at size ", chunk_size, " (earlier chunks succeeded)."),
            class = "px_error_query_too_large"
          )
        }
        results[[i]] <- r
        cli::cli_progress_update(id = pb)
      }

      cli::cli_progress_done(id = pb)
    }

    result <- do.call(rbind, results)
    # Stash the title so px_fetch() can pass it to px_tag().
    attr(result, "px_title") <- attr(meta, "px_title")
    return(result)
  }

  rlang::abort(
    c(
      "Query too large: unable to chunk small enough to satisfy the API.",
      i = paste0('Use `px_meta("', table_id, '")` to narrow your selection.')
    ),
    class = "px_error_query_too_large"
  )
}

# Pick the variable to chunk on.
# Prefers px_all("*") selections — user asked for all values, so the
# cardinality is large and known from meta. Among those, picks the one with
# the most values. Falls back to the explicit selection with the most values.
pick_chunk_variable <- function(selections, meta) {
  px_all_vars <- names(selections)[vapply(selections, .is_px_all_star, logical(1L))]

  if (length(px_all_vars) > 0L) {
    counts <- vapply(px_all_vars, function(nm) sum(meta$variable == nm), integer(1L))
    return(px_all_vars[which.max(counts)])
  }

  if (length(selections) == 0L) {
    rlang::abort(
      "Cannot chunk: no selections to split on.",
      class = "px_error_query_too_large"
    )
  }

  lengths <- vapply(selections, function(v) length(as.character(v)), integer(1L))
  names(selections)[which.max(lengths)]
}

# Return the values to iterate over for the chosen chunk variable.
# For px_all("*"), all values come from meta. For explicit selections, use
# the values directly.
chunk_variable_values <- function(chunk_var, selections, meta) {
  v <- selections[[chunk_var]]
  if (.is_px_all_star(v)) return(meta$value[meta$variable == chunk_var])
  as.character(v)
}

# Is this selection px_all("*") — literally all values?
.is_px_all_star <- function(v) {
  identical(attr(v, ".px_filter"), "all") && identical(as.character(v), "*")
}

# Attempt one chunk. Returns the parsed tibble on success, NULL on 403
# (still too large — caller should halve chunk_size and retry from scratch).
try_chunk <- function(url, selections, chunk_var, chunk_values, .codes, .lang, version) {
  sel              <- selections
  sel[[chunk_var]] <- chunk_values

  too_large <- function(e) NULL
  tryCatch(
    {
      resp <- if (version == 1L) {
        px_post_json(url, build_query_v1(sel))
      } else {
        px_get(build_v2_data_url(url, build_query_v2(sel), .lang))
      }
      parse_jsonstat(resp, .codes = .codes)$data
    },
    px_error_http_403 = too_large,
    px_error_http_400 = too_large
  )
}
