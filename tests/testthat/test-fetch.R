# Helpers ----------------------------------------------------------------------

fake_json_resp <- function(x) {
  structure(
    list(
      status_code = 200L,
      headers     = list("content-type" = "application/json; charset=utf-8"),
      body        = charToRaw(jsonlite::toJSON(x, auto_unbox = TRUE, null = "null")),
      cache       = new.env(parent = emptyenv())
    ),
    class = "httr2_response"
  )
}

# 2x2 table: gender (M, F) x year (2020, 2021)
# Row-major values: (M,2020)=10, (M,2021)=20, (F,2020)=30, (F,2021)=40
jsonstat_v1 <- list(
  dataset = list(
    label = "Population by gender and year",
    dimension = list(
      id     = list("gender", "year"),
      size   = list(2L, 2L),
      gender = list(
        label    = "Gender",
        category = list(
          index = list(M = 0L, F = 1L),
          label = list(M = "Male", F = "Female")
        )
      ),
      year = list(
        label    = "Year",
        category = list(
          index = list(`2020` = 0L, `2021` = 1L),
          label = list(`2020` = "2020", `2021` = "2021")
        )
      )
    ),
    value = list(10, 20, 30, 40)
  )
)

jsonstat_v2 <- list(
  version = "2.0",
  class   = "dataset",
  label   = "Population by gender and year",
  id      = list("gender", "year"),
  size    = list(2L, 2L),
  dimension = list(
    gender = list(
      label    = "Gender",
      category = list(
        index = list(M = 0L, F = 1L),
        label = list(M = "Male", F = "Female")
      )
    ),
    year = list(
      label    = "Year",
      category = list(
        index = list(`2020` = 0L, `2021` = 1L),
        label = list(`2020` = "2020", `2021` = "2021")
      )
    )
  ),
  value = list(10, 20, 30, 40)
)

# resolve_code_selection() -----------------------------------------------------

test_that("resolve_code_selection() returns character(0) for FALSE", {
  out <- resolve_code_selection(FALSE, c("A", "B", "C"))
  expect_identical(out, character(0))
})

test_that("resolve_code_selection() returns all_ids for TRUE", {
  ids <- c("gender", "year", "region")
  out <- resolve_code_selection(TRUE, ids)
  expect_identical(out, ids)
})

test_that("resolve_code_selection() coerces character vector as-is", {
  out <- resolve_code_selection(c("gender", "year"), c("gender", "year", "region"))
  expect_identical(out, c("gender", "year"))
})

test_that("resolve_code_selection() coerces non-character to character", {
  out <- resolve_code_selection(c(1L, 2L), character(0))
  expect_type(out, "character")
})

# parse_jsonstat() — format detection ------------------------------------------

test_that("parse_jsonstat() detects json-stat via $dataset wrapper", {
  out <- parse_jsonstat(fake_json_resp(jsonstat_v1))
  expect_s3_class(out$data, "tbl_df")
})

test_that("parse_jsonstat() detects json-stat2 via top-level fields", {
  out <- parse_jsonstat(fake_json_resp(jsonstat_v2))
  expect_s3_class(out$data, "tbl_df")
})

test_that("parse_jsonstat() extracts title from json-stat", {
  out <- parse_jsonstat(fake_json_resp(jsonstat_v1))
  expect_equal(out$title, "Population by gender and year")
})

test_that("parse_jsonstat() extracts title from json-stat2", {
  out <- parse_jsonstat(fake_json_resp(jsonstat_v2))
  expect_equal(out$title, "Population by gender and year")
})

# parse_jsonstat() — row-major ordering ----------------------------------------

test_that("parse_jsonstat() produces 4 rows for a 2x2 table", {
  out <- parse_jsonstat(fake_json_resp(jsonstat_v1))
  expect_equal(nrow(out$data), 4L)
})

test_that("parse_jsonstat() first dimension varies slowest (row-major)", {
  out   <- parse_jsonstat(fake_json_resp(jsonstat_v1), .column_codes = TRUE, .value_codes = TRUE)
  tbl   <- out$data
  # First dim (gender) should go M, M, F, F — not M, F, M, F
  expect_equal(tbl$gender, c("M", "M", "F", "F"))
})

test_that("parse_jsonstat() last dimension varies fastest (row-major)", {
  out <- parse_jsonstat(fake_json_resp(jsonstat_v1), .column_codes = TRUE, .value_codes = TRUE)
  tbl <- out$data
  expect_equal(tbl$year, c("2020", "2021", "2020", "2021"))
})

test_that("parse_jsonstat() maps values in row-major order", {
  out <- parse_jsonstat(fake_json_resp(jsonstat_v1))
  expect_equal(out$data$value, c(10, 20, 30, 40))
})

# parse_jsonstat() — labels vs. codes ------------------------------------------

test_that("parse_jsonstat() applies value labels by default (.value_codes=FALSE)", {
  out <- parse_jsonstat(fake_json_resp(jsonstat_v1))
  expect_equal(unique(out$data$Gender), c("Male", "Female"))
})

test_that("parse_jsonstat() keeps value codes when .value_codes=TRUE", {
  out <- parse_jsonstat(fake_json_resp(jsonstat_v1), .value_codes = TRUE)
  expect_true("gender" %in% names(out$data) || "Gender" %in% names(out$data))
  gender_col <- out$data[[grep("ender", names(out$data))]]
  expect_true(all(gender_col %in% c("M", "F")))
})

test_that("parse_jsonstat() renames columns to variable labels by default", {
  out <- parse_jsonstat(fake_json_resp(jsonstat_v1))
  expect_named(out$data, c("Gender", "Year", "value"))
})

test_that("parse_jsonstat() keeps column codes when .column_codes=TRUE", {
  out <- parse_jsonstat(fake_json_resp(jsonstat_v1), .column_codes = TRUE)
  expect_true("gender" %in% names(out$data))
  expect_true("year" %in% names(out$data))
})

test_that("parse_jsonstat() selective .column_codes keeps only named vars as codes", {
  out <- parse_jsonstat(fake_json_resp(jsonstat_v1), .column_codes = "gender")
  expect_true("gender" %in% names(out$data))
  expect_true("Year" %in% names(out$data))
})

test_that("parse_jsonstat() selective .value_codes keeps only named vars as codes", {
  out <- parse_jsonstat(fake_json_resp(jsonstat_v1), .value_codes = "gender")
  tbl <- out$data
  gender_col <- tbl[[grep("ender", names(tbl))]]
  expect_true(all(gender_col %in% c("M", "F")))
  # year values should still be labels (which happen to equal codes here)
  expect_true("Year" %in% names(tbl) || "year" %in% names(tbl))
})

# parse_jsonstat() — missing values --------------------------------------------

test_that("parse_jsonstat() converts NULL values to NA_real_", {
  body      <- jsonstat_v2
  body$value <- list(10, NULL, 30, 40)
  out       <- parse_jsonstat(fake_json_resp(body))
  expect_true(is.na(out$data$value[[2L]]))
  expect_type(out$data$value, "double")
})

# parse_jsonstat() — json-stat2 index ordering ---------------------------------

test_that("parse_jsonstat() respects index position order, not insertion order", {
  body <- jsonstat_v2
  # Reverse the index insertion order for year
  body$dimension$year$category$index <- list(`2021` = 1L, `2020` = 0L)
  out  <- parse_jsonstat(body |> fake_json_resp(), .column_codes = TRUE, .value_codes = TRUE)
  year_vals <- out$data$year[out$data$gender == "M"]
  expect_equal(year_vals, c("2020", "2021"))
})

# build_v2_data_url() ----------------------------------------------------------

test_that("build_v2_data_url() appends /data suffix", {
  url <- build_v2_data_url("https://example.com/api/v2/TBL", "", NULL)
  expect_true(startsWith(url, "https://example.com/api/v2/TBL/data"))
})

test_that("build_v2_data_url() always includes outputFormat=json-stat2", {
  url <- build_v2_data_url("https://example.com/api/v2/TBL", "", NULL)
  expect_true(grepl("outputFormat=json-stat2", url, fixed = TRUE))
})

test_that("build_v2_data_url() adds lang parameter when .lang is set", {
  url <- build_v2_data_url("https://example.com/api/v2/TBL", "", "en")
  expect_true(grepl("lang=en", url, fixed = TRUE))
})

test_that("build_v2_data_url() omits lang when .lang is NULL", {
  url <- build_v2_data_url("https://example.com/api/v2/TBL", "", NULL)
  expect_false(grepl("lang=", url, fixed = TRUE))
})

test_that("build_v2_data_url() appends non-empty query string", {
  url <- build_v2_data_url("https://example.com/api/v2/TBL", "valueCodes[x]=A,B", NULL)
  expect_true(grepl("valueCodes[x]=A,B", url, fixed = TRUE))
})

test_that("build_v2_data_url() does not double-append empty query string", {
  url <- build_v2_data_url("https://example.com/api/v2/TBL", "", NULL)
  expect_false(grepl("&&", url, fixed = TRUE))
})

# px_fetch() — argument validation ---------------------------------------------

test_that("px_fetch() errors on unnamed ... arguments", {
  withr::with_options(list(px.api_url = "https://example.com/api/v1/en/"), {
    expect_error(
      px_fetch("TBL", "M"),
      class = "px_error_unnamed_selection"
    )
  })
})

test_that("px_fetch() errors when any ... argument is unnamed", {
  withr::with_options(list(px.api_url = "https://example.com/api/v1/en/"), {
    expect_error(
      px_fetch("TBL", gender = "M", "extra"),
      class = "px_error_unnamed_selection"
    )
  })
})

# px_fetch() — dry run ---------------------------------------------------------

test_that("px_fetch() dry_run=TRUE returns a list without sending a request", {
  withr::with_options(list(px.api_url = "https://example.com/api/v1/en/"), {
    out <- px_fetch("TBL", gender = "M", .dry_run = TRUE)
    expect_type(out, "list")
  })
})

test_that("px_fetch() dry_run result contains url, version, selections, body for v1", {
  withr::with_options(list(px.api_url = "https://example.com/api/v1/en/"), {
    out <- px_fetch("TBL", gender = "M", .dry_run = TRUE)
    expect_named(out, c("url", "version", "selections", "body"))
    expect_equal(out$version, 1L)
  })
})

test_that("px_fetch() dry_run result contains url, version, selections, query for v2", {
  withr::with_options(list(px.api_url = "https://example.com/api/v2/"), {
    out <- px_fetch("TBL", gender = "M", .dry_run = TRUE)
    expect_named(out, c("url", "version", "selections", "query"))
    expect_equal(out$version, 2L)
  })
})

test_that("px_fetch() dry_run preserves selections", {
  withr::with_options(list(px.api_url = "https://example.com/api/v1/en/"), {
    out <- px_fetch("TBL", gender = c("M", "F"), .dry_run = TRUE)
    expect_equal(out$selections$gender, c("M", "F"))
  })
})

test_that("px_fetch() dry_run url contains table_id", {
  withr::with_options(list(px.api_url = "https://example.com/api/v1/en/"), {
    out <- px_fetch("MYTBL", .dry_run = TRUE)
    expect_true(grepl("MYTBL", out$url, fixed = TRUE))
  })
})

test_that("px_fetch() dry_run rewrites lang in URL for v1", {
  withr::with_options(list(px.api_url = "https://example.com/api/v1/en/"), {
    out <- px_fetch("TBL", .lang = "da", .dry_run = TRUE)
    expect_true(grepl("/da/", out$url, fixed = TRUE))
    expect_false(grepl("/en/", out$url, fixed = TRUE))
  })
})

# px_fetch() — mocked HTTP -----------------------------------------------------

test_that("px_fetch() returns a px_ball tibble for a v1 API", {
  withr::with_options(list(px.api_url = "https://example.com/api/v1/en/"), {
    local_mocked_bindings(
      px_post_json = function(...) fake_json_resp(jsonstat_v1),
      .package = "pxfetch"
    )
    out <- px_fetch("TBL", gender = c("M", "F"))
    expect_s3_class(out, "px_ball")
    expect_s3_class(out, "tbl_df")
  })
})

test_that("px_fetch() returns a px_ball tibble for a v2 API", {
  withr::with_options(list(px.api_url = "https://example.com/api/v2/"), {
    local_mocked_bindings(
      px_get = function(...) fake_json_resp(jsonstat_v2),
      .package = "pxfetch"
    )
    out <- px_fetch("TBL", gender = c("M", "F"))
    expect_s3_class(out, "px_ball")
  })
})

test_that("px_fetch() attaches table_id attribute", {
  withr::with_options(list(px.api_url = "https://example.com/api/v1/en/"), {
    local_mocked_bindings(
      px_post_json = function(...) fake_json_resp(jsonstat_v1),
      .package = "pxfetch"
    )
    out <- px_fetch("MYTBL")
    expect_equal(attr(out, "px_table_id"), "MYTBL")
  })
})

test_that("px_fetch() attaches title attribute from response", {
  withr::with_options(list(px.api_url = "https://example.com/api/v1/en/"), {
    local_mocked_bindings(
      px_post_json = function(...) fake_json_resp(jsonstat_v1),
      .package = "pxfetch"
    )
    out <- px_fetch("TBL")
    expect_equal(attr(out, "px_title"), "Population by gender and year")
  })
})

test_that("px_fetch() .column_codes=TRUE keeps variable codes as column names", {
  withr::with_options(list(px.api_url = "https://example.com/api/v1/en/"), {
    local_mocked_bindings(
      px_post_json = function(...) fake_json_resp(jsonstat_v1),
      .package = "pxfetch"
    )
    out <- px_fetch("TBL", .column_codes = TRUE)
    expect_true("gender" %in% names(out))
    expect_true("year" %in% names(out))
  })
})

test_that("px_fetch() .value_codes=TRUE keeps raw codes in cells", {
  withr::with_options(list(px.api_url = "https://example.com/api/v1/en/"), {
    local_mocked_bindings(
      px_post_json = function(...) fake_json_resp(jsonstat_v1),
      .package = "pxfetch"
    )
    out <- px_fetch("TBL", .value_codes = TRUE)
    gender_col <- out[[grep("ender", names(out))]]
    expect_true(all(gender_col %in% c("M", "F")))
  })
})

test_that("px_fetch() raises px_error_query_too_large on 403", {
  withr::with_options(list(px.api_url = "https://example.com/api/v1/en/"), {
    local_mocked_bindings(
      px_post_json = function(...) {
        rlang::abort("403", class = c("px_error_http_403", "px_error_http", "error"))
      },
      chunk_large_query = function(...) {
        rlang::abort("Query too large", class = "px_error_query_too_large")
      },
      .package = "pxfetch"
    )
    expect_error(px_fetch("TBL"), class = "px_error_query_too_large")
  })
})
