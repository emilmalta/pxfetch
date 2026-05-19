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

# validate_codes_arg() ---------------------------------------------------------

test_that("validate_codes_arg() accepts valid global strings", {
  expect_no_error(validate_codes_arg("none"))
  expect_no_error(validate_codes_arg("both"))
  expect_no_error(validate_codes_arg("columns"))
  expect_no_error(validate_codes_arg("values"))
})

test_that("validate_codes_arg() errors on invalid string", {
  expect_error(validate_codes_arg("bad"), class = "px_error_bad_codes")
})

test_that('validate_codes_arg() gives "Did you mean \\"both\\"?" hint for "all"', {
  expect_error(
    validate_codes_arg("all"),
    regexp = 'Did you mean "both"',
    class  = "px_error_bad_codes"
  )
})

test_that("validate_codes_arg() accepts a valid named character vector", {
  expect_no_error(validate_codes_arg(c(Region = "both", Tid = "none")))
})

test_that("validate_codes_arg() accepts a named vector with .default", {
  expect_no_error(validate_codes_arg(c(.default = "both", Tid = "none")))
})

test_that("validate_codes_arg() errors on invalid value in named vector", {
  expect_error(
    validate_codes_arg(c(Region = "all")),
    class = "px_error_bad_codes"
  )
})

test_that("validate_codes_arg() errors on unnamed non-singleton", {
  expect_error(
    validate_codes_arg(c("both", "none")),
    class = "px_error_bad_codes"
  )
})

# resolve_codes_per_var() ------------------------------------------------------

test_that("resolve_codes_per_var() global string applies to all vars", {
  out <- resolve_codes_per_var("both", c("gender", "year"))
  expect_equal(unname(out), c("both", "both"))
  expect_named(out, c("gender", "year"))
})

test_that("resolve_codes_per_var() named vector overrides specific vars", {
  out <- resolve_codes_per_var(c(gender = "both"), c("gender", "year"))
  expect_equal(out[["gender"]], "both")
  expect_equal(out[["year"]],   "none")
})

test_that("resolve_codes_per_var() .default slot sets the fallback", {
  out <- resolve_codes_per_var(c(.default = "both", Tid = "none"), c("Region", "Tid"))
  expect_equal(out[["Region"]], "both")
  expect_equal(out[["Tid"]],   "none")
})

test_that("resolve_codes_per_var() ignores names not in var_ids", {
  out <- resolve_codes_per_var(c(Unknown = "both"), c("gender", "year"))
  expect_equal(unname(out), c("none", "none"))
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
  out <- parse_jsonstat(fake_json_resp(jsonstat_v1), .codes = "both")
  expect_equal(out$data$gender, c("M", "M", "F", "F"))
})

test_that("parse_jsonstat() last dimension varies fastest (row-major)", {
  out <- parse_jsonstat(fake_json_resp(jsonstat_v1), .codes = "both")
  expect_equal(out$data$year, c("2020", "2021", "2020", "2021"))
})

test_that("parse_jsonstat() maps values in row-major order", {
  out <- parse_jsonstat(fake_json_resp(jsonstat_v1))
  expect_equal(out$data$value, c(10, 20, 30, 40))
})

# parse_jsonstat() — .codes behaviour ------------------------------------------

test_that('parse_jsonstat() .codes="none" labels column names and cell values', {
  out <- parse_jsonstat(fake_json_resp(jsonstat_v1), .codes = "none")
  expect_named(out$data, c("Gender", "Year", "value"))
  expect_equal(unique(out$data$Gender), c("Male", "Female"))
})

test_that('parse_jsonstat() .codes="both" keeps variable codes as column names', {
  out <- parse_jsonstat(fake_json_resp(jsonstat_v1), .codes = "both")
  expect_true("gender" %in% names(out$data))
  expect_true("year"   %in% names(out$data))
})

test_that('parse_jsonstat() .codes="both" keeps raw codes in cell values', {
  out <- parse_jsonstat(fake_json_resp(jsonstat_v1), .codes = "both")
  expect_true(all(out$data$gender %in% c("M", "F")))
})

test_that('parse_jsonstat() .codes="columns" keeps code column names, labels values', {
  out <- parse_jsonstat(fake_json_resp(jsonstat_v1), .codes = "columns")
  expect_true("gender" %in% names(out$data))
  expect_true(all(out$data$gender %in% c("Male", "Female")))
})

test_that('parse_jsonstat() .codes="values" labels column names, keeps code values', {
  out <- parse_jsonstat(fake_json_resp(jsonstat_v1), .codes = "values")
  expect_true("Gender" %in% names(out$data))
  expect_true(all(out$data$Gender %in% c("M", "F")))
})

test_that("parse_jsonstat() per-variable .codes applies selectively", {
  out <- parse_jsonstat(fake_json_resp(jsonstat_v1), .codes = c(gender = "both"))
  # gender: code column name + code values
  expect_true("gender" %in% names(out$data))
  expect_true(all(out$data$gender %in% c("M", "F")))
  # year: label column name (default "none")
  expect_true("Year" %in% names(out$data))
})

# parse_jsonstat() — missing values --------------------------------------------

test_that("parse_jsonstat() converts NULL values to NA_real_", {
  body       <- jsonstat_v2
  body$value <- list(10, NULL, 30, 40)
  out        <- parse_jsonstat(fake_json_resp(body))
  expect_true(is.na(out$data$value[[2L]]))
  expect_type(out$data$value, "double")
})

# parse_jsonstat() — json-stat2 index ordering ---------------------------------

test_that("parse_jsonstat() respects index position order, not insertion order", {
  body <- jsonstat_v2
  # Reverse the index insertion order for year
  body$dimension$year$category$index <- list(`2021` = 1L, `2020` = 0L)
  out  <- parse_jsonstat(body |> fake_json_resp(), .codes = "both")
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

test_that("px_fetch() errors on invalid .codes string", {
  withr::with_options(list(px.api_url = "https://example.com/api/v1/en/"), {
    expect_error(px_fetch("TBL", .codes = "bad"), class = "px_error_bad_codes")
  })
})

test_that('px_fetch() gives "Did you mean \\"both\\"?" hint for .codes = "all"', {
  withr::with_options(list(px.api_url = "https://example.com/api/v1/en/"), {
    expect_error(
      px_fetch("TBL", .codes = "all"),
      regexp = 'Did you mean "both"',
      class  = "px_error_bad_codes"
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

empty_meta <- function(...) {
  tibble::tibble(
    variable    = character(),
    label       = character(),
    eliminable  = logical(),
    is_time     = logical(),
    value       = character(),
    value_label = character()
  )
}

test_that("px_fetch() returns a tbl_px tibble for a v1 API", {
  withr::with_options(list(px.api_url = "https://example.com/api/v1/en/"), {
    local_mocked_bindings(
      px_post_json = function(...) fake_json_resp(jsonstat_v1),
      px_meta      = empty_meta,
      .package = "pxfetch"
    )
    out <- px_fetch("TBL", gender = c("M", "F"))
    expect_s3_class(out, "tbl_px")
    expect_s3_class(out, "tbl_df")
  })
})

test_that("px_fetch() returns a tbl_px tibble for a v2 API", {
  withr::with_options(list(px.api_url = "https://example.com/api/v2/"), {
    local_mocked_bindings(
      px_get  = function(...) fake_json_resp(jsonstat_v2),
      px_meta = empty_meta,
      .package = "pxfetch"
    )
    out <- px_fetch("TBL", gender = c("M", "F"))
    expect_s3_class(out, "tbl_px")
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

test_that('px_fetch() .codes="both" keeps variable codes as column names', {
  withr::with_options(list(px.api_url = "https://example.com/api/v1/en/"), {
    local_mocked_bindings(
      px_post_json = function(...) fake_json_resp(jsonstat_v1),
      .package = "pxfetch"
    )
    out <- px_fetch("TBL", .codes = "both")
    expect_true("gender" %in% names(out))
    expect_true("year"   %in% names(out))
  })
})

test_that('px_fetch() .codes="both" keeps raw codes in cells', {
  withr::with_options(list(px.api_url = "https://example.com/api/v1/en/"), {
    local_mocked_bindings(
      px_post_json = function(...) fake_json_resp(jsonstat_v1),
      .package = "pxfetch"
    )
    out <- px_fetch("TBL", .codes = "both")
    expect_true(all(out$gender %in% c("M", "F")))
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
