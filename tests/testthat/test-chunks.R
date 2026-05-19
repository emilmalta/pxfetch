fake_meta <- tibble::tibble(
  variable    = c("gender", "gender", "gender", "year", "year"),
  label       = c("Gender", "Gender", "Gender", "Year", "Year"),
  eliminable  = c(TRUE, TRUE, TRUE, FALSE, FALSE),
  is_time     = c(FALSE, FALSE, FALSE, TRUE, TRUE),
  value       = c("M", "F", "T", "2020", "2021"),
  value_label = c("Male", "Female", "Total", "2020", "2021")
)

# .is_px_all_star() ------------------------------------------------------------

test_that(".is_px_all_star() is TRUE for px_all('*')", {
  expect_true(.is_px_all_star(px_all("*")))
})

test_that(".is_px_all_star() is FALSE for a wildcard pattern", {
  expect_false(.is_px_all_star(px_all("*0")))
})

test_that(".is_px_all_star() is FALSE for a plain character vector", {
  expect_false(.is_px_all_star(c("M", "F")))
})

test_that(".is_px_all_star() is FALSE for px_top()", {
  expect_false(.is_px_all_star(px_top(5L)))
})

# pick_chunk_variable() --------------------------------------------------------

test_that("pick_chunk_variable() prefers px_all('*') over explicit selections", {
  sel <- list(gender = px_all("*"), year = c("2020", "2021"))
  expect_equal(pick_chunk_variable(sel, fake_meta), "gender")
})

test_that("pick_chunk_variable() picks px_all('*') var with most values", {
  meta <- tibble::tibble(
    variable   = c("a", "a", "b", "b", "b"),
    value      = c("1", "2", "x", "y", "z"),
    eliminable = TRUE, label = "", is_time = FALSE, value_label = ""
  )
  sel <- list(a = px_all("*"), b = px_all("*"))
  expect_equal(pick_chunk_variable(sel, meta), "b")
})

test_that("pick_chunk_variable() falls back to longest explicit selection", {
  sel <- list(gender = c("M", "F", "T"), year = c("2020", "2021"))
  expect_equal(pick_chunk_variable(sel, fake_meta), "gender")
})

# chunk_variable_values() ------------------------------------------------------

test_that("chunk_variable_values() returns meta values for px_all('*')", {
  sel <- list(gender = px_all("*"))
  expect_equal(
    chunk_variable_values("gender", sel, fake_meta),
    c("M", "F", "T")
  )
})

test_that("chunk_variable_values() returns explicit values as character", {
  sel <- list(year = c("2020", "2021"))
  expect_equal(chunk_variable_values("year", sel, fake_meta), c("2020", "2021"))
})

# chunk_large_query() end-to-end -----------------------------------------------

# Minimal json-stat response for a single gender value x two years
make_jsonstat <- function(gender_code, gender_label) {
  list(
    version = "2.0", class = "dataset",
    label   = "Test table",
    id      = list("gender", "year"),
    size    = list(1L, 2L),
    dimension = list(
      gender = list(
        label    = "Gender",
        category = list(
          index = stats::setNames(list(0L), gender_code),
          label = stats::setNames(list(gender_label), gender_code)
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
    value = list(1, 2)
  )
}

test_that("chunk_large_query() splits and reassembles chunks correctly", {
  px_meta_cache_clear()
  withr::with_options(list(px.api_url = "https://example.com/api/v1/en/"), {
    local_mocked_bindings(
      px_get      = function(...) fake_json_resp(list()),
      px_post_json = function(url, body, ...) {
        # Detect which gender value is being fetched from the query body
        codes <- body$query[[1]]$selection$values
        if (length(codes) != 1L) {
          rlang::abort("too many", class = c("px_error_http_403", "px_error_http", "error"))
        }
        gender_code  <- codes[[1]]
        gender_label <- list(M = "Male", F = "Female", T = "Total")[[gender_code]]
        fake_json_resp(make_jsonstat(gender_code, gender_label))
      },
      px_meta = function(...) {
        structure(fake_meta, px_title = "Test table")
      },
      .package = "pxfetch"
    )

    # selections: gender = px_all("*") should trigger chunking
    result <- chunk_large_query(
      table_id   = "TBL",
      selections = list(gender = px_all("*"), year = c("2020", "2021")),
      .codes     = "none",
      .lang      = NULL,
      .api_url   = "https://example.com/api/v1/en/"
    )

    expect_s3_class(result, "data.frame")
    expect_equal(nrow(result), 6L)  # 3 genders x 2 years
    expect_equal(attr(result, "px_title"), "Test table")
  })
})

test_that("chunk_large_query() works for v2 APIs (triggers on HTTP 400)", {
  px_meta_cache_clear()
  withr::with_options(list(px.api_url = "https://example.com/api/v2/en/"), {
    local_mocked_bindings(
      px_post_json = function(...) fake_json_resp(list()),
      px_get = function(url, ...) {
        # Parse the gender valueCodes from the query string
        m <- regmatches(url, regexpr("(?<=valueCodes\\[gender\\]=)[^&]+", url, perl = TRUE))
        codes <- strsplit(m, ",")[[1L]]
        if (length(codes) != 1L) {
          rlang::abort("too many", class = c("px_error_http_400", "px_error_http", "error"))
        }
        gender_code  <- codes[[1L]]
        gender_label <- list(M = "Male", F = "Female", T = "Total")[[gender_code]]
        fake_json_resp(make_jsonstat(gender_code, gender_label))
      },
      px_meta = function(...) {
        structure(fake_meta, px_title = "Test table")
      },
      .package = "pxfetch"
    )

    result <- chunk_large_query(
      table_id   = "TBL",
      selections = list(gender = px_all("*"), year = c("2020", "2021")),
      .codes     = "none",
      .lang      = NULL,
      .api_url   = "https://example.com/api/v2/en/"
    )

    expect_s3_class(result, "data.frame")
    expect_equal(nrow(result), 6L)  # 3 genders x 2 years
    expect_equal(attr(result, "px_title"), "Test table")
  })
})
