v1_body <- list(
  title = "Test table v1",
  variables = list(
    list(
      code        = "gender",
      text        = "Gender",
      values      = list("M", "F", "T"),
      valueTexts  = list("Male", "Female", "Total"),
      elimination = TRUE
    ),
    list(
      code       = "time",
      text       = "Year",
      values     = list("2020", "2021"),
      valueTexts = list("2020", "2021"),
      time       = TRUE
    )
  )
)

v2_body <- list(
  version = "2.0",
  class   = "dataset",
  label   = "Test table v2",
  role    = list(time = list("Tid"), geo = list(), metric = list()),
  id      = list("ContentsCode", "Tid"),
  dimension = list(
    ContentsCode = list(
      label    = "Contents",
      category = list(
        index = list(A = 0L, B = 1L),
        label = list(A = "Value A", B = "Value B")
      ),
      extension = list(elimination = FALSE)
    ),
    Tid = list(
      label    = "Year",
      category = list(
        index = list(`2021` = 1L, `2020` = 0L),  # deliberately out of order
        label = list(`2020` = "2020", `2021` = "2021")
      ),
      extension = list(elimination = FALSE)
    )
  )
)

# parse_meta_v1 ----------------------------------------------------------------

test_that("parse_meta_v1() returns a tibble with correct columns", {
  tbl <- parse_meta_v1(fake_json_resp(v1_body))
  expect_s3_class(tbl, "tbl_df")
  expect_named(tbl, c("variable", "label", "eliminable", "is_time", "value", "value_label"))
})

test_that("parse_meta_v1() expands all variable-value combinations", {
  tbl <- parse_meta_v1(fake_json_resp(v1_body))
  expect_equal(nrow(tbl), 5L)  # 3 gender + 2 time
})

test_that("parse_meta_v1() sets eliminable correctly", {
  tbl <- parse_meta_v1(fake_json_resp(v1_body))
  expect_true(all(tbl$eliminable[tbl$variable == "gender"]))
  expect_true(all(!tbl$eliminable[tbl$variable == "time"]))
})

test_that("parse_meta_v1() sets is_time correctly", {
  tbl <- parse_meta_v1(fake_json_resp(v1_body))
  expect_true(all(!tbl$is_time[tbl$variable == "gender"]))
  expect_true(all(tbl$is_time[tbl$variable == "time"]))
})

test_that("parse_meta_v1() attaches px_title attribute", {
  tbl <- parse_meta_v1(fake_json_resp(v1_body))
  expect_equal(attr(tbl, "px_title"), "Test table v1")
})

test_that("parse_meta_v1() coerces values to character", {
  tbl <- parse_meta_v1(fake_json_resp(v1_body))
  expect_type(tbl$value, "character")
  expect_type(tbl$value_label, "character")
})

# parse_meta_v2 ----------------------------------------------------------------

test_that("parse_meta_v2() returns a tibble with correct columns", {
  tbl <- parse_meta_v2(fake_json_resp(v2_body))
  expect_s3_class(tbl, "tbl_df")
  expect_named(tbl, c("variable", "label", "eliminable", "is_time", "value", "value_label"))
})

test_that("parse_meta_v2() expands all variable-value combinations", {
  tbl <- parse_meta_v2(fake_json_resp(v2_body))
  expect_equal(nrow(tbl), 4L)  # 2 ContentsCode + 2 Tid
})

test_that("parse_meta_v2() respects index order (not insertion order)", {
  tbl <- parse_meta_v2(fake_json_resp(v2_body))
  tid_vals <- tbl$value[tbl$variable == "Tid"]
  expect_equal(tid_vals, c("2020", "2021"))
})

test_that("parse_meta_v2() sets is_time from role", {
  tbl <- parse_meta_v2(fake_json_resp(v2_body))
  expect_true(all(tbl$is_time[tbl$variable == "Tid"]))
  expect_true(all(!tbl$is_time[tbl$variable == "ContentsCode"]))
})

test_that("parse_meta_v2() sets eliminable from extension", {
  tbl <- parse_meta_v2(fake_json_resp(v2_body))
  expect_true(all(!tbl$eliminable))
})

test_that("parse_meta_v2() attaches px_title attribute", {
  tbl <- parse_meta_v2(fake_json_resp(v2_body))
  expect_equal(attr(tbl, "px_title"), "Test table v2")
})

# px_meta() caching ------------------------------------------------------------

test_that("px_meta() caches result on first call", {
  px_meta_cache_clear()
  withr::with_options(list(px.api_url = "https://example.com/api/v1/en/"), {
    local_mocked_bindings(
      px_get = function(...) fake_json_resp(v1_body),
      .package = "pxfetch"
    )
    px_meta("TBL")
    expect_true(.px_meta_is_cached("TBL", NULL, "https://example.com/api/v1/en/"))
  })
})

test_that("px_meta() returns cached result without a second network call", {
  px_meta_cache_clear()
  withr::with_options(list(px.api_url = "https://example.com/api/v1/en/"), {
    call_count <- 0L
    local_mocked_bindings(
      px_get = function(...) { call_count <<- call_count + 1L; fake_json_resp(v1_body) },
      .package = "pxfetch"
    )
    px_meta("TBL")
    px_meta("TBL")
    expect_equal(call_count, 1L)
  })
})

test_that("px_meta_cache_clear() empties the cache", {
  withr::with_options(list(px.api_url = "https://example.com/api/v1/en/"), {
    local_mocked_bindings(
      px_get = function(...) fake_json_resp(v1_body),
      .package = "pxfetch"
    )
    px_meta("TBL")
    px_meta_cache_clear()
    expect_false(.px_meta_is_cached("TBL", NULL, "https://example.com/api/v1/en/"))
  })
})

test_that("px_meta() caches different table IDs independently", {
  px_meta_cache_clear()
  withr::with_options(list(px.api_url = "https://example.com/api/v1/en/"), {
    local_mocked_bindings(
      px_get = function(...) fake_json_resp(v1_body),
      .package = "pxfetch"
    )
    px_meta("TBL1")
    expect_true(.px_meta_is_cached("TBL1", NULL, "https://example.com/api/v1/en/"))
    expect_false(.px_meta_is_cached("TBL2", NULL, "https://example.com/api/v1/en/"))
  })
})
