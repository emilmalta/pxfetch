# px_tag() ---------------------------------------------------------------------

test_that("px_tag() returns a px_ball", {
  df  <- tibble::tibble(x = 1L)
  out <- px_tag(df, "TBL")
  expect_s3_class(out, "px_ball")
})

test_that("px_tag() prepends px_ball to the existing class vector", {
  df  <- tibble::tibble(x = 1L)
  out <- px_tag(df, "TBL")
  expect_equal(class(out), c("px_ball", "tbl_df", "tbl", "data.frame"))
})

test_that("px_tag() sets px_table_id attribute", {
  out <- px_tag(tibble::tibble(x = 1L), "BEXSTA")
  expect_equal(attr(out, "px_table_id"), "BEXSTA")
})

test_that("px_tag() sets px_title attribute from title argument", {
  out <- px_tag(tibble::tibble(x = 1L), "TBL", title = "My table")
  expect_equal(attr(out, "px_title"), "My table")
})

test_that("px_tag() sets px_title to NA_character_ when title is NULL", {
  out <- px_tag(tibble::tibble(x = 1L), "TBL")
  expect_identical(attr(out, "px_title"), NA_character_)
})

test_that("px_tag() sets px_class attribute to 'ball'", {
  out <- px_tag(tibble::tibble(x = 1L), "TBL")
  expect_equal(attr(out, "px_class"), "ball")
})

test_that("px_tag() sets px_api_url attribute", {
  withr::with_options(list(px.api_url = "https://example.com/api/v1/en/"), {
    out <- px_tag(tibble::tibble(x = 1L), "TBL")
    expect_equal(attr(out, "px_api_url"), "https://example.com/api/v1/en/")
  })
})

test_that("px_tag() preserves data frame content", {
  df  <- tibble::tibble(a = 1:3, b = c("x", "y", "z"))
  out <- px_tag(df, "TBL")
  expect_equal(nrow(out), 3L)
  expect_named(out, c("a", "b"))
})

test_that("px_tag() errors on non-data-frame input", {
  expect_error(px_tag(list(x = 1), "TBL"), class = "px_error_bad_data")
})

test_that("px_tag() errors on vector input", {
  expect_error(px_tag(1:5, "TBL"), class = "px_error_bad_data")
})

# Print methods ----------------------------------------------------------------

make_px_ball <- function(title = "My table", table_id = "TBL") {
  px_tag(tibble::tibble(a = 1L, b = "x"), table_id = table_id, title = title)
}

test_that("tbl_sum.px_ball includes 'Title' entry when title is present", {
  x   <- make_px_ball(title = "Population by region")
  out <- pillar::tbl_sum(x)
  expect_true("Title" %in% names(out))
  expect_equal(out[["Title"]], "Population by region")
})

test_that("tbl_sum.px_ball omits 'Title' when title is NA", {
  x   <- make_px_ball(title = NA_character_)
  out <- pillar::tbl_sum(x)
  expect_false("Title" %in% names(out))
})

test_that("tbl_sum.px_ball omits 'Title' when title is empty string", {
  x   <- px_tag(tibble::tibble(a = 1L), "TBL", title = "")
  out <- pillar::tbl_sum(x)
  expect_false("Title" %in% names(out))
})

test_that("tbl_sum.px_ball retains the parent tibble header entry", {
  x   <- make_px_ball()
  out <- pillar::tbl_sum(x)
  expect_true(length(out) >= 1L)
  expect_true(nzchar(out[[1L]]))  # e.g. "2 x 2"
})

test_that("print output includes table title when present", {
  x     <- make_px_ball(title = "My population table")
  lines <- capture.output(print(x))
  expect_true(any(grepl("My population table", lines, fixed = TRUE)))
})

test_that("print output includes px_meta() hint", {
  x     <- make_px_ball(table_id = "BEXSTA")
  lines <- capture.output(print(x))
  expect_true(any(grepl("px_meta", lines, fixed = TRUE)))
  expect_true(any(grepl("BEXSTA", lines, fixed = TRUE)))
})

test_that("print output omits px_meta() hint when table_id is NULL", {
  x     <- tibble::tibble(a = 1L)
  class(x) <- c("px_ball", class(x))
  attr(x, "px_table_id") <- NULL
  lines <- capture.output(print(x))
  expect_false(any(grepl("px_meta", lines, fixed = TRUE)))
})
