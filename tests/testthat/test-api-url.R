test_that("px_api_url() returns the option value when set", {
  withr::with_options(
    list(px.api_url = "https://bank.stat.gl/api/v1/en/Greenland/"),
    expect_equal(px_api_url(), "https://bank.stat.gl/api/v1/en/Greenland/")
  )
})

test_that("px_api_url() aborts with px_error_no_url when option is not set", {
  withr::with_options(
    list(px.api_url = NULL),
    expect_error(px_api_url(), class = "px_error_no_url")
  )
})

test_that("px_api_version() detects v1", {
  expect_equal(
    px_api_version("https://bank.stat.gl/api/v1/en/Greenland/"),
    1L
  )
})

test_that("px_api_version() detects v2", {
  expect_equal(
    px_api_version("https://example.stat.org/api/v2/sv/"),
    2L
  )
})

test_that("px_api_version() aborts with px_error_bad_url when version is missing", {
  expect_error(
    px_api_version("https://example.stat.org/no/version/here/"),
    class = "px_error_bad_url"
  )
})

test_that("px_api_version() aborts on NULL input", {
  expect_error(px_api_version(NULL), class = "px_error_bad_url")
})

test_that("px_api_version() aborts on empty string", {
  expect_error(px_api_version(""), class = "px_error_bad_url")
})
