# Helpers -----------------------------------------------------------------------

# Minimal httr2_response skeleton. httr2::resp_status() reads $status_code;
# httr2::resp_body_string() reads $body (raw). The tryCatch in
# px_stop_for_status() handles bodies that can't be parsed.
fake_resp <- function(status, body = raw(0)) {
  structure(
    list(
      status_code = as.integer(status),
      headers     = list(),
      body        = body,
      cache       = new.env(parent = emptyenv())
    ),
    class = "httr2_response"
  )
}

# Tests -------------------------------------------------------------------------

test_that("px_stop_for_status() is silent on 200", {
  expect_no_error(px_stop_for_status(fake_resp(200L), "https://example.com"))
})

test_that("px_stop_for_status() is silent on 204", {
  expect_no_error(px_stop_for_status(fake_resp(204L), "https://example.com"))
})

test_that("px_stop_for_status() throws px_error_http on 404", {
  expect_error(
    px_stop_for_status(fake_resp(404L), "https://example.com"),
    class = "px_error_http"
  )
})

test_that("px_stop_for_status() throws px_error_http_404 on 404", {
  expect_error(
    px_stop_for_status(fake_resp(404L), "https://example.com"),
    class = "px_error_http_404"
  )
})

test_that("px_stop_for_status() throws px_error_http_403 on 403", {
  expect_error(
    px_stop_for_status(fake_resp(403L), "https://example.com"),
    class = "px_error_http_403"
  )
})

test_that("px_error_http_403 is also a px_error_http", {
  err <- rlang::catch_cnd(
    px_stop_for_status(fake_resp(403L), "https://example.com"),
    classes = "px_error_http"
  )
  expect_s3_class(err, "px_error_http_403")
  expect_s3_class(err, "px_error_http")
})

test_that("px_stop_for_status() error carries status and url fields", {
  err <- rlang::catch_cnd(
    px_stop_for_status(fake_resp(500L), "https://example.com"),
    classes = "px_error_http"
  )
  expect_equal(err$status, 500L)
  expect_equal(err$url, "https://example.com")
})

test_that("px_stop_for_status() throws px_error_http on 500", {
  expect_error(
    px_stop_for_status(fake_resp(500L), "https://example.com"),
    class = "px_error_http_500"
  )
})
