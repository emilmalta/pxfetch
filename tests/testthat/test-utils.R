test_that("%||% returns left-hand side when not NULL", {
  expect_equal("a" %||% "b", "a")
})

test_that("%||% returns right-hand side when NULL", {
  expect_equal(NULL %||% "b", "b")
})

test_that("is_valid_url accepts http and https", {
  expect_true(is_valid_url("https://bank.stat.gl/api/v1/en/"))
  expect_true(is_valid_url("http://example.com/api/v1/"))
})

test_that("is_valid_url rejects non-URLs", {
  expect_false(is_valid_url("BEESTA"))
  expect_false(is_valid_url(""))
})
