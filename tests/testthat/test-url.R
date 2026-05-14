test_that("px_url() builds a v1 URL correctly", {
  expect_equal(
    px_url("BEXSTA", .api_url = "https://bank.stat.gl/api/v1/en/Greenland/"),
    "https://bank.stat.gl/api/v1/en/Greenland/BEXSTA"
  )
})

test_that("px_url() handles base URL without trailing slash", {
  expect_equal(
    px_url("BEXSTA", .api_url = "https://bank.stat.gl/api/v1/en/Greenland"),
    "https://bank.stat.gl/api/v1/en/Greenland/BEXSTA"
  )
})

test_that("px_url() builds a v2 URL correctly (SSB-style path)", {
  expect_equal(
    px_url("05279", .api_url = "https://data.ssb.no/api/pxwebapi/v2/"),
    "https://data.ssb.no/api/pxwebapi/v2/tables/05279"
  )
})

test_that("px_url() tolerates /tables in the v2 base URL", {
  expect_equal(
    px_url("05279", .api_url = "https://data.ssb.no/api/pxwebapi/v2/tables"),
    "https://data.ssb.no/api/pxwebapi/v2/tables/05279"
  )
})

test_that("px_url() respects .api_url over the global option", {
  withr::with_options(
    list(px.api_url = "https://bank.stat.gl/api/v1/en/Greenland/"),
    expect_equal(
      px_url("T05279", .api_url = "https://data.ssb.no/api/pxwebapi/v2/"),
      "https://data.ssb.no/api/pxwebapi/v2/tables/T05279"
    )
  )
})

test_that("px_url() passes table_id through unchanged (case-sensitive)", {
  expect_equal(
    px_url("bexsta", .api_url = "https://bank.stat.gl/api/v1/en/Greenland/"),
    "https://bank.stat.gl/api/v1/en/Greenland/bexsta"
  )
})

test_that("px_url() aborts on empty table_id", {
  expect_error(
    px_url("", .api_url = "https://bank.stat.gl/api/v1/en/Greenland/"),
    class = "px_error_bad_table_id"
  )
})

test_that("px_url() aborts on non-character table_id", {
  expect_error(
    px_url(123L, .api_url = "https://bank.stat.gl/api/v1/en/Greenland/"),
    class = "px_error_bad_table_id"
  )
})
