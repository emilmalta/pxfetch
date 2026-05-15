# DSL helpers ------------------------------------------------------------------

test_that("pxw_top() sets .pxw_filter = 'Top'", {
  expect_equal(attr(pxw_top(3L), ".pxw_filter"), "Top")
})

test_that("pxw_top() coerces to integer", {
  expect_type(pxw_top(3), "integer")
})

test_that("pxw_all() sets .pxw_filter = 'all'", {
  expect_equal(attr(pxw_all(), ".pxw_filter"), "all")
})

test_that("pxw_all() defaults to '*'", {
  expect_equal(as.character(pxw_all()), "*")
})

test_that("pxw_all() accepts a wildcard pattern", {
  expect_equal(as.character(pxw_all("*0")), "*0")
})

test_that("top() is an alias for pxw_top()", {
  expect_equal(top(2L), pxw_top(2L))
})

test_that("every() is an alias for pxw_all()", {
  expect_equal(every("*"), pxw_all("*"))
})

# build_query_v1 ---------------------------------------------------------------

test_that("build_query_v1() returns a list with query and response", {
  out <- build_query_v1(list())
  expect_named(out, c("query", "response"))
  expect_equal(out$response$format, jsonlite::unbox("json-stat2"))
})

test_that("build_query_v1() builds an item filter for plain values", {
  out <- build_query_v1(list(gender = c("M", "K")))
  item <- out$query[[1]]
  expect_equal(as.character(item$code), "gender")
  expect_equal(as.character(item$selection$filter), "item")
  expect_equal(item$selection$values, c("M", "K"))
})

test_that("build_query_v1() builds a Top filter for pxw_top()", {
  out <- build_query_v1(list(time = pxw_top(5L)))
  item <- out$query[[1]]
  expect_equal(as.character(item$selection$filter), "Top")
  expect_equal(item$selection$values, "5")
})

test_that("build_query_v1() builds an all filter for pxw_all()", {
  out <- build_query_v1(list(time = pxw_all("*0")))
  item <- out$query[[1]]
  expect_equal(as.character(item$selection$filter), "all")
  expect_equal(item$selection$values, "*0")
})

test_that("build_query_v1() handles multiple variables", {
  out <- build_query_v1(list(gender = c("M", "K"), time = pxw_top(3L)))
  expect_length(out$query, 2L)
  expect_equal(as.character(out$query[[1]]$code), "gender")
  expect_equal(as.character(out$query[[2]]$code), "time")
})

# build_query_v2 ---------------------------------------------------------------

test_that("build_query_v2() returns empty string for empty selections", {
  expect_equal(build_query_v2(list()), "")
})

test_that("build_query_v2() builds valueCodes for plain values", {
  out <- build_query_v2(list(gender = c("M", "K")))
  expect_equal(out, "valueCodes[gender]=M,K")
})

test_that("build_query_v2() builds top() syntax for pxw_top()", {
  out <- build_query_v2(list(time = pxw_top(5L)))
  expect_equal(out, "valueCodes[time]=top(5)")
})

test_that("build_query_v2() passes wildcard through for pxw_all()", {
  out <- build_query_v2(list(region = pxw_all("324*")))
  expect_equal(out, "valueCodes[region]=324*")
})

test_that("build_query_v2() joins multiple variables with &", {
  out <- build_query_v2(list(gender = c("M", "K"), time = pxw_top(3L)))
  expect_equal(out, "valueCodes[gender]=M,K&valueCodes[time]=top(3)")
})

test_that("build_query_v2() preserves variable order", {
  out <- build_query_v2(list(time = pxw_top(2L), region = c("0301")))
  expect_equal(out, "valueCodes[time]=top(2)&valueCodes[region]=0301")
})
