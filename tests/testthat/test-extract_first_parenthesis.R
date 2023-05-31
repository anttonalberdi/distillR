test_that("Test extract_first_parenthesis", {
  definition <- "a (b (c d)) (e f)"
  actual <- extract_first_parenthesis(definition)
  expected <- "(c d)"
  expect_equal(actual, expected)
})
