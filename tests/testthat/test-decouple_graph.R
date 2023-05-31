test_that("Test decouple_graph", {
  definition <- "a (b c)"
  actual <- decouple_graph(definition)
  expected <- list(
    subgraph_0 = "b c",
    root = "a subgraph_0"
  )
  expect_equal(actual, expected)
})
