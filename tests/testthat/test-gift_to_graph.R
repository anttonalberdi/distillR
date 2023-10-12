test_that("Test extract_first_parenthesis", {
  definition <- "a (b (c d)) (e f)"
  actual <- extract_first_parenthesis(definition)
  expected <- "(c d)"
  expect_equal(actual, expected)
})

test_that("Test decouple_graph", {
  definition <- "a (b c)"
  actual <- decouple_graph(definition)
  expected <- list(
    subgraph_0 = "b c",
    root = "a subgraph_0"
  )
  expect_equal(actual, expected)
})

test_that("Test plus_to_space", {
  definition <- "a+b"
  actual <- plus_to_space(definition)
  expected <- "a b"
  expect_equal(actual, expected)
})

test_that("Test dereplicate_graph", {
  definition <- "a (b c)"
  actual <- definition %>%
    plus_to_space() %>%
    decouple_graph() %>%
    dereplicate_graph()
  expected <- list(
    subgraph_0 = "b_0 c_0",
    root = "a_1 subgraph_0"
  )
  expect_equal(actual, expected)
})


test_that("Test dereplicated_graph_to_adjacency_list", {
  definition <- "a (b c)"
  actual <- definition %>%
    plus_to_space() %>%
    decouple_graph() %>%
    dereplicate_graph() %>%
    dereplicated_graph_to_adjacency_list()
  expected <- list(
    subgraph_0 = data.frame(
      from = c("subgraph_0_source", "b_0", "c_0"),
      to = c("b_0", "c_0", "subgraph_0_sink")
    ),
    root = data.frame(
      from = c("root_source", "a_1", "subgraph_0"),
      to = c("a_1", "subgraph_0", "root_sink")
    )
  )
  expect_equal(actual, expected)
})
