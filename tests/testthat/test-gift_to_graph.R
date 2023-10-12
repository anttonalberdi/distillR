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
  actual <-
  "a (b c)" %>%
    plus_to_space() %>%
    decouple_graph() %>%
    bind_rows() %>%
    dereplicate_graph()
  expected <- list(
    subgraph_0 = "b_0 c_0",
    root = "a_1 subgraph_0"
  )
  expect_equal(actual, expected)
})

test_that("Test process_comma_subdefinition", {
  actual <- process_comma_subdefinition("b,c", "subgraph_1")
  expected <- tibble(
    from = c("subgraph_1_source", "subgraph_1_source", "b_sink", "c"),
    to = c("b_source", "c", "subgraph_1_sink", "subgraph_1_sink")
  )
  expect_equal(actual, expected)
})

test_that("Test process_space_subdefinition", {
  actual <- process_space_subdefinition("b c", "subgraph_1")
  expected <- tibble(
    from = c("subgraph_1_source", "b_sink", "c"),
    to = c("b_source", "c", "subgraph_1_sink")
  )
  expect_equal(actual, expected)
})


test_that("Test dereplicated_graph_to_adjacency_list", {
  actual <-
    "a (b,c)" %>%
    plus_to_space() %>%
    decouple_graph() %>%
    dereplicate_graph() %>%
    dereplicated_graph_to_adjacency_list()
  expected <- list(
    subgraph_0 = tibble(
      from = c("subgraph_0_source", "subgraph_0_source", "b_0", "c_0"),
      to = c("b_0", "c_0", "subgraph_0_sink", "subgraph_0_sink")
    ),
    root = tibble(
      from = c("root_source", "a_1", "subgraph_0"),
      to = c("a_1", "subgraph_0", "root_sink")
    )
  )
  expect_equal(actual, expected)
})
