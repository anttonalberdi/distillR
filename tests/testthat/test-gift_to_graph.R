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
    from = c("subgraph_1_source", "subgraph_1_source", "b", "c"),
    to = c("b", "c", "subgraph_1_sink", "subgraph_1_sink")
  )
  expect_equal(actual, expected)
})

test_that("Test process_comma_subdefinition", {
  actual <- process_comma_subdefinition("b,subgraph_0", "subgraph_1")
  expected <- tibble(
    from = c("subgraph_1_source", "subgraph_1_source", "b", "subgraph_0_sink"),
    to = c("b", "subgraph_0_source", "subgraph_1_sink", "subgraph_1_sink")
  )
  expect_equal(actual, expected)
})


test_that("Test process_space_subdefinition", {
  actual <- process_space_subdefinition("b c", "subgraph_1")
  expected <- tibble(
    from = c("subgraph_1_source", "b", "c"),
    to = c("b", "c", "subgraph_1_sink")
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
      from = c("root_source", "a_1", "subgraph_0_sink"),
      to = c("a_1", "subgraph_0_source", "root_sink")
    )
  )
  expect_equal(actual, expected)
})


test_that("Test trim_intermediate_sources_and_sinks_df", {
  actual <-
    "a (b,c) (c+d)" %>%
    plus_to_space() %>%
    decouple_graph() %>%
    dereplicate_graph() %>%
    dereplicated_graph_to_adjacency_list() %>%
    dplyr::bind_rows() %>%
    trim_intermediate_sources_and_sinks_df()
  expected <-
    tibble(
      from = c("c_1", "root_source", "a_2", "a_2", "b_0", "c_0", "d_1"),
      to = c("d_1", "a_2", "b_0", "c_0", "c_1", "c_1", "root_sink")
    )
  expect_equal(actual, expected)
})


test_that("Test append_gift_id_to_df", {
  actual <-
    tibble(
      from = c("root", "a"),
      to = c("a", "sink")
    ) %>%
    append_gift_id_to_df("tag")
  expected <- tibble(
    from = c("tag_root", "tag_a"),
    to = c("tag_a", "tag_sink")
  )
  expect_equal(actual, expected)
})
