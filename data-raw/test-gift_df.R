# extract_first_parenthesis ----
test_that("Test extract_first_parenthesis", {
  definition <- "a (b (c d)) (e f)"
  actual <- extract_first_parenthesis(definition)
  expected <- "(c d)"
  expect_equal(actual, expected)
})

# decouple_graph ----
test_that("Test decouple_graph", {
  definition <- "a (b c)"
  actual <- decouple_graph(definition)
  expected <- list(
    subgraph_0 = "b c",
    root = "a subgraph_0"
  )
  expect_equal(actual, expected)
})

test_that("Test decouple_graph with a EC definition", {
  definition <-
    "1.4.1.2 1.1.1.399 2.8.3.12 4.2.1.167 7.2.4.5 1.3.1.109 (2.8.3.1,2.8.3.8)"
  actual <- decouple_graph(definition)
  expected <- list(
    subgraph_0 = "2.8.3.1,2.8.3.8",
    root = "1.4.1.2 1.1.1.399 2.8.3.12 4.2.1.167 7.2.4.5 1.3.1.109 subgraph_0"
  )
  expect_equal(actual, expected)
})

# plus_to_space ----
test_that("Test plus_to_space", {
  definition <- "a+b"
  actual <- plus_to_space(definition)
  expected <- "a b"
  expect_equal(actual, expected)
})

# dereplicate_graph ----
test_that("Test dereplicate_graph", {
  actual <-
    "a (b c)" %>%
    plus_to_space() %>%
    decouple_graph() %>%
    dplyr::bind_rows() %>%
    dereplicate_graph()
  expected <- list(
    subgraph_0 = "b_0 c_0",
    root = "a_1 subgraph_0"
  )
  expect_equal(actual, expected)
})

test_that("Test dereplicate_graph with a EC definition", {

  # Trying strwrapping this results in a failed test
  definition <-
    "1.4.1.2 1.1.1.399 2.8.3.12 4.2.1.167 7.2.4.5 1.3.1.109 (2.8.3.1,2.8.3.8)"

  actual <-
    definition %>%
    plus_to_space() %>%
    decouple_graph() %>%
    dplyr::bind_rows() %>%
    dereplicate_graph()

  expected <-
    list(
      subgraph_0 = "2.8.3.1_0,2.8.3.8_0",
      root = strwrap(
        "1.4.1.2_1 1.1.1.399_1 2.8.3.12_1 4.2.1.167_1 7.2.4.5_1 1.3.1.109_1
        subgraph_0"
      )
    )

  expect_equal(names(actual), names(expected))
})

# process_comma_subdefinition ----
test_that("Test process_comma_subdefinition", {
  actual <- process_comma_subdefinition("b,c", "subgraph_1")
  expected <- tibble::tibble(
    from = c("subgraph_1_source", "subgraph_1_source", "b", "c"),
    to = c("b", "c", "subgraph_1_sink", "subgraph_1_sink")
  )
  expect_equal(actual, expected)
})

test_that("Test process_comma_subdefinition", {
  actual <- process_comma_subdefinition("b,subgraph_0", "subgraph_1")
  expected <- tibble::tibble(
    from = c("subgraph_1_source", "subgraph_1_source", "b", "subgraph_0_sink"),
    to = c("b", "subgraph_0_source", "subgraph_1_sink", "subgraph_1_sink")
  )
  expect_equal(actual, expected)
})

# process_space_subdefinition ----
test_that("Test process_space_subdefinition", {
  actual <- process_space_subdefinition("b c", "subgraph_1")
  expected <- tibble::tibble(
    from = c("subgraph_1_source", "b", "c"),
    to = c("b", "c", "subgraph_1_sink")
  )
  expect_equal(actual, expected)
})

# process_single_node_subdefinition ----
test_that("Test process_single_node_subdefinition", {

  actual <- process_single_node_subdefinition("b", "subgraph_1")

  expected <- tibble::tibble(
    from = c("subgraph_1_source", "b"),
    to = c("b", "subgraph_1_sink")
  )

  expect_equal(actual, expected)
})

# dereplicated_graph_to_adjacency_list ----
test_that("Test dereplicated_graph_to_adjacency_list", {

  actual <-
    "a (b,c)" %>%
    plus_to_space() %>%
    decouple_graph() %>%
    dereplicate_graph() %>%
    dereplicated_graph_to_adjacency_list()

  expected <- list(
    subgraph_0 = tibble::tibble(
      from = c("subgraph_0_source", "subgraph_0_source", "b_0", "c_0"),
      to = c("b_0", "c_0", "subgraph_0_sink", "subgraph_0_sink")
    ),
    root = tibble::tibble(
      from = c("root_source", "a_1", "subgraph_0_sink"),
      to = c("a_1", "subgraph_0_source", "root_sink")
    )
  )

  expect_equal(actual, expected)
})


test_that("Test dereplicated_graph_to_adjacency_list with a single gene
  definition", {
  actual <-
    "a" %>%
    plus_to_space() %>%
    decouple_graph() %>%
    dereplicate_graph() %>%
    dereplicated_graph_to_adjacency_list()
  expected <- list(
    root = tibble::tibble(
      from = c("root_source", "a_0"),
      to = c("a_0", "root_sink")
    )
  )
  expect_equal(actual, expected)
})

# trim_intermediate_sources_and_sinks_df ----
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
    tibble::tibble(
      from = c("c_1", "root_source", "a_2", "a_2", "b_0", "c_0", "d_1"),
      to = c("d_1", "a_2", "b_0", "c_0", "c_1", "c_1", "root_sink")
    )

  expect_equal(actual, expected)
})

# append_gift_id_to_df ----
test_that("Test append_gift_id_to_df", {

  actual <- tibble::tibble(
      from = c("root", "a"),
      to = c("a", "sink")
    ) %>%
    append_gift_id_to_df("tag")

  expected <- tibble::tibble(
    from = c("tag_root", "tag_a"),
    to = c("tag_a", "tag_sink")
  )

  expect_equal(actual, expected)
})

# definition_to_edge_df ----
test_that("Test definition_to_edge_df", {

  actual <- definition_to_edge_df("a (b c)", "tag")

  expected <- tibble::tibble(
    from = c("tag_b_0", "tag_root_source", "tag_a_1", "tag_c_0"),
    to = c("tag_c_0", "tag_a_1", "tag_b_0", "tag_root_sink")
  )

  expect_equal(actual, expected)
})


test_that("Test definition_to_edge_df with an EC definition", {

  actual <- definition_to_edge_df(
    "1.4.1.2 1.1.1.399 2.8.3.12 4.2.1.167 7.2.4.5 1.3.1.109 (2.8.3.1,2.8.3.8)",
    "B040207"
  )

  expected <- tibble::tibble(
    from = c(
      "B040207_root_source", "B040207_1.4.1.2_1", "B040207_1.1.1.399_1",
      "B040207_2.8.3.12_1", "B040207_4.2.1.167_1", "B040207_7.2.4.5_1",
      "B040207_1.3.1.109_1", "B040207_1.3.1.109_1", "B040207_2.8.3.1_0",
      "B040207_2.8.3.8_0"
    ),
    to = c(
      "B040207_1.4.1.2_1", "B040207_1.1.1.399_1", "B040207_2.8.3.12_1",
      "B040207_4.2.1.167_1", "B040207_7.2.4.5_1", "B040207_1.3.1.109_1",
      "B040207_2.8.3.1_0", "B040207_2.8.3.8_0", "B040207_root_sink",
      "B040207_root_sink"
    )
  )
  expect_equal(actual, expected)
})

# build_gift_df ----
test_that("Test build_gift_df", {
  actual <- build_gift_df()

  ncols <- ncol(actual)

  nrows <- nrow(actual)

  ngenes_plus_root <-
    actual %>%
    tidyr::pivot_longer(c(from, to)) %>%
    dplyr::select(value) %>%
    tidyr::separate(
      value,
      into = c("gift_id", "gene_id", "number"),
      sep = "_"
    ) %>%
    dplyr::pull(gene_id) %>%
    unique() %>%
    length()

  expect_equal(ncol(actual), 8)
  expect_equal(nrow(actual), 6750)
  expect_equal(ngenes_plus_root, 1547)
})
