test_that("compute_shortest_paths returns something", {
  annotation_vector <- c("S09X", "6.1.1.4")
  expect_visible(compute_shortest_paths(annotation_vector))
})


test_that("compute_shortest_paths returns the correct tibble size", {
  annotation_vector <- c("S09X", "6.1.1.4")
  dims <- dim(compute_shortest_paths(annotation_vector))
  expect_equal(dims, c(315, 3))
})

test_that("compute_shortest_paths returns the correct tibble data", {
  actual <-
    distillR::gene_annotations %>%
    distillR::import_dram() %>%
    dplyr::filter(mag_id == "MAG1") %>%
    dplyr::pull(annotation_id) %>%
    compute_shortest_paths()
  # readr::write_rds(x = actual, file = "tests/testthat/fixtures/compute_shortest_paths_expected.rds")  # nolint
  expected <- readr::read_rds(
    test_path("fixtures", "compute_shortest_paths_expected.rds")
  )
  expect_equal(actual, expected)
})


test_that("compute_shortest_paths computes as complete an entire element", {

  annotations <- c(
    "K00601", "K00602", "K00764", "K01492", "K01587", "K01588", "K01589",
    "K01756", "K01923", "K01933", "K01945", "K01952", "K06863", "K08289",
    "K11176", "K11787", "K11788", "K11808", "K13713", "K23264", "K23265",
    "K23269", "K23270"
  )

  observed_cost <-
    annotations %>%
    compute_shortest_paths() %>%
    dplyr::filter(pathway_id == "B010101") %>%
    dplyr::pull(cost)

  expect_equal(observed_cost, 0)
})
