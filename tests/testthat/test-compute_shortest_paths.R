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
