test_that("get_bundle_cost returns something", {
  annotation_vector <- c("S09X", "6.1.1.4")
  expect_visible(get_bundle_cost(annotation_vector))
})


test_that("get_bundle_cost returns the correct tibble", {
  annotation_vector <- c("S09X", "6.1.1.4")
  dims <- dim(get_bundle_cost(annotation_vector))
  expect_equal(dims, c(315, 3))
})

test_that("get_bundle_cost returns the correct dataframe", {
  actual <-
    distillR::gene_annotations %>%
    distillR::import_dram() %>%
    dplyr::filter(mag_id == "MAG1") %>%
    dplyr::pull(annotation_id) %>%
    get_bundle_cost()
  # readr::write_rds(x = actual, file = "tests/testthat/fixtures/get_bundle_cost_expected.rds")  # nolint
  expected <- readr::read_rds(
    test_path("fixtures", "get_bundle_cost_expected.rds")
  )
  expect_equal(actual, expected)
})
