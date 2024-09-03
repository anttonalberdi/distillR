test_that("distill returns something", {
  annotations <- gene_annotations %>%
    import_dram() %>%
    dplyr::filter(mag_id == "MAG1")

  expect_visible(distill(annotations))
})


test_that("distill returns the correct tibble dimensions", {
  annotations <-
    gene_annotations %>%
    import_dram() %>%
    dplyr::filter(mag_id == "MAG1")

  actual_dims <- dim(distill(annotations))

  expected_dims <- c(315, 4)

  expect_equal(actual_dims, expected_dims)
})


test_that("distill returns the correct tibble", {
  actual <-
    gene_annotations %>%
    import_dram() %>%
    dplyr::filter(mag_id == "MAG1") %>%
    distill()
  # readr::write_rds(x = actual, file = "tests/testthat/fixtures/distill_expected.rds")  # nolint

  expected <- readr::read_rds(
    test_path("fixtures", "distill_expected.rds")
  )

  expect_equal(actual, expected)
})
