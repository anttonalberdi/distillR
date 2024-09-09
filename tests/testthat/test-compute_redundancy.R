test_that("compute_redundancy returns the correct tibble", {

  annotation_vector <- dram %>%
    import_dram() %>%
    dplyr::pull(annotation_id) %>%
    unique()

  actual <- compute_redundancy(annotation_vector)
  # readr::write_rds(actual, file = test_path("fixtures", "compute_redundancy.rds"), compress = "xz", version = 2, compression = 9)  # nolint

  expected <- readr::read_rds(test_path("fixtures", "compute_redundancy.rds"))

  expect_equal(actual, expected)
})
