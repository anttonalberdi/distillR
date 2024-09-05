test_that("distill returns the correct tibble", {
  actual <-
    gene_annotations %>%
    import_dram() %>%
    dplyr::filter(mag_id == "MAG1") %>%
    distill()
  # readr::write_rds(x = actual, file = "tests/testthat/fixtures/distill_expected.rds")  # nolint

  expected <- readr::read_rds(
    test_path("fixtures", "distill.rds")
  )

  expect_equal(actual, expected)
})
