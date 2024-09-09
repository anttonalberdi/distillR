test_that("distill returns the correct tibble", {
  actual <-
    dram %>%
    import_dram() %>%
    dplyr::filter(mag_id == "GPB:bin_000004") %>%
    distill()
  # readr::write_rds(x = actual, file = "tests/testthat/fixtures/distill.rds", compress = "xz", version = 2, compression = 9)  # nolint

  expected <- readr::read_rds(
    test_path("fixtures", "distill.rds")
  )

  expect_equal(actual, expected)
})
