test_that("to_domains returns the correct tibble", {
  distilled <- readr::read_rds(test_path("fixtures", "distill.rds"))
  actual <- distilled |> to_domains()

  # readr::write_rds(x = actual, file = "tests/testthat/fixtures/to_domains.rds", compress = "xz", version = 2, compression = 9)  # nolint
  expected <- readr::read_rds(
    test_path("fixtures", "to_domains.rds")
  )

  expect_equal(actual, expected)
})
