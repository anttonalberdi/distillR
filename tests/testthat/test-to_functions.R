test_that("to_functions returns the correct tibble", {
  distilled <- readr::read_rds(test_path("fixtures", "distill.rds"))
  actual <- distilled %>% to_functions()

  # readr::write_rds(x = actual, file = "tests/testthat/fixtures/to_functions.rds")  # nolint
  expected <- readr::read_rds(
    test_path("fixtures", "to_functions.rds")
  )

  expect_equal(actual, expected)
})
