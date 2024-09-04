test_that("to_function returns something", {
  distilled <- readr::read_rds(test_path("fixtures", "distill_expected.rds"))
  expect_visible(to_elements(distilled))
})


test_that("to_function returns the correct tibble dimensions", {
  distilled <- readr::read_rds(test_path("fixtures", "distill_expected.rds"))

  actual <- to_elements(distilled)
  actual_dims <- dim(distilled)
  expect_equal(actual_dims, c(315, 4))
})


test_that("to_function returns the correct tibble", {
  distilled <- readr::read_rds(test_path("fixtures", "distill_expected.rds"))
  actual <- distilled %>% to_elements()

  # readr::write_rds(x = actual, file = "tests/testthat/fixtures/to_elements_expected.rds")  # nolint
  expected <- readr::read_rds(
    test_path("fixtures", "to_elements_expected.rds")
  )

  expect_equal(actual, expected)
})
