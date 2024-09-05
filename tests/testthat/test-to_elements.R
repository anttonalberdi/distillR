test_that("to_elements returns the correct tibble", {
  distilled <- readr::read_rds(test_path("fixtures", "distill.rds"))
  actual <- distilled %>% to_elements()

  # readr::write_rds(x = actual, file = "tests/testthat/fixtures/to_elements.rds")  # nolint
  expected <- readr::read_rds(
    test_path("fixtures", "to_elements.rds")
  )

  expect_equal(actual, expected)
})
