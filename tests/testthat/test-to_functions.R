test_that("Test to_functions", {
  elements <- readr::read_rds(test_path("fixtures", "gift_elements.rds"))
  actual <- to_functions(elements, distillR::GIFT_db)
  # readr::write_rds(actual, test_path("fixtures", "gift_functions.rds"))  # nolint
  expected <- readr::read_rds(test_path("fixtures", "gift_functions.rds"))
  expect_equal(actual, expected)
})
