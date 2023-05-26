test_that("Test to_functions", {
  elements <- read_rds(test_path("fixtures", "gift_elements.rds"))
  actual <- to_functions(elements, distillR::GIFT_db)
  # write_rds(actual, test_path("fixtures", "gift_functions.rds"))  # nolint
  expected <- read_rds(test_path("fixtures", "gift_functions.rds"))
  expect_equal(actual, expected)
})
